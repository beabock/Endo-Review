# ML Compare Models Optimized: four-class labels with feature selection & parallel processing
# Date: 7/9/2025

library(tidyverse)
library(tidytext)
library(caret)
library(Matrix)
library(text)
library(tm)
library(recipes)
library(themis)
library(janitor)
library(tictoc)
library(doParallel)
library(irlba)

# Parallel backend --------------------------------------------------------
cores <- parallel::detectCores() - 1
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
on.exit(stopCluster(cl), add = TRUE)

# Utility to save plots --------------------------------------------------
save_plot <- function(filename, plot, width = 12, height = 7, units = "in", ...) {
  ggsave(filename, plot, width = width, height = height, units = units, ...)
}

# Read & filter labeled abstracts ----------------------------------------
set.seed(1998)
labeled <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(label %in% c("Presence", "Absence", "Review", "Other")) %>%
  mutate(id = row_number())

# Remove low-quality Presence samples
bad <- labeled %>%
  filter(label=="Presence", is.na(doi) | doi=="", is.na(authors) | authors=="")
labeled <- anti_join(labeled, bad, by="id")

# Tokenize & build DTM ----------------------------------------------------
dtm <- labeled %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "\\d")) %>%
  mutate(word = str_to_lower(word)) %>%
  count(id, word, sort=TRUE) %>%
  cast_dtm(document = id, term = word, value = n)

dtm_mat <- as.matrix(dtm)
rownames(dtm_mat) <- dtm$dimnames$Docs

# Align labeled data
labeled <- labeled %>%
  filter(as.character(id) %in% rownames(dtm_mat)) %>%
  mutate(label = factor(label))

# Train/test split
idx <- createDataPartition(labeled$label, p=0.8, list=FALSE)
train_lab <- labeled[idx, ]
test_lab  <- labeled[-idx, ]

train_mat <- dtm_mat[as.character(train_lab$id), ]
test_mat  <- dtm_mat[as.character(test_lab$id), ]

train_df <- as.data.frame(train_mat) %>% mutate(label = train_lab$label)
test_df  <- as.data.frame(test_mat)  %>% mutate(label = test_lab$label)

# Remove near-zero variance predictors -----------------------------------
nzv <- nearZeroVar(train_df, saveMetrics = TRUE)
keep <- rownames(nzv)[!nzv$nzv]
train_df <- train_df %>% select(all_of(keep), label)
test_df  <- test_df  %>% select(all_of(keep), label)

# Recipe: up-sample & prep -----------------------------------------------
rec <- recipe(label ~ ., data = train_df) %>%
  step_upsample(label) %>%
  prep()

balanced <- juice(rec)

# Train control uses multiclass summary, allows parallel
control <- trainControl(
  method = "repeatedcv", number = 5, repeats = 3,
  classProbs = TRUE, summaryFunction = multiClassSummary,
  savePredictions = "final", allowParallel = TRUE
)

# Train & evaluate function ---------------------------------------------
train_and_eval <- function(data, test_data, method, tune_len=10) {
  tic(method)
  model <- train(
    label ~ ., data = data, method = method,
    trControl = control, tuneLength = tune_len,
    metric = "Accuracy"
  )
  toc()
  preds <- predict(model, newdata = test_data)
  cm <- confusionMatrix(preds, test_data$label)
  list(model = model, cm = cm, acc = cm$overall["Accuracy"])
}

# Compare models ----------------------------------------------------------
models <- c("glmnet", "svmLinear")
results <- map(models, ~ train_and_eval(balanced, test_df, .x, tune_len = 10)) %>%
  set_names(models)

# Accuracy table & plot --------------------------------------------------
acc_tbl <- tibble(
  model = models,
  accuracy = map_dbl(results, "acc")
) %>% arrange(desc(accuracy))

print(acc_tbl)

acc_tbl %>%
  ggplot(aes(x = reorder(model, accuracy), y = accuracy)) +
  geom_col(fill="steelblue") + coord_flip() +
  labs(title="Model Accuracy Comparison", x="Model", y="Accuracy") +
  theme_minimal(base_size=14)

save_plot("accuracy_comparison.png", last_plot())

# Save best model ---------------------------------------------------------
best_name <- acc_tbl$model[1]
best_model <- results[[best_name]]$model
saveRDS(best_model, paste0("best_model_", best_name, "_optimized.rds"))

# PCA visualization of training data -------------------------------------
svd_res <- irlba(train_mat, nv=2)
coords <- svd_res$u %*% diag(svd_res$d)
pca_df <- tibble(PC1 = coords[,1], PC2 = coords[,2], label = train_lab$label)

ggplot(pca_df, aes(PC1, PC2, color = label)) +
  geom_point(alpha=0.5) +
  labs(title="PCA of Word Features", x="PC1", y="PC2") +
  theme_minimal(base_size=12)

save_plot("PCA_optimized.png", last_plot())
