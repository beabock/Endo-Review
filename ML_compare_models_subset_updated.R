#B. Bock
#6/19/25
#ML Compare Models with four-class labels: Presence, Absence, Review, Other

# Library loading ---------------------------------------------------------

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

# Custom palette
cus_pal <- c(
  "#A1C181",  # soft sage green — for plants
  "#619B8A",  # muted teal — evokes moss or lichens
  "#C97E7E",  # dusty rose — for fungi like Russula or Hygrophoropsis
  "#D9AE94"   # pale mushroom beige — for caps and forest floor tones
)

# Functions ---------------------------------------------------------------
save_plot <- function(filename, plot, width = 12, height = 7, units = "in", ...) {
  ggsave(filename, plot, width = width, height = height, units = units, ...)
}

train_and_evaluate <- function(train_df, test_df, method_name, tune_len = 10, metric="Accuracy", ...) {
  train_control <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    classProbs = TRUE,
    summaryFunction = multiClassSummary,
    savePredictions = "final"
  )
  
  model <- train(
    label ~ .,
    data = train_df,
    method = method_name,
    trControl = train_control,
    tuneLength = tune_len,
    metric = metric,
    ...
  )
  
  preds <- predict(model, newdata = test_df)
  cm <- confusionMatrix(preds, test_df$label)
  
  list(
    model = model,
    confusion_matrix = cm,
    accuracy = cm$overall["Accuracy"]
  )
}

# Load and preprocess labeled abstracts ----------------------------------

set.seed(1998)

labeled_abstracts <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(label %in% c("Presence", "Absence", "Review", "Other")) %>%
  mutate(id = row_number())

# Remove artificial or duplicate Presence examples
rows_to_remove <- labeled_abstracts %>%
  filter(
    is.na(doi) | doi %in% c("", "<NA>", "NA"),
    label == "Presence",
    is.na(authors) | authors == ""
  )

labeled_abstracts <- labeled_abstracts %>%
  anti_join(rows_to_remove, by = "id") %>%
  group_by(label) %>%
  ungroup()

# Create document-term matrix
dtm <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

rownames_dtm <- dtm$dimnames$Docs
dtm_matrix <- as.matrix(dtm)

# Prepare data split
labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% rownames_dtm) %>%
  mutate(label = factor(label))

train_index <- createDataPartition(labeled_abstracts$label, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data  <- labeled_abstracts[-train_index, ]

train_ids <- as.character(train_data$id)
test_ids  <- as.character(test_data$id)

train_matrix <- dtm_matrix[train_ids, ]
test_matrix  <- dtm_matrix[test_ids, ]

train_df <- as.data.frame(train_matrix) %>% mutate(label = train_data$label)
test_df  <- as.data.frame(test_matrix)  %>% mutate(label = test_data$label)

# Balance classes via up-sampling
train_recipe <- recipe(label ~ ., data = train_df) %>%
  step_upsample(label) %>%
  prep()

balanced_train <- juice(train_recipe)

# Compare models ----------------------------------------------------------
models_to_try <- c("glmnet", "svmLinear")
results <- list()

for(method in models_to_try) {
  cat("Training", method, "\n")
  tic(method)
  res <- tryCatch({
    train_and_evaluate(balanced_train, test_df, method_name = method, tune_len = 10)
  }, error = function(e) {
    message("Error in ", method, ": ", e$message)
    NULL
  })
  toc()
  results[[method]] <- res
}

# Compile accuracy table
accuracy_table <- tibble(
  model = names(results),
  accuracy = map_dbl(results, ~ .x$accuracy)
) %>%
  arrange(desc(accuracy))

print(accuracy_table)

# Plot accuracy comparison
accuracy_table %>%
  ggplot(aes(x = reorder(model, accuracy), y = accuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy") +
  theme_minimal(base_size = 14)

# Select best model and save
best <- results[[accuracy_table$model[1]]]$model
saveRDS(best, "best_model_svmLinear_four_classes.rds")

# PCA visualization -------------------------------------------------------
library(irlba)
svd_res <- irlba(train_matrix, nv = 2)
coords <- svd_res$u %*% diag(svd_res$d)
pca_df <- data.frame(PC1 = coords[,1], PC2 = coords[,2], label = train_data$label)

ggplot(pca_df, aes(x = PC1, y = PC2, color = label)) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA of Word Features", x = "PC1", y = "PC2") +
  scale_color_manual(values = cus_pal)

save_plot("PCA_Label_four_classes.png", last_plot())
