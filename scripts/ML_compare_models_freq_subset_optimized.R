# ML Compare Models Optimized: four-class labels with feature selection & parallel processing
# Date: 7/22/2025
#Frequency based'

#idea for next time... maybe try relevance and then presence/absence like original. go back to when it worked better around EMSL time.

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
library(randomForest)
library(xgboost)

# Parallel backend --------------------------------------------------------
cores <- parallel::detectCores() - 1
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
on.exit(stopCluster(cl), add = TRUE)

# Utility to save plots --------------------------------------------------
save_plot <- function(filename, plot, width = 12, height = 7, units = "in", ...) {
  if (!dir.exists("results")) dir.create("results")
  ggsave(file.path("results", filename), plot, width = width, height = height, units = units, ...)
}

# Read & filter labeled abstracts ----------------------------------------
set.seed(1998)
labeled <- read.csv("data/Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(label %in% c("Presence", "Absence", "Review", "Other", "Both")) %>%
  mutate(id = row_number())

labeled %>%
  group_by(label) %>%
  summarise(count = n())

#Could use like 70 more Other

# Remove low-quality Presence samples
bad <- labeled %>%
  filter(label=="Presence", is.na(doi) | doi=="", is.na(authors) | authors=="")
labeled <- anti_join(labeled, bad, by="id")

labeled %>%
  group_by(label) %>%
  summarise(count = n())

# Create a two-stage classification approach -----------------------------
# First, create a binary relevance label (Relevant vs. Irrelevant)
labeled <- labeled %>%
  mutate(
    relevance = case_when(
      label %in% c("Presence", "Absence", "Both") ~ "Relevant",
      label %in% c("Review", "Other") ~ "Irrelevant",
      TRUE ~ NA_character_
    ),
    relevance = factor(relevance)
  )


# For the second stage, create a presence/both/absence label for relevant abstracts only
labeled <- labeled %>%
  mutate(
    presence_both_absence = case_when(
      label == "Presence" ~ "Presence",
      label == "Absence" ~ "Absence",
      label == "Both" ~ "Both",
      TRUE ~ NA_character_
    ),
    presence_both_absence = factor(presence_both_absence, levels = c("Presence", "Both", "Absence"))
  )

# Keep the original label for comparison
labeled <- labeled %>%
  mutate(original_label = label,
         label = relevance)  # Use relevance as the primary label for the first stage

# Tokenize & build DTM with unigrams and bigrams --------------------------
dtm_unigrams <- labeled %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "\\d")) %>%
  mutate(word = str_to_lower(word)) %>%
  count(id, word, sort=TRUE)

dtm_bigrams <- labeled %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!str_detect(word1, "\\d")) %>%
  filter(!str_detect(word2, "\\d")) %>%
  unite(bigram, word1, word2, sep = "_") %>%
  count(id, bigram, sort=TRUE) %>%
  rename(word = bigram)

# Combine unigrams and bigrams (FREQUENCY ONLY - no TF-IDF weighting)
dtm_combined <- bind_rows(dtm_unigrams, dtm_bigrams) %>%
  group_by(id, word) %>%
  summarise(freq = sum(n), .groups = 'drop') %>%
  cast_dtm(document = id, term = word, value = freq)

dtm <- dtm_combined

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

# Recipe: up-sample with class weights & prep ----------------------------
# Calculate class weights (inverse of class frequency)
class_counts <- table(train_df$label)
class_weights <- 1 / (class_counts / sum(class_counts))
class_weights <- class_weights / min(class_weights)  # Normalize weights

# Create sample weights based on class
train_weights <- class_weights[train_df$label]
names(train_weights) <- rownames(train_df)

# Upsample with higher weight for "Other" class
# First create a separate recipe just for the "Other" class
other_indices <- which(train_df$label == "Other")
if (length(other_indices) > 0) {
  # Duplicate "Other" samples to increase their representation
  other_samples <- train_df[other_indices, ]
  # Duplicate 3 times (adjust as needed)
  other_samples_extra <- do.call(rbind, replicate(3, other_samples, simplify = FALSE))
  
  # Combine with original data
  train_df_balanced <- rbind(train_df, other_samples_extra)
} else {
  train_df_balanced <- train_df
}

# Now apply regular upsampling to balance all classes
rec <- recipe(label ~ ., data = train_df_balanced) %>%
  step_upsample(label) %>%  # Regular upsampling for all classes
  prep()

balanced <- juice(rec)

# Custom summary function that emphasizes recall for binary classification
custom_summary <- function(data, lev = NULL, model = NULL) {
  # For binary classification
  if (length(lev) == 2) {
    # Calculate standard metrics
    stats <- defaultSummary(data, lev, model)
    
    # Add F2 score which weights recall higher than precision (5:1)
    # For the "Relevant" class (which we want to minimize false negatives for)
    relevant_idx <- which(lev == "Relevant")
    if (length(relevant_idx) > 0) {
      cm <- confusionMatrix(data$pred, data$obs, positive = lev[relevant_idx])
      precision <- cm$byClass["Pos Pred Value"]
      recall <- cm$byClass["Sensitivity"]
      
      # F2 score formula: (1+beta^2) * (precision*recall) / (beta^2*precision + recall)
      # where beta=2 to weight recall higher
      beta <- 2
      f2 <- ifelse(
        precision + recall > 0,
        (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall),
        0
      )
      stats["F2"] <- f2
    }
    
    return(stats)
  } else {
    # For multi-class, use the standard multiClassSummary
    return(multiClassSummary(data, lev, model))
  }
}

# Custom summary for presence/absence that heavily penalizes false positives
custom_summary_presence <- function(data, lev = NULL, model = NULL) {
  # Calculate standard metrics
  stats <- defaultSummary(data, lev, model)
  
  # Add custom metrics that penalize false positives heavily
  if (length(lev) == 2) {
    # Assume "Presence" is the positive class
    presence_idx <- which(lev == "Presence")
    if (length(presence_idx) > 0) {
      cm <- confusionMatrix(data$pred, data$obs, positive = lev[presence_idx])
      
      # Specificity is crucial - we want very few false positives (predicting Absence when it's Presence)
      specificity <- cm$byClass["Specificity"]
      sensitivity <- cm$byClass["Sensitivity"]
      
      # Custom metric: heavily weight specificity (minimize false positives)
      # This penalizes predicting "Absence" when it's actually "Presence"
      weighted_score <- 0.9 * specificity + 0.1 * sensitivity
      
      stats["WeightedScore"] <- weighted_score
      stats["Specificity"] <- specificity
    }
  }
  
  return(stats)
}

# Train control with custom summary function
control <- trainControl(
  method = "repeatedcv", number = 5, repeats = 3,
  classProbs = TRUE, summaryFunction = custom_summary,
  savePredictions = "final", allowParallel = TRUE
)

# Train control specifically for relevance classification
control_relevance <- trainControl(
  method = "repeatedcv", number = 5, repeats = 3,
  classProbs = TRUE, summaryFunction = custom_summary,
  savePredictions = "final", allowParallel = TRUE
)

# Train control specifically for presence/absence classification
control_presence <- trainControl(
  method = "repeatedcv", number = 5, repeats = 3,
  classProbs = TRUE, summaryFunction = custom_summary_presence,
  savePredictions = "final", allowParallel = TRUE
)

# Train & evaluate function ---------------------------------------------
train_and_eval <- function(data, test_data, method, tune_len=10, weights=NULL, 
                          optimize_metric="Accuracy", threshold=0.5, use_presence_control=FALSE, ...) {
  tic(method)
  
  # Choose appropriate control based on task
  train_control <- if(use_presence_control) control_presence else control
  
  # Special handling for different model types
  if (method == "xgbTree") {
    # XGBoost needs special handling for class weights
    model <- train(
      label ~ ., data = data, method = method,
      trControl = train_control, tuneLength = tune_len,
      metric = optimize_metric,  # Use the specified metric
      weights = weights,
      ...
    )
  } else {
    # Default handling for other models
    model <- train(
      label ~ ., data = data, method = method,
      trControl = train_control, tuneLength = tune_len,
      metric = optimize_metric,  # Use the specified metric
      weights = weights,
      ...
    )
  }
  
  toc()
  
  # For binary classification with threshold adjustment
  if (length(levels(test_data$label)) == 2 && threshold != 0.5) {
    # Get class probabilities
    probs <- predict(model, newdata = test_data, type = "prob")
    
    # Apply custom threshold (assuming second class is the positive class)
    positive_class <- levels(test_data$label)[2]
    preds <- factor(
      ifelse(probs[[positive_class]] > threshold, positive_class, levels(test_data$label)[1]),
      levels = levels(test_data$label)
    )
  } else {
    # Standard prediction
    preds <- predict(model, newdata = test_data)
  }
  
  cm <- confusionMatrix(preds, test_data$label)
  
  # Calculate class-specific metrics - handle both binary and multi-class cases
  if (length(levels(test_data$label)) == 2) {
    # Binary classification - byClass is a vector
    class_metrics <- data.frame(
      Class = levels(test_data$label),
      Sensitivity = c(cm$byClass["Sensitivity"], NA),  # First class sensitivity
      Specificity = c(cm$byClass["Specificity"], NA),  # First class specificity
      Balanced_Accuracy = c(cm$byClass["Balanced Accuracy"], NA),  # First class balanced accuracy
      row.names = NULL
    )
    
    # Fill in second class metrics (reverse of first class)
    class_metrics$Sensitivity[2] <- cm$byClass["Specificity"]  # Second class sensitivity = first class specificity
    class_metrics$Specificity[2] <- cm$byClass["Sensitivity"]  # Second class specificity = first class sensitivity
    class_metrics$Balanced_Accuracy[2] <- cm$byClass["Balanced Accuracy"]  # Same balanced accuracy
    
  } else {
    # Multi-class classification - byClass is a matrix
    class_metrics <- data.frame(
      Class = levels(test_data$label),
      Sensitivity = cm$byClass[, "Sensitivity"],
      Specificity = cm$byClass[, "Specificity"],
      Balanced_Accuracy = cm$byClass[, "Balanced Accuracy"],
      row.names = NULL
    )
  }
  
  list(
    model = model, 
    cm = cm, 
    acc = cm$overall["Accuracy"],
    class_metrics = class_metrics
  )
}

# Compare models for Stage 1: Relevance Classification -------------------
cat("\n--- STAGE 1: RELEVANCE CLASSIFICATION (Relevant vs. Irrelevant) ---\n")
# Add more models for Stage 1
models <- c("glmnet", "svmLinear", "rf", "xgbTree")

# More aggressive feature selection for relevance classification
# Use different upsampling ratios for different classes to penalize false negatives
rec_relevance <- recipe(label ~ ., data = train_df) %>%
  step_corr(all_predictors(), threshold = 0.9) %>%  # Remove highly correlated features
  step_nzv(all_predictors(), freq_cut = 95/5) %>%   # More aggressive NZV filtering
  # Upsample with different ratios - higher for Relevant class to reduce false negatives
  step_upsample(label, over_ratio = 2.0) %>%
  prep()

balanced_relevance <- juice(rec_relevance)

# Train all models for relevance classification
results_relevance <- list()
# Try multiple thresholds for best recall
thresholds <- c(0.4, 0.3, 0.25)
for (m in models) {
  for (thresh in thresholds) {
    cat("Training model:", m, "with threshold", thresh, "\n")
    res <- train_and_eval(
      balanced_relevance, test_df, m, tune_len = 5,
      weights = NULL,
      optimize_metric = "F2",
      threshold = thresh
    )
    results_relevance[[paste0(m, "_thr", thresh)]] <- res
  }
}


# Now train models for Stage 2: Presence/Both/Absence classification ------
cat("\n--- STAGE 2: PRESENCE/BOTH/ABSENCE CLASSIFICATION ---\n")

# Filter training and test data for only Presence, Both, and Absence
train_relevant <- train_lab %>%
  filter(original_label %in% c("Presence", "Absence", "Both")) %>%
  mutate(label = factor(presence_both_absence, levels = c("Presence", "Both", "Absence")))

test_relevant <- test_lab %>%
  filter(original_label %in% c("Presence", "Absence", "Both")) %>%
  mutate(label = factor(presence_both_absence, levels = c("Presence", "Both", "Absence")))

# Get corresponding rows from the DTM
train_mat_relevant <- train_mat[as.character(train_relevant$id), ]
test_mat_relevant <- test_mat[as.character(test_relevant$id), ]

# Create data frames
train_df_relevant <- as.data.frame(train_mat_relevant) %>% 
  mutate(label = train_relevant$label)
test_df_relevant <- as.data.frame(test_mat_relevant) %>% 
  mutate(label = test_relevant$label)

# Remove near-zero variance predictors for the relevant subset
nzv_relevant <- nearZeroVar(train_df_relevant, saveMetrics = TRUE)
keep_relevant <- rownames(nzv_relevant)[!nzv_relevant$nzv]
train_df_relevant <- train_df_relevant %>% select(all_of(keep_relevant), label)
test_df_relevant <- test_df_relevant %>% select(all_of(keep_relevant), label)

# Apply upsampling to balance all three classes
rec_relevant <- recipe(label ~ ., data = train_df_relevant) %>%
  step_upsample(label) %>%
  prep()

balanced_relevant <- juice(rec_relevant)

# Calculate class weights for real-world distribution (optional, can be adjusted)
class_counts <- table(balanced_relevant$label)
class_weights <- 1 / (class_counts / sum(class_counts))
class_weights <- class_weights / min(class_weights)  # Normalize weights
train_weights_relevant <- class_weights[balanced_relevant$label]
names(train_weights_relevant) <- rownames(balanced_relevant)

# Train models for Presence/Both/Absence with class weights
results_presence_both_absence <- list()
for (m in models) {
  cat("Training model:", m, "with class weights\n")
  results_presence_both_absence[[m]] <- train_and_eval(
    balanced_relevant, test_df_relevant, m, tune_len = 5,
    weights = train_weights_relevant,
    optimize_metric = "Accuracy",  # Use multiclass accuracy or update as needed
    use_presence_control = FALSE
  )
}

# Store both sets of results
results <- list(
  relevance = results_relevance,
  presence_both_absence = results_presence_both_absence
)

# Accuracy tables & plots for both stages --------------------------------

# Function to create accuracy table and plots
create_accuracy_plots <- function(results_list, stage_name) {
  # Create accuracy table
  acc_tbl <- tibble(
    model = names(results_list),
    accuracy = map_dbl(results_list, "acc")
  ) %>% arrange(desc(accuracy))
  
  print(paste("Accuracy Table for", stage_name))
  print(acc_tbl)
  
  # Plot accuracy comparison
  p <- acc_tbl %>%
    ggplot(aes(x = reorder(model, accuracy), y = accuracy)) +
    geom_col(fill="steelblue") + coord_flip() +
    labs(title=paste("Model Accuracy Comparison -", stage_name), 
         x="Model", y="Accuracy") +
    theme_minimal(base_size=14)
  
  save_plot(paste0("accuracy_comparison_", tolower(gsub(" ", "_", stage_name)), ".png"), p)
  
  # Class-specific metrics
  class_metrics <- map_df(names(results_list), function(m) {
    results_list[[m]]$class_metrics %>%
      mutate(Model = m) %>%
      select(Model, everything())
  })
  
  # Plot class-specific balanced accuracy
  p2 <- class_metrics %>%
    ggplot(aes(x = Model, y = Balanced_Accuracy, fill = Class)) +
    geom_col(position = "dodge") +
    labs(title = paste("Balanced Accuracy by Class and Model -", stage_name), 
         x = "Model", y = "Balanced Accuracy") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  save_plot(paste0("class_balanced_accuracy_", tolower(gsub(" ", "_", stage_name)), ".png"), p2)
  
  # Find best model
  best_name <- acc_tbl$model[1]
  best_model <- results_list[[best_name]]$model
  
  # Save best model
  saveRDS(best_model, file.path("models", paste0("best_model_", best_name, "_", tolower(gsub(" ", "_", stage_name)), ".rds")))
  
  return(list(
    acc_tbl = acc_tbl,
    best_name = best_name,
    best_model = best_model
  ))
}

# Function to create confusion matrix plots
plot_confusion_matrix <- function(cm, model_name, stage_name, normalized = FALSE) {
  if (normalized) {
    # Normalize by row (reference)
    cm_data <- as.data.frame(prop.table(cm$table, 1))
    title <- paste("Normalized Confusion Matrix -", model_name, "-", stage_name)
    subtitle <- "Values normalized by row (Reference)"
    label_format <- function(x) sprintf("%.2f", x)
  } else {
    cm_data <- as.data.frame(cm$table)
    title <- paste("Confusion Matrix -", model_name, "-", stage_name)
    subtitle <- NULL
    label_format <- function(x) as.character(x)
  }
  
  p <- ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = label_format(Freq)), color = "black") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, subtitle = subtitle,
         x = "Reference", y = "Prediction") +
    theme_minimal(base_size = 12)
  
  stage_suffix <- tolower(gsub(" ", "_", stage_name))
  filename <- ifelse(normalized, 
                    paste0("confusion_matrix_", model_name, "_", stage_suffix, "_normalized.png"),
                    paste0("confusion_matrix_", model_name, "_", stage_suffix, ".png"))
  save_plot(filename, p)
  
  return(p)
}

# Create plots for Stage 1: Relevance Classification
cat("\n--- EVALUATING STAGE 1: RELEVANCE CLASSIFICATION ---\n")
# Automated model and threshold selection based on F2 score
f2_scores <- sapply(results$relevance, function(res) {
  if (!is.null(res$class_metrics) && "Relevant" %in% res$class_metrics$Class) {
    idx <- which(res$class_metrics$Class == "Relevant")
    # Use F2 from custom_summary if available, else fallback to Sensitivity
    if (!is.null(res$model$results$F2)) {
      return(max(res$model$results$F2, na.rm = TRUE))
    } else {
      return(res$class_metrics$Sensitivity[idx])
    }
  } else {
    return(NA)
  }
})
best_idx <- which.max(f2_scores)
best_model_name <- names(results$relevance)[best_idx]
cat("\nBest relevance model (by F2):", best_model_name, "F2=", f2_scores[best_idx], "\n")
stage1_results <- create_accuracy_plots(results$relevance, "Relevance Classification")

# Error analysis: show misclassified abstracts for best model
best_model_preds <- results$relevance[[best_model_name]]$cm$table
cat("\nError analysis for best relevance model:", best_model_name, "\n")
# Get predictions and actuals
preds <- predict(results$relevance[[best_model_name]]$model, newdata = test_df)
actuals <- test_df$label
misclassified_idx <- which(preds != actuals)
if (length(misclassified_idx) > 0) {
  cat("Misclassified abstracts (first 10):\n")
  print(test_lab[misclassified_idx[1:min(10, length(misclassified_idx))], c("id", "abstract", "label", "original_label")])
} else {
  cat("No misclassifications found.\n")
}

# Active learning: show least confident predictions for manual review
if ("prob" %in% methods(class(results$relevance[[best_model_name]]$model))) {
  probs <- predict(results$relevance[[best_model_name]]$model, newdata = test_df, type = "prob")
  # Get confidence for predicted class
  conf <- apply(probs, 1, max)
  low_conf_idx <- order(conf)[1:10]
  cat("\nActive learning candidates (lowest confidence, first 10):\n")
  print(test_lab[low_conf_idx, c("id", "abstract", "label", "original_label")])
}


# Create plots for Stage 2: Presence/Both/Absence
cat("\n--- EVALUATING STAGE 2: PRESENCE/BOTH/ABSENCE CLASSIFICATION ---\n")
stage2_results <- create_accuracy_plots(results$presence_both_absence, "Presence Both Absence")

# Plot confusion matrices for all models in both stages
# Stage 1: Relevance Classification
for (m in names(results$relevance)) {
  cat("Creating confusion matrix for Stage 1:", m, "\n")
  cm <- results$relevance[[m]]$cm
  print(cm)  # Print confusion matrix to console
  # Plot and save regular confusion matrix
  plot_confusion_matrix(cm, m, "Relevance", normalized = FALSE)
  # Plot and save normalized confusion matrix
  plot_confusion_matrix(cm, m, "Relevance", normalized = TRUE)
}

# Stage 2: Presence/Both/Absence
for (m in names(results$presence_both_absence)) {
  cat("Creating confusion matrix for Stage 2:", m, "\n")
  cm <- results$presence_both_absence[[m]]$cm
  print(cm)  # Print confusion matrix to console
  # Plot and save regular confusion matrix
  plot_confusion_matrix(cm, m, "Presence_Both_Absence", normalized = FALSE)
  # Plot and save normalized confusion matrix
  plot_confusion_matrix(cm, m, "Presence_Both_Absence", normalized = TRUE)
}

# Plot confusion matrices for best models
cat("\nBest model for Stage 1 (Relevance):", stage1_results$best_name, "\n")
best_cm_relevance <- results$relevance[[stage1_results$best_name]]$cm
print(best_cm_relevance)

saveRDS(list(model_name = stage1_results$best_name, threshold = stage1_results$best_model$results$threshold),
        file = paste0("best_model_relevance_", stage1_results$best_name, ".rds"))

cat("\nBest model for Stage 2 (Presence/Both/Absence):", stage2_results$best_name, "\n")
best_cm_presence_both_absence <- results$presence_both_absence[[stage2_results$best_name]]$cm
print(best_cm_presence_both_absence)

saveRDS(list(model_name = stage2_results$best_name),
        file = paste0("best_model_presence_both_absence_", stage2_results$best_name, ".rds"))


# PCA visualization of training data -------------------------------------
svd_res <- irlba(train_mat, nv=2)
coords <- svd_res$u %*% diag(svd_res$d)
pca_df <- tibble(PC1 = coords[,1], PC2 = coords[,2], label = train_lab$label)

ggplot(pca_df, aes(PC1, PC2, color = label)) +
  geom_point(alpha=0.5) +
  labs(title="PCA of Word Features", x="PC1", y="PC2") +
  theme_minimal(base_size=12)

save_plot("PCA_optimized.png", last_plot())
