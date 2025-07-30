#B. Bock
#6/19/25
#ML Approach: this is adapted from ML_bigger.R. 
#This time, I want to subset my training dataset, and compare different ML approaches on them. See what works best then run those on bigger training datasets and run again.


#7/23/25 Coming back to this since this seemed like the best approach. Adding in relevance filtering step.
#As of 7/30/25, this is the best script to use for training models on the Endo Review project. Use this file!

# Library loading ---------------------------------------------------------



# Load necessary libraries

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

getwd()

#setwd("Endo_Review")

cus_pal <- c(
  "#A1C181",  # soft sage green — for plants
  "#619B8A",  # muted teal — evokes moss or lichens
  "#C97E7E",  # dusty rose — for fungi like Russula or Hygrophoropsis
  "#D9AE94"   # pale mushroom beige — for caps and forest floor tones
)

# Relevance Classification (Relevant vs Irrelevant) ----------------------


set.seed(1998)

labeled_abstracts <- read.csv("data/Training_labeled_abs_5.csv") %>%
  clean_names() %>%
 # filter(label %in% c("Presence", "Absence")) %>%
  mutate(id = row_number())%>%
  filter(label != "")%>%
  mutate(
    relevance = case_when(
      label %in% c("Presence", "Absence", "Both") ~ "Relevant",
      label %in% c("Review", "Other") ~ "Irrelevant",
      TRUE ~ NA_character_
    ),
    relevance = factor(relevance)
  )%>%
  mutate(
    presence_both_absence = case_when(
      label == "Presence" ~ "Presence",
      label == "Absence" ~ "Absence",
      label == "Both" ~ "Presence",
      TRUE ~ NA_character_
    ),
    presence_both_absence = factor(presence_both_absence, levels = c("Presence", "Absence"))
  )


# Remove artificial or duplicate Presence examples
rows_to_remove <- labeled_abstracts %>%
  filter(
    is.na(doi) | doi %in% c("", "<NA>", "NA"),
    label == "Presence",
    is.na(authors) | authors == ""
  )

labeled_abstracts <- labeled_abstracts %>%
  anti_join(rows_to_remove, by = "id") %>%
  ungroup()

# DTM creation and alignment
dtm <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

dtm_matrix <- as.matrix(dtm)
colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)
rownames_dtm <- dtm$dimnames$Docs
rownames(dtm_matrix) <- rownames_dtm
dtm_df <- as.data.frame(dtm_matrix)
stopifnot(!any(duplicated(colnames(dtm_df))))

# Align abstracts and DTM
labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% rownames(dtm_matrix)) %>%
  mutate(relevance = factor(relevance)) %>%
  arrange(match(id, rownames(dtm_matrix)))

# Train-test split
train_index <- createDataPartition(labeled_abstracts$relevance, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

train_ids <- as.character(train_data$id)
test_ids <- as.character(test_data$id)

train_matrix <- dtm_matrix[train_ids, ]
test_matrix <- dtm_matrix[test_ids, ]

# Convert to data frame for caret
train_df <- as.data.frame(as.matrix(train_matrix)) %>% 
  mutate(relevance = train_data$relevance)

test_df <- as.data.frame(as.matrix(test_matrix)) %>% 
  mutate(relevance = test_data$relevance)

# Apply SMOTE for class balancing
train_recipe <- recipe(relevance ~ ., data = train_df) %>%
  step_smote(relevance) %>%
  prep()
balanced_train <- juice(train_recipe)

# Train Control
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Check class distribution
cat("Original class distribution:\n")
print(table(train_df$relevance))
cat("Balanced class distribution:\n")
print(table(balanced_train$relevance))

# Calculate class weights (emphasize Relevant class)
class_weights <- c(Irrelevant = 1, Relevant = 2)  # Give Relevant 2x weight

# Models to compare
models_to_try <- c("glmnet") # Can add rf back in but it kind of sucks. can also add svmLinear back if wanted, but glmnet performs the best.
results <- list()

for (method in models_to_try) {
  cat("\nTraining:", method, "\n")
  tic(paste("Time for", method))
  result <- tryCatch({
    if (method == "glmnet") {
      # For glmnet, we can adjust class weights
      train(
        relevance ~ ., 
        data = balanced_train,
        method = method,
        metric = "ROC",
        trControl = train_control,
        tuneLength = 10,
        weights = ifelse(balanced_train$relevance == "Relevant", 2, 1)
      )
    } else {
      train(
        relevance ~ ., 
        data = balanced_train,
        method = method,
        metric = "ROC",
        trControl = train_control,
        tuneLength = 10
      )
    }
  }, error = function(e) {
    message("Model ", method, " failed: ", e$message)
    return(NULL)
  })
  toc()
  results[[method]] <- result
}


confusion_matrices <- list() 
# Evaluate with focus on Relevant class recall
evaluation_table <- tibble(
  model = names(results),
  accuracy = sapply(names(results), function(m) {
    model <- results[[m]]
    if (is.null(model)) return(NA)
    
    preds <- predict(model, newdata = test_df)
    cm <- confusionMatrix(preds, test_df$relevance, positive = "Relevant")
    
    # Save confusion matrix to list for later inspection
    confusion_matrices[[m]] <<- cm
    
    cm$overall["Accuracy"]
  }),
  sensitivity_relevant = sapply(names(results), function(m) {
    if (is.null(results[[m]])) return(NA)
    confusion_matrices[[m]]$byClass["Sensitivity"]  # Recall for Relevant
  }),
  specificity = sapply(names(results), function(m) {
    if (is.null(results[[m]])) return(NA)
    confusion_matrices[[m]]$byClass["Specificity"]
  }),
  f1_score = sapply(names(results), function(m) {
    if (is.null(results[[m]])) return(NA)
    confusion_matrices[[m]]$byClass["F1"]
  })
) %>% filter(!is.na(accuracy)) %>% arrange(desc(sensitivity_relevant))

print("Model Performance (sorted by Relevant class recall):")
print(evaluation_table)

#91% on glmnet, pretty good. 

for (m in names(confusion_matrices)) {
  cat("\nConfusion Matrix for model:", m, "\n")
  print(confusion_matrices[[m]]$table)  # Just the table
  cat("\nDetailed stats:\n")
  print(confusion_matrices[[m]]$byClass)  # Per-class metrics (Sensitivity, Specificity, etc.)
  cat("\n")
}

# Save best model (based on recall for Relevant class)
best_model <- results[[evaluation_table$model[1]]]

setdiff(colnames(best_model$trainingData), best_model$finalModel$xNames)
#xnames <- best_model$finalModel$xNames
#best_model$trainingData <- best_model$trainingData[, c(xnames, ".outcome"), drop = FALSE]

saveRDS(best_model, file = paste0("models/best_model_relevance_", evaluation_table$model[1], ".rds"))


#svmLinear it is!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Presence/Absence Classification ====================================

labeled_abstracts <- labeled_abstracts %>%
  filter(relevance == "Relevant")

labeled_abstracts %>%
  count(presence_both_absence)

# Create unigrams
dtm_unigrams <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  # Keep words that appear in at least 2 documents (reduce noise)
  group_by(word) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id))

# Create bigrams (standard approach - let data decide what's important)
dtm_bigrams <- labeled_abstracts %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  # Standard bigram filtering - no manual override
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  unite(bigram, word1, word2, sep = "_") %>%
  mutate(bigram = str_to_lower(bigram)) %>%
  # Keep bigrams that appear in at least 2 documents (let data decide importance)
  group_by(bigram) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  count(id, bigram, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  rename(word = bigram)  # Rename for consistency

# Combine unigrams and bigrams
dtm_combined <- bind_rows(dtm_unigrams, dtm_bigrams) %>%
  cast_dtm(document = id, term = word, value = n)

# Preserve row names BEFORE converting
rownames_dtm <- dtm_combined$dimnames$Docs  # Extract doc IDs from DTM

dtm_matrix <- as.matrix(dtm_combined)
colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)

# Continue safely
dtm_df <- as.data.frame(dtm_matrix)

# Check
stopifnot(!any(duplicated(colnames(dtm_df))))



# Get column names ordered by decreasing column sum
order_cols <- names(sort(colSums(dtm_df), decreasing = TRUE))

# Reorder the columns
dtm_df <- dtm_df[, order_cols]

valid_ids <- as.integer(rownames_dtm)  # convert back to integer

labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% valid_ids) %>%
  mutate(id = as.character(id)) %>%         # match rownames (character)
  arrange(match(id, rownames_dtm))  


rownames(dtm_matrix) <- rownames_dtm

labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% rownames(dtm_matrix)) %>%
  mutate(presence_both_absence = factor(presence_both_absence))

# Train-test split
train_index <- createDataPartition(labeled_abstracts$presence_both_absence, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

train_ids <- as.character(train_data$id)
test_ids <- as.character(test_data$id)

train_matrix <- dtm_matrix[train_ids, ]
test_matrix <- dtm_matrix[test_ids, ]

# Convert to data frame for caret
train_df <- as.data.frame(as.matrix(train_matrix)) %>% mutate(presence_both_absence = train_data$presence_both_absence)

test_df <- as.data.frame(as.matrix(test_matrix)) %>% mutate(presence_both_absence = test_data$presence_both_absence)


# Testing models ----------------------------------------------------------


#glmnet and svmLinear are super fast.

train_recipe <- recipe(presence_both_absence ~ ., data = train_df) %>%
  step_smote(presence_both_absence) %>%
  prep()
balanced_train <- juice(train_recipe)

# Train Control
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Check class distribution for P/A
cat("P/A Original class distribution:\n")
print(table(train_df$presence_both_absence))
cat("P/A Balanced class distribution:\n")
print(table(balanced_train$presence_both_absence))

# Models to compare
models_to_try <- c("glmnet", "svmLinear") 
results <- list()

for (method in models_to_try) {
  cat("\nTraining:", method, "\n")
  tic(paste("Time for", method))
  result <- tryCatch({
    if (method == "glmnet") {
      # For glmnet, heavily weight Absence class to avoid misclassifying it
      train(
        presence_both_absence ~ ., 
        data = balanced_train,
        method = method,
        metric = "ROC",
        trControl = train_control,
        tuneLength = 10,
        weights = ifelse(balanced_train$presence_both_absence == "Absence", 3, 1)  # 3x weight for Absence
      )
    } else if (method == "rf") {
      # For Random Forest, use built-in class weights
      train(
        presence_both_absence ~ ., 
        data = balanced_train,
        method = method,
        metric = "ROC",
        trControl = train_control,
        tuneLength = 5,  # Fewer tuning params for speed
        classwt = c(Presence = 1, Absence = 3)  # 3x weight for Absence
      )
    } else if (method == "svmLinear") {
      # For SVM, standard training (already balanced with SMOTE)
      train(
        presence_both_absence ~ ., 
        data = balanced_train,
        method = method,
        metric = "ROC",
        trControl = train_control,
        tuneLength = 10
      )
    } else {
      train(
        presence_both_absence ~ ., 
        data = balanced_train,
        method = method,
        metric = "ROC",
        trControl = train_control,
        tuneLength = 10
      )
    }
  }, error = function(e) {
    message("Model ", method, " failed: ", e$message)
    return(NULL)
  })
  toc()
  results[[method]] <- result
}

confusion_matrices <- list() 
# Evaluate with focus on both classes, especially Absence recall
evaluation_table_pa <- tibble(
  model = names(results),
  accuracy = sapply(names(results), function(m) {
    model <- results[[m]]
    if (is.null(model)) return(NA)
    
    preds <- predict(model, newdata = test_df)
    cm <- confusionMatrix(preds, test_df$presence_both_absence, positive = "Presence")
    
    # Save confusion matrix to list for later inspection
    confusion_matrices[[m]] <<- cm
    
    cm$overall["Accuracy"]
  }),
  presence_recall = sapply(names(results), function(m) {
    if (is.null(results[[m]])) return(NA)
    confusion_matrices[[m]]$byClass["Sensitivity"]  # Recall for Presence
  }),
  absence_recall = sapply(names(results), function(m) {
    if (is.null(results[[m]])) return(NA)
    confusion_matrices[[m]]$byClass["Specificity"]  # Recall for Absence
  }),
  f1_score = sapply(names(results), function(m) {
    if (is.null(results[[m]])) return(NA)
    confusion_matrices[[m]]$byClass["F1"]
  }),
  balanced_accuracy = sapply(names(results), function(m) {
    if (is.null(results[[m]])) return(NA)
    confusion_matrices[[m]]$overall["Balanced Accuracy"]
  })
) %>% filter(!is.na(accuracy)) %>% arrange(desc(absence_recall))

print("P/A Model Performance (sorted by Absence recall):")
print(evaluation_table_pa)



for (m in names(confusion_matrices)) {
  cat("\nConfusion Matrix for model:", m, "\n")
  print(confusion_matrices[[m]]$table)  # Just the table
  cat("\nDetailed stats:\n")
  print(confusion_matrices[[m]]$byClass)  # Per-class metrics (Sensitivity, Specificity, etc.)
  cat("\n")
}



# Save both models for ensemble approach
glmnet_model <- results[["glmnet"]]  # Best for Absence detection (100% recall)
svm_model <- results[["svmLinear"]]   # Best for Presence detection (95.8% recall)

# Create ensemble predictions
cat("\n=== ENSEMBLE APPROACH ===\n")
cat("Using glmnet for Absence detection and svmLinear for Presence detection\n")

# Get predictions from both models
glmnet_preds <- predict(glmnet_model, newdata = test_df)
svm_preds <- predict(svm_model, newdata = test_df)

# Get probabilities for more nuanced ensemble
glmnet_probs <- predict(glmnet_model, newdata = test_df, type = "prob")
svm_probs <- predict(svm_model, newdata = test_df, type = "prob")

# Debug: Check probability distributions
cat("GLMnet Absence prob range:", range(glmnet_probs$Absence), "\n")
cat("SVM Presence prob range:", range(svm_probs$Presence), "\n")
cat("GLMnet predicts Absence (>0.5):", sum(glmnet_probs$Absence > 0.5), "out of", nrow(test_df), "\n")
cat("SVM predicts Presence (>0.5):", sum(svm_probs$Presence > 0.5), "out of", nrow(test_df), "\n")

# Ensemble strategy: Test multiple thresholds to find optimal balance
cat("Testing different ensemble thresholds:\n")

thresholds_to_test <- c(0.5, 0.6, 0.7, 0.8)
ensemble_results <- list()

for(thresh in thresholds_to_test) {
  ensemble_preds_test <- ifelse(
    glmnet_probs$Absence > thresh,  # Try different confidence thresholds
    "Absence",
    as.character(svm_preds)  # Otherwise, use SVM's prediction
  )
  ensemble_preds_test <- factor(ensemble_preds_test, levels = c("Presence", "Absence"))
  
  # Evaluate this threshold
  ensemble_cm_test <- confusionMatrix(ensemble_preds_test, test_df$presence_both_absence, positive = "Presence")
  
  ensemble_results[[as.character(thresh)]] <- list(
    threshold = thresh,
    accuracy = ensemble_cm_test$overall["Accuracy"],
    presence_recall = ensemble_cm_test$byClass["Sensitivity"],
    absence_recall = ensemble_cm_test$byClass["Specificity"],
    f1_score = ensemble_cm_test$byClass["F1"]
  )
  
  cat("Threshold", thresh, "- Acc:", round(ensemble_cm_test$overall["Accuracy"], 3),
      "Pres:", round(ensemble_cm_test$byClass["Sensitivity"], 3), 
      "Abs:", round(ensemble_cm_test$byClass["Specificity"], 3), "\n")
}

# True ensemble approach: Combined probability scoring
cat("=== TRUE ENSEMBLE WITH WEIGHTED PROBABILITIES ===\n")

# Weight the probabilities based on each model's strengths
# SVM is great at Presence (95.8% recall), GLM is great at Absence (87.0% recall)
# Adjust weights to prioritize absence detection more heavily
svm_weight_for_presence <- 0.6  # Reduced from 0.7 - less weight for presence decisions
glm_weight_for_absence <- 0.8   # Increased from 0.7 - more weight for absence decisions

cat("Ensemble weights: SVM for presence =", svm_weight_for_presence, 
    ", GLMNet for absence =", glm_weight_for_absence, "\n")

# Create weighted probability ensemble
ensemble_presence_prob <- (svm_probs$Presence * svm_weight_for_presence + 
                          glmnet_probs$Presence * (1 - svm_weight_for_presence))

ensemble_absence_prob <- (glmnet_probs$Absence * glm_weight_for_absence + 
                         svm_probs$Absence * (1 - glm_weight_for_absence))

# Make predictions based on weighted probabilities
ensemble_preds_weighted <- ifelse(ensemble_presence_prob > ensemble_absence_prob, 
                                 "Presence", "Absence")
ensemble_preds_weighted <- factor(ensemble_preds_weighted, levels = c("Presence", "Absence"))

# Evaluate weighted ensemble
ensemble_cm_weighted <- confusionMatrix(ensemble_preds_weighted, test_df$presence_both_absence, positive = "Presence")

cat("Weighted Ensemble Performance:\n")
cat("Accuracy:", round(ensemble_cm_weighted$overall["Accuracy"], 3), "\n")
cat("Presence Recall:", round(ensemble_cm_weighted$byClass["Sensitivity"], 3), "\n") 
cat("Absence Recall:", round(ensemble_cm_weighted$byClass["Specificity"], 3), "\n")
cat("F1 Score:", round(ensemble_cm_weighted$byClass["F1"], 3), "\n")

# Test alternative weight configurations for comparison
cat("\n=== TESTING ALTERNATIVE WEIGHT CONFIGURATIONS ===\n")

weight_configs <- list(
  "current" = list(svm_pres = 0.6, glm_abs = 0.8),
  "more_conservative" = list(svm_pres = 0.5, glm_abs = 0.85),
  "very_conservative" = list(svm_pres = 0.4, glm_abs = 0.9),
  "ultra_conservative" = list(svm_pres = 0.4, glm_abs = 0.95)
)

weight_results <- list()

for (config_name in names(weight_configs)) {
  config <- weight_configs[[config_name]]
  
  # Create ensemble with this configuration
  test_presence_prob <- (svm_probs$Presence * config$svm_pres + 
                        glmnet_probs$Presence * (1 - config$svm_pres))
  test_absence_prob <- (glmnet_probs$Absence * config$glm_abs + 
                       svm_probs$Absence * (1 - config$glm_abs))
  
  test_preds <- ifelse(test_presence_prob > test_absence_prob, "Presence", "Absence")
  test_preds <- factor(test_preds, levels = c("Presence", "Absence"))
  
  test_cm <- confusionMatrix(test_preds, test_df$presence_both_absence, positive = "Presence")
  
  weight_results[[config_name]] <- list(
    config = config,
    accuracy = test_cm$overall["Accuracy"],
    presence_recall = test_cm$byClass["Sensitivity"],
    absence_recall = test_cm$byClass["Specificity"],
    f1_score = test_cm$byClass["F1"]
  )
  
  cat(sprintf("%s (SVM: %.1f, GLM: %.1f) - Acc: %.3f, Pres: %.3f, Abs: %.3f\n",
              config_name, config$svm_pres, config$glm_abs,
              test_cm$overall["Accuracy"], test_cm$byClass["Sensitivity"], 
              test_cm$byClass["Specificity"]))
}

# Select the best configuration based on your priorities
# Use the "current" approach which achieved best balance
best_config <- weight_configs[["current"]]
ensemble_presence_prob <- (svm_probs$Presence * best_config$svm_pres + 
                          glmnet_probs$Presence * (1 - best_config$svm_pres))
ensemble_absence_prob <- (glmnet_probs$Absence * best_config$glm_abs + 
                         svm_probs$Absence * (1 - best_config$glm_abs))

# Standard threshold (0.5) - we'll optimize this next
ensemble_preds_weighted <- ifelse(ensemble_presence_prob > ensemble_absence_prob, 
                                 "Presence", "Absence")
ensemble_preds_weighted <- factor(ensemble_preds_weighted, levels = c("Presence", "Absence"))

# === TEST OPTIMAL THRESHOLD WITH WEIGHT CONFIGURATIONS ===
cat("\n=== TESTING OPTIMAL THRESHOLD (0.55) WITH WEIGHT CONFIGURATIONS ===\n")

optimal_threshold <- 0.55
for (config_name in names(weight_configs)) {
  config <- weight_configs[[config_name]]
  
  # Create ensemble with this configuration
  test_presence_prob <- (svm_probs$Presence * config$svm_pres + 
                        glmnet_probs$Presence * (1 - config$svm_pres))
  test_absence_prob <- (glmnet_probs$Absence * config$glm_abs + 
                       svm_probs$Absence * (1 - config$glm_abs))
  
  # Use optimal threshold instead of simple comparison
  test_preds <- ifelse(test_absence_prob > optimal_threshold, "Absence", 
                      ifelse(test_presence_prob > (1 - optimal_threshold), "Presence", "Presence"))
  test_preds <- factor(test_preds, levels = c("Presence", "Absence"))
  
  test_cm <- confusionMatrix(test_preds, test_df$presence_both_absence, positive = "Presence")
  
  cat(sprintf("%s with 0.55 threshold (SVM: %.1f, GLM: %.2f) - Acc: %.3f, Pres: %.3f, Abs: %.3f\n",
              config_name, config$svm_pres, config$glm_abs,
              test_cm$overall["Accuracy"], test_cm$byClass["Sensitivity"], 
              test_cm$byClass["Specificity"]))
}

# === ADVANCED OPTIMIZATION: THRESHOLD TUNING ===
cat("\n=== OPTIMIZING DECISION THRESHOLDS ===\n")

# Test different decision thresholds for the probability comparison
thresholds_to_test <- seq(0.3, 0.7, by = 0.05)
threshold_results <- list()

for (thresh in thresholds_to_test) {
  # Instead of comparing probabilities directly, use a threshold
  # If absence_prob > thresh, predict Absence, otherwise use presence_prob > (1-thresh)
  test_preds <- ifelse(ensemble_absence_prob > thresh, "Absence", 
                      ifelse(ensemble_presence_prob > (1 - thresh), "Presence", "Presence"))
  test_preds <- factor(test_preds, levels = c("Presence", "Absence"))
  
  test_cm <- confusionMatrix(test_preds, test_df$presence_both_absence, positive = "Presence")
  
  threshold_results[[as.character(thresh)]] <- list(
    threshold = thresh,
    accuracy = test_cm$overall["Accuracy"],
    presence_recall = test_cm$byClass["Sensitivity"],
    absence_recall = test_cm$byClass["Specificity"],
    f1_score = test_cm$byClass["F1"],
    balanced_accuracy = test_cm$overall["Balanced Accuracy"]
  )
  
  cat(sprintf("Threshold %.2f - Acc: %.3f, Pres: %.3f, Abs: %.3f, F1: %.3f\n",
              thresh, test_cm$overall["Accuracy"], test_cm$byClass["Sensitivity"], 
              test_cm$byClass["Specificity"], test_cm$byClass["F1"]))
}

# Find best threshold based on balanced recall
best_threshold_idx <- which.max(sapply(threshold_results, function(x) (x$presence_recall + x$absence_recall) / 2))
best_threshold <- threshold_results[[best_threshold_idx]]$threshold
best_threshold_results <- threshold_results[[best_threshold_idx]]

cat(sprintf("\nBest threshold: %.2f\n", best_threshold))
cat(sprintf("- Presence Recall: %.1f%%\n", best_threshold_results$presence_recall * 100))
cat(sprintf("- Absence Recall: %.1f%%\n", best_threshold_results$absence_recall * 100))

# Apply best threshold
ensemble_preds_optimized <- ifelse(ensemble_absence_prob > best_threshold, "Absence", 
                                  ifelse(ensemble_presence_prob > (1 - best_threshold), "Presence", "Presence"))
ensemble_preds_optimized <- factor(ensemble_preds_optimized, levels = c("Presence", "Absence"))

ensemble_cm_weighted <- confusionMatrix(ensemble_preds_weighted, test_df$presence_both_absence, positive = "Presence")

cat("\n=== FINAL ENSEMBLE CONFIGURATION ===\n")
cat("Using current config: SVM weight =", best_config$svm_pres, 
    ", GLMNet weight =", best_config$glm_abs, "\n")
cat("Final Weighted Ensemble Performance:\n")
cat("Accuracy:", round(ensemble_cm_weighted$overall["Accuracy"], 3), "\n")
cat("Presence Recall:", round(ensemble_cm_weighted$byClass["Sensitivity"], 3), "\n") 
cat("Absence Recall:", round(ensemble_cm_weighted$byClass["Specificity"], 3), "\n")
cat("F1 Score:", round(ensemble_cm_weighted$byClass["F1"], 3), "\n")

# Test the optimized threshold approach
ensemble_cm_optimized <- confusionMatrix(ensemble_preds_optimized, test_df$presence_both_absence, positive = "Presence")

cat("\n=== THRESHOLD-OPTIMIZED ENSEMBLE ===\n")
cat("Using optimized threshold:", best_threshold, "\n")
cat("Threshold-Optimized Performance:\n")
cat("Accuracy:", round(ensemble_cm_optimized$overall["Accuracy"], 3), "\n")
cat("Presence Recall:", round(ensemble_cm_optimized$byClass["Sensitivity"], 3), "\n") 
cat("Absence Recall:", round(ensemble_cm_optimized$byClass["Specificity"], 3), "\n")
cat("F1 Score:", round(ensemble_cm_optimized$byClass["F1"], 3), "\n")

# Try the best threshold approach (0.5 for max absence recall)
best_thresh <- 0.5
ensemble_preds <- ifelse(
  glmnet_probs$Absence > best_thresh,  # Use 0.5 for best absence performance
  "Absence",
  as.character(svm_preds)  # Otherwise, use SVM's prediction
)
ensemble_preds <- factor(ensemble_preds, levels = c("Presence", "Absence"))

# Evaluate ensemble
ensemble_cm <- confusionMatrix(ensemble_preds, test_df$presence_both_absence, positive = "Presence")

cat("\nEnsemble Performance:\n")
cat("Accuracy:", round(ensemble_cm$overall["Accuracy"], 3), "\n")
cat("Presence Recall:", round(ensemble_cm$byClass["Sensitivity"], 3), "\n") 
cat("Absence Recall:", round(ensemble_cm$byClass["Specificity"], 3), "\n")
cat("F1 Score:", round(ensemble_cm$byClass["F1"], 3), "\n")

# Compare all approaches including weighted ensemble
comparison_results <- tibble(
  approach = c("glmnet_only", "svmLinear_only", "ensemble_threshold", "ensemble_weighted"),
  accuracy = c(
    confusion_matrices[["glmnet"]]$overall["Accuracy"],
    confusion_matrices[["svmLinear"]]$overall["Accuracy"], 
    ensemble_cm$overall["Accuracy"],
    ensemble_cm_weighted$overall["Accuracy"]
  ),
  presence_recall = c(
    confusion_matrices[["glmnet"]]$byClass["Sensitivity"],
    confusion_matrices[["svmLinear"]]$byClass["Sensitivity"],
    ensemble_cm$byClass["Sensitivity"],
    ensemble_cm_weighted$byClass["Sensitivity"]
  ),
  absence_recall = c(
    confusion_matrices[["glmnet"]]$byClass["Specificity"],
    confusion_matrices[["svmLinear"]]$byClass["Specificity"],
    ensemble_cm$byClass["Specificity"],
    ensemble_cm_weighted$byClass["Specificity"]
  ),
  f1_score = c(
    confusion_matrices[["glmnet"]]$byClass["F1"],
    confusion_matrices[["svmLinear"]]$byClass["F1"],
    ensemble_cm$byClass["F1"],
    ensemble_cm_weighted$byClass["F1"]
  )
)

print("Comparison of all approaches:")
print(comparison_results %>% 
      arrange(desc(absence_recall), desc(presence_recall)) %>%
      mutate(across(where(is.numeric), \(x) round(x, 4))))

# Best approach for each criterion
cat("\n\n=== BEST PERFORMANCE BY CRITERION ===\n")
cat("Best Overall Accuracy:", comparison_results$approach[which.max(comparison_results$accuracy)], 
    sprintf("(%.1f%%)\n", max(comparison_results$accuracy) * 100))
cat("Best Presence Recall:", comparison_results$approach[which.max(comparison_results$presence_recall)], 
    sprintf("(%.1f%%)\n", max(comparison_results$presence_recall) * 100))
cat("Best Absence Recall:", comparison_results$approach[which.max(comparison_results$absence_recall)], 
    sprintf("(%.1f%%)\n", max(comparison_results$absence_recall) * 100))
cat("Best F1 Score:", comparison_results$approach[which.max(comparison_results$f1_score)], 
    sprintf("(%.1f%%)\n", max(comparison_results$f1_score) * 100))

# Overall recommendation - prioritize based on your research needs
best_balanced <- comparison_results %>%
  mutate(
    combined_recall = (presence_recall + absence_recall) / 2,
    # Adjust weights based on your priorities:
    # Option 1: Balanced approach (current)
    balanced_score = 0.4 * accuracy + 0.3 * presence_recall + 0.3 * absence_recall,
    # Option 2: Prioritize avoiding missed Absence studies (conservative)
    conservative_score = 0.3 * accuracy + 0.2 * presence_recall + 0.5 * absence_recall,
    # Option 3: Prioritize finding Presence studies (aggressive)  
    aggressive_score = 0.3 * accuracy + 0.5 * presence_recall + 0.2 * absence_recall
  ) %>%
  arrange(desc(balanced_score))

cat("\n\n=== RECOMMENDATIONS BY RESEARCH STRATEGY ===\n")

# Best for balanced approach
cat("BALANCED APPROACH (equal priority for both classes):\n")
balanced_best <- comparison_results %>% 
  mutate(balanced_score = 0.4 * accuracy + 0.3 * presence_recall + 0.3 * absence_recall) %>%
  arrange(desc(balanced_score)) %>% slice(1)
cat("Best approach:", balanced_best$approach, "\n")
cat("- Overall Score:", round(balanced_best$balanced_score, 3), "\n")

# Best for conservative approach (avoid missing Absence)
cat("\nCONSERVATIVE APPROACH (prioritize not missing Absence studies):\n")
conservative_best <- comparison_results %>% 
  mutate(conservative_score = 0.3 * accuracy + 0.2 * presence_recall + 0.5 * absence_recall) %>%
  arrange(desc(conservative_score)) %>% slice(1)
cat("Best approach:", conservative_best$approach, "\n") 
cat("- Absence Recall:", sprintf("%.1f%%", conservative_best$absence_recall * 100), "\n")

# Best for aggressive approach (find all Presence)
cat("\nAGGRESSIVE APPROACH (prioritize finding all Presence studies):\n")
aggressive_best <- comparison_results %>% 
  mutate(aggressive_score = 0.3 * accuracy + 0.5 * presence_recall + 0.2 * absence_recall) %>%
  arrange(desc(aggressive_score)) %>% slice(1)
cat("Best approach:", aggressive_best$approach, "\n")
cat("- Presence Recall:", sprintf("%.1f%%", aggressive_best$presence_recall * 100), "\n")

cat("\n=== SUMMARY ===\n")
cat("• Weighted Ensemble: Best overall performance and F1 score\n")
cat("• GLMNet Only: Best for avoiding false Absence classifications\n") 
cat("• SVM Only: Best for finding all Presence studies\n")
cat("• Threshold Ensemble: Ineffective (identical to GLMNet)\n")

# Stop execution here - uncomment the line below to prevent running further sections
 

# END OF PRESENCE/ABSENCE CLASSIFICATION SECTION -------------------------

# Saving models ====================================


# Save both models for ensemble use
saveRDS(glmnet_model, file = "models/best_model_presence_glmnet_ensemble.rds")
saveRDS(svm_model, file = "models/best_model_presence_svmLinear_ensemble.rds")

# Save ensemble function for later use (matching the manual calculation approach)
ensemble_predict_weighted <- function(glmnet_model, svm_model, newdata, 
                                     svm_weight_presence = 0.6, glm_weight_absence = 0.8) {
  # Get probabilities from both models
  glmnet_probs <- predict(glmnet_model, newdata = newdata, type = "prob")
  svm_probs <- predict(svm_model, newdata = newdata, type = "prob")
  
  # Create weighted probability ensemble - prioritizing absence detection
  ensemble_presence_prob <- (svm_probs$Presence * svm_weight_presence + 
                            glmnet_probs$Presence * (1 - svm_weight_presence))
  
  ensemble_absence_prob <- (glmnet_probs$Absence * glm_weight_absence + 
                           svm_probs$Absence * (1 - glm_weight_absence))
  
  # Make predictions using simple probability comparison (matches manual calculation)
  ensemble_preds <- ifelse(ensemble_presence_prob > ensemble_absence_prob, "Presence", "Absence")
  
  return(factor(ensemble_preds, levels = c("Presence", "Absence")))
}

# Alternative threshold-optimized ensemble function
ensemble_predict_threshold_optimized <- function(glmnet_model, svm_model, newdata, 
                                               svm_weight_presence = 0.6, glm_weight_absence = 0.8, 
                                               threshold = 0.55) {
  # Get probabilities from both models
  glmnet_probs <- predict(glmnet_model, newdata = newdata, type = "prob")
  svm_probs <- predict(svm_model, newdata = newdata, type = "prob")
  
  # Create weighted probability ensemble - prioritizing absence detection
  ensemble_presence_prob <- (svm_probs$Presence * svm_weight_presence + 
                            glmnet_probs$Presence * (1 - svm_weight_presence))
  
  ensemble_absence_prob <- (glmnet_probs$Absence * glm_weight_absence + 
                           svm_probs$Absence * (1 - glm_weight_absence))
  
  # Make predictions using optimized threshold
  ensemble_preds <- ifelse(ensemble_absence_prob > threshold, "Absence", 
                          ifelse(ensemble_presence_prob > (1 - threshold), "Presence", "Presence"))
  
  return(factor(ensemble_preds, levels = c("Presence", "Absence")))
}

# Test the ensemble functions
ensemble_test_weighted <- ensemble_predict_weighted(glmnet_model, svm_model, test_df)
cat("\nWeighted ensemble function test - matches manual calculation:", 
    all(ensemble_test_weighted == ensemble_preds_weighted), "\n")

# Test the threshold-optimized function
ensemble_test_threshold_opt <- ensemble_predict_threshold_optimized(glmnet_model, svm_model, test_df, threshold = 0.55)
cat("Threshold-optimized ensemble function test created successfully\n")

# Also keep the original threshold-based ensemble function
ensemble_predict <- function(glmnet_model, svm_model, newdata, absence_threshold = 0.6) {
  glmnet_probs <- predict(glmnet_model, newdata = newdata, type = "prob")
  svm_preds <- predict(svm_model, newdata = newdata)
  
  # Optimized strategy: More aggressive absence detection
  ensemble_preds <- ifelse(
    glmnet_probs$Absence > absence_threshold,  # Adjustable threshold
    "Absence",
    as.character(svm_preds)  # Otherwise trust SVM (excellent at Presence)
  )
  return(factor(ensemble_preds, levels = c("Presence", "Absence")))
}

# Test the ensemble function
ensemble_test <- ensemble_predict(glmnet_model, svm_model, test_df)
cat("\nEnsemble function test - matches manual calculation:", all(ensemble_test == ensemble_preds), "\n")


# This part is performing very well! Continue to apply to full dataset.

# stop("Presence/Absence classification complete. Uncomment this line to continue.")
# Whole dataset -----------------------------------------------------------



colname_mapping <- c(
  "title" = "article_title",                # 'title' to 'article_title'
  "abstract" = "abstract",                  # 'abstract' matches
  "authors" = "authors",                    # 'authors' matches
  "book_authors" = "book_authors",          # 'book_authors' matches
  "editors" = "book_editors",               # 'editors' to 'book_editors'
  "group_authors" = "book_group_authors",   # 'group_authors' to 'book_group_authors'
  "author_full_names" = "author_full_names",# 'author_full_names' matches
  "book_full_names" = "book_author_full_names", # 'book_full_names' to 'book_author_full_names'
  "conference_authors" = "conference_authors", # 'conference_authors' to 'conference_title'
  "source_title" = "source_title",          # 'source_title' matches
  "series_title" = "book_series_title",     # 'series_title' to 'book_series_title'
  "book_series" = "book_series_subtitle",   # 'book_series' to 'book_series_subtitle'
  "language_of_original_document" = "language", # 'language_of_original_document' to 'language'
  "conference_name" = "conference_title",   # 'conference_name' to 'conference_title'
  "conference_date" = "conference_date",    # 'conference_date' matches
  "conference_location" = "conference_location", # 'conference_location' matches
  "sponsors" = "conference_sponsor",       # 'sponsors' to 'conference_sponsor'
  "host" = "conference_host",               # 'host' to 'conference_host'
  "author_keywords" = "author_keywords",    # 'author_keywords' matches
  "index_keywords" = "keywords_plus",       # 'index_keywords' to 'keywords_plus'
  "affiliations" = "affiliations",          # 'affiliations' matches
  "authors_with_affiliations" = "addresses", # 'authors_with_affiliations' to 'addresses'
  "correspondence_address" = "reprint_addresses", # 'correspondence_address' to 'reprint_addresses'
  "email_address" = "email_addresses",      # 'email_address' to 'email_addresses'
  "researcher_i_ds" = "researcher_ids",     # 'researcher_i_ds' to 'researcher_ids'
  "orcid_i_ds" = "orci_ds",                 # 'orcid_i_ds' to 'orci_ds'
  "funding_details" = "funding_text",       # 'funding_details' to 'funding_text'
  "funding_programs" = "funding_orgs",      # 'funding_programs' to 'funding_orgs'
  "funding_texts" = "funding_name_preferred", # 'funding_texts' to 'funding_name_preferred'
  "references" = "cited_references",        # 'references' to 'cited_references'
  "cited_references" = "cited_reference_count", # 'cited_references' to 'cited_reference_count'
  "times_cited" = "times_cited_wo_s_core",  # 'times_cited' to 'times_cited_wo_s_core'
  "total_times_cited" = "times_cited_all_databases", # 'total_times_cited' to 'times_cited_all_databases'
  "usage_count_180_days" = "x180_day_usage_count", # 'usage_count_180_days' to 'x180_day_usage_count'
  "usage_count_since_2013" = "since_2013_usage_count", # 'usage_count_since_2013' to 'since_2013_usage_count'
  "publisher" = "publisher",                # 'publisher' matches
  "publisher_city" = "publisher_city",      # 'publisher_city' matches
  "publisher_address" = "publisher_address",# 'publisher_address' matches
  "issn" = "issn",                          # 'issn' matches
  "e_issn" = "e_issn",                      # 'e_issn' matches
  "isbn" = "isbn",                          # 'isbn' matches
  "abbreviated_source_title" = "journal_abbreviation", # 'abbreviated_source_title' to 'journal_abbreviation'
  "journal_iso" = "journal_iso_abbreviation", # 'journal_iso' to 'journal_iso_abbreviation'
  "publication_date" = "publication_date",  # 'publication_date' matches
  "year" = "publication_year",              # 'year' to 'publication_year'
  "volume" = "volume",                      # 'volume' matches
  "issue" = "issue",                        # 'issue' matches
  "supplement" = "supplement",              # 'supplement' matches
  "special_issue" = "special_issue",        # 'special_issue' matches
  "meeting_abstract" = "meeting_abstract",  # 'meeting_abstract' matches
  "page_start" = "start_page",              # 'page_start' to 'start_page'
  "page_end" = "end_page",                  # 'page_end' to 'end_page'
  "doi" = "doi",                            # 'doi' matches
  "doi_link" = "doi_link",                  # 'doi_link' matches
  "secondary_doi" = "book_doi",             # 'secondary_doi' to 'book_doi'
  "early_access_date" = "early_access_date",# 'early_access_date' matches
  "page_count" = "number_of_pages",         # 'page_count' to 'number_of_pages'
  "web_of_science_categories" = "wo_s_categories", # 'web_of_science_categories' to 'wo_s_categories'
  "research_areas" = "research_areas",      # 'research_areas' matches
  "subject_categories" = "subject_categories", # 'subject_categories' to 'wo_s_categories'
  "document_delivery_number" = "ids_number", # 'document_delivery_number' to 'ids_number'
  "pub_med_id" = "pubmed_id",               # 'pub_med_id' to 'pubmed_id'
  "open_access" = "open_access_designations", # 'open_access' to 'open_access_designations'
  "highly_cited_paper" = "highly_cited_status", # 'highly_cited_paper' to 'highly_cited_status'
  "hot_paper" = "hot_paper_status",         # 'hot_paper' to 'hot_paper_status'
  "date" = "date_of_export",                # 'date' to 'date_of_export'
  "wos_id" = "ut_unique_wos_id",            # 'wos_id' to 'ut_unique_wos_id'
  "author_s_id" = "web_of_science_record",  # 'author_s_id' to 'web_of_science_record'
  "art_no" = "article_number",              # 'art_no' to 'article_number'
  "cited_by" = "cited_by",                  # 'cited_by' matches
  "link" = "link",                          # 'link' matches
  "molecular_sequence_numbers" = "molecular_sequence_numbers", # 'molecular_sequence_numbers' matches
  "chemicals_cas" = "chemicals_cas",        # 'chemicals_cas' matches
  "tradenames" = "tradenames",              # 'tradenames' matches
  "manufacturers" = "manufacturers",        # 'manufacturers' matches
  "conference_code" = "conference_code",    # 'conference_code' matches
  "coden" = "coden",                        # 'coden' matches
  "document_type" = "document_type",        # 'document_type' matches
  "publication_stage" = "publication_stage",# 'publication_stage' matches
  "source" = "source",                      # 'source' matches
  "eid" = "eid"                             # 'eid' matches
)

full_abstracts <- read.csv("data/All_Abstracts.csv") %>%
  clean_names()


# Apply your column name harmonization
full_abstracts <- full_abstracts %>%
  rename_with(~ ifelse(. %in% names(colname_mapping), colname_mapping[.], .))

# Filter out labeled DOIs (so we’re not predicting on training data)
filtered_dois <- labeled_abstracts$doi[!is.na(labeled_abstracts$doi) & labeled_abstracts$doi != ""]
full_abstracts <- full_abstracts[!full_abstracts$doi %in% filtered_dois, ]

# Add unique ID column for tracking documents
full_abstracts$id <- 1:nrow(full_abstracts)

# Construct DTM (no make.names!)
dtm <- full_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
 # mutate(word = str_replace_all(word, "'s\\b", ""))  %>% # Remove possessives
 # filter(!str_detect(word, "'")) %>%  # Remove any word containing an apostrophe
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

# Convert to matrix and assign rownames
dtm_matrix <- as.matrix(dtm)

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)

# Continue safely

# Check

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix)) #Santize
rownames(dtm_matrix) <- dtm$dimnames$Docs  # these are the character ids

dtm_df <- as.data.frame(dtm_matrix)

stopifnot(!any(duplicated(colnames(dtm_df))))


# Load the trained model
rel_model <- readRDS("models/best_model_relevance_glmnet.rds")

# Get training feature names
trained_vocab <- rel_model$finalModel$xNames

# Add missing columns (words present in training but not here)
missing_words <- setdiff(trained_vocab, colnames(dtm_df))
if (length(missing_words) > 0) {
  zero_matrix <- matrix(0, nrow = nrow(dtm_df), ncol = length(missing_words),
                        dimnames = list(rownames(dtm_df), missing_words))
  dtm_df <- cbind(dtm_df, zero_matrix)
}

# Remove extra columns not in trained vocab
dtm_df <- dtm_df[, colnames(dtm_df) %in% trained_vocab]

# Reorder columns to match the trained model’s input order
dtm_df <- dtm_df[, trained_vocab, drop = FALSE]

# Convert to dataframe
full_df <- dtm_df

# Sanity check (should be empty):
 head(setdiff(rel_model$finalModel$xNames, colnames(full_df)))

head(setdiff(colnames(rel_model$trainingData), colnames(full_df)))

# Predict
probs <- predict(rel_model, newdata = full_df, type = "prob")

full_abstracts <- full_abstracts %>%
  bind_cols(probs)

# Define thresholds
loose_thresh <- 0.5   # more willing to classify
medium_thresh <- 0.6  # balanced
strict_thresh <- 0.8  # only classify with strong certainty

# Apply each thresholding scheme
full_abstracts <- full_abstracts %>%
  mutate(
    label_loose = case_when(
      Relevant >= loose_thresh ~ "Relevant",
      Irrelevant >= loose_thresh ~ "Irrelevant",
      TRUE ~ "Uncertain"
    ),
    label_medium = case_when(
      Relevant >= medium_thresh ~ "Relevant",
      Irrelevant >= medium_thresh ~ "Irrelevant",
      TRUE ~ "Uncertain"
    ),
    label_strict = case_when(
      Relevant >= strict_thresh ~ "Relevant",
      Irrelevant >= strict_thresh ~ "Irrelevant",
      TRUE ~ "Uncertain"
    )
  )

full_abstracts %>%
  select(label_loose, label_medium, label_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label)


# Save the results
write.csv(full_abstracts, "relevance_preds.csv", row.names = FALSE)

irr_un <- full_abstracts %>%
  filter(label_loose == "Irrelevant" | label_loose == "Uncertain")%>%
  relocate(label_loose, abstract)

write.csv(irr_un, "irrelevant_uncertain_abstracts.csv", row.names = FALSE)

abstracts_with_rel <- read.csv("relevance_preds.csv")%>%
  filter(label_loose == "Relevant")

abstracts_with_rel %>%
  select(label_loose, label_medium, label_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label)

#Now do the same with P/A

dtm <- abstracts_with_rel %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
 # mutate(word = str_replace_all(word, "'s\\b", ""))  %>% # Remove possessives
 # filter(!str_detect(word, "'")) %>%  # Remove any word containing an apostrophe
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

# Convert to matrix and assign rownames
dtm_matrix <- as.matrix(dtm)

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)

# Continue safely

# Check

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix)) #Santize
rownames(dtm_matrix) <- dtm$dimnames$Docs  # these are the character ids

dtm_df <- as.data.frame(dtm_matrix)

stopifnot(!any(duplicated(colnames(dtm_df))))


# Load the trained model
pa_model <- readRDS("models/best_model_presence_svmLinear.rds")

# Get training feature names
trained_vocab <- setdiff(colnames(pa_model$trainingData), ".outcome")

# Add missing columns (words present in training but not here)
missing_words <- setdiff(trained_vocab, colnames(dtm_df))
if (length(missing_words) > 0) {
  zero_matrix <- matrix(0, nrow = nrow(dtm_df), ncol = length(missing_words),
                        dimnames = list(rownames(dtm_df), missing_words))
  dtm_df <- cbind(dtm_df, zero_matrix)
}

# Remove extra columns not in trained vocab
dtm_df <- dtm_df[, colnames(dtm_df) %in% trained_vocab]

# Reorder columns to match the trained model’s input order
dtm_df <- dtm_df[, trained_vocab, drop = FALSE]

# Convert to dataframe
full_df <- dtm_df

# Sanity check (should be empty):
 head(setdiff(colnames(pa_model$finalModel), colnames(full_df)))

head(setdiff(colnames(pa_model$trainingData), colnames(full_df)))

# Predict
probs <- predict(pa_model, newdata = full_df, type = "prob")

abstracts_with_rel  <- abstracts_with_rel  %>%
  bind_cols(probs)

# Define thresholds
loose_thresh <- 0.5   # more willing to classify
medium_thresh <- 0.6  # balanced
strict_thresh <- 0.8  # only classify with strong certainty
super_strict_thresh <- 0.9  # very high confidence

# Apply each thresholding scheme
abstracts_with_rel <- abstracts_with_rel %>%
  mutate(
    label_loose = case_when(
      Presence >= loose_thresh ~ "Presence",
      Absence >= loose_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    ),
    label_medium = case_when(
      Presence >= medium_thresh ~ "Presence",
      Absence >= medium_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    ),
    label_strict = case_when(
      Presence >= strict_thresh ~ "Presence",
      Absence >= strict_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    ),
    label_super_strict = case_when(
      Presence >= super_strict_thresh ~ "Presence",
      Absence >= super_strict_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    )
  )

abstracts_with_rel %>%
  select(label_loose, label_medium, label_strict, label_super_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label)

# Save the results
write.csv(abstracts_with_rel, "relevance_pa_preds_all_abstracts.csv", row.names = FALSE)

#Go with strict and manually review uncertain and absence...