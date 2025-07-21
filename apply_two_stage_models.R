# Apply Two-Stage Models to All Abstracts Dataset - Version 2
# Date: 7/21/2025
# Purpose: Apply trained relevance and presence/absence models to the complete All_abstracts.csv dataset

library(tidyverse)
library(tidytext)
library(caret)
library(Matrix)
library(text)
library(tm)
library(recipes)
library(janitor)
library(tictoc)

# Read the complete abstracts dataset ----------------------------------------
cat("Loading All_abstracts.csv...\n")
all_abstracts <- read.csv("All_abstracts.csv") %>%
  clean_names() %>%
  filter(!is.na(abstract), nchar(abstract) > 10) %>%  # Filter out empty or very short abstracts
  mutate(id = row_number())

cat("Total abstracts loaded:", nrow(all_abstracts), "\n")

# Load the trained models ----------------------------------------------------
cat("Loading trained models...\n")

# Try to load the best models based on the naming convention from ML_compare_models
# Stage 1: Relevance Classification
relevance_model_files <- list.files(pattern = "best_model.*relevance.*\\.rds$", full.names = TRUE)
if (length(relevance_model_files) == 0) {
  stop("No relevance classification model found. Please run ML_compare_models_subset_optimized.R first.")
}

# Use the first available relevance model (or you can specify which one to use)
relevance_model_path <- relevance_model_files[1]
cat("Loading relevance model:", basename(relevance_model_path), "\n")
relevance_model <- readRDS(relevance_model_path)

# Stage 2: Presence vs Absence Classification
presence_model_files <- list.files(pattern = "best_model.*presence.*\\.rds$", full.names = TRUE)
if (length(presence_model_files) == 0) {
  stop("No presence/absence classification model found. Please run ML_compare_models_subset_optimized.R first.")
}

# Stage 2: Presence vs Absence Classification
presence_model_files <- list.files(pattern = "best_model.*presence.*\\.rds$", full.names = TRUE)
if (length(presence_model_files) == 0) {
  stop("No presence/absence classification model found. Please run ML_compare_models_subset_optimized.R first.")
}

# List all available presence/absence models
cat("Available presence/absence models:\n")
for (i in seq_along(presence_model_files)) {
  cat(i, ":", basename(presence_model_files[i]), "\n")
}

# Option to manually select a different model (comment/uncomment as needed)
# model_choice <- 1  # Change this number to select a different model
# presence_model_path <- presence_model_files[model_choice]

presence_model_path <- presence_model_files[1]
cat("Loading presence/absence model:", basename(presence_model_path), "\n")
presence_model <- readRDS(presence_model_path)

# Verify this is a presence/absence model by checking the outcome levels
if (!is.null(presence_model$trainingData) && ".outcome" %in% colnames(presence_model$trainingData)) {
  outcome_levels <- levels(presence_model$trainingData$.outcome)
  cat("Model outcome levels:", paste(outcome_levels, collapse = ", "), "\n")
  if (!all(c("Presence", "Absence") %in% outcome_levels)) {
    warning("This model doesn't seem to be trained for Presence/Absence classification!")
  }
}

# Load training data to get feature vocabulary -------------------------------
cat("Loading training data to extract feature vocabulary...\n")
training_data <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(label %in% c("Presence", "Absence", "Review", "Other")) %>%
  mutate(id = row_number())

# Function to extract feature names from trained model
extract_model_features <- function(model) {
  if (!is.null(model$trainingData)) {
    # Remove the .outcome column
    feature_names <- colnames(model$trainingData)
    feature_names <- feature_names[feature_names != ".outcome"]
    return(feature_names)
  } else if (!is.null(model$terms)) {
    return(attr(model$terms, "term.labels"))
  } else if (!is.null(model$finalModel$xNames)) {
    return(model$finalModel$xNames)
  } else {
    stop("Cannot extract feature names from model")
  }
}

# Get feature vocabulary from the relevance model
relevance_features <- extract_model_features(relevance_model)
cat("Relevance model features:", length(relevance_features), "\n")

# Debug: Check what type of models we're working with
cat("Relevance model class:", class(relevance_model), "\n")
cat("Relevance model method:", relevance_model$method, "\n")

# Check if model has training data info
if (!is.null(relevance_model$trainingData)) {
  cat("Relevance model training data dimensions:", dim(relevance_model$trainingData), "\n")
  cat("Sample features:", head(colnames(relevance_model$trainingData), 10), "\n")
}

# Recreate the same text processing pipeline as in training ------------------
cat("Processing text features...\n")

# Function to create DTM with same features as training
create_dtm_from_abstracts <- function(abstracts_df, feature_vocabulary, recalculate_tfidf = TRUE) {
  cat("Processing abstracts with", length(feature_vocabulary), "features...\n")
  
  # Debug: Check input abstracts
  cat("Input abstracts:", nrow(abstracts_df), "\n")
  cat("Sample abstract length:", mean(nchar(abstracts_df$abstract), na.rm = TRUE), "characters\n")
  
  # Tokenize new data for unigrams
  dtm_unigrams_new <- abstracts_df %>%
    unnest_tokens(word, abstract, token = "words") %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "\\d")) %>%
    mutate(word = str_to_lower(word)) %>%
    filter(word %in% feature_vocabulary) %>%  # Only keep words from training vocabulary
    count(id, word, sort = TRUE)
  
  # Debug unigrams
  cat("Unique unigrams found:", length(unique(dtm_unigrams_new$word)), "\n")
  if (nrow(dtm_unigrams_new) > 0) {
    cat("Top 10 unigrams:", paste(head(unique(dtm_unigrams_new$word), 10), collapse = ", "), "\n")
  }
  
  # Tokenize new data for bigrams
  dtm_bigrams_new <- abstracts_df %>%
    unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!str_detect(word1, "\\d")) %>%
    filter(!str_detect(word2, "\\d")) %>%
    unite(bigram, word1, word2, sep = "_") %>%
    filter(bigram %in% feature_vocabulary) %>%  # Only keep bigrams from training vocabulary
    count(id, bigram, sort = TRUE) %>%
    rename(word = bigram)
  
  # Debug bigrams
  cat("Unique bigrams found:", length(unique(dtm_bigrams_new$word)), "\n")
  if (nrow(dtm_bigrams_new) > 0) {
    cat("Top 10 bigrams:", paste(head(unique(dtm_bigrams_new$word), 10), collapse = ", "), "\n")
  }
  
  # Combine unigrams and bigrams for new data
  if (recalculate_tfidf) {
    # Recalculate TF-IDF based on the current corpus (this is what was done during training)
    new_combined <- bind_rows(dtm_unigrams_new, dtm_bigrams_new) %>%
      bind_tf_idf(word, id, n)
    cat("TF-IDF recalculated for current corpus (matching training process)\n")
  } else {
    # Use raw term frequencies 
    new_combined <- bind_rows(dtm_unigrams_new, dtm_bigrams_new) %>%
      rename(tf_idf = n)
    cat("Using raw term frequencies\n")
  }
  
  cat("Total term-document pairs:", nrow(new_combined), "\n")
  
  # Create DTM
  dtm <- new_combined %>%
    select(id, word, tf_idf) %>%
    cast_dtm(document = id, term = word, value = tf_idf)
  
  # Convert to matrix
  dtm_mat <- as.matrix(dtm)
  rownames(dtm_mat) <- dtm$dimnames$Docs
  
  # Convert to data frame and ensure all required features are present
  dtm_df <- as.data.frame(dtm_mat)
  
  # Add missing features as columns of zeros
  missing_features <- setdiff(feature_vocabulary, colnames(dtm_df))
  if (length(missing_features) > 0) {
    cat("Adding", length(missing_features), "missing features as zero columns\n")
    cat("Sample missing features:", paste(head(missing_features, 10), collapse = ", "), "\n")
    for (feature in missing_features) {
      dtm_df[[feature]] <- 0
    }
  }
  
  # Reorder columns to match model feature order
  dtm_df <- dtm_df[, feature_vocabulary, drop = FALSE]
  
  return(dtm_df)
}

# Create DTM for all abstracts using relevance model features
tic("Creating DTM for relevance classification")
relevance_dtm_df <- create_dtm_from_abstracts(all_abstracts, relevance_features)

# Align abstracts data with DTM
all_abstracts_aligned <- all_abstracts %>%
  filter(as.character(id) %in% rownames(relevance_dtm_df))

cat("Abstracts aligned with DTM:", nrow(all_abstracts_aligned), "\n")

# Ensure DTM rows match abstracts
relevance_dtm_df <- relevance_dtm_df[as.character(all_abstracts_aligned$id), ]

cat("Feature matrix dimensions:", dim(relevance_dtm_df), "\n")
toc()

# Stage 1: Apply Relevance Classification Model -----------------------------
cat("\n=== STAGE 1: RELEVANCE CLASSIFICATION ===\n")
tic("Relevance predictions")

# Make predictions for relevance
relevance_predictions <- predict(relevance_model, newdata = relevance_dtm_df)
relevance_probabilities <- predict(relevance_model, newdata = relevance_dtm_df, type = "prob")

# Add predictions to results
results <- all_abstracts_aligned %>%
  mutate(
    relevance_prediction = relevance_predictions,
    relevance_prob_irrelevant = relevance_probabilities$Irrelevant,
    relevance_prob_relevant = relevance_probabilities$Relevant,
    relevance_confidence = pmax(relevance_prob_irrelevant, relevance_prob_relevant)
  )

# Summary of relevance predictions
relevance_summary <- results %>%
  count(relevance_prediction) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

cat("Relevance Classification Results:\n")
print(relevance_summary)

toc()

# Stage 2: Apply Presence/Absence Classification to Relevant Abstracts -------
cat("\n=== STAGE 2: PRESENCE/ABSENCE CLASSIFICATION ===\n")
tic("Presence/absence predictions")

# Filter for relevant abstracts only
relevant_abstracts <- results %>%
  filter(relevance_prediction == "Relevant")

cat("Number of relevant abstracts for stage 2:", nrow(relevant_abstracts), "\n")

if (nrow(relevant_abstracts) > 0) {
  # Get features for presence/absence model
  presence_features <- extract_model_features(presence_model)
  cat("Presence model features:", length(presence_features), "\n")
  
  # Debug: Check presence/absence model details
  cat("Presence model class:", class(presence_model), "\n")
  cat("Presence model method:", presence_model$method, "\n")
  
  # Check if model has training data info
  if (!is.null(presence_model$trainingData)) {
    cat("Presence model training data dimensions:", dim(presence_model$trainingData), "\n")
    cat("Sample presence features:", head(colnames(presence_model$trainingData), 10), "\n")
    
    # Check training data class distribution
    if (".outcome" %in% colnames(presence_model$trainingData)) {
      cat("Training data class distribution:\n")
      train_classes <- table(presence_model$trainingData$.outcome)
      print(train_classes)
      cat("Training Presence percentage:", round(train_classes["Presence"] / sum(train_classes) * 100, 1), "%\n")
    }
    
    # Check model performance metrics from training
    if (!is.null(presence_model$results)) {
      cat("Best training performance:\n")
      best_idx <- which.max(presence_model$results$Accuracy)
      best_results <- presence_model$results[best_idx, ]
      print(best_results)
    }
  }
  
  # Debug: Compare feature overlap
  common_features <- intersect(relevance_features, presence_features)
  cat("Features in common between models:", length(common_features), "\n")
  cat("Features unique to relevance model:", length(setdiff(relevance_features, presence_features)), "\n")
  cat("Features unique to presence model:", length(setdiff(presence_features, relevance_features)), "\n")
  
  # IMPORTANT: Recreate DTM specifically for relevant abstracts to match training conditions
  # This ensures TF-IDF weights are calculated on the relevant subset only, just like during training
  cat("\nRecreating DTM matrix for relevant abstracts only (matching training conditions)...\n")
  presence_dtm_df <- create_dtm_from_abstracts(relevant_abstracts, presence_features)
  
  # Debug: Check the DTM characteristics
  cat("Presence DTM dimensions:", dim(presence_dtm_df), "\n")
  cat("Number of non-zero entries in DTM:", sum(presence_dtm_df > 0), "\n")
  cat("Percentage of non-zero entries:", round(sum(presence_dtm_df > 0) / (nrow(presence_dtm_df) * ncol(presence_dtm_df)) * 100, 2), "%\n")
  
  # Check if abstracts have any features at all
  row_sums <- rowSums(presence_dtm_df)
  cat("Abstracts with zero features:", sum(row_sums == 0), "\n")
  cat("Mean features per abstract:", round(mean(row_sums), 2), "\n")
  cat("Median features per abstract:", round(median(row_sums), 2), "\n")
  
  # Sample a few abstracts to see their feature distributions
  if (nrow(presence_dtm_df) > 0) {
    sample_indices <- head(which(row_sums > 0), 3)
    if (length(sample_indices) > 0) {
      cat("\nSample feature distributions for first few abstracts:\n")
      for (i in sample_indices) {
        non_zero_features <- presence_dtm_df[i, presence_dtm_df[i,] > 0]
        cat("Abstract", i, "- Non-zero features:", length(non_zero_features), 
            "- Max value:", round(max(non_zero_features), 4), "\n")
      }
    }
  }
  
  # Ensure DTM rows match relevant abstracts
  presence_dtm_df <- presence_dtm_df[as.character(relevant_abstracts$id), ]
  
  # Make predictions for presence/absence
  presence_predictions <- predict(presence_model, newdata = presence_dtm_df)
  presence_probabilities <- predict(presence_model, newdata = presence_dtm_df, type = "prob")
  
  # Debug: Check prediction distributions
  cat("\nPresence/Absence prediction distribution (default threshold 0.5):\n")
  print(table(presence_predictions))
  
  cat("\nPresence probability statistics:\n")
  if ("Presence" %in% colnames(presence_probabilities)) {
    presence_probs <- presence_probabilities$Presence
    cat("Min presence prob:", round(min(presence_probs), 4), "\n")
    cat("Max presence prob:", round(max(presence_probs), 4), "\n")
    cat("Mean presence prob:", round(mean(presence_probs), 4), "\n")
    cat("Median presence prob:", round(median(presence_probs), 4), "\n")
    
    # Show distribution of presence probabilities
    prob_breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    prob_dist <- cut(presence_probs, breaks = prob_breaks, include.lowest = TRUE)
    cat("Presence probability distribution:\n")
    print(table(prob_dist))
    
    # Try different thresholds to see if we can get better results
    cat("\n=== TESTING DIFFERENT THRESHOLDS ===\n")
    thresholds <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    for (thresh in thresholds) {
      custom_predictions <- ifelse(presence_probs > thresh, "Presence", "Absence")
      presence_count <- sum(custom_predictions == "Presence")
      presence_pct <- round(presence_count / length(custom_predictions) * 100, 1)
      cat("Threshold", thresh, "- Presence:", presence_count, "(", presence_pct, "%)\n")
    }
    
    # Ask user which threshold to use or automatically select one
    # For now, let's try a threshold that gives us closer to expected results
    # If you expect <1% absence, that means >99% presence
    # Let's find the threshold that gives us ~95-99% presence
    target_presence_pct <- 99  # Adjust this to your expected percentage
    best_threshold <- 0.5  # default
    best_diff <- Inf
    
    for (thresh in seq(0.1, 0.9, 0.05)) {
      custom_predictions <- ifelse(presence_probs > thresh, "Presence", "Absence")
      presence_pct <- sum(custom_predictions == "Presence") / length(custom_predictions) * 100
      diff <- abs(presence_pct - target_presence_pct)
      if (diff < best_diff) {
        best_diff <- diff
        best_threshold <- thresh
      }
    }
    
    cat("\nOptimal threshold for ~", target_presence_pct, "% presence:", best_threshold, "\n")
    
    # Apply the optimal threshold
    optimal_predictions <- ifelse(presence_probs > best_threshold, "Presence", "Absence")
    optimal_presence_pct <- round(sum(optimal_predictions == "Presence") / length(optimal_predictions) * 100, 1)
    cat("With threshold", best_threshold, "- Presence:", sum(optimal_predictions == "Presence"), 
        "(", optimal_presence_pct, "%)\n")
    
    # Use the optimal predictions instead of default
    presence_predictions <- factor(optimal_predictions, levels = c("Absence", "Presence"))
    cat("Using optimal threshold", best_threshold, "for final predictions\n")
  }
  
  # Add presence/absence predictions to relevant abstracts
  relevant_results <- relevant_abstracts %>%
    mutate(
      presence_prediction = presence_predictions,
      presence_prob_absence = presence_probabilities$Absence,
      presence_prob_presence = presence_probabilities$Presence,
      presence_confidence = pmax(presence_prob_absence, presence_prob_presence, na.rm = TRUE)
    )
  
  # Summary of presence/absence predictions
  presence_summary <- relevant_results %>%
    count(presence_prediction) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  cat("Presence/Absence Classification Results (for relevant abstracts):\n")
  print(presence_summary)
  
  # Combine results: relevant abstracts with presence/absence predictions
  # and irrelevant abstracts with NA for presence/absence
  final_results <- results %>%
    left_join(
      relevant_results %>% select(id, presence_prediction, presence_prob_absence, 
                                 presence_prob_presence, presence_confidence),
      by = "id"
    )
} else {
  cat("No relevant abstracts found for presence/absence classification.\n")
  final_results <- results %>%
    mutate(
      presence_prediction = NA_character_,
      presence_prob_absence = NA_real_,
      presence_prob_presence = NA_real_,
      presence_confidence = NA_real_
    )
}

toc()

# Final Summary and Export ---------------------------------------------------
cat("\n=== FINAL RESULTS SUMMARY ===\n")

# Create final classification combining both stages
final_results <- final_results %>%
  mutate(
    final_classification = case_when(
      relevance_prediction == "Irrelevant" ~ relevance_prediction,
      relevance_prediction == "Relevant" & !is.na(presence_prediction) ~ presence_prediction,
      TRUE ~ "Relevant_Unclassified"
    )
  )

# Final summary
final_summary <- final_results %>%
  count(final_classification) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

cat("Final Classification Summary:\n")
print(final_summary)

# Export results
output_filename <- paste0("all_abstracts_predictions_", Sys.Date(), ".csv")
write.csv(final_results, output_filename, row.names = FALSE)
cat("\nResults exported to:", output_filename, "\n")

# Export high-confidence predictions for manual review
high_confidence_presence <- final_results %>%
  filter(final_classification == "Presence", presence_confidence > 0.8) %>%
  arrange(desc(presence_confidence)) %>%
  select(id, abstract, title, authors, final_classification, 
         relevance_confidence, presence_confidence)

if (nrow(high_confidence_presence) > 0) {
  high_conf_filename <- paste0("high_confidence_presence_", Sys.Date(), ".csv")
  write.csv(high_confidence_presence, high_conf_filename, row.names = FALSE)
  cat("High-confidence presence predictions exported to:", high_conf_filename, "\n")
}

# Export high-confidence absence predictions
high_confidence_absence <- final_results %>%
  filter(final_classification == "Absence", presence_confidence > 0.8) %>%
  arrange(desc(presence_confidence)) %>%
  select(id, abstract, title, authors, final_classification, 
         relevance_confidence, presence_confidence)

if (nrow(high_confidence_absence) > 0) {
  high_conf_abs_filename <- paste0("high_confidence_absence_", Sys.Date(), ".csv")
  write.csv(high_confidence_absence, high_conf_abs_filename, row.names = FALSE)
  cat("High-confidence absence predictions exported to:", high_conf_abs_filename, "\n")
}

# Export uncertain predictions for manual review
uncertain_predictions <- final_results %>%
  filter(
    (relevance_prediction == "Relevant" & relevance_confidence < 0.7) |
    (!is.na(presence_confidence) & presence_confidence < 0.7)
  ) %>%
  arrange(relevance_confidence, presence_confidence) %>%
  select(id, abstract, title, authors, final_classification, 
         relevance_confidence, presence_confidence)

if (nrow(uncertain_predictions) > 0) {
  uncertain_filename <- paste0("uncertain_predictions_", Sys.Date(), ".csv")
  write.csv(uncertain_predictions, uncertain_filename, row.names = FALSE)
  cat("Uncertain predictions for manual review exported to:", uncertain_filename, "\n")
}

# Create summary statistics
cat("\n=== FINAL STATISTICS ===\n")
cat("Total abstracts processed:", nrow(final_results), "\n")
cat("Relevant abstracts:", sum(final_results$relevance_prediction == "Relevant", na.rm = TRUE), "\n")
cat("Irrelevant abstracts:", sum(final_results$relevance_prediction == "Irrelevant", na.rm = TRUE), "\n")
cat("Presence predictions:", sum(final_results$final_classification == "Presence", na.rm = TRUE), "\n")
cat("Absence predictions:", sum(final_results$final_classification == "Absence", na.rm = TRUE), "\n")
cat("High-confidence presence (>80%):", sum(final_results$final_classification == "Presence" & 
                                            final_results$presence_confidence > 0.8, na.rm = TRUE), "\n")
cat("High-confidence absence (>80%):", sum(final_results$final_classification == "Absence" & 
                                           final_results$presence_confidence > 0.8, na.rm = TRUE), "\n")

cat("\nModels used:\n")
cat("- Relevance:", basename(relevance_model_path), "\n")
cat("- Presence/Absence:", basename(presence_model_path), "\n")

cat("\n=== PROCESSING COMPLETE ===\n")
