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
all_abstracts <- read.csv("data/All_abstracts.csv") %>%
  clean_names() %>%
  filter(!is.na(abstract), nchar(abstract) > 10) %>%  # Filter out empty or very short abstracts
  mutate(id = row_number())

cat("Total abstracts loaded:", nrow(all_abstracts), "\n")

# Load the trained models ----------------------------------------------------
cat("Loading trained models...\n")

# Try to load the best models based on the naming convention from ML_compare_models
# Stage 1: Relevance Classification



 # Find relevance model (e.g., best_model_xgbTree_thr0.25_relevance_classification.rds)
relevance_model_files <- list.files("models", pattern = "^best_model_.*relevance_classification.*\\.rds$", full.names = TRUE)
if (length(relevance_model_files) == 0) {
  stop("No relevance classification model found. Please run ML_compare_models_subset_optimized.R first.")
}
relevance_model_path <- relevance_model_files[1]
cat("Loading relevance model:", basename(relevance_model_path), "\n")
relevance_model <- readRDS(relevance_model_path)



# Find presence/both/absence model (e.g., best_model_svmLinear_presence_both_absence.rds)
presence_model_files <- list.files("models", pattern = "^best_model_.*presence_both_absence.*\\.rds$", full.names = TRUE)
if (length(presence_model_files) == 0) {
  stop("No presence/both/absence classification model found. Please run ML_compare_models_subset_optimized.R first.")
}
cat("Available presence/both/absence models:\n")
for (i in seq_along(presence_model_files)) {
  cat(i, ":", basename(presence_model_files[i]), "\n")
}
presence_model_path <- presence_model_files[1]
cat("Loading presence/both/absence model:", basename(presence_model_path), "\n")
presence_model <- readRDS(presence_model_path)


# Verify this is a presence/absence model by checking the outcome levels
if (!is.null(presence_model$trainingData) && ".outcome" %in% colnames(presence_model$trainingData)) {
  outcome_levels <- levels(presence_model$trainingData$.outcome)
  cat("Model outcome levels:", paste(outcome_levels, collapse = ", "), "\n")
  if (!all(c("Presence", "Absence") %in% outcome_levels)) {
    warning("This model doesn't seem to be trained for Presence/Absence classification!")
  }
}


# Stage 2: Apply Presence/Both/Absence Classification to Relevant Abstracts -------
cat("\n=== STAGE 2: PRESENCE/BOTH/ABSENCE CLASSIFICATION ===\n")
tic("Presence/Both/Absence predictions")

# Filter for relevant abstracts only
relevant_abstracts <- results %>%
  filter(relevance_prediction == "Relevant")

cat("Number of relevant abstracts for stage 2:", nrow(relevant_abstracts), "\n")

if (nrow(relevant_abstracts) > 0) {
  # Get features for presence/both/absence model
  presence_features <- extract_model_features(presence_model)
  cat("Presence model features:", length(presence_features), "\n")

  # Recreate DTM for relevant abstracts
  presence_dtm_df <- create_dtm_from_abstracts(relevant_abstracts, presence_features)
  cat("\n[DEBUG] DTM for relevant abstracts created. Dimensions:", dim(presence_dtm_df)[1], "x", dim(presence_dtm_df)[2], "\n")
  cat("[DEBUG] Number of nonzero entries in DTM:", sum(presence_dtm_df > 0), "\n")
  cat("[DEBUG] Sample of DTM (first row):\n")
  print(head(presence_dtm_df, 1))
  cat("[DEBUG] Number of abstracts with all-zero features:", sum(rowSums(presence_dtm_df) == 0), "\n")

  # Ensure DTM rownames match relevant_abstracts$id
  presence_dtm_df <- presence_dtm_df[as.character(relevant_abstracts$id), ]

  # Make predictions for presence/both/absence
  presence_predictions <- predict(presence_model, newdata = presence_dtm_df)
  presence_probabilities <- predict(presence_model, newdata = presence_dtm_df, type = "prob")

  # Debug: Check for NA predictions
  cat("[DEBUG] Number of NA predictions:", sum(is.na(presence_predictions)), "\n")
  cat("[DEBUG] Table of predictions:\n")
  print(table(presence_predictions, useNA = "ifany"))

  # Add predictions to relevant abstracts
  # For confidence, use the max probability among the three classes
  presence_confidence <- apply(presence_probabilities, 1, max)
  relevant_results <- relevant_abstracts %>%
    mutate(
      presence_prediction = presence_predictions,
      presence_prob_presence = presence_probabilities$Presence,
      presence_prob_both = if ("Both" %in% colnames(presence_probabilities)) presence_probabilities$Both else NA_real_,
      presence_prob_absence = presence_probabilities$Absence,
      presence_confidence = presence_confidence
    )

  # Summary of predictions
  presence_summary <- relevant_results %>%
    count(presence_prediction) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  cat("Presence/Both/Absence Classification Results (for relevant abstracts):\n")
  print(presence_summary)

  # Combine results: relevant abstracts with predictions, others NA
  final_results <- results %>%
    left_join(
      relevant_results %>% select(id, presence_prediction, presence_prob_presence, presence_prob_both, presence_prob_absence, presence_confidence),
      by = "id"
    )
} else {
  cat("No relevant abstracts found for presence/both/absence classification.\n")
  final_results <- results %>%
    mutate(
      presence_prediction = NA_character_,
      presence_prob_presence = NA_real_,
      presence_prob_both = NA_real_,
      presence_prob_absence = NA_real_,
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
      relevance_prediction == "Relevant" & !is.na(presence_prediction) ~ as.character(presence_prediction),
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

# Ensure results directory exists
if (!dir.exists("results")) dir.create("results")

output_filename <- file.path("results", paste0("all_abstracts_predictions_", Sys.Date(), ".csv"))
write.csv(final_results, output_filename, row.names = FALSE)
cat("\nResults exported to:", output_filename, "\n")


# Export high-confidence predictions for manual review (Presence, Both, Absence)
for (class_label in c("Presence", "Both", "Absence")) {
  high_conf <- final_results %>%
    filter(final_classification == class_label, presence_confidence > 0.8) %>%
    arrange(desc(presence_confidence)) %>%
    select(id, abstract, title, authors, final_classification, 
           relevance_confidence, presence_confidence)
  if (nrow(high_conf) > 0) {
    high_conf_filename <- file.path("results", paste0("high_confidence_", tolower(class_label), "_", Sys.Date(), ".csv"))
    write.csv(high_conf, high_conf_filename, row.names = FALSE)
    cat("High-confidence", class_label, "predictions exported to:", high_conf_filename, "\n")
  }
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
  uncertain_filename <- file.path("results", paste0("uncertain_predictions_", Sys.Date(), ".csv"))
  write.csv(uncertain_predictions, uncertain_filename, row.names = FALSE)
  cat("Uncertain predictions for manual review exported to:", uncertain_filename, "\n")
}


# Create summary statistics
cat("\n=== FINAL STATISTICS ===\n")
cat("Total abstracts processed:", nrow(final_results), "\n")
cat("Relevant abstracts:", sum(final_results$relevance_prediction == "Relevant", na.rm = TRUE), "\n")
cat("Irrelevant abstracts:", sum(final_results$relevance_prediction == "Irrelevant", na.rm = TRUE), "\n")
for (class_label in c("Presence", "Both", "Absence")) {
  cat(class_label, "predictions:", sum(final_results$final_classification == class_label, na.rm = TRUE), "\n")
  cat("High-confidence", class_label, "(>80%):", sum(final_results$final_classification == class_label & final_results$presence_confidence > 0.8, na.rm = TRUE), "\n")
}

cat("\nModels used:\n")
cat("- Relevance:", basename(relevance_model_path), "\n")
cat("- Presence/Absence:", basename(presence_model_path), "\n")

cat("\n=== PROCESSING COMPLETE ===\n")

