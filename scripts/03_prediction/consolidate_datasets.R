# Author: Endo-Review Team
# Date: 2025-09-26
# Purpose: Enhanced consolidation of all abstracts with metadata and labels from multiple sources
# Description: This R script uses full_dataset_predictions.csv as the master dataset containing all abstracts with complete metadata and model predictions. It merges labels with priority: training data > manual presence validations > manual absence validations > model predictions. Training data rows not in master are added. For abstracts without any labels, consolidated_label is set to NA. The output consolidated_dataset.csv includes all metadata columns plus consolidated_label and source indicator.

# Enhanced Dataset Consolidation Script
# This script consolidates all abstracts with metadata using labels from training data, manual presence/absence validations, and model predictions,
# applying priority logic: training > manual presence > manual absence > model predictions, with NA for no labels.

# Load required libraries
library(dplyr)
library(readr)
library(caret)  # For confusion matrix and ML metrics
library(ggplot2)  # For visualizations (if needed)

# Function to log messages
log_message <- function(message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] ", message, "\n")
  cat(log_entry)
  # Optionally write to a log file
  write(log_entry, file = "scripts/03b_dataset_consolidation.log", append = TRUE)
}

# Error handling wrapper
safe_read_csv <- function(file_path, ...) {
  tryCatch({
    read_csv(file_path, ...)
  }, error = function(e) {
    log_message(paste("Error reading file:", file_path, " - ", e$message))
    stop(e)
  })
}

# Function to calculate ML accuracy metrics
calculate_ml_metrics <- function(manual_labels, ml_predictions, confidence_scores = NULL) {
  log_message("Calculating ML accuracy metrics...")

  # Create confusion matrix
  valid_idx <- !is.na(manual_labels) & !is.na(ml_predictions) & manual_labels != "Irrelevant"
  manual_valid <- manual_labels[valid_idx]
  ml_valid <- ml_predictions[valid_idx]

  if (length(manual_valid) == 0) {
    log_message("Warning: No valid manual vs ML comparisons available for accuracy analysis")
    return(list(
      confusion_matrix = NULL,
      metrics = data.frame(),
      confidence_analysis = data.frame()
    ))
  }

  # Get all unique labels from both manual and ML predictions
  all_labels <- unique(c(manual_valid, ml_valid))
  manual_factor <- factor(manual_valid, levels = all_labels)
  ml_factor <- factor(ml_valid, levels = all_labels)

  # Confusion matrix
  cm <- confusionMatrix(ml_factor, manual_factor)

  # Extract metrics by class with error handling
  if (length(cm$byClass) > 0 && !is.null(dim(cm$byClass))) {
    metrics_df <- data.frame(
      Class = rownames(cm$byClass),
      Precision = cm$byClass[, "Pos Pred Value"],
      Recall = cm$byClass[, "Sensitivity"],
      F1_Score = cm$byClass[, "F1"],
      Support = cm$byClass[, "Prevalence"] * length(manual_valid)
    )
  } else {
    # Fallback when byClass is not available or has unexpected structure
    log_message("Warning: Unable to extract per-class metrics, using basic counts")
    class_counts <- table(manual_valid)
    metrics_df <- data.frame(
      Class = names(class_counts),
      Precision = NA,
      Recall = NA,
      F1_Score = NA,
      Support = as.numeric(class_counts)
    )
  }

  # Overall accuracy metrics
  overall_metrics <- data.frame(
    Metric = c("Accuracy", "Kappa", "McnemarPValue"),
    Value = c(cm$overall["Accuracy"], cm$overall["Kappa"], cm$overall["McnemarPValue"])
  )

  # Confidence score analysis
  confidence_analysis <- data.frame()
  if (!is.null(confidence_scores) && length(confidence_scores) > 0) {
    conf_valid <- confidence_scores[valid_idx]

    confidence_analysis <- data.frame(
      Class = c("All", levels(factor(manual_valid))),
      Mean_Confidence = c(mean(conf_valid, na.rm = TRUE),
                         tapply(conf_valid, manual_valid, mean, na.rm = TRUE)),
      Median_Confidence = c(median(conf_valid, na.rm = TRUE),
                           tapply(conf_valid, manual_valid, median, na.rm = TRUE)),
      Confidence_SD = c(sd(conf_valid, na.rm = TRUE),
                       tapply(conf_valid, manual_valid, sd, na.rm = TRUE))
    )
  }

  return(list(
    confusion_matrix = cm,
    metrics = metrics_df,
    overall_metrics = overall_metrics,
    confidence_analysis = confidence_analysis
  ))
}

# Step 1: Load master dataset (full dataset predictions with metadata)
log_message("Loading master dataset (full dataset predictions)...")
master_path <- "results/full_dataset_predictions.csv"
if (!file.exists(master_path)) {
  log_message("Master dataset file not found. Please check the path.")
  stop("Master dataset file not found.")
}
master <- safe_read_csv(master_path) %>% mutate(id = as.character(id)) %>% distinct(id, .keep_all = TRUE)
log_message(paste("Loaded", nrow(master), "rows from master dataset."))

# Step 2: Load training data
log_message("Loading training data...")
training_path <- "data/raw/Training_labeled_abs_6.csv"
if (!file.exists(training_path)) {
  log_message("Training data file not found. Please check the path.")
  training_data <- data.frame(id = numeric(), label = character())
} else {
  training_data <- safe_read_csv(training_path) %>%
    filter(origin == "Literature") %>% #Removing any synthetic abstracts
    mutate(id = as.character(id))
  log_message(paste("Loaded", nrow(training_data), "rows from training data."))
}

training_data <- training_data %>%
  mutate(id = as.character(50000 + row_number() - 1))%>% #Creating unique id numbers for the training dataset
  mutate(cited_reference_count = as.numeric(cited_reference_count),
  volume = as.numeric(volume),
  meeting_abstract = as.numeric(meeting_abstract),
  number_of_pages = as.character(number_of_pages),ids_number = as.character(ids_number), pubmed_id = as.character(pubmed_id))%>%
  filter(!is.na(label))

# Step 3: Load manual presence validations
log_message("Loading manual presence validations...")
presence_path <- "results/manual_validation/presence_validation_sample_for_manual_review.csv"
if (!file.exists(presence_path)) {
  log_message("Manual presence validation file not found. Please check the path.")
  presence_validations <- data.frame(id = numeric(), manual_label = character())
} else {
  presence_validations <- safe_read_csv(presence_path) %>%
    select(id, manual_label, manual_confidence, notes) %>%
    mutate(id = as.character(id)) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(!is.na(manual_label))
  log_message(paste("Loaded", nrow(presence_validations), "rows from manual presence validations."))
}

# Step 4: Load manual absence validations
log_message("Loading manual absence validations...")
absence_path <- "results/manual_validation/absence_validation_sample_for_manual_review_BB.csv"
if (!file.exists(absence_path)) {
  log_message("Manual absence validation file not found. Please check the path.")
  absence_validations <- data.frame(id = numeric(), manual_label = character())
} else {
  absence_validations <- safe_read_csv(absence_path) %>%
    filter(absence_source_confirmed == "ML") %>%
    select(id, manual_label, manual_confidence, notes) %>%
    mutate(id = as.character(id)) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(!is.na(manual_label))
  log_message(paste("Loaded", nrow(absence_validations), "rows from manual absence validations."))
}

# Combine manual validations
manual_validations <- bind_rows(
  presence_validations %>% mutate(validation_type = "presence"),
  absence_validations %>% mutate(validation_type = "absence")
) %>% distinct(id, .keep_all = TRUE)

log_message(paste("Total manual validations:", nrow(manual_validations)))
manual_validations %>% group_by(manual_label, validation_type) %>% tally()

# Step 5: Consolidate datasets with priority logic
log_message("Consolidating datasets with priority: training > manual presence > manual absence > ML predictions...")
log_message(paste("master$id type:", class(master$id)))
log_message(paste("training_data$id type:", class(training_data$id)))
log_message(paste("manual_validations$id type:", class(manual_validations$id)))

# Remove overlapping columns from training data before joining to avoid .x/.y suffixes
# Keep only unique training data columns (like 'label') and ensure 'id' is preserved
overlapping_cols <- setdiff(intersect(names(training_data), names(master)), "id")
training_unique_cols <- training_data %>% select(-all_of(overlapping_cols))

consolidated <- master %>%
  left_join(training_unique_cols, by = "id") %>%
  left_join(manual_validations, by = "id", suffix = c("", "_manual"))

log_message(paste("After joining all datasets:", nrow(consolidated)))

# Apply priority logic: training > manual presence > manual absence > ML predictions
consolidated <- consolidated %>%
  mutate(
    consolidated_label = case_when(
      !is.na(label) ~ label,                                    # Priority 1: Training data
      !is.na(manual_label) & validation_type == "presence" ~ manual_label,  # Priority 2: Manual presence
      !is.na(manual_label) & validation_type == "absence" ~ manual_label,   # Priority 3: Manual absence
      !is.na(final_classification) ~ final_classification,     # Priority 4: Model predictions
      TRUE ~ NA_character_                                     # No labels available
    ),
    validation_source = case_when(
      !is.na(label) ~ "training",
      !is.na(manual_label) & validation_type == "presence" ~ "manual_presence",
      !is.na(manual_label) & validation_type == "absence" ~ "manual_absence",
      !is.na(final_classification) ~ "ml_predictions",
      TRUE ~ NA_character_
    )
  )

# Standardize labels immediately after consolidation (Both -> Presence, Review -> Irrelevant)
consolidated <- consolidated %>%
  mutate(
    consolidated_label = case_when(
      consolidated_label == "Both" ~ "Presence", #Treating both as presence
      consolidated_label == "Other" ~ "Irrelevant",
      consolidated_label == "Review" ~ "Irrelevant",
      TRUE ~ consolidated_label
    )
  )

# Check for duplicates
dup_count <- sum(duplicated(consolidated$id))
log_message(paste("Number of duplicate IDs:", dup_count))
if (dup_count > 0) {
  log_message("Warning: Duplicate IDs found. Keeping them for analysis.")
}

log_message(paste("Consolidated dataset has", nrow(consolidated), "rows."))

# Analyze label distributions
log_message("Label distribution analysis:")
consolidated %>%
  group_by(consolidated_label, validation_source) %>%
  tally() %>%
  print()

# Filter out irrelevant labels (already standardized above)
consolidated <- consolidated %>%
  filter(consolidated_label != "Irrelevant") #Filter out anything irrelevant for analysis.

log_message(paste("After filtering irrelevant:", nrow(consolidated), "rows."))

# Remove junky columns before final output
junk_columns <- c(
  "conference_authors", "book_series_title", "book_series_subtitle",
  "conference_title", "conference_date", "conference_location",
  "conference_sponsor", "conference_host", "affiliations", "addresses",
  "reprint_addresses", "email_addresses", "funding_name_preferred",
  "publisher_city", "publisher_address", "supplement", "meeting_abstract",
  "start_page", "end_page", "early_access_date", "open_access_designations",
  "highly_cited_status", "hot_paper_status", "date_of_export",
  "author_full_names_1", "web_of_science_record", "article_number",
  "page_count_1", "link", "authors_with_affiliations_1",
  "molecular_sequence_numbers", "chemicals_cas", "tradenames", "manufacturers",
  "funding_details_1", "funding_texts_1", "correspondence_address_1",
  "conference_name_1", "conference_date_1", "conference_location_1",
  "conference_code", "coden", "abbreviated_source_title_1",
  "publication_stage", "open_access_1", "X", "predicted_label",
  "X.1", "X.2", "X.3", "X.4", "X.5", "X.6", "X.7", "X.8", "X.9",
  "X.10", "X.11", "X.12", "X.13"
)

# Remove junk columns that exist in the dataset
existing_junk_cols <- intersect(junk_columns, names(consolidated))
if (length(existing_junk_cols) > 0) {
  consolidated <- consolidated %>% select(-all_of(existing_junk_cols))
  log_message(paste("Removed", length(existing_junk_cols), "junk columns from consolidated dataset"))
}

# Reorder columns to put key columns (id, abstract, consolidated_label) at the front
key_columns <- c("id", "abstract", "consolidated_label")
existing_key_cols <- intersect(key_columns, names(consolidated))
remaining_cols <- setdiff(names(consolidated), existing_key_cols)

if (length(existing_key_cols) > 0) {
  consolidated <- consolidated %>% select(all_of(existing_key_cols), all_of(remaining_cols))
  log_message(paste("Reordered columns with", paste(existing_key_cols, collapse = ", "), "at the front"))
}

# Step 6: ML Accuracy Analysis
log_message("Performing ML accuracy analysis...")

# Prepare data for analysis (labels already standardized)
ml_analysis_data <- consolidated %>%
  filter(!is.na(final_classification) & !is.na(consolidated_label) & consolidated_label != "Irrelevant") %>%
  mutate(
    ml_prediction = final_classification,
    manual_label = consolidated_label
  )

# Calculate metrics
if (nrow(ml_analysis_data) > 0) {
  ml_metrics <- calculate_ml_metrics(
    ml_analysis_data$manual_label,
    ml_analysis_data$ml_prediction,
    ml_analysis_data$confidence
  )

  # Save ML accuracy metrics
  write_csv(ml_metrics$metrics, "results/ml_accuracy_metrics.csv")
  log_message("ML accuracy metrics saved to results/ml_accuracy_metrics.csv")

  # Save confusion matrix details
  if (!is.null(ml_metrics$confusion_matrix)) {
    cm_details <- as.data.frame(ml_metrics$confusion_matrix$table)
    write_csv(cm_details, "results/ml_confusion_matrix.csv")
    log_message("Confusion matrix saved to results/ml_confusion_matrix.csv")
  }

  # Save confidence analysis
  if (nrow(ml_metrics$confidence_analysis) > 0) {
    write_csv(ml_metrics$confidence_analysis, "results/confidence_distribution_analysis.csv")
    log_message("Confidence distribution analysis saved to results/confidence_distribution_analysis.csv")
  }

  # Create comprehensive comparison report
  report_content <- paste0(
    "ML Accuracy Analysis Report\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
    "Overall Accuracy Metrics:\n",
    paste(capture.output(print(ml_metrics$overall_metrics)), collapse = "\n"), "\n\n",
    "Per-Class Performance:\n",
    paste(capture.output(print(ml_metrics$metrics)), collapse = "\n"), "\n\n",
    "Confidence Analysis:\n",
    paste(capture.output(print(ml_metrics$confidence_analysis)), collapse = "\n")
  )

  writeLines(report_content, "results/validation_comparison_report.txt")
  log_message("Validation comparison report saved to results/validation_comparison_report.txt")

  # Training vs Manual comparison
  training_vs_manual <- consolidated %>%
    filter(!is.na(label) & !is.na(manual_label)) %>%
    group_by(label, manual_label) %>%
    tally() %>%
    mutate(agreement = label == manual_label)

  write_csv(training_vs_manual, "results/training_vs_manual_comparison.csv")
  log_message("Training vs manual comparison saved to results/training_vs_manual_comparison.csv")
}

# Step 7: Output consolidated dataset
output_path <- "results/consolidated_dataset.csv"
log_message("Writing consolidated dataset to file...")
write_csv(consolidated, output_path)
log_message("Consolidated dataset written successfully.")

# Step 8: Summary statistics
log_message("Consolidation complete.")
log_message(paste("Total rows:", nrow(consolidated)))
log_message(paste("Rows with training labels:", sum(!is.na(consolidated$label))))
log_message(paste("Rows with manual presence labels:", sum(consolidated$validation_source == "manual_presence", na.rm = TRUE)))
log_message(paste("Rows with manual absence labels:", sum(consolidated$validation_source == "manual_absence", na.rm = TRUE)))
log_message(paste("Rows with ML predictions:", sum(!is.na(consolidated$final_classification))))
log_message(paste("Rows with consolidated labels:", sum(!is.na(consolidated$consolidated_label))))
log_message(paste("Unique sources used:", paste(unique(consolidated$validation_source), collapse = ", ")))
log_message("Script execution finished.")