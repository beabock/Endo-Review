# Author: Endo-Review Team
# Date: 2025-09-25
# Purpose: Consolidate all abstracts with metadata and labels from multiple sources
# Description: This R script uses full_dataset_predictions.csv as the master dataset containing all abstracts with complete metadata and model predictions. It merges labels with priority: manual validations override, then training labels, then model predictions. Training data rows not in master are added. For abstracts without any labels, consolidated_label is set to NA. The output consolidated_dataset.csv includes all metadata columns plus consolidated_label and source indicator.

# Dataset Consolidation Script
# This script consolidates all abstracts with metadata using labels from manual validations, training data, and model predictions,
# applying priority logic: manual validations override, then training data, then model predictions, with NA for no labels.

# Load required libraries
library(dplyr)
library(readr)

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

# Step 1: Load master dataset (full dataset predictions with metadata)
log_message("Loading master dataset (full dataset predictions)...")
master_path <- "results/full_dataset_predictions.csv"
if (!file.exists(master_path)) {
  log_message("Master dataset file not found. Please check the path.")
  stop("Master dataset file not found.")
}
master <- safe_read_csv(master_path) %>% mutate(id = as.character(id)) %>% distinct(id, .keep_all = TRUE)
log_message(paste("Loaded", nrow(master), "rows from master dataset."))

# Master dataset already contains predictions

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
  mutate(id = as.character(50000 + row_number() - 1))%>% #Creating unique id numebrs for the training dataset since they were a different set form the rest of the dataset.
  mutate(cited_reference_count = as.numeric(cited_reference_count),
  volume = as.numeric(volume),
  meeting_abstract = as.numeric(meeting_abstract),
  number_of_pages = as.character(number_of_pages),ids_number = as.character(ids_number), pubmed_id = as.character(pubmed_id))%>%
  filter(!is.na(label))

# Step 3: Load manual validations
log_message("Loading manual validations...")
manual_path <- "results/manual_validation/absence_validation_sample_for_manual_review_BB.csv"
if (!file.exists(manual_path)) {
  log_message("Manual validation file not found. Please check the path.")
  manual_validations <- data.frame(id = numeric(), manual_label = character())
} else {
  manual_validations <- safe_read_csv(manual_path) %>%
    filter(absence_source_confirmed == "ML")%>%
    select(id, manual_label) %>%
    mutate(id = as.character(id)) %>%
    distinct(id, .keep_all = TRUE)%>%
    filter(!is.na(manual_label))
  log_message(paste("Loaded", nrow(manual_validations), "rows from manual validations."))
}

manual_validations %>% group_by(manual_label) %>% tally()
View(manual_validations%>%filter(manual_label=="Absence"))

# Step 4: Consolidate datasets with priority logic
log_message("Consolidating datasets...")
log_message(paste("master$id type:", class(master$id)))
log_message(paste("training_data$id type:", class(training_data$id)))
log_message(paste("manual_validations$id type:", class(manual_validations$id)))
consolidated <- master %>%
  left_join(manual_validations ,by = "id")

log_message(paste("After joining manual:", nrow(consolidated)))
consolidated <- consolidated %>%
  full_join(training_data) #Only 1 absence in training data



log_message(paste("After joining training", nrow(consolidated)))
consolidated <- consolidated %>%
  mutate(
    consolidated_label = case_when(
      !is.na(manual_label) ~ manual_label,  # Priority 1: Manual validations
      !is.na(label) ~ label,               # Priority 2: Training data
      !is.na(final_classification) ~ final_classification,  # Priority 3: Model predictions
      TRUE ~ NA_character_                  # No labels available
    ),
    source = case_when(
      !is.na(manual_label) ~ "manual",
      !is.na(label) ~ "training",
      !is.na(final_classification) ~ "predictions",
      TRUE ~ NA_character_
    )
  )



# Check for duplicates
dup_count <- sum(duplicated(consolidated$id))

log_message(paste("Number of duplicate IDs:", dup_count))
if (dup_count > 0) {
  log_message("Warning: Duplicate IDs found. Keepig them for now.")
}

log_message(paste("Consolidated dataset has", nrow(consolidated), "rows."))

consolidated %>%
group_by(consolidated_label) %>%
tally()

consolidated <- consolidated %>%
  mutate(
    consolidated_label = case_when(
      consolidated_label == "Both" ~ "Presence", #Treating both as presence
      consolidated_label == "Other" ~ "Irrelevant",
      consolidated_label == "Review" ~ "Irrelevant",
      TRUE ~ consolidated_label
    )
  )%>%
  filter(consolidated_label != "Irrelevant") #Filter out anything irrelevant for analysis.

dim(consolidated)

# Step 5: Output consolidated dataset
output_path <- "results/consolidated_dataset.csv"
log_message("Writing consolidated dataset to file...")
write_csv(consolidated, output_path)
log_message("Consolidated dataset written successfully.")

# Summary
log_message("Consolidation complete.")
log_message(paste("Total rows:", nrow(consolidated)))
log_message(paste("Rows with manual labels:", sum(!is.na(consolidated$manual_label))))
log_message(paste("Rows with training labels:", sum(!is.na(consolidated$label))))
log_message(paste("Rows with model predictions:", sum(!is.na(consolidated$final_classification))))
log_message(paste("Rows with consolidated labels:", sum(!is.na(consolidated$consolidated_label))))
log_message("Script execution finished.")