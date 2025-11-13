#!/usr/bin/env Rscript
# Script to analyze the absence validation data from manual validation
# This script reads the absence validation CSV file and computes various metrics
# to assess the performance of absence detection methods

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(knitr)

#' Read and validate the absence validation data
#' @param file_path Path to the CSV file
#' @return Cleaned data frame
read_validation_data <- function(file_path) {
  # Read the CSV file
  data <- read_csv(file_path, show_col_types = FALSE)

  # Basic validation
  required_cols <- c("validation_id", "validator_name", "validation_date",
                     "manual_label", "absence_source_confirmed", "manual_confidence")

  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Clean and standardize data
  data_clean <- data %>%
    mutate(
      # Standardize manual labels
      manual_label = case_when(
        str_detect(tolower(manual_label), "^absence|^abs$") ~ "Absence",
        
        
        str_detect(tolower(manual_label), "^presence|^pres$") ~ "Presence",
        str_detect(tolower(manual_label), "^both$") ~ "Both", 
        
        # Treat 'Review' and 'Irrelevant' as 'Irrelevant'
        str_detect(tolower(manual_label), "^irrelevant|^irr$") ~ "Irrelevant",
        str_detect(tolower(manual_label), "^review$") ~ "Irrelevant", 
        
        TRUE ~ manual_label
      ),

      # Standardize absence source confirmed
      absence_source_confirmed = case_when(
        str_detect(tolower(absence_source_confirmed), "^ml$|^machine|^model$") ~ "ML",
        str_detect(tolower(absence_source_confirmed), "^string$|^text$") ~ "String",
        str_detect(tolower(absence_source_confirmed), "^training|^manual$") ~ "Training_Manual",
        str_detect(tolower(absence_source_confirmed), "^both$") ~ "Both",
        str_detect(tolower(absence_source_confirmed), "^neither$") ~ "Neither",
        TRUE ~ absence_source_confirmed
      ),

      # Standardize confidence levels
      manual_confidence = case_when(
        str_detect(tolower(manual_confidence), "^high$") ~ "High",
        str_detect(tolower(manual_confidence), "^medium|^med$") ~ "Medium",
        str_detect(tolower(manual_confidence), "^low$") ~ "Low",
        TRUE ~ manual_confidence
      )
    ) %>%
    # Convert confidence to factor with proper ordering
    mutate(
      manual_confidence = factor(manual_confidence, levels = c("Low", "Medium", "High"), ordered = TRUE)
    )

  return(data_clean)
}

#' Compute precision metrics for absence detection
#' @param data Cleaned validation data
#' @return Data frame with precision metrics
compute_precision_metrics <- function(data) {
  # Filter to only Absence/Presence classifications (exclude Irrelevant)
  absence_data <- data %>%
    filter(manual_label %in% c("Absence", "Presence"))

  # Overall precision by detection method
  # Precision = True Positives / (True Positives + False Positives)
  method_precision <- absence_data %>%
    group_by(absence_source_confirmed) %>%
    summarise(
      total_validated = n(),
      true_positives = sum(manual_label == "Absence"),  # Code said Absence, manual check confirms
      false_positives = sum(manual_label == "Presence"), # Code said Absence, manual check says Presence
      precision = true_positives / total_validated * 100,
      false_discovery_rate = false_positives / total_validated * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(precision))

  # Performance by confidence level
  confidence_performance <- absence_data %>%
    group_by(manual_confidence, absence_source_confirmed) %>%
    summarise(
      total_cases = n(),
      precision = sum(manual_label == "Absence") / n() * 100, # This is precision
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = absence_source_confirmed, values_from = c(total_cases, precision),
                names_prefix = "")

  return(list(
    method_precision = method_precision,
    confidence_performance = confidence_performance
  ))
}

#' Analyze patterns and biases in the data
#' @param data Cleaned validation data
#' @return List of analysis results
analyze_patterns <- function(data) {
  # Distribution of manual labels
  label_distribution <- data %>%
    count(manual_label) %>%
    mutate(percentage = n / sum(n) * 100)

  # Detection method effectiveness
  method_effectiveness <- data %>%
    filter(!is.na(absence_source_confirmed)) %>%
    count(absence_source_confirmed, manual_label) %>%
    group_by(absence_source_confirmed) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ungroup()

  # Confidence level analysis
  confidence_analysis <- data %>%
    count(manual_confidence, manual_label) %>%
    group_by(manual_confidence) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ungroup()

  # Cross-tabulation of detection method vs manual label
  detection_vs_manual <- data %>%
    filter(!is.na(absence_source_confirmed)) %>%
    count(absence_source_confirmed, manual_label) %>%
    pivot_wider(names_from = manual_label, values_from = n, values_fill = 0)

  # Notes and issues analysis
  # Check if 'notes' column exists to prevent error
  if ("notes" %in% colnames(data)) {
    notes_analysis <- data %>%
      filter(!is.na(notes) & notes != "") %>%
      mutate(
        has_notes = TRUE,
        notes_length = nchar(notes)
      ) %>%
      select(validation_id, manual_label, absence_source_confirmed,
             manual_confidence, notes, has_notes, notes_length)
  } else {
    # Create an empty data frame with the expected structure if 'notes' is missing
    notes_analysis <- data.frame(
      validation_id = character(0),
      manual_label = character(0),
      absence_source_confirmed = character(0),
      manual_confidence = factor(character(0), levels = c("Low", "Medium", "High")),
      notes = character(0),
      has_notes = logical(0),
      notes_length = integer(0)
    )
  }
  
  # Actionable Notes Analysis: Extract notes from False Positives
  if ("notes" %in% colnames(data)) {
    fp_notes <- data %>%
      filter(manual_label == "Presence" & absence_source_confirmed == "ML") %>%
      filter(!is.na(notes) & notes != "") %>%
      select(validation_id, `False Positive Note` = notes)
  } else {
    fp_notes <- data.frame(
      validation_id = character(0),
      `False Positive Note` = character(0)
    )
  }

  return(list(
    label_distribution = label_distribution,
    method_effectiveness = method_effectiveness,
    confidence_analysis = confidence_analysis,
    detection_vs_manual = detection_vs_manual,
    notes_analysis = notes_analysis,
    fp_notes = fp_notes # Add new item to list
  ))
}

#' Generate visualizations
#' @param data Cleaned validation data
#' @param output_dir Directory to save plots
generate_visualizations <- function(data, output_dir = "results/manual_validation") {
  # Create output directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Distribution of manual labels
  p1 <- ggplot(data, aes(x = manual_label, fill = manual_label)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Distribution of Manual Validation Labels",
         x = "Manual Label", y = "Count") +
    theme(legend.position = "none")

  ggsave(file.path(output_dir, "manual_labels_distribution.png"), p1, width = 8, height = 6)

  # 2. Detection method performance
  method_data <- data %>%
    filter(!is.na(absence_source_confirmed)) %>%
    count(absence_source_confirmed, manual_label) %>%
    group_by(absence_source_confirmed) %>%
    mutate(percentage = n / sum(n) * 100)

  p2 <- ggplot(method_data, aes(x = absence_source_confirmed, y = percentage, fill = manual_label)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    labs(title = "Detection Method Performance by Manual Label",
         x = "Detection Method", y = "Percentage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(output_dir, "detection_method_performance.png"), p2, width = 10, height = 6)

  # 3. Confidence level distribution
  p3 <- ggplot(data, aes(x = manual_confidence, fill = manual_confidence)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Distribution of Manual Confidence Levels",
         x = "Confidence Level", y = "Count") +
    theme(legend.position = "none")

  ggsave(file.path(output_dir, "confidence_distribution.png"), p3, width = 8, height = 6)

  # 4. Cross-tabulation heatmap
  crosstab_data <- data %>%
    filter(!is.na(absence_source_confirmed)) %>%
    count(absence_source_confirmed, manual_label) %>%
    complete(absence_source_confirmed, manual_label, fill = list(n = 0))

  p4 <- ggplot(crosstab_data, aes(x = absence_source_confirmed, y = manual_label, fill = n)) +
    geom_tile() +
    geom_text(aes(label = n), color = "white", size = 3) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    labs(title = "Detection Method vs Manual Label Cross-tabulation",
         x = "Detection Method", y = "Manual Label", fill = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(output_dir, "method_vs_manual_crosstab.png"), p4, width = 10, height = 6)

  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}

#' Generate comprehensive analysis report
#' @param data Cleaned validation data
#' @param output_dir Directory to save report
generate_report <- function(data, output_dir = "results/manual_validation") {
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Compute metrics
  precision_metrics <- compute_precision_metrics(data)
  pattern_analysis <- analyze_patterns(data)

  # Generate visualizations
  plots <- generate_visualizations(data, output_dir)

  # --- Dynamic recommendations based on data ---
  
  # 1. Method to improve: the one with the lowest precision
  worst_method <- "N/A"
  if(nrow(precision_metrics$method_precision) > 0) {
    worst_method <- precision_metrics$method_precision %>%
      arrange(precision) %>%
      slice(1) %>%
      pull(absence_source_confirmed)
  }

  # 2. Confidence level to review: the one with the lowest precision
  confidence_to_review <- data %>%
    filter(manual_label %in% c("Absence", "Presence")) %>%
    group_by(manual_confidence) %>%
    summarise(precision = mean(manual_label == "Absence")) %>%
    filter(!is.na(manual_confidence)) %>%
    arrange(precision) %>%
    slice(1) %>%
    pull(manual_confidence) %>%
    as.character()
  if (length(confidence_to_review) == 0) confidence_to_review <- "Low" # Fallback

  # 4. Cases for double-validation: The worst-performing confidence level
  qc_confidence <- confidence_to_review
  
  # --- Data Quality Notes ---
  notes_df <- pattern_analysis$notes_analysis
  avg_notes_len <- if (nrow(notes_df) > 0) {
    mean(notes_df$notes_length, na.rm = TRUE)
  } else {
    0
  }
  
  irrelevant_count <- sum(data$manual_label == "Irrelevant", na.rm = TRUE)
  irrelevant_pct <- mean(data$manual_label == "Irrelevant", na.rm = TRUE) * 100

  # --- Actionable Notes Table ---
  fp_notes_table <- "No notes found for ML False Positives."
  if(nrow(pattern_analysis$fp_notes) > 0) {
    fp_notes_table <- paste(
      knitr::kable(pattern_analysis$fp_notes, format = "markdown"), 
      collapse = "\n"
    )
  }

  # Create report
  report_content <- sprintf("
# Absence Validation Analysis Report

Generated on: %s

## Summary Statistics
- Total abstracts validated: %d
- Total validators: %d
- Validation period: %s to %s

## Manual Label Distribution
%s

## Detection Method Precision
This table shows the precision of your 'Absence' predictions.
Precision = (Correct 'Absence' labels) / (Total 'Absence' labels checked)
%s

## Performance by Confidence Level
%s

## Key Findings

### Method Effectiveness
%s

### Confidence Analysis
%s

### Patterns and Biases
- Detection vs Manual Label Correlation: %s

### Recommendations
1. **Method Improvements**: The '%s' detection method has the lowest precision. Focus on improving its logic.
2. **Confidence Review**: Review classifications with '%s' confidence, as they have the lowest precision.
3. **Training Data**: The model incorrectly labeled 'Presence' cases as 'Absence'. Incorporate more of these false positive examples into future training data to improve discrimination.
4. **Quality Control**: Implement double-validation for all '%s' confidence classifications to catch potential errors early.

## Data Quality Notes
- Abstracts with notes/issues: %d
- Average notes length: %.1f characters
- Irrelevant classifications: %d (%.1f%%)

---
## Actionable Insights from Notes
### Notes from ML False Positives
*These are the notes from cases your ML model labeled 'Absence' but were manually corrected to 'Presence'. This is the key to fixing the 0%% precision.*

%s

---
*Report generated by absence_validation_analysis.R*
",
    format(Sys.Date(), "%Y-%m-%d"),
    nrow(data),
    n_distinct(data$validator_name),
    min(data$validation_date, na.rm = TRUE),
    max(data$validation_date, na.rm = TRUE),
    paste(knitr::kable(pattern_analysis$label_distribution, format = "markdown"), collapse = "\n"), 
    paste(knitr::kable(precision_metrics$method_precision, format = "markdown"), collapse = "\n"), 
    paste(knitr::kable(precision_metrics$confidence_performance, format = "markdown"), collapse = "\n"), 
    paste(knitr::kable(pattern_analysis$method_effectiveness, format = "markdown"), collapse = "\n"), 
    paste(knitr::kable(pattern_analysis$confidence_analysis, format = "markdown"), collapse = "\n"), 
    paste(knitr::kable(pattern_analysis$detection_vs_manual, format = "markdown"), collapse = "\n"), 
    # Dynamic recommendations
    worst_method,
    confidence_to_review,
    qc_confidence,
    # Data quality
    nrow(notes_df),
    avg_notes_len,
    irrelevant_count,
    irrelevant_pct,
    # Actionable notes
    fp_notes_table 
  )

  # Write report
  writeLines(report_content, file.path(output_dir, "absence_validation_report.md"))
  cat("Report saved to:", file.path(output_dir, "absence_validation_report.md"), "\n")

  return(report_content)
}

#' Main analysis function
#' @param input_file Path to input CSV file
#' @param output_dir Output directory for results
main <- function(input_file = "results/manual_validation/absence_validation_sample_for_manual_review_BB.csv",
                 output_dir = "results/manual_validation/analysis") {

  # Read and validate data
  cat("Reading validation data...\n")
  data <- read_validation_data(input_file)
  cat(sprintf("Loaded %d validation records\n", nrow(data)))

  # Generate analysis report
  cat("Generating analysis report...\n")
  report <- generate_report(data, output_dir)

  # Save processed data
  write_csv(data, file.path(output_dir, "processed_validation_data.csv"))

  cat("Analysis complete!\n")
  cat("Results saved to:", output_dir, "\n")

  return(list(data = data, report = report))
}

# Run analysis if script is called directly
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- if (length(args) >= 1) args[1] else "results/manual_validation/absence_validation_sample_for_manual_review_BB.csv"
  output_dir <- if (length(args) >= 2) args[2] else "results/manual_validation/analysis"

  main(input_file, output_dir)
}