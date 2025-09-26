# =============================================================================
# 03b_consolidate_presences.R - Presence Label Consolidation and Validation
# =============================================================================
#
# Purpose: Consolidate presence/absence labels from plant parts detection output
# using manual validation labels as ground truth, perform validation analysis,
# and filter for publication-quality data
#
# Description: This script takes the output from 03_extract_plant_parts.R and
# merges it with manual validation labels from the presence validation file.
# It overrides model predictions with manual labels, performs validation
# analysis, and filters the dataset to include only Presence and Absence labels.
#
# Dependencies: tidyverse, stringr, caret, yardstick
#
# Author: B. Bock
# Date: 2024-09-26
#
# Inputs/Outputs:
# - Input: results/plant_parts_detection_results.csv (from 03_extract_plant_parts)
# - Input: results/manual_validation/presence_validation_sample_for_manual_review.csv
# - Output: results/consolidated_presence_labels.csv
# - Output: results/presence_validation_analysis.txt
#
# =============================================================================

library(tidyverse)
library(stringr)
library(caret)      # For confusion matrix
library(yardstick)  # For performance metrics

# Source utilities if available
if (file.exists("scripts/04_analysis/utilities/reference_data_utils.R")) {
  source("scripts/04_analysis/utilities/reference_data_utils.R")
}

cat("=== PRESENCE LABEL CONSOLIDATION AND VALIDATION ===\n")
cat("Consolidating presence/absence labels with manual validation\n\n")

# =============================================================================
# Data Loading Functions
# =============================================================================

load_plant_parts_data <- function(file_path = "results/plant_parts_detection_results.csv") {
  if (!file.exists(file_path)) {
    stop("❌ Plant parts detection results not found. Run 03_extract_plant_parts.R first.")
  }

  cat("📖 Loading plant parts detection data from:", file_path, "\n")
  data <- read_csv(file_path, show_col_types = FALSE)

  if (nrow(data) == 0) {
    stop("❌ Plant parts detection results file is empty.")
  }

  cat("   ✅ Loaded", nrow(data), "records with", ncol(data), "columns\n")
  return(data)
}

load_manual_validation_data <- function(file_path = "results/manual_validation/presence_validation_sample_for_manual_review.csv") {
  if (!file.exists(file_path)) {
    stop("❌ Manual validation file not found at:", file_path)
  }

  cat("📖 Loading manual validation data from:", file_path, "\n")
  data <- read_csv(file_path, show_col_types = FALSE)

  if (nrow(data) == 0) {
    stop("❌ Manual validation file is empty.")
  }

  cat("   ✅ Loaded", nrow(data), "validation records\n")
  return(data)
}

# =============================================================================
# Label Consolidation Functions
# =============================================================================

consolidate_labels <- function(plant_parts_data, manual_validation_data) {
  cat("🔄 Consolidating labels with manual validation...\n")

  # Identify common columns for merging
  common_ids <- intersect(plant_parts_data$id, manual_validation_data$id)

  if (length(common_ids) == 0) {
    warning("⚠️  No common IDs found between plant parts data and manual validation data")
    return(NULL)
  }

  cat("   Found", length(common_ids), "common records for consolidation\n")

  # Merge datasets based on ID
  consolidated <- plant_parts_data %>%
    inner_join(manual_validation_data, by = "id") %>%
    mutate(
      # Create consolidated label using manual labels as ground truth
      original_prediction = final_classification,
      consolidated_label = manual_label,

      # Create confidence tracking
      manual_confidence_level = factor(manual_confidence, levels = c("Low", "Medium", "High")),

      # Track label changes
      label_changed = final_classification != manual_label,

      # Create validation status
      validation_status = case_when(
        manual_label %in% c("Presence", "Absence") ~ "Validated",
        manual_label %in% c("Review", "Irrelevant") ~ "Filtered",
        TRUE ~ "Unknown"
      )
    )

  cat("   ✅ Consolidated", nrow(consolidated), "records\n")
  return(consolidated)
}

# =============================================================================
# Validation Analysis Functions
# =============================================================================

perform_validation_analysis <- function(consolidated_data) {
  cat("📊 Performing validation analysis...\n")

  # Filter for records where we have both predictions and manual labels
  validation_subset <- consolidated_data %>%
    filter(!is.na(original_prediction) & !is.na(manual_label)) %>%
    filter(manual_label %in% c("Presence", "Absence", "Both")) %>%
    mutate(
      # Convert "Both" to "Presence" for analysis purposes
      analysis_prediction = if_else(original_prediction == "Both", "Presence", original_prediction),
      analysis_manual = if_else(manual_label == "Both", "Presence", manual_label)
    )

  if (nrow(validation_subset) == 0) {
    cat("⚠️  No valid records for validation analysis\n")
    return(NULL)
  }

  # Calculate confusion matrix
  confusion_matrix <- caret::confusionMatrix(
    factor(validation_subset$analysis_prediction),
    factor(validation_subset$analysis_manual),
    positive = "Presence"
  )

  # Calculate detailed metrics
  metrics <- yardstick::metrics(
    validation_subset,
    truth = factor(analysis_manual),
    estimate = factor(analysis_prediction)
  )

  # Calculate agreement statistics
  agreement_stats <- validation_subset %>%
    summarise(
      total_records = n(),
      agreements = sum(analysis_prediction == analysis_manual, na.rm = TRUE),
      disagreements = sum(analysis_prediction != analysis_manual, na.rm = TRUE),
      agreement_rate = agreements / total_records,
      label_changes = sum(label_changed, na.rm = TRUE),
      high_confidence = sum(manual_confidence == "High", na.rm = TRUE),
      medium_confidence = sum(manual_confidence == "Medium", na.rm = TRUE),
      low_confidence = sum(manual_confidence == "Low", na.rm = TRUE)
    )

  analysis_results <- list(
    confusion_matrix = confusion_matrix,
    metrics = metrics,
    agreement_stats = agreement_stats,
    validation_subset = validation_subset
  )

  cat("   ✅ Analysis completed for", nrow(validation_subset), "records\n")
  return(analysis_results)
}

# =============================================================================
# Filtering Functions
# =============================================================================

filter_publication_data <- function(consolidated_data) {
  cat("🔍 Filtering data for publication (Presence/Absence only)...\n")

  # Filter for only validated Presence and Absence labels
  filtered_data <- consolidated_data %>%
    filter(
      validation_status == "Validated",
      consolidated_label %in% c("Presence", "Absence")
    ) %>%
    mutate(
      # Ensure consistent labeling
      presence_label = factor(consolidated_label, levels = c("Absence", "Presence")),
      confidence_numeric = case_when(
        manual_confidence == "High" ~ 3,
        manual_confidence == "Medium" ~ 2,
        manual_confidence == "Low" ~ 1,
        TRUE ~ 1
      )
    )

  if (nrow(filtered_data) == 0) {
    cat("⚠️  No records meet publication criteria\n")
    return(NULL)
  }

  # Calculate filtering statistics
  filtering_stats <- consolidated_data %>%
    summarise(
      original_records = n(),
      validated_records = sum(validation_status == "Validated", na.rm = TRUE),
      presence_records = sum(consolidated_label == "Presence", na.rm = TRUE),
      absence_records = sum(consolidated_label == "Absence", na.rm = TRUE),
      filtered_records = sum(validation_status == "Filtered", na.rm = TRUE),
      filtered_out = original_records - validated_records,
      publication_ready = nrow(filtered_data)
    )

  cat("   ✅ Filtered to", nrow(filtered_data), "publication-ready records\n")

  results <- list(
    filtered_data = filtered_data,
    filtering_stats = filtering_stats
  )

  return(results)
}

# =============================================================================
# Reporting Functions
# =============================================================================

generate_analysis_report <- function(analysis_results, filtering_results, output_file = "results/presence_validation_analysis.txt") {
  cat("📝 Generating analysis report...\n")

  if (is.null(analysis_results) || is.null(filtering_results)) {
    cat("⚠️  Insufficient data for report generation\n")
    return(NULL)
  }

  # Create report content
  report_content <- paste0(
    "PRESENCE VALIDATION ANALYSIS REPORT\n",
    "===================================\n\n",
    "Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",

    "VALIDATION PERFORMANCE\n",
    "----------------------\n",
    "Total records analyzed: ", analysis_results$agreement_stats$total_records, "\n",
    "Agreement rate: ", round(analysis_results$agreement_stats$agreement_rate * 100, 1), "%\n",
    "Label changes: ", analysis_results$agreement_stats$label_changes, "\n\n",

    "Confusion Matrix:\n",
    capture.output(print(analysis_results$confusion_matrix)), "\n\n",

    "Detailed Metrics:\n",
    capture.output(print(analysis_results$metrics)), "\n\n",

    "Confidence Distribution:\n",
    "- High confidence: ", analysis_results$agreement_stats$high_confidence, "\n",
    "- Medium confidence: ", analysis_results$agreement_stats$medium_confidence, "\n",
    "- Low confidence: ", analysis_results$agreement_stats$low_confidence, "\n\n",

    "FILTERING RESULTS\n",
    "-----------------\n",
    "Original records: ", filtering_results$filtering_stats$original_records, "\n",
    "Publication-ready records: ", filtering_results$filtering_stats$publication_ready, "\n",
    "Presence labels: ", filtering_results$filtering_stats$presence_records, "\n",
    "Absence labels: ", filtering_results$filtering_stats$absence_records, "\n",
    "Filtered out: ", filtering_results$filtering_stats$filtered_out, "\n\n",

    "RECOMMENDATIONS\n",
    "---------------\n",
    "✓ Use consolidated presence labels for publication\n",
    "✓ High confidence manual labels provide reliable ground truth\n",
    "✓ Consider ", filtering_results$filtering_stats$presence_records + filtering_results$filtering_stats$absence_records,
    " validated records for statistical analysis\n",
    "✓ ", round((analysis_results$agreement_stats$agreement_rate) * 100, 1),
    "% model accuracy achieved with manual validation\n"
  )

  # Write report to file
  writeLines(report_content, output_file)
  cat("   ✅ Report saved to:", output_file, "\n")

  return(report_content)
}

# =============================================================================
# Main Consolidation Function
# =============================================================================

consolidate_presence_data <- function(
  plant_parts_file = "results/plant_parts_detection_results.csv",
  manual_validation_file = "results/manual_validation/presence_validation_sample_for_manual_review.csv",
  output_file = "results/consolidated_presence_labels.csv",
  analysis_file = "results/presence_validation_analysis.txt",
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Check for existing results
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing consolidated presence labels\n")
    existing_results <- read_csv(output_file, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
    return(existing_results)
  }

  if (verbose) {
    cat("🚀 Starting presence label consolidation and validation\n")
    cat("📁 Input files:\n")
    cat("   - Plant parts data:", plant_parts_file, "\n")
    cat("   - Manual validation:", manual_validation_file, "\n")
    cat("📄 Output files:\n")
    cat("   - Consolidated data:", output_file, "\n")
    cat("   - Analysis report:", analysis_file, "\n\n")
  }

  # Load data
  plant_parts_data <- load_plant_parts_data(plant_parts_file)
  manual_validation_data <- load_manual_validation_data(manual_validation_file)

  # Consolidate labels
  consolidated_data <- consolidate_labels(plant_parts_data, manual_validation_data)

  if (is.null(consolidated_data)) {
    stop("❌ Failed to consolidate data - no common records found")
  }

  # Perform validation analysis
  analysis_results <- perform_validation_analysis(consolidated_data)

  # Filter for publication
  filtering_results <- filter_publication_data(consolidated_data)

  if (is.null(filtering_results)) {
    warning("⚠️  No records meet publication criteria")
    # Still save consolidated data even if no publication-ready records
    write_csv(consolidated_data, output_file)
    return(consolidated_data)
  }

  # Generate analysis report
  report_content <- generate_analysis_report(analysis_results, filtering_results, analysis_file)

  # Save consolidated results
  write_csv(consolidated_data, output_file)

  # Save filtered publication-ready data
  publication_file <- str_replace(output_file, "\\.csv$", "_publication.csv")
  write_csv(filtering_results$filtered_data, publication_file)

  if (verbose) {
    cat("\n🎉 Presence consolidation and validation completed!\n")
    cat("📈 Key Results:\n")
    cat("   - Total consolidated records:", nrow(consolidated_data), "\n")
    cat("   - Publication-ready records:", filtering_results$filtering_stats$publication_ready, "\n")
    if (!is.null(analysis_results)) {
      cat("   - Model accuracy: ", round(analysis_results$agreement_stats$agreement_rate * 100, 1), "%\n")
    }
    cat("💾 Results saved to:\n")
    cat("   -", output_file, "\n")
    cat("   -", publication_file, "\n")
    cat("   -", analysis_file, "\n")
  }

  return(consolidated_data)
}

# =============================================================================
# Run if called directly
# =============================================================================

if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "03b_consolidate_presences.R")) {
  verbose <- TRUE

  # Run the consolidation with default parameters
  consolidated_results <- consolidate_presence_data(
    plant_parts_file = "results/plant_parts_detection_results.csv",
    manual_validation_file = "results/manual_validation/presence_validation_sample_for_manual_review.csv",
    output_file = "results/consolidated_presence_labels.csv",
    analysis_file = "results/presence_validation_analysis.txt",
    force_rerun = FALSE,
    verbose = TRUE
  )

  cat("\n✅ Presence label consolidation component completed!\n")
  cat("🔗 Successfully integrated plant parts data with manual validation\n")
  cat("📊 Generated comprehensive validation analysis report\n")
  cat("🎯 Filtered dataset ready for publication\n")
}