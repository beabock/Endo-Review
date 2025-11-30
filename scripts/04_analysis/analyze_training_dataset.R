# Analyze Training Dataset Script
# This script loads and analyzes the training dataset: Training_labeled_abs_6.csv
# Dataset location: data/raw/Training_labeled_abs_6.csv

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Set working directory to project root
setwd("C:/Users/beabo/OneDrive - Northern Arizona University/NAU/Endo-Review")

# Load the dataset
ds_path <- "data/raw/Training_labeled_abs_6.csv"
if (!file.exists(ds_path)) {
  stop("Training dataset not found at: ", ds_path)
}

# Read with proper encoding handling
ds <- tryCatch({
  read.csv(ds_path, stringsAsFactors = FALSE, encoding = "UTF-8")
}, error = function(e) {
  message("UTF-8 encoding failed, trying latin1...")
  read.csv(ds_path, stringsAsFactors = FALSE, encoding = "latin1")
})

# Basic dataset information
cat("Dataset dimensions:", dim(ds), "\n")
cat("Column names:\n")
print(colnames(ds))

# Summary statistics
cat("\n=== BASIC SUMMARY STATISTICS ===\n")
summary(ds)

# Analyze key columns
cat("\n=== LABEL ANALYSIS ===\n")
if ("label" %in% colnames(ds)) {
  cat("Label distribution:\n")
  print(table(ds$label, useNA = "ifany"))
  cat("Percentage distribution:\n")
  print(prop.table(table(ds$label)) * 100)

  # Origin distribution by label
  if ("origin" %in% colnames(ds)) {
    cat("\nOrigin distribution by label:\n")
   origin_by_label <- table(ds$origin, ds$label, useNA = "ifany")

# Add a row-sum column
    origin_by_label_with_sum <- cbind(origin_by_label,
                                  RowSum = rowSums(origin_by_label))
    print(origin_by_label_with_sum)
    cat("\nOrigin percentages by label:\n")
    print(prop.table(origin_by_label, margin = 2) * 100)
  }
}

if ("predicted_label" %in% colnames(ds)) {
  cat("\nPredicted label distribution:\n")
  print(table(ds$predicted_label, useNA = "ifany"))
}

cat("\n=== PUBLICATION YEAR ANALYSIS ===\n")
if ("publication_year" %in% colnames(ds)) {
  ds$publication_year <- as.numeric(ds$publication_year)
  cat("Publication year summary:\n")
  print(summary(ds$publication_year))
  cat("Unique years:", length(unique(ds$publication_year)), "\n")

  # Plot publication year distribution
  p_year <- ggplot(ds, aes(x = publication_year)) +
    geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
    labs(title = "Distribution of Publication Years",
         x = "Publication Year", y = "Frequency") +
    theme_minimal()
  ggsave("plots/training_publication_years.png", p_year, width = 8, height = 6)
  cat("Publication year histogram saved to plots/training_publication_years.png\n")
}

cat("\n=== ABSTRACT LENGTH ANALYSIS ===\n")
if ("abstract" %in% colnames(ds)) {
  # Handle encoding issues by using nchar with allowNA = TRUE
  ds$abstract_length <- sapply(ds$abstract, function(x) {
    tryCatch(nchar(x), error = function(e) NA)
  })
  cat("Abstract length summary:\n")
  print(summary(ds$abstract_length))

  # Plot abstract length distribution
  p_abstract <- ggplot(ds[!is.na(ds$abstract_length), ], aes(x = abstract_length)) +
    geom_histogram(bins = 50, fill = "green", alpha = 0.7) +
    labs(title = "Distribution of Abstract Lengths",
         x = "Abstract Length (characters)", y = "Frequency") +
    theme_minimal()
  ggsave("plots/training_abstract_lengths.png", p_abstract, width = 8, height = 6)
  cat("Abstract length histogram saved to plots/training_abstract_lengths.png\n")
}

cat("\n=== DOCUMENT TYPE ANALYSIS ===\n")
if ("document_type" %in% colnames(ds)) {
  cat("Document type distribution:\n")
  print(sort(table(ds$document_type), decreasing = TRUE))
}

cat("\n=== CITATION ANALYSIS ===\n")
if ("times_cited_all_databases" %in% colnames(ds)) {
  ds$times_cited_all_databases <- as.numeric(ds$times_cited_all_databases)
  cat("Citation summary:\n")
  print(summary(ds$times_cited_all_databases))

  # Plot citation distribution (log scale for better visualization)
  p_citations <- ggplot(ds, aes(x = times_cited_all_databases)) +
    geom_histogram(bins = 50, fill = "red", alpha = 0.7) +
    scale_x_log10() +
    labs(title = "Distribution of Citations (Log Scale)",
         x = "Times Cited (All Databases)", y = "Frequency") +
    theme_minimal()
  ggsave("plots/training_citations.png", p_citations, width = 8, height = 6)
  cat("Citation histogram saved to plots/training_citations.png\n")
}

cat("\n=== MISSING DATA ANALYSIS ===\n")
missing_data <- sapply(ds, function(x) sum(is.na(x) | x == ""))
cat("Missing data per column:\n")
print(missing_data)
cat("Percentage missing per column:\n")
print((missing_data / nrow(ds)) * 100)

# Cross-tabulation of label vs predicted label if both exist
if ("label" %in% colnames(ds) && "predicted_label" %in% colnames(ds)) {
  cat("\n=== LABEL VS PREDICTED LABEL CROSS-TAB ===\n")
  confusion_matrix <- table(ds$label, ds$predicted_label, useNA = "ifany")
  print(confusion_matrix)

  # Calculate accuracy if labels are comparable
  if (length(unique(ds$label)) == length(unique(ds$predicted_label))) {
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    cat("Accuracy:", round(accuracy * 100, 2), "%\n")
  }
}

cat("\n=== TOP JOURNALS ===\n")
if ("source_title" %in% colnames(ds)) {
  cat("Top 10 source titles:\n")
  print(head(sort(table(ds$source_title), decreasing = TRUE), 10))
}

cat("\n=== TOP RESEARCH AREAS ===\n")
if ("research_areas" %in% colnames(ds)) {
  # Split research areas (assuming semicolon-separated), handle NA values
  research_areas <- unlist(strsplit(as.character(ds$research_areas), "; "))
  research_areas <- research_areas[!is.na(research_areas) & research_areas != ""]
  if (length(research_areas) > 0) {
    cat("Top 10 research areas:\n")
    print(head(sort(table(research_areas), decreasing = TRUE), 10))
  } else {
    cat("No research areas data available.\n")
  }
}

# Save summary to file
summary_output <- capture.output({
  cat("TRAINING DATASET ANALYSIS SUMMARY\n")
  cat("==================================\n\n")
  cat("Dataset:", ds_path, "\n")
  cat("Dimensions:", dim(ds), "\n")
  cat("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

  cat("Label Distribution:\n")
  if ("label" %in% colnames(ds)) {
    print(table(ds$label))
  }

  cat("\nPublication Year Range:", range(ds$publication_year, na.rm = TRUE), "\n")
  cat("Abstract Length Range:", range(ds$abstract_length, na.rm = TRUE), "\n")
  cat("Citation Range:", range(ds$times_cited_all_databases, na.rm = TRUE), "\n")

  cat("\nMissing Data Summary:\n")
  print(missing_data[missing_data > 0])
})

writeLines(summary_output, "results/training_dataset_analysis_summary.txt")
cat("\nSummary saved to results/training_dataset_analysis_summary.txt\n")

# Load presence validation data if available
presence_validation_path <- "results/manual_validation/presence_validation_sample_for_manual_review_BB.csv"
if (file.exists(presence_validation_path)) {
  cat("\n=== PRESENCE VALIDATION ANALYSIS ===\n")
  presence_ds <- read.csv(presence_validation_path, stringsAsFactors = FALSE)

  cat("Presence validation dataset dimensions:", dim(presence_ds), "\n")

  if ("manual_label" %in% colnames(presence_ds)) {
    cat("Manual label distribution:\n")
    print(table(presence_ds$manual_label, useNA = "ifany"))
  }

  if ("predicted_label" %in% colnames(presence_ds)) {
    cat("\nPredicted label distribution:\n")
    print(table(presence_ds$predicted_label, useNA = "ifany"))
  }

  # Compare manual vs predicted labels
  if ("manual_label" %in% colnames(presence_ds) && "predicted_label" %in% colnames(presence_ds)) {
    cat("\nManual vs Predicted label comparison:\n")
    comparison_table <- table(presence_ds$manual_label, presence_ds$predicted_label, useNA = "ifany")
    print(comparison_table)

    # Calculate agreement metrics
    agreement <- sum(diag(comparison_table)) / sum(comparison_table)
    cat("Agreement rate:", round(agreement * 100, 2), "%\n")

    # Cohen's Kappa if possible
    if (nrow(comparison_table) == 2 && ncol(comparison_table) == 2) {
      # Simple Kappa calculation
      total <- sum(comparison_table)
      row_sums <- rowSums(comparison_table)
      col_sums <- colSums(comparison_table)
      expected <- sum(row_sums * col_sums) / total
      observed <- sum(diag(comparison_table))
      kappa <- (observed - expected) / (total - expected)
      cat("Cohen's Kappa:", round(kappa, 3), "\n")
    }
  }

  if ("manual_confidence" %in% colnames(presence_ds)) {
    presence_ds$manual_confidence <- as.numeric(presence_ds$manual_confidence)
    cat("\nManual confidence summary:\n")
    print(summary(presence_ds$manual_confidence))
  }

  if ("confidence" %in% colnames(presence_ds)) {
    presence_ds$confidence <- as.numeric(presence_ds$confidence)
    cat("\nModel confidence summary:\n")
    print(summary(presence_ds$confidence))
  }
} else {
  cat("\nPresence validation file not found at:", presence_validation_path, "\n")
}

cat("\nAnalysis complete. Check plots/ for visualizations and results/ for summary.\n")