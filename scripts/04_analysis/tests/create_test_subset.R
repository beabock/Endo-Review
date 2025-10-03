# Create Test Subset for Pipeline Testing
# B. Bock
# Script to create representative subsets of the dataset for pipeline testing
# Allows testing pipeline components without running on full dataset

library(tidyverse)

cat("=== DATASET SUBSAMPLING TOOL ===\n")
cat("Creating representative subsets for pipeline testing\n\n")

# Main function to create test subsets
create_test_subset <- function(
  input_file = "results/relevant_abstracts_with_pa_predictions.csv",
  output_dir = "test_data",
  sample_sizes = c(100, 500, 1000),
  sampling_method = "random",  # "random", "stratified", "balanced"
  stratification_column = "final_classification",
  seed = 1998,
  verbose = TRUE
) {

  # Set seed for reproducibility
  set.seed(seed)

  # Validate inputs
  if (!file.exists(input_file)) {
    stop("❌ Input file not found: ", input_file)
  }

  if (!sampling_method %in% c("random", "stratified", "balanced")) {
    stop("❌ Invalid sampling method. Use: 'random', 'stratified', or 'balanced'")
  }

  if (verbose) {
    cat("📊 Creating test subsets from:", input_file, "\n")
    cat("🎯 Sampling method:", sampling_method, "\n")
    cat("📏 Sample sizes:", paste(sample_sizes, collapse = ", "), "\n")
    if (sampling_method == "stratified") {
      cat("📋 Stratification column:", stratification_column, "\n")
    }
    cat("\n")
  }

  # Load data
  if (verbose) cat("📥 Loading dataset...\n")
  data <- read_csv(input_file, show_col_types = FALSE)

  if (verbose) {
    cat("   Dataset size:", nrow(data), "rows\n")
    cat("   Available columns:", paste(names(data), collapse = ", "), "\n\n")
  }

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Create subsets for each sample size
  for (sample_size in sample_sizes) {
    if (verbose) cat("🔄 Creating subset of", sample_size, "abstracts...\n")

    subset_data <- create_subset(
      data = data,
      sample_size = sample_size,
      method = sampling_method,
      stratification_column = stratification_column,
      verbose = verbose
    )

    # Save subset
    output_file <- file.path(output_dir, sprintf("test_subset_%s_%d.csv", sampling_method, sample_size))
    write_csv(subset_data, output_file)

    if (verbose) {
      cat("   ✅ Saved to:", output_file, "\n")
      cat("   📊 Subset statistics:\n")

      # Show basic statistics
      if ("final_classification" %in% names(subset_data)) {
        class_counts <- subset_data %>%
          count(final_classification) %>%
          arrange(desc(n))
        cat("      Classifications:", paste(class_counts$final_classification, class_counts$n, sep=": ", collapse=", "), "\n")
      }

      if ("confidence" %in% names(subset_data)) {
        cat("      Confidence range:", round(min(subset_data$confidence, na.rm=TRUE), 3),
            "to", round(max(subset_data$confidence, na.rm=TRUE), 3), "\n")
      }

      cat("\n")
    }
  }

  # Create a summary report
  create_summary_report(
    data = data,
    sample_sizes = sample_sizes,
    sampling_method = sampling_method,
    stratification_column = stratification_column,
    output_dir = output_dir,
    original_file = input_file
  )

  if (verbose) {
    cat("🎉 Subset creation complete!\n")
    cat("📁 Output directory:", output_dir, "\n")
    cat("📋 Files created:\n")
    for (sample_size in sample_sizes) {
      cat("   - test_subset_", sampling_method, "_", sample_size, ".csv\n", sep="")
    }
    cat("   - subset_summary.txt\n")
    cat("\n💡 Next steps:\n")
    cat("   1. Test pipeline on small subset first\n")
    cat("   2. Gradually increase subset size\n")
    cat("   3. Compare results consistency\n")
    cat("   4. Run on full dataset when ready\n")
  }

  return(list(
    output_dir = output_dir,
    sample_sizes = sample_sizes,
    method = sampling_method
  ))
}

# Function to create subset based on sampling method
create_subset <- function(data, sample_size, method, stratification_column, verbose = TRUE) {

  if (method == "random") {
    # Simple random sampling
    if (sample_size >= nrow(data)) {
      return(data)
    }
    subset_indices <- sample(1:nrow(data), sample_size, replace = FALSE)
    return(data[subset_indices, ])

  } else if (method == "stratified") {
    # Stratified sampling based on classification
    if (!stratification_column %in% names(data)) {
      warning("Stratification column not found, falling back to random sampling")
      return(create_subset(data, sample_size, "random", stratification_column, verbose))
    }

    # Get strata
    strata <- data %>%
      group_by(.data[[stratification_column]]) %>%
      group_split()

    # Calculate proportional allocation
    total_rows <- nrow(data)
    strata_sizes <- sapply(strata, nrow)
    strata_props <- strata_sizes / total_rows

    # Allocate samples proportionally
    allocations <- round(sample_size * strata_props)
    allocations <- pmax(allocations, 1)  # Ensure at least 1 sample per stratum

    # Adjust if total allocation exceeds sample size
    while (sum(allocations) > sample_size) {
      allocations[which.max(allocations)] <- allocations[which.max(allocations)] - 1
    }

    # Sample from each stratum
    subset_list <- list()
    for (i in seq_along(strata)) {
      stratum_data <- strata[[i]]
      stratum_size <- allocations[i]

      if (stratum_size >= nrow(stratum_data)) {
        subset_list[[i]] <- stratum_data
      } else {
        subset_indices <- sample(1:nrow(stratum_data), stratum_size, replace = FALSE)
        subset_list[[i]] <- stratum_data[subset_indices, ]
      }
    }

    return(bind_rows(subset_list))

  } else if (method == "balanced") {
    # Balanced sampling - ensure representation across key variables
    if (!stratification_column %in% names(data)) {
      warning("Stratification column not found, falling back to random sampling")
      return(create_subset(data, sample_size, "random", stratification_column, verbose))
    }

    # Get unique classes
    classes <- unique(data[[stratification_column]])
    n_classes <- length(classes)

    if (sample_size < n_classes) {
      # If sample size is smaller than number of classes, take one from each
      subset_data <- data %>%
        group_by(.data[[stratification_column]]) %>%
        slice_sample(n = 1) %>%
        ungroup()

      # If we still need more samples, add random samples
      remaining <- sample_size - nrow(subset_data)
      if (remaining > 0) {
        remaining_indices <- sample(setdiff(1:nrow(data), subset_data$id), remaining, replace = FALSE)
        remaining_data <- data[remaining_indices, ]
        subset_data <- bind_rows(subset_data, remaining_data)
      }

      return(subset_data)
    }

    # Balanced allocation
    base_allocation <- floor(sample_size / n_classes)
    extra_samples <- sample_size %% n_classes

    allocations <- rep(base_allocation, n_classes)
    if (extra_samples > 0) {
      extra_indices <- sample(1:n_classes, extra_samples, replace = FALSE)
      allocations[extra_indices] <- allocations[extra_indices] + 1
    }

    # Sample from each class
    subset_list <- list()
    for (i in seq_along(classes)) {
      class_data <- data %>% filter(.data[[stratification_column]] == classes[i])
      class_size <- allocations[i]

      if (class_size >= nrow(class_data)) {
        subset_list[[i]] <- class_data
      } else {
        subset_indices <- sample(1:nrow(class_data), class_size, replace = FALSE)
        subset_list[[i]] <- class_data[subset_indices, ]
      }
    }

    return(bind_rows(subset_list))
  }
}

# Function to create summary report
create_summary_report <- function(data, sample_sizes, sampling_method, stratification_column,
                                  output_dir, original_file) {

  capture.output({
    cat("=== DATASET SUBSAMPLING SUMMARY ===\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

    cat("ORIGINAL DATASET:\n")
    cat("File:", original_file, "\n")
    cat("Total abstracts:", nrow(data), "\n")
    cat("Columns:", paste(names(data), collapse = ", "), "\n\n")

    if (stratification_column %in% names(data)) {
      cat("CLASS DISTRIBUTION:\n")
      class_dist <- data %>%
        count(.data[[stratification_column]]) %>%
        arrange(desc(n))
      print(class_dist)
      cat("\n")
    }

    if ("confidence" %in% names(data)) {
      cat("CONFIDENCE STATISTICS:\n")
      cat("Mean confidence:", round(mean(data$confidence, na.rm = TRUE), 3), "\n")
      cat("Median confidence:", round(median(data$confidence, na.rm = TRUE), 3), "\n")
      cat("Range:", round(min(data$confidence, na.rm = TRUE), 3),
          "to", round(max(data$confidence, na.rm = TRUE), 3), "\n\n")
    }

    cat("SAMPLING PARAMETERS:\n")
    cat("Method:", sampling_method, "\n")
    cat("Sample sizes:", paste(sample_sizes, collapse = ", "), "\n")
    if (sampling_method == "stratified") {
      cat("Stratification column:", stratification_column, "\n")
    }
    cat("\n")

    cat("CREATED SUBSETS:\n")
    for (sample_size in sample_sizes) {
      cat("-", sprintf("test_subset_%s_%d.csv", sampling_method, sample_size), "\n")
    }
    cat("\n")

    cat("USAGE RECOMMENDATIONS:\n")
    cat("1. Start with smallest subset (100) for quick testing\n")
    cat("2. Test pipeline components individually\n")
    cat("3. Gradually increase subset size\n")
    cat("4. Compare results across subset sizes\n")
    cat("5. Run full pipeline on largest subset\n")
    cat("6. Proceed to full dataset when confident\n\n")

    cat("EXAMPLE PIPELINE TESTING:\n")
    cat("# Test on small subset\n")
    cat("source('scripts/04_analysis/run_extraction_pipeline.R')\n")
    cat("run_extraction_pipeline(\n")
    cat("  input_file = 'test_data/test_subset_random_100.csv',\n")
    cat("  run_species = TRUE,\n")
    cat("  run_methods = TRUE,\n")
    cat("  run_parts = TRUE,\n")
    cat("  run_geography = TRUE,\n")
    cat("  run_merge = TRUE\n")
    cat(")\n\n")

    cat("PIPELINE TESTING CHECKLIST:\n")
    cat("□ Test each component individually\n")
    cat("□ Verify output file creation\n")
    cat("□ Check result quality and consistency\n")
    cat("□ Monitor memory usage\n")
    cat("□ Time each component\n")
    cat("□ Compare results across subset sizes\n")
    cat("□ Validate against known examples\n")

  }, file = file.path(output_dir, "subset_summary.txt"))
}

# Quick test functions
test_pipeline_on_subset <- function(subset_file, components = c("species", "methods", "parts", "geography")) {
  # This function helps run quick tests on subsets

  if (!file.exists(subset_file)) {
    stop("Subset file not found: ", subset_file)
  }

  cat("🧪 Testing pipeline on subset:", basename(subset_file), "\n")

  # Load and check subset
  subset_data <- read_csv(subset_file, show_col_types = FALSE)
  cat("   Loaded", nrow(subset_data), "abstracts\n")

  # Check required columns
  required_cols <- c("id", "article_title", "abstract")
  missing_cols <- setdiff(required_cols, names(subset_data))
  if (length(missing_cols) > 0) {
    cat("   ⚠️  Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  } else {
    cat("   ✅ All required columns present\n")
  }

  # Show sample data
  if (nrow(subset_data) > 0) {
    cat("   📝 Sample abstract:\n")
    sample_abstract <- subset_data$abstract[1]
    if (nchar(sample_abstract) > 100) {
      sample_abstract <- paste0(substr(sample_abstract, 1, 100), "...")
    }
    cat("      ", sample_abstract, "\n")
  }

  cat("\n💡 Run pipeline with:\n")
  cat("source('scripts/04_analysis/run_extraction_pipeline.R')\n")
  cat("run_extraction_pipeline(\n")
  cat("  input_file = '", subset_file, "',\n", sep="")
  cat("  run_data_prep = TRUE,\n")
  for (comp in components) {
    cat("  run_", comp, " = TRUE,\n", sep="")
  }
  cat("  force_rerun = TRUE\n")
  cat(")\n\n")
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "create_test_subset.R")) {

  # Default: Create multiple subset sizes with random sampling
  result <- create_test_subset(
    sample_sizes = c(100, 500, 1000),
    sampling_method = "random",
    verbose = TRUE
  )

  cat("\n✅ Test subset creation complete!\n")
  cat("📁 Check the 'test_data' folder for your subsets\n")
}

# USAGE EXAMPLES:
#
# # Basic usage - create random subsets of different sizes
# create_test_subset(sample_sizes = c(100, 500, 1000))
#
# # Stratified sampling based on classification
# create_test_subset(
#   sample_sizes = c(200, 1000),
#   sampling_method = "stratified",
#   stratification_column = "final_classification"
# )
#
# # Balanced sampling to ensure representation
# create_test_subset(
#   sample_sizes = c(150, 750),
#   sampling_method = "balanced"
# )
#
# # Custom input file
# create_test_subset(
#   input_file = "path/to/your/data.csv",
#   sample_sizes = c(50, 200)
# )
#
# # Test pipeline on subset
# test_pipeline_on_subset("test_data/test_subset_random_100.csv")
