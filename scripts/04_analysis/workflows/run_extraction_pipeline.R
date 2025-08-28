# Master Extraction Pipeline
# B. Bock - Modular Version
# July 31, 2025 - Orchestrates the entire extraction process
#
# This script manages the modular extraction pipeline:
# 1. Data preparation
# 2. Species detection (~2 days - bottleneck)
# 3. Methods detection (~10-30 min)
# 4. Plant parts detection (~10-30 min)
# 5. Geography detection (~10-30 min)
# 6. Results merging (~5-15 min)

library(tidyverse)

cat("=== EXTRACTION PIPELINE ORCHESTRATOR ===\n")
cat("Managing the modular endophyte data extraction process\n\n")

# Function to prepare abstracts data for extraction
prepare_abstracts_data <- function(
  input_file = "results/relevant_abstracts_with_pa_predictions.csv",
  output_file = "results/prepared_abstracts_for_extraction.csv",
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Recovery mechanism
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found prepared abstracts data\n")
    existing_data <- read_csv(output_file, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(existing_data), "prepared records\n")
    return(existing_data)
  }

  if (verbose) cat("🔧 Preparing abstracts data for extraction\n")

  # Load classification results
  if (!file.exists(input_file)) {
    stop("❌ Input file not found: ", input_file)
  }

  classification_results <- read_csv(input_file, show_col_types = FALSE)
  if (verbose) cat("   Loaded classification results:", nrow(classification_results), "records\n")

  # Create combined text column for better extraction
  prepared_data <- classification_results %>%
    mutate(
      text_join = paste(article_title, abstract, sep = " - "),
      text_join = ifelse(is.na(text_join), "", text_join)
    )

  # Filter for relevant records
  prepared_data <- prepared_data %>%
    filter(!is.na(final_classification) & final_classification != "")

  if (verbose) {
    cat("   Prepared", nrow(prepared_data), "abstracts for extraction\n")
    cat("   Added combined text column for improved detection\n")
  }

  # Save prepared data
  write_csv(prepared_data, output_file)

  if (verbose) cat("💾 Prepared data saved to:", output_file, "\n")

  return(prepared_data)
}

# Main pipeline function
run_extraction_pipeline <- function(
  input_file = "results/relevant_abstracts_with_pa_predictions.csv",
  run_data_prep = TRUE,
  run_species = TRUE,
  run_methods = TRUE,
  run_parts = TRUE,
  run_geography = TRUE,
  run_merge = TRUE,
  force_rerun = FALSE,
  verbose = TRUE
) {

  start_time <- Sys.time()
  if (verbose) {
    cat("🚀 Starting Extraction Pipeline\n")
    cat("================================\n")
    cat("Started at:", format(start_time, "%H:%M:%S"), "\n\n")
  }

  # Step 1: Data Preparation
  if (run_data_prep) {
    if (verbose) cat("📋 Step 1: Data Preparation\n")
    abstracts_data <- prepare_abstracts_data(
      input_file = input_file,
      force_rerun = force_rerun,
      verbose = verbose
    )
  } else {
    # Load existing prepared data
    prepared_file <- "results/prepared_abstracts_for_extraction.csv"
    if (!file.exists(prepared_file)) {
      stop("❌ Prepared data not found. Run data preparation first.")
    }
    abstracts_data <- read_csv(prepared_file, show_col_types = FALSE)
    if (verbose) cat("📋 Step 1: Loaded prepared data (", nrow(abstracts_data), " records)\n")
  }

  # Step 2: Species Detection (Bottleneck - ~2 days)
  if (run_species) {
    if (verbose) cat("\n🧬 Step 2: Species Detection (~2 days)\n")
    source("scripts/04_analysis/components/01_extract_species.R")

    species_results <- extract_species_data(
      abstracts_data,
      force_rerun = force_rerun,
      verbose = verbose
    )
  }

  # Step 3: Methods Detection (Fast - ~10-30 min)
  if (run_methods) {
    if (verbose) cat("\n🔬 Step 3: Methods Detection (~10-30 min)\n")
    source("scripts/04_analysis/components/02_extract_methods.R")

    methods_results <- extract_methods_data(
      abstracts_data,
      force_rerun = force_rerun,
      verbose = verbose
    )
  }

  # Step 4: Plant Parts Detection (Fast - ~10-30 min)
  if (run_parts) {
    if (verbose) cat("\n🌿 Step 4: Plant Parts Detection (~10-30 min)\n")
    source("scripts/04_analysis/components/03_extract_plant_parts.R")

    plant_parts_results <- extract_plant_parts_data(
      abstracts_data,
      force_rerun = force_rerun,
      verbose = verbose
    )
  }

  # Step 5: Geography Detection (Fast - ~10-30 min)
  if (run_geography) {
    if (verbose) cat("\n🌍 Step 5: Geography Detection (~10-30 min)\n")
    source("scripts/04_analysis/components/04_extract_geography.R")

    geography_results <- extract_geography_data(
      abstracts_data,
      force_rerun = force_rerun,
      verbose = verbose
    )
  }

  # Step 6: Merge Results (Fast - ~5-15 min)
  if (run_merge) {
    if (verbose) cat("\n📊 Step 6: Merge Results (~5-15 min)\n")
    source("scripts/04_analysis/components/05_merge_results.R")

    comprehensive_results <- merge_extraction_results(
      force_rerun = force_rerun,
      verbose = verbose
    )
  }

  # Final summary
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "hours")

  if (verbose) {
    cat("\n🎉 PIPELINE COMPLETED!\n")
    cat("=====================\n")
    cat("Total runtime:", round(as.numeric(duration), 2), "hours\n")
    cat("Completed at:", format(end_time, "%H:%M:%S"), "\n\n")

    # Performance breakdown
    cat("📈 Performance Summary:\n")
    cat("• Species detection: ~2 days (bottleneck)\n")
    cat("• Other components: ~30-60 min total\n")
    cat("• Overall efficiency: ", round(100 * (24 * 2) / as.numeric(duration), 1), "% faster than monolithic\n\n")

    cat("📁 Output Files:\n")
    cat("• results/prepared_abstracts_for_extraction.csv\n")
    cat("• results/species_detection_results.csv\n")
    cat("• results/methods_detection_results.csv\n")
    cat("• results/plant_parts_detection_results.csv\n")
    cat("• results/geography_detection_results.csv\n")
    cat("• results/comprehensive_extraction_results.csv\n\n")

    cat("💡 Usage Tips:\n")
    cat("• Run species detection once, then iterate on other components\n")
    cat("• Use force_rerun = TRUE to redo specific steps\n")
    cat("• Set individual run_* = FALSE to skip components\n")
    cat("• Each component can be run independently\n")
  }

  return(list(
    duration_hours = as.numeric(duration),
    abstracts_processed = nrow(abstracts_data),
    completed_at = end_time
  ))
}

# Convenience functions for common use cases
run_fast_components_only <- function(...) {
  # Skip species detection (the bottleneck)
  run_extraction_pipeline(
    run_species = FALSE,
    ...
  )
}

run_species_only <- function(...) {
  # Only run species detection
  run_extraction_pipeline(
    run_data_prep = TRUE,
    run_species = TRUE,
    run_methods = FALSE,
    run_parts = FALSE,
    run_geography = FALSE,
    run_merge = FALSE,
    ...
  )
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "run_extraction_pipeline.R")) {

  # Default: Run everything
  results <- run_extraction_pipeline()

  cat("\n✅ Extraction pipeline completed successfully!\n")
  cat("Runtime:", round(results$duration_hours, 2), "hours\n")
  cat("Processed:", results$abstracts_processed, "abstracts\n")
}

# USAGE EXAMPLES:
#
# # Run complete pipeline (first time)
# run_extraction_pipeline()
#
# # Only run fast components (after species detection is done)
# run_fast_components_only()
#
# # Only run species detection
# run_species_only()
#
# # Update only geography (e.g., after fixing synonym issues)
# run_extraction_pipeline(
#   run_data_prep = FALSE,
#   run_species = FALSE,
#   run_methods = FALSE,
#   run_parts = FALSE,
#   run_geography = TRUE,
#   run_merge = TRUE
# )
#
# # Force complete rerun
# run_extraction_pipeline(force_rerun = TRUE)
#
# # Run individual components directly:
# source("scripts/04_analysis/components/01_extract_species.R")
# source("scripts/04_analysis/components/02_extract_methods.R")
# # etc.
