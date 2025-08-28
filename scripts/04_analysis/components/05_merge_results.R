# Merge Results Component
# B. Bock - Modular Version
# July 31, 2025 - Results merging (fast ~5-15 min)
#
# This script merges all extraction results into comprehensive dataset
# Part of the modular extraction pipeline

library(tidyverse)

cat("=== MERGE RESULTS COMPONENT ===\n")
cat("Combining all extraction results into comprehensive dataset\n\n")

# Main merge function
merge_extraction_results <- function(
  output_file = "results/comprehensive_extraction_results.csv",
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Recovery mechanism
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing comprehensive results\n")
    existing_results <- read_csv(output_file, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
    return(existing_results)
  }

  # Check for base abstracts data
  abstracts_file <- "results/prepared_abstracts_for_extraction.csv"
  if (!file.exists(abstracts_file)) {
    stop("❌ Base abstracts data not found: ", abstracts_file)
  }

  if (verbose) cat("🔬 Starting results merge process\n")

  # Load base abstracts data
  base_data <- read_csv(abstracts_file, show_col_types = FALSE)
  if (verbose) cat("   Loaded base abstracts:", nrow(base_data), "records\n")

  # Initialize merged data with base information
  merged_data <- base_data

  # List of component result files to merge
  component_files <- c(
    "results/species_detection_results.csv",
    "results/methods_detection_results.csv",
    "results/plant_parts_detection_results.csv",
    "results/geography_detection_results.csv"
  )

  component_names <- c("species", "methods", "plant_parts", "geography")
  loaded_components <- 0

  # Merge each component
  for (i in seq_along(component_files)) {
    file_path <- component_files[i]
    component_name <- component_names[i]

    if (file.exists(file_path)) {
      if (verbose) cat("   📥 Loading", component_name, "results...\n")

      component_data <- read_csv(file_path, show_col_types = FALSE)

      # Merge with base data
      merged_data <- merged_data %>%
        left_join(component_data, by = "id")

      loaded_components <- loaded_components + 1
      if (verbose) cat("      ✅ Merged", nrow(component_data), "records\n")

    } else {
      if (verbose) cat("   ⚠️  ", component_name, "results not found (skipping)\n")
    }
  }

  if (loaded_components == 0) {
    stop("❌ No component result files found. Run individual extraction components first.")
  }

  if (verbose) {
    cat("   📊 Successfully merged", loaded_components, "of", length(component_files), "components\n")
  }

  # Remove duplicate columns that might have been created during merging
  # Keep the first occurrence of each column name
  merged_data <- merged_data %>%
    select(-matches("\\.y$")) %>%  # Remove .y suffix columns (duplicates)
    rename_with(~str_remove(., "\\.x$"), matches("\\.x$"))  # Remove .x suffix

  # Save comprehensive results
  write_csv(merged_data, output_file)

  # Generate summary report
  if (verbose) {
    cat("\n📈 Comprehensive Extraction Results Summary:\n")
    cat("==========================================\n")
    cat("Total abstracts processed:", nrow(merged_data), "\n")

    # Species summary
    if ("resolved_name" %in% names(merged_data) || "canonicalName" %in% names(merged_data)) {
      species_cols <- intersect(c("resolved_name", "canonicalName"), names(merged_data))
      species_found <- merged_data %>%
        select(any_of(species_cols)) %>%
        mutate(has_species = rowSums(!is.na(across(everything()))) > 0) %>%
        pull(has_species) %>%
        sum()

      cat("✓ Species detection:", species_found,
          "(", round(100 * species_found / nrow(merged_data), 1), "%)\n")
    }

    # Methods summary
    if ("molecular_methods" %in% names(merged_data)) {
      molecular <- sum(merged_data$molecular_methods, na.rm = TRUE)
      culture <- sum(merged_data$culture_based_methods, na.rm = TRUE)
      microscopy <- sum(merged_data$microscopy_methods, na.rm = TRUE)

      cat("✓ Methods detection: Molecular(", molecular, "), Culture(", culture, "), Microscopy(", microscopy, ")\n")
    }

    # Plant parts summary
    if ("plant_parts_detected" %in% names(merged_data)) {
      parts_found <- sum(!is.na(merged_data$plant_parts_detected))
      cat("✓ Plant parts detection:", parts_found,
          "(", round(100 * parts_found / nrow(merged_data), 1), "%)\n")
    }

    # Geography summary
    if ("countries_detected" %in% names(merged_data)) {
      countries_found <- sum(!is.na(merged_data$countries_detected))
      continents_found <- sum(!is.na(merged_data$continents_detected))
      regions_found <- sum(!is.na(merged_data$regions_detected))

      cat("✓ Geography detection: Countries(", countries_found, "), Continents(", continents_found, "), Regions(", regions_found, ")\n")
    }

    cat("\n💾 Results saved to:", output_file, "\n")
    cat("📊 Total columns:", ncol(merged_data), "\n")
  }

  return(merged_data)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "05_merge_results.R")) {

  # Merge all results
  comprehensive_results <- merge_extraction_results()

  cat("\n✅ Results merge component completed!\n")
}
