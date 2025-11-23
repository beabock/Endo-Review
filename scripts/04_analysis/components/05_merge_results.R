# =============================================================================
# 05_merge_results.R - Merge results component
# =============================================================================
#
# Purpose: Merge all extraction results into comprehensive dataset
#
# Description: Script that combines species+mycorrhizal, methods, plant parts, and geography detection results
# from individual extraction components into a single comprehensive dataset for downstream analysis
# and reporting. Uses memory-efficient approach starting from consolidated_dataset as base.
#
# Dependencies: tidyverse
#
# Author: B. Bock
# Date: 2024-09-26
#
# Inputs/Outputs: Reads consolidated_dataset and individual component result CSV files;
# outputs comprehensive_extraction_results.csv with merged data
#
# =============================================================================

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
  #if (file.exists(output_file) && !force_rerun) {
   # if (verbose) cat("✅ Found existing comprehensive results\n")
    #existing_results <- read_csv(output_file, show_col_types = FALSE)
    #if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
    #return(existing_results)
  #}

  # Check for consolidated dataset
  consolidated_file <- "results/consolidated_dataset.csv"
  if (!file.exists(consolidated_file)) {
    stop("❌ Consolidated dataset not found: ", consolidated_file)
  }

  if (verbose) cat("🔬 Starting results merge process\n")

  # Load consolidated dataset as base
  base_data <- read_csv(consolidated_file, show_col_types = FALSE)
  if (verbose) cat("   Loaded consolidated dataset:", nrow(base_data), "records with", ncol(base_data), "columns\n")

  # Initialize merged data with consolidated dataset
  merged_data <- base_data

  # List of component result files to merge (memory-efficient outputs)
  component_files <- c(
    "results/mycorrhizal_only_detailed_results.csv", #species detection + mycorrhizal
    "results/methods_detection_results.csv",
    "results/plant_parts_detection_results.csv",
    "results/geography_detection_results.csv"
  )

  component_names <- c("species_mycorrhizal", "methods", "plant_parts", "geography")
  loaded_components <- 0

  # Merge each component
  for (i in seq_along(component_files)) {
    file_path <- component_files[i]
    component_name <- component_names[i]

    if (file.exists(file_path)) {
      if (verbose) cat("   📥 Loading", component_name, "results...\n")

      component_data <- read_csv(file_path, show_col_types = FALSE)

      # Get columns that exist in both datasets (excluding 'id')
      common_cols <- intersect(names(merged_data), names(component_data))
      common_cols <- setdiff(common_cols, "id")

      if (length(common_cols) > 0) {
        if (verbose) cat("      ℹ️  Handling", length(common_cols), "overlapping columns\n")

        # For overlapping columns, we want to keep the component data (enhanced)
        # Remove overlapping columns from base data before joining
        merged_data <- merged_data %>%
          select(-all_of(common_cols))
      }

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

  # Step 1: Remove .y suffix columns (duplicates from right join)
  merged_data <- merged_data %>%
    select(-matches("\\.y$"))

  # Step 2: Handle any remaining .x suffixes by removing them
  x_cols <- names(merged_data)[str_ends(names(merged_data), "\\.x")]
  if (length(x_cols) > 0) {
    # Create a mapping from old names to new names
    new_names <- str_remove(x_cols, "\\.x$")
    names_map <- setNames(new_names, x_cols)

    # Only proceed if the mapping doesn't create duplicates
    if (length(unique(new_names)) == length(new_names) &&
        !any(new_names %in% setdiff(names(merged_data), x_cols))) {
      merged_data <- merged_data %>%
        rename(!!!names_map)
    }
  }

  # Step 3: Check for any remaining duplicate column names and handle them
  dup_cols <- names(merged_data)[duplicated(names(merged_data))]
  if (length(dup_cols) > 0) {
    if (verbose) cat("   ⚠️  Found duplicate columns:", paste(dup_cols, collapse = ", "), "- keeping first occurrence\n")

    # For each duplicate column name, keep only the first occurrence
    for (col_name in dup_cols) {
      col_positions <- which(names(merged_data) == col_name)
      if (length(col_positions) > 1) {
        # Keep the first occurrence, remove the rest
        cols_to_remove <- col_positions[2:length(col_positions)]
        merged_data <- merged_data %>%
          select(-all_of(col_positions[2:length(col_positions)]))
      }
    }
  }

  # Final check: ensure all column names are unique
  if (length(names(merged_data)) != length(unique(names(merged_data)))) {
    stop("❌ Still have duplicate column names after cleanup!")
  }

  # Clean up junky columns that start with X and are all NA
  na_cols <- names(merged_data)[str_starts(names(merged_data), "^X$|^X\\.")]
  if (length(na_cols) > 0) {
    cols_to_check <- merged_data %>%
      select(all_of(na_cols)) %>%
      summarise(across(everything(), ~all(is.na(.)))) %>%
      select(where(~.)) %>%
      names()

    if (length(cols_to_check) > 0) {
      if (verbose) cat("   🧹 Removing", length(cols_to_check), "NA-only columns starting with X\n")
      merged_data <- merged_data %>%
        select(-all_of(cols_to_check))
    }
  }

  # Remove training columns (they end with "_training")
  training_cols <- names(merged_data)[str_ends(names(merged_data), "_training")]
  if (length(training_cols) > 0) {
    if (verbose) cat("   🧹 Removing", length(training_cols), "training columns\n")
    merged_data <- merged_data %>%
      select(-all_of(training_cols))
  }


  # Generate summary report
  if (verbose) {
    cat("\n📈 Comprehensive Extraction Results Summary:\n")
    cat("==========================================\n")
    
    total_unique_abstracts <- length(unique(merged_data$id))
    cat("Total unique abstracts processed:", total_unique_abstracts, "\n")

    # Species and mycorrhizal summary
    if ("resolved_name" %in% names(merged_data) || "canonicalName" %in% names(merged_data)) {
      
      # Correctly count unique abstracts with species
      abstracts_with_species <- merged_data %>%
        filter(!is.na(resolved_name) | !is.na(canonicalName)) %>%
        distinct(id) %>%
        nrow()

      cat("✓ Species detection:", abstracts_with_species,
          "(", round(100 * abstracts_with_species / total_unique_abstracts, 1), "%)\n")
    }

    # Mycorrhizal summary
    if ("is_mycorrhizal" %in% names(merged_data)) {
      mycorrhizal_papers <- sum(merged_data$is_mycorrhizal_only, na.rm = TRUE)
      cat("✓ Mycorrhizal-only papers:", mycorrhizal_papers,
          "(", round(100 * mycorrhizal_papers / nrow(merged_data), 1), "%)\n")
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

  cat("Number of mycorrhizal only rows:", sum(merged_data$is_mycorrhizal_only, na.rm = TRUE), "\n")
  cat("Number of mycorrhizal rows:", sum(merged_data$is_mycorrhizal == "TRUE", na.rm = TRUE), "\n")
  cat("Unique abstracts before  filtering:", length(unique(merged_data$id)), "\n")
  cat("rows before filtering out mycorrhizal entries (is_mycorrhizal or Glomeromycota):", nrow(merged_data), "\n")
  cat("number of rows with phylum Glomeromycota:", sum(merged_data$phylum == "Glomeromycota", na.rm = TRUE), "\n")

    merged_data <- merged_data %>%
          filter(is_mycorrhizal == FALSE | is.na(is_mycorrhizal)) 

   cat("number of rows after filtering for is_mycorrhizal FALSE and NA:", nrow(merged_data), "\n")
   cat("number of unique abstracts after filtering for is_mycorrhizal FALSE and NA:", length(unique(merged_data$id)), "\n")

  merged_data <- merged_data %>%
          filter(phylum != "Glomeromycota" | is.na(phylum)) #AMF, just in case not captured by funguild
  # Save comprehensive results
  
  cat("Rows after filtering out mycorrhizal entries (is_mycorrhizal or Glomeromycota):", nrow(merged_data), "\n")
  
  cat("Number of mycorrhizal only rows after filtering:", sum(merged_data$is_mycorrhizal_only, na.rm = TRUE), "\n")
  merged_data <- merged_data %>%
         filter(is_mycorrhizal_only == FALSE) 
  cat("Rows after filtering out mycorrhizal only entries:", nrow(merged_data), "\n")
  cat("Unique abstracts before final save:", length(unique(merged_data$id)), "\n")
  
  cat("Unique abstracts after filtering:", length(unique(merged_data$id)), "\n")
  write_csv(merged_data, output_file)


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
