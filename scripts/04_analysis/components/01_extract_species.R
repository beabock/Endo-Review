# Species Detection Component
# B. Bock - Modular Version
# July 31, 2025 - Species extraction (most time-consuming part ~1-2 days)
#
# This script handles species detection for plants and fungi
# Part of the modular extraction pipeline

library(tidyverse)
library(tictoc)
library(janitor)

# Source required functions
source("scripts/04_analysis/optimized_taxa_detection.R")
source("scripts/04_analysis/utilities/reference_data_utils.R")

cat("=== SPECIES DETECTION COMPONENT ===\n")
cat("Extracting plant and fungal species information\n\n")

# Function to create optimized lookup tables
create_lookup_tables_optimized <- function(species_df) {
  # Call the original function
  lookup_tables <- create_lookup_tables(species_df)

  # Add performance optimizations
  if (!is.null(lookup_tables$accepted_species)) {
    lookup_tables$species_names_vector <- lookup_tables$accepted_species$canonicalName_lower
  }
  if (!is.null(lookup_tables$genus_list)) {
    lookup_tables$genus_names_vector <- lookup_tables$genus_list$canonicalName_lower
  }
  if (!is.null(lookup_tables$family_list)) {
    lookup_tables$family_names_vector <- lookup_tables$family_list$canonicalName_lower
  }

  # Create hash tables for O(1) lookups if dataset is large
  if (nrow(species_df) > 10000) {
    if (!is.null(lookup_tables$species_names_vector)) {
      lookup_tables$species_hash <- setNames(rep(TRUE, length(lookup_tables$species_names_vector)),
                                            lookup_tables$species_names_vector)
    }
    if (!is.null(lookup_tables$genus_names_vector)) {
      lookup_tables$genus_hash <- setNames(rep(TRUE, length(lookup_tables$genus_names_vector)),
                                          lookup_tables$genus_names_vector)
    }
  }

  return(lookup_tables)
}

# Main species extraction function
extract_species_data <- function(
  abstracts_data,
  output_file = "results/species_detection_results.csv",
  batch_size = 100,
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Recovery mechanism - check for existing results
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing species detection results\n")
    existing_results <- read_csv(output_file, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
    return(existing_results)
  }

  if (verbose) cat("🔬 Starting species detection for", nrow(abstracts_data), "abstracts\n")

  # Load species reference data
  if (file.exists("species.rds")) {
    species <- readRDS("species.rds")
  } else if (file.exists("models/species.rds")) {
    species <- readRDS("models/species.rds")
  } else {
    stop("❌ Species reference data not found. Please ensure species.rds exists.")
  }

  if (verbose) cat("   Loaded species reference data:", nrow(species), "species records\n")

  # Set up parallel processing
  setup_parallel(workers = min(6, parallel::detectCores() - 1))
  lookup_tables <- create_lookup_tables_optimized(species)
  plant_parts_keywords <- get_plant_parts_keywords()

  # Process in batches
  tic("Species detection")
  n_batches <- ceiling(nrow(abstracts_data) / batch_size)

  if (verbose) {
    cat("📊 Processing", nrow(abstracts_data), "abstracts in", n_batches, "batches\n")
    cat("⚙️  Batch size:", batch_size, "abstracts per batch\n")
    cat("🕐 Started at", format(Sys.time(), "%H:%M:%S"), "\n\n")
  }

  all_species_results <- map_dfr(1:n_batches, function(i) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(abstracts_data))

    batch_data <- abstracts_data[start_idx:end_idx, ]

    if (verbose) {
      cat("   🔬 Batch", i, "of", n_batches, "(", nrow(batch_data), "abstracts)\n")
      cat("      Processing rows", start_idx, "to", end_idx, "\n")
    }

    # Process batch
    batch_results <- process_abstracts_parallel(
      abstracts = batch_data,
      lookup_tables = lookup_tables,
      plant_parts_keywords = plant_parts_keywords,
      batch_size = 50
    )

    # Progress reporting
    species_found <- sum(!is.na(batch_results$resolved_name) | !is.na(batch_results$canonicalName), na.rm = TRUE)
    if (verbose) {
      cat("      ✅ Found species in", species_found, "abstracts\n")
    }

    # Save intermediate results every 5 batches
    if (i %% 5 == 0) {
      temp_file <- paste0("results/temp_species_batch_", i, ".csv")
      write_csv(batch_results, temp_file)
      if (verbose) cat("      💾 Saved intermediate results to", temp_file, "\n")
    }

    # Overall progress
    total_processed <- i * batch_size
    if (total_processed > nrow(abstracts_data)) total_processed <- nrow(abstracts_data)
    progress_pct <- round(100 * total_processed / nrow(abstracts_data), 1)
    if (verbose) {
      cat("      📊 Progress:", total_processed, "/", nrow(abstracts_data),
          "abstracts (", progress_pct, "%)\n\n")
    }

    return(batch_results)
  })

  # Save final results
  write_csv(all_species_results, output_file)

  # Summary
  total_species_found <- sum(!is.na(all_species_results$resolved_name) | !is.na(all_species_results$canonicalName), na.rm = TRUE)
  unique_species <- length(unique(c(all_species_results$resolved_name, all_species_results$canonicalName)))
  unique_species <- unique_species - 1  # Remove NA count

  toc()

  if (verbose) {
    cat("🎉 Species detection completed!\n")
    cat("📈 Results:\n")
    cat("   - Total abstracts processed:", nrow(abstracts_data), "\n")
    cat("   - Abstracts with species found:", total_species_found,
        "(", round(100 * total_species_found / nrow(abstracts_data), 1), "%)\n")
    cat("   - Unique species detected:", unique_species, "\n")
    cat("💾 Results saved to:", output_file, "\n")
  }

  return(all_species_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "01_extract_species.R")) {

  # Load abstracts data (should be prepared by pipeline)
  abstracts_file <- "results/prepared_abstracts_for_extraction.csv"
  if (!file.exists(abstracts_file)) {
    stop("❌ Prepared abstracts not found. Run the pipeline script first or prepare data manually.")
  }

  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)

  # Extract species
  species_results <- extract_species_data(abstracts_data)

  cat("\n✅ Species extraction component completed!\n")
}
