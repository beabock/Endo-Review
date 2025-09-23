# =============================================================================
# 01_extract_species.R - Species detection component for extraction pipeline
# =============================================================================
#
# Purpose: Extract plant and fungal species information from abstracts
#
# Description: Script that handles species detection for plants and fungi as part of the modular extraction pipeline,
# using optimized lookup tables and parallel processing for efficient taxonomic name recognition.
#
# Dependencies: tidyverse, tictoc, janitor; scripts/04_analysis/optimized_taxa_detection.R, scripts/04_analysis/utilities/reference_data_utils.R
#
# Author: B. Bock
# Date: 2024-09-22
#
# Inputs/Outputs: Reads prepared abstracts from results/prepared_abstracts_for_extraction.csv; outputs species detection results to results/species_detection_results.csv
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(janitor)

# Source required functions for optimized taxa detection and reference data utilities
source("scripts/04_analysis/optimized_taxa_detection.R")
source("scripts/04_analysis/utilities/reference_data_utils.R")

cat("=== SPECIES DETECTION COMPONENT ===\n")
cat("Extracting plant and fungal species information\n\n")

# Function to create optimized lookup tables
create_lookup_tables_optimized <- function(species_df, threshold = 10000) {
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

  # Create hash tables for O(1) lookups if dataset is large enough to benefit from the optimization
  # Rationale: For datasets above the threshold, hash tables provide faster lookups than linear searches, improving performance for species matching
  if (nrow(species_df) > threshold) {
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

#' Extract species information from abstracts
#'
#' This function processes a dataset of abstracts to detect and extract plant and fungal species mentions.
#' It uses optimized lookup tables and parallel processing for efficient taxonomic name recognition.
#' Supports recovery from existing results and batch processing to handle large datasets.
#'
#' @param abstracts_data Data frame containing abstracts with text to process
#' @param output_file Path to save the species detection results (default: "results/species_detection_results.csv")
#' @param batch_size Number of abstracts to process per batch (default: 100)
#' @param force_rerun Logical, if TRUE forces re-extraction even if results exist (default: FALSE)
#' @param verbose Logical, if TRUE prints progress messages (default: TRUE)
#' @param hash_threshold Minimum species count to enable hash table optimization (default: 10000)
#' @return Data frame with species detection results
extract_species_data <- function(
  abstracts_data,
  output_file = "results/species_detection_results.csv",
  batch_size = 100,
  force_rerun = FALSE,
  verbose = TRUE,
  hash_threshold = 10000
) {

  # Input validation
  if (!is.data.frame(abstracts_data)) {
    stop("'abstracts_data' must be a data frame.")
  }
  if (!"abstract" %in% colnames(abstracts_data)) {
    stop("'abstracts_data' must contain a 'abstract' column for abstracts.")
  }

  # Handle empty datasets
  if (nrow(abstracts_data) == 0) {
    warning("Input dataset is empty. Skipping species extraction.")
    return(data.frame())  # Return empty data frame
  }

  # Recovery mechanism - check for existing results
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing species detection results\n")
    existing_results <- tryCatch({
      read_csv(output_file, show_col_types = FALSE)
    }, error = function(e) {
      warning("Failed to load existing species detection results: ", e$message, ". Proceeding with fresh extraction.")
      NULL
    })
    if (!is.null(existing_results)) {
      if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
      return(existing_results)
    }
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
  # Scale cores based on dataset size: more cores for larger datasets, up to available cores - 1
  n_cores <- min(max(2, floor(nrow(abstracts_data) / 2000) + 1), parallel::detectCores() - 1)
  tryCatch({
    setup_parallel(workers = n_cores)
  }, error = function(e) {
    warning("Parallel setup failed: ", e$message, ". Falling back to sequential processing.")
    n_cores <- 1
  })
  lookup_tables <- create_lookup_tables_optimized(species, hash_threshold)
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
      species_path = if (file.exists("species.rds")) "species.rds" else "models/species.rds",
      plant_parts_keywords = plant_parts_keywords,
      batch_size = 50
    )

    # Progress reporting
    species_found <- sum(!is.na(batch_results$resolved_name) | !is.na(batch_results$canonicalName), na.rm = TRUE)
    if (verbose) {
      cat("      ✅ Found species in", species_found, "abstracts\n")
    }

    # Save intermediate results every 5 batches to allow recovery from interruptions and monitor progress
    if (i %% 5 == 0) {
      temp_file <- paste0("results/temp_species_batch_", i, ".csv")
      tryCatch({
        write_csv(batch_results, temp_file)
        if (verbose) cat("      💾 Saved intermediate results to", temp_file, "\n")
      }, error = function(e) {
        warning("Failed to save intermediate results to ", temp_file, ": ", e$message)
      })
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
  tryCatch({
    write_csv(all_species_results, output_file)
  }, error = function(e) {
    warning("Failed to save final results to ", output_file, ": ", e$message)
  })

  # Summary statistics
  # Count abstracts with at least one species detected
  total_species_found <- sum(!is.na(all_species_results$resolved_name) | !is.na(all_species_results$canonicalName), na.rm = TRUE)
  # Calculate unique species, handling NA values by omitting them to ensure accurate counts regardless of NA presence
  unique_species <- length(unique(c(unique(na.omit(all_species_results$resolved_name)), unique(na.omit(all_species_results$canonicalName)))))

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
