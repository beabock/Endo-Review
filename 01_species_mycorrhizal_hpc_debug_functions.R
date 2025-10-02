
# =============================================================================
# 01_species_mycorrhizal_hpc_debug_functions.R - Function-Level Debug Version
# =============================================================================
#
# Purpose: Enhanced debugging version with per-abstract logging to diagnose empty results
#
# Description: This version adds detailed logging to show exactly what happens during
# species detection processing for each abstract. It helps identify why no species
# are being found.
#
# Usage: Run this to see detailed processing for the first batch (25 abstracts)
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(janitor)
library(parallel)

# Source required functions for optimized taxa detection and reference data utilities
source("optimized_taxa_detection.R")
source("reference_data_utils.R")

# Source memory optimization utilities if available
tryCatch({
  source("memory_optimization.R")
}, error = function(e) {
  cat("⚠️ Memory optimization utilities not found, running with basic memory management\n")
})

# =============================================================================
# ENHANCED LOGGING WITH MULTI-PART MESSAGES
# =============================================================================

log_message <- function(..., log_file = NULL, verbose = TRUE) {
  # Concatenate all message components
  message <- paste(..., collapse = " ")

  if (verbose) {
    cat(message, "\n")
  }

  if (!is.null(log_file) && file.exists(log_file)) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    log_entry <- paste0("[", timestamp, "] ", message)
    write(log_entry, file = log_file, append = TRUE)
  }
}

# =============================================================================
# LOAD PRE-COMPUTED LOOKUP TABLES (SAME AS OPTIMIZED VERSION)
# =============================================================================

load_precomputed_lookup_tables <- function(verbose = TRUE) {
  if (verbose) cat("🔍 Loading pre-computed lookup tables...\n")

  # Check if pre-computed files exist
  required_files <- c("lookup_tables.rds")
  missing_files <- c()

  for (file in required_files) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }

  if (length(missing_files) > 0) {
    cat("❌ Missing required lookup table files:\n")
    for (file in missing_files) {
      cat("   - ", file, "\n")
    }
    cat("\nPlease run 02_precompute_lookup_tables.R first!\n")
    return(NULL)
  }

  # Load complete lookup tables
  if (verbose) cat("   📂 Loading lookup_tables.rds...\n")
  lookup_tables <- readRDS("lookup_tables.rds")

  # Load individual hash tables if they exist
  hash_files <- c("species_hash.rds", "genus_hash.rds", "family_hash.rds")
  for (hash_file in hash_files) {
    if (file.exists(hash_file)) {
      if (verbose) cat("   📂 Loading", hash_file, "...\n")
      hash_data <- readRDS(hash_file)
      # Add to lookup tables
      hash_name <- gsub(".rds", "", hash_file)
      lookup_tables[[hash_name]] <- hash_data
    }
  }

  # Load vector files if they exist
  vector_files <- c("species_names_vector.rds", "genus_names_vector.rds", "family_names_vector.rds")
  for (vector_file in vector_files) {
    if (file.exists(vector_file)) {
      if (verbose) cat("   📂 Loading", vector_file, "...\n")
      vector_data <- readRDS(vector_file)
      vector_name <- gsub(".rds", "", vector_file)
      lookup_tables[[vector_name]] <- vector_data
    }
  }

  if (verbose) {
    cat("✅ Pre-computed lookup tables loaded successfully!\n")
    cat("   📊 Components loaded:\n")

    if (!is.null(lookup_tables$accepted_species)) {
      cat("      • Species:", nrow(lookup_tables$accepted_species), "records\n")
    }
    if (!is.null(lookup_tables$species_hash)) {
      cat("      • Species hash: Available\n")
    }
    if (!is.null(lookup_tables$genus_hash)) {
      cat("      • Genus hash: Available\n")
    }
    if (!is.null(lookup_tables$family_hash)) {
      cat("      • Family hash: Available\n")
    }
  }

  return(lookup_tables)
}

# =============================================================================
# ENHANCED SEQUENTIAL PROCESSING WITH DETAILED DEBUGGING
# =============================================================================

debug_sequential_processing <- function(batch_data, lookup_tables, batch_number, verbose = TRUE, log_file = NULL) {
  if (verbose) {
    cat("\n📊 Batch", batch_number, "Debug Processing (", nrow(batch_data), "abstracts):\n")
    log_message(paste0("Batch ", batch_number, " debug processing started (", nrow(batch_data), " abstracts)"), log_file = log_file, verbose = verbose)
  }

  sequential_results <- list()
  batch_errors <- 0
  species_found_count <- 0
  candidates_total <- 0
  valid_species_total <- 0

  batch_start_time <- Sys.time()

  for (j in 1:nrow(batch_data)) {
    abstract_start_time <- Sys.time()

    abstract_text <- batch_data$abstract[j]
    abstract_id <- batch_data$id[j]
    predicted_label <- if ("predicted_label" %in% colnames(batch_data)) {
      batch_data$predicted_label[j]
    } else {
      "Presence"
    }

    if (verbose) {
      cat("   📄 Abstract", j, "of", nrow(batch_data), "- ID:", abstract_id, "\n")
      if (nchar(abstract_text) == 0) {
        cat("      ⚠️ Empty abstract text!\n")
      }
    }

    tryCatch({
      # Step 1: Extract candidate names
      if (verbose) cat("      Step 1: Extracting candidate names...")
      candidate_start <- Sys.time()
      candidate_result <- extract_candidate_names(abstract_text)
      candidates <- candidate_result$candidates
      candidate_time <- as.numeric(difftime(Sys.time(), candidate_start, units = "secs"))

      candidates_total <- candidates_total + length(candidates)

      if (verbose) {
        cat(" Found", length(candidates), "candidates (", round(candidate_time, 2), "s)\n")
        if (length(candidates) > 0) {
          cat("         Sample candidates:", paste(head(candidates, 3), collapse = ", "), "\n")
        } else {
          cat("         ⚠️ No candidate names found in abstract!\n")
        }
      }

      # Step 2: Validate candidates
      if (length(candidates) > 0) {
        if (verbose) cat("      Step 2: Validating names against species database...")
        validation_start <- Sys.time()
        valid_species <- batch_validate_names(candidates, lookup_tables)
        validation_time <- as.numeric(difftime(Sys.time(), validation_start, units = "secs"))

        valid_species_total <- valid_species_total + length(valid_species)

        if (verbose) {
          cat(" Found", length(valid_species), "valid species (", round(validation_time, 2), "s)\n")
          if (length(valid_species) > 0) {
            cat("         Sample valid species:", paste(head(valid_species, 3), collapse = ", "), "\n")
          } else {
            cat("         ⚠️ No valid species found! Possible lookup table issue.\n")
          }
        }
      } else {
        valid_species <- character(0)
      }

      # Step 3: Process taxonomic matches
      if (length(valid_species) > 0) {
        if (verbose) cat("      Step 3: Processing taxonomic matches...")
        match_start <- Sys.time()
        all_rows <- process_taxonomic_matches(
          valid_species, lookup_tables, abstract_text,
          abstract_id, predicted_label
        )
        match_time <- as.numeric(difftime(Sys.time(), match_start, units = "secs"))

        if (verbose) {
          cat(" Generated", length(all_rows), "match rows (", round(match_time, 2), "s)\n")
          if (length(all_rows) > 0) {
            cat("         Sample match:", head(all_rows, 1)$resolved_name, "\n")
          }
        }

        # Count species found
        species_found_count <- species_found_count + length(unique(all_rows$resolved_name[!is.na(all_rows$resolved_name)]))

        # Return results
        if (length(all_rows) > 0) {
          result <- bind_rows(all_rows)
          sequential_results[[j]] <- result
        } else {
          sequential_results[[j]] <- tibble(id = abstract_id, match_type = "none")
        }
      } else {
        sequential_results[[j]] <- tibble(id = abstract_id, match_type = "no_valid_species")
      }

      abstract_time <- as.numeric(difftime(Sys.time(), abstract_start_time, units = "secs"))
      if (verbose && j %% 5 == 0) {
        cat("      📈 Abstract", j, "completed in", round(abstract_time, 1), "seconds\n")
      }

    }, error = function(e) {
      cat("      ❌ Error in abstract", j, ":", e$message, "\n")
      batch_errors <- batch_errors + 1
      sequential_results[[j]] <- tibble(id = abstract_id, error = e$message)
    })
  }

  batch_results <- bind_rows(sequential_results)

  batch_time <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))

  if (verbose) {
    cat("\n      📊 Batch", batch_number, "Summary:\n")
    cat("         Abstracts processed:", nrow(batch_data), "\n")
    cat("         Total candidates found:", candidates_total, "\n")
    cat("         Valid species found:", valid_species_total, "\n")
    cat("         Unique species matches:", species_found_count, "\n")
    cat("         Processing errors:", batch_errors, "\n")
    cat("         Batch time:", round(batch_time, 1), "seconds (", round(batch_time / nrow(batch_data), 1), "s/abstract)\n")
  }

  log_message(paste0("Batch ", batch_number, ": ", nrow(batch_data), " abstracts, ", candidates_total, " candidates, ", valid_species_total, " valid, ", species_found_count, " species, ", batch_errors, " errors (", round(batch_time, 1), "s)"), log_file = log_file, verbose = verbose)

  return(batch_results)
}
>>>>>>> REPLACE
</diff>
</apply_diff>