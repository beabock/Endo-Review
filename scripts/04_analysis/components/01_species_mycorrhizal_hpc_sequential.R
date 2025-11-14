# =============================================================================
# 01_species_mycorrhizal_hpc_sequential.R - HPC Sequential Version (Robust)
# =============================================================================
#
# Purpose: Robust sequential version for HPC when parallel processing hangs
#
# Description: Alternative implementation of species detection using sequential processing
# instead of parallel execution. Provides reliable fallback when cluster setup fails.
#
# Author: B. Bock
# Date: 2024-09-26
#
# Usage: Use this if the parallel version hangs during cluster setup
#
# Dependencies: optimized_taxa_detection.R, reference_data_utils.R, memory_optimization.R
#
# Inputs: consolidated_dataset.csv, lookup_tables.rds, species.rds, funtothefun.csv
# Outputs: species_mycorrhizal_results_sequential.csv, processing logs
#
# =============================================================================


# Force sequential mode from the start
cat("🔄 HPC SEQUENTIAL MODE - Robust processing without parallel overhead\n\n")

library(tidyverse)
library(tictoc)
library(janitor)

# Source required functions only (not the main script)
source("scripts/04_analysis/components/optimized_taxa_detection.R")
source("scripts/04_analysis/utilities/reference_data_utils.R")

# Source memory optimization utilities if available
tryCatch({
  source("scripts/utils/memory_optimization.R")
}, error = function(e) {
  cat("⚠️ Memory optimization utilities not found, running with basic memory management\n")
})

# =============================================================================
# SEQUENTIAL LOGGING FUNCTIONS
# =============================================================================

log_message <- function(..., log_file = NULL, verbose = TRUE) {
  message <- paste(..., collapse = " ")
  if (verbose) cat(message, "\n")
  if (!is.null(log_file) && file.exists(log_file)) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    log_entry <- paste0("[", timestamp, "] ", message)
    write(log_entry, file = log_file, append = TRUE)
  }
}

setup_logging <- function(output_file) {
  log_file <- paste0(tools::file_path_sans_ext(output_file), "_processing.log")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  log_entry <- paste0("\n", strrep("=", 80), "\n")
  log_entry <- paste0(log_entry, "SEQUENTIAL HPC SPECIES DETECTION - PROCESSING LOG\n")
  log_entry <- paste0(log_entry, "Started at: ", timestamp, "\n")
  log_entry <- paste0(log_entry, strrep("=", 80), "\n")
  
  cat(log_entry)
  write(log_entry, file = log_file, append = FALSE)
  return(log_file)
}

# =============================================================================
# LOAD LOOKUP TABLES FUNCTION
# =============================================================================

load_precomputed_lookup_tables <- function(verbose = TRUE) {
  if (verbose) cat("🔍 Loading pre-computed lookup tables...\n")
  
  required_files <- c("models/lookup_tables.rds")
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
    stop("Please ensure lookup_tables.rds exists!")
  }
  
  if (verbose) cat("   📂 Loading lookup_tables.rds...\n")
  lookup_tables <- readRDS("models/lookup_tables.rds")
  
  # Load optional hash tables
  hash_files <- c("models/species_hash.rds", "models/genus_hash.rds", "models/family_hash.rds")
  for (hash_file in hash_files) {
    if (file.exists(hash_file)) {
      if (verbose) cat("   📂 Loading", hash_file, "...\n")
      hash_data <- readRDS(hash_file)
      hash_name <- gsub(".rds", "", hash_file)
      lookup_tables[[hash_name]] <- hash_data
    }
  }
  
  if (verbose) {
    cat("✅ Pre-computed lookup tables loaded successfully!\n")
    if (!is.null(lookup_tables$accepted_species)) {
      cat("   📊 Species records:", nrow(lookup_tables$accepted_species), "\n")
    }
  }
  
  return(lookup_tables)
}

# Helper function to create empty result
create_empty_result_df <- function() {
  tibble(
    id = character(0),
    predicted_label = character(0),
    match_type = character(0),
    canonicalName = character(0),
    kingdom = character(0),
    phylum = character(0),
    family = character(0),
    genus = character(0),
    status = character(0),
    resolved_name = character(0),
    acceptedScientificName = character(0)
  )
}

# =============================================================================
# SEQUENTIAL EXTRACTION FUNCTION - NO PARALLEL PROCESSING
# =============================================================================

extract_species_data <- function(
  abstracts_data,
  output_file = "species_mycorrhizal_results_sequential.csv",
  batch_size = 100,
  force_rerun = FALSE,
  verbose = TRUE,
  hash_threshold = 10000
) {

  # Validate input data structure
  required_cols <- c("id", "abstract")
  missing_cols <- setdiff(required_cols, colnames(abstracts_data))

  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns in abstracts_data: ", paste(missing_cols, collapse = ", "))
  }

  # Check for empty abstract column
  non_na_abstracts <- sum(!is.na(abstracts_data$abstract) & abstracts_data$abstract != "", na.rm = TRUE)
  if (non_na_abstracts == 0) {
    if (verbose) log_message("⚠️ All abstracts are empty or NA, returning empty results", log_file = NULL)
    empty_result <- create_empty_result_df()
    write_csv(empty_result, output_file)
    return(empty_result)
  }
  
  cat("🚀 SEQUENTIAL MODE - No parallel processing overhead\n")
  cat("   Processing mode: Sequential only\n")
  cat("   Batch size:", batch_size, "\n")
  cat("   Checkpoint interval: 5 batches\n\n")
  
  # Load lookup tables
  lookup_tables <- load_precomputed_lookup_tables(verbose)
  if (is.null(lookup_tables)) {
    stop("❌ Could not load pre-computed lookup tables")
  }
  
  # Setup logging
  log_file <- setup_logging(output_file)
  
  # Load species reference data
  species_file <- "models/species.rds"
  if (!file.exists(species_file)) {
    stop("❌ Species reference data not found: species.rds")
  }
  
  species <- readRDS(species_file)
  if (verbose) log_message("   Loaded species reference data:", nrow(species), "species records", log_file = log_file)
  
  # Handle empty data early
  if (nrow(abstracts_data) == 0) {
    if (verbose) log_message("⚠️ Empty abstracts data provided, returning empty results", log_file = log_file)
    empty_result <- create_empty_result_df()
    write_csv(empty_result, output_file)
    return(empty_result)
  }

  # Process in batches
  tic("Sequential species detection")
  n_batches <- ceiling(nrow(abstracts_data) / batch_size)
  all_results <- list()

  if (verbose) {
    log_message("📊 Processing", nrow(abstracts_data), "abstracts in", n_batches, "batches", log_file = log_file)
    log_message("⚙️ Batch size:", batch_size, "abstracts per batch", log_file = log_file)
    log_message("🕐 Started at", format(Sys.time(), "%H:%M:%S"), log_file = log_file)
  }

  overall_start_time <- Sys.time()
  total_species_found <- 0  # Initialize cumulative counter outside loop
  
  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(abstracts_data))
    batch_data <- abstracts_data[start_idx:end_idx, ]
    
    if (verbose) {
      log_message("   🚀 Batch", i, "of", n_batches, "(", nrow(batch_data), "abstracts)", log_file = log_file)
    }
    
    # Process batch sequentially
    batch_results <- tryCatch({
      sequential_results <- list()
      batch_errors <- 0
      batch_species_count <- 0  # Count for this batch only
      batch_start_time <- Sys.time()
      
      for (j in 1:nrow(batch_data)) {
        if (verbose && j %% 10 == 1) {
          cat("      📄 Processing abstract", j, "of", nrow(batch_data), "\n")
        }
        
        abstract_text <- batch_data$abstract[j]
        abstract_id <- batch_data$id[j]
        predicted_label <- if ("predicted_label" %in% colnames(batch_data)) {
          batch_data$predicted_label[j]
        } else {
          "Presence"
        }
        
        tryCatch({
          # Extract candidate names
          candidate_result <- extract_candidate_names(abstract_text)
          candidates <- candidate_result$candidates
          
          # Validate candidates
          valid_species <- batch_validate_names(candidates, lookup_tables)
          
          # Process matches
          all_rows <- process_taxonomic_matches(
            valid_species, lookup_tables, abstract_text,
            abstract_id, predicted_label
          )
          
          # Store results and count actual species records
          if (length(all_rows) > 0) {
            result <- bind_rows(all_rows)
            sequential_results[[j]] <- result
            
            # Count actual species records (not just candidates)
            if ("match_type" %in% colnames(result)) {
              species_in_result <- sum(result$match_type == "species", na.rm = TRUE)
              batch_species_count <- batch_species_count + species_in_result
            } else {
              # Fallback: count rows that have resolved names
              species_in_result <- sum(!is.na(result$resolved_name), na.rm = TRUE)
              batch_species_count <- batch_species_count + species_in_result
            }
          } else {
            sequential_results[[j]] <- tibble(id = abstract_id, match_type = "none")
          }
        }, error = function(e) {
          batch_errors <- batch_errors + 1
          sequential_results[[j]] <- tibble(id = abstract_id, error = e$message)
        })
      }
      
      batch_result <- bind_rows(sequential_results)
      
      # Update cumulative count
      total_species_found <<- total_species_found + batch_species_count
      
      if (verbose) {
        batch_time <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
        
        # Additional diagnostics
        total_output_rows <- nrow(batch_result)
        species_rows <- if ("match_type" %in% colnames(batch_result)) {
          sum(batch_result$match_type == "species", na.rm = TRUE)
        } else {
          sum(!is.na(batch_result$resolved_name), na.rm = TRUE)
        }
        
        cat("      📊 Batch", i, "summary:\n")
        cat("         Abstracts processed:", nrow(batch_data), "\n")
        cat("         Total output rows:", total_output_rows, "\n") 
        cat("         Species this batch:", species_rows, "\n")
        cat("         Cumulative species:", total_species_found, "\n")
        cat("         Errors:", batch_errors, "\n")
        cat("         Time:", round(batch_time, 1), "seconds\n")
        
        # Quick sanity check - only warn if multiple batches have identical counts
        if (i > 1 && exists("previous_species_count") && species_rows == previous_species_count) {
          cat("         ⚠️  WARNING: Identical species count as previous batch (", species_rows, ")\n")
        }
        
        # Store for next batch comparison
        previous_species_count <<- species_rows
      }
      
      batch_result
      
    }, error = function(e) {
      if (verbose) cat("     ⚠️ Batch", i, "failed:", e$message, "\n")
      tibble(id = character(0))
    })
    
    all_results[[i]] <- batch_results
    
    # Checkpoint every 5 batches
    if (i %% 5 == 0 || i == n_batches) {
      checkpoint_data <- bind_rows(all_results)
      checkpoint_file <- paste0(tools::file_path_sans_ext(output_file), "_checkpoint_batch_", i, ".csv")
      write_csv(checkpoint_data, checkpoint_file)
      
      if (verbose) {
        cat("   💾 Checkpoint saved:", checkpoint_file, "(", nrow(checkpoint_data), "rows)\n")
      }
    }
    
    # Progress report
    if (i %% 10 == 0 || i == n_batches) {
      progress_pct <- round(100 * i / n_batches, 1)
      elapsed <- as.numeric(difftime(Sys.time(), overall_start_time, units = "mins"))
      cat("   📊 Progress:", progress_pct, "% (", i, "/", n_batches, " batches) - ", round(elapsed, 1), " minutes elapsed\n")
    }
    
    # Memory cleanup
    if (i %% 10 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  # Combine final results
  if (verbose) log_message("   Combining all batch results...", log_file = log_file)
  final_results <- bind_rows(all_results)
  
  # Save final results
  write_csv(final_results, output_file)
  
  total_time <- toc(quiet = TRUE)
  
  if (verbose) {
    cat("🚀 Sequential processing completed!\n")
    cat("📈 Results: ", nrow(final_results), " species detection records\n")
    cat("💾 Final results saved to:", output_file, "\n")
  }
  
  return(final_results)
}

# =============================================================================
# MAIN EXECUTION - SEQUENTIAL ONLY
# =============================================================================

cat("🚀 === HPC SEQUENTIAL SPECIES DETECTION ===\n")
cat("Pure sequential mode - no parallel processing\n\n")

# Output file
output_file <- "species_mycorrhizal_results_sequential.csv"

# Load consolidated dataset
abstracts_file <- "results/consolidated_dataset.csv"
cat("📂 Loading consolidated dataset...\n")

if (!file.exists(abstracts_file)) {
  stop("❌ consolidated_dataset.csv not found in root directory")
}

load_start <- Sys.time()
abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)

load_time <- as.numeric(difftime(Sys.time(), load_start, units = "secs"))
cat("✅ Loaded", nrow(abstracts_data), "abstracts in", round(load_time, 1), "seconds\n\n")

# Check for FUNGuild dataset
if (!file.exists("data/raw/funtothefun.csv")) {
  stop("❌ funtothefun.csv not found in root directory")
}
cat("✅ FUNGuild dataset found\n\n")

# Process with sequential mode only
cat("🚀 Starting sequential processing...\n")
processing_start <- Sys.time()

results <- extract_species_data(
  abstracts_data = abstracts_data,
  output_file = output_file,
  batch_size = 100,
  verbose = TRUE
)

processing_time <- as.numeric(difftime(Sys.time(), processing_start, units = "mins"))
total_time <- as.numeric(difftime(Sys.time(), load_start, units = "mins"))

cat("\n", strrep("=", 60), "\n")
cat("🎉 HPC SEQUENTIAL PROCESSING COMPLETED!\n")
cat("📊 Total runtime:", round(total_time, 1), "minutes\n")
cat("� Processing time:", round(processing_time, 1), "minutes\n")
cat("📈 Results:", nrow(results), "species detection records\n")
cat("💾 Output saved to:", output_file, "\n")
cat("✅ Ready for analysis!\n")
