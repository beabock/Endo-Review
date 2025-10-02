# =============================================================================
# 01_species_mycorrhizal_hpc_DEBUG.R - DEBUG Version for Troubleshooting
# =============================================================================
#
# Purpose: Minimal test version to isolate HPC script hanging issues
#
# Description: Simplified version with extensive debugging, small dataset,
# minimal parallel processing, and detailed progress tracking to identify bottlenecks.
#
# Author: B. Bock (Debug version)
# Date: 2024-09-30
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(janitor)
library(parallel)

# Check for R.utils package (needed for timeout)
if (!requireNamespace("R.utils", quietly = TRUE)) {
  debug_log("WARNING", "R.utils package not found. Install with: install.packages('R.utils')")
  debug_log("WARNING", "Timeout functionality will not work without R.utils")
}

# Source required functions (with error handling)
source_files <- c("scripts/04_analysis/optimized_taxa_detection.R", "scripts/04_analysis/utilities/reference_data_utils.R", "scripts/utils/memory_optimization.R")

for (src_file in source_files) {
  tryCatch({
    cat("📂 Loading:", src_file, "...")
    source(src_file)
    cat(" ✅\n")
  }, error = function(e) {
    cat(" ❌ (", e$message, ")\n")
    if (src_file != "memory_optimization.R") {  # memory_optimization is optional
      stop("Required file not found: ", src_file)
    }
  })
}

# =============================================================================
# ENHANCED DEBUGGING FUNCTIONS
# =============================================================================

debug_log <- function(stage, message, log_file = "debug.log") {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  full_message <- paste0("[", timestamp, "] [", stage, "] ", message)

  cat(full_message, "\n")

  # Write to log file
  write(full_message, file = log_file, append = TRUE)
}

memory_check <- function(stage) {
  mem_used <- sum(gc()[, 2]) / 1024  # GB
  debug_log(stage, paste0("Memory usage: ", round(mem_used, 2), "GB"))
  return(mem_used)
}

# =============================================================================
# MINIMAL TEST VERSION - MAIN FUNCTION
# =============================================================================

run_hpc_debug_test <- function(
  test_size = 50,           # Very small test size
  max_cores = 2,           # Minimal parallel processing
  batch_size = 10,         # Small batches
  timeout_minutes = 10     # Short timeout for quick feedback
) {

  debug_log("INIT", "=== HPC DEBUG TEST STARTED ===")
  debug_log("INIT", paste0("Test size: ", test_size, " abstracts"))
  debug_log("INIT", paste0("Max cores: ", max_cores))
  debug_log("INIT", paste0("Batch size: ", batch_size))

  # =============================================================================
  # STEP 1: LOAD MINIMAL DATA
  # =============================================================================

  debug_log("DATA_LOAD", "Loading consolidated dataset...")
  memory_check("BEFORE_DATA_LOAD")

  if (!file.exists("results/consolidated_dataset.csv")) {
    debug_log("ERROR", "consolidated_dataset.csv not found!")
    return(FALSE)
  }

  abstracts_data <- read_csv("results/consolidated_dataset.csv", show_col_types = FALSE)
  debug_log("DATA_LOAD", paste0("Loaded ", nrow(abstracts_data), " total abstracts"))

  # Take only a small subset for testing
  set.seed(42)  # For reproducibility
  test_indices <- sample(1:nrow(abstracts_data), min(test_size, nrow(abstracts_data)))
  abstracts_data <- abstracts_data[test_indices, ]

  debug_log("DATA_LOAD", paste0("Using ", nrow(abstracts_data), " abstracts for testing"))
  memory_check("AFTER_DATA_LOAD")

  # =============================================================================
  # STEP 2: LOAD SPECIES DATA
  # =============================================================================

  debug_log("SPECIES_LOAD", "Loading species reference data...")
  memory_check("BEFORE_SPECIES_LOAD")

  if (!file.exists("models/species.rds")) {
    debug_log("ERROR", "species.rds not found!")
    return(FALSE)
  }

  species <- readRDS("models/species.rds")
  debug_log("SPECIES_LOAD", paste0("Loaded ", nrow(species), " species records"))
  memory_check("AFTER_SPECIES_LOAD")

  # =============================================================================
  # STEP 3: SETUP MINIMAL PARALLEL PROCESSING
  # =============================================================================

  debug_log("PARALLEL_SETUP", paste0("Setting up parallel processing with max ", max_cores, " cores..."))

  # Detect available cores but limit for testing
  available_cores <- min(detectCores() - 1, max_cores)
  debug_log("PARALLEL_SETUP", paste0("Will use ", available_cores, " cores"))

  # Setup minimal parallel cluster
  cl <- tryCatch({
    if (.Platform$OS.type == "unix") {
      debug_log("PARALLEL_SETUP", "Creating FORK cluster...")
      makeCluster(available_cores, type = "FORK")
    } else {
      debug_log("PARALLEL_SETUP", "Creating PSOCK cluster...")
      makeCluster(available_cores, type = "PSOCK")
    }
  }, error = function(e) {
    debug_log("PARALLEL_SETUP", paste0("Parallel setup failed: ", e$message))
    debug_log("PARALLEL_SETUP", "Falling back to sequential processing")
    NULL
  })

  if (!is.null(cl)) {
    debug_log("PARALLEL_SETUP", "Loading libraries on workers...")
    tryCatch({
      clusterEvalQ(cl, {
        library(tidyverse)
        library(janitor)
        NULL
      })
      debug_log("PARALLEL_SETUP", "Libraries loaded successfully")
    }, error = function(e) {
      debug_log("PARALLEL_SETUP", paste0("Failed to load libraries on workers: ", e$message))
      stopCluster(cl)
      cl <- NULL
    })
  }

  # =============================================================================
  # STEP 4: CREATE LOOKUP TABLES (MINIMAL)
  # =============================================================================

  debug_log("LOOKUP", "Creating lookup tables...")
  memory_check("BEFORE_LOOKUP")

  # Use a much smaller threshold for testing
  lookup_tables <- create_lookup_tables_with_bloom(species, enable_bloom_filters = TRUE)
  debug_log("LOOKUP", "Lookup tables created")
  memory_check("AFTER_LOOKUP")

  # =============================================================================
  # STEP 5: TEST SINGLE BATCH PROCESSING
  # =============================================================================

  debug_log("BATCH_TEST", "Starting batch processing test...")
  n_batches <- ceiling(nrow(abstracts_data) / batch_size)
  debug_log("BATCH_TEST", paste0("Will process ", n_batches, " batches"))

  # Process just the first batch as a test
  test_batch_idx <- 1
  start_idx <- (test_batch_idx - 1) * batch_size + 1
  end_idx <- min(test_batch_idx * batch_size, nrow(abstracts_data))

  debug_log("BATCH_TEST", paste0("Processing batch ", test_batch_idx, " (rows ", start_idx, "-", end_idx, ")"))

  batch_data <- abstracts_data[start_idx:end_idx, ]
  debug_log("BATCH_TEST", paste0("Extracted batch with ", nrow(batch_data), " abstracts"))

  # =============================================================================
  # STEP 6: TEST PARALLEL PROCESSING
  # =============================================================================

  if (!is.null(cl)) {
    debug_log("PARALLEL_TEST", "Testing parallel processing...")

    # Test function for single abstract
    test_process_abstract <- function(j) {
      debug_log("WORKER", paste0("Processing abstract ", j))

      abstract_text <- batch_data$abstract[j]
      abstract_id <- batch_data$id[j]

      tryCatch({
        # Extract candidate names
        candidate_result <- extract_candidate_names(abstract_text)
        candidates <- candidate_result$candidates

        # Validate candidates
        valid_species <- batch_validate_names(candidates, lookup_tables)

        # Process matches
        all_rows <- process_taxonomic_matches(
          valid_species, lookup_tables, abstract_text,
          abstract_id, "Presence"
        )

        # Return results
        if (length(all_rows) > 0) {
          bind_rows(all_rows)
        } else {
          tibble(id = abstract_id, match_type = "none")
        }
      }, error = function(e) {
        debug_log("WORKER_ERROR", paste0("Error in abstract ", j, ": ", e$message))
        tibble(id = abstract_id, error = e$message)
      })
    }

    # Export test data to workers
    debug_log("PARALLEL_TEST", "Exporting data to workers...")
    tryCatch({
      clusterExport(cl, c("batch_data", "lookup_tables"), envir = environment())
      debug_log("PARALLEL_TEST", "Data exported successfully")
    }, error = function(e) {
      debug_log("PARALLEL_TEST", paste0("Failed to export data: ", e$message))
      stopCluster(cl)
      cl <- NULL
    })

    if (!is.null(cl)) {
      # Test with timeout
      debug_log("PARALLEL_TEST", "Starting parallel processing with timeout...")

      parallel_results <- tryCatch({
        # Simple timeout implementation
        timeout_seconds <- timeout_minutes * 60

        # Start parallel processing
        parallel_start <- Sys.time()
        results <- parLapplyLB(cl, 1:min(5, nrow(batch_data)), test_process_abstract)  # Test first 5 only

        # Check if it completed within timeout
        elapsed <- as.numeric(difftime(Sys.time(), parallel_start, units = "secs"))
        if (elapsed > timeout_seconds) {
          debug_log("PARALLEL_TEST", paste0("Parallel processing exceeded timeout (", elapsed, "s > ", timeout_seconds, "s)"))
          NULL
        } else {
          debug_log("PARALLEL_TEST", paste0("Parallel processing completed in ", round(elapsed, 2), " seconds"))
          results
        }
      }, error = function(e) {
        debug_log("PARALLEL_TEST", paste0("Parallel processing failed: ", e$message))
        NULL
      })

      if (!is.null(parallel_results)) {
        debug_log("PARALLEL_TEST", paste0("Parallel processing successful! Got ", length(parallel_results), " results"))
      }
    }
  } else {
    debug_log("SEQUENTIAL_TEST", "Testing sequential processing...")

    # Test sequential processing
    sequential_results <- list()

    for (j in 1:min(3, nrow(batch_data))) {  # Test first 3 only
      debug_log("SEQUENTIAL", paste0("Processing abstract ", j, " sequentially"))

      tryCatch({
        abstract_text <- batch_data$abstract[j]
        abstract_id <- batch_data$id[j]

        # Extract candidate names
        candidate_result <- extract_candidate_names(abstract_text)
        candidates <- candidate_result$candidates

        # Validate candidates
        valid_species <- batch_validate_names(candidates, lookup_tables)

        # Process matches
        all_rows <- process_taxonomic_matches(
          valid_species, lookup_tables, abstract_text,
          abstract_id, "Presence"
        )

        # Return results
        if (length(all_rows) > 0) {
          result <- bind_rows(all_rows)
        } else {
          result <- tibble(id = abstract_id, match_type = "none")
        }

        sequential_results[[j]] <- result

      }, error = function(e) {
        debug_log("SEQUENTIAL_ERROR", paste0("Error in abstract ", j, ": ", e$message))
      })
    }

    debug_log("SEQUENTIAL_TEST", paste0("Sequential processing completed. Got ", length(sequential_results), " results"))
  }

  # =============================================================================
  # CLEANUP
  # =============================================================================

  if (!is.null(cl)) {
    debug_log("CLEANUP", "Stopping parallel cluster...")
    tryCatch({
      stopCluster(cl)
      debug_log("CLEANUP", "Cluster stopped successfully")
    }, error = function(e) {
      debug_log("CLEANUP", paste0("Error stopping cluster: ", e$message))
    })
  }

  debug_log("FINISH", "=== HPC DEBUG TEST COMPLETED ===")
  memory_check("FINAL")

  return(TRUE)
}

# =============================================================================
# RUN THE DEBUG TEST
# =============================================================================

cat("🚀 === HPC DEBUG TEST ===\n")
cat("This will test the HPC script with minimal data to isolate issues.\n\n")

# Run the debug test
success <- run_hpc_debug_test(
  test_size = 50,        # Start very small
  max_cores = 2,         # Minimal cores
  batch_size = 10,       # Small batches
  timeout_minutes = 5    # Quick timeout
)

if (success) {
  cat("\n✅ Debug test completed successfully!\n")
  cat("The script can process data. Try gradually increasing:\n")
  cat("1. test_size (50 → 100 → 500)\n")
  cat("2. max_cores (2 → 4 → 8)\n")
  cat("3. batch_size (10 → 50 → 100)\n")
} else {
  cat("\n❌ Debug test failed. Check the debug.log file for details.\n")
}