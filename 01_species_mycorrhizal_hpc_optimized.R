# =============================================================================
# 01_species_mycorrhizal_hpc_optimized.R - HPC-Optimized with Pre-computed Lookup Tables
# =============================================================================
#
# Purpose: HPC-optimized version that loads pre-computed lookup tables instead of creating them
#
# Description: This version eliminates the lookup table creation bottleneck by loading
# pre-computed tables from disk. Also includes smaller batch sizes and enhanced debugging.
#
# Requirements: Run 02_precompute_lookup_tables.R first to create lookup table files
#
# Author: B. Bock (Optimized version)
# Date: 2024-09-30
#
# =============================================================================

# Ensure we're working from the correct root directory
if (!file.exists("01_species_mycorrhizal_hpc_optimized.R")) {
  stop("❌ Script must be run from the project root directory containing this file")
}

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

# Setup logging function (global)
setup_logging <- function(output_file) {
  log_file <- paste0(tools::file_path_sans_ext(output_file), "_processing.log")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  log_entry <- paste0("\n", strrep("=", 80), "\n")
  log_entry <- paste0(log_entry, "OPTIMIZED HPC SPECIES DETECTION - PROCESSING LOG\n")
  log_entry <- paste0(log_entry, "Started at: ", timestamp, "\n")
  log_entry <- paste0(log_entry, strrep("=", 80), "\n")

  cat(log_entry)
  write(log_entry, file = log_file, append = FALSE)
  return(log_file)
}

# =============================================================================
# OPTIMIZED HPC CONFIGURATION
# =============================================================================

detect_hpc_environment <- function() {
  # Detect HPC environment based on available cores and memory
  n_cores <- detectCores()
  mem_gb <- if (.Platform$OS.type == "unix") {
    as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2}'", intern = TRUE)) / (1024*1024)
  } else {
    # Windows fallback
    memory.limit() / 1024
  }

  # Conservative HPC-optimized settings for stability
  if (n_cores >= 32 && mem_gb >= 128) {
    # Large HPC node - use conservative settings
    return(list(
      cores = min(n_cores - 4, 32),  # Reserve more cores for system
      batch_size = 100,             # Smaller batches for stability
      memory_limit_gb = 80,         # Conservative memory limit
      checkpoint_interval = 5,      # More frequent checkpoints
      hash_threshold = 50000,
      verbose = TRUE
    ))
  } else if (n_cores >= 16 && mem_gb >= 64) {
    # Medium HPC node
    return(list(
      cores = min(n_cores - 2, 16),
      batch_size = 75,
      memory_limit_gb = 40,
      checkpoint_interval = 8,
      hash_threshold = 30000,
      verbose = TRUE
    ))
  } else if (n_cores >= 8 && mem_gb >= 32) {
    # Small HPC node or high-end workstation
    return(list(
      cores = min(n_cores - 1, 8),
      batch_size = 50,
      memory_limit_gb = 20,
      checkpoint_interval = 10,
      hash_threshold = 20000,
      verbose = TRUE
    ))
  } else {
    # Standard desktop/local machine
    return(list(
      cores = min(n_cores - 1, 2),   # Very conservative
      batch_size = 25,               # Small batches
      memory_limit_gb = 8,
      checkpoint_interval = 15,
      hash_threshold = 10000,
      verbose = TRUE
    ))
  }
}

# =============================================================================
# LOAD PRE-COMPUTED LOOKUP TABLES
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
# MAIN OPTIMIZED HPC FUNCTION
# =============================================================================

extract_species_mycorrhizal_data_optimized <- function(
  abstracts_data,
  output_file = "species_mycorrhizal_results.csv",
  batch_size = NULL,
  force_rerun = FALSE,
  verbose = TRUE,
  hpc_config = NULL
) {

  # HPC environment detection and configuration
  if (is.null(hpc_config)) {
    hpc_config <- detect_hpc_environment()
    if (verbose) {
      cat("🚀 HPC Environment Detected:\n")
      cat("   Cores:", hpc_config$cores, "\n")
      cat("   Memory limit:", hpc_config$memory_limit_gb, "GB\n")
      cat("   Batch size:", hpc_config$batch_size, "\n")
      cat("   Checkpoint interval:", hpc_config$checkpoint_interval, "batches\n")
    }
  }

  if (is.null(batch_size)) {
    batch_size <- hpc_config$batch_size
  }

  # Load pre-computed lookup tables (this should be fast!)
  if (verbose) cat("🔍 Loading pre-computed lookup tables...\n")

  lookup_tables <- load_precomputed_lookup_tables(verbose)
  if (is.null(lookup_tables)) {
    stop("❌ Could not load pre-computed lookup tables. Run 02_precompute_lookup_tables.R first.")
  }

  # Setup logging
  log_file <- setup_logging(output_file)

  if (verbose) {
    log_message("🚀 Starting optimized HPC species detection for", nrow(abstracts_data), "abstracts", log_file = log_file, verbose = verbose)
  }

  # Load species reference data (from root directory)
  species_file <- "species.rds"
  if (!file.exists(species_file)) {
    error_msg <- "❌ Species reference data not found in root directory. Please ensure species.rds exists."
    log_message(error_msg, log_file = log_file, verbose = TRUE)
    stop(error_msg)
  }

  species <- readRDS(species_file)
  if (verbose) log_message("   Loaded species reference data:", nrow(species), "species records", log_file = log_file, verbose = verbose)

  # HPC-optimized parallel processing setup
  if (verbose) log_message("   Setting up HPC parallel processing with", hpc_config$cores, "cores", log_file = log_file, verbose = verbose)

  # HPC Parallel Processing (enable for production runs)
  # For testing/debugging, uncomment the sequential mode block below:
  
  # SEQUENTIAL MODE (for testing):
  # cl <- NULL
  # hpc_config$cores <- 1  
  # batch_size <- min(25, batch_size)
  # hpc_config$checkpoint_interval <- 3
  # if (verbose) cat("   ✅ Running in sequential mode for reliability\n")
  
  # PARALLEL MODE (for HPC production):
  # Set up parallel cluster with timeout and diagnostics
  cluster_setup_start <- Sys.time()
  cl <- tryCatch({
    # Try FORK on Unix systems (Linux/macOS HPC) for better performance
    if (.Platform$OS.type == "unix") {
      if (verbose) cat("   🚀 Setting up FORK cluster with", hpc_config$cores, "cores...\n")
      cluster <- makeCluster(hpc_config$cores, type = "FORK")
      
      # Test cluster responsiveness
      if (verbose) cat("   📝 Testing cluster responsiveness...\n")
      test_result <- tryCatch({
        clusterEvalQ(cluster, {Sys.getpid()})
        TRUE
      }, error = function(e) FALSE)
      
      if (!test_result) {
        if (verbose) cat("   ⚠️ Cluster test failed, stopping cluster\n")
        stopCluster(cluster)
        stop("Cluster responsiveness test failed")
      }
      
      cluster_time <- as.numeric(difftime(Sys.time(), cluster_setup_start, units = "secs"))
      if (verbose) cat("   ✅ Cluster setup completed in", round(cluster_time, 1), "seconds\n")
      cluster
      
    } else {
      # Use PSOCK on Windows
      if (verbose) cat("   🚀 Setting up PSOCK cluster with", hpc_config$cores, "cores...\n")
      cluster <- makeCluster(hpc_config$cores, type = "PSOCK")
      cluster_time <- as.numeric(difftime(Sys.time(), cluster_setup_start, units = "secs"))
      if (verbose) cat("   ✅ Cluster setup completed in", round(cluster_time, 1), "seconds\n")
      cluster
    }
  }, error = function(e) {
    cluster_time <- as.numeric(difftime(Sys.time(), cluster_setup_start, units = "secs"))
    if (verbose) cat("   ❌ Parallel setup failed after", round(cluster_time, 1), "seconds:", e$message, "\n")
    if (verbose) cat("   🔄 Falling back to sequential processing\n")
    
    # Reduce to sequential-compatible settings
    hpc_config$cores <<- 1  # Use <<- to modify in parent scope
    batch_size <<- min(50, batch_size)  # Larger batches for sequential
    hpc_config$checkpoint_interval <<- 5
    
    if (verbose) cat("   ✅ Sequential mode configured (batch_size:", batch_size, ")\n")
    NULL
  })

  # Load required libraries on workers (only if cluster exists)
  if (!is.null(cl)) {
    if (verbose) cat("   📚 Loading required libraries on cluster workers...\n")
    lib_start <- Sys.time()
    
    tryCatch({
      clusterEvalQ(cl, {
        library(tidyverse)
        library(janitor)
        NULL
      })
      lib_time <- as.numeric(difftime(Sys.time(), lib_start, units = "secs"))
      if (verbose) cat("   ✅ Libraries loaded on workers in", round(lib_time, 1), "seconds\n")
      
      # If library loading took too long, consider fallback
      if (lib_time > 120) {  # 2 minutes
        if (verbose) cat("   ⚠️ Library loading took", round(lib_time, 1), "seconds - cluster may be slow\n")
      }
      
    }, error = function(e) {
      lib_time <- as.numeric(difftime(Sys.time(), lib_start, units = "secs"))
      if (verbose) cat("   ❌ Failed to load libraries on workers after", round(lib_time, 1), "seconds:", e$message, "\n")
      if (verbose) cat("   🔄 Stopping cluster and falling back to sequential\n")
      
      # Stop the problematic cluster
      tryCatch(stopCluster(cl), error = function(e2) {})
      cl <<- NULL
      
      # Update settings for sequential processing
      hpc_config$cores <<- 1
      batch_size <<- min(50, batch_size)
      hpc_config$checkpoint_interval <<- 5
      if (verbose) cat("   ✅ Sequential mode activated\n")
    })
  }

  # Process in batches with periodic checkpointing
  tic("Optimized HPC species detection")
  n_batches <- ceiling(nrow(abstracts_data) / batch_size)
  all_results <- list()

  if (verbose) {
    log_message("📊 Processing", nrow(abstracts_data), "abstracts in", n_batches, "batches", log_file = log_file, verbose = verbose)
    log_message("⚙️ Batch size:", batch_size, "abstracts per batch", log_file = log_file, verbose = verbose)
    log_message("💾 Checkpoint interval:", hpc_config$checkpoint_interval, "batches", log_file = log_file, verbose = verbose)
    log_message("🕐 Started at", format(Sys.time(), "%H:%M:%S"), log_file = log_file, verbose = verbose)
  }

  # Overall processing timeout (6 hours for large datasets)
  overall_start_time <- Sys.time()
  overall_timeout <- 6 * 60 * 60  # 6 hours in seconds

  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(abstracts_data))

    batch_data <- abstracts_data[start_idx:end_idx, ]

    if (verbose && (i %% max(1, floor(n_batches / 10)) == 0)) {
      log_message("   🚀 Batch", i, "of", n_batches, "(", nrow(batch_data), "abstracts)", log_file = log_file, verbose = verbose)
      log_message("      Processing rows", start_idx, "to", end_idx, log_file = log_file, verbose = verbose)
    }

    # Process batch (parallel if available, otherwise sequential)
    batch_results <- tryCatch({
      if (!is.null(cl)) {
        # Use HPC parallel processing
        if (verbose) log_message("      🚀 Processing with HPC parallel optimization", log_file = log_file, verbose = verbose)

        # Export batch data and lookup tables to workers with timeout
        if (verbose) cat("      🔄 Exporting data to", length(cl), "workers...\n")
        export_start <- Sys.time()
        
        tryCatch({
          # Export in stages to identify bottlenecks
          if (verbose) cat("      ⏱️ Exporting batch_data...\n")
          clusterExport(cl, "batch_data", envir = environment())
          
          if (verbose) cat("      ⏱️ Exporting lookup_tables (this may take a while)...\n")
          clusterExport(cl, "lookup_tables", envir = environment())
          
          if (verbose) cat("      ⏱️ Exporting batch index...\n")
          clusterExport(cl, "i", envir = environment())
          
          export_time <- as.numeric(difftime(Sys.time(), export_start, units = "secs"))
          if (verbose) cat("      ✅ Data export completed in", round(export_time, 1), "seconds\n")
          
          # Check if export took too long (over 5 minutes)
          if (export_time > 300) {
            if (verbose) cat("      ⚠️ Export took", round(export_time, 1), "seconds - may indicate memory issues\n")
          }
          
        }, error = function(e) {
          export_time <- as.numeric(difftime(Sys.time(), export_start, units = "secs"))
          if (verbose) cat("      ❌ Export failed after", round(export_time, 1), "seconds:", e$message, "\n")
          if (verbose) cat("      🔄 Falling back to sequential processing\n")
          # Set cl to NULL to trigger sequential fallback
          cl <- NULL
        })

        if (!is.null(cl)) {
          # Process abstracts in parallel across workers using parLapply
          process_single_abstract <- function(j) {
            abstract_text <- batch_data$abstract[j]
            abstract_id <- batch_data$id[j]
            predicted_label <- if ("predicted_label" %in% colnames(batch_data)) {
              batch_data$predicted_label[j]
            } else {
              "Presence"
            }

            # Use the optimized taxa detection functions
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

              # Return results
              if (length(all_rows) > 0) {
                bind_rows(all_rows)
              } else {
                tibble(id = abstract_id, match_type = "none")
              }
            }, error = function(e) {
              # Return error result
              tibble(id = abstract_id, error = e$message)
            })
          }

          # Export the processing function to workers
          clusterExport(cl, c("process_single_abstract", "batch_data", "lookup_tables"), envir = environment())

          # Process abstracts in parallel with timeout protection
          if (verbose) cat("      🚀 Starting parallel processing of", nrow(batch_data), "abstracts...\n")
          parallel_start <- Sys.time()
          timeout_per_batch <- 15 * 60  # 15 minutes per batch (increased for HPC)

          parallel_results <- tryCatch({
            # Simple timeout implementation
            results <- parLapplyLB(cl, 1:nrow(batch_data), process_single_abstract)

            # Check if it completed within timeout
            elapsed <- as.numeric(difftime(Sys.time(), parallel_start, units = "secs"))
            if (elapsed > timeout_per_batch) {
              cat("      ⚠️ Batch", i, "exceeded timeout (", round(elapsed, 1), "s)\n")
              NULL
            } else {
              results
            }
          }, error = function(e) {
            cat("      ⚠️ Parallel processing failed:", e$message, "\n")
            NULL
          })

          if (!is.null(parallel_results)) {
            batch_results <- bind_rows(parallel_results)
            # Don't return here, let it continue to the end of tryCatch
          } else {
            cat("      🔄 Using sequential fallback processing\n")
            cl <- NULL  # Force sequential fallback
          }
        }
      }

      # Sequential processing (either by choice or fallback)
      if (is.null(cl)) {
        if (verbose) log_message("      🔄 Processing with optimized sequential mode", log_file = log_file, verbose = verbose)

        sequential_results <- list()
        batch_errors <- 0
        species_found_count <- 0
        batch_start_time <- Sys.time()  # Add this line to fix the batch time error

        for (j in 1:nrow(batch_data)) {
          if (verbose && j %% 5 == 1) {
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

            if (length(candidates) > 0) {
              if (verbose && j %in% c(1, nrow(batch_data))) {
                cat("         Found", length(candidates), "candidate names\n")
              }
            } else {
              if (verbose && j %in% c(1, nrow(batch_data))) {
                cat("         No candidate names found\n")
              }
            }

            # Validate candidates
            valid_species <- batch_validate_names(candidates, lookup_tables)

            if (length(valid_species) > 0) {
              if (verbose && j %in% c(1, nrow(batch_data))) {
                cat("         Found", length(valid_species), "valid species\n")
              }
              species_found_count <- species_found_count + length(valid_species)
            } else {
              if (verbose && j %in% c(1, nrow(batch_data))) {
                cat("         No valid species found\n")
              }
            }

            # Process matches
            all_rows <- process_taxonomic_matches(
              valid_species, lookup_tables, abstract_text,
              abstract_id, predicted_label
            )

            # Return results
            if (length(all_rows) > 0) {
              result <- bind_rows(all_rows)
              sequential_results[[j]] <- result
            } else {
              sequential_results[[j]] <- tibble(id = abstract_id, match_type = "none")
            }
          }, error = function(e) {
            cat("         ⚠️ Error in abstract", j, ":", e$message, "\n")
            batch_errors <- batch_errors + 1
            sequential_results[[j]] <- tibble(id = abstract_id, error = e$message)
          })
        }

        batch_results <- bind_rows(sequential_results)

        if (verbose) {
          cat("      📊 Batch summary:\n")
          cat("         Abstracts processed:", nrow(batch_data), "\n")
          cat("         Species found:", species_found_count, "\n")
          cat("         Errors:", batch_errors, "\n")
          cat("         Batch time:", round(as.numeric(difftime(Sys.time(), batch_start_time, units = "secs")), 1), "seconds\n")
        }

        log_message(paste0("      Batch ", i, ": ", nrow(batch_data), " abstracts, ", species_found_count, " species, ", batch_errors, " errors"), log_file = log_file, verbose = verbose)
      }

      # Ensure batch_results is not NULL
      if (is.null(batch_results) || nrow(batch_results) == 0) {
        if (verbose) {
          cat("      ⚠️ No results from batch processing, creating empty result\n")
        }
        batch_results <- tibble(id = character(0))
      }
      
      # Return batch_results from tryCatch (assign to batch_results, don't return from function)
      batch_results
    }, error = function(e) {
      if (verbose) cat("     ⚠️ Batch", i, "failed:", e$message, ". Skipping.\n")
      # Return empty result for failed batch
      tibble(id = character(0))
    })

    all_results[[i]] <- batch_results
    cat("Batch", i, "added to all_results. nrow(batch_results):", nrow(batch_results), "\n")

    # Check for overall timeout
    overall_elapsed <- as.numeric(difftime(Sys.time(), overall_start_time, units = "secs"))
    if (overall_elapsed > overall_timeout) {
      timeout_msg <- paste0("⏰ Overall processing timed out after ", round(overall_elapsed/3600, 1), " hours")
      log_message(timeout_msg, log_file = log_file, verbose = TRUE)
      log_message("      🛑 Stopping processing due to overall timeout", log_file = log_file, verbose = TRUE)
      break
    }

    # Progress reporting
    if (verbose && (i %% max(1, floor(n_batches / 20)) == 0)) {
      species_found <- sum(unlist(lapply(all_results, function(x) {
        if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(0)
        sum(!is.na(x$resolved_name) | !is.na(x$canonicalName), na.rm = TRUE)
      })))
      progress_pct <- round(100 * i / n_batches, 1)
      progress_msg <- paste0("      📊 Progress: ", progress_pct, "% (", species_found, " species found)")
      log_message(progress_msg, log_file = log_file, verbose = verbose)

      # Also log to separate progress file for easy monitoring
      progress_file <- paste0(tools::file_path_sans_ext(output_file), "_progress.txt")
      write(paste0(format(Sys.time(), "%H:%M:%S"), " - ", progress_msg), progress_file, append = TRUE)
    }

    # Periodic checkpoint saving (more frequent for smaller batches)
    if (i %% hpc_config$checkpoint_interval == 0 || i == n_batches) {
      if (verbose) {
        checkpoint_msg <- paste0("💾 Saving checkpoint at batch ", i, " of ", n_batches)
        log_message(checkpoint_msg, log_file = log_file, verbose = verbose)
      }

      # Debug log for checkpoint
      cat("Checkpoint elements types:", paste(sapply(all_results, class), collapse = ", "), "\n")

      # Combine results so far with error handling
      checkpoint_data <- tryCatch({
        if (length(all_results) > 0) {
          bind_rows(all_results)
        } else {
          if (verbose) cat("   ⚠️ No results to save in checkpoint\n")
          tibble(id = character(0))
        }
      }, error = function(e) {
        if (verbose) cat("   ⚠️ Error combining results for checkpoint:", e$message, "\n")
        tibble(id = character(0))
      })

      # Only save if we have valid data or if it's the final batch
      if (nrow(checkpoint_data) > 0 || i == n_batches) {
        # Save checkpoint
        checkpoint_file <- paste0(tools::file_path_sans_ext(output_file), "_checkpoint_batch_", i, "_of_", n_batches, ".csv")
        write_csv(checkpoint_data, checkpoint_file)

        if (verbose) {
          cat("   💾 Checkpoint saved:", checkpoint_file, "\n")
          file_size_mb <- file.size(checkpoint_file) / (1024*1024)
          cat("      Size:", round(file_size_mb, 1), "MB\n")
        }
      }
    }

    # More frequent garbage collection for memory management
    if (i %% 5 == 0) {
      if (exists("aggressive_gc")) {
        aggressive_gc(verbose = FALSE)
      } else {
        gc(verbose = FALSE)
      }
    }
    
    # More frequent garbage collection for memory management
    if (i %% 5 == 0) {
      if (exists("aggressive_gc")) {
        aggressive_gc(verbose = FALSE)
      } else {
        gc(verbose = FALSE)
      }
    }
  }

  # Combine all results (robust binding)
  if (verbose) log_message("   Combining all batch results...", log_file = log_file, verbose = verbose)
  
  final_results <- tryCatch({
    # Filter valid data frames first for more robust combining
    valid_results <- all_results[sapply(all_results, function(x) is.data.frame(x) && nrow(x) > 0)]
    
    if (length(valid_results) > 0) {
      bind_rows(valid_results)
    } else {
      tibble(id = character(0))
    }
  }, error = function(e) {
    cat("Final bind_rows error:", e$message, "\n")
    bind_rows(all_results)  # Fallback
  })

  # Final results summary (fixed column access)
  tryCatch({
    total_abstracts <- length(unique(final_results$id))
    total_species <- 0
    if ("resolved_name" %in% colnames(final_results)) {
      total_species <- sum(!is.na(final_results$resolved_name))
    } else if ("match_type" %in% colnames(final_results)) {
      total_species <- sum(final_results$match_type == "species", na.rm = TRUE)
    }
    if (verbose) {
      cat("🚀 Optimized HPC processing completed!\n")
      cat("📈 Results:\n")
      cat("   - Total abstracts processed:", total_abstracts, "\n")
      cat("   - Total species detected:", total_species, "\n")
      cat("💾 Final results saved to:", output_file, "\n")
    }
  }, error = function(e) {
    cat("⚠️ Warning: Error generating summary statistics:", e$message, "\n")
    cat("   Results were still processed successfully\n")
  })

  # Save final results
  if (verbose) cat("   Saving final results...\n")

  tryCatch({
    write_csv(final_results, output_file)

    if (verbose) {
      file_size_mb <- file.size(output_file) / (1024*1024)
      cat("   💾 Final results saved (", round(file_size_mb, 1), "MB)\n")
    }
  }, error = function(e) {
    warning("Failed to save results to ", output_file, ": ", e$message)
  })

  # Final memory cleanup
  if (verbose) cat("   🧹 Final memory cleanup...\n")
  tryCatch({
    if (exists("aggressive_gc")) {
      aggressive_gc(verbose = verbose)
    } else {
      gc(verbose = verbose)
    }

    # Stop parallel cluster (if it exists)
    if (!is.null(cl)) {
      stopCluster(cl)
    }
  }, error = function(e) {
    cat("   ⚠️ Warning: Memory cleanup encountered minor issue:", e$message, "\n")
  })

  if (verbose) cat("   ✅ Optimized HPC processing complete\n")
  return(final_results)
}

# =============================================================================
# MAIN EXECUTION BLOCK
# =============================================================================

# Always run the main execution (removed interactive check for HPC usage)
  cat("🚀 === OPTIMIZED HPC SPECIES DETECTION ===\n")
  cat("Using pre-computed lookup tables for faster processing\n\n")

  # Setup comprehensive logging
  output_file <- "species_mycorrhizal_results_optimized.csv"
  log_file <- setup_logging(output_file)

  # Load consolidated dataset (from root directory)
  abstracts_file <- "consolidated_dataset.csv"
  log_message("📂 Loading consolidated dataset...", log_file = log_file, verbose = TRUE)

  if (!file.exists(abstracts_file)) {
    error_msg <- "❌ Consolidated dataset not found in root directory. Please ensure consolidated_dataset.csv exists."
    log_message(error_msg, log_file = log_file, verbose = TRUE)
    stop(error_msg)
  }

  load_start <- Sys.time()
  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)

  # PRODUCTION MODE: Process all abstracts (comment out for testing)
  # For testing, uncomment the next line:
  # abstracts_data <- abstracts_data[1:50, ]

  load_time <- as.numeric(difftime(Sys.time(), load_start, units = "secs"))
  log_message("✅ Loaded", nrow(abstracts_data), "abstracts from", abstracts_file, "in", round(load_time, 1), "seconds", log_file = log_file, verbose = TRUE)

  # Check if FUNGuild dataset exists (relative to project root)
  fun_data_path <- "funtothefun.csv"
  log_message("🔍 Checking for FUNGuild dataset...", log_file = log_file, verbose = TRUE)

  if (!file.exists(fun_data_path)) {
    error_msg <- paste0("⚠️ funtothefun.csv dataset not found in root directory.\n",
                       "   Expected path: ", fun_data_path, "\n",
                       "   Please ensure the dataset is available for mycorrhizal classification.")
    log_message(error_msg, log_file = log_file, verbose = TRUE)
    stop("❌ funtothefun.csv dataset not found")
  } else {
    log_message("✅ FUNGuild dataset found", log_file = log_file, verbose = TRUE)
  }

  # Detect HPC environment and get optimized configuration
  config_start <- Sys.time()
  hpc_config <- detect_hpc_environment()
  config_time <- as.numeric(difftime(Sys.time(), config_start, units = "secs"))
  log_message("HPC environment detection completed in", round(config_time, 1), "seconds", log_file = log_file, verbose = TRUE)

  if (exists("monitor_memory")) {
    mem_status <- monitor_memory(threshold_gb = hpc_config$memory_limit_gb, context = "Pre-processing")
    if (mem_status$above_threshold) {
      cat("⚠️ High memory usage detected. Consider using a node with more memory.\n")
      cat("   Will proceed with aggressive memory management.\n")
    }
  }

  # Extract species and mycorrhizal information with optimized settings
  log_message("🚀 Starting optimized processing pipeline...", log_file = log_file, verbose = TRUE)

  processing_start <- Sys.time()
  species_mycorrhizal_results <- extract_species_mycorrhizal_data_optimized(
    abstracts_data,
    output_file = output_file,
    batch_size = NULL,  # Use HPC-detected batch size
    verbose = TRUE,
    hpc_config = hpc_config
  )
  processing_time <- as.numeric(difftime(Sys.time(), processing_start, units = "mins"))
  log_message("Complete optimized species detection pipeline completed in", round(processing_time, 1), "minutes", log_file = log_file, verbose = TRUE)

  # Final memory cleanup
  log_message("🧹 Performing final memory cleanup...", log_file = log_file, verbose = TRUE)
  cleanup_start <- Sys.time()
  if (exists("aggressive_gc")) {
    aggressive_gc(verbose = TRUE)
  } else {
    gc(verbose = TRUE)
  }
  cleanup_time <- as.numeric(difftime(Sys.time(), cleanup_start, units = "secs"))
  log_message("Memory cleanup completed in", round(cleanup_time, 1), "seconds", log_file = log_file, verbose = TRUE)

  # Final summary with performance metrics
  total_time <- as.numeric(difftime(Sys.time(), load_start, units = "mins"))
  log_message("🎉 Optimized HPC processing completed successfully!", log_file = log_file, verbose = TRUE)
  log_message("📊 Total runtime:", round(total_time, 1), "minutes", log_file = log_file, verbose = TRUE)
  log_message("📈 Results:", nrow(species_mycorrhizal_results), "species detection records", log_file = log_file, verbose = TRUE)
  log_message("💾 Final output saved to:", output_file, log_file = log_file, verbose = TRUE)
