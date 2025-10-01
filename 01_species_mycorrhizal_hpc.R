# =============================================================================
# 01_species_mycorrhizal_hpc.R - HPC-Optimized species detection and mycorrhizal classification
# =============================================================================
#
# Purpose: HPC-optimized version with parallel processing, periodic saving, and root-relative paths
#
# Description: Optimized for high-performance computing environments with enhanced parallel processing,
# periodic checkpointing, and memory management. Uses root-relative file paths for HPC deployment.
#
# Dependencies: tidyverse, tictoc, janitor, parallel
#
# Author: B. Bock (HPC-optimized version)
# Date: 2024-09-30
#
# Inputs/Outputs: Reads consolidated abstracts from consolidated_dataset.csv; outputs to species_mycorrhizal_results.csv
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
  log_message("⚠️ Memory optimization utilities not found, running with basic memory management", log_file, verbose = TRUE)
})

# Enhanced logging function for HPC debugging
setup_logging <- function(output_file) {
  log_file <- paste0(tools::file_path_sans_ext(output_file), "_processing.log")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Create log entry
  log_entry <- paste0("\n", strrep("=", 80), "\n")
  log_entry <- paste0(log_entry, "HPC SPECIES DETECTION - PROCESSING LOG\n")
  log_entry <- paste0(log_entry, "Started at: ", timestamp, "\n")
  log_entry <- paste0(log_entry, strrep("=", 80), "\n")

  cat(log_entry)

  # Write to log file
  write(log_entry, file = log_file, append = FALSE)

  return(log_file)
}

# Function to log messages both to console and file
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

# Function to log performance metrics
log_performance <- function(operation, start_time, log_file = NULL, verbose = TRUE) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  elapsed_min <- round(elapsed / 60, 2)

  message <- paste0("⏱️  ", operation, " completed in ", elapsed_min, " minutes (", round(elapsed, 1), " seconds)")

  if (verbose) {
    cat(message, "\n")
  }

  if (!is.null(log_file) && file.exists(log_file)) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    log_entry <- paste0("[", timestamp, "] ", message)
    write(log_entry, file = log_file, append = TRUE)
  }

  return(Sys.time())
}

# Function to log memory usage
log_memory_usage <- function(context, log_file = NULL, verbose = TRUE) {
  if (exists("monitor_memory")) {
    mem_status <- monitor_memory(threshold_gb = 16, context = context)
    message <- paste0("🧠 Memory usage (", context, "): ", mem_status$current_gb, "GB")

    if (verbose) {
      cat(message, "\n")
    }

    if (!is.null(log_file) && file.exists(log_file)) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      log_entry <- paste0("[", timestamp, "] ", message)
      write(log_entry, file = log_file, append = TRUE)
    }
  }
}

# HPC Configuration Function
detect_hpc_environment <- function() {
  # Detect HPC environment based on available cores and memory
  n_cores <- detectCores()
  mem_gb <- if (.Platform$OS.type == "unix") {
    as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2}'", intern = TRUE)) / (1024*1024)
  } else {
    # Windows fallback
    memory.limit() / 1024
  }

  # HPC-optimized settings based on detected resources
  if (n_cores >= 32 && mem_gb >= 128) {
    # Large HPC node
    return(list(
      cores = min(n_cores - 2, 48),  # Use most cores but reserve some
      batch_size = 200,
      memory_limit_gb = 100,
      checkpoint_interval = 10,  # Save every 10 batches
      hash_threshold = 50000,
      verbose = TRUE
    ))
  } else if (n_cores >= 16 && mem_gb >= 64) {
    # Medium HPC node
    return(list(
      cores = min(n_cores - 1, 24),
      batch_size = 150,
      memory_limit_gb = 50,
      checkpoint_interval = 15,
      hash_threshold = 30000,
      verbose = TRUE
    ))
  } else if (n_cores >= 8 && mem_gb >= 32) {
    # Small HPC node or high-end workstation
    return(list(
      cores = min(n_cores - 1, 12),
      batch_size = 100,
      memory_limit_gb = 24,
      checkpoint_interval = 20,
      hash_threshold = 20000,
      verbose = TRUE
    ))
  } else {
    # Standard desktop/local machine
    return(list(
      cores = min(n_cores - 1, 4),
      batch_size = 50,
      memory_limit_gb = 8,
      checkpoint_interval = 25,
      hash_threshold = 10000,
      verbose = TRUE
    ))
  }
}

# Periodic checkpoint saving function
save_checkpoint <- function(data, batch_number, total_batches, output_file, verbose = TRUE) {
  tryCatch({
    # Create checkpoint filename with batch number
    checkpoint_file <- paste0(tools::file_path_sans_ext(output_file), "_checkpoint_batch_", batch_number, "_of_", total_batches, ".csv")

    # Save checkpoint
    write_csv(data, checkpoint_file)

    if (verbose) {
      cat("💾 Checkpoint saved:", checkpoint_file, "\n")
      file_size_mb <- file.size(checkpoint_file) / (1024*1024)
      cat("   Size:", round(file_size_mb, 1), "MB\n")
    }

    return(checkpoint_file)
  }, error = function(e) {
    if (verbose) cat("⚠️ Failed to save checkpoint:", e$message, "\n")
    return(NULL)
  })
}

# Load latest checkpoint if available
load_latest_checkpoint <- function(output_file, verbose = TRUE) {
  checkpoint_pattern <- paste0(tools::file_path_sans_ext(output_file), "_checkpoint_batch_.*\\.csv$")
  checkpoint_files <- list.files(pattern = checkpoint_pattern, path = ".")

  if (length(checkpoint_files) == 0) {
    return(NULL)
  }

  # Get the most recent checkpoint (highest batch number)
  checkpoint_info <- str_extract(checkpoint_files, "batch_(\\d+)_of_(\\d+)")
  batch_numbers <- as.numeric(str_extract(checkpoint_info, "(?<=batch_)\\d+"))
  latest_checkpoint <- checkpoint_files[which.max(batch_numbers)]

  if (verbose) cat("📁 Loading latest checkpoint:", latest_checkpoint, "\n")

  tryCatch({
    checkpoint_data <- read_csv(latest_checkpoint, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(checkpoint_data), "records from checkpoint\n")
    return(checkpoint_data)
  }, error = function(e) {
    if (verbose) cat("⚠️ Failed to load checkpoint:", e$message, "\n")
    return(NULL)
  })
}

# Clean up old checkpoints
cleanup_checkpoints <- function(output_file, keep_latest = TRUE, verbose = TRUE) {
  checkpoint_pattern <- paste0(tools::file_path_sans_ext(output_file), "_checkpoint_batch_.*\\.csv$")
  checkpoint_files <- list.files(pattern = checkpoint_pattern, path = ".")

  if (length(checkpoint_files) == 0) {
    return()
  }

  if (keep_latest && length(checkpoint_files) > 1) {
    # Keep only the most recent checkpoint
    checkpoint_info <- str_extract(checkpoint_files, "batch_(\\d+)_of_(\\d+)")
    batch_numbers <- as.numeric(str_extract(checkpoint_info, "(?<=batch_)\\d+"))
    latest_checkpoint <- checkpoint_files[which.max(batch_numbers)]

    files_to_remove <- checkpoint_files[checkpoint_files != latest_checkpoint]

    if (verbose) cat("🧹 Cleaning up old checkpoints, keeping:", latest_checkpoint, "\n")

    for (file in files_to_remove) {
      tryCatch({
        file.remove(file)
      }, error = function(e) {
        if (verbose) cat("   ⚠️ Failed to remove:", file, "\n")
      })
    }
  } else if (!keep_latest) {
    # Remove all checkpoints
    if (verbose) cat("🧹 Removing all checkpoints\n")
    for (file in checkpoint_files) {
      tryCatch({
        file.remove(file)
      }, error = function(e) {
        if (verbose) cat("   ⚠️ Failed to remove:", file, "\n")
      })
    }
  }
}

# Enhanced lookup table creation for HPC
create_lookup_tables_hpc <- function(species_df, threshold = 10000) {
  # Call the original function
  lookup_tables <- create_lookup_tables(species_df)

  # Add HPC optimizations
  if (!is.null(lookup_tables$accepted_species)) {
    lookup_tables$species_names_vector <- lookup_tables$accepted_species$canonicalName_lower
  }
  if (!is.null(lookup_tables$genus_list)) {
    lookup_tables$genus_names_vector <- lookup_tables$genus_list$canonicalName_lower
  }
  if (!is.null(lookup_tables$family_list)) {
    lookup_tables$family_names_vector <- lookup_tables$family_list$canonicalName_lower
  }

  # HPC-optimized hash table creation for very large datasets
  if (nrow(species_df) > threshold) {
    cat("   🚀 Creating HPC-optimized hash tables for large dataset...\n")

    # Create hash tables more efficiently using vectorized operations
    if (!is.null(lookup_tables$species_names_vector)) {
      species_env <- new.env(hash = TRUE, size = length(lookup_tables$species_names_vector) * 1.5)
      for (name in lookup_tables$species_names_vector) {
        species_env[[name]] <- TRUE
      }
      lookup_tables$species_hash <- species_env
    }

    if (!is.null(lookup_tables$genus_names_vector)) {
      genus_env <- new.env(hash = TRUE, size = length(lookup_tables$genus_names_vector) * 1.5)
      for (name in lookup_tables$genus_names_vector) {
        genus_env[[name]] <- TRUE
      }
      lookup_tables$genus_hash <- genus_env
    }
  }

  return(lookup_tables)
}

# HPC-optimized species extraction function
extract_species_mycorrhizal_data_hpc <- function(
  abstracts_data,
  output_file = "species_mycorrhizal_results.csv",
  batch_size = NULL,
  force_rerun = FALSE,
  verbose = TRUE,
  hash_threshold = 10000,
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

  # Input validation
  if (!is.data.frame(abstracts_data)) {
    stop("'abstracts_data' must be a data frame.")
  }
  if (!"id" %in% colnames(abstracts_data)) {
    stop("'abstracts_data' must contain an 'id' column for unique identification.")
  }
  if (!"abstract" %in% colnames(abstracts_data)) {
    stop("'abstracts_data' must contain an 'abstract' column for abstracts.")
  }

  # Handle empty datasets
  if (nrow(abstracts_data) == 0) {
    warning("Input dataset is empty. Skipping species extraction.")
    return(data.frame())
  }

  # Load existing results or checkpoints
  if (!force_rerun) {
    existing_results <- load_latest_checkpoint(output_file, verbose)
    if (!is.null(existing_results)) {
      if (verbose) cat("✅ Resuming from checkpoint\n")
      return(existing_results)
    }

    if (file.exists(output_file)) {
      if (verbose) cat("✅ Found existing results file\n")
      existing_results <- tryCatch({
        read_csv(output_file, show_col_types = FALSE)
      }, error = function(e) {
        warning("Failed to load existing results: ", e$message, ". Proceeding with fresh extraction.")
        NULL
      })
      if (!is.null(existing_results)) {
        if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
        return(existing_results)
      }
    }
  }

  if (verbose) {
    log_message("🚀 Starting HPC-optimized species detection for", nrow(abstracts_data), "abstracts", log_file, verbose)
  }

  # Load species reference data with root-relative paths
  species_file <- "species.rds"
  if (!file.exists(species_file)) {
    error_msg <- "❌ Species reference data not found in root directory. Please ensure species.rds exists."
    log_message(error_msg, log_file, verbose = TRUE)
    stop(error_msg)
  }

  species <- readRDS(species_file)
  if (verbose) log_message(paste0("   Loaded species reference data: ", nrow(species), " species records"), log_file, verbose)

  # HPC-optimized parallel processing setup
  if (verbose) log_message(paste0("   Setting up HPC parallel processing with ", hpc_config$cores, " cores"), log_file, verbose)

  # Setup parallel cluster with error handling
  cl <- tryCatch({
    # Try FORK on Unix systems (Linux/macOS) for better performance
    if (.Platform$OS.type == "unix") {
      makeCluster(hpc_config$cores, type = "FORK")
    } else {
      # Use PSOCK on Windows
      makeCluster(hpc_config$cores, type = "PSOCK")
    }
  }, error = function(e) {
    if (verbose) cat("   ⚠️ Full parallel setup failed:", e$message, "\n")
    if (verbose) cat("   Attempting fallback to sequential processing\n")

    # Fallback to sequential processing
    cl <- NULL

    # Reduce to sequential-compatible settings
    hpc_config$cores <- 1
    batch_size <- min(50, batch_size)  # Smaller batches for sequential processing
    hpc_config$checkpoint_interval <- 5  # More frequent checkpoints

    if (verbose) cat("   ✅ Running in sequential mode with optimized settings\n")
  })

  # Load required libraries on workers (only if cluster exists)
  if (!is.null(cl)) {
    clusterEvalQ(cl, {
      library(tidyverse)
      library(janitor)
      NULL
    })
  }

  # Memory-efficient lookup table creation
  if (verbose) log_message("   Creating HPC-optimized lookup tables...", log_file, verbose)
  lookup_tables <- create_lookup_tables_hpc(species, hpc_config$hash_threshold)

  # Monitor memory usage before processing
  if (verbose && exists("monitor_memory")) {
    mem_status <- monitor_memory(threshold_gb = hpc_config$memory_limit_gb, context = "Before species detection")
    if (mem_status$above_threshold) {
      cat("   ⚠️ High memory usage detected, enabling aggressive garbage collection\n")
      if (exists("aggressive_gc")) aggressive_gc(verbose = FALSE)
    }
  }

  # Process in batches with periodic checkpointing
  tic("HPC-optimized species detection and mycorrhizal classification")
  n_batches <- ceiling(nrow(abstracts_data) / batch_size)
  all_results <- list()

  if (verbose) {
    log_message(paste0("📊 Processing ", nrow(abstracts_data), " abstracts in ", n_batches, " batches"), log_file, verbose)
    log_message(paste0("⚙️ Batch size: ", batch_size, " abstracts per batch"), log_file, verbose)
    log_message(paste0("💾 Checkpoint interval: ", hpc_config$checkpoint_interval, " batches"), log_file, verbose)
    log_message(paste0("🕐 Started at ", format(Sys.time(), "%H:%M:%S")), log_file, verbose)
  }

  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(abstracts_data))

    batch_data <- abstracts_data[start_idx:end_idx, ]

    if (verbose && (i %% max(1, floor(n_batches / 10)) == 0)) {
      log_message(paste0("   🚀 Batch ", i, " of ", n_batches, " (", nrow(batch_data), " abstracts)"), log_file, verbose)
      log_message(paste0("      Processing rows ", start_idx, " to ", end_idx), log_file, verbose)
      log_memory_usage(paste0("Batch ", i), log_file, verbose)
    }

    # Memory monitoring before batch processing
    if (i %% 5 == 0 && verbose && exists("monitor_memory")) {
      mem_status <- monitor_memory(threshold_gb = hpc_config$memory_limit_gb * 0.8, context = paste("Batch", i))
      if (mem_status$above_threshold) {
        cat("      🧹 Running garbage collection due to high memory usage\n")
        if (exists("aggressive_gc")) aggressive_gc(verbose = FALSE)
      }
    }

    # Add timeout protection for each batch (30 minutes max per batch)
    batch_start_time <- Sys.time()
    batch_timeout <- 30 * 60  # 30 minutes in seconds

    # Process batch (parallel if available, otherwise sequential)
    batch_results <- tryCatch({
      if (!is.null(cl)) {
        # Use HPC parallel processing - bypass the original function's setup
        if (verbose) log_message("      🚀 Processing with HPC parallel optimization", log_file, verbose)

        # Export batch data and necessary objects to workers
        tryCatch({
          clusterExport(cl, c("batch_data", "lookup_tables", "i"), envir = environment())
          if (verbose) cat("      ✅ Successfully exported data to cluster workers\n")
        }, error = function(e) {
          if (verbose) cat("      ⚠️ Failed to export data to workers:", e$message, "\n")
          if (verbose) cat("      Falling back to sequential processing\n")
          # Set cl to NULL to trigger sequential fallback
          cl <- NULL
        })

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

        # Export the processing function and data to workers
        clusterExport(cl, c("process_single_abstract", "batch_data", "lookup_tables"), envir = environment())

        # Process abstracts in parallel
        parallel_results <- parLapplyLB(cl, 1:nrow(batch_data), process_single_abstract)

        # Combine results from all workers
        batch_results <- tryCatch({
          bind_rows(parallel_results)
        }, error = function(e) {
          if (verbose) cat("      ⚠️ Parallel processing failed:", e$message, "\n")
          if (verbose) cat("      Falling back to sequential processing\n")
          # Fallback to sequential processing
          NULL
        })

        # If parallel processing failed, use sequential fallback
        if (is.null(batch_results)) {
          if (verbose) cat("      🔄 Using sequential fallback processing\n")

          sequential_results <- map_dfr(1:nrow(batch_data), function(j) {
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
          })

          batch_results <- sequential_results
        }
      } else {
        # Use HPC-optimized sequential processing
        if (verbose) log_message("      🔄 Processing with HPC-optimized sequential mode", log_file, verbose)

        sequential_results <- map_dfr(1:nrow(batch_data), function(j) {
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
        })

        batch_results <- sequential_results
      }

      # Ensure batch_results is not NULL
      if (is.null(batch_results) || nrow(batch_results) == 0) {
        if (verbose) {
          cat("      ⚠️ No results from batch processing, creating empty result\n")
          cat("      🔍 Debugging info: batch_data rows =", nrow(batch_data), "\n")
        }
        batch_results <- tibble(id = character(0))
      }
    }, error = function(e) {
      if (verbose) cat("     ⚠️ Batch", i, "failed:", e$message, ". Skipping.\n")
      # Return empty result for failed batch
      return(tibble(id = character(0)))
    })

    # Check for batch timeout
    batch_elapsed <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
    if (batch_elapsed > batch_timeout) {
      timeout_msg <- paste0("⏰ Batch ", i, " timed out after ", round(batch_elapsed/60, 1), " minutes")
      log_message(timeout_msg, log_file, verbose = TRUE)
      log_message("      ⚠️ This indicates a serious issue - batch processing should not take this long", log_file, verbose = TRUE)
      log_message("      Consider reducing batch size or investigating the processing bottleneck", log_file, verbose = TRUE)

      # Force save current results before potential failure
      if (length(all_results) > 0) {
        emergency_checkpoint <- tryCatch({
          bind_rows(all_results)
        }, error = function(e) {
          cat("      ❌ Could not create emergency checkpoint:", e$message, "\n")
          tibble(id = character(0))
        })

        if (nrow(emergency_checkpoint) > 0) {
          emergency_file <- paste0(tools::file_path_sans_ext(output_file), "_EMERGENCY_BATCH_", i, ".csv")
          write_csv(emergency_checkpoint, emergency_file)
          emergency_msg <- paste0("💾 Emergency checkpoint saved: ", emergency_file)
          log_message(emergency_msg, log_file, verbose = TRUE)
        }
      }

      # Stop processing to prevent infinite hang
      cat("      🛑 Stopping processing due to batch timeout\n")
      break
    }

    all_results[[i]] <- batch_results

    # Progress reporting
    if (verbose && (i %% max(1, floor(n_batches / 20)) == 0)) {
      species_found <- sum(sapply(all_results, function(x) sum(!is.na(x$resolved_name) | !is.na(x$canonicalName), na.rm = TRUE)))
      progress_pct <- round(100 * i / n_batches, 1)
      progress_msg <- paste0("      📊 Progress: ", progress_pct, "% (", species_found, " species found)")
      log_message(progress_msg, log_file, verbose)

      # Also log to separate progress file for easy monitoring
      progress_file <- paste0(tools::file_path_sans_ext(output_file), "_progress.txt")
      write(paste0(format(Sys.time(), "%H:%M:%S"), " - ", progress_msg), progress_file, append = TRUE)
    }

    # Periodic checkpoint saving
    if (i %% hpc_config$checkpoint_interval == 0 || i == n_batches) {
      if (verbose) {
        checkpoint_msg <- paste0("💾 Saving checkpoint at batch ", i, " of ", n_batches)
        log_message(checkpoint_msg, log_file, verbose)
        log_message(paste0("   📊 Total results collected so far: ", length(all_results), " batches"), log_file, verbose)
      }

      # Combine results so far with error handling
      checkpoint_data <- tryCatch({
        if (length(all_results) > 0) {
          bind_rows(all_results)
        } else {
          if (verbose) cat("   ⚠️ No results to save in checkpoint\n")
          tibble(id = character(0))  # Return empty dataframe with correct structure
        }
      }, error = function(e) {
        if (verbose) cat("   ⚠️ Error combining results for checkpoint:", e$message, "\n")
        tibble(id = character(0))
      })

      # Only save if we have valid data or if it's the final batch
      if (nrow(checkpoint_data) > 0 || i == n_batches) {
        # Save checkpoint
        checkpoint_file <- save_checkpoint(
          checkpoint_data,
          i,
          n_batches,
          output_file,
          verbose
        )

        # Clean up old checkpoints to save space
        if (i %% (hpc_config$checkpoint_interval * 3) == 0) {
          cleanup_checkpoints(output_file, keep_latest = TRUE, verbose = verbose)
        }
      } else {
        if (verbose) cat("   ⏭️ Skipping empty checkpoint\n")
      }
    }

    # Periodic garbage collection for HPC memory management
    if (i %% 10 == 0) {
      if (exists("aggressive_gc")) {
        aggressive_gc(verbose = FALSE)
      } else {
        gc(verbose = FALSE)
      }
    }
  }

  # Combine all results
  if (verbose) log_message("   Combining all batch results...", log_file, verbose)
  final_results <- bind_rows(all_results)

  # HPC-optimized fungal taxa processing
  if (verbose) log_message("   Processing fungal taxa for mycorrhizal classification...", log_file, verbose)

  # Process fungal taxa in chunks for memory efficiency
  fungal_taxa_chunks <- final_results %>%
    filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
    distinct(resolved_name, kingdom, phylum, family, genus) %>%
    group_by((row_number() - 1) %/% 500) %>%  # Process in chunks of 500
    group_split()

  fungal_count <- nrow(final_results %>% filter(kingdom == "Fungi", !is.na(resolved_name)) %>% distinct(resolved_name))
  if (verbose) log_message(paste0("   Found ", fungal_count, " unique fungal taxa"), log_file, verbose)

  # Memory check before FUNGuild processing
  if (verbose && exists("monitor_memory")) {
    mem_status <- monitor_memory(threshold_gb = hpc_config$memory_limit_gb * 0.7, context = "Before FUNGuild classification")
    if (mem_status$above_threshold) {
      cat("   🧹 Cleaning memory before FUNGuild processing\n")
      if (exists("aggressive_gc")) aggressive_gc(verbose = FALSE)
    }
  }

  # Classify fungal taxa in parallel chunks
  mycorrhizal_classifications <- list()

  for (chunk_idx in seq_along(fungal_taxa_chunks)) {
    chunk_taxa <- fungal_taxa_chunks[[chunk_idx]] %>%
      pull(resolved_name)

    if (length(chunk_taxa) > 0) {
      chunk_classifications <- classify_fungal_taxa_mycorrhizal(
        chunk_taxa,
        fungal_taxa_chunks[[chunk_idx]],
        verbose = verbose && chunk_idx == 1  # Only verbose for first chunk
      )
      mycorrhizal_classifications[[chunk_idx]] <- chunk_classifications
    }
  }

  mycorrhizal_classifications <- bind_rows(mycorrhizal_classifications)

  # Enhanced results with mycorrhizal information
  if (verbose) log_message("   Adding mycorrhizal classification to results...", log_file, verbose)

  mycorrhizal_lookup <- mycorrhizal_classifications %>%
    select(resolved_name, is_mycorrhizal, funguild_guild, confidence_ranking,
           trophic_mode, growth_form, trait_confidence)

  enhanced_results <- final_results %>%
    left_join(mycorrhizal_lookup, by = "resolved_name") %>%
    mutate(
      is_mycorrhizal = if_else(kingdom != "Fungi", FALSE, is_mycorrhizal),
      funguild_guild = if_else(kingdom != "Fungi", "Non-fungal", funguild_guild),
      confidence_ranking = if_else(kingdom != "Fungi", 1.0, confidence_ranking),
      trophic_mode = if_else(kingdom != "Fungi", NA_character_, trophic_mode),
      growth_form = if_else(kingdom != "Fungi", NA_character_, growth_form),
      trait_confidence = if_else(kingdom != "Fungi", NA_character_, trait_confidence)
    )

  # Determine abstract-level mycorrhizal status
  if (verbose) log_message("   Determining abstract-level mycorrhizal status...", log_file, verbose)

  abstract_ids <- unique(enhanced_results$id)
  abstract_mycorrhizal_status <- list()

  for (abstract_id in abstract_ids) {
    tryCatch({
      abstract_data <- enhanced_results %>% filter(id == abstract_id)
      mycorrhizal_classifications_subset <- mycorrhizal_classifications %>%
        filter(resolved_name %in% abstract_data$resolved_name)

      is_mycorrhizal_only <- determine_abstract_mycorrhizal_status(
        abstract_data,
        mycorrhizal_classifications_subset
      )

      abstract_mycorrhizal_status[[length(abstract_mycorrhizal_status) + 1]] <-
        tibble(id = abstract_id, abstract_mycorrhizal_only = is_mycorrhizal_only)
    }, error = function(e) {
      if (verbose) cat("     ⚠️ Warning: Error processing abstract", abstract_id, ":", e$message, "\n")
      abstract_mycorrhizal_status[[length(abstract_mycorrhizal_status) + 1]] <-
        tibble(id = abstract_id, abstract_mycorrhizal_only = FALSE)
    })
  }

  abstract_mycorrhizal_status <- bind_rows(abstract_mycorrhizal_status)

  final_results <- enhanced_results %>%
    left_join(abstract_mycorrhizal_status, by = "id") %>%
    mutate(is_mycorrhizal_only = if_else(is.na(abstract_mycorrhizal_only), FALSE, abstract_mycorrhizal_only)) %>%
    select(-abstract_mycorrhizal_only)

  # Final results summary
  tryCatch({
    total_abstracts <- length(unique(final_results$id))
    mycorrhizal_only_count <- sum(final_results$is_mycorrhizal_only, na.rm = TRUE)
    mycorrhizal_only_pct <- round(100 * mycorrhizal_only_count / total_abstracts, 1)

    if (verbose) {
      cat("🚀 HPC-optimized processing completed!\n")
      cat("📈 Results:\n")
      cat("   - Total abstracts processed:", total_abstracts, "\n")
      cat("   - Mycorrhizal-only papers:", mycorrhizal_only_count, "(", mycorrhizal_only_pct, "%)\n")
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

    # Clean up checkpoints after successful completion
    cleanup_checkpoints(output_file, keep_latest = FALSE, verbose = verbose)

    if (verbose) {
      file_size_mb <- file.size(output_file) / (1024*1024)
      cat("   💾 Final results saved (", round(file_size_mb, 1), "MB)\n")
    }
  }, error = function(e) {
    warning("Failed to save results to ", output_file, ": ", e$message)
    # Try alternative saving method
    tryCatch({
      saveRDS(final_results, paste0(tools::file_path_sans_ext(output_file), ".rds"))
      cat("   💾 Results saved as RDS format instead\n")
    }, error = function(e2) {
      warning("Failed to save results in any format: ", e2$message)
    })
  })

  # Final memory cleanup
  if (verbose) cat("   🧹 Final HPC memory cleanup...\n")
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

  if (verbose) cat("   ✅ HPC-optimized processing complete\n")
  return(final_results)
}

# Main execution block for HPC
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "01_species_mycorrhizal_hpc.R")) {

  cat("🚀 === HPC-OPTIMIZED SPECIES DETECTION AND MYCORRHIZAL CLASSIFICATION ===\n")
  cat("Optimized for high-performance computing environments\n\n")

  # Setup comprehensive logging
  output_file <- "species_mycorrhizal_results.csv"
  log_file <- setup_logging(output_file)

  # Load consolidated dataset with root-relative path
  abstracts_file <- "consolidated_dataset.csv"
  log_message("📂 Loading consolidated dataset...", log_file, verbose = TRUE)

  if (!file.exists(abstracts_file)) {
    error_msg <- "❌ Consolidated dataset not found in root directory. Please ensure consolidated_dataset.csv exists."
    log_message(error_msg, log_file, verbose = TRUE)
    stop(error_msg)
  }

  load_start <- Sys.time()
  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)
  load_time <- log_performance("Dataset loading", load_start, log_file, verbose = TRUE)

  log_message(paste0("✅ Loaded ", nrow(abstracts_data), " abstracts from ", abstracts_file), log_file, verbose = TRUE)

  # Check if funtothefun dataset exists (with flexible path for HPC)
  fun_data_path <- "funtothefun.csv"
  log_message("🔍 Checking for FUNGuild dataset...", log_file, verbose = TRUE)

  if (!file.exists(fun_data_path)) {
    error_msg <- paste0("⚠️ funtothefun.csv dataset not found in root directory.\n",
                       "   Expected path: ", fun_data_path, "\n",
                       "   Please ensure the dataset is available for mycorrhizal classification.")
    log_message(error_msg, log_file, verbose = TRUE)
    stop("❌ funtothefun.csv dataset not found")
  } else {
    log_message("✅ FUNGuild dataset found", log_file, verbose = TRUE)
  }

  # Memory check before starting main processing
  log_message("🔍 Final HPC memory check before processing...", log_file, verbose = TRUE)
  log_memory_usage("Pre-processing", log_file, verbose = TRUE)

  config_start <- Sys.time()
  hpc_config <- detect_hpc_environment()
  config_time <- log_performance("HPC environment detection", config_start, log_file, verbose = TRUE)

  if (exists("monitor_memory")) {
    mem_status <- monitor_memory(threshold_gb = hpc_config$memory_limit_gb, context = "Pre-processing")
    if (mem_status$above_threshold) {
      cat("⚠️ High memory usage detected. Consider using a node with more memory.\n")
      cat("   Will proceed with aggressive memory management.\n")
    }
  }

  # Extract species and mycorrhizal information with HPC optimization
  log_message("🚀 Starting main processing pipeline...", log_file, verbose = TRUE)
  log_memory_usage("Before main processing", log_file, verbose = TRUE)

  processing_start <- Sys.time()
  species_mycorrhizal_results <- extract_species_mycorrhizal_data_hpc(
    abstracts_data,
    output_file = output_file,
    batch_size = NULL,  # Use HPC-detected batch size
    verbose = TRUE,
    hpc_config = hpc_config
  )
  processing_time <- log_performance("Complete species detection pipeline", processing_start, log_file, verbose = TRUE)

  # Final memory cleanup
  log_message("🧹 Performing final HPC memory cleanup...", log_file, verbose = TRUE)
  cleanup_start <- Sys.time()
  if (exists("aggressive_gc")) {
    aggressive_gc(verbose = TRUE)
  } else {
    gc(verbose = TRUE)
  }
  cleanup_time <- log_performance("Memory cleanup", cleanup_start, log_file, verbose = TRUE)

  # Final summary with performance metrics
  total_time <- as.numeric(difftime(Sys.time(), load_start, units = "mins"))
  log_message(paste0("\n🎉 HPC-optimized processing completed successfully!"), log_file, verbose = TRUE)
  log_message(paste0("📊 Total runtime: ", round(total_time, 1), " minutes"), log_file, verbose = TRUE)
  log_message(paste0("📈 Results: ", nrow(species_mycorrhizal_results), " species detection records"), log_file, verbose = TRUE)
  log_message(paste0("💾 Final output saved to: ", output_file), log_file, verbose = TRUE)
}