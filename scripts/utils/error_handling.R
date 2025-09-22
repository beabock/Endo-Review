# =============================================================================
# error_handling.R - Error handling and validation utilities for pipeline
# =============================================================================
#
# Purpose: Provide robust error handling, validation, and recovery functions
#
# Description: Comprehensive error handling utilities including safe execution wrappers, file validation,
# data recovery, progress tracking, memory-safe processing with chunking, backup creation, and
# time estimation functions for the endophyte review pipeline.
#
# Dependencies: None
#
# Author: B. Bock
# Date: 2024-09-22
#
# Inputs/Outputs: Logs errors to results/logs/pipeline_errors.log; creates backup files in results/backups/
#
# =============================================================================
# ERROR HANDLING UTILITIES
# =============================================================================

#' Safe execution wrapper with detailed error reporting
#' @param expr Expression to execute safely
#' @param context Description of what's being executed
#' @param fallback_value Value to return on error (default: NULL)
#' @param log_errors Whether to log errors to file (default: TRUE)
safe_execute <- function(expr, context = "Unknown operation",
                        fallback_value = NULL, log_errors = TRUE) {

  # Capture the expression for re-evaluation if needed
  expr_sub <- substitute(expr)

  result <- NULL
  warning_msg <- NULL

  tryCatch({
    result <- eval(expr_sub, parent.frame())
    return(list(success = TRUE, result = result, error = NULL, warning = warning_msg))

  }, error = function(e) {
    error_msg <- paste("Error in", context, ":", e$message)

    if (log_errors) {
      log_error(error_msg, context)
    }

    cat("⚠️ ", error_msg, "\n")

    return(list(success = FALSE, result = fallback_value, error = e$message, warning = NULL))

  }, warning = function(w) {
    warning_msg <- paste("Warning in", context, ":", w$message)
    cat("⚠️ ", warning_msg, "\n")

    # Continue execution but log the warning
    if (log_errors) {
      log_error(paste("WARNING:", warning_msg), context)
    }

    # Re-evaluate the expression to get the result
    result <- eval(expr_sub, parent.frame())
    return(list(success = TRUE, result = result, error = NULL, warning = warning_msg))
  })
}

#' Log errors to file with timestamp
#' @param error_msg Error message to log
#' @param context Context where error occurred
log_error <- function(error_msg, context = "Unknown") {
  log_dir <- "results/logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  log_file <- file.path(log_dir, "pipeline_errors.log")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  log_entry <- paste0("[", timestamp, "] [", context, "] ", error_msg, "\n")
  
  cat(log_entry, file = log_file, append = TRUE)
}

#' Validate data frame structure
#' @param data Data frame to validate
#' @param required_columns Vector of required column names
#' @param min_rows Minimum number of rows required
#' @param context Description for error messages
validate_dataframe <- function(data, required_columns = NULL, min_rows = 1, 
                              context = "Data validation") {
  
  errors <- c()
  
  # Check if it's a data frame
  if (!is.data.frame(data)) {
    errors <- c(errors, "Input is not a data frame")
  }
  
  # Check minimum rows
  if (nrow(data) < min_rows) {
    errors <- c(errors, paste("Insufficient rows:", nrow(data), "< required", min_rows))
  }
  
  # Check required columns
  if (!is.null(required_columns)) {
    missing_cols <- setdiff(required_columns, names(data))
    if (length(missing_cols) > 0) {
      errors <- c(errors, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Check for completely empty data frame
  if (nrow(data) == 0 || ncol(data) == 0) {
    errors <- c(errors, "Data frame is empty")
  }
  
  if (length(errors) > 0) {
    error_msg <- paste("Data validation failed in", context, ":", paste(errors, collapse = "; "))
    stop(error_msg)
  }
  
  return(TRUE)
}

#' Memory-safe data processing with chunking
#' @param data Data to process
#' @param process_func Function to apply to each chunk
#' @param chunk_size Size of each chunk
#' @param context Description for error messages
process_in_chunks <- function(data, process_func, chunk_size = 1000, 
                             context = "Chunk processing") {
  
  if (nrow(data) <= chunk_size) {
    # Process all at once if small enough
    return(safe_execute(process_func(data), context)$result)
  }
  
  # Process in chunks
  n_chunks <- ceiling(nrow(data) / chunk_size)
  results <- list()
  
  cat("Processing", nrow(data), "rows in", n_chunks, "chunks of size", chunk_size, "\n")
  
  for (i in 1:n_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, nrow(data))
    
    chunk_data <- data[start_idx:end_idx, ]
    
    cat("  Processing chunk", i, "of", n_chunks, "(rows", start_idx, "to", end_idx, ")\n")
    
    chunk_result <- safe_execute(
      process_func(chunk_data), 
      paste(context, "- chunk", i),
      fallback_value = data.frame()
    )
    
    if (chunk_result$success) {
      results[[i]] <- chunk_result$result
    } else {
      cat("  ⚠️ Chunk", i, "failed, skipping\n")
    }
    
    # Memory management
    if (i %% 10 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  # Combine results
  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    stop("All chunks failed in ", context)
  }
}

# =============================================================================
# FILE VALIDATION UTILITIES
# =============================================================================

#' Comprehensive file validation
#' @param file_path Path to file to validate
#' @param expected_columns Expected column names (for CSV files)
#' @param min_size_mb Minimum file size in MB
#' @param max_age_days Maximum age in days (NULL to skip)
validate_file <- function(file_path, expected_columns = NULL, 
                         min_size_mb = 0, max_age_days = NULL) {
  
  validation_results <- list(
    exists = file.exists(file_path),
    readable = FALSE,
    size_ok = FALSE,
    age_ok = TRUE,
    columns_ok = TRUE,
    errors = c()
  )
  
  # Check existence
  if (!validation_results$exists) {
    validation_results$errors <- c(validation_results$errors, "File does not exist")
    return(validation_results)
  }
  
  # Check readability
  validation_results$readable <- file.access(file_path, 4) == 0
  if (!validation_results$readable) {
    validation_results$errors <- c(validation_results$errors, "File is not readable")
  }
  
  # Check file size
  file_info <- file.info(file_path)
  file_size_mb <- file_info$size / (1024^2)
  validation_results$size_ok <- file_size_mb >= min_size_mb
  if (!validation_results$size_ok) {
    validation_results$errors <- c(validation_results$errors, 
                                  paste("File too small:", round(file_size_mb, 2), "MB <", min_size_mb, "MB"))
  }
  
  # Check file age
  if (!is.null(max_age_days)) {
    file_age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))
    validation_results$age_ok <- file_age_days <= max_age_days
    if (!validation_results$age_ok) {
      validation_results$errors <- c(validation_results$errors,
                                    paste("File too old:", round(file_age_days, 1), "days >", max_age_days, "days"))
    }
  }
  
  # Check columns (for CSV files)
  if (!is.null(expected_columns) && tools::file_ext(file_path) == "csv") {
    tryCatch({
      # Read just the header
      header <- names(readr::read_csv(file_path, n_max = 0, show_col_types = FALSE))
      missing_cols <- setdiff(expected_columns, header)
      validation_results$columns_ok <- length(missing_cols) == 0
      if (!validation_results$columns_ok) {
        validation_results$errors <- c(validation_results$errors,
                                      paste("Missing columns:", paste(missing_cols, collapse = ", ")))
      }
    }, error = function(e) {
      validation_results$columns_ok <- FALSE
      validation_results$errors <- c(validation_results$errors, paste("Cannot read file header:", e$message))
    })
  }
  
  return(validation_results)
}

#' Validate multiple files at once
#' @param file_list Named list of file paths to validate
#' @param requirements Named list of requirements for each file
validate_file_set <- function(file_list, requirements = list()) {
  
  results <- list()
  all_valid <- TRUE
  
  for (file_name in names(file_list)) {
    file_path <- file_list[[file_name]]
    file_requirements <- requirements[[file_name]] %||% list()
    
    cat("Validating", file_name, ":", file_path, "\n")
    
    validation <- do.call(validate_file, c(list(file_path = file_path), file_requirements))
    results[[file_name]] <- validation
    
    if (length(validation$errors) > 0) {
      all_valid <- FALSE
      cat("  ❌ Validation failed:\n")
      for (error in validation$errors) {
        cat("    -", error, "\n")
      }
    } else {
      cat("  ✅ Validation passed\n")
    }
  }
  
  return(list(all_valid = all_valid, file_results = results))
}

# =============================================================================
# RECOVERY UTILITIES
# =============================================================================

#' Attempt to recover from common data issues
#' @param data Data frame with potential issues
#' @param context Description for logging
recover_data_issues <- function(data, context = "Data recovery") {
  
  original_rows <- nrow(data)
  recovery_log <- c()
  
  # Remove completely empty rows
  empty_rows <- rowSums(is.na(data) | data == "", na.rm = TRUE) == ncol(data)
  if (any(empty_rows)) {
    data <- data[!empty_rows, ]
    removed_empty <- sum(empty_rows)
    recovery_log <- c(recovery_log, paste("Removed", removed_empty, "completely empty rows"))
  }
  
  # Handle duplicate IDs if ID column exists
  if ("id" %in% names(data)) {
    duplicate_ids <- duplicated(data$id)
    if (any(duplicate_ids)) {
      data <- data[!duplicate_ids, ]
      removed_dups <- sum(duplicate_ids)
      recovery_log <- c(recovery_log, paste("Removed", removed_dups, "duplicate ID rows"))
    }
  }
  
  # Convert character columns that should be numeric
  numeric_candidates <- c("publication_year", "confidence", "id")
  for (col in intersect(numeric_candidates, names(data))) {
    if (is.character(data[[col]])) {
      numeric_version <- suppressWarnings(as.numeric(data[[col]]))
      if (!all(is.na(numeric_version))) {
        data[[col]] <- numeric_version
        recovery_log <- c(recovery_log, paste("Converted", col, "to numeric"))
      }
    }
  }
  
  # Log recovery actions
  if (length(recovery_log) > 0) {
    cat("Data recovery in", context, ":\n")
    for (action in recovery_log) {
      cat("  -", action, "\n")
    }
    cat("  Rows:", original_rows, "→", nrow(data), "\n")
  }
  
  return(data)
}

#' Create backup of important files
#' @param file_path File to backup
#' @param backup_dir Directory for backups
create_backup <- function(file_path, backup_dir = "results/backups") {
  
  if (!file.exists(file_path)) {
    return(FALSE)
  }
  
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  
  # Create timestamped backup name
  file_name <- basename(file_path)
  file_ext <- tools::file_ext(file_name)
  file_base <- tools::file_path_sans_ext(file_name)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  backup_name <- paste0(file_base, "_backup_", timestamp, ".", file_ext)
  backup_path <- file.path(backup_dir, backup_name)
  
  success <- file.copy(file_path, backup_path)
  
  if (success) {
    cat("✅ Backup created:", backup_path, "\n")
  } else {
    cat("❌ Backup failed for:", file_path, "\n")
  }
  
  return(success)
}

# =============================================================================
# PROGRESS TRACKING
# =============================================================================

#' Simple progress tracker for long operations
#' @param current Current iteration
#' @param total Total iterations
#' @param context Description of operation
#' @param update_frequency How often to print updates
track_progress <- function(current, total, context = "Processing", 
                          update_frequency = 100) {
  
  if (current %% update_frequency == 0 || current == total) {
    percent <- round(100 * current / total, 1)
    cat("\r", context, ":", current, "/", total, "(", percent, "%)", sep = "")
    
    if (current == total) {
      cat(" ✅ Complete!\n")
    }
  }
}

#' Estimate remaining time for operations
#' @param start_time Start time of operation
#' @param current Current iteration
#' @param total Total iterations
estimate_remaining_time <- function(start_time, current, total) {
  
  if (current == 0) return("Unknown")
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  rate <- current / elapsed
  remaining_items <- total - current
  remaining_seconds <- remaining_items / rate
  
  if (remaining_seconds < 60) {
    return(paste(round(remaining_seconds), "seconds"))
  } else if (remaining_seconds < 3600) {
    return(paste(round(remaining_seconds / 60, 1), "minutes"))
  } else {
    return(paste(round(remaining_seconds / 3600, 1), "hours"))
  }
}

# =============================================================================
# FILE READING UTILITIES
# =============================================================================

#' Safe file reading with fallback options and error handling
#' @param primary_file Primary file to try
#' @param backup_files Vector of backup file options
#' @param script_name Name of calling script
safe_read_csv <- function(primary_file, backup_files = NULL, script_name = "Unknown") {

  # Try primary file first
  if (file.exists(primary_file)) {
    cat("Reading primary file:", primary_file, "\n")
    tryCatch({
      data <- readr::read_csv(primary_file, show_col_types = FALSE)
      cat("✓ Successfully read", nrow(data), "rows from primary file\n")
      return(list(success = TRUE, result = data, error = NULL))
    }, error = function(e) {
      cat("❌ Error reading primary file:", e$message, "\n")
      # Don't return here, try backup files
    })
  } else {
    cat("❌ Primary file not found:", primary_file, "\n")
  }

  # Try backup files
  if (!is.null(backup_files)) {
    for (backup_file in backup_files) {
      if (file.exists(backup_file)) {
        cat("Trying backup file:", backup_file, "\n")
        tryCatch({
          data <- readr::read_csv(backup_file, show_col_types = FALSE)
          cat("✓ Successfully read", nrow(data), "rows from backup file\n")
          return(list(success = TRUE, result = data, error = NULL))
        }, error = function(e) {
          cat("❌ Error reading backup file:", e$message, "\n")
          # Continue to next backup file
        })
      } else {
        cat("❌ Backup file not found:", backup_file, "\n")
      }
    }
  }

  # If nothing works, return error
  error_msg <- paste("Could not read data file for", script_name,
                    ". Tried files:", paste(c(primary_file, backup_files), collapse = ", "))
  cat("❌ ", error_msg, "\n")
  return(list(success = FALSE, result = NULL, error = error_msg))
}

# =============================================================================
# INITIALIZATION
# =============================================================================

# Create logs directory
if (!dir.exists("results/logs")) {
  dir.create("results/logs", recursive = TRUE)
}

cat("Error handling utilities loaded successfully!\n")
