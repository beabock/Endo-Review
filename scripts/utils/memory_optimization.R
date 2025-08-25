# Memory Optimization Utilities
# B. Bock
# Memory-efficient processing utilities for large datasets

# =============================================================================
# MEMORY MANAGEMENT UTILITIES
# =============================================================================

#' Monitor memory usage and provide warnings
#' @param threshold_gb Memory threshold in GB to trigger warnings
#' @param context Description of current operation
monitor_memory <- function(threshold_gb = 4, context = "Memory check") {
  
  # Get memory usage (works on most systems)
  mem_info <- tryCatch({
    if (Sys.info()["sysname"] == "Windows") {
      # Windows memory check
      system("wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /format:csv", intern = TRUE)
    } else {
      # Unix-like systems
      system("free -m", intern = TRUE)
    }
  }, error = function(e) NULL)
  
  # R's internal memory usage
  mem_used_mb <- sum(gc()[, 2])  # Sum of Ncells and Vcells in MB
  mem_used_gb <- mem_used_mb / 1024
  
  if (mem_used_gb > threshold_gb) {
    cat("⚠️ High memory usage in", context, ":", round(mem_used_gb, 2), "GB\n")
    cat("   Consider running gc() or processing in smaller chunks\n")
  }
  
  return(list(
    memory_used_gb = mem_used_gb,
    memory_used_mb = mem_used_mb,
    above_threshold = mem_used_gb > threshold_gb
  ))
}

#' Aggressive garbage collection with reporting
#' @param verbose Whether to report memory freed
aggressive_gc <- function(verbose = TRUE) {
  
  if (verbose) {
    mem_before <- sum(gc(verbose = FALSE)[, 2])
  }
  
  # Multiple rounds of garbage collection
  for (i in 1:3) {
    gc(verbose = FALSE)
  }
  
  if (verbose) {
    mem_after <- sum(gc(verbose = FALSE)[, 2])
    mem_freed <- mem_before - mem_after
    
    if (mem_freed > 0) {
      cat("🧹 Freed", round(mem_freed, 1), "MB of memory\n")
    }
  }
  
  invisible(gc(verbose = FALSE))
}

#' Create sparse matrix from dense matrix to save memory
#' @param matrix Dense matrix to convert
#' @param threshold Minimum value to keep (values below become 0)
create_sparse_matrix <- function(matrix, threshold = 0) {
  
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Matrix package required for sparse matrix operations")
  }
  
  # Set small values to zero
  if (threshold > 0) {
    matrix[abs(matrix) < threshold] <- 0
  }
  
  # Convert to sparse matrix
  sparse_matrix <- Matrix::Matrix(matrix, sparse = TRUE)
  
  # Report memory savings
  dense_size <- object.size(matrix)
  sparse_size <- object.size(sparse_matrix)
  savings_pct <- round(100 * (1 - as.numeric(sparse_size) / as.numeric(dense_size)), 1)
  
  cat("📊 Sparse matrix created - Memory savings:", savings_pct, "%\n")
  
  return(sparse_matrix)
}

#' Memory-efficient DTM creation with sparse matrices
#' @param data Data frame with text data
#' @param text_column Name of text column
#' @param id_column Name of ID column
#' @param min_term_freq Minimum term frequency to include
#' @param max_features Maximum number of features to keep
create_sparse_dtm <- function(data, text_column = "abstract", id_column = "id",
                              min_term_freq = 2, max_features = 10000) {
  
  if (!requireNamespace("tidytext", quietly = TRUE) || 
      !requireNamespace("Matrix", quietly = TRUE)) {
    stop("tidytext and Matrix packages required")
  }
  
  cat("Creating sparse DTM from", nrow(data), "documents\n")
  
  # Tokenize and count terms
  cat("  Tokenizing text...\n")
  tokens <- data %>%
    tidytext::unnest_tokens(word, !!sym(text_column), token = "words") %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    mutate(word = str_to_lower(word)) %>%
    filter(!str_detect(word, "\\d")) %>%
    count(!!sym(id_column), word, sort = TRUE)
  
  # Filter by term frequency
  cat("  Filtering terms (min frequency:", min_term_freq, ")...\n")
  term_counts <- tokens %>%
    group_by(word) %>%
    summarise(total_freq = sum(n), .groups = "drop") %>%
    filter(total_freq >= min_term_freq) %>%
    arrange(desc(total_freq))
  
  # Limit features if specified
  if (!is.null(max_features) && nrow(term_counts) > max_features) {
    cat("  Limiting to top", max_features, "features\n")
    keep_terms <- term_counts$word[1:max_features]
    tokens <- tokens %>% filter(word %in% keep_terms)
  }
  
  # Create sparse DTM
  cat("  Creating sparse matrix...\n")
  dtm <- tokens %>%
    tidytext::cast_sparse(!!sym(id_column), word, n)
  
  cat("  DTM created:", nrow(dtm), "documents ×", ncol(dtm), "terms\n")
  cat("  Sparsity:", round(100 * (1 - length(dtm@x) / (nrow(dtm) * ncol(dtm))), 1), "%\n")
  
  return(dtm)
}

# =============================================================================
# CHUNKED PROCESSING UTILITIES
# =============================================================================

#' Process large datasets in memory-efficient chunks
#' @param data Large dataset to process
#' @param process_function Function to apply to each chunk
#' @param chunk_size Number of rows per chunk
#' @param output_file Optional file to save results incrementally
#' @param combine_results Whether to combine all results in memory
process_large_dataset <- function(data, process_function, chunk_size = 1000,
                                 output_file = NULL, combine_results = TRUE) {
  
  total_rows <- nrow(data)
  n_chunks <- ceiling(total_rows / chunk_size)
  
  cat("Processing", total_rows, "rows in", n_chunks, "chunks of", chunk_size, "\n")
  
  results <- list()
  start_time <- Sys.time()
  
  for (i in 1:n_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, total_rows)
    
    # Extract chunk
    chunk_data <- data[start_idx:end_idx, ]
    
    # Progress reporting
    cat("  Chunk", i, "/", n_chunks, "(rows", start_idx, "-", end_idx, ")")
    
    # Estimate remaining time
    if (i > 1) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      rate <- (i - 1) / elapsed
      remaining_chunks <- n_chunks - i + 1
      eta_mins <- remaining_chunks / rate
      cat(" - ETA:", round(eta_mins, 1), "min")
    }
    cat("\n")
    
    # Process chunk
    chunk_result <- tryCatch({
      process_function(chunk_data)
    }, error = function(e) {
      cat("    ⚠️ Error in chunk", i, ":", e$message, "\n")
      return(NULL)
    })
    
    # Handle results
    if (!is.null(chunk_result)) {
      if (combine_results) {
        results[[i]] <- chunk_result
      }
      
      # Save incrementally if requested
      if (!is.null(output_file)) {
        chunk_file <- paste0(tools::file_path_sans_ext(output_file), 
                            "_chunk_", i, ".", tools::file_ext(output_file))
        
        if (is.data.frame(chunk_result)) {
          readr::write_csv(chunk_result, chunk_file)
        } else {
          saveRDS(chunk_result, chunk_file)
        }
      }
    }
    
    # Memory management
    if (i %% 5 == 0) {
      aggressive_gc(verbose = FALSE)
    }
    
    # Memory monitoring
    if (i %% 10 == 0) {
      mem_status <- monitor_memory(context = paste("Chunk", i))
      if (mem_status$above_threshold) {
        cat("    Consider reducing chunk_size or enabling incremental output\n")
      }
    }
  }
  
  # Combine results if requested
  if (combine_results && length(results) > 0) {
    cat("Combining", length(results), "chunk results...\n")
    
    # Check if results are data frames
    if (all(sapply(results, is.data.frame))) {
      final_result <- do.call(rbind, results)
    } else {
      final_result <- results
    }
    
    # Save final combined result
    if (!is.null(output_file)) {
      if (is.data.frame(final_result)) {
        readr::write_csv(final_result, output_file)
      } else {
        saveRDS(final_result, output_file)
      }
      cat("Final results saved to:", output_file, "\n")
    }
    
    return(final_result)
  }
  
  cat("Processing complete! Results saved incrementally.\n")
  return(invisible(NULL))
}

#' Memory-efficient text processing for large corpora
#' @param texts Vector of texts to process
#' @param process_function Function to apply to each text batch
#' @param batch_size Number of texts per batch
process_texts_efficiently <- function(texts, process_function, batch_size = 100) {
  
  n_texts <- length(texts)
  n_batches <- ceiling(n_texts / batch_size)
  
  cat("Processing", n_texts, "texts in", n_batches, "batches\n")
  
  results <- list()
  
  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_texts)
    
    batch_texts <- texts[start_idx:end_idx]
    
    cat("  Batch", i, "/", n_batches, "(", length(batch_texts), "texts)\n")
    
    batch_result <- tryCatch({
      process_function(batch_texts)
    }, error = function(e) {
      cat("    ⚠️ Error in batch", i, ":", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(batch_result)) {
      results[[i]] <- batch_result
    }
    
    # Memory cleanup every few batches
    if (i %% 10 == 0) {
      aggressive_gc(verbose = FALSE)
    }
  }
  
  # Combine results
  if (length(results) > 0) {
    if (all(sapply(results, is.data.frame))) {
      return(do.call(rbind, results))
    } else if (all(sapply(results, is.list))) {
      return(do.call(c, results))
    } else {
      return(results)
    }
  }
  
  return(NULL)
}

# =============================================================================
# TEMPORARY FILE MANAGEMENT
# =============================================================================

#' Create and manage temporary files for large operations
#' @param prefix Prefix for temporary file names
#' @param cleanup Whether to register cleanup on exit
create_temp_workspace <- function(prefix = "endophyte_temp", cleanup = TRUE) {
  
  temp_dir <- file.path(tempdir(), prefix)
  
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  if (cleanup) {
    # Register cleanup function
    reg.finalizer(environment(), function(e) {
      if (dir.exists(temp_dir)) {
        unlink(temp_dir, recursive = TRUE)
        cat("🧹 Cleaned up temporary workspace:", temp_dir, "\n")
      }
    }, onexit = TRUE)
  }
  
  cat("📁 Temporary workspace created:", temp_dir, "\n")
  return(temp_dir)
}

#' Save intermediate results to temporary files
#' @param data Data to save
#' @param name Name for the temporary file
#' @param temp_dir Temporary directory
save_temp_result <- function(data, name, temp_dir) {
  
  temp_file <- file.path(temp_dir, paste0(name, ".rds"))
  saveRDS(data, temp_file)
  
  file_size_mb <- file.size(temp_file) / (1024^2)
  cat("💾 Saved", name, "to temp file (", round(file_size_mb, 1), "MB)\n")
  
  return(temp_file)
}

#' Load intermediate results from temporary files
#' @param name Name of the temporary file
#' @param temp_dir Temporary directory
load_temp_result <- function(name, temp_dir) {
  
  temp_file <- file.path(temp_dir, paste0(name, ".rds"))
  
  if (!file.exists(temp_file)) {
    stop("Temporary file not found: ", temp_file)
  }
  
  data <- readRDS(temp_file)
  cat("📂 Loaded", name, "from temp file\n")
  
  return(data)
}

# =============================================================================
# MEMORY-EFFICIENT DATA STRUCTURES
# =============================================================================

#' Convert data frame to more memory-efficient formats
#' @param data Data frame to optimize
#' @param string_as_factor Convert strings to factors
#' @param compress_integers Use smaller integer types where possible
optimize_dataframe <- function(data, string_as_factor = TRUE, compress_integers = TRUE) {
  
  original_size <- object.size(data)
  
  # Convert strings to factors if they have few unique values
  if (string_as_factor) {
    for (col in names(data)) {
      if (is.character(data[[col]])) {
        unique_vals <- length(unique(data[[col]]))
        total_vals <- length(data[[col]])
        
        # Convert to factor if less than 50% unique values
        if (unique_vals / total_vals < 0.5) {
          data[[col]] <- as.factor(data[[col]])
          cat("  Converted", col, "to factor (", unique_vals, "levels)\n")
        }
      }
    }
  }
  
  # Compress integers where possible
  if (compress_integers) {
    for (col in names(data)) {
      if (is.numeric(data[[col]]) && all(data[[col]] == floor(data[[col]]), na.rm = TRUE)) {
        max_val <- max(abs(data[[col]]), na.rm = TRUE)
        
        if (max_val <= 127) {
          # Can use 8-bit integer
          data[[col]] <- as.integer(data[[col]])
          cat("  Compressed", col, "to integer\n")
        }
      }
    }
  }
  
  optimized_size <- object.size(data)
  savings_pct <- round(100 * (1 - as.numeric(optimized_size) / as.numeric(original_size)), 1)
  
  if (savings_pct > 0) {
    cat("📊 Data frame optimized - Memory savings:", savings_pct, "%\n")
  }
  
  return(data)
}

# =============================================================================
# INITIALIZATION
# =============================================================================

cat("Memory optimization utilities loaded successfully!\n")
cat("Available functions:\n")
cat("  • monitor_memory() - Check memory usage\n")
cat("  • aggressive_gc() - Free memory\n")
cat("  • create_sparse_dtm() - Memory-efficient text matrices\n")
cat("  • process_large_dataset() - Chunked processing\n")
cat("  • optimize_dataframe() - Compress data structures\n")
