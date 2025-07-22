# Test Taxa Detection
# This script tests the key functionality of the taxa_detection.R file

library(tidyverse)
source("taxa_detection.R")

# Test function for abbreviated genus names and synonym handling
test_taxa_detection <- function() {
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  # Test cases
  test_cases <- list(
    # Test standard species names
    "Standard" = "This study examines the endophytic fungi associated with Acer macrophyllum (bigleaf maple) in the Pacific Northwest.",
    
    # Test abbreviated genus names
    "Abbreviated" = "After initial identification, A. macrophyllum was found to host over 20 fungal species.",
    
    # Test multiple species
    "Multiple species" = "We compared Acer macrophyllum and Pseudotsuga menziesii in our study area.",
    
    # Test with punctuation
    "With punctuation" = "Acer macrophyllum, commonly known as bigleaf maple, hosts diverse fungal communities."
  )
  
  # Find some synonyms for testing
  if (nrow(lookup_tables$synonym_resolution) > 0) {
    # Add synonym test cases
    synonyms <- lookup_tables$synonym_resolution %>% head(2)
    
    for (i in 1:nrow(synonyms)) {
      if (is.na(synonyms$synonymName[i]) || is.na(synonyms$acceptedName[i])) next
      
      test_cases[[paste0("Synonym ", i)]] <- paste0(
        "This study examines the endophytic fungi associated with ", 
        synonyms$synonymName[i], 
        " in the Pacific Northwest."
      )
    }
  }
  
  # Define plant parts for testing
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  # Run tests
  results <- list()
  
  cat("\n=== Testing Taxa Detection ===\n")
  
  for (name in names(test_cases)) {
    cat("\n---", name, "---\n")
    text <- test_cases[[name]]
    cat("Text:", text, "\n")
    
    # Extract candidate names
    candidates <- extract_candidate_names(text)
    cat("Extracted candidates:", paste(candidates, collapse = ", "), "\n")
    
    # Validate names
    valid_species <- batch_validate_names(candidates, lookup_tables)
    cat("Valid species found:", nrow(valid_species), "\n")
    if (nrow(valid_species) > 0) {
      print(valid_species %>% select(user_supplied_name, resolved_name, status))
    }
    
    # Process full abstract
    plant_info <- extract_plant_info(
      text = text,
      abstract_id = which(names(test_cases) == name),
      predicted_label = "Presence",
      lookup_tables = lookup_tables,
      plant_parts_keywords = plant_parts_keywords
    )
    
    cat("\nFinal detected taxa:\n")
    print(plant_info %>% select(match_type, resolved_name, status))
    
    results[[name]] <- plant_info
  }
  
  # Summary
  cat("\n=== Summary ===\n")
  success_count <- sum(sapply(results, function(r) any(r$match_type == "species")))
  cat("Successfully detected species in", success_count, "out of", length(test_cases), "test cases\n")
  
  # Check specifically for abbreviated genus names
  abbrev_count <- sum(sapply(names(test_cases), function(n) {
    grepl("Abbreviated", n) && any(results[[n]]$match_type == "species")
  }))
  if (abbrev_count > 0) {
    cat("Successfully detected abbreviated genus names!\n")
  } else {
    cat("Failed to detect abbreviated genus names.\n")
  }
  
  # Check specifically for synonym resolution
  synonym_count <- sum(sapply(results, function(r) any(r$status == "SYNONYM")))
  if (synonym_count > 0) {
    cat("Successfully resolved", synonym_count, "synonyms!\n")
  } else {
    cat("No synonyms were resolved. This could be because no synonyms were in the test cases or because synonym resolution isn't working.\n")
  }
  
  return(results)
}

# Test function for performance benchmarking
test_performance <- function(sample_size = 10) {
  # Create a sample dataset
  abstracts <- tibble(
    id = 1:sample_size,
    abstract = rep(
      "This study examines the endophytic fungi associated with Acer macrophyllum (bigleaf maple) in the Pacific Northwest. After initial identification, A. macrophyllum was found to host over 20 fungal species.",
      sample_size
    ),
    predicted_label = rep("Presence", sample_size)
  )
  
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  # Define plant parts keywords
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  # Test different batch sizes
  batch_sizes <- c(5, 10)
  
  cat("\n=== Testing Performance ===\n")
  cat("Testing with", sample_size, "abstracts\n")
  
  for (batch_size in batch_sizes) {
    cat("\nTesting batch size:", batch_size, "\n")
    
    # Time the processing
    start_time <- Sys.time()
    
    # Process abstracts
    results <- process_abstracts_parallel(
      abstracts, 
      lookup_tables, 
      plant_parts_keywords,
      batch_size = batch_size
    )
    
    # Record time
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    cat("Processed", sample_size, "abstracts in", round(elapsed, 2), "seconds\n")
    cat("Average time per abstract:", round(elapsed / sample_size, 4), "seconds\n")
    cat("Detected", nrow(results), "taxa mentions\n")
  }
  
  cat("\nEstimated time for 10,000 abstracts:", 
      round((elapsed / sample_size) * 10000 / 60, 1), "minutes\n")
}

# Test function for hardware optimization
test_hardware_optimization <- function() {
  cat("\n=== Testing Hardware Optimization ===\n")
  
  # Get optimal settings
  optimal_settings <- get_optimal_settings()
  
  cat("Detected optimal settings for your hardware:\n")
  cat("  - Workers:", optimal_settings$workers, "\n")
  cat("  - Batch size:", optimal_settings$batch_size, "\n")
  cat("  - Memory limit:", optimal_settings$memory_limit, "MB\n")
  cat("  - Low memory mode:", optimal_settings$low_memory_mode, "\n")
  
  # Get CPU info
  cpu_info <- tryCatch({
    if (Sys.info()["sysname"] == "Windows") {
      system("wmic cpu get name", intern = TRUE)[2]
    } else {
      system("cat /proc/cpuinfo | grep 'model name' | head -1", intern = TRUE)
    }
  }, error = function(e) {
    "CPU information not available"
  })
  
  cat("\nDetected CPU:", cpu_info, "\n")
  cat("Available cores:", availableCores(), "\n")
  
  # Get memory info
  mem_info <- get_memory_usage()
  cat("Total RAM:", round(mem_info$total, 1), "MB\n")
  cat("Current R process memory usage:", round(mem_info$r_used, 1), "MB\n")
  
  # Estimate processing time
  cat("\nEstimated processing time for 10,000 abstracts with these settings:\n")
  
  # Assume 0.05 seconds per abstract as a baseline
  base_time_per_abstract <- 0.05
  
  # Adjust for parallelization
  parallel_factor <- 1 / optimal_settings$workers
  
  # Adjust for batch size (larger batches are more efficient)
  batch_factor <- 1 - (log10(optimal_settings$batch_size) / 10)
  
  # Calculate estimated time
  estimated_time_per_abstract <- base_time_per_abstract * parallel_factor * batch_factor
  estimated_total_time <- estimated_time_per_abstract * 10000
  
  cat("  - Estimated time per abstract:", round(estimated_time_per_abstract, 4), "seconds\n")
  cat("  - Estimated total time:", round(estimated_total_time / 60, 1), "minutes\n")
  
  return(optimal_settings)
}

# Run the tests
if (interactive()) {
  cat("Running tests for taxa detection...\n")
  test_results <- test_taxa_detection()
  
  cat("\nRunning performance tests...\n")
  test_performance(sample_size = 10)
  
  cat("\nTesting hardware optimization...\n")
  optimal_settings <- test_hardware_optimization()
  
  cat("\nTo run the full workflow on your data with optimal settings:\n")
  cat('results <- run_taxa_detection(\n')
  cat('  input_file = "full_predictions_with_metadata.csv",\n')
  cat('  output_file = "taxa_info_results.csv",\n')
  cat('  batch_size = ', optimal_settings$batch_size, ',\n', sep = '')
  cat('  workers = ', optimal_settings$workers, ',\n', sep = '')
  cat('  memory_limit = ', optimal_settings$memory_limit, '\n)', sep = '')
}
