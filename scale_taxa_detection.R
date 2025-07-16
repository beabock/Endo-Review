# Scale Taxa Detection for Large Datasets
# This script demonstrates how to efficiently process 10,000+ abstracts
# with proper synonym handling and performance optimizations

library(tidyverse)
library(rgbif)
library(furrr)
library(tictoc)  # For timing
library(vroom)   # For fast file reading
source("optimized_taxa_detection.R")

# Function to run a benchmark test with different batch sizes
benchmark_batch_sizes <- function(abstracts, lookup_tables, plant_parts_keywords, 
                                 sample_size = 100) {
  # Take a sample of abstracts for benchmarking
  if (nrow(abstracts) > sample_size) {
    test_abstracts <- abstracts %>% 
      sample_n(sample_size) %>%
      arrange(id)
  } else {
    test_abstracts <- abstracts
  }
  
  # Test different batch sizes
  batch_sizes <- c(10, 25, 50, 100)
  results <- list()
  
  cat("\n=== Benchmarking Batch Sizes ===\n")
  cat("Testing with", nrow(test_abstracts), "abstracts\n")
  
  for (batch_size in batch_sizes) {
    cat("\nTesting batch size:", batch_size, "\n")
    
    # Time the processing
    tic(paste("Batch size", batch_size))
    
    # Process abstracts
    batch_results <- process_abstracts_parallel(
      test_abstracts, 
      lookup_tables, 
      plant_parts_keywords,
      batch_size = batch_size
    )
    
    # Record time
    timing <- toc(quiet = TRUE)
    elapsed <- timing$toc - timing$tic
    
    cat("Processed", nrow(test_abstracts), "abstracts in", round(elapsed, 2), "seconds\n")
    cat("Average time per abstract:", round(elapsed / nrow(test_abstracts), 4), "seconds\n")
    
    # Store results
    results[[as.character(batch_size)]] <- list(
      batch_size = batch_size,
      elapsed_time = elapsed,
      avg_time_per_abstract = elapsed / nrow(test_abstracts),
      detected_taxa = nrow(batch_results),
      abstracts_with_species = length(unique(batch_results$id[batch_results$match_type == "species"]))
    )
  }
  
  # Summarize results
  summary <- bind_rows(results)
  
  cat("\n=== Benchmark Summary ===\n")
  print(summary)
  
  # Recommend optimal batch size
  optimal_batch <- summary %>%
    arrange(avg_time_per_abstract) %>%
    slice(1)
  
  cat("\nRecommended batch size for your system:", optimal_batch$batch_size, "\n")
  cat("Estimated time for 10,000 abstracts:", 
      round(optimal_batch$avg_time_per_abstract * 10000 / 60, 1), "minutes\n")
  
  return(summary)
}

# Function to process a large dataset with progress tracking
process_large_dataset <- function(abstracts_file, output_file, batch_size = 100, 
                                 sample_size = NULL, workers = NULL) {
  # Load abstracts
  cat("Loading abstracts from", abstracts_file, "\n")
  abstracts <- vroom(abstracts_file, show_col_types = FALSE)
  
  # Take a sample if requested
  if (!is.null(sample_size) && sample_size > 0 && sample_size < nrow(abstracts)) {
    cat("Using a sample of", sample_size, "abstracts\n")
    abstracts <- abstracts %>% 
      sample_n(sample_size) %>%
      arrange(id)
  }
  
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create optimized lookup tables
  cat("Creating optimized lookup tables...\n")
  lookup_tables <- create_lookup_tables(species)
  
  # Define plant parts keywords
  plant_parts_keywords <- c(
    # Basic structures
    "fruit", "fruits", "root", "roots", "leaf", "leaves", "stem", "stems", 
    "flower", "flowers", "seed", "seeds", "bark", "branch", "branches",
    "twig", "twigs", "shoot", "shoots", "bud", "buds", "trunk", "trunks",
    
    # Reproductive structures
    "pistil", "pistils", "anther", "anthers", "carpel", "carpels", 
    "sepal", "sepals", "petal", "petals", "stigma", "stigmas", 
    "style", "styles", "ovary", "ovaries", "ovule", "ovules",
    "calyx", "calyces", "corolla", "corollas", "pollen",
    "inflorescence", "inflorescences", "floret", "florets",
    
    # Specialized structures
    "rhizome", "rhizomes", "tuber", "tubers", "bulb", "bulbs", 
    "corm", "corms", "tendril", "tendrils", "thorn", "thorns",
    "cone", "cones", "needle", "needles"
  )
  
  # Process abstracts in parallel
  cat("Processing", nrow(abstracts), "abstracts with batch size", batch_size, "\n")
  tic("Processing abstracts")
  
  results <- process_abstracts_parallel(
    abstracts, 
    lookup_tables, 
    plant_parts_keywords,
    batch_size = batch_size,
    workers = workers
  )
  
  timing <- toc(quiet = TRUE)
  elapsed <- timing$toc - timing$tic
  
  cat("Processed", nrow(abstracts), "abstracts in", round(elapsed / 60, 2), "minutes\n")
  cat("Average time per abstract:", round(elapsed / nrow(abstracts), 4), "seconds\n")
  
  # Save results
  cat("Saving results to", output_file, "\n")
  write_csv(results, output_file)
  
  # Generate summary
  summary <- results %>%
    group_by(match_type) %>%
    summarise(
      count = n(),
      unique_taxa = n_distinct(resolved_name),
      abstracts = n_distinct(id)
    )
  
  cat("\n=== Results Summary ===\n")
  print(summary)
  
  # Check synonym handling
  synonym_count <- sum(results$status == "SYNONYM", na.rm = TRUE)
  cat("\nSynonyms resolved:", synonym_count, "\n")
  
  if (synonym_count > 0) {
    synonym_examples <- results %>%
      filter(status == "SYNONYM") %>%
      select(id, match_type, resolved_name, status) %>%
      head(5)
    
    cat("Example synonyms resolved:\n")
    print(synonym_examples)
  }
  
  return(results)
}

# Function to compare original vs optimized results
compare_results <- function(original_file, optimized_file) {
  # Load results
  cat("Loading original results from", original_file, "\n")
  original <- read_csv(original_file, show_col_types = FALSE)
  
  cat("Loading optimized results from", optimized_file, "\n")
  optimized <- read_csv(optimized_file, show_col_types = FALSE)
  
  # Compare species detection
  original_species <- original %>%
    filter(match_type == "species") %>%
    group_by(id) %>%
    summarise(
      species_count_original = n(),
      species_names_original = paste(resolved_name, collapse = ", "),
      .groups = "drop"
    )
  
  optimized_species <- optimized %>%
    filter(match_type == "species") %>%
    group_by(id) %>%
    summarise(
      species_count_optimized = n(),
      species_names_optimized = paste(resolved_name, collapse = ", "),
      .groups = "drop"
    )
  
  # Join results
  comparison <- full_join(
    original_species, 
    optimized_species,
    by = "id"
  ) %>%
    mutate(
      species_count_original = replace_na(species_count_original, 0),
      species_count_optimized = replace_na(species_count_optimized, 0),
      species_names_original = replace_na(species_names_original, ""),
      species_names_optimized = replace_na(species_names_optimized, ""),
      difference = species_count_optimized - species_count_original
    )
  
  # Summary
  cat("\n=== Comparison Summary ===\n")
  cat("Total abstracts:", nrow(comparison), "\n")
  cat("Abstracts with more species detected in optimized version:", 
      sum(comparison$difference > 0), "\n")
  cat("Abstracts with same number of species detected:", 
      sum(comparison$difference == 0), "\n")
  cat("Abstracts with fewer species detected in optimized version:", 
      sum(comparison$difference < 0), "\n")
  
  # Save comparison
  write_csv(comparison, "detection_comparison.csv")
  
  return(comparison)
}

# Main function to run the entire workflow
run_workflow <- function(abstracts_file, output_file, run_benchmark = TRUE, 
                        sample_size = NULL, batch_size = 100, workers = NULL) {
  # Load abstracts
  cat("Loading abstracts from", abstracts_file, "\n")
  abstracts <- vroom(abstracts_file, show_col_types = FALSE)
  
  # Run benchmark if requested
  if (run_benchmark) {
    # Load species data
    if (!file.exists("species.rds")) {
      stop("species.rds file not found. Please run the main script first.")
    }
    species <- readRDS("species.rds")
    
    # Create optimized lookup tables
    cat("Creating optimized lookup tables for benchmark...\n")
    lookup_tables <- create_lookup_tables(species)
    
    # Define plant parts keywords
    plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
    
    # Run benchmark
    benchmark_results <- benchmark_batch_sizes(
      abstracts, 
      lookup_tables, 
      plant_parts_keywords,
      sample_size = min(100, nrow(abstracts))
    )
    
    # Use recommended batch size
    batch_size <- benchmark_results %>%
      arrange(avg_time_per_abstract) %>%
      slice(1) %>%
      pull(batch_size)
    
    cat("Using recommended batch size:", batch_size, "\n")
  }
  
  # Process the dataset
  results <- process_large_dataset(
    abstracts_file, 
    output_file, 
    batch_size = batch_size,
    sample_size = sample_size,
    workers = workers
  )
  
  return(results)
}

# Example usage
if (interactive()) {
  cat("To run the workflow with your data:\n")
  cat('run_workflow("full_predictions_with_metadata.csv", "taxa_info_results_optimized.csv")\n\n')
  
  cat("To run with a small sample first:\n")
  cat('run_workflow("full_predictions_with_metadata.csv", "taxa_info_results_sample.csv", sample_size = 100)\n\n')
  
  cat("To compare with original results:\n")
  cat('compare_results("taxa_info_results_label_loose.csv", "taxa_info_results_optimized.csv")\n')
}
