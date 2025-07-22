# Run Taxa Detection
# Simple wrapper script to run the optimized taxa detection on your data

library(tidyverse)
library(tictoc)  # For timing
source("optimized_taxa_detection.R")

# Function to run the optimized taxa detection on a dataset
run_taxa_detection <- function(input_file, output_file, sample_size = NULL, batch_size = 50) {
  # Load abstracts
  cat("Loading abstracts from", input_file, "\n")
  abstracts <- read_csv(input_file, show_col_types = FALSE)
  
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
  
  # Set up parallel processing
  setup_parallel()
  
  results <- process_abstracts_parallel(
    abstracts, 
    lookup_tables, 
    plant_parts_keywords,
    batch_size = batch_size
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

# Example usage
if (interactive()) {
  # Run with a small sample first to test
  cat("To run with a small sample (10 abstracts):\n")
  cat('results <- run_taxa_detection("full_predictions_with_metadata.csv", "taxa_info_results_sample.csv", sample_size = 10)\n\n')
  
  # Run with the full dataset
  cat("To run with the full dataset:\n")
  cat('results <- run_taxa_detection("full_predictions_with_metadata.csv", "taxa_info_results_optimized.csv")\n')
}
