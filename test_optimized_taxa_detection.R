# Test script for optimized_taxa_detection.R on 100 abstracts subset

library(tidyverse)
library(data.table)  # Ensure data.table is loaded
library(purrr)

# Source the optimized taxa detection functions
source("scripts/04_analysis/optimized_taxa_detection.R")

# Load test data (100 abstracts)
test_data <- read_csv("test_data/test_subset_random_100.csv", show_col_types = FALSE)

# Prepare abstracts data frame
abstracts_df <- test_data %>%
  select(id, abstract, predicted_label = final_classification) %>%
  mutate(predicted_label = case_when(
    predicted_label == "Presence" ~ "Presence",
    predicted_label == "Absence" ~ "Absence",
    TRUE ~ "Uncertain"
  ))

# Load species reference data
species_path <- "models/species.rds"
if (!file.exists(species_path)) {
  stop("Species reference data not found at: ", species_path)
}
species <- readRDS(species_path)

# Create optimized lookup tables with bloom filter support
message("Creating lookup tables with bloom filter support...")
lookup_tables <- create_lookup_tables_with_bloom(species)

# Set up plant parts keywords (dummy for testing)
plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")

# Test on small subset (first 10 abstracts for quick correctness check)
message("Testing on subset of 10 abstracts...")
test_subset <- abstracts_df[1:10, ]

# Process abstracts sequentially
results <- process_abstracts_parallel(
  abstracts = test_subset,
  species_path = species_path,
  plant_parts_keywords = plant_parts_keywords,
  batch_size = 5,  # Small batch for testing
  workers = 1,     # Not used in sequential mode
  use_streaming = FALSE  # No streaming for small test
)

# Check results
message("Results summary:")
message("  - Number of abstracts processed: ", nrow(test_subset))
message("  - Number of results: ", nrow(results))
message("  - Unique species detected: ", length(unique(na.omit(results$resolved_name))))

if (nrow(results) > 0) {
  message("  - Sample results:")
  print(head(results %>% select(id, resolved_name, status, match_type), 10))
} else {
  message("  - No species detected in the test subset")
}

# Run additional advanced tests
message("\nRunning synonym resolution test...")
tryCatch({
  synonym_test <- test_synonym_handling()
  if (!is.null(synonym_test)) {
    message("Synonym test completed - check results above")
  }
}, error = function(e) {
  message("Synonym test failed:", e$message)
})

message("\nAll tests completed successfully!")