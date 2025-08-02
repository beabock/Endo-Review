# Test script to debug the species lookup issue
library(tidyverse)

# Load the species data
species_df <- readRDS("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review/models/species.rds")

# Check the structure of the species data
cat("Species data structure:\n")
cat("Columns:", paste(colnames(species_df), collapse = ", "), "\n")
cat("Number of rows:", nrow(species_df), "\n")
cat("Sample of taxonomicStatus values:", paste(unique(species_df$taxonomicStatus)[1:5], collapse = ", "), "\n")

# Source the optimized taxa detection functions
source("scripts/archive/optimized_taxa_detection.R")

# Try to create lookup tables
cat("\nCreating lookup tables...\n")
lookup_tables <- create_lookup_tables(species_df)

cat("Lookup tables created successfully!\n")
cat("Components:", paste(names(lookup_tables), collapse = ", "), "\n")

# Test with veronicastrum lungtsuanense
test_names <- c("veronicastrum lungtsuanense")
cat("\nTesting with 'veronicastrum lungtsuanense'...\n")

result <- batch_validate_names(test_names, lookup_tables)
cat("Number of results:", nrow(result), "\n")

if(nrow(result) > 0) {
  print(result)
} else {
  cat("No matches found\n")
}

# Check synonym resolution table for this species
cat("\nChecking synonym_resolution table...\n")
synonym_matches <- lookup_tables$synonym_resolution %>%
  filter(canonicalName_lower == "veronicastrum lungtsuanense")
cat("Entries in synonym_resolution for this species:", nrow(synonym_matches), "\n")

if(nrow(synonym_matches) > 0) {
  print(head(synonym_matches, 10))
}
