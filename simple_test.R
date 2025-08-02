# Simple test for veronicastrum lungtsuanense
library(tidyverse)

# Load the species data
species_df <- readRDS("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review/models/species.rds")

# Source the fixed functions
source("scripts/archive/optimized_taxa_detection.R")

# Create lookup tables
lookup_tables <- create_lookup_tables(species_df)

# Test just veronicastrum lungtsuanense
test_names <- c("veronicastrum lungtsuanense")
result <- batch_validate_names(test_names, lookup_tables)

cat("Results for veronicastrum lungtsuanense:\n")
cat("Number of rows:", nrow(result), "\n")
if(nrow(result) > 0 && nrow(result) <= 10) {
  print(result)
} else if(nrow(result) > 10) {
  cat("Too many results to display, showing first 5:\n")
  print(head(result, 5))
  cat("...\nShowing last 5:\n")
  print(tail(result, 5))
}

# Also check the synonym resolution table directly
cat("\nChecking synonym_resolution table for this species:\n")
synonym_check <- lookup_tables$synonym_resolution %>%
  filter(canonicalName_lower == "veronicastrum lungtsuanense")
cat("Rows in synonym_resolution:", nrow(synonym_check), "\n")
if(nrow(synonym_check) > 0) {
  print(synonym_check)
}
