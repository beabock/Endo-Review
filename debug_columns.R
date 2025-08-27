# Debug script to check column names in your results
# This will help us understand what columns are actually available

library(tidyverse)

# Check what columns exist in your species detection results
if (file.exists("results/species_detection_weighted_ensemble.csv")) {
  cat("=== SPECIES DETECTION RESULTS COLUMNS ===\n")
  species_results <- read_csv("results/species_detection_weighted_ensemble.csv", show_col_types = FALSE)
  cat("Total columns:", ncol(species_results), "\n")
  cat("Column names:\n")
  print(names(species_results))
  
  # Look for species-related columns
  species_cols <- names(species_results)[grepl("species|name|canonical|accepted|resolved", names(species_results), ignore.case = TRUE)]
  cat("\nPotential species columns:\n")
  print(species_cols)
  
  # Check first few rows of species columns
  if (length(species_cols) > 0) {
    cat("\nFirst few rows of species columns:\n")
    print(species_results %>% select(all_of(species_cols)) %>% head(3))
  }
} else {
  cat("Species detection results file not found\n")
}

# Check comprehensive results if they exist
if (file.exists("results/comprehensive_extraction_results.csv")) {
  cat("\n=== COMPREHENSIVE RESULTS COLUMNS ===\n")
  comprehensive_results <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)
  cat("Total columns:", ncol(comprehensive_results), "\n")
  
  # Look for species-related columns
  species_cols <- names(comprehensive_results)[grepl("species|name|canonical|accepted|resolved", names(comprehensive_results), ignore.case = TRUE)]
  cat("\nPotential species columns:\n")
  print(species_cols)
  
  # Check for geographic columns
  geo_cols <- names(comprehensive_results)[grepl("country|continent|region|geographic", names(comprehensive_results), ignore.case = TRUE)]
  cat("\nGeographic columns:\n")
  print(geo_cols)
  
  # Sample data from geographic columns
  if (length(geo_cols) > 0) {
    cat("\nSample geographic data:\n")
    sample_geo <- comprehensive_results %>% 
      select(all_of(geo_cols)) %>% 
      filter(if_any(everything(), ~!is.na(.))) %>%
      head(5)
    print(sample_geo)
  }
} else {
  cat("Comprehensive results file not found\n")
}

cat("\n=== DONE ===\n")
