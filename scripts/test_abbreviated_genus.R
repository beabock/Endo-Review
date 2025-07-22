# Test script for abbreviated genus names
# This script tests if the improved code can detect "A. macrophyllum" correctly

library(tidyverse)
library(rgbif)
source("improved_taxa_detection.R")

# Test function for abbreviated genus names
test_abbreviated_genus <- function() {
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  # Test cases with abbreviated genus names
  test_cases <- list(
    "Single abbreviated" = "After initial identification, A. macrophyllum was found to host over 20 fungal species.",
    "Multiple abbreviated" = "We compared A. macrophyllum and P. menziesii in our study area.",
    "Mixed format" = "Acer macrophyllum (bigleaf maple) and A. macrophyllum were studied.",
    "With punctuation" = "A. macrophyllum, commonly known as bigleaf maple, hosts diverse fungal communities.",
    "End of sentence" = "The primary tree species in our study area was A. macrophyllum."
  )
  
  # Define plant parts for testing
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  # Run tests
  results <- list()
  
  cat("\n=== Testing Abbreviated Genus Names ===\n")
  
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
  
  # Check specifically for Acer macrophyllum
  acer_count <- sum(sapply(results, function(r) any(r$resolved_name == "Acer macrophyllum")))
  cat("Successfully detected Acer macrophyllum in", acer_count, "out of", length(test_cases), "test cases\n")
  
  return(results)
}

# Run the test
cat("Testing improved abbreviated genus detection...\n")
test_results <- test_abbreviated_genus()

# Compare with original code
cat("\n\nTo compare with the original code, you would need to modify the original functions and run a similar test.")
