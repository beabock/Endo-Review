# Test Dataset for Taxa Detection
# This script creates a small test dataset with known species to validate the improved detection functions

library(tidyverse)
library(rgbif)
source("improved_taxa_detection.R")  # Load the improved functions

# Create test abstracts with known species
create_test_dataset <- function() {
  # Test abstracts with various formats of species names
  test_abstracts <- tibble(
    id = 1:5,
    abstract = c(
      # Abstract 1: Standard format with Acer macrophyllum
      "This study examines the endophytic fungi associated with Acer macrophyllum (bigleaf maple) in the Pacific Northwest. We collected leaf samples from mature trees and isolated fungal endophytes using standard culturing techniques.",
      
      # Abstract 2: With punctuation after species name
      "Acer macrophyllum, commonly known as bigleaf maple, hosts diverse fungal communities. The leaves of Acer macrophyllum. were collected from multiple sites.",
      
      # Abstract 3: With abbreviated genus
      "After initial identification, A. macrophyllum was found to host over 20 fungal species. We compared these results with previous studies on A. macrophyllum endophytes.",
      
      # Abstract 4: Multiple species
      "We compared Acer macrophyllum and Pseudotsuga menziesii in our study area. Both species showed distinct endophyte communities, with P. menziesii hosting fewer fungal species overall.",
      
      # Abstract 5: Complex case with multiple formats
      "The study focused on three tree species: Acer macrophyllum, Pseudotsuga menziesii, and Thuja plicata. A. macrophyllum showed the highest diversity of endophytes, while T. plicata had the lowest."
    ),
    expected_species = c(
      "Acer macrophyllum",
      "Acer macrophyllum",
      "Acer macrophyllum",
      "Acer macrophyllum,Pseudotsuga menziesii",
      "Acer macrophyllum,Pseudotsuga menziesii,Thuja plicata"
    ),
    predicted_label = "Presence"
  )
  
  return(test_abstracts)
}

# Run test with both original and improved functions
run_comparison_test <- function() {
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  # Create test dataset
  test_data <- create_test_dataset()
  
  # Define plant parts for testing
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  # Results table
  results <- tibble(
    abstract_id = integer(),
    expected_species = character(),
    detected_species = character(),
    detection_status = character()
  )
  
  # Process each abstract
  for (i in 1:nrow(test_data)) {
    cat("\n\n=== Processing Abstract", i, "===\n")
    abstract <- test_data$abstract[i]
    expected <- strsplit(test_data$expected_species[i], ",")[[1]]
    
    cat("Abstract:", abstract, "\n\n")
    cat("Expected species:", paste(expected, collapse = ", "), "\n")
    
    # Extract plant info using improved functions
    plant_info <- extract_plant_info(
      text = abstract,
      abstract_id = test_data$id[i],
      predicted_label = test_data$predicted_label[i],
      lookup_tables = lookup_tables,
      plant_parts_keywords = plant_parts_keywords
    )
    
    # Extract detected species
    detected <- plant_info %>%
      filter(match_type == "species") %>%
      pull(resolved_name)
    
    if (length(detected) == 0) {
      detected <- character(0)
    }
    
    cat("Detected species:", paste(detected, collapse = ", "), "\n")
    
    # Check if all expected species were detected
    all_detected <- all(expected %in% detected)
    extra_detected <- setdiff(detected, expected)
    
    if (all_detected && length(extra_detected) == 0) {
      status <- "PERFECT"
    } else if (all_detected) {
      status <- "GOOD_WITH_EXTRAS"
    } else {
      status <- "MISSING_SPECIES"
    }
    
    # Add to results
    results <- results %>%
      add_row(
        abstract_id = test_data$id[i],
        expected_species = paste(expected, collapse = ", "),
        detected_species = paste(detected, collapse = ", "),
        detection_status = status
      )
    
    # Show detailed results
    cat("\nDetection status:", status, "\n")
    if (status != "PERFECT") {
      if (!all_detected) {
        cat("Missing species:", paste(setdiff(expected, detected), collapse = ", "), "\n")
      }
      if (length(extra_detected) > 0) {
        cat("Extra species detected:", paste(extra_detected, collapse = ", "), "\n")
      }
    }
    
    # Show all taxa detected
    cat("\nAll taxa detected:\n")
    print(plant_info %>% select(match_type, resolved_name, status))
  }
  
  # Summary
  cat("\n\n=== Summary ===\n")
  cat("Perfect matches:", sum(results$detection_status == "PERFECT"), "/", nrow(test_data), "\n")
  cat("Good with extras:", sum(results$detection_status == "GOOD_WITH_EXTRAS"), "/", nrow(test_data), "\n")
  cat("Missing species:", sum(results$detection_status == "MISSING_SPECIES"), "/", nrow(test_data), "\n")
  
  return(results)
}

# Function to create a custom test with user-provided abstract
test_custom_abstract <- function(abstract_text) {
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  # Define plant parts for testing
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  cat("Testing custom abstract:\n")
  cat(abstract_text, "\n\n")
  
  # Extract candidate names
  candidates <- extract_candidate_names(abstract_text)
  cat("Extracted candidates:", paste(candidates, collapse = ", "), "\n\n")
  
  # Validate names
  valid_species <- batch_validate_names(candidates, lookup_tables)
  cat("Valid species found:", nrow(valid_species), "\n")
  if (nrow(valid_species) > 0) {
    print(valid_species %>% select(user_supplied_name, resolved_name, status))
  }
  
  # Process full abstract
  plant_info <- extract_plant_info(
    text = abstract_text,
    abstract_id = 999,
    predicted_label = "Presence",
    lookup_tables = lookup_tables,
    plant_parts_keywords = plant_parts_keywords
  )
  
  cat("\nFinal detected taxa:\n")
  print(plant_info %>% select(match_type, resolved_name, status))
  
  return(plant_info)
}

# Run the tests if this script is executed directly
if (interactive()) {
  cat("Running comparison test...\n")
  test_results <- run_comparison_test()
  
  cat("\n\nTo test a custom abstract, use the test_custom_abstract() function.\n")
  cat("Example: test_custom_abstract(\"This is a study about Acer macrophyllum.\")\n")
}
