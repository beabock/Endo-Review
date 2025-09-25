# =============================================================================
# test_mycorrhizal_check.R - Test script for mycorrhizal checking component
# =============================================================================
#
# Purpose: Test the mycorrhizal checking functionality with sample data
#
# Description: Creates mock species detection data and tests the mycorrhizal
# classification to ensure it works correctly before running on full dataset.
#
# Author: B. Bock
# Date: 2024-09-25
#
# =============================================================================

library(tidyverse)
library(funguildr)

cat("=== TESTING MYCORRHIZAL CHECKING COMPONENT ===\n")

# Source the mycorrhizal checking functions
source("scripts/04_analysis/components/01b_mycorrhizal_check.R")

#' Create mock species detection data for testing
#'
#' Creates a small dataset that simulates the output from 01_extract_species.R
#' with a mix of mycorrhizal and non-mycorrhizal fungal taxa.
#'
#' @return Dataframe with mock species detection results
create_mock_species_data <- function() {

  # Mock abstracts with different fungal taxa combinations
  mock_data <- tribble(
    # Abstract 1: Only mycorrhizal fungi (should be TRUE)
    ~id, ~abstract, ~predicted_label, ~match_type, ~canonicalName, ~kingdom, ~phylum, ~family, ~genus, ~status, ~resolved_name, ~acceptedScientificName,
    1, "Abstract with Glomus and Rhizophagus", "Presence", "species", "Glomus intraradices", "Fungi", "Glomeromycota", "Glomeraceae", "Glomus", "ACCEPTED", "Glomus intraradices", "Glomus intraradices",
    1, "Abstract with Glomus and Rhizophagus", "Presence", "species", "Rhizophagus irregularis", "Fungi", "Glomeromycota", "Glomeraceae", "Rhizophagus", "ACCEPTED", "Rhizophagus irregularis", "Rhizophagus irregularis",

    # Abstract 2: Mix of mycorrhizal and non-mycorrhizal (should be FALSE)
    2, "Abstract with Glomus and Candida", "Presence", "species", "Glomus intraradices", "Fungi", "Glomeromycota", "Glomeraceae", "Glomus", "ACCEPTED", "Glomus intraradices", "Glomus intraradices",
    2, "Abstract with Glomus and Candida", "Presence", "species", "Candida albicans", "Fungi", "Ascomycota", "Saccharomycetaceae", "Candida", "ACCEPTED", "Candida albicans", "Candida albicans",

    # Abstract 3: No fungal taxa (should be FALSE)
    3, "Abstract with only plants", "Presence", "species", "Quercus robur", "Plantae", "Tracheophyta", "Fagaceae", "Quercus", "ACCEPTED", "Quercus robur", "Quercus robur",
    3, "Abstract with only plants", "Presence", "species", "Pinus sylvestris", "Plantae", "Tracheophyta", "Pinaceae", "Pinus", "ACCEPTED", "Pinus sylvestris", "Pinus sylvestris",

    # Abstract 4: Only non-mycorrhizal fungi (should be FALSE)
    4, "Abstract with only Candida", "Presence", "species", "Candida albicans", "Fungi", "Ascomycota", "Saccharomycetaceae", "Candida", "ACCEPTED", "Candida albicans", "Candida albicans",
    4, "Abstract with only Candida", "Presence", "species", "Aspergillus niger", "Fungi", "Ascomycota", "Aspergillaceae", "Aspergillus", "ACCEPTED", "Aspergillus niger", "Aspergillus niger",

    # Abstract 5: Ectomycorrhizal fungi only (should be TRUE)
    5, "Abstract with ectomycorrhizal fungi", "Presence", "species", "Pisolithus tinctorius", "Fungi", "Basidiomycota", "Pisolithaceae", "Pisolithus", "ACCEPTED", "Pisolithus tinctorius", "Pisolithus tinctorius",
    5, "Abstract with ectomycorrhizal fungi", "Presence", "species", "Rhizopogon vulgaris", "Fungi", "Basidiomycota", "Rhizopogonaceae", "Rhizopogon", "ACCEPTED", "Rhizopogon vulgaris", "Rhizopogon vulgaris"
  )

  return(mock_data)
}

#' Test the mycorrhizal classification functions
#'
#' Tests individual functions to ensure they work correctly with mock data.
test_mycorrhizal_functions <- function() {

  cat("\n1. Testing individual functions...\n")

  # Create mock data
  mock_data <- create_mock_species_data()

  # Test FUNGuild classification
  cat("   Testing FUNGuild classification...\n")
  fungal_taxa <- mock_data %>%
    filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
    distinct(resolved_name, kingdom, phylum, family, genus)

  mycorrhizal_classifications <- classify_fungal_taxa_mycorrhizal(
    fungal_taxa$resolved_name,
    fungal_taxa
  )

  cat("   Classified", nrow(mycorrhizal_classifications), "fungal taxa\n")
  print(mycorrhizal_classifications %>%
    select(resolved_name, is_mycorrhizal, funguild_guild) %>%
    arrange(resolved_name))

  # Test abstract-level classification
  cat("\n   Testing abstract-level classification...\n")
  abstract_ids <- unique(mock_data$id)

  for (abstract_id in abstract_ids) {
    abstract_data <- mock_data %>% filter(id == abstract_id)

    is_mycorrhizal_only <- determine_abstract_mycorrhizal_status(
      abstract_data,
      mycorrhizal_classifications
    )

    cat("   Abstract", abstract_id, ":", is_mycorrhizal_only, "\n")
  }

  return(TRUE)
}

#' Test the complete pipeline
#'
#' Tests the full mycorrhizal checking pipeline with mock data.
test_complete_pipeline <- function() {

  cat("\n2. Testing complete pipeline...\n")

  # Create a temporary CSV file with mock data
  mock_file <- "test_species_detection_results.csv"
  mock_data <- create_mock_species_data()

  tryCatch({
    write_csv(mock_data, mock_file)
    cat("   Created test data file:", mock_file, "\n")

    # Run the complete analysis
    results <- identify_mycorrhizal_papers(
      input_file = mock_file,
      output_file = "test_mycorrhizal_results.csv",
      force_rerun = TRUE,
      verbose = TRUE
    )

    # Check results
    cat("\n   Results summary:\n")
    results_summary <- results %>%
      group_by(id, is_mycorrhizal_only) %>%
      summarise(count = n(), .groups = "drop")

    print(results_summary)

    # Verify expected results
    expected_results <- tribble(
      ~id, ~is_mycorrhizal_only,
      1, TRUE,   # Only mycorrhizal
      2, FALSE,  # Mixed fungi
      3, FALSE,  # No fungi
      4, FALSE,  # Only non-mycorrhizal
      5, TRUE    # Only ectomycorrhizal
    )

    # Compare with expected
    actual_results <- results %>%
      distinct(id, is_mycorrhizal_only) %>%
      arrange(id)

    cat("\n   Expected vs Actual:\n")
    comparison <- expected_results %>%
      left_join(actual_results, by = "id", suffix = c("_expected", "_actual")) %>%
      mutate(match = is_mycorrhizal_only_expected == is_mycorrhizal_only_actual)

    print(comparison)

    all_correct <- all(comparison$match, na.rm = TRUE)
    cat("\n   All results correct:", all_correct, "\n")

    return(all_correct)

  }, finally = {
    # Clean up test files
    if (file.exists(mock_file)) file.remove(mock_file)
    if (file.exists("test_mycorrhizal_results.csv")) file.remove("test_mycorrhizal_results.csv")
  })
}

#' Run all tests
run_tests <- function() {

  cat("Starting mycorrhizal checking tests...\n")

  # Test individual functions
  function_test_passed <- FALSE
  tryCatch({
    function_test_passed <- test_mycorrhizal_functions()
  }, error = function(e) {
    cat("   ❌ Function test failed:", e$message, "\n")
  })

  # Test complete pipeline
  pipeline_test_passed <- FALSE
  tryCatch({
    pipeline_test_passed <- test_complete_pipeline()
  }, error = function(e) {
    cat("   ❌ Pipeline test failed:", e$message, "\n")
  })

  # Summary
  cat("\n=== TEST SUMMARY ===\n")
  cat("Function tests passed:", function_test_passed, "\n")
  cat("Pipeline tests passed:", pipeline_test_passed, "\n")
  cat("Overall result:", function_test_passed && pipeline_test_passed, "\n")

  if (function_test_passed && pipeline_test_passed) {
    cat("✅ All tests passed! Mycorrhizal checking component is ready.\n")
  } else {
    cat("❌ Some tests failed. Please review the implementation.\n")
  }

  return(function_test_passed && pipeline_test_passed)
}

# Run tests if script is called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "test_mycorrhizal_check.R")) {

  # Check if FUNGuild package is available
  if (!require("funguildr", quietly = TRUE)) {
    cat("⚠️  FUNGuild package not available for testing.\n")
    cat("   Installing funguildr...\n")

    tryCatch({
      install.packages("funguildr")
      library(funguildr)
      cat("   ✅ FUNGuild package installed successfully\n")
    }, error = function(e) {
      cat("   ❌ Failed to install funguildr:", e$message, "\n")
      cat("   Tests will use taxonomic fallback method\n")
    })
  }

  # Run the tests
  test_results <- run_tests()

  cat("\n✅ Testing completed!\n")
}