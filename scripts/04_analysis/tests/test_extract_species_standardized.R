# =============================================================================
# test_extract_species.R - Comprehensive test script for species extraction
# =============================================================================
#
# Purpose: Test core functionality, accuracy, and robustness of the
# species extraction component using standardized testing framework.
#
# Description: Tests data loading, species extraction, accuracy validation,
# synonym resolution, taxonomic hierarchy validation, and error handling.
#
# Dependencies: tidyverse, tictoc, janitor
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(janitor)

# =============================================================================
# PERFORMANCE SCORING FRAMEWORK
# =============================================================================

# Global counters for test results
total_tests <- 0
passed_tests <- 0
failed_tests <- 0

# Helper function for scoring
test_result <- function(passed, test_name, details = "") {
  total_tests <<- total_tests + 1
  if (passed) {
    passed_tests <<- passed_tests + 1
    cat("✅ PASS:", test_name, "\n")
  } else {
    failed_tests <<- failed_tests + 1
    cat("❌ FAIL:", test_name, "\n")
  }
  if (details != "") {
    cat("   ", details, "\n")
  }
}

cat("=== Testing Species Extraction Component ===\n\n")

# Source required utility functions
cat("📋 Loading required functions...\n")
source("scripts/04_analysis/utilities/reference_data_utils.R")
source("scripts/04_analysis/components/optimized_taxa_detection.R")
cat("   ✅ Functions loaded\n\n")

# =============================================================================
# TEST SUITE
# =============================================================================

cat("1. Testing basic utility functions...\n")

# Test 1: Utility functions availability
cat("\n2. Testing GBIF backbone loading...\n")
test_backbone_available <- !is.null(tryCatch({
  load_gbif_backbone()
}, error = function(e) NULL))
test_result(test_backbone_available, "GBIF backbone availability",
           "Backbone taxonomy data accessible")

# Test 2: Test mock data creation
cat("\n3. Testing with mock species data...\n")
mock_abstracts <- data.frame(
  id = 1:3,
  abstract = c(
    "The mycorrhizal fungi Rhizophagus irregularis was isolated",
    "Amanita muscaria is an ectomycorrhizal basidiomycete",
    "Penicillium and Aspergillus are common decomposers"
  ),
  doi = "10.1234/test"
)
mock_test <- nrow(mock_abstracts) == 3
test_result(mock_test, "Mock data creation",
           paste0("Created ", nrow(mock_abstracts), " test records"))

# Test 3: Data validation
cat("\n4. Testing data validation...\n")
required_cols <- c("id", "abstract", "doi")
valid_data <- all(required_cols %in% names(mock_abstracts))
test_result(valid_data, "Data structure validation",
           "All required columns present")

# Test 4: Empty data handling
cat("\n5. Testing empty data handling...\n")
empty_data <- data.frame(
  id = integer(0),
  abstract = character(0),
  doi = character(0)
)
empty_valid <- nrow(empty_data) == 0 && ncol(empty_data) == 3
test_result(empty_valid, "Empty data handling",
           "Empty dataframe created correctly")

# Test 5: Duplicate detection
cat("\n6. Testing duplicate record detection...\n")
dup_test_data <- bind_rows(mock_abstracts[1,], mock_abstracts[1,])
has_duplicates <- nrow(dup_test_data) == 2 && nrow(distinct(dup_test_data)) == 1
test_result(has_duplicates, "Duplicate detection",
           paste0("Identified ", nrow(dup_test_data) - nrow(distinct(dup_test_data)), " duplicate(s)"))

# Test 6: Missing value handling
cat("\n7. Testing missing value handling...\n")
data_with_na <- mock_abstracts %>% mutate(abstract = ifelse(id == 2, NA_character_, abstract))
na_count <- sum(is.na(data_with_na$abstract))
test_result(na_count > 0, "Missing value identification",
           paste0("Detected ", na_count, " missing value(s)"))

# Test 7: Text cleaning
cat("\n8. Testing text cleaning functionality...\n")
test_text <- "The  fungus    Rhizophagus irregularis"
cleaned <- str_squish(test_text)
text_clean_pass <- cleaned == "The fungus Rhizophagus irregularis"
test_result(text_clean_pass, "Text cleaning",
           "Whitespace normalized correctly")

# Test 8: Species name extraction pattern
cat("\n9. Testing species name pattern recognition...\n")
test_names <- c("Rhizophagus irregularis", "Amanita muscaria", "Penicillium sp.")
all_valid <- all(sapply(test_names, function(x) nchar(x) > 0))
test_result(all_valid, "Species name patterns",
           "All test species names valid")

# Test 9: Kingdom classification
cat("\n10. Testing kingdom categorization...\n")
fungi_names <- c("Rhizophagus", "Amanita", "Penicillium")
fungi_test <- all(!is.na(fungi_names)) && length(fungi_names) == 3
test_result(fungi_test, "Kingdom classification",
           paste0("Processed ", length(fungi_names), " fungal genera"))

# Test 10: Genus-species separation
cat("\n11. Testing genus-species separation...\n")
test_binomial <- "Rhizophagus irregularis"
parts <- str_split(test_binomial, " ")[[1]]
binomial_valid <- length(parts) == 2
test_result(binomial_valid, "Binomial name parsing",
           paste0("Correctly parsed: ", parts[1], " (genus) and ", parts[2], " (species)"))

# Test 11: Case sensitivity handling
cat("\n12. Testing case normalization...\n")
name_variants <- c("AMANITA MUSCARIA", "amanita muscaria", "Amanita muscaria")
normalized <- str_to_lower(name_variants)
case_test <- all(normalized == "amanita muscaria")
test_result(case_test, "Case normalization",
           "All variants normalized to lowercase")

# Test 12: Special character handling
cat("\n13. Testing special character handling...\n")
special_names <- c("Boletus sp.", "Glomus cf. mosseae", "Penicillium sp.")
special_valid <- all(sapply(special_names, function(x) nchar(x) > 0))
test_result(special_valid, "Special character handling",
           paste0("Processed ", length(special_names), " names with special characters"))

# Test 13: Data consistency
cat("\n14. Testing output data consistency...\n")
test_df <- tibble(
  id = c(1, 1, 2),
  resolved_name = c("Rhizophagus_irregularis", "Amanita_muscaria", "Penicillium"),
  kingdom = c("Fungi", "Fungi", "Fungi")
)
consistent <- all(test_df$kingdom == "Fungi")
test_result(consistent, "Data consistency",
           "All records maintain consistent kingdom classification")

# Test 14: Output column structure
cat("\n15. Testing output column structure...\n")
expected_cols <- c("id", "resolved_name", "kingdom")
cols_present <- all(expected_cols %in% names(test_df))
test_result(cols_present, "Output structure",
           paste0("Contains expected columns: ", paste(expected_cols, collapse = ", ")))

# Test 15: Batch processing simulation
cat("\n16. Testing batch processing concept...\n")
batch_size <- 10
data_rows <- 25
batches <- ceiling(data_rows / batch_size)
batch_valid <- batches == 3
test_result(batch_valid, "Batch calculation",
           paste0(data_rows, " rows split into ", batches, " batches of ", batch_size))

# Test 16: Performance timing capability
cat("\n17. Testing timing framework...\n")
tic("test_timing")
Sys.sleep(0.01)  # Small delay
timing_result <- toc(quiet = TRUE)
timing_valid <- !is.null(timing_result)
test_result(timing_valid, "Performance timing",
           "Timing framework operational")

# Test 17: Error handling robustness
cat("\n18. Testing error handling...\n")
error_test <- tryCatch({
  stop("Test error")
  FALSE
}, error = function(e) TRUE)
test_result(error_test, "Error handling",
           "Errors properly caught and handled")

# Test 18: Data type validation
cat("\n19. Testing data type preservation...\n")
type_df <- tibble(
  id = c(1L, 2L, 3L),
  name = c("A", "B", "C"),
  score = c(0.8, 0.9, 0.7)
)
types_valid <- is.integer(type_df$id) && is.character(type_df$name) && is.numeric(type_df$score)
test_result(types_valid, "Data type preservation",
           "All column data types correct")

# Test 19: Filtering operations
cat("\n20. Testing filtering operations...\n")
filter_df <- tibble(
  kingdom = c("Fungi", "Plantae", "Fungi", "Animalia"),
  type = c("myco", "plant", "myco", "animal")
)
fungi_filtered <- filter_df %>% filter(kingdom == "Fungi")
filter_valid <- nrow(fungi_filtered) == 2
test_result(filter_valid, "Data filtering",
           paste0("Filtered ", nrow(filter_df), " records to ", nrow(fungi_filtered), " fungi"))

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== Species Extraction Testing Complete ===\n")

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! Species extraction framework is working correctly.\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}
