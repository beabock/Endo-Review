# =============================================================================
# test_geography_deduplication.R - Test script to verify geographical deduplication
# =============================================================================
#
# Purpose: Test that geographical entities are only counted once per abstract
#
# =============================================================================

library(tidyverse)
library(stringr)

# Source the geography extraction function
source("scripts/04_analysis/components/04_extract_geography.R")

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

cat("🧪 TESTING GEOGRAPHY DEDUPLICATION\n")
cat("=================================\n\n")

# Create test cases with duplicate mentions
test_cases <- tibble(
  id = 1:5,
  abstract = c(
    # Test case 1: USA mentioned multiple times
    "This study was conducted in the USA. We collected samples from various locations across the USA. The United States provides an ideal environment for this research.",
    
    # Test case 2: Multiple countries mentioned, some duplicated
    "Research was conducted in China, Japan, and China again. We also studied populations in Japan and South Korea.",
    
    # Test case 3: Country mentioned with synonyms
    "This research took place in the United States. We collected data from the US and also from America.",
    
    # Test case 4: Multiple continents and regions
    "We studied sites across Europe, North America, and Europe. The European sites were particularly interesting. We also examined North American locations.",
    
    # Test case 5: Mixed geographical entities
    "Research sites included Brazil, South America, and Brazil. We also studied sites in Argentina, which is also in South America."
  )
)

cat("📝 Test cases:\n")
for (i in 1:nrow(test_cases)) {
  cat("   ", i, ": ", substr(test_cases$abstract[i], 1, 60), "...\n")
}

cat("\n🔬 Running geography detection...\n")

# Test the geography detection function directly
results <- detect_geographic_locations_batch(test_cases$abstract)

# Score geography detection
geography_detection_works <- !is.null(results) && nrow(results) == nrow(test_cases)
test_result(geography_detection_works, "Geography detection", paste0("Processed ", nrow(results), " abstracts"))

# Add IDs for clarity
results_with_ids <- bind_cols(
  select(test_cases, id),
  results
)

cat("\n📊 RESULTS:\n")
cat("=========\n")

for (i in 1:nrow(results_with_ids)) {
  cat("\n🔍 Test case", i, ":\n")
  cat("   Abstract: ", substr(test_cases$abstract[i], 1, 80), "...\n")
  
  if (!is.na(results_with_ids$countries_detected[i])) {
    countries <- str_split(results_with_ids$countries_detected[i], "; ")[[1]]
    cat("   Countries detected: ", paste(countries, collapse = ", "), " (", length(countries), " unique)\n")
  } else {
    cat("   Countries detected: None\n")
  }
  
  if (!is.na(results_with_ids$continents_detected[i])) {
    continents <- str_split(results_with_ids$continents_detected[i], "; ")[[1]]
    cat("   Continents detected: ", paste(continents, collapse = ", "), " (", length(continents), " unique)\n")
  } else {
    cat("   Continents detected: None\n")
  }
  
  if (!is.na(results_with_ids$regions_detected[i])) {
    regions <- str_split(results_with_ids$regions_detected[i], "; ")[[1]]
    cat("   Regions detected: ", paste(regions, collapse = ", "), " (", length(regions), " unique)\n")
  } else {
    cat("   Regions detected: None\n")
  }
}

cat("\n✅ VERIFICATION:\n")
cat("===============\n")

# Check for duplicates in results
duplicate_issues <- FALSE

for (i in 1:nrow(results_with_ids)) {
  # Check countries
  if (!is.na(results_with_ids$countries_detected[i])) {
    countries <- str_split(results_with_ids$countries_detected[i], "; ")[[1]]
    if (length(countries) != length(unique(countries))) {
      cat("❌ Test case", i, ": Duplicate countries found:", paste(countries, collapse = ", "), "\n")
      duplicate_issues <- TRUE
    }
  }
  
  # Check continents
  if (!is.na(results_with_ids$continents_detected[i])) {
    continents <- str_split(results_with_ids$continents_detected[i], "; ")[[1]]
    if (length(continents) != length(unique(continents))) {
      cat("❌ Test case", i, ": Duplicate continents found:", paste(continents, collapse = ", "), "\n")
      duplicate_issues <- TRUE
    }
  }
  
  # Check regions
  if (!is.na(results_with_ids$regions_detected[i])) {
    regions <- str_split(results_with_ids$regions_detected[i], "; ")[[1]]
    if (length(regions) != length(unique(regions))) {
      cat("❌ Test case", i, ": Duplicate regions found:", paste(regions, collapse = ", "), "\n")
      duplicate_issues <- TRUE
    }
  }
}

if (!duplicate_issues) {
  cat("✅ All test cases passed! No duplicates found.\n")
  cat("✅ Each geographical entity is counted only once per abstract.\n")
} else {
  cat("❌ Some test cases failed. Duplicates detected.\n")
}

# Score deduplication
test_result(!duplicate_issues, "Geography deduplication", ifelse(duplicate_issues, "Duplicates found in results", "All geographical entities properly deduplicated"))

cat("\n📋 Test completed.\n")

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! Geography deduplication is working correctly.\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}