# Test script to verify Australia detection in geography extraction
# This test checks if Australia is correctly detected as both a country and a continent

library(tidyverse)
library(stringr)

# Source the required scripts
source("scripts/04_analysis/utilities/reference_data_utils.R")
source("scripts/04_analysis/components/04_extract_geography.R")

cat("=== Testing Australia Detection in Geography Extraction ===\n\n")

# Define test abstracts that mention Australia in different contexts
test_abstracts <- c(
  "This study examines endophytes from Australian forests.",
  "Research conducted in Australia shows interesting results.",
  "Samples were collected from plants in the Australian outback.",
  "The study focuses on Australian biodiversity.",
  "Endophyte diversity in Australia is remarkable.",
  "Comparative analysis between Australian and European samples.",
  "The Australian continent hosts diverse fungal communities.",
  "Studies in Australia reveal new species."
)

cat("Test abstracts:\n")
for (i in seq_along(test_abstracts)) {
  cat(sprintf("%d. %s\n", i, test_abstracts[i]))
}
cat("\n")

# Run the geography detection
cat("Running geography detection...\n")
results <- detect_geographic_locations_batch(test_abstracts)

cat("Results:\n")
print(results)

# Save detailed results to a file for inspection
write_csv(results, "test_geography_australia_results.csv")
cat("Detailed results saved to test_geography_australia_results.csv\n")

cat("\nDetailed analysis:\n")
for (i in seq_along(test_abstracts)) {
  cat(sprintf("\nAbstract %d: %s...\n", i, substr(test_abstracts[i], 1, 50)))
  cat(sprintf("  Countries detected: %s\n", results$countries_detected[i] %||% "NONE"))
  cat(sprintf("  Continents detected: %s\n", results$continents_detected[i] %||% "NONE"))
  cat(sprintf("  Geographic summary: %s\n", results$geographic_summary[i] %||% "NONE"))

  # Check for Australia detection
  has_australia_country <- !is.na(results$countries_detected[i]) && grepl("Australia", results$countries_detected[i])
  has_australia_continent <- !is.na(results$continents_detected[i]) && grepl("australia", results$continents_detected[i])

  cat(sprintf("  Australia as country: %s\n", ifelse(has_australia_country, "YES", "NO")))
  cat(sprintf("  Australia as continent: %s\n", ifelse(has_australia_continent, "YES", "NO")))
}

# Overall summary
total_abstracts <- length(test_abstracts)
australia_country_count <- sum(!is.na(results$countries_detected) & grepl("Australia", results$countries_detected))
australia_continent_count <- sum(!is.na(results$continents_detected) & grepl("australia", results$continents_detected))

cat(sprintf("\n=== Summary ===\n"))
cat(sprintf("Total test abstracts: %d\n", total_abstracts))
cat(sprintf("Australia detected as country: %d/%d (%.1f%%)\n",
            australia_country_count, total_abstracts, 100 * australia_country_count / total_abstracts))
cat(sprintf("Australia detected as continent: %d/%d (%.1f%%)\n",
            australia_continent_count, total_abstracts, 100 * australia_continent_count / total_abstracts))

# Check for issues
if (australia_country_count == 0) {
  cat("\n❌ ISSUE: Australia not detected as country in any test case!\n")
  cat("This confirms the reported problem.\n")
} else if (australia_country_count < australia_continent_count) {
  cat("\n⚠️  WARNING: Australia detected as country fewer times than as continent.\n")
  cat("Investigating which abstracts failed...\n")

  failed_abstracts <- which(!(!is.na(results$countries_detected) & grepl("Australia", results$countries_detected)))
  for (i in failed_abstracts) {
    cat(sprintf("Abstract %d failed: '%s'\n", i, test_abstracts[i]))
    cat(sprintf("  Countries: %s\n", results$countries_detected[i] %||% "NONE"))
    cat(sprintf("  Continents: %s\n", results$continents_detected[i] %||% "NONE"))
  }
} else {
  cat("\n✅ Australia detection appears correct.\n")
}

cat("\n=== Test Complete ===\n")