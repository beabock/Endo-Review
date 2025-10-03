# Test script to check country code conversion and normalization
library(tidyverse)
library(countrycode)

# Source the utilities
source("scripts/04_analysis/utilities/reference_data_utils.R")

# Test Australia specifically
test_countries <- c("Australia", "Australia", "australia", "AUSTRALIA")

cat("=== Testing Country Code Conversion ===\n")

for (country in test_countries) {
  cat(sprintf("Testing: '%s'\n", country))

  # Test normalize_country_vector
  normalized <- normalize_country_vector(country, ensure_in_classification = TRUE)
  cat(sprintf("  Normalized: '%s'\n", normalized))

  # Test countrycode conversion
  iso3 <- countrycode(normalized, "country.name", "iso3c", warn = FALSE)
  cat(sprintf("  ISO3: '%s'\n", iso3))

  # Check if in all countries
  all_countries <- get_all_countries()
  cat(sprintf("  In all_countries: %s\n", normalized %in% all_countries))

  cat("\n")
}

# Test the full geography detection on a simple Australia text
test_text <- "This study was conducted in Australia."

cat("=== Testing Geography Detection on Sample Text ===\n")
cat("Text:", test_text, "\n")

source("scripts/04_analysis/components/04_extract_geography.R")

results <- detect_geographic_locations_batch(test_text)
print(results)

# Check if Australia is detected as country
has_australia <- grepl("Australia", results$countries_detected[1]) |
                 grepl("australia", results$countries_detected[1])
cat(sprintf("Australia detected as country: %s\n", has_australia))

has_australia_continent <- grepl("australia", results$continents_detected[1])
cat(sprintf("Australia detected as continent: %s\n", has_australia_continent))

cat("\n=== Test Complete ===\n")