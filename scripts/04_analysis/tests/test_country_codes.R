# Test script to check country code conversion and normalization
library(tidyverse)
library(countrycode)

# Source the utilities
source("scripts/04_analysis/utilities/reference_data_utils.R")

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
  in_countries <- normalized %in% all_countries
  cat(sprintf("  In all_countries: %s\n", in_countries))

  # Score this test case
  test_passed <- !is.na(normalized) && normalized == "Australia" && !is.na(iso3) && iso3 == "AUS" && in_countries
  test_result(test_passed, paste0("Country normalization: ", country), 
              ifelse(test_passed, "Successfully normalized to Australia/AUS", 
                     paste0("Expected Australia/AUS, got ", normalized, "/", iso3)))

  cat("\n")
}

# Additional country normalization tests
additional_countries <- list(
  list(input = c("USA", "usa", "U.S.A.", "United States"), expected = "United States", iso3 = "USA"),
  list(input = c("UK", "uk", "United Kingdom", "Great Britain"), expected = "United Kingdom", iso3 = "GBR"),
  list(input = c("Germany", "germany"), expected = "Germany", iso3 = "DEU"),
  list(input = c("Brazil", "brazil"), expected = "Brazil", iso3 = "BRA"),
  list(input = c("Japan", "japan"), expected = "Japan", iso3 = "JPN"),
  list(input = c("China", "china", "People's Republic of China"), expected = "China", iso3 = "CHN"),
  list(input = c("India", "india"), expected = "India", iso3 = "IND"),
  list(input = c("France", "france"), expected = "France", iso3 = "FRA"),
  list(input = c("Canada", "canada"), expected = "Canada", iso3 = "CAN"),
  list(input = c("Mexico"), expected = "Mexico", iso3 = "MEX")
)

cat("=== Additional Country Normalization Tests ===\n")

for (country_test in additional_countries) {
  for (country_input in country_test$input) {
    cat(sprintf("Testing: '%s'\n", country_input))

    # Test normalize_country_vector
    normalized <- normalize_country_vector(country_input, ensure_in_classification = TRUE)
    cat(sprintf("  Normalized: '%s'\n", normalized))

    # Test countrycode conversion
    iso3 <- countrycode(normalized, "country.name", "iso3c", warn = FALSE)
    cat(sprintf("  ISO3: '%s'\n", iso3))

    # Check if in all countries
    all_countries <- get_all_countries()
    in_countries <- normalized %in% all_countries
    cat(sprintf("  In all_countries: %s\n", in_countries))

    # Score this test case
    test_passed <- !is.na(normalized) && normalized == country_test$expected &&
                   !is.na(iso3) && iso3 == country_test$iso3 && in_countries
    test_result(test_passed, paste0("Country normalization: ", country_input),
                ifelse(test_passed, paste0("Successfully normalized to ", country_test$expected, "/", country_test$iso3),
                       paste0("Expected ", country_test$expected, "/", country_test$iso3, ", got ", normalized, "/", iso3)))

    cat("\n")
  }
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

# Score geography detection
geography_detection_works <- has_australia  # Should detect as country (continent detection is also OK since Australia is both)
test_result(geography_detection_works, "Australia geography detection",
           ifelse(geography_detection_works,
                  ifelse(has_australia_continent,
                         "Australia correctly detected as both country and continent",
                         "Australia correctly detected as country"),
                  "Australia detection failed"))

cat("\n=== Additional Geography Detection Tests ===\n")

# Test cases for geography detection
geography_test_cases <- list(
  list(text = "Research conducted in the United States and Canada.",
       expected_countries = c("United States", "Canada"),
       description = "Multiple North American countries"),
  list(text = "Studies from Germany, France, and the United Kingdom.",
       expected_countries = c("Germany", "France", "United Kingdom"),
       description = "European countries"),
  list(text = "Field work in Brazil and Argentina, South America.",
       expected_countries = c("Brazil", "Argentina"),
       expected_continents = c("south america"),
       description = "South American countries with continent"),
  list(text = "Samples collected across Asia including China and Japan.",
       expected_countries = c("China", "Japan"),
       expected_continents = c("asia"),
       description = "Asian countries with continent"),
  list(text = "African research in Kenya and South Africa.",
       expected_countries = c("Kenya", "South Africa"),
       expected_continents = c("africa"),
       description = "African countries with continent"),
  list(text = "The study spans Europe from Spain to Sweden.",
       expected_continents = c("europe"),
       description = "Continent-only detection"),
  list(text = "Research in Oceania including Australia and New Zealand.",
       expected_countries = c("Australia", "New Zealand"),
       expected_continents = c("oceania"),
       description = "Oceanian countries with continent"),
  list(text = "North American species found in USA, Mexico, and Canada in North America.",
       expected_countries = c("USA", "Mexico", "Canada"),
       expected_continents = c("north america"),
       description = "North American countries with continent"),
  list(text = "No geographic information in this abstract about methodology only.",
       expected_countries = NULL,
       expected_continents = NULL,
       description = "No geography expected"),
  list(text = "Global study covering all continents from Antarctica to Europe.",
       expected_continents = c("antarctica", "europe"),
       description = "Multiple continents")
)

for (test_case in geography_test_cases) {
  cat(sprintf("\nTesting: %s\n", test_case$description))
  cat(sprintf("Text: %s\n", test_case$text))

  results <- detect_geographic_locations_batch(test_case$text)

  # Check countries
  detected_countries <- NA
  if (!is.na(results$countries_detected[1]) && results$countries_detected[1] != "") {
    detected_countries <- str_split(results$countries_detected[1], "; ")[[1]]
  }

  # Check continents
  detected_continents <- NA
  if (!is.na(results$continents_detected[1]) && results$continents_detected[1] != "") {
    detected_continents <- str_split(results$continents_detected[1], "; ")[[1]]
  }

  cat(sprintf("Detected countries: %s\n", ifelse(is.na(detected_countries), "NONE",
                                                 paste(detected_countries, collapse = ", "))))
  cat(sprintf("Detected continents: %s\n", ifelse(is.na(detected_continents), "NONE",
                                                 paste(detected_continents, collapse = ", "))))

  # Score country detection
  if (!is.null(test_case$expected_countries)) {
    countries_correct <- all(test_case$expected_countries %in% detected_countries)
    test_result(countries_correct, paste0("Geography detection - ", test_case$description, " (countries)"),
               ifelse(countries_correct,
                      paste0("All expected countries detected: ", paste(test_case$expected_countries, collapse = ", ")),
                      paste0("Expected: ", paste(test_case$expected_countries, collapse = ", "),
                             ", Detected: ", paste(detected_countries, collapse = ", "))))
  }

  # Score continent detection
  if (!is.null(test_case$expected_continents)) {
    continents_correct <- all(test_case$expected_continents %in% detected_continents)
    test_result(continents_correct, paste0("Geography detection - ", test_case$description, " (continents)"),
               ifelse(continents_correct,
                      paste0("All expected continents detected: ", paste(test_case$expected_continents, collapse = ", ")),
                      paste0("Expected: ", paste(test_case$expected_continents, collapse = ", "),
                             ", Detected: ", paste(detected_continents, collapse = ", "))))
  }

  # Score no geography detection
  if (is.null(test_case$expected_countries) && is.null(test_case$expected_continents)) {
    no_geography_correct <- (is.na(detected_countries) || length(detected_countries) == 0) &&
                            (is.na(detected_continents) || length(detected_continents) == 0)
    test_result(no_geography_correct, paste0("Geography detection - ", test_case$description),
               ifelse(no_geography_correct, "Correctly detected no geographic entities",
                      paste0("Unexpected geography detected: countries=", paste(detected_countries, collapse = ", "),
                             ", continents=", paste(detected_continents, collapse = ", "))))
  }
}

cat("\n=== Edge Case Tests ===\n")

# Test edge cases
edge_cases <- list(
  list(input = "US", expected = "United States", description = "US abbreviation"),
  list(input = "U.S.", expected = "United States", description = "U.S. with periods"),
  list(input = "UK", expected = "United Kingdom", description = "UK abbreviation"),
  list(input = "Brasil", expected = "Brasil", description = "Non-standard name (should remain unchanged)"),
  list(input = "México", expected = "México", description = "Non-standard name (should remain unchanged)"),
  list(input = "Deutschland", expected = "Deutschland", description = "Non-standard name (should remain unchanged)"),
  list(input = "Nippon", expected = "Nippon", description = "Non-standard name (should remain unchanged)"),
  list(input = "Bharat", expected = "Bharat", description = "Non-standard name (should remain unchanged)")
)

for (edge_case in edge_cases) {
  cat(sprintf("Testing edge case: %s ('%s')\n", edge_case$description, edge_case$input))

  normalized <- normalize_country_vector(edge_case$input, ensure_in_classification = TRUE)
  cat(sprintf("  Normalized: '%s'\n", normalized))

  iso3 <- countrycode(normalized, "country.name", "iso3c", warn = FALSE)
  cat(sprintf("  ISO3: '%s'\n", iso3))

  test_passed <- if (grepl("Non-standard name", edge_case$description)) {
    # For non-standard names, we expect them to remain unchanged with no ISO3
    !is.na(normalized) && normalized == edge_case$input && (is.na(iso3) || iso3 == "NA")
  } else {
    # For standard abbreviations, expect proper normalization and ISO3
    !is.na(normalized) && normalized == edge_case$expected && !is.na(iso3)
  }
  test_result(test_passed, paste0("Edge case: ", edge_case$description),
             ifelse(test_passed, 
                    ifelse(grepl("Non-standard name", edge_case$description),
                           paste0("Non-standard name '", edge_case$input, "' correctly left unchanged"),
                           paste0("Successfully normalized ", edge_case$input, " to ", edge_case$expected)),
                    paste0("Unexpected result for ", edge_case$input, ": got ", normalized, "/", iso3)))

  cat("\n")
}

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! Country code conversion and geography detection working correctly.\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}