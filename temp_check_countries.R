library(tidyverse)

# Source our reference data
source("scripts/04_analysis/utilities/reference_data_utils.R")

# Load the geography detection results
if (!file.exists("results/geographic/geography_detection_results.csv")) {
  stop("Geography detection results file not found")
}

geo <- read_csv("results/geographic/geography_detection_results.csv", show_col_types = FALSE)

# Get detected countries (standardize them to canonical forms)
detected_countries <- geo %>%
  filter(!is.na(countries_detected), countries_detected != "") %>%
  select(id, countries_detected) %>%
  distinct(id, countries_detected) %>%
  separate_rows(countries_detected, sep = "; ") %>%
  filter(!is.na(countries_detected), countries_detected != "") %>%
  mutate(
    # Standardize to canonical form
    country_canonical = map_chr(countries_detected, standardize_country_name)
  ) %>%
  distinct(country_canonical) %>%
  pull(country_canonical)

# Get canonical countries from our reference data (not all variations)
canonical_countries <- get_canonical_countries()

# Find countries not detected
undetected_countries <- setdiff(canonical_countries, detected_countries)

cat("=== CANONICAL COUNTRY DETECTION SUMMARY ===\n")
cat("Total canonical countries:", length(canonical_countries), "\n")
cat("Detected countries:", length(detected_countries), "\n")
cat("Undetected countries:", length(undetected_countries), "\n")
cat("Detection rate:", round(100 * length(detected_countries) / length(canonical_countries), 1), "%\n\n")

if (length(undetected_countries) > 0) {
  cat("List of undetected canonical countries:\n")
  cat(paste(sort(undetected_countries), collapse = "\n"), "\n")
} else {
  cat("All canonical countries detected!\n")
}

# Optional: Show which variations were actually found in the data
cat("\n=== VARIATIONS FOUND IN DATA ===\n")
cat("(Shows the actual text forms detected, before standardization)\n\n")
geo %>%
  filter(!is.na(countries_detected), countries_detected != "") %>%
  select(countries_detected) %>%
  separate_rows(countries_detected, sep = "; ") %>%
  filter(!is.na(countries_detected), countries_detected != "") %>%
  count(countries_detected, sort = TRUE) %>%
  head(20) %>%
  print()