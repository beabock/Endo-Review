library(tidyverse)

# Source our reference data
source("scripts/04_analysis/utilities/reference_data_utils.R")

# Load the geography detection results
if (!file.exists("results/geographic/geography_detection_results.csv")) {
  stop("Geography detection results file not found")
}

geo <- read_csv("results/geographic/geography_detection_results.csv", show_col_types = FALSE)

# Get detected countries
detected_countries <- geo %>%
  filter(!is.na(countries_detected), countries_detected != "") %>%
  select(id, countries_detected) %>%
  distinct(id, countries_detected) %>%
  separate_rows(countries_detected, sep = "; ") %>%
  filter(!is.na(countries_detected), countries_detected != "") %>%
  mutate(country_std = stringr::str_to_title(countries_detected)) %>%
  distinct(country_std) %>%
  pull(country_std)

# Get all countries from our reference data
all_countries <- get_all_countries() %>%
  stringr::str_to_title()

# Find countries not detected
undetected_countries <- setdiff(all_countries, detected_countries)

cat("Total world countries:", length(all_countries), "\n")
cat("Detected countries:", length(detected_countries), "\n")
cat("Undetected countries:", length(undetected_countries), "\n\n")
cat("List of undetected countries:\n")
cat(paste(sort(undetected_countries), collapse = "\n"), "\n")