# Test Script for Geographic Detection Debugging
# B. Bock - August 27, 2025
# Purpose: Debug why geographic detection is finding fewer countries than expected

library(tidyverse)
library(stringr)

# Source the required functions
source("scripts/04_analysis/reference_data_utils.R")
source("scripts/04_analysis/extract_species_simple.R")

cat("=== GEOGRAPHIC DETECTION DEBUG TEST ===\n\n")

# Test 1: Check reference data functions
cat("1. REFERENCE DATA CHECK:\n")
all_countries <- get_all_countries()
global_north <- get_global_north_countries()
global_south <- get_global_south_countries()
continents <- get_continent_keywords()
regions <- get_region_keywords()

cat("   Total countries:", length(all_countries), "\n")
cat("   Global North countries:", length(global_north), "\n")
cat("   Global South countries:", length(global_south), "\n")
cat("   Continents:", length(continents), "\n")
cat("   Regions:", length(regions), "\n")
cat("   First 10 countries:", paste(head(all_countries, 10), collapse = ", "), "\n\n")

# Test 2: Simple test cases
cat("2. SIMPLE TEST CASES:\n")
test_texts <- c(
  "Study conducted in the United States using samples from California.",
  "Research from Canada and Mexico with field sites in British Columbia.",
  "Samples collected in Brazil, Argentina, and Chile during the summer season.",
  "Field work in New Zealand and Australia across multiple forest types.",
  "Laboratory analysis performed in Germany at the University of Munich.",
  "Collection sites in Japan, China, and South Korea were examined.",
  "Tropical samples from Costa Rica, Panama, and Ecuador were analyzed.",
  "European study including sites in France, Italy, and Spain.",
  "African samples from Kenya, Tanzania, and South Africa.",
  "No geographic information in this abstract about methodology only."
)

cat("Testing with", length(test_texts), "sample abstracts...\n\n")

# Test the batch function
result <- detect_geographic_locations_batch(test_texts)

# Display results for each test case
for (i in 1:length(test_texts)) {
  cat("Test", i, ":\n")
  cat("  Text: \"", substr(test_texts[i], 1, 60), "...\"\n", sep = "")
  cat("  Countries detected:", ifelse(is.na(result$countries_detected[i]), "NONE", result$countries_detected[i]), "\n")
  cat("  Global North:", ifelse(is.na(result$global_north_countries[i]), "NONE", result$global_north_countries[i]), "\n")
  cat("  Global South:", ifelse(is.na(result$global_south_countries[i]), "NONE", result$global_south_countries[i]), "\n")
  cat("  Geographic summary:", ifelse(is.na(result$geographic_summary[i]), "NONE", result$geographic_summary[i]), "\n\n")
}

# Test 3: Individual country detection testing
cat("3. INDIVIDUAL COUNTRY DETECTION TEST:\n")
test_single <- "Study conducted in the United States with samples from Canada."
cat("Test text:", test_single, "\n")

# Test individual country matches
test_countries <- c("United States", "USA", "US", "Canada", "Mexico", "Brazil", "Germany", "Japan")
cat("Testing individual country detection:\n")

for (country in test_countries) {
  country_lower <- str_to_lower(country)
  text_lower <- str_to_lower(test_single)
  
  # Test the regex pattern that should be used
  pattern <- paste0("\\b", str_replace_all(country_lower, "\\s+", "\\\\s+"), "\\b")
  match_result <- str_detect(text_lower, pattern)
  
  cat("  ", country, ": Pattern='", pattern, "' -> ", match_result, "\n", sep = "")
}

# Test 4: Compare original vs batch function
cat("\n4. COMPARE ORIGINAL VS BATCH FUNCTION:\n")
original_result <- detect_geographic_locations(test_single)
batch_result <- detect_geographic_locations_batch(c(test_single))

cat("Original function result:\n")
cat("  Countries:", original_result$countries, "\n")
cat("  Global North:", original_result$global_north_countries, "\n")
cat("  Global South:", original_result$global_south_countries, "\n\n")

cat("Batch function result:\n")
cat("  Countries:", batch_result$countries_detected[1], "\n")
cat("  Global North:", batch_result$global_north_countries[1], "\n")
cat("  Global South:", batch_result$global_south_countries[1], "\n\n")

# Test 5: Load a sample of real data to test
cat("5. REAL DATA SAMPLE TEST:\n")
if (file.exists("results/comprehensive_extraction_results.csv")) {
  cat("Loading real data sample...\n")
  real_data <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)
  
  # Get abstracts that should have countries
  sample_abstracts <- real_data %>%
    filter(!is.na(abstract)) %>%
    slice_sample(n = 10) %>%
    pull(abstract)
  
  cat("Testing", length(sample_abstracts), "real abstracts...\n")
  
  real_results <- detect_geographic_locations_batch(sample_abstracts)
  
  countries_found <- sum(!is.na(real_results$countries_detected))
  cat("Real data test: Found countries in", countries_found, "out of", length(sample_abstracts), "abstracts\n\n")
  
  # Show examples
  for (i in 1:min(3, length(sample_abstracts))) {
    cat("Real example", i, ":\n")
    cat("  Abstract excerpt: \"", substr(sample_abstracts[i], 1, 100), "...\"\n", sep = "")
    cat("  Countries found:", ifelse(is.na(real_results$countries_detected[i]), "NONE", real_results$countries_detected[i]), "\n\n")
  }
}

# Test 6: Manual regex testing
cat("6. MANUAL REGEX PATTERN TESTING:\n")
manual_test_text <- "Study conducted in the United States and Canada"
manual_test_lower <- str_to_lower(manual_test_text)

test_patterns <- c(
  "\\bunited states\\b",
  "\\bcanada\\b",
  "\\busa\\b",
  "\\bus\\b"
)

for (pattern in test_patterns) {
  result <- str_detect(manual_test_lower, pattern)
  cat("  Pattern '", pattern, "' in '", manual_test_lower, "': ", result, "\n", sep = "")
}

# Test 7: Check for problematic characters or encoding
cat("\n7. TEXT ENCODING AND CHARACTER CHECK:\n")
for (i in 1:min(3, length(test_texts))) {
  text <- test_texts[i]
  cat("Text", i, ":\n")
  cat("  Length:", nchar(text), "\n")
  cat("  Encoding:", Encoding(text), "\n")
  cat("  Contains non-ASCII:", any(charToRaw(text) > 127), "\n")
  cat("  Lowercase version: \"", str_to_lower(text), "\"\n\n")
}

# Test 8: Performance check
cat("8. PERFORMANCE CHECK:\n")
large_test <- rep(test_texts, 100)  # 1000 texts
cat("Testing with", length(large_test), "texts...\n")

start_time <- Sys.time()
large_result <- detect_geographic_locations_batch(large_test)
end_time <- Sys.time()

countries_detected_large <- sum(!is.na(large_result$countries_detected))
cat("Large test: Found countries in", countries_detected_large, "texts\n")
cat("Processing time:", round(as.numeric(difftime(end_time, start_time, units = "secs")), 2), "seconds\n")
cat("Rate:", round(length(large_test) / as.numeric(difftime(end_time, start_time, units = "secs"))), "texts/second\n\n")

cat("=== DEBUG TEST COMPLETE ===\n")
cat("If you see issues above, check:\n")
cat("1. Are reference functions returning the expected data?\n")
cat("2. Are regex patterns matching correctly?\n")
cat("3. Are there encoding issues with the text?\n")
cat("4. Does the original function work better than the batch function?\n")
cat("5. Are real abstracts formatted differently than test cases?\n")
