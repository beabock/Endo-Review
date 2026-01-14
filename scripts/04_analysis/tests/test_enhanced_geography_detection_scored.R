# Enhanced Geography Detection Test Suite with Scoring
# B. Bock - January 14, 2026
# Purpose: Comprehensive testing of enhanced geography detection with scoring

library(tidyverse)
library(stringr)

# Source the enhanced geography detection component
source("scripts/04_analysis/utilities/reference_data_utils.R")
source("scripts/04_analysis/components/04_extract_geography.R")

cat("=== ENHANCED GEOGRAPHY DETECTION TEST SUITE WITH SCORING ===\n")
cat("Testing improved geography extraction with comprehensive scoring\n\n")

# Initialize scoring counters
total_tests <- 0
passed_tests <- 0
failed_tests <- 0
test_results <- list()

# Helper function to check if detection matches expected
check_detection <- function(actual, expected, test_name, test_category) {
  total_tests <<- total_tests + 1
  
  # Normalize for comparison
  actual_clean <- if(is.na(actual) || actual == "NONE") "" else tolower(str_trim(actual))
  expected_clean <- if(is.na(expected) || expected == "NONE") "" else tolower(str_trim(expected))
  
  # Check if all expected countries are in actual (allows for extra detections)
  if (expected_clean == "") {
    passed <- actual_clean == ""
  } else {
    expected_items <- str_split(expected_clean, ";\\s*")[[1]]
    actual_items <- str_split(actual_clean, ";\\s*")[[1]]
    passed <- all(expected_items %in% actual_items)
  }
  
  if (passed) {
    passed_tests <<- passed_tests + 1
    status <- "✓ PASS"
  } else {
    failed_tests <<- failed_tests + 1
    status <- "✗ FAIL"
  }
  
  test_results[[length(test_results) + 1]] <<- list(
    category = test_category,
    test = test_name,
    expected = expected,
    actual = actual,
    status = status
  )
  
  return(passed)
}

# Test 1: Homonym Disambiguation (Critical for accuracy)
cat("\n1. TESTING CONTEXT-AWARE HOMONYM DISAMBIGUATION:\n")
cat("=" , rep("=", 70), "\n", sep = "")

homonym_tests <- tribble(
  ~text, ~expected_country,
  "Niger is located in West Africa", "Niger",
  "Aspergillus niger fungal growth", "NONE",
  "Turkey tail fungus identification", "NONE",
  "Republic of Turkey in the Middle East", "Turkey",
  "Chile pepper resistance to pathogens", "NONE",
  "Republic of Chile in South America", "Chile",
  "Georgia state in the United States", "United States",
  "Republic of Georgia in the Caucasus", "Georgia",
  "China plate ceramic analysis", "NONE",
  "People's Republic of China research", "China",
  "Poland spring water quality", "NONE",
  "Republic of Poland in Europe", "Poland",
  "Armenian apricot variety testing", "NONE",
  "Republic of Armenia in Asia", "Armenia",
  "Serbia spruce growth patterns", "NONE",
  "Republic of Serbia in the Balkans", "Serbia",
  "Iran produces high-quality saffron", "Iran",
  "Iranian nuclear program discussions", "Iran",
  "Persian Gulf oil reserves", "NONE",
  "Persian cat breeds are popular", "NONE"
)

homonym_results <- detect_geographic_locations_batch(homonym_tests$text)

for (i in 1:nrow(homonym_tests)) {
  actual <- ifelse(is.na(homonym_results$countries_detected[i]), "NONE", homonym_results$countries_detected[i])
  expected <- homonym_tests$expected_country[i]
  
  passed <- check_detection(actual, expected, homonym_tests$text[i], "Homonym Disambiguation")
  
  cat(sprintf("%-60s %s\n", substr(homonym_tests$text[i], 1, 60), 
              if(passed) "✓" else "✗"))
  if (!passed) {
    cat(sprintf("  Expected: %s | Got: %s\n", expected, actual))
  }
}

# Test 2: Panama and Central American Countries
cat("\n2. TESTING PANAMA AND CENTRAL AMERICAN DETECTION:\n")
cat("=", rep("=", 70), "\n", sep = "")

panama_tests <- tribble(
  ~text, ~expected_country,
  "Research in Panama and Costa Rica", "Panama; Costa Rica",
  "Studies from Guatemala, Honduras, and El Salvador", "Guatemala; Honduras; El Salvador",
  "Field work in Nicaragua and Belize", "Nicaragua; Belize",
  "Panama Canal biodiversity surveys", "Panama",
  "Barro Colorado Island in Panama", "Panama"
)

panama_results <- detect_geographic_locations_batch(panama_tests$text)

for (i in 1:nrow(panama_tests)) {
  actual <- ifelse(is.na(panama_results$countries_detected[i]), "NONE", panama_results$countries_detected[i])
  expected <- panama_tests$expected_country[i]
  
  passed <- check_detection(actual, expected, panama_tests$text[i], "Panama Detection")
  
  cat(sprintf("%-60s %s\n", substr(panama_tests$text[i], 1, 60), 
              if(passed) "✓" else "✗"))
  if (!passed) {
    cat(sprintf("  Expected: %s | Got: %s\n", expected, actual))
  }
}

# Test 3: Research Institution Mapping
cat("\n3. TESTING RESEARCH INSTITUTION DETECTION:\n")
cat("=", rep("=", 70), "\n", sep = "")

institution_tests <- tribble(
  ~text, ~expected_country,
  "Samples collected at STRI in Panama", "Panama",
  "Research conducted at the Smithsonian Tropical Research Institute", "Panama",
  "Studies from Kew Gardens in the United Kingdom", "United Kingdom",
  "CSIRO scientists in Australia discovered new species", "Australia",
  "Collaboration with Royal Botanic Gardens Kew", "United Kingdom",
  "Work done at the Missouri Botanical Garden", "United States",
  "Research at INPA in the Brazilian Amazon", "Brazil",
  "Scientists from the Max Planck Institute studied endophytes", "Germany",
  "CIAT researchers in Colombia investigated tropical species", "Colombia",
  "Joint project between USDA and Chinese Academy of Sciences", "United States; China",
  "RIKEN researchers in Japan sequenced fungal genomes", "Japan",
  "INRA studies in France focused on mycorrhizae", "France",
  "Wageningen University research in the Netherlands", "Netherlands",
  "UNAM scientists in Mexico studied desert fungi", "Mexico",
  "ICRAF projects in Kenya promote agroforestry", "Kenya",
  "CIMMYT work in Mexico improves maize varieties", "Mexico",
  "IRRI research in the Philippines enhances rice", "Philippines",
  "Harvard University Herbaria collections", "United States",
  "Smithsonian Institution biodiversity surveys", "United States",
  "Royal Botanic Gardens Sydney research", "Australia"
)

institution_results <- detect_geographic_locations_batch(institution_tests$text)

for (i in 1:nrow(institution_tests)) {
  actual <- ifelse(is.na(institution_results$countries_detected[i]), "NONE", institution_results$countries_detected[i])
  expected <- institution_tests$expected_country[i]
  
  passed <- check_detection(actual, expected, institution_tests$text[i], "Institution Mapping")
  
  cat(sprintf("%-60s %s\n", substr(institution_tests$text[i], 1, 60), 
              if(passed) "✓" else "✗"))
  if (!passed) {
    cat(sprintf("  Expected: %s | Got: %s\n", expected, actual))
  }
}

# Test 4: Adjectival and Regional Forms
cat("\n4. TESTING ADJECTIVAL AND REGIONAL FORM DETECTION:\n")
cat("=", rep("=", 70), "\n", sep = "")

adjectival_tests <- tribble(
  ~text, ~expected_country, ~expected_region,
  "Amazonian rainforest endophytes show high diversity", "Brazil; Peru; Colombia; Ecuador; Bolivia; Venezuela", "amazonian",
  "Andean cloud forest species from high elevations", "Peru; Ecuador; Colombia; Bolivia; Chile; Argentina", "andean",
  "Mesoamerican tropical forests harbor unique fungi", "Mexico; Guatemala; Belize; Honduras; El Salvador; Nicaragua; Costa Rica; Panama", "mesoamerican",
  "Southeast Asian dipterocarp forests", "Thailand; Vietnam; Malaysia; Indonesia; Philippines; Myanmar; Cambodia; Laos", "southeast",
  "Panamanian endophytes from Barro Colorado Island", "Panama", "panamanian",
  "Mediterranean climate species show drought tolerance", "Spain; Italy; Greece; Turkey; France", "mediterranean",
  "Neotropical fungal diversity in Central America", "NONE", "neotropical",
  "Bornean rainforest endophyte assemblages", "Indonesia; Malaysia; Brunei", "bornean",
  "Californian chaparral fungal communities", "United States", "californian",
  "Iberian cork oak endophytes", "Spain; Portugal", "iberian",
  "Sub-Saharan African fungi exhibit unique adaptations", "NONE", "sub-saharan",
  "East African savanna ecosystems support diverse mycobiota", "Kenya; Tanzania; Uganda; Rwanda; Burundi; Ethiopia", "east african; savanna",
  "Scandinavian boreal forests contain specialized endophytes", "Sweden; Norway; Denmark; Finland; Iceland", "scandinavian",
  "Baltic Sea coastal fungi show marine adaptations", "Estonia; Latvia; Lithuania; Poland; Germany", "baltic; coastal; marine",
  "Balkan mountain ranges harbor endemic species", "Greece; Bulgaria; Albania; Serbia; Bosnia and Herzegovina; Croatia", "balkan",
  "Himalayan alpine fungi thrive in extreme conditions", "Nepal; India; Bhutan; China; Pakistan", "himalayan",
  "Australasian mycorrhizal networks in eucalyptus forests", "Australia; New Zealand", "australasian",
  "Guinean forests harbor unique fungal species", "Guinea; Guinea-Bissau", "guinean",
  "Congolese rainforest biodiversity studies", "Democratic Republic of the Congo; Republic of the Congo", "congolese",
  "Indochinese fungal diversity patterns", "Vietnam; Cambodia; Laos; Thailand; Myanmar", "indochinese"
)

adjectival_results <- detect_geographic_locations_batch(adjectival_tests$text)

for (i in 1:nrow(adjectival_tests)) {
  actual_country <- ifelse(is.na(adjectival_results$countries_detected[i]), "NONE", adjectival_results$countries_detected[i])
  actual_region <- ifelse(is.na(adjectival_results$regions_detected[i]), "NONE", adjectival_results$regions_detected[i])
  expected_country <- adjectival_tests$expected_country[i]
  expected_region <- adjectival_tests$expected_region[i]
  
  # Check both country and region
  country_passed <- check_detection(actual_country, expected_country, 
                                    paste(adjectival_tests$text[i], "(countries)"), 
                                    "Adjectival Forms - Countries")
  region_passed <- check_detection(actual_region, expected_region, 
                                   paste(adjectival_tests$text[i], "(regions)"), 
                                   "Adjectival Forms - Regions")
  
  cat(sprintf("%-60s %s\n", substr(adjectival_tests$text[i], 1, 60), 
              if(country_passed && region_passed) "✓" else "✗"))
  if (!country_passed) {
    cat(sprintf("  Expected countries: %s | Got: %s\n", expected_country, actual_country))
  }
  if (!region_passed) {
    cat(sprintf("  Expected regions: %s | Got: %s\n", expected_region, actual_region))
  }
}

# Test 5: Combined Detection (Institution + Adjectival + Direct)
cat("\n5. TESTING COMBINED DETECTION:\n")
cat("=", rep("=", 70), "\n", sep = "")

combined_tests <- tribble(
  ~text, ~expected_country,
  "STRI researchers studied Panamanian endophytes in tropical forests", "Panama",
  "Amazonian species collected in Brazil by INPA scientists", "Brazil; Peru; Colombia; Ecuador; Bolivia; Venezuela",
  "Kew Gardens analyzed Mediterranean flora from Spain and Portugal", "Spain; Portugal; United Kingdom; Italy; Greece; Turkey; France",
  "Mesoamerican biodiversity studied at CIAT in Colombia", "Colombia; Mexico; Guatemala; Belize; Honduras; El Salvador; Nicaragua; Costa Rica; Panama",
  "CSIRO investigated Australasian species in Papua New Guinea", "Papua New Guinea; Australia; New Zealand",
  "Harvard researchers studied Californian fungi in the United States", "United States",
  "Max Planck Institute collaborated with Iranian scientists on fungal genomics", "Germany; Iran",
  "INRA studies in France examined Balkan fungal communities", "France; Greece; Bulgaria; Albania; Serbia; Bosnia and Herzegovina; Croatia",
  "Wageningen University research on Scandinavian mycorrhizae", "Netherlands; Sweden; Norway; Denmark; Finland; Iceland",
  "UNAM scientists in Mexico investigated Mesoamerican endophyte diversity", "Mexico"
)

combined_results <- detect_geographic_locations_batch(combined_tests$text)

for (i in 1:nrow(combined_tests)) {
  actual <- ifelse(is.na(combined_results$countries_detected[i]), "NONE", combined_results$countries_detected[i])
  expected <- combined_tests$expected_country[i]
  
  passed <- check_detection(actual, expected, combined_tests$text[i], "Combined Detection")
  
  cat(sprintf("%-60s %s\n", substr(combined_tests$text[i], 1, 60), 
              if(passed) "✓" else "✗"))
  if (!passed) {
    cat(sprintf("  Expected: %s | Got: %s\n", expected, actual))
  }
}

# Test 6: Coordinate Detection
cat("\n6. TESTING COORDINATE DETECTION:\n")
cat("=", rep("=", 70), "\n", sep = "")

coord_tests <- tribble(
  ~text, ~expected_coords,
  "Samples collected at 45.5°N, 122.3°W in Oregon", TRUE,
  "Field site located at 34°30'15\"N 118°45'30\"W", TRUE,
  "GPS reading: 51.5074N, 0.1278W for London", TRUE,
  "Study area: 35-40°N, 120-125°W along the coast", TRUE,
  "No coordinates provided in this abstract", FALSE,
  "Location: 9°58'S, 76°15'W in Peru", TRUE,
  "Sampling at 55.7558°N, 37.6176°E Moscow", TRUE,
  "Coordinates: 40.7128° N, 74.0060° W New York", TRUE,
  "Site at -33.8688, 151.2093 Sydney Australia", TRUE,
  "No GPS data available for this study", FALSE,
  "Position: 48°51'29\"N 2°21'03\"E Paris", TRUE,
  "Lat/Long: 35.6762° N, 139.6503° E Tokyo", TRUE,
  "No spatial coordinates recorded", FALSE,
  "Global study with no specific locations", FALSE,
  "Coordinates 41.9028° N, 12.4964° E Rome Italy", TRUE,
  "Location: 55°45'N, 37°37'E Moscow Russia", TRUE,
  "GPS: 39.9042°N, 116.4074°E Beijing", TRUE,
  "No coordinate information provided", FALSE
)

coord_results <- detect_geographic_locations_batch(coord_tests$text)

for (i in 1:nrow(coord_tests)) {
  actual <- coord_results$has_coordinates[i]
  expected <- coord_tests$expected_coords[i]
  
  passed <- actual == expected
  total_tests <- total_tests + 1
  
  if (passed) {
    passed_tests <- passed_tests + 1
    status <- "✓"
  } else {
    failed_tests <- failed_tests + 1
    status <- "✗"
  }
  
  test_results[[length(test_results) + 1]] <- list(
    category = "Coordinate Detection",
    test = coord_tests$text[i],
    expected = as.character(expected),
    actual = as.character(actual),
    status = if(passed) "✓ PASS" else "✗ FAIL"
  )
  
  cat(sprintf("%-60s %s\n", substr(coord_tests$text[i], 1, 60), status))
  if (!passed) {
    cat(sprintf("  Expected: %s | Got: %s\n", expected, actual))
  }
}

# Test 7: Additional Edge Cases and Complex Scenarios
cat("\n7. TESTING ADDITIONAL EDGE CASES:\n")
cat("=", rep("=", 70), "\n", sep = "")

edge_tests <- tribble(
  ~text, ~expected_country,
  "Paris, Texas has unique fungal communities", "United States",
  "Paris, France is a center for mycological research", "France",
  "Rome, Georgia hosts agricultural research", "Georgia",
  "Rome, Italy has ancient olive tree endophytes", "Italy",
  "London, Kentucky forests show diverse mycobiota", "NONE",
  "London, England research on fungal pathogens", "England",
  "Athens, Georgia climate affects fungal growth", "Georgia",
  "Athens, Greece studies Mediterranean fungi", "Greece",
  "Manchester, New Hampshire weather patterns", "NONE",
  "Manchester, England industrial revolution impacts", "England",
  "Berlin, Wisconsin local ecology studies", "NONE",
  "Berlin, Germany mycological congress", "Germany",
  "Vienna, Virginia historical collections", "NONE",
  "Vienna, Austria fungal taxonomy work", "Austria",
  "Oxford, Mississippi university research", "NONE",
  "Oxford, England academic publications", "England",
  "Cambridge, Massachusetts biotechnology research", "NONE",
  "Cambridge, England university studies", "England"
)

edge_results <- detect_geographic_locations_batch(edge_tests$text)

for (i in 1:nrow(edge_tests)) {
  actual <- ifelse(is.na(edge_results$countries_detected[i]), "NONE", edge_results$countries_detected[i])
  expected <- edge_tests$expected_country[i]
  
  passed <- check_detection(actual, expected, edge_tests$text[i], "Edge Cases")
  
  cat(sprintf("%-60s %s\n", substr(edge_tests$text[i], 1, 60), 
              if(passed) "✓" else "✗"))
  if (!passed) {
    cat(sprintf("  Expected: %s | Got: %s\n", expected, actual))
  }
}

# Summary Report
cat("\n")
cat("=" , rep("=", 80), "\n", sep = "")
cat("                           TEST SUMMARY REPORT\n")
cat("=", rep("=", 80), "\n", sep = "")
cat(sprintf("Total Tests:        %d\n", total_tests))
cat(sprintf("Passed:             %d (%.1f%%)\n", passed_tests, 100 * passed_tests / total_tests))
cat(sprintf("Failed:             %d (%.1f%%)\n", failed_tests, 100 * failed_tests / total_tests))
cat("=", rep("=", 80), "\n", sep = "")

# Breakdown by category
cat("\nRESULTS BY CATEGORY:\n")
cat("-", rep("-", 80), "\n", sep = "")

results_df <- bind_rows(test_results)
category_summary <- results_df %>%
  group_by(category) %>%
  summarise(
    total = n(),
    passed = sum(grepl("PASS", status)),
    failed = sum(grepl("FAIL", status)),
    pass_rate = 100 * passed / total,
    .groups = "drop"
  ) %>%
  arrange(desc(pass_rate))

for (i in 1:nrow(category_summary)) {
  cat(sprintf("%-40s %2d/%2d tests (%.1f%%) %s\n",
              category_summary$category[i],
              category_summary$passed[i],
              category_summary$total[i],
              category_summary$pass_rate[i],
              if(category_summary$pass_rate[i] == 100) "✓" else "✗"))
}

# Show failed tests
if (failed_tests > 0) {
  cat("\n")
  cat("FAILED TESTS:\n")
  cat("-", rep("-", 80), "\n", sep = "")
  
  failed_df <- results_df %>% filter(grepl("FAIL", status))
  
  for (i in 1:nrow(failed_df)) {
    cat(sprintf("\n%s: %s\n", failed_df$category[i], failed_df$test[i]))
    cat(sprintf("  Expected: %s\n", failed_df$expected[i]))
    cat(sprintf("  Got:      %s\n", failed_df$actual[i]))
  }
}

cat("\n")
cat("=", rep("=", 80), "\n", sep = "")
cat("TEST SUITE COMPLETED\n")
cat("=", rep("=", 80), "\n", sep = "")

# Return summary
invisible(list(
  total = total_tests,
  passed = passed_tests,
  failed = failed_tests,
  pass_rate = 100 * passed_tests / total_tests,
  results = results_df,
  category_summary = category_summary
))
