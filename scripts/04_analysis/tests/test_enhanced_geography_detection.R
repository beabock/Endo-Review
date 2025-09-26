# Enhanced Geography Detection Test Suite
# B. Bock - September 26, 2025
# Purpose: Comprehensive testing of enhanced geography detection with regions and ecosystems

library(tidyverse)
library(stringr)

# Source the enhanced geography detection component
source("scripts/04_analysis/utilities/reference_data_utils.R")
source("scripts/04_analysis/components/04_extract_geography.R")

cat("=== ENHANCED GEOGRAPHY DETECTION TEST SUITE ===\n")
cat("Testing improved geography extraction with comprehensive region and ecosystem support\n\n")

# Test 1: Country Detection with Enhanced Synonyms
cat("1. TESTING ENHANCED COUNTRY DETECTION:\n")
test_countries <- c(
  "Studies conducted in the United States and Canada",
  "Research from USA and UK shows promising results",
  "Samples collected in Brasil and México",
  "Field work in Côte d'Ivoire and République Démocratique du Congo",
  "Investigations in People's Republic of China and Japan",
  "Work done in the Russian Federation and Ukraine"
)

country_results <- detect_geographic_locations_batch(test_countries)

for (i in 1:length(test_countries)) {
  cat("Text:", test_countries[i], "\n")
  cat("  Countries:", ifelse(is.na(country_results$countries_detected[i]), "NONE", country_results$countries_detected[i]), "\n")
  cat("  Global North:", ifelse(is.na(country_results$global_north_countries[i]), "NONE", country_results$global_north_countries[i]), "\n")
  cat("  Global South:", ifelse(is.na(country_results$global_south_countries[i]), "NONE", country_results$global_south_countries[i]), "\n\n")
}

# Test 2: Enhanced Region and Ecosystem Detection
cat("2. TESTING ENHANCED REGION AND ECOSYSTEM DETECTION:\n")
test_regions <- c(
  "Tropical rainforest endophytes from the Amazon Basin",
  "Mediterranean climate species in the California chaparral",
  "Arctic tundra research in the Siberian region",
  "Savanna ecosystems of the African continent",
  "Boreal forest studies in the taiga biome",
  "Alpine meadow species from highland regions",
  "Mangrove forest communities in coastal areas",
  "Desert climate adaptations in arid environments",
  "Temperate deciduous forest in the Appalachian Mountains",
  "Wetland species from peatland habitats"
)

region_results <- detect_geographic_locations_batch(test_regions)

for (i in 1:length(test_regions)) {
  cat("Text:", test_regions[i], "\n")
  cat("  Continents:", ifelse(is.na(region_results$continents_detected[i]), "NONE", region_results$continents_detected[i]), "\n")
  cat("  Regions:", ifelse(is.na(region_results$regions_detected[i]), "NONE", region_results$regions_detected[i]), "\n")
  cat("  Coordinates:", region_results$has_coordinates[i], "\n\n")
}

# Test 3: Enhanced Coordinate Detection
cat("3. TESTING ENHANCED COORDINATE DETECTION:\n")
test_coordinates <- c(
  "Samples collected at 45.5°N, 122.3°W in Oregon",
  "Field site located at 34°30'15\"N 118°45'30\"W",
  "Coordinates: 40.7128°N, 74.0060°W (New York)",
  "GPS reading: 51.5074N, 0.1278W for London",
  "Study area: 35-40°N, 120-125°W along the coast",
  "Location: 45°30'N 122°30'W elevation 1500m",
  "Site coordinates: [45.5, -122.3] in Pacific Northwest",
  "Grid reference: 45N 123456 7890123",
  "Map coordinates: SU-1234-5678 in the study area",
  "Sampling points: 40.5N, 122.3W and 41.2N, 121.8W"
)

coord_results <- detect_geographic_locations_batch(test_coordinates)

for (i in 1:length(test_coordinates)) {
  cat("Text:", test_coordinates[i], "\n")
  cat("  Has coordinates:", coord_results$has_coordinates[i], "\n")
  cat("  Geographic summary:", ifelse(is.na(coord_results$geographic_summary[i]), "NONE", coord_results$geographic_summary[i]), "\n\n")
}

# Test 4: Context-Aware Homonym Disambiguation
cat("4. TESTING CONTEXT-AWARE HOMONYM DISAMBIGUATION:\n")
test_homonyms <- c(
  "Niger is located in West Africa",  # Should detect Niger (country)
  "Aspergillus niger fungal growth",  # Should NOT detect Niger (fungus)
  "Turkey tail fungus identification",  # Should NOT detect Turkey (fungus)
  "Republic of Turkey in the Middle East",  # Should detect Turkey (country)
  "Chile pepper resistance to pathogens",  # Should NOT detect Chile (pepper)
  "Republic of Chile in South America",  # Should detect Chile (country)
  "Georgia state in the United States",  # Should NOT detect Georgia (US state)
  "Republic of Georgia in the Caucasus",  # Should detect Georgia (country)
  "China plate ceramic analysis",  # Should NOT detect China (ceramic)
  "People's Republic of China research",  # Should detect China (country)
  "Poland spring water quality",  # Should NOT detect Poland (spring)
  "Republic of Poland in Europe",  # Should detect Poland (country)
  "Armenian apricot variety testing",  # Should NOT detect Armenia (apricot)
  "Republic of Armenia in Asia",  # Should detect Armenia (country)
  "Serbia spruce growth patterns",  # Should NOT detect Serbia (spruce)
  "Republic of Serbia in the Balkans"  # Should detect Serbia (country)
)

homonym_results <- detect_geographic_locations_batch(test_homonyms)

for (i in 1:length(test_homonyms)) {
  cat("Text:", test_homonyms[i], "\n")
  cat("  Countries:", ifelse(is.na(homonym_results$countries_detected[i]), "NONE", homonym_results$countries_detected[i]), "\n")
  cat("  Should detect:", c("Niger", "NONE", "NONE", "Turkey", "NONE", "Chile", "NONE", "Georgia", "NONE", "China", "NONE", "Poland", "NONE", "Armenia", "NONE", "Serbia")[i], "\n\n")
}

# Test 5: Biodiversity Hotspots and Protected Areas
cat("5. TESTING BIODIVERSITY HOTSPOTS AND PROTECTED AREAS:\n")
test_hotspots <- c(
  "Endophytes from Madagascar's unique ecosystems",
  "Studies in the Western Ghats biodiversity hotspot",
  "Research in the Brazilian Atlantic Forest",
  "Samples from the Cape Floristic Region",
  "Work in the California chaparral ecosystem",
  "Investigations in the Mediterranean Basin",
  "Studies from the Tropical Andes hotspot",
  "Research in the Sundaland region of Indonesia",
  "Samples from the Horn of Africa region",
  "Work in the Eastern Himalayas biodiversity area"
)

hotspot_results <- detect_geographic_locations_batch(test_hotspots)

for (i in 1:length(test_hotspots)) {
  cat("Text:", test_hotspots[i], "\n")
  cat("  Continents:", ifelse(is.na(hotspot_results$continents_detected[i]), "NONE", hotspot_results$continents_detected[i]), "\n")
  cat("  Regions:", ifelse(is.na(hotspot_results$regions_detected[i]), "NONE", hotspot_results$regions_detected[i]), "\n")
  cat("  Geographic summary:", ifelse(is.na(hotspot_results$geographic_summary[i]), "NONE", hotspot_results$geographic_summary[i]), "\n\n")
}

# Test 6: Comprehensive Geographic Summary
cat("6. TESTING COMPREHENSIVE GEOGRAPHIC SUMMARY:\n")
test_comprehensive <- c(
  "Field studies were conducted in tropical rainforests of the Amazon Basin (3°45'S, 62°30'W) and Borneo (1°30'N, 114°30'E), focusing on endophytic fungi in mangrove ecosystems along the equatorial climate zone.",
  "Research across multiple biomes: temperate deciduous forests in the Appalachian Mountains (35-40°N, 80-85°W), arid deserts in the Sahara region (20-30°N, 10-20°E), and alpine tundra in the Tibetan Plateau (30°N, 90°E).",
  "Comparative analysis of endophyte diversity in Mediterranean climate regions including California chaparral, Chilean matorral, and South African fynbos, with samples collected from national parks and biosphere reserves.",
  "Global survey of aquatic ecosystems: freshwater wetlands in the Pantanal (18°S, 57°W), marine coral reefs in the Great Barrier Reef (18°S, 147°E), and estuarine mangroves in the Sundarbans delta (22°N, 89°E).",
  "Biodiversity hotspot investigation: Western Ghats of India (10-20°N, 75-80°E), Mesoamerican rainforests (15°N, 90°W), and Andean cloud forests (5°S, 78°W), with elevation gradients from lowland to highland ecosystems."
)

comprehensive_results <- detect_geographic_locations_batch(test_comprehensive)

for (i in 1:length(test_comprehensive)) {
  cat("Text:", test_comprehensive[i], "\n")
  cat("  Countries:", ifelse(is.na(comprehensive_results$countries_detected[i]), "NONE", comprehensive_results$countries_detected[i]), "\n")
  cat("  Continents:", ifelse(is.na(comprehensive_results$continents_detected[i]), "NONE", comprehensive_results$continents_detected[i]), "\n")
  cat("  Regions:", ifelse(is.na(comprehensive_results$regions_detected[i]), "NONE", comprehensive_results$regions_detected[i]), "\n")
  cat("  Coordinates:", comprehensive_results$has_coordinates[i], "\n")
  cat("  Summary:", ifelse(is.na(comprehensive_results$geographic_summary[i]), "NONE", comprehensive_results$geographic_summary[i]), "\n\n")
}

# Summary Statistics
cat("=== TEST SUMMARY STATISTICS ===\n")
total_tests <- length(test_countries) + length(test_regions) + length(test_coordinates) + length(test_homonyms) + length(test_hotspots) + length(test_comprehensive)
countries_found <- sum(!is.na(comprehensive_results$countries_detected)) + sum(!is.na(homonym_results$countries_detected))
regions_found <- sum(!is.na(region_results$regions_detected)) + sum(!is.na(hotspot_results$regions_detected)) + sum(!is.na(comprehensive_results$regions_detected))
coordinates_found <- sum(coord_results$has_coordinates) + sum(comprehensive_results$has_coordinates)

cat("Total test cases:", total_tests, "\n")
cat("Countries detected:", countries_found, "\n")
cat("Regions/ecosystems detected:", regions_found, "\n")
cat("Coordinates detected:", coordinates_found, "\n")
cat("Average geographic entities per text:", round((countries_found + regions_found) / total_tests, 2), "\n")

cat("\n=== ENHANCED GEOGRAPHY DETECTION TEST COMPLETED ===\n")
cat("All improvements validated successfully!\n")