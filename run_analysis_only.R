# Quick Analysis Script for Existing Species Detection Results
# Fixes the issues with redundant columns and missing analysis

setwd("c:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")
library(tidyverse)
library(tictoc)
library(janitor)

cat("=== LOADING EXISTING RESULTS FOR ANALYSIS ===\n")

# Load existing species detection results
species_results <- read_csv("results/species_detection_weighted_ensemble.csv", show_col_types = FALSE)
cat("Loaded", nrow(species_results), "species detection records\n")

# Load classification results for additional data
classification_results <- read_csv("results/predictions/relevant_abstracts_with_pa_predictions.csv")

# Prepare abstracts data (simplified)
abstracts_for_species <- classification_results %>%
  filter(!is.na(weighted_ensemble)) %>%
  select(
    id, article_title, abstract, authors, source_title, 
    publication_year, doi, weighted_ensemble,
    glmnet_prob_presence, glmnet_prob_absence
  ) %>%
  rename(
    title = article_title,
    predicted_label = weighted_ensemble
  ) %>%
  mutate(
    confidence = pmax(glmnet_prob_presence, glmnet_prob_absence, na.rm = TRUE)
  )

cat("Prepared", nrow(abstracts_for_species), "abstracts for analysis\n")

# Define extraction functions (simplified versions)
detect_research_methods <- function(text) {
  method_categories <- list(
    molecular = c("pcr", "dna", "rna", "sequenc", "primer", "amplif", "gene", "genom", 
                 "transcript", "clone", "phylogen", "molecular", "extraction", "isolat", 
                 "genetic", "marker", "polymorphism", "nucleotide", "hybridiz", "its", 
                 "rrna", "18s", "28s", "rdna", "barcode", "phylogeny"),
    
    culture_based = c("culture", "isolat", "plate", "medium", "agar", "petri", "colony", 
                     "incubat", "inocul", "sterile", "aseptic", "axenic", "pure culture", 
                     "ferment", "broth", "in vitro", "cultivation", "cultured"),
    
    microscopy = c("microscop", "stain", "section", "histolog", "morpholog", "ultrastructur", 
                  "sem", "tem", "scanning electron", "transmission electron", "light microscop", 
                  "confocal", "fluorescen", "magnification", "micrograph", "optical")
  )
  
  if (is.na(text)) text <- ""
  text_lower <- tolower(text)
  
  results <- sapply(names(method_categories), function(category) {
    keywords <- method_categories[[category]]
    matches <- sapply(keywords, function(keyword) grepl(keyword, text_lower))
    any(matches)
  })
  
  methods_found <- names(results)[results]
  return(list(
    molecular = results["molecular"],
    culture_based = results["culture_based"],
    microscopy = results["microscopy"],
    methods_detected = if(length(methods_found) > 0) paste(methods_found, collapse = "; ") else NA
  ))
}

detect_geographic_locations <- function(text) {
  global_north_countries <- c(
    "usa", "united states", "america", "canada", "greenland",
    "germany", "france", "italy", "spain", "uk", "united kingdom", "england", 
    "scotland", "wales", "ireland", "poland", "sweden", "norway", "finland",
    "denmark", "netherlands", "belgium", "switzerland", "austria", "portugal",
    "greece", "czech republic", "hungary", "slovakia", "slovenia", "estonia",
    "latvia", "lithuania", "iceland", "luxembourg", "malta", "cyprus",
    "croatia", "romania", "bulgaria", "albania", "montenegro", "serbia",
    "bosnia and herzegovina", "macedonia", "north macedonia", "moldova",
    "belarus", "ukraine", "japan", "south korea", "australia", "new zealand", 
    "singapore", "israel", "brunei", "russia"
  )
  
  global_south_countries <- c(
    "china", "india", "indonesia", "pakistan", "bangladesh", "vietnam", 
    "philippines", "thailand", "malaysia", "myanmar", "cambodia", "laos",
    "sri lanka", "nepal", "bhutan", "afghanistan", "mongolia", "north korea",
    "taiwan", "hong kong", "macao", "maldives", "timor-leste", "east timor",
    "brazil", "mexico", "argentina", "colombia", "peru", "venezuela", "chile",
    "ecuador", "bolivia", "paraguay", "uruguay", "nigeria", "ethiopia", "egypt", 
    "south africa", "kenya", "uganda", "tanzania", "ghana", "mozambique"
  )
  
  all_countries <- c(global_north_countries, global_south_countries)
  continents <- c("africa", "asia", "europe", "north america", "south america", 
                  "australia", "oceania", "antarctica")
  
  if (is.na(text)) text <- ""
  text_lower <- tolower(text)
  
  countries_found <- all_countries[sapply(all_countries, function(country) {
    grepl(paste0("\\b", country, "\\b"), text_lower)
  })]
  
  north_countries <- intersect(countries_found, global_north_countries)
  south_countries <- intersect(countries_found, global_south_countries)
  
  continents_found <- continents[sapply(continents, function(continent) {
    grepl(paste0("\\b", continent, "\\b"), text_lower)
  })]
  
  regions <- c("mediterranean", "tropical", "temperate", "boreal", "arctic", "alpine", 
               "coastal", "mountain", "desert", "rainforest", "savanna", "grassland",
               "wetland", "forest", "woodland", "prairie", "steppe", "tundra", "taiga")
  
  regions_found <- regions[sapply(regions, function(region) {
    grepl(paste0("\\b", region, "\\b"), text_lower)
  })]
  
  coord_pattern <- "\\b\\d{1,2}[°]?\\s*[NS]?\\s*,?\\s*\\d{1,3}[°]?\\s*[EW]?\\b"
  has_coordinates <- grepl(coord_pattern, text)
  
  return(list(
    countries = if(length(countries_found) > 0) paste(countries_found, collapse = "; ") else NA,
    global_north_countries = if(length(north_countries) > 0) paste(north_countries, collapse = "; ") else NA,
    global_south_countries = if(length(south_countries) > 0) paste(south_countries, collapse = "; ") else NA,
    continents = if(length(continents_found) > 0) paste(continents_found, collapse = "; ") else NA,
    regions = if(length(regions_found) > 0) paste(regions_found, collapse = "; ") else NA,
    has_coordinates = has_coordinates,
    geographic_info = paste(c(countries_found, continents_found, regions_found), collapse = "; ")
  ))
}

detect_plant_parts <- function(text) {
  plant_parts_keywords <- c(
    "fruit", "fruits", "root", "roots", "leaf", "leaves", "stem", "stems", 
    "flower", "flowers", "seed", "seeds", "bark", "branch", "branches",
    "twig", "twigs", "shoot", "shoots", "bud", "buds", "trunk", "trunks"
  )
  
  if (is.na(text)) text <- ""
  text_lower <- tolower(text)
  
  parts_found <- plant_parts_keywords[sapply(plant_parts_keywords, function(part) {
    grepl(paste0("\\b", part, "\\b"), text_lower)
  })]
  
  return(list(
    plant_parts_detected = if(length(parts_found) > 0) paste(parts_found, collapse = "; ") else NA,
    parts_count = length(parts_found)
  ))
}

# Extract additional information
cat("Extracting methods, plant parts, and geography...\n")
tic("Additional extraction")

methods_results <- map_dfr(1:nrow(abstracts_for_species), function(i) {
  if (i %% 1000 == 0) cat("  Processed", i, "of", nrow(abstracts_for_species), "abstracts\n")
  
  abstract_text <- abstracts_for_species$abstract[i]
  
  methods <- detect_research_methods(abstract_text)
  plant_parts <- detect_plant_parts(abstract_text)
  geography <- detect_geographic_locations(abstract_text)
  
  return(data.frame(
    id = abstracts_for_species$id[i],
    molecular_methods = methods$molecular,
    culture_based_methods = methods$culture_based,
    microscopy_methods = methods$microscopy,
    methods_summary = methods$methods_detected,
    plant_parts_detected = plant_parts$plant_parts_detected,
    parts_count = plant_parts$parts_count,
    countries_detected = geography$countries,
    global_north_countries = geography$global_north_countries,
    global_south_countries = geography$global_south_countries,
    continents_detected = geography$continents,
    regions_detected = geography$regions,
    has_coordinates = geography$has_coordinates,
    geographic_summary = geography$geographic_info
  ))
})

toc()

# Combine results
cat("Combining results...\n")
comprehensive_results <- species_results %>%
  left_join(methods_results, by = "id") %>%
  left_join(
    abstracts_for_species %>% 
      select(id, confidence, glmnet_prob_presence, glmnet_prob_absence),
    by = "id"
  )

# Clean up redundant plant parts columns from species detection
# Keep only the essential columns
essential_columns <- c(
  "user_supplied_name", "canonicalName", "kingdom", "phylum", "family", "genus",
  "resolved_name", "status", "acceptedScientificName", "synonymName", "acceptedName",
  "id", "predicted_label", "match_type",
  # New extraction results
  "molecular_methods", "culture_based_methods", "microscopy_methods", "methods_summary",
  "plant_parts_detected", "parts_count", "countries_detected", "global_north_countries",
  "global_south_countries", "continents_detected", "regions_detected", "has_coordinates",
  "geographic_summary", "confidence", "glmnet_prob_presence", "glmnet_prob_absence"
)

# Select only essential columns that exist
available_columns <- intersect(essential_columns, names(comprehensive_results))
comprehensive_results_clean <- comprehensive_results %>%
  select(all_of(available_columns))

# Save cleaned comprehensive results
write_csv(comprehensive_results_clean, "results/comprehensive_extraction_results.csv")

# Generate analysis
cat("\n=== GENERATING ANALYSIS ===\n")
cat("Total abstracts processed:", nrow(comprehensive_results_clean), "\n")

# Check for species columns
species_columns <- c("resolved_name", "canonicalName", "acceptedScientificName")
species_col <- species_columns[species_columns %in% names(comprehensive_results_clean)][1]

if (!is.na(species_col)) {
  abstracts_with_species <- comprehensive_results_clean %>%
    filter(!is.na(.data[[species_col]])) %>%
    nrow()
  
  unique_species <- comprehensive_results_clean %>%
    filter(!is.na(.data[[species_col]])) %>%
    distinct(.data[[species_col]]) %>%
    nrow()
  
  cat("Abstracts with species:", abstracts_with_species, 
      "(", round(100 * abstracts_with_species / nrow(comprehensive_results_clean), 1), "%)\n")
  cat("Unique species:", unique_species, "\n")
} else {
  cat("No species column found\n")
  abstracts_with_species <- 0
  unique_species <- 0
}

# Methods analysis
methods_summary <- comprehensive_results_clean %>%
  summarise(
    molecular = sum(molecular_methods, na.rm = TRUE),
    culture = sum(culture_based_methods, na.rm = TRUE),
    microscopy = sum(microscopy_methods, na.rm = TRUE),
    total_with_methods = sum(!is.na(methods_summary))
  )

cat("Methods detected:\n")
cat("  Molecular:", methods_summary$molecular, "\n")
cat("  Culture-based:", methods_summary$culture, "\n")
cat("  Microscopy:", methods_summary$microscopy, "\n")
cat("  Any method info:", methods_summary$total_with_methods, "\n")

# Plant parts analysis
abstracts_with_parts <- comprehensive_results_clean %>%
  filter(!is.na(plant_parts_detected)) %>%
  nrow()

cat("Plant parts information:\n")
cat("  Abstracts with plant parts:", abstracts_with_parts, 
    "(", round(100 * abstracts_with_parts / nrow(comprehensive_results_clean), 1), "%)\n")

# Geographic analysis
geographic_summary <- comprehensive_results_clean %>%
  summarise(
    with_countries = sum(!is.na(countries_detected)),
    with_global_north = sum(!is.na(global_north_countries)),
    with_global_south = sum(!is.na(global_south_countries)),
    with_continents = sum(!is.na(continents_detected)),
    with_regions = sum(!is.na(regions_detected)),
    with_coordinates = sum(has_coordinates, na.rm = TRUE),
    with_any_geography = sum(!is.na(countries_detected) | !is.na(continents_detected) | !is.na(regions_detected))
  )

cat("Geographic info:\n")
cat("  Countries:", geographic_summary$with_countries, "\n")
cat("  Global North:", geographic_summary$with_global_north, "\n")
cat("  Global South:", geographic_summary$with_global_south, "\n")
cat("  Continents:", geographic_summary$with_continents, "\n")
cat("  Regions:", geographic_summary$with_regions, "\n")
cat("  Coordinates:", geographic_summary$with_coordinates, "\n")
cat("  Any geographic info:", geographic_summary$with_any_geography, "\n")

# Kingdom analysis
kingdom_columns <- c("kingdom", "kingdom.x", "kingdom.y")
kingdom_col <- kingdom_columns[kingdom_columns %in% names(comprehensive_results_clean)][1]

if (!is.na(kingdom_col) && !is.na(species_col)) {
  kingdom_summary <- comprehensive_results_clean %>%
    filter(!is.na(.data[[species_col]])) %>%
    count(.data[[kingdom_col]], name = "abstracts") %>%
    arrange(desc(abstracts))
  
  cat("Species by kingdom:\n")
  for (i in 1:min(nrow(kingdom_summary), 10)) {
    kingdom_name <- kingdom_summary[[kingdom_col]][i]
    if (!is.na(kingdom_name)) {
      cat("  ", kingdom_name, ":", kingdom_summary$abstracts[i], "\n")
    }
  }
}

# Prediction analysis
prediction_analysis <- comprehensive_results_clean %>%
  group_by(predicted_label) %>%
  summarise(
    total = n(),
    with_species = if(!is.na(species_col)) sum(!is.na(.data[[species_col]])) else 0,
    molecular = sum(molecular_methods, na.rm = TRUE),
    culture = sum(culture_based_methods, na.rm = TRUE),
    microscopy = sum(microscopy_methods, na.rm = TRUE),
    with_geography = sum(!is.na(countries_detected) | !is.na(continents_detected) | !is.na(regions_detected)),
    .groups = "drop"
  )

cat("Analysis by prediction type:\n")
print(prediction_analysis)

# Create summary report
capture.output({
  cat("=== COMPREHENSIVE EXTRACTION SUMMARY REPORT ===\n")
  cat("Generated:", Sys.time(), "\n\n")
  
  cat("OVERVIEW:\n")
  cat("Total abstracts processed:", nrow(comprehensive_results_clean), "\n")
  cat("Abstracts with species:", abstracts_with_species, 
      "(", round(100 * abstracts_with_species / nrow(comprehensive_results_clean), 1), "%)\n")
  cat("Unique species detected:", unique_species, "\n\n")
  
  cat("RESEARCH METHODS:\n")
  cat("Molecular methods:", methods_summary$molecular, "abstracts\n")
  cat("Culture-based methods:", methods_summary$culture, "abstracts\n")
  cat("Microscopy methods:", methods_summary$microscopy, "abstracts\n")
  cat("Any method information:", methods_summary$total_with_methods, "abstracts\n\n")
  
  cat("PLANT PARTS:\n")
  cat("Abstracts with plant parts:", abstracts_with_parts, 
      "(", round(100 * abstracts_with_parts / nrow(comprehensive_results_clean), 1), "%)\n\n")
  
  cat("GEOGRAPHIC INFORMATION:\n")
  cat("Countries mentioned:", geographic_summary$with_countries, "abstracts\n")
  cat("Global North countries:", geographic_summary$with_global_north, "abstracts\n")
  cat("Global South countries:", geographic_summary$with_global_south, "abstracts\n")
  cat("Continents mentioned:", geographic_summary$with_continents, "abstracts\n")
  cat("Regions mentioned:", geographic_summary$with_regions, "abstracts\n")
  cat("Coordinates provided:", geographic_summary$with_coordinates, "abstracts\n")
  cat("Any geographic info:", geographic_summary$with_any_geography, "abstracts\n\n")
  
  if (!is.na(kingdom_col) && !is.na(species_col)) {
    cat("SPECIES BY KINGDOM:\n")
    for (i in 1:nrow(kingdom_summary)) {
      kingdom_name <- kingdom_summary[[kingdom_col]][i]
      if (!is.na(kingdom_name)) {
        cat(kingdom_name, ":", kingdom_summary$abstracts[i], "abstracts\n")
      }
    }
    cat("\n")
  }
  
  cat("ANALYSIS BY PREDICTION TYPE:\n")
  print(prediction_analysis)
  cat("\n")
  
  cat("DATA QUALITY INDICATORS:\n")
  cat("1. Species detection rate:", round(100 * abstracts_with_species / nrow(comprehensive_results_clean), 1), "%\n")
  cat("2. Method information coverage:", round(100 * methods_summary$total_with_methods / nrow(comprehensive_results_clean), 1), "%\n")
  cat("3. Plant parts coverage:", round(100 * abstracts_with_parts / nrow(comprehensive_results_clean), 1), "%\n")
  cat("4. Geographic coverage:", round(100 * geographic_summary$with_any_geography / nrow(comprehensive_results_clean), 1), "%\n\n")
  
  cat("RECOMMENDATIONS:\n")
  cat("1. Focus manual review on abstracts with species + methods + geography\n")
  cat("2. Prioritize 'Presence' predictions with comprehensive information\n")
  cat("3. Review 'Absence' predictions with species detected (potential misclassification)\n")
  cat("4. Use geographic and method information for study characterization\n")
  cat("5. Consider plant parts information for endophyte ecology analysis\n")
}, file = "results/comprehensive_extraction_report.txt")

cat("\n=== EXTRACTION COMPLETE ===\n")
cat("Files created:\n")
cat("- results/comprehensive_extraction_results.csv (cleaned, no redundant columns)\n")
cat("- results/comprehensive_extraction_report.txt\n")

cat("\nData extracted:\n")
cat("✓ Species identification (plants and fungi)\n")
cat("✓ Plant parts studied (roots, leaves, stems, etc.)\n")
cat("✓ Research methods (molecular, culture, microscopy)\n")
cat("✓ Geographic locations (countries, regions, coordinates)\n")
cat("✓ Removed redundant plant parts columns from species detection\n")
cat("✓ Fixed column naming issues\n\n")

cat("Summary statistics:\n")
cat("- Total records:", nrow(comprehensive_results_clean), "\n")
cat("- With species:", abstracts_with_species, "\n")
cat("- With methods:", methods_summary$total_with_methods, "\n")
cat("- With geography:", geographic_summary$with_any_geography, "\n")
cat("- With plant parts:", abstracts_with_parts, "\n")
