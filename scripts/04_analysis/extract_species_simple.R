# Simple Species Extraction for Classification Results
# B. Bock  
# July 30, 2ed: July 31, 2025 - Using new comprehensive search results
#
# Comprehensive script to extract:
# 1. Species information (plants and fungi)
# 2. Plant parts studied
# 3. Research methods (culture, microscopy, molecular)
# 4. Geographic locations: one thing I need to fix is niger is both a species name and a country.... the country niger is showing up as represented more than it is
#
# Data Source: Web of Science search conducted July 31, 2025
# Search Strategy: ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi")
#                 AND (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR 
#                      lycophyte* OR pteridophyte* OR algae OR green alga* OR macroalga* OR 
#                      cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
#
# Focuses on the weighted ensemble (89.8% accuracy) predictions

library(tidyverse)
library(tictoc)
library(janitor)

# Source the detection functions
source("scripts/04_analysis/optimized_taxa_detection.R") #Maybe remove the whole testing thing from that script bc its annoying when it runs every time.

# Load centralized reference data utilities
source("scripts/04_analysis/reference_data_utils.R")

cat("=== COMPREHENSIVE ENDOPHYTE EXTRACTION ===
")
cat("Processing weighted ensemble results (best ML performance)\n")
cat("Extracting: Species | Plant Parts | Methods | Geography\n\n")

# Define extraction functions ------------------------------------------------

# Function to detect research methods in abstracts (from visualize_taxa_results.R)
detect_research_methods <- function(text) {
  # Use centralized method keywords
  method_categories <- get_method_keywords()
  
  text_lower <- tolower(text)
  results <- sapply(names(method_categories), function(category) {
    keywords <- method_categories[[category]]
    matches <- sapply(keywords, function(keyword) {
      grepl(keyword, text_lower)
    })
    any(matches)
  })
  
  # Create summary
  methods_found <- names(results)[results]
  return(list(
    molecular = results["molecular"],
    culture_based = results["culture"],
    microscopy = results["microscopy"],
    methods_detected = if(length(methods_found) > 0) paste(methods_found, collapse = "; ") else NA
  ))
}

# Function to detect geographic locations
detect_geographic_locations <- function(text) {
  # Use centralized country classifications and geographic keywords
  all_countries <- get_all_countries()
  global_north_countries <- get_global_north_countries()
  global_south_countries <- get_global_south_countries()
  
  # Get geographic regions from centralized utility
  regions <- get_geographic_keywords()
  
  # Get country classifications from centralized utilities
  global_north_countries <- get_global_north_countries()
  global_south_countries <- get_global_south_countries()
  all_countries <- c(global_north_countries, global_south_countries)
  
  text_lower <- tolower(text)

  # Get continent and region keywords from centralized utilities
  continents <- get_continent_keywords()
  regions <- get_region_keywords()
  
  text_lower <- tolower(text)
  
  # Find countries and categorize with special handling for problematic names
  cleaned_countries <- sapply(all_countries, function(country) {
    if (country == "niger") {
      # Match only if "Niger" appears capitalized in original text or as "Republic of Niger"
      # Exclude if preceded by species-like terms
      grepl("\\bRepublic of Niger\\b", text, ignore.case = TRUE) ||
      (grepl("\\bNiger\\b", text) && !grepl("\\b(Aspergillus|Rhizopus|Penicillium|Fusarium|Alternaria|Cladosporium)\\s+niger\\b", text, ignore.case = TRUE))
    } else if (country == "turkey") {
      # Match only if "Turkey" appears capitalized and not as "turkey tail" or similar mushroom contexts
      grepl("\\bTurkey\\b", text) && 
      !grepl("\\b(turkey\\s+tail|trametes\\s+versicolor|bracket\\s+fungus|polypore|mushroom)\\b", text, ignore.case = TRUE) &&
      !grepl("\\bturkey\\s+(mushroom|fungus|fungi)\\b", text, ignore.case = TRUE)
    } else if (country == "chile") {
      # Match "Chile" (country) but not "chili" (pepper) - use capitalization
      grepl("\\bChile\\b", text) && !grepl("\\bchil[ei]\\s+(pepper|pod|sauce|spice)\\b", text, ignore.case = TRUE)
    } else if (country == "georgia") {
      # Match "Georgia" but prefer if it's clearly a country (with state/region context or capitalized)
      grepl("\\bGeorgia\\b", text) && !grepl("\\bgeorgia\\s+(pine|oak|southern)\\b", text, ignore.case = TRUE)
    } else if (country == "guinea") {
      # Match "Guinea" but not "guinea pig" contexts
      grepl("\\bGuinea\\b", text) && !grepl("\\bguinea\\s+pig\\b", text, ignore.case = TRUE)
    } else if (country == "mali") {
      # Match "Mali" but not in species contexts (some fungi have "mali" in names)
      grepl("\\bMali\\b", text) && !grepl("\\b\\w+\\s+mali\\b", text, ignore.case = TRUE)
    } else {
      # Standard lowercase matching for other countries
      grepl(paste0("\\b", country, "\\b"), text_lower)
    }
  })

  countries_found <- all_countries[cleaned_countries]
  
  country_presence <- setNames(as.integer(all_countries %in% countries_found), all_countries)

  # Categorize found countries
  north_countries <- intersect(countries_found, global_north_countries)
  south_countries <- intersect(countries_found, global_south_countries)
  
  # Find continents
  continents_found <- continents[sapply(continents, function(continent) {
    grepl(paste0("\\b", continent, "\\b"), text_lower)
  })]
  
  # Find regions/ecosystems
  regions_found <- regions[sapply(regions, function(region) {
    grepl(paste0("\\b", region, "\\b"), text_lower)
  })]
  
  # Look for coordinate patterns (latitude/longitude)
  coord_pattern <- "\\b\\d{1,2}[°]?\\s*[NS]?\\s*,?\\s*\\d{1,3}[°]?\\s*[EW]?\\b"
  has_coordinates <- grepl(coord_pattern, text)
  
  return(list(
  countries = if(length(countries_found) > 0) paste(countries_found, collapse = "; ") else NA,
  global_north_countries = if(length(north_countries) > 0) paste(north_countries, collapse = "; ") else NA,
  global_south_countries = if(length(south_countries) > 0) paste(south_countries, collapse = "; ") else NA,
  continents = if(length(continents_found) > 0) paste(continents_found, collapse = "; ") else NA,
  regions = if(length(regions_found) > 0) paste(regions_found, collapse = "; ") else NA,
  has_coordinates = has_coordinates,
  geographic_info = paste(c(countries_found, continents_found, regions_found), collapse = "; "),
  country_presence = country_presence
))
}

# Function to detect plant parts (enhanced from existing)
detect_plant_parts <- function(text) {
  # Use centralized plant parts keywords
  plant_parts_keywords <- get_plant_parts_keywords()
  
  text_lower <- tolower(text)
  parts_found <- plant_parts_keywords[sapply(plant_parts_keywords, function(part) {
    grepl(paste0("\\b", part, "\\b"), text_lower)
  })]
  
  return(list(
    plant_parts_detected = if(length(parts_found) > 0) paste(parts_found, collapse = "; ") else NA,
    parts_count = length(parts_found)
  ))
}

# Step 1: Prepare the data ------------------------------------------------

cat("Step 1: Preparing classification results for species detection...\n")

# Load the relevant abstracts with predictions
classification_results <- read_csv("results/relevant_abstracts_with_pa_predictions.csv")

library(tidyverse)
library(stringr)


# Helpful text column (combine title + abstract to increase recall)
classification_results <- classification_results %>%
  mutate(text_join = paste(article_title, abstract, sep = " - "),
         text_join = ifelse(is.na(text_join), "", text_join))

# Load and bind training dataset (only those with real DOIs)
cat("  Loading training dataset for inclusion...\n")
training_data_raw <- read_csv("data/raw/Training_labeled_abs_6.csv") %>%
  clean_names() %>%
  filter(!is.na(doi) & doi != "" & nchar(doi) > 5) %>%  # Only real DOIs
  filter(label %in% c("Presence", "Absence"))  # Only relevant labels

# Create training data that matches classification_results structure
training_data <- training_data_raw %>%
  select(article_title, abstract, authors, source_title, publication_year, doi, label) %>%
  mutate(
    id = max(classification_results$id, na.rm = TRUE) + row_number(),  # Unique IDs
    # Map to classification_results column names
    Relevant = ifelse(label == "Presence", TRUE, FALSE),
    relevance_loose = ifelse(label == "Presence", TRUE, FALSE),
    glmnet_pred = label,
    svm_pred = label,
    weighted_ensemble = label,
    threshold_ensemble = label,
    glmnet_prob_presence = ifelse(label == "Presence", 0.95, 0.05),
    glmnet_prob_absence = ifelse(label == "Absence", 0.95, 0.05),
    svm_prob_presence = ifelse(label == "Presence", 0.95, 0.05),
    svm_prob_absence = ifelse(label == "Absence", 0.95, 0.05),
    ensemble_presence_prob = ifelse(label == "Presence", 0.95, 0.05),
    ensemble_absence_prob = ifelse(label == "Absence", 0.95, 0.05),
    ensemble_prediction = ifelse(label == "Presence", TRUE, FALSE),
    pa_loose = ifelse(label == "Presence", TRUE, FALSE),
    pa_medium = ifelse(label == "Presence", TRUE, FALSE),
    pa_strict = ifelse(label == "Presence", TRUE, FALSE),
    pa_super_strict = ifelse(label == "Presence", TRUE, FALSE),
    final_classification = label,
    conservative_classification = label,
    source = "training"
  ) %>%
  # Select only the columns that exist in classification_results, plus source
  # use any_of() so missing columns in the training data don't cause an error
  select(any_of(names(classification_results)), source)

cat("  Added", nrow(training_data), "training abstracts with valid DOIs\n")

# Combine classification results with training data
combined_results <- classification_results %>%
  mutate(
    source = "classification",
    # Ensure consistent data types
    publication_year = as.character(publication_year),
    Relevant = as.character(Relevant),
    relevance_loose = as.character(relevance_loose),
    pa_loose = as.character(pa_loose),
    pa_medium = as.character(pa_medium),
    pa_strict = as.character(pa_strict),
    pa_super_strict = as.character(pa_super_strict),
    ensemble_prediction = as.character(ensemble_prediction)
  ) %>%
  bind_rows(
    training_data %>%
      mutate(
        # Ensure training data has consistent types
        publication_year = as.character(publication_year),
        Relevant = as.character(Relevant),
        relevance_loose = as.character(relevance_loose),
        pa_loose = as.character(pa_loose),
        pa_medium = as.character(pa_medium),
        pa_strict = as.character(pa_strict),
        pa_super_strict = as.character(pa_super_strict),
        ensemble_prediction = as.character(ensemble_prediction)
      )
  )

cat("  Combined dataset:", nrow(combined_results), "total abstracts\n")

# Focus on the weighted ensemble predictions (best performance) + training data
## Align with `apply_models_to_full_dataset.R`: prefer `final_classification` as the primary ensemble output
# If `final_classification` is missing, populate it from other ensemble columns produced by earlier scripts
if (!"final_classification" %in% names(combined_results)) {
  combined_results <- combined_results %>%
    mutate(final_classification = coalesce(weighted_ensemble, ensemble_prediction, threshold_ensemble, conservative_classification, glmnet_pred, svm_pred))
}

# Create ensemble presence/absence probability columns if not present, pulling from available model probs
if (!"ensemble_presence_prob" %in% names(combined_results)) {
  combined_results <- combined_results %>%
    mutate(ensemble_presence_prob = coalesce(ensemble_presence_prob, glmnet_prob_presence, svm_prob_presence),
           ensemble_absence_prob = coalesce(ensemble_absence_prob, glmnet_prob_absence, svm_prob_absence))
}

# Ensure model-specific probability columns exist (create as NA if missing) so downstream mutate() won't error
missing_prob_cols <- setdiff(c("glmnet_prob_presence", "glmnet_prob_absence", "svm_prob_presence", "svm_prob_absence"), names(combined_results))
if (length(missing_prob_cols) > 0) {
  combined_results[missing_prob_cols] <- NA_real_
}

# Ensure a combined text column exists on the combined dataset
if (!"text_join" %in% names(combined_results)) {
  combined_results <- combined_results %>%
    mutate(text_join = paste(article_title, abstract, sep = " - "),
           text_join = ifelse(is.na(text_join), "", text_join))
}

# Use `final_classification` as the canonical predicted label and keep metadata
abstracts_for_species <- combined_results %>%
  filter(!is.na(final_classification) & final_classification != "") %>%
  # Keep ALL columns from combined_results to preserve metadata
  rename(
    title = article_title,
    predicted_label = final_classification
  ) %>%
  # Add confidence score: prefer ensemble_presence_prob, fallback to model probs
  mutate(
    # Safely build presence/absence confidence by coalescing available probability columns
    presence_conf = coalesce(ensemble_presence_prob, glmnet_prob_presence, svm_prob_presence, NA_real_),
    absence_conf  = coalesce(ensemble_absence_prob,  glmnet_prob_absence,  svm_prob_absence,  NA_real_),
    confidence = pmax(presence_conf, absence_conf, na.rm = TRUE),
    confidence = ifelse(is.infinite(confidence), NA_real_, confidence)
  ) %>%
  # Remove temporary helper columns
  select(-any_of(c("presence_conf", "absence_conf")))

cat("  Prepared", nrow(abstracts_for_species), "abstracts with weighted ensemble predictions\n")

# Create breakdown by prediction type
prediction_summary <- abstracts_for_species %>%
  count(predicted_label, name = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 1))

cat("  Prediction breakdown:\n")
print(prediction_summary)

# Step 2: Run species detection -------------------------------------------

cat("\nStep 2: Running species detection...\n")

# Check if species detection results already exist (recovery mechanism)
species_file <- "results/species_detection_weighted_ensemble.csv"
if (file.exists(species_file)) {
  cat("⚠️  Found existing species detection results. Do you want to:\n")
  cat("   - Skip species detection and use existing results\n")
  cat("   - Or delete this file and rerun from scratch\n")
  cat("   File:", species_file, "\n")
  
  # For now, we'll load existing results and continue
  cat("   Loading existing species detection results...\n")
  all_species_results <- read_csv(species_file, show_col_types = FALSE)
  cat("   ✅ Loaded", nrow(all_species_results), "existing species detection records\n")
  
  # Skip to step 2.5
  skip_species_detection <- FALSE #Changing this right now to force it to run
} else {
  skip_species_detection <- FALSE
}

if (!skip_species_detection) {

# Load species reference data and set up processing
if (file.exists("species.rds")) {
  species <- readRDS("species.rds")
} else if (file.exists("models/species.rds")) {
  species <- readRDS("models/species.rds")
} else {
  stop("Species reference data not found. Please ensure species.rds exists.")
}

cat("  Loaded species reference data:", nrow(species), "species records\n")

# Set up parallel processing and lookup tables
setup_parallel(workers = 2)
lookup_tables <- create_lookup_tables(species)

# Get plant parts keywords from centralized utilities
plant_parts_keywords_species <- get_plant_parts_keywords()

# Process species detection in batches
tic("Species detection")

batch_size <- 25
n_batches <- ceiling(nrow(abstracts_for_species) / batch_size)

cat("Processing", nrow(abstracts_for_species), "abstracts in", n_batches, "batches...\n")

all_species_results <- map_dfr(1:n_batches, function(i) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(abstracts_for_species))
  
  batch_data <- abstracts_for_species[start_idx:end_idx, ]
  
  cat("  Processing batch", i, "of", n_batches, "(rows", start_idx, "to", end_idx, ")\n")
  
  # Process this batch
  batch_results <- process_abstracts_parallel(
    abstracts = batch_data,
    lookup_tables = lookup_tables,
    plant_parts_keywords = plant_parts_keywords_species,
    batch_size = 10,
    workers = 1
  )
  
  # Save intermediate results every 10 batches to prevent data loss
  if (i %% 10 == 0) {
    temp_file <- paste0("results/temp_species_batch_", i, ".csv")
    write_csv(batch_results, temp_file)
    cat("    ✅ Saved intermediate results to", temp_file, "\n")
  }
  
  return(batch_results)
})

# Save species detection results
write_csv(all_species_results, "results/species_detection_weighted_ensemble.csv")
cat("Species detection completed. Results saved to: results/species_detection_weighted_ensemble.csv\n")

toc()

} # End of species detection if-block

# Step 2.5: Extract additional information -----------------------------------

cat("\nStep 2.5: Extracting plant parts, methods, and geographic information...\n")
tic("Additional information extraction")

# Apply extraction functions to all abstracts
cat("  Extracting research methods...\n")
methods_results <- map_dfr(1:nrow(abstracts_for_species), function(i) {
  if (i %% 100 == 0) cat("    Processed", i, "of", nrow(abstracts_for_species), "abstracts\n")
  
  abstract_text <- abstracts_for_species$abstract[i]
  if (is.na(abstract_text)) abstract_text <- ""
  
  # Extract methods
  methods <- detect_research_methods(abstract_text)
  
  # Extract plant parts  
  plant_parts <- detect_plant_parts(abstract_text)
  
  # Extract geographic info
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

cat("  Additional information extraction completed\n")
toc()

# Step 3: Combine and analyze results -----------------------------------------

cat("\nStep 3: Combining species and additional information...\n")

if (file.exists("results/species_detection_weighted_ensemble.csv")) {
  
  # Load species detection results
  species_results <- read_csv("results/species_detection_weighted_ensemble.csv", show_col_types = FALSE)
  
  # Combine with additional extraction results
  comprehensive_results <- species_results %>%
    left_join(methods_results, by = "id") %>%
    # Add ALL original classification data and metadata
    left_join(
      abstracts_for_species %>% 
        # Select metadata columns that aren't already in species_results
        select(id, abstract, authors, source_title, publication_year, doi, 
               source, confidence, glmnet_prob_presence, glmnet_prob_absence,
               # Include any other prediction columns
               starts_with("glmnet_"), starts_with("svm_"), 
               starts_with("pa_"), contains("ensemble"), 
               contains("Relevant"), contains("classification")),
      by = "id"
    )
  
  # Save comprehensive results
  write_csv(comprehensive_results, "results/comprehensive_extraction_results.csv")
  
  # Calculate total unique abstracts for accurate reporting
  total_unique_abstracts <- length(unique(comprehensive_results$id))
  
  # Report on metadata preservation
  cat("Metadata check in comprehensive results:\n")
  metadata_cols <- c("abstract", "authors", "source_title", "publication_year", "doi", "source")
  for (col in metadata_cols) {
    if (col %in% names(comprehensive_results)) {
      # Count unique abstracts with non-NA values for this column
      non_na_count <- comprehensive_results %>%
        group_by(id) %>%
        summarise(has_data = any(!is.na(.data[[col]])), .groups = "drop") %>%
        pull(has_data) %>%
        sum()
      cat("  ✓", col, ":", non_na_count, "abstracts with data out of", total_unique_abstracts, "total\n")
    } else {
      cat("  ✗", col, ": MISSING from comprehensive results\n")
    }
  }
  
  # Also report total columns
  cat("  Total columns in comprehensive results:", ncol(comprehensive_results), "\n")
  cat("  Key sections: Species info, Plant parts, Methods, Geography, Metadata\n")
  
  cat("Analysis of comprehensive extraction results:\n")
  cat("  Total unique abstracts processed:", total_unique_abstracts, "\n")

  # Species analysis (check for actual species column names)
  species_columns <- c("resolved_name", "canonicalName", "acceptedScientificName")
  species_col <- species_columns[species_columns %in% names(comprehensive_results)][1]
  
  if (!is.na(species_col)) {
    # Count unique abstracts with species detected
    abstracts_with_species <- comprehensive_results %>%
      group_by(id) %>%
      summarise(has_species = any(!is.na(.data[[species_col]])), .groups = "drop") %>%
      pull(has_species) %>%
      sum()
    
    cat("  Abstracts with species detected:", abstracts_with_species, 
        "(", round(100 * abstracts_with_species / total_unique_abstracts, 1), "%)\n")

    # Unique species
    unique_species <- comprehensive_results %>%
      filter(!is.na(.data[[species_col]])) %>%
      distinct(.data[[species_col]]) %>%
      nrow()
  } else {
    cat("  No species column found in results\n")
    abstracts_with_species <- 0
    unique_species <- 0
  }
  
  cat("  Unique species detected:", unique_species, "\n")
  
  # Methods analysis - count unique abstracts
  methods_summary <- comprehensive_results %>%
    group_by(id) %>%
    summarise(
      molecular = any(molecular_methods, na.rm = TRUE),
      culture = any(culture_based_methods, na.rm = TRUE),
      microscopy = any(microscopy_methods, na.rm = TRUE),
      has_methods = any(!is.na(methods_summary) | molecular | culture | microscopy),
      .groups = "drop"
    ) %>%
    summarise(
      molecular = sum(molecular),
      culture = sum(culture),
      microscopy = sum(microscopy),
      total_with_methods = sum(has_methods)
    )
  
  cat("  Research methods detected:\n")
  cat("    Molecular methods:", methods_summary$molecular, "abstracts\n")
  cat("    Culture-based methods:", methods_summary$culture, "abstracts\n") 
  cat("    Microscopy methods:", methods_summary$microscopy, "abstracts\n")
  cat("    Abstracts with any method info:", methods_summary$total_with_methods, "\n")
  
  # Plant parts analysis - count unique abstracts
  abstracts_with_parts <- comprehensive_results %>%
    group_by(id) %>%
    summarise(has_parts = any(!is.na(plant_parts_detected)), .groups = "drop") %>%
    pull(has_parts) %>%
    sum()
  
  cat("  Plant parts information:\n")
  cat("    Abstracts with plant parts:", abstracts_with_parts, 
      "(", round(100 * abstracts_with_parts / total_unique_abstracts, 1), "%)\n")
  
  # Geographic analysis - count unique abstracts
  geographic_summary <- comprehensive_results %>%
    group_by(id) %>%
    summarise(
      with_countries = any(!is.na(countries_detected)),
      with_global_north = any(!is.na(global_north_countries)),
      with_global_south = any(!is.na(global_south_countries)),
      with_continents = any(!is.na(continents_detected)),
      with_regions = any(!is.na(regions_detected)),
      with_coordinates = any(has_coordinates, na.rm = TRUE),
      with_any_geography = any(!is.na(countries_detected) | !is.na(continents_detected) | !is.na(regions_detected)),
      .groups = "drop"
    ) %>%
    summarise(
      with_countries = sum(with_countries),
      with_global_north = sum(with_global_north),
      with_global_south = sum(with_global_south),
      with_continents = sum(with_continents),
      with_regions = sum(with_regions),
      with_coordinates = sum(with_coordinates),
      with_any_geography = sum(with_any_geography)
    )
  
  cat("  Geographic information:\n")
  cat("    Abstracts with countries:", geographic_summary$with_countries, "\n")
  cat("    Abstracts with Global North countries:", geographic_summary$with_global_north, "\n")
  cat("    Abstracts with Global South countries:", geographic_summary$with_global_south, "\n")
  cat("    Abstracts with continents:", geographic_summary$with_continents, "\n")
  cat("    Abstracts with regions:", geographic_summary$with_regions, "\n")
  cat("    Abstracts with coordinates:", geographic_summary$with_coordinates, "\n")
  cat("    Abstracts with any geographic info:", geographic_summary$with_any_geography, "\n")
  
  # Kingdom analysis (check for kingdom column)
  kingdom_columns <- c("kingdom", "kingdom.x", "kingdom.y")
  kingdom_col <- kingdom_columns[kingdom_columns %in% names(comprehensive_results)][1]
  
  if (!is.na(kingdom_col) && !is.na(species_col)) {
    kingdom_summary <- comprehensive_results %>%
      filter(!is.na(.data[[species_col]])) %>%
      count(.data[[kingdom_col]], name = "abstracts") %>%
      arrange(desc(abstracts))
  } else {
    kingdom_summary <- data.frame()
  }
  
  if (nrow(kingdom_summary) > 0) {
    cat("  Species by kingdom:\n")
    for (i in 1:nrow(kingdom_summary)) {
      kingdom_name <- kingdom_summary[[kingdom_col]][i]
      if (!is.na(kingdom_name)) {
        cat("    ", kingdom_name, ":", kingdom_summary$abstracts[i], "abstracts\n")
      }
    }
  }
  
  # Prediction type analysis - count unique abstracts per prediction type
  prediction_analysis <- comprehensive_results %>%
    group_by(id, predicted_label) %>%
    summarise(
      has_species = if(!is.na(species_col)) any(!is.na(.data[[species_col]])) else FALSE,
      molecular = any(molecular_methods, na.rm = TRUE),
      culture = any(culture_based_methods, na.rm = TRUE),
      microscopy = any(microscopy_methods, na.rm = TRUE),
      with_geography = any(!is.na(countries_detected) | !is.na(continents_detected) | !is.na(regions_detected)),
      .groups = "drop"
    ) %>%
    group_by(predicted_label) %>%
    summarise(
      total = n(),
      with_species = sum(has_species),
      molecular = sum(molecular),
      culture = sum(culture),
      microscopy = sum(microscopy),
      with_geography = sum(with_geography),
      .groups = "drop"
    )
  
  cat("  Analysis by prediction type:\n")
  print(prediction_analysis)
  
  # Create detailed summary report
  capture.output({
    cat("=== COMPREHENSIVE EXTRACTION SUMMARY REPORT ===\n")
    cat("Generated:", Sys.time(), "\n")
    cat("Input: Weighted ensemble predictions (89.8% ML accuracy)\n")
    cat("Extractions: Species, Plant Parts, Methods, Geography\n\n")
    
    cat("OVERVIEW:\n")
    cat("Total unique abstracts processed:", total_unique_abstracts, "\n")
    cat("Abstracts with species:", abstracts_with_species, 
        "(", round(100 * abstracts_with_species / total_unique_abstracts, 1), "%)\n")
    cat("Unique species detected:", unique_species, "\n\n")
    
    cat("RESEARCH METHODS:\n")
    cat("Molecular methods:", methods_summary$molecular, "abstracts\n")
    cat("Culture-based methods:", methods_summary$culture, "abstracts\n")
    cat("Microscopy methods:", methods_summary$microscopy, "abstracts\n")
    cat("Any method information:", methods_summary$total_with_methods, "abstracts\n\n")
    
    cat("PLANT PARTS:\n")
    cat("Abstracts with plant parts:", abstracts_with_parts, 
        "(", round(100 * abstracts_with_parts / total_unique_abstracts, 1), "%)\n\n")
    
    cat("GEOGRAPHIC INFORMATION:\n")
    cat("Countries mentioned:", geographic_summary$with_countries, "abstracts\n")
    cat("Global North countries:", geographic_summary$with_global_north, "abstracts\n")
    cat("Global South countries:", geographic_summary$with_global_south, "abstracts\n")
    cat("Continents mentioned:", geographic_summary$with_continents, "abstracts\n")
    cat("Regions/ecosystems mentioned:", geographic_summary$with_regions, "abstracts\n")
    cat("Coordinates provided:", geographic_summary$with_coordinates, "abstracts\n")
    cat("Any geographic info:", geographic_summary$with_any_geography, "abstracts\n\n")
    
    cat("SPECIES BY KINGDOM:\n")
    if (nrow(kingdom_summary) > 0) {
      for (i in 1:nrow(kingdom_summary)) {
        kingdom_name <- kingdom_summary[[kingdom_col]][i]
        if (!is.na(kingdom_name)) {
          cat(kingdom_name, ":", kingdom_summary$abstracts[i], "abstracts\n")
        }
      }
    }
    cat("\n")
    
    cat("ANALYSIS BY PREDICTION TYPE:\n")
    print(prediction_analysis)
    cat("\n")
    
    cat("DATA QUALITY INDICATORS:\n")
    cat("1. Species detection rate: ", round(100 * abstracts_with_species / total_unique_abstracts, 1), "%\n")
    cat("2. Method information coverage: ", round(100 * methods_summary$total_with_methods / total_unique_abstracts, 1), "%\n")
    cat("3. Plant parts coverage: ", round(100 * abstracts_with_parts / total_unique_abstracts, 1), "%\n")
    cat("4. Geographic coverage: ", round(100 * geographic_summary$with_any_geography / total_unique_abstracts, 1), "%\n\n")
    
    cat("RECOMMENDATIONS:\n")
    cat("1. Focus manual review on abstracts with species + methods + geography\n")
    cat("2. Prioritize 'Presence' predictions with comprehensive information\n")
    cat("3. Review 'Absence' predictions with species detected (potential misclassification)\n")
    cat("4. Use geographic and method information for study characterization\n")
    cat("5. Consider plant parts information for endophyte ecology analysis\n")
  }, file = "results/comprehensive_extraction_report.txt")
  
  cat("Comprehensive report saved to: results/comprehensive_extraction_report.txt\n")
  
} else {
  cat("No species detection results found - running methods and geography extraction only.\n")
  
  # Save just the additional extraction results
  write_csv(methods_results, "results/methods_geography_extraction.csv")
  cat("Methods and geography results saved to: results/methods_geography_extraction.csv\n")
}

cat("\n=== COMPREHENSIVE EXTRACTION COMPLETE ===\n")
cat("Key output files:\n")
cat("- results/comprehensive_extraction_results.csv: Complete results with all extractions\n")
cat("- results/species_detection_weighted_ensemble.csv: Species detection details\n") 
cat("- results/comprehensive_extraction_report.txt: Detailed analysis report\n\n")
cat("Data extracted:\n")
cat("✓ Species identification (plants and fungi)\n")
cat("✓ Plant parts studied (roots, leaves, stems, etc.)\n")
cat("✓ Research methods (molecular, culture, microscopy)\n")
cat("✓ Geographic locations (countries, regions, coordinates)\n\n")
cat("Next steps:\n")
cat("1. Review comprehensive_extraction_results.csv for systematic review data\n")
cat("2. Use geographic and method information for study characterization\n")
cat("3. Focus on abstracts with complete information for priority review\n")
cat("4. Consider running visualization scripts for deeper analysis\n\n")

cat("Comprehensive extraction pipeline complete! 🧬🔬🌍\n")
