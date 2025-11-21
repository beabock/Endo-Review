# Simple Species Extraction for Classification Results
# B. Bock
# July 30, 2ed: July 31, 2025 - Using new comprehensive search results
# MODULAR VERSION: Run individual components or use pipeline script
#
# This script can be run in parts:
# 1. Species information (plants and fungi) - MOST TIME-CONSUMING (~1-2 days)
# 2. Plant parts studied - FAST (~10-30 min)
# 3. Research methods - FAST (~10-30 min)
# 4. Geographic locations - FAST (~10-30 min)
#
# For production use, consider running components separately or using the pipeline script
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
source("scripts/04_analysis/components/optimized_taxa_detection.R") #Maybe remove the whole testing thing from that script bc its annoying when it runs every time.

# Load centralized reference data utilities
source("scripts/04_analysis/utilities/reference_data_utils.R")

cat("=== COMPREHENSIVE ENDOPHYTE EXTRACTION ===
")
cat("Processing weighted ensemble results (best ML performance)\n")
cat("Extracting: Species | Plant Parts | Methods | Geography\n\n")

# Define extraction functions ------------------------------------------------

# OPTIMIZATION: Enhanced lookup table creation function
create_lookup_tables_optimized <- function(species_df) {
  # Call the original function
  lookup_tables <- create_lookup_tables(species_df)
  
  # Add performance optimizations
  if (!is.null(lookup_tables$accepted_species)) {
    lookup_tables$species_names_vector <- lookup_tables$accepted_species$canonicalName_lower
  }
  if (!is.null(lookup_tables$genus_list)) {
    lookup_tables$genus_names_vector <- lookup_tables$genus_list$canonicalName_lower
  }
  if (!is.null(lookup_tables$family_list)) {
    lookup_tables$family_names_vector <- lookup_tables$family_list$canonicalName_lower
  }
  
  # Create hash tables for O(1) lookups if dataset is large
  if (nrow(species_df) > 10000) {
    if (!is.null(lookup_tables$species_names_vector)) {
      lookup_tables$species_hash <- setNames(rep(TRUE, length(lookup_tables$species_names_vector)), 
                                            lookup_tables$species_names_vector)
    }
    if (!is.null(lookup_tables$genus_names_vector)) {
      lookup_tables$genus_hash <- setNames(rep(TRUE, length(lookup_tables$genus_names_vector)), 
                                          lookup_tables$genus_names_vector)
    }
  }
  
  return(lookup_tables)
}


# OPTIMIZED: Vectorized function to detect research methods in abstracts
detect_research_methods_batch <- function(text_vector) {
  method_categories <- get_method_keywords()
  
  # Pre-compile patterns for better performance
  patterns <- map(method_categories, ~paste(., collapse = "|"))
  
  text_lower <- str_to_lower(text_vector)
  
  results <- map_dfc(names(patterns), function(category) {
    matches <- str_detect(text_lower, patterns[[category]])
    setNames(list(matches), category)
  })
  
  # Create methods summary
  methods_detected <- pmap_chr(results, function(...) {
    found <- names(list(...))[unlist(list(...))]
    if(length(found) > 0) paste(found, collapse = "; ") else NA_character_
  })
  
  return(results %>% 
    rename(molecular = molecular, culture_based = culture, microscopy = microscopy) %>%
    mutate(methods_summary = methods_detected))
}

# Original function for backwards compatibility
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

# Enhanced geographic detection with comprehensive synonym handling and context-aware disambiguation
detect_geographic_locations_batch <- function(text_vector) {
  # Get comprehensive reference data
  all_countries <- get_all_countries()
  global_north <- get_global_north_countries()
  global_south <- get_global_south_countries()
  continents <- get_continent_keywords()
  regions <- get_region_keywords()

  # Function to normalize country names using enhanced synonym system
  normalize_country_batch <- function(country_vector) {
    # Apply the enhanced standardize_country_name function to each country
    map_chr(country_vector, function(country) {
      if (is.na(country) || country == "") return(NA_character_)
      result <- standardize_country_name(country)
      # If standardization returns the same as input, it might be unrecognized
      if (result == country && !country %in% all_countries) {
        return(NA_character_)  # Mark as unrecognized
      }
      return(result)
    })
  }

  # Context-aware country detection with enhanced synonym handling
  country_matches <- map(text_vector, function(text) {
    if (is.na(text) || text == "") return(character(0))

    text_lower <- str_to_lower(text)
    text_title <- str_to_title(text)  # For proper name matching
    found <- character(0)

    # First pass: Direct pattern matching with enhanced synonyms
    for(country in all_countries) {
      country_lower <- str_to_lower(country)

      # Enhanced homonym handling with context awareness
      if (country_lower == "niger") {
        # Context-aware detection for Niger (country vs fungus)
        if (str_detect(text, "\\bRepublic of Niger\\b|\\bNiger\\b.*\\b(africa|country|nation|west|sahel|niamey)\\b",
                       case_insensitive = TRUE) ||
            (str_detect(text, "\\bNiger\\b") &&
             !str_detect(text, "\\b(Aspergillus|Rhizopus|Penicillium|Fusarium|Alternaria|Cladosporium)\\s+niger\\b",
                        case_insensitive = TRUE))) {
          found <- c(found, country)
        }
      } else if (country_lower == "turkey") {
        # Enhanced Turkey detection (country vs bird/fungus)
        if (str_detect(text, "\\bTurkey\\b") &&
            !str_detect(text, "\\b(turkey\\s+tail|trametes\\s+versicolor|bracket\\s+fungus|polypore|mushroom|bird|poultry)\\b",
                       case_insensitive = TRUE) &&
            !str_detect(text, "\\bturkey\\s+(mushroom|fungus|fungi|mycology)\\b", case_insensitive = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "chile") {
        # Chile (country vs pepper)
        if (str_detect(text, "\\bChile\\b") &&
            !str_detect(text, "\\bchil[ei]\\s+(pepper|pod|sauce|spice|powder)\\b", case_insensitive = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "georgia") {
        # Georgia (country vs US state)
        if (str_detect(text, "\\bGeorgia\\b") &&
            !str_detect(text, "\\bgeorgia\\s+(pine|oak|southern|peach|usa|united\\s+states|america)\\b",
                       case_insensitive = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "guinea") {
        # Guinea (country vs animal)
        if (str_detect(text, "\\bGuinea\\b") &&
            !str_detect(text, "\\bguinea\\s+pig\\b", case_insensitive = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "mali") {
        # Mali (country vs fungus)
        if (str_detect(text, "\\bMali\\b") &&
            !str_detect(text, "\\b\\w+\\s+mali\\b", case_insensitive = TRUE)) {
          found <- c(found, country)
        }
      } else if (country == "South Korea" || country == "North Korea") {
        # Context-aware Korea disambiguation
        if (str_detect(text, "\\bkorea\\b", case_insensitive = TRUE)) {
          if (str_detect(text, "\\b(north|dprk|pyongyang|kim)\\b", case_insensitive = TRUE)) {
            if (country == "North Korea") found <- c(found, country)
          } else {
            # Default to South Korea for general "Korea" mentions
            if (country == "South Korea") found <- c(found, country)
          }
        }
      } else {
        # Standard pattern matching for other countries
        country_pattern <- paste0("\\b", str_replace_all(country, "\\s+", "\\\\s+"), "\\b")
        if (str_detect(text, country_pattern, case_insensitive = TRUE)) {
          found <- c(found, country)
        }

        # Also try matching against title case version
        if (str_detect(text_title, country_pattern)) {
          found <- c(found, country)
        }
      }
    }

    # Second pass: Try to find synonyms using enhanced standardization
    if (length(found) == 0) {
      # Extract potential country names using pattern matching
      potential_countries <- str_extract_all(text, "\\b[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)*\\b")[[1]]

      if (length(potential_countries) > 0) {
        # Try to standardize each potential country name
        standardized <- normalize_country_batch(potential_countries)
        valid_countries <- standardized[!is.na(standardized) & standardized %in% all_countries]
        found <- c(found, valid_countries)
      }
    }

    return(unique(found))
  })

  # Enhanced continent and region detection using comprehensive keywords
  if (length(continents) > 0) {
    continent_pattern <- paste0("\\b(", paste(str_to_lower(continents), collapse = "|"), ")\\b")
    continents_found <- str_extract_all(str_to_lower(text_vector), continent_pattern)
  } else {
    continents_found <- map(text_vector, ~character(0))
  }

  # Enhanced ecosystem/region detection with comprehensive keywords
  if (length(regions) > 0) {
    region_pattern <- paste0("\\b(", paste(str_to_lower(regions), collapse = "|"), ")\\b")
    regions_found <- str_extract_all(str_to_lower(text_vector), region_pattern)
  } else {
    regions_found <- map(text_vector, ~character(0))
  }

  # Enhanced coordinate detection
  coord_patterns <- c(
    # Standard formats: 45.5°N, 122.3°W or 45°30'N, 122°45'W
    "\\b\\d{1,2}(?:\\.\\d+)?[°]?\\s*[NS]\\b.*?\\b\\d{1,3}(?:\\.\\d+)?[°]?\\s*[EW]\\b",
    # Decimal degrees: 45.5, -122.3
    "\\b-?\\d{1,2}(?:\\.\\d+)?\\s*,\\s*-?\\d{1,3}(?:\\.\\d+)?\\b",
    # DMS format: 45°30'45"N, 122°45'30"W
    "\\b\\d{1,2}[°]\\s*\\d{1,2}['′]?\\s*\\d{0,2}[″\"]?\\s*[NS]\\b.*?\\b\\d{1,3}[°]\\s*\\d{1,2}['′]?\\s*\\d{0,2}[″\"]?\\s*[EW]\\b"
  )

  has_coordinates <- map_lgl(text_vector, function(text) {
    if (is.na(text)) return(FALSE)
    any(map_lgl(coord_patterns, ~str_detect(text, .)))
  })

  # Build results with enhanced categorization
  results <- tibble(
    countries_detected = map_chr(country_matches, ~if(length(.) > 0) paste(., collapse = "; ") else NA_character_),
    global_north_countries = map_chr(country_matches, ~{
      found <- intersect(., global_north)
      if(length(found) > 0) paste(found, collapse = "; ") else NA_character_
    }),
    global_south_countries = map_chr(country_matches, ~{
      found <- intersect(., global_south)
      if(length(found) > 0) paste(found, collapse = "; ") else NA_character_
    }),
    continents_detected = map_chr(continents_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    regions_detected = map_chr(regions_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    has_coordinates = has_coordinates,
    geographic_summary = pmap_chr(list(country_matches, continents_found, regions_found),
                                  function(countries, continents, regions) {
                                    all_geo <- c(countries, continents, regions)
                                    if(length(all_geo) > 0) paste(all_geo, collapse = "; ") else NA_character_
                                  })
  )

  return(results)
}

# Original function for backwards compatibility
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
      grepl("\\bRepublic of Niger\\b", text, case_insensitive = TRUE) ||
      (grepl("\\bNiger\\b", text) && !grepl("\\b(Aspergillus|Rhizopus|Penicillium|Fusarium|Alternaria|Cladosporium)\\s+niger\\b", text, case_insensitive = TRUE))
    } else if (country == "turkey") {
      # Match only if "Turkey" appears capitalized and not as "turkey tail" or similar mushroom contexts
      grepl("\\bTurkey\\b", text) && 
      !grepl("\\b(turkey\\s+tail|trametes\\s+versicolor|bracket\\s+fungus|polypore|mushroom)\\b", text, case_insensitive = TRUE) &&
      !grepl("\\bturkey\\s+(mushroom|fungus|fungi)\\b", text, case_insensitive = TRUE)
    } else if (country == "chile") {
      # Match "Chile" (country) but not "chili" (pepper) - use capitalization
      grepl("\\bChile\\b", text) && !grepl("\\bchil[ei]\\s+(pepper|pod|sauce|spice)\\b", text, case_insensitive = TRUE)
    } else if (country == "georgia") {
      # Match "Georgia" but prefer if it's clearly a country (with state/region context or capitalized)
      grepl("\\bGeorgia\\b", text) && !grepl("\\bgeorgia\\s+(pine|oak|southern)\\b", text, case_insensitive = TRUE)
    } else if (country == "guinea") {
      # Match "Guinea" but not "guinea pig" contexts
      grepl("\\bGuinea\\b", text) && !grepl("\\bguinea\\s+pig\\b", text, case_insensitive = TRUE)
    } else if (country == "mali") {
      # Match "Mali" but not in species contexts (some fungi have "mali" in names)
      grepl("\\bMali\\b", text) && !grepl("\\b\\w+\\s+mali\\b", text, case_insensitive = TRUE)
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

# OPTIMIZED: Vectorized function to detect plant parts
detect_plant_parts_batch <- function(text_vector) {
  plant_parts_keywords <- get_plant_parts_keywords()
  pattern <- paste0("\\b(", paste(plant_parts_keywords, collapse = "|"), ")\\b")
  
  text_lower <- str_to_lower(text_vector)
  parts_found <- str_extract_all(text_lower, pattern)
  
  tibble(
    plant_parts_detected = map_chr(parts_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    parts_count = map_int(parts_found, ~length(unique(.)))
  )
}

# Original function for backwards compatibility
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
    Relevant = ifelse(label == "Presence", 0.95, 0.05),
    Irrelevant = ifelse(label == "Absence", 0.95, 0.05),
    relevance_loose = ifelse(label == "Presence", "Relevant", "Irrelevant"),
    relevance_medium = ifelse(label == "Presence", "Relevant", "Irrelevant"),
    relevance_strict = ifelse(label == "Presence", "Relevant", "Irrelevant"),
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
    pa_loose = ifelse(label == "Presence", "Presence", "Absence"),
    pa_medium = ifelse(label == "Presence", "Presence", "Absence"),
    pa_strict = ifelse(label == "Presence", "Presence", "Absence"),
    pa_super_strict = ifelse(label == "Presence", "Presence", "Absence"),
    final_classification = label,
    conservative_classification = label,
    source = "training"
  ) %>%
  # Select only the columns that exist in classification_results, plus source
  # use any_of() so missing columns in the training data don't cause an error
  select(any_of(names(classification_results)), source)

cat("  Added", nrow(training_data), "training abstracts with valid DOIs\n")

# Skip Irrelevant column creation - not needed for analysis
combined_results <- classification_results %>%
  mutate(
    source = "classification",
    # Ensure consistent data types - only convert columns that exist
    publication_year = as.character(publication_year),
    Relevant = as.character(Relevant),
    relevance_loose = as.character(relevance_loose),
    pa_loose = as.character(pa_loose),
    pa_medium = as.character(pa_medium),
    pa_strict = as.character(pa_strict),
    pa_super_strict = as.character(pa_super_strict)
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
        pa_super_strict = as.character(pa_super_strict)
      )
  )

cat("  Combined dataset:", nrow(combined_results), "total abstracts\n")

# Focus on the weighted ensemble predictions (best performance) + training data
## Align with `apply_models_to_full_dataset.R`: prefer `final_classification` as the primary ensemble output
# If `final_classification` is missing, populate it from other ensemble columns produced by earlier scripts
if (!"final_classification" %in% names(combined_results)) {
  combined_results <- combined_results %>%
    mutate(final_classification = coalesce(weighted_ensemble, threshold_ensemble, conservative_classification, glmnet_pred, svm_pred))
}

# Create ensemble presence/absence probability columns if not present, pulling from available model probs
if (!"ensemble_presence_prob" %in% names(combined_results)) {
  combined_results <- combined_results %>%
    mutate(ensemble_presence_prob = coalesce(glmnet_prob_presence, svm_prob_presence),
           ensemble_absence_prob = coalesce(glmnet_prob_absence, svm_prob_absence))
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

# Step 2: Run species detection (OPTIMIZED) -------------------------------

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
  skip_species_detection <- TRUE #Change to TRUE to skip, FALSE to rerun
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

# Set up parallel processing and lookup tables - MORE WORKERS
setup_parallel(workers = min(6, parallel::detectCores() - 1))  # Increased from 2
lookup_tables <- create_lookup_tables_optimized(species)

# Get plant parts keywords from centralized utilities
plant_parts_keywords_species <- get_plant_parts_keywords()

# Process species detection in LARGER batches for better performance
tic("Species detection")

batch_size <- 100  # Increased from 25
n_batches <- ceiling(nrow(abstracts_for_species) / batch_size)

cat("Processing", nrow(abstracts_for_species), "abstracts in", n_batches, "batches...\n")
cat("⚙️  Batch configuration: ", batch_size, "abstracts per batch, 50 internal batch size, 1 worker per batch\n")
cat("🕐 Species detection started at", format(Sys.time(), "%H:%M:%S"), "\n\n")

all_species_results <- map_dfr(1:n_batches, function(i) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(abstracts_for_species))
  
  batch_data <- abstracts_for_species[start_idx:end_idx, ]
  
  cat("  🔬 Processing batch", i, "of", n_batches, "(", nrow(batch_data), "abstracts: rows", start_idx, "to", end_idx, ")\n")
  cat("     Starting species detection for batch", i, "at", format(Sys.time(), "%H:%M:%S"), "\n")
  
  # Process this batch with OPTIMIZED settings and verbose output
  batch_results <- process_abstracts_parallel(
    abstracts = batch_data,
    species_path = "models/species.rds",
    batch_size = 50,  # Increased internal batch size from 10
    workers = 1       # Avoid nested parallelism
  )
  
  # Report batch completion
  species_found <- sum(!is.na(batch_results$resolved_name) | !is.na(batch_results$canonicalName), na.rm = TRUE)
  cat("     ✅ Batch", i, "completed at", format(Sys.time(), "%H:%M:%S"), 
      "- Found species in", species_found, "abstracts\n")
  
  # Save intermediate results every 5 batches (fewer saves, larger batches)
  if (i %% 5 == 0) {
    temp_file <- paste0("results/temp_species_batch_", i, ".csv")
    write_csv(batch_results, temp_file)
    cat("     💾 Saved intermediate results to", temp_file, "\n")
  }
  
  # Progress summary every batch
  total_processed <- i * batch_size
  if (total_processed > nrow(abstracts_for_species)) total_processed <- nrow(abstracts_for_species)
  progress_pct <- round(100 * total_processed / nrow(abstracts_for_species), 1)
  cat("     📊 Progress:", total_processed, "/", nrow(abstracts_for_species), 
      "abstracts (", progress_pct, "% complete)\n\n")
  
  return(batch_results)
})

# Save species detection results
write_csv(all_species_results, "results/species_detection_weighted_ensemble.csv")

# Final summary of species detection
total_species_found <- sum(!is.na(all_species_results$resolved_name) | !is.na(all_species_results$canonicalName), na.rm = TRUE)
unique_species <- length(unique(c(all_species_results$resolved_name, all_species_results$canonicalName)))
unique_species <- unique_species - 1  # Remove NA count

cat("🎉 Species detection completed at", format(Sys.time(), "%H:%M:%S"), "\n")
cat("📈 Final Results:\n")
cat("   - Total abstracts processed:", nrow(abstracts_for_species), "\n")
cat("   - Abstracts with species found:", total_species_found, 
    "(", round(100 * total_species_found / nrow(abstracts_for_species), 1), "%)\n")
cat("   - Unique species detected:", unique_species, "\n")
cat("💾 Results saved to: results/species_detection_weighted_ensemble.csv\n\n")

toc()

} # End of species detection if-block

# Step 2.5: Extract additional information (OPTIMIZED) ----------------------

cat("\nStep 2.5: Extracting plant parts, methods, and geographic information...\n")
tic("Additional information extraction")

# Process all abstracts at once using vectorized functions
cat("  Extracting all information in optimized batches...\n")

# Handle missing abstracts
abstracts_text <- ifelse(is.na(abstracts_for_species$abstract), "", abstracts_for_species$abstract)

# Process in memory-efficient batches
batch_size <- 1000  # Process 1000 abstracts at a time
n_batches <- ceiling(length(abstracts_text) / batch_size)

cat("  Processing", length(abstracts_text), "abstracts in", n_batches, "batches...\n")

methods_results <- map_dfr(1:n_batches, function(batch_num) {
  start_idx <- (batch_num - 1) * batch_size + 1
  end_idx <- min(batch_num * batch_size, length(abstracts_text))
  
  batch_text <- abstracts_text[start_idx:end_idx]
  batch_ids <- abstracts_for_species$id[start_idx:end_idx]
  
  cat("    🔍 Batch", batch_num, "of", n_batches, "(", length(batch_text), "abstracts) - Starting at", format(Sys.time(), "%H:%M:%S"), "\n")
  
  # Process entire batch at once using vectorized functions
  cat("       Detecting research methods...\n")
  methods <- detect_research_methods_batch(batch_text)
  
  cat("       Detecting plant parts...\n")
  plant_parts <- detect_plant_parts_batch(batch_text)
  
  cat("       Detecting geographic locations...\n")
  geography <- detect_geographic_locations_batch(batch_text)
  
  # Quick summary of findings
  methods_found <- sum(methods$molecular | methods$culture_based | methods$microscopy, na.rm = TRUE)
  parts_found <- sum(!is.na(plant_parts$plant_parts_detected))
  countries_found <- sum(!is.na(geography$countries_detected))
  
  cat("       ✅ Batch", batch_num, "completed - Found: Methods(", methods_found, "), Parts(", parts_found, "), Countries(", countries_found, ")\n")
  
  # Progress tracking
  total_processed <- batch_num * batch_size
  if (total_processed > length(abstracts_text)) total_processed <- length(abstracts_text)
  progress_pct <- round(100 * total_processed / length(abstracts_text), 1)
  cat("       📊 Progress:", total_processed, "/", length(abstracts_text), 
      "abstracts (", progress_pct, "% complete)\n\n")
  
  # Combine results
  bind_cols(
    tibble(id = batch_ids),
    methods %>% rename(
      molecular_methods = molecular,
      culture_based_methods = culture_based,
      microscopy_methods = microscopy
    ),
    plant_parts,
    geography
  )
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
