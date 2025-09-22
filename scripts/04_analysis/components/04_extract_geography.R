# =============================================================================
# 04_extract_geography.R - Geographic locations detection component
# =============================================================================
#
# Purpose: Detect geographic locations from abstracts using comprehensive matching
#
# Description: Script that detects countries, continents, regions, and coordinates from abstracts
# using advanced keyword matching, synonym handling, and context-aware homonym disambiguation
# for accurate geographic information extraction.
#
# Dependencies: tidyverse, stringr, progress; scripts/04_analysis/utilities/reference_data_utils.R
#
# Author: B. Bock
# Date: 2024-09-22
#
# Inputs/Outputs: Reads prepared abstracts from results/prepared_abstracts_for_extraction.csv; outputs geography detection results to results/geography_detection_results.csv
#
# =============================================================================

library(tidyverse)
library(stringr)
library(progress)

# Source utilities
source("scripts/04_analysis/utilities/reference_data_utils.R")

cat("=== GEOGRAPHY DETECTION COMPONENT ===\n")
cat("Extracting geographic location information\n\n")

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
    purrr::map_chr(country_vector, function(country) {
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
  country_matches <- purrr::map(text_vector, function(text) {
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
      if (grepl("\\bRepublic of Niger\\b|\\bNiger\\b.*\\b(africa|country|nation|west|sahel|niamey)\\b", text,
                ignore.case = TRUE) ||
          (str_detect(text, "\\bNiger\\b") &&
           !grepl("\\b(Aspergillus|Rhizopus|Penicillium|Fusarium|Alternaria|Cladosporium)\\s+niger\\b", text,
                 ignore.case = TRUE))) {
          found <- c(found, country)
        }
      } else if (country_lower == "turkey") {
        # Enhanced Turkey detection (country vs bird/fungus)
        if (str_detect(text, "\\bTurkey\\b") &&
            !grepl("\\b(turkey\\s+tail|trametes\\s+versicolor|bracket\\s+fungus|polypore|mushroom|bird|poultry)\\b", text,
                   ignore.case = TRUE) &&
            !grepl("\\bturkey\\s+(mushroom|fungus|fungi|mycology)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "chile") {
        # Chile (country vs pepper)
        if (str_detect(text, "\\bChile\\b") &&
            !grepl("\\bchil[ei]\\s+(pepper|pod|sauce|spice|powder)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "georgia") {
        # Georgia (country vs US state)
        if (str_detect(text, "\\bGeorgia\\b") &&
            !grepl("\\bgeorgia\\s+(pine|oak|southern|peach|usa|united\\s+states|america)\\b", text,
                   ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "guinea") {
        # Guinea (country vs animal)
        if (str_detect(text, "\\bGuinea\\b") &&
            !grepl("\\bguinea\\s+pig\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "mali") {
        # Mali (country vs fungus)
        if (str_detect(text, "\\bMali\\b") &&
            !grepl("\\b\\w+\\s+mali\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country == "South Korea" || country == "North Korea") {
        # Context-aware Korea disambiguation
        if (grepl("\\bkorea\\b", text, ignore.case = TRUE)) {
          if (grepl("\\b(north|dprk|pyongyang|kim)\\b", text, ignore.case = TRUE)) {
            if (country == "North Korea") found <- c(found, country)
          } else {
            # Default to South Korea for general "Korea" mentions
            if (country == "South Korea") found <- c(found, country)
          }
        }
      } else {
        # Standard pattern matching for other countries
        country_pattern <- paste0("\\b", str_replace_all(country, "\\s+", "\\\\s+"), "\\b")
        if (grepl(country_pattern, text, ignore.case = TRUE)) {
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
    continents_found <- purrr::map(text_vector, ~character(0))
  }

  # Enhanced ecosystem/region detection with comprehensive keywords
  if (length(regions) > 0) {
    region_pattern <- paste0("\\b(", paste(str_to_lower(regions), collapse = "|"), ")\\b")
    regions_found <- str_extract_all(str_to_lower(text_vector), region_pattern)
  } else {
    regions_found <- purrr::map(text_vector, ~character(0))
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
    countries_detected = purrr::map_chr(country_matches, ~if(length(.) > 0) paste(., collapse = "; ") else NA_character_),
    global_north_countries = purrr::map_chr(country_matches, ~{
      found <- intersect(., global_north)
      if(length(found) > 0) paste(found, collapse = "; ") else NA_character_
    }),
    global_south_countries = purrr::map_chr(country_matches, ~{
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

# Main geography extraction function
extract_geography_data <- function(
  abstracts_data,
  output_file = "results/geography_detection_results.csv",
  batch_size = 1000,
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Recovery mechanism
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing geography detection results\n")
    existing_results <- read_csv(output_file, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
    return(existing_results)
  }

  if (verbose) cat("🔬 Starting geography detection for", nrow(abstracts_data), "abstracts\n")

  # Handle missing abstracts
  abstracts_text <- ifelse(is.na(abstracts_data$abstract), "", abstracts_data$abstract)

  # Process in batches for memory efficiency
  n_batches <- ceiling(length(abstracts_text) / batch_size)

  if (verbose) {
    cat("📊 Processing", length(abstracts_text), "abstracts in", n_batches, "batches\n")
    cat("⚙️  Batch size:", batch_size, "abstracts per batch\n")
    cat("🕐 Started at", format(Sys.time(), "%H:%M:%S"), "\n\n")
  }

  # Initialize progress bar
  if (verbose) {
    pb <- progress_bar$new(
      format = "Geography [:bar] :percent | ETA: :eta | :current/:total batches",
      total = n_batches,
      clear = FALSE
    )
  }

  geography_results <- purrr::map_dfr(1:n_batches, function(batch_num) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, length(abstracts_text))

    batch_text <- abstracts_text[start_idx:end_idx]
    batch_ids <- abstracts_data$id[start_idx:end_idx]

    if (verbose) {
      pb$tick()
    } else {
      cat("   🌍 Batch", batch_num, "of", n_batches, "(", length(batch_text), "abstracts)\n")
    }

    # Detect geography
    geography <- detect_geographic_locations_batch(batch_text)

    # Quick summary
    countries_found <- sum(!is.na(geography$countries_detected))
    continents_found <- sum(!is.na(geography$continents_detected))
    regions_found <- sum(!is.na(geography$regions_detected))
    coords_found <- sum(geography$has_coordinates)

    if (verbose && batch_num %% 10 == 0) {
      cat("      Found: Countries(", countries_found, "), Continents(", continents_found, "), Regions(", regions_found, "), Coords(", coords_found, ")\n")
    }

    # Combine with IDs
    bind_cols(
      tibble(id = batch_ids),
      geography
    )
  })

  # Save results
  write_csv(geography_results, output_file)

  # Summary
  total_abstracts <- nrow(geography_results)
  countries_total <- sum(!is.na(geography_results$countries_detected))
  continents_total <- sum(!is.na(geography_results$continents_detected))
  regions_total <- sum(!is.na(geography_results$regions_detected))
  coords_total <- sum(geography_results$has_coordinates)
  any_geography <- sum(!is.na(geography_results$geographic_summary))

  if (verbose) {
    cat("\n🎉 Geography detection completed!\n")
    cat("📈 Results:\n")
    cat("   - Total abstracts processed:", total_abstracts, "\n")
    cat("   - Abstracts with countries:", countries_total,
        "(", round(100 * countries_total / total_abstracts, 1), "%)\n")
    cat("   - Abstracts with continents:", continents_total,
        "(", round(100 * continents_total / total_abstracts, 1), "%)\n")
    cat("   - Abstracts with regions:", regions_total,
        "(", round(100 * regions_total / total_abstracts, 1), "%)\n")
    cat("   - Abstracts with coordinates:", coords_total,
        "(", round(100 * coords_total / total_abstracts, 1), "%)\n")
    cat("   - Any geographic info:", any_geography,
        "(", round(100 * any_geography / total_abstracts, 1), "%)\n")
    cat("💾 Results saved to:", output_file, "\n")
  }

  return(geography_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "04_extract_geography.R")) {

  # Load abstracts data
  abstracts_file <- "results/prepared_abstracts_for_extraction.csv"
  if (!file.exists(abstracts_file)) {
    stop("❌ Prepared abstracts not found. Run the pipeline script first or prepare data manually.")
  }

  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)

  # Extract geography
  geography_results <- extract_geography_data(abstracts_data)

  cat("\n✅ Geography extraction component completed!\n")
}
