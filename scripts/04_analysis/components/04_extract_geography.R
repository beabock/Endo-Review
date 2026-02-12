# =============================================================================
# 04_extract_geography.R - Geographic locations detection component
# =============================================================================
#
# Purpose: Detect geographic locations from abstracts using comprehensive matching
#
# Description: Script that detects countries, continents, regions, and coordinates from abstracts
# using advanced keyword matching, synonym handling, and context-aware homonym disambiguation
# for accurate geographic information extraction. Part of the memory-efficient extraction pipeline
# that minimizes data duplication by only outputting id + geography columns.
#
# Dependencies: tidyverse, stringr, progress; scripts/04_analysis/utilities/reference_data_utils.R
#
# Author: B. Bock
# Date: 2024-09-26
#
# Inputs/Outputs: Reads consolidated dataset from results/consolidated_dataset.csv; outputs geography detection results to results/geography_detection_results.csv
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
  all_countries_raw <- get_all_countries()
  
  # Sort countries by length (longest first) to avoid partial matches
  # e.g., "Democratic Republic of the Congo" should be checked before "Republic of the Congo"
  all_countries <- all_countries_raw[order(-nchar(all_countries_raw))]
  
  # Standardize global north/south lists to canonical forms for proper matching
  global_north <- get_global_north_countries() %>%
    purrr::map_chr(standardize_country_name) %>%
    unique()
  global_south <- get_global_south_countries() %>%
    purrr::map_chr(standardize_country_name) %>%
    unique()
  
  continents <- get_continent_keywords()
  regions <- get_region_keywords()
  institutions <- get_research_institution_mappings()
  adjectival_mappings <- get_adjectival_region_mappings()

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
    
    # Pre-check: Identify texts that should be filtered out due to homonyms
    # Filter out obvious non-country contexts
    is_fungal_context <- grepl("\\b(fungus|fungi|fungal|mycology|mold|spore|aspergillus|fusarium|trametes|polypore)\\b", text, ignore.case = TRUE)
    is_food_context <- grepl("\\b(pepper|spice|sauce|culinary|cooking|recipe|flavor|taste)\\b", text, ignore.case = TRUE)
    is_ceramic_context <- grepl("\\b(plate|ceramic|porcelain|pottery|dishware|china ware)\\b", text, ignore.case = TRUE)
    is_water_context <- grepl("\\b(spring water|mineral water|bottled water|water quality|water source)\\b", text, ignore.case = TRUE)
    is_tree_context <- grepl("\\b(spruce|pine|tree|conifer|growth pattern|forestry)\\b", text, ignore.case = TRUE)
    
    # First pass: Direct pattern matching with enhanced synonyms
    for(country in all_countries) {
      country_lower <- str_to_lower(country)

      # Enhanced homonym handling with context awareness and additional cases
      if (country_lower == "niger") {
        # Context-aware detection for Niger (country vs fungus)
        if (grepl("\\bRepublic of Niger\\b|\\bNiger\\b.*\\b(africa|country|nation|west|sahel|niamey)\\b", text,
                  ignore.case = TRUE) ||
            (str_detect(text, "\\bNiger\\b") &&
             !grepl("\\b(Aspergillus|Rhizopus|Penicillium|Fusarium|Alternaria|Cladosporium|Curvularia)\\s+niger\\b", text,
                   ignore.case = TRUE))) {
          found <- c(found, country)
        }
      } else if (country_lower == "turkey") {
        # Enhanced Turkey detection (country vs bird/fungus/food)
        if (grepl("\\bturkey\\b", text, ignore.case = TRUE) &&
            !grepl("\\bturkey\\s+tail\\b", text, ignore.case = TRUE) &&
            !grepl("\\b(trametes|polypore|bracket\\s+fungus)\\b", text, ignore.case = TRUE) &&
            !grepl("\\bturkey\\s+(red|wild|domestic|mushroom|bird|meat)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "chile") {
        # Chile (country vs pepper)
        if (grepl("\\bchile\\b", text, ignore.case = TRUE) &&
            !grepl("\\bchil[ei]\\s+pepper\\b", text, ignore.case = TRUE) &&
            !grepl("\\bpepper\\b", text, ignore.case = TRUE) &&
            !grepl("\\b(capsicum|jalape[ñn]o|habanero|spice)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "georgia") {
        # Georgia (country vs US state)
        if (str_detect(text, "\\bGeorgia\\b") &&
            !grepl("\\bgeorgia\\s+(pine|oak|southern|peach|usa|united\\s+states|america|tech|bulldog)\\b", text,
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
      } else if (country_lower == "china") {
        # China (country vs ceramics)
        if (grepl("\\bchina\\b", text, ignore.case = TRUE) &&
            !grepl("\\bchina\\s+plate\\b", text, ignore.case = TRUE) &&
            !grepl("\\b(plate|ceramic|porcelain|pottery|dishware)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "japan") {
        # Japan (country vs lacquer/enamel)
        if (str_detect(text, "\\bJapan\\b") &&
            !grepl("\\b(japan\\s+)?(black|red|gold|green)\\s+(lacquer|enamel|finish|coating)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "poland") {
        # Poland (country vs polish/finish/water brand)
        if (grepl("\\bpoland\\b", text, ignore.case = TRUE) &&
            !grepl("\\bpoland\\s+spring\\b", text, ignore.case = TRUE) &&
            !grepl("\\b(water|quality|mineral|spring)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "armenia") {
        # Armenia (country vs apricot variety)
        if (str_detect(text, "\\bArmenia\\b") &&
            !grepl("\\barmenian\\s+apricot\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "albania") {
        # Albania (country vs plant genus)
        if (str_detect(text, "\\bAlbania\\b") &&
            !grepl("\\balbania\\s+(officinalis|verticillata)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "serbia") {
        # Serbia (country vs spruce)
        if (grepl("\\bserbia\\b", text, ignore.case = TRUE) &&
            !grepl("\\bserbia\\s+spruce\\b", text, ignore.case = TRUE) &&
            !grepl("\\b(spruce|picea|growth\\s+pattern|tree|conifer)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country == "South Korea" || country == "North Korea") {
        # Context-aware Korea disambiguation
        if (grepl("\\bkorea\\b", text, ignore.case = TRUE)) {
          if (grepl("\\b(north|dprk|pyongyang|kim|juche|songun)\\b", text, ignore.case = TRUE)) {
            if (country == "North Korea") found <- c(found, country)
          } else {
            # Default to South Korea for general "Korea" mentions
            if (country == "South Korea") found <- c(found, country)
          }
        }
      } else if (country == "Republic of the Congo") {
        # Don't match "Republic of the Congo" if it's part of "Democratic Republic of the Congo"
        # Also skip if we already found "Democratic Republic of the Congo"
        if (!("Democratic Republic of the Congo" %in% found) &&
            grepl("\\bRepublic\\s+of\\s+the\\s+Congo\\b", text, ignore.case = TRUE) &&
            !grepl("\\bDemocratic\\s+Republic\\s+of\\s+the\\s+Congo\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country == "Democratic Republic of the Congo") {
        # Always check for the full name first
        if (grepl("\\bDemocratic\\s+Republic\\s+of\\s+the\\s+Congo\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else {
        # Standard pattern matching for other countries - but skip if context suggests homonym
        should_skip <- FALSE
        
        # Check for homonym contexts that should prevent country detection
        if (country_lower == "turkey" && (is_fungal_context || grepl("\\bturkey\\s+tail\\b", text, ignore.case = TRUE))) {
          should_skip <- TRUE
        } else if (country_lower == "chile" && is_food_context) {
          should_skip <- TRUE
        } else if (country_lower == "china" && is_ceramic_context) {
          should_skip <- TRUE
        } else if (country_lower == "poland" && is_water_context) {
          should_skip <- TRUE
        } else if (country_lower == "serbia" && is_tree_context) {
          should_skip <- TRUE
        }
        
        if (!should_skip) {
          # Create flexible pattern that handles periods, spaces, and special characters
          # Escape special regex characters but preserve spaces and periods
          country_escaped <- str_replace_all(country, "([.()])", "\\\\\\1")
          country_pattern <- str_replace_all(country_escaped, "\\s+", "\\\\s+")
          
          # Use lookahead/lookbehind for better word boundary detection with periods
          pattern <- paste0("(?<!\\w)", country_pattern, "(?!\\w)")
          
          if (grepl(pattern, text, ignore.case = TRUE, perl = TRUE)) {
            found <- c(found, country)
          }

          # Also try matching against title case version
          if (str_detect(text_title, pattern)) {
            found <- c(found, country)
          }
        }
      }
    }

    # Second pass: Try to find synonyms using enhanced standardization
    if (length(found) == 0) {
      # Extract potential country names using pattern matching
      potential_countries <- str_extract_all(text, "\\b[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)*\\b")[[1]]

      if (length(potential_countries) > 0) {
        # Filter out potential countries that should be skipped due to homonym contexts
        filtered_countries <- character(0)
        for (potential in potential_countries) {
          pot_lower <- str_to_lower(potential)
          should_skip <- FALSE
          
          if (pot_lower == "turkey" && (is_fungal_context || grepl("\\bturkey\\s+tail\\b", text, ignore.case = TRUE))) {
            should_skip <- TRUE
          } else if (pot_lower == "chile" && is_food_context) {
            should_skip <- TRUE
          } else if (pot_lower == "china" && is_ceramic_context) {
            should_skip <- TRUE
          } else if (pot_lower == "poland" && is_water_context) {
            should_skip <- TRUE
          } else if (pot_lower == "serbia" && is_tree_context) {
            should_skip <- TRUE
          }
          
          if (!should_skip) {
            filtered_countries <- c(filtered_countries, potential)
          }
        }
        
        # Try to standardize each filtered potential country name
        if (length(filtered_countries) > 0) {
          standardized <- normalize_country_batch(filtered_countries)
          valid_countries <- standardized[!is.na(standardized) & standardized %in% all_countries]
          found <- c(found, valid_countries)
        }
      }
    }

    # Ensure each country is only counted once per abstract, even if mentioned multiple times
    return(unique(found))
  })

  # Detect countries from research institutions
  institution_countries <- purrr::map(text_vector, function(text) {
    if (is.na(text) || text == "") return(character(0))
    
    found_countries <- character(0)
    
    # Check each institution pattern
    for (institution_name in names(institutions)) {
      # Create pattern that matches institution name (case insensitive)
      pattern <- paste0("\\b", str_replace_all(institution_name, "\\s+", "\\\\s+"), "\\b")
      
      if (grepl(pattern, text, ignore.case = TRUE)) {
        country <- institutions[[institution_name]]
        found_countries <- c(found_countries, country)
      }
    }
    
    return(unique(found_countries))
  })
  
  # Detect countries from adjectival/regional forms
  adjectival_countries <- purrr::map(text_vector, function(text) {
    if (is.na(text) || text == "") return(character(0))
    
    found_countries <- character(0)
    
    # Check each adjectival form
    for (adj_form in names(adjectival_mappings)) {
      # Create pattern that matches the adjectival form (case insensitive)
      pattern <- paste0("\\b", adj_form, "\\b")
      
      if (grepl(pattern, text, ignore.case = TRUE)) {
        # Get the countries associated with this adjectival form
        mapping <- adjectival_mappings[[adj_form]]
        if ("countries" %in% names(mapping)) {
          # Split the semicolon-separated country list
          countries <- str_split(mapping["countries"], ";\\s*")[[1]]
          # Only add if it's a specific country (not "Multiple ... countries" or placeholder text)
          valid_countries <- countries[!grepl("Multiple|countries$", countries)]
          # Also filter out any that aren't in the all_countries list
          valid_countries <- valid_countries[valid_countries %in% all_countries]
          found_countries <- c(found_countries, valid_countries)
        }
      }
    }
    
    return(unique(found_countries))
  })
  
  # Merge country detections from all sources
  country_matches <- purrr::map2(country_matches, institution_countries, function(direct, inst) {
    unique(c(direct, inst))
  })
  
  country_matches <- purrr::map2(country_matches, adjectival_countries, function(current, adj) {
    unique(c(current, adj))
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

  # Coordinate validation function to filter false positives
  validate_coordinates <- function(coord_strings) {
    if (length(coord_strings) == 0) return(logical(0))

    valid <- logical(length(coord_strings))

    for (i in seq_along(coord_strings)) {
      coord_str <- coord_strings[i]

      # Extract numeric values from coordinate string
      numbers <- as.numeric(unlist(regmatches(coord_str, gregexpr("-?\\d+\\.?\\d*", coord_str))))

      if (length(numbers) >= 2) {
        # Check if numbers are in valid ranges
        lat_candidates <- numbers[abs(numbers) <= 90]
        lon_candidates <- numbers[abs(numbers) <= 180]

        # Must have at least one latitude and one longitude candidate
        if (length(lat_candidates) > 0 && length(lon_candidates) > 0) {
          # Check for reasonable coordinate pairs
          for (lat in lat_candidates) {
            for (lon in lon_candidates) {
              # Allow for some tolerance in coordinate ranges
              if (abs(lat) <= 90 && abs(lon) <= 180) {
                valid[i] <- TRUE
                break
              }
            }
            if (valid[i]) break
          }
        }
      } else if (length(numbers) == 1) {
        # Single number might be valid (e.g., in UTM or grid references)
        num <- numbers[1]
        if (abs(num) <= 90 || abs(num) <= 180 || num >= 100000) {
          valid[i] <- TRUE
        }
      }
    }

    return(valid)
  }

  # Enhanced coordinate detection with comprehensive patterns
  coord_patterns <- c(
    # Standard formats: 45.5°N, 122.3°W or 45°30'N, 122°45'W
    "\\b\\d{1,2}(?:\\.\\d+)?[°]?\\s*[NS]\\b.*?\\b\\d{1,3}(?:\\.\\d+)?[°]?\\s*[EW]\\b",
    # Decimal degrees: 45.5, -122.3 (latitude, longitude)
    "\\b-?\\d{1,2}(?:\\.\\d+)?\\s*,\\s*-?\\d{1,3}(?:\\.\\d+)?\\b",
    # DMS format: 45°30'45"N, 122°45'30"W
    "\\b\\d{1,2}[°]\\s*\\d{1,2}['′]?\\s*\\d{0,2}[″\"]?\\s*[NS]\\b.*?\\b\\d{1,3}[°]\\s*\\d{1,2}['′]?\\s*\\d{0,2}[″\"]?\\s*[EW]\\b",

    # Additional coordinate formats
    # Decimal degrees with direction: 45.5N, 122.3W
    "\\b\\d{1,2}\\.\\d+[NS]\\s*,?\\s*\\d{1,3}\\.\\d+[EW]\\b",
    # Degrees only: 45N, 122W
    "\\b\\d{1,2}[NS]\\s*,?\\s*\\d{1,3}[EW]\\b",
    # DMS with spaces: 45 30 N, 122 45 W
    "\\b\\d{1,2}\\s+\\d{1,2}[′']?\\s*[NS]\\s*,?\\s*\\d{1,3}\\s+\\d{1,2}[′']?\\s*[EW]\\b",
    # Coordinates in parentheses: (45.5, -122.3)
    "\\(\\s*-?\\d{1,2}(?:\\.\\d+)?\\s*,\\s*-?\\d{1,3}(?:\\.\\d+)?\\s*\\)",
    # Coordinates with degree symbol variations: 45°30′, 122°45′
    "\\b\\d{1,2}°\\d{1,2}['′]\\s*[NS]\\s*,?\\s*\\d{1,3}°\\d{1,2}['′]\\s*[EW]\\b",
    # UTM coordinates: 45N 123456 7890123
    "\\b\\d{1,2}[NS]\\s+\\d{6}\\s+\\d{7}\\b",
    # Military grid: 45N1234567890123
    "\\b\\d{1,2}[NS]\\d{6,7}\\b",

    # Geographic coordinates in various formats
    # Latitude/longitude ranges: 40-50°N, 120-130°W
    "\\b\\d{1,2}-\\d{1,2}[°]?[NS]\\s*,?\\s*\\d{2,3}-\\d{2,3}[°]?[EW]\\b",
    # Coordinates with cardinal directions: N45.5, W122.3
    "\\b[NS]\\d{1,2}\\.\\d+\\s*,?\\s*[EW]\\d{2,3}\\.\\d+\\b",
    # Coordinates in square brackets: [45.5, -122.3]
    "\\[\\s*-?\\d{1,2}(?:\\.\\d+)?\\s*,\\s*-?\\d{1,3}(?:\\.\\d+)?\\s*\\]",

    # Elevation coordinates: 45.5°N 122.3°W 1500m
    "\\b\\d{1,2}(?:\\.\\d+)?[°]?\\s*[NS]\\b.*?\\b\\d{1,3}(?:\\.\\d+)?[°]?\\s*[EW]\\b.*?\\b\\d+[mft]\\b",
    # Grid references and map coordinates
    "\\b[a-zA-Z]{2}\\s*\\d{4}\\s*\\d{4}\\b",  # e.g., SU123456
    "\\b[a-zA-Z]{2}-\\d{4}-\\d{4}\\b",      # e.g., SU-1234-5678
    "\\b\\d{4}[NS]\\d{6}[EW]\\b",           # numeric grid references
    "\\b\\d{6}[NS]\\s*\\d{6}[EW]\\b",       # 6-digit coordinates
    "\\b\\d{8}[NS][EW]\\b"                 # 8-digit combined format
  )

  has_coordinates <- map_lgl(text_vector, function(text) {
    if (is.na(text)) return(FALSE)

    # Find all potential coordinate matches
    matches <- map_lgl(coord_patterns, ~str_detect(text, .))

    # If no matches found, return FALSE
    if (!any(matches)) return(FALSE)

    # Get the actual matched text for validation
    matched_text <- str_extract_all(text, paste(coord_patterns[matches], collapse = "|"))[[1]]

    # Validate coordinates to filter false positives
    valid_coords <- validate_coordinates(matched_text)

    return(any(valid_coords))
  })

  # Standardize all detected countries to canonical forms
  # This ensures that variations like "USA", "U.S.A.", "US" all become "United States"
  # and historical names like "Ceylon" become "Sri Lanka"
  country_matches_standardized <- purrr::map(country_matches, function(countries) {
    if (length(countries) == 0) return(character(0))
    
    # Standardize each country name to its canonical form
    canonical <- purrr::map_chr(countries, standardize_country_name)
    
    # Remove duplicates (multiple variations may map to same canonical name)
    return(unique(canonical))
  })
  
  # Build results with enhanced categorization and ensure unique countries per abstract
  # Note: unique() calls ensure each geographical entity is counted only once per abstract
  results <- tibble(
    countries_detected = purrr::map_chr(country_matches_standardized, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    global_north_countries = purrr::map_chr(country_matches_standardized, ~{
      found <- intersect(unique(.), global_north)
      if(length(found) > 0) paste(unique(found), collapse = "; ") else NA_character_
    }),
    global_south_countries = purrr::map_chr(country_matches_standardized, ~{
      found <- intersect(unique(.), global_south)
      if(length(found) > 0) paste(unique(found), collapse = "; ") else NA_character_
    }),
    continents_detected = map_chr(continents_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    regions_detected = map_chr(regions_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    has_coordinates = has_coordinates,
    geographic_summary = pmap_chr(list(country_matches_standardized, continents_found, regions_found),
                                  function(countries, continents, regions) {
                                    all_geo <- c(unique(countries), unique(continents), unique(regions))
                                    if(length(all_geo) > 0) paste(unique(all_geo), collapse = "; ") else NA_character_
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

  # Keep only id and abstract columns for memory efficiency
  abstracts_data <- abstracts_data %>%
    select(id, abstract)

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

  # Load consolidated dataset
  consolidated_file <- "results/datasets/consolidated_dataset.csv"
  if (!file.exists(consolidated_file)) {
    stop("❌ Consolidated dataset not found. Run the consolidation script first.")
  }

  abstracts_data <- read_csv(consolidated_file, show_col_types = FALSE)

  # Check if we have the required columns
  required_cols <- c("id", "abstract")
  missing_cols <- setdiff(required_cols, colnames(abstracts_data))
  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns in consolidated data:", paste(missing_cols, collapse = ", "))
  }

  # Extract geography
  geography_results <- extract_geography_data(abstracts_data)

  cat("\n✅ Geography extraction component completed!\n")
}
