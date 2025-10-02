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
        if (str_detect(text, "\\bTurkey\\b") &&
            !grepl("\\b(turkey\\s+(tail|red|wild|domestic)|trametes\\s+versicolor|bracket\\s+fungus|polypore|mushroom|bird|poultry|roast|meat)\\b", text,
                   ignore.case = TRUE) &&
            !grepl("\\bturkey\\s+(mushroom|fungus|fungi|mycology|culinary|cooking)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "chile") {
        # Chile (country vs pepper)
        if (str_detect(text, "\\bChile\\b") &&
            !grepl("\\bchil[ei]\\s+(pepper|pod|sauce|spice|powder|flakes|paste)\\b", text, ignore.case = TRUE)) {
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
        if (str_detect(text, "\\bChina\\b") &&
            !grepl("\\bchina\\s+(plate|cup|ware|porcelain|ceramic|clay|pottery)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "japan") {
        # Japan (country vs lacquer/enamel)
        if (str_detect(text, "\\bJapan\\b") &&
            !grepl("\\b(japan\\s+)?(black|red|gold|green)\\s+(lacquer|enamel|finish|coating)\\b", text, ignore.case = TRUE)) {
          found <- c(found, country)
        }
      } else if (country_lower == "poland") {
        # Poland (country vs polish/finish)
        if (str_detect(text, "\\bPoland\\b") &&
            !grepl("\\bpoland\\s+(spring|notation|ball|chicken)\\b", text, ignore.case = TRUE)) {
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
        if (str_detect(text, "\\bSerbia\\b") &&
            !grepl("\\bserbia\\s+spruce\\b", text, ignore.case = TRUE)) {
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

    # Ensure each country is only counted once per abstract, even if mentioned multiple times
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

  # Build results with enhanced categorization and ensure unique countries per abstract
  # Note: unique() calls ensure each geographical entity is counted only once per abstract
  results <- tibble(
    countries_detected = purrr::map_chr(country_matches, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    global_north_countries = purrr::map_chr(country_matches, ~{
      found <- intersect(unique(.), global_north)
      if(length(found) > 0) paste(unique(found), collapse = "; ") else NA_character_
    }),
    global_south_countries = purrr::map_chr(country_matches, ~{
      found <- intersect(unique(.), global_south)
      if(length(found) > 0) paste(unique(found), collapse = "; ") else NA_character_
    }),
    continents_detected = map_chr(continents_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    regions_detected = map_chr(regions_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    has_coordinates = has_coordinates,
    geographic_summary = pmap_chr(list(country_matches, continents_found, regions_found),
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
  consolidated_file <- "results/consolidated_dataset.csv"
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
