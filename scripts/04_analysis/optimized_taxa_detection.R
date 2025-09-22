# Optimized Taxa Detection Functions
# Addressing performance concerns for large datasets (10,000+ abstracts)
# and ensuring proper synonym handling

library(tidyverse)
library(rgbif)
library(stringr)
library(furrr)  # For parallel processing
library(future)
library(data.table)  # For faster joins and data manipulation
library(purrr)  # For vectorized operations

# By default do not run the internal tests/examples when this file is sourced.
# Set run_examples <- TRUE below (or override in your environment) to enable.
run_examples <- FALSE

# Enhanced parallel processing setup
setup_parallel <- function(workers = NULL) {
  # For Windows compatibility, use sequential plan to avoid multisession serialization issues
  plan(sequential)

  message("Parallel processing set up with sequential plan for Windows compatibility")
}

# Create optimized lookup tables with better structure for performance
create_lookup_tables <- function(species_df) {
  # Create a more efficient lookup structure
  species_df <- species_df %>%
    mutate(canonicalName_lower = tolower(canonicalName))
  
  # Create a specialized lookup for synonyms
  synonyms <- species_df %>%
    filter(taxonomicStatus != "accepted" & !is.na(acceptedNameUsageID)) %>%
    select(taxonID, canonicalName, canonicalName_lower, acceptedNameUsageID)
  
  # Create a lookup for accepted names
  accepted_species <- species_df %>%
    filter(taxonomicStatus == "accepted") %>%
    select(taxonID, canonicalName, canonicalName_lower, kingdom, phylum, family, genus)
  
  # Create genus and family lookups
  genus_list <- accepted_species %>%
    filter(!is.na(genus)) %>%
    distinct(genus) %>%
    rename(canonicalName = genus) %>%
    mutate(canonicalName_lower = tolower(canonicalName))
  
  family_list <- accepted_species %>%
    filter(!is.na(family)) %>%
    distinct(family) %>%
    rename(canonicalName = family) %>%
    mutate(canonicalName_lower = tolower(canonicalName))
  
  # Pre-compute some common joins for performance
  synonym_resolution <- synonyms %>%
    left_join(
      accepted_species %>% select(taxonID, canonicalName),
      by = c("acceptedNameUsageID" = "taxonID")
    ) %>%
    rename(acceptedName = canonicalName.y, synonymName = canonicalName.x) %>%
    filter(!is.na(acceptedName)) %>%  # Remove entries where accepted name is missing
    select(synonymName, acceptedName, canonicalName_lower) %>%
    # Group by synonym name and take the first accepted name to avoid duplicates
    group_by(canonicalName_lower) %>%
    slice(1) %>%
    ungroup() %>%
    distinct()  # Remove any remaining duplicate entries
  
  # Integration: Add hash tables for O(1) lookups (from 01_extract_species.R optimizations)
  # Optimization: Pre-compute hash tables for faster lookups on large datasets
  result <- list(
    species_df = species_df,
    accepted_species = accepted_species,
    genus_list = genus_list,
    family_list = family_list,
    synonyms = synonyms,
    synonym_resolution = synonym_resolution
  )

  # Add vectorized name lists for fast lookups
  if (!is.null(accepted_species)) {
    result$species_names_vector <- accepted_species$canonicalName_lower
  }
  if (!is.null(genus_list)) {
    result$genus_names_vector <- genus_list$canonicalName_lower
  }
  if (!is.null(family_list)) {
    result$family_names_vector <- family_list$canonicalName_lower
  }

  # Create hash tables for O(1) lookups if dataset is large enough
  threshold <- 10000  # Same threshold as in 01_extract_species.R
  if (nrow(species_df) > threshold) {
    if (!is.null(result$species_names_vector)) {
      result$species_hash <- setNames(rep(TRUE, length(result$species_names_vector)),
                                      result$species_names_vector)
    }
    if (!is.null(result$genus_names_vector)) {
      result$genus_hash <- setNames(rep(TRUE, length(result$genus_names_vector)),
                                    result$genus_names_vector)
    }
  }

  return(result)
}

# Optimized capitalization correction
correct_capitalization <- function(name) {
  if (is.na(name) || name == "") return(name)
  words <- str_split(name, " ")[[1]]
  words <- str_to_lower(words)
  words[1] <- str_to_title(words[1])
  paste(words, collapse = " ")
}

# Optimized extract_candidate_names with text preprocessing caching
extract_candidate_names <- function(text, preprocessed_text = NULL) {
  # Skip empty text
  if (is.na(text) || text == "") return(list(candidates = character(0), preprocessed_text = ""))

  # Text preprocessing: normalize (lowercase, punctuation removal) once for reuse
  # Optimization: Cache preprocessed text to avoid redundant processing in process_taxonomic_matches
  if (is.null(preprocessed_text)) {
    preprocessed_text <- tolower(gsub("[[:punct:][:digit:]]", " ", text))
  }

  # Enhanced pattern for "Genus species" that handles punctuation
  genus_species <- str_extract_all(text, "\\b[A-Z][a-z]+\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  genus_species <- str_replace_all(genus_species, "[,\\.;:\\)\\(]$", "")

  # Better pattern for abbreviated genus names
  abbreviated <- str_extract_all(text, "\\b[A-Z]\\.\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  abbreviated <- str_replace_all(abbreviated, "[,\\.;:\\)\\(]$", "")

  # Extract bigrams from preprocessed text for additional candidates
  bigram_pattern <- "\\b[a-z]+\\s+[a-z]+\\b"
  bigrams <- str_extract_all(preprocessed_text, bigram_pattern)[[1]]

  # Filter bigrams to only those that might be species names
  potential_bigrams <- bigrams[nchar(bigrams) > 5 & nchar(bigrams) < 40]

  # Store abbreviated genus references for later expansion
  abbrev_refs <- str_extract_all(text, "\\b[A-Z]\\.\\s+[a-z]+\\b")[[1]]

  # Combine all candidate names
  candidates <- unique(c(
    tolower(genus_species),
    tolower(abbreviated),
    potential_bigrams,
    tolower(abbrev_refs)
  ))

  return(list(candidates = candidates, preprocessed_text = preprocessed_text))
}

# Optimized batch_validate_names with Bloom filter option
batch_validate_names <- function(names, lookup_tables, use_fuzzy = FALSE, use_bloom_filter = FALSE) {
  if (length(names) == 0) return(tibble())
  
  # Filter out invalid names
  names <- unique(names[!is.na(names) & names != ""])
  
  # Skip processing if too many names (likely noise)
  if (length(names) > 200) {
    # Keep only names that look like species names (contain a space)
    names <- names[grepl(" ", names)]
    # Further limit if still too many
    if (length(names) > 100) {
      names <- names[1:100]
    }
  }
  
  # Expand abbreviated genus names using vectorized operations
  # Optimization: Replace nested loops with purrr::map for better performance
  expanded_names <- character(0)
  abbrev_matches <- names[grepl("^[A-Za-z]\\.\\s+[a-z]+", names)]

  if (length(abbrev_matches) > 0) {
    # Vectorized abbreviation expansion using purrr
    expanded_names <- abbrev_matches %>%
      map_chr(function(name) {
        # Extract first letter and species epithet
        parts <- strsplit(name, "\\.")[[1]]
        if (length(parts) < 2) return(NA_character_)

        first_letter <- tolower(substr(parts[1], 1, 1))
        species_epithet <- trimws(gsub("^\\s+", "", parts[2]))

        # Get all genera starting with that letter (limit to 50 for performance)
        potential_genera <- lookup_tables$genus_list %>%
          filter(str_starts(canonicalName_lower, first_letter)) %>%
          head(50) %>%  # Limit to 50 most common genera for performance
          pull(canonicalName)

        # Find first matching full name using vectorized check
        matches <- paste(potential_genera, species_epithet)
        matches_lower <- tolower(matches)
        matching_idx <- which(matches_lower %in% lookup_tables$accepted_species$canonicalName_lower)[1]

        if (!is.na(matching_idx)) {
          return(matches[matching_idx])
        } else {
          return(NA_character_)
        }
      }) %>%
      na.omit() %>%
      as.character()
  }
  
  names <- unique(c(names, expanded_names))
  
  # Bloom filter optimization: Pre-filter candidates to reduce join overhead
  # Optimization: Use hash-based filtering for O(1) lookups when enabled
  if (use_bloom_filter && !is.null(lookup_tables$species_hash)) {
    # Filter names that might exist using fast hash lookup
    names <- names[names %in% names(lookup_tables$species_hash)]
    if (length(names) == 0) return(tibble())
  }

  # Direct species match using data.table for faster joins
  # Optimization: Convert tibbles to data.table for significantly faster joins on large datasets
  names_dt <- data.table(
    user_supplied_name = names,
    user_supplied_name_lower = tolower(names)
  )

  # Convert lookup tables to data.table for faster joins
  species_df_dt <- as.data.table(lookup_tables$species_df)
  accepted_species_dt <- as.data.table(lookup_tables$accepted_species)
  synonym_resolution_dt <- as.data.table(lookup_tables$synonym_resolution)

  # First check which names might match using data.table semi-join equivalent
  potential_matches <- names_dt[species_df_dt, on = .(user_supplied_name_lower = canonicalName_lower), nomatch = NULL]

  # Only do the expensive joins on potential matches
  if (nrow(potential_matches) > 0) {
    # Match against accepted species
    accepted_matches <- potential_matches[accepted_species_dt, on = .(user_supplied_name_lower = canonicalName_lower), nomatch = NULL][
      , .(
        user_supplied_name,
        resolved_name = canonicalName,
        status = "ACCEPTED",
        acceptedScientificName = canonicalName,
        kingdom, phylum, family, genus
      )
    ]

    # Match against synonyms (exclude already matched accepted species)
    unmatched_names <- potential_matches[!accepted_matches, on = .(user_supplied_name)]

    if (nrow(unmatched_names) > 0) {
      synonym_matches <- unmatched_names[synonym_resolution_dt, on = .(user_supplied_name_lower = canonicalName_lower), nomatch = NULL][
        accepted_species_dt, on = .(acceptedName = canonicalName), nomatch = NULL
      ][
        , .(
          user_supplied_name,
          resolved_name = acceptedName,
          status = "SYNONYM",
          acceptedScientificName = acceptedName,
          kingdom, phylum, family, genus
        )
      ]
    } else {
      synonym_matches <- data.table()
    }

    # Combine matches
    resolved <- rbindlist(list(accepted_matches, synonym_matches), fill = TRUE)
  } else {
    resolved <- data.table()
  }
  
  # Fuzzy matching is disabled by default for performance
  # Only enable if specifically requested and for small datasets

  # Convert back to tibble for backward compatibility
  return(as_tibble(resolved))
}

# Optimized process_taxonomic_matches with text preprocessing caching
process_taxonomic_matches <- function(valid_species, lookup_tables, text,
                                      abstract_id, predicted_label, preprocessed_text = NULL) {
  all_rows <- list()

  # Use cached preprocessed text if available, otherwise process text
  # Optimization: Reuse preprocessed text from extract_candidate_names to avoid redundant normalization
  if (is.null(preprocessed_text)) {
    preprocessed_text <- tolower(gsub("[[:punct:][:digit:]]", " ", text))
  }

  # Create tokens and n-grams for matching
  tokens_vec <- unlist(strsplit(preprocessed_text, "\\s+"))
  tokens_vec <- tokens_vec[tokens_vec != ""]
  
  # Create bigrams to preserve multi-word taxa names
  bigrams <- character(0)
  if (length(tokens_vec) > 1) {
    for (i in 1:(length(tokens_vec) - 1)) {
      bigrams <- c(bigrams, paste(tokens_vec[i], tokens_vec[i + 1]))
    }
  }
  
  # Combine tokens and bigrams for matching
  all_tokens <- c(tokens_vec, bigrams)
  
  # Species matches
  if (is.data.frame(valid_species) && nrow(valid_species) > 0) {
    # Match against both individual tokens and bigrams
    # Check both resolved names and original user-supplied names
    species_matches <- valid_species %>%
      filter(tolower(resolved_name) %in% all_tokens | 
             tolower(user_supplied_name) %in% all_tokens)
    
    if (nrow(species_matches) > 0) {
      # Add metadata (taxonomic info already present from batch_validate_names)
      species_matches <- species_matches %>%
        mutate(
          id = abstract_id,
          predicted_label = predicted_label,
          match_type = "species"
        )
      
      all_rows <- append(all_rows, list(species_matches))
    }
  }
  
  # Genus matches
  genus_names_lower <- tolower(lookup_tables$genus_list$canonicalName)
  genus_mentions <- unique(tokens_vec[tokens_vec %in% genus_names_lower])
  
  if (length(genus_mentions) > 0) {
    genus_df <- lookup_tables$genus_list %>%
      filter(tolower(canonicalName) %in% genus_mentions) %>%
      left_join(
        lookup_tables$accepted_species %>% 
          select(genus, kingdom, phylum, family) %>% 
          distinct(),
        by = c("canonicalName" = "genus")
      ) %>%
      filter(!is.na(phylum)) %>%
      mutate(
        id = abstract_id,
        predicted_label = predicted_label,
        match_type = "genus",
        status = "ACCEPTED",
        resolved_name = canonicalName,
        acceptedScientificName = canonicalName
      )
    
    if (nrow(genus_df) > 0) {
      all_rows <- append(all_rows, list(genus_df))
    }
  }
  
  # Family matches
  family_names_lower <- tolower(lookup_tables$family_list$canonicalName)
  family_mentions <- unique(tokens_vec[tokens_vec %in% family_names_lower])
  
  if (length(family_mentions) > 0) {
    family_df <- lookup_tables$family_list %>%
      filter(tolower(canonicalName) %in% family_mentions) %>%
      left_join(
        lookup_tables$accepted_species %>% 
          select(family, kingdom, phylum) %>% 
          distinct(),
        by = c("canonicalName" = "family")
      ) %>%
      filter(!is.na(phylum)) %>%
      mutate(
        id = abstract_id,
        predicted_label = predicted_label,
        match_type = "family",
        status = "ACCEPTED",
        resolved_name = canonicalName,
        acceptedScientificName = canonicalName
      )
    
    if (nrow(family_df) > 0) {
      all_rows <- append(all_rows, list(family_df))
    }
  }
  
  return(all_rows)
}

# Optimized extract_plant_info function with text preprocessing caching
extract_plant_info <- function(text, abstract_id, predicted_label, lookup_tables, plant_parts_keywords) {
  if (is.na(text) || text == "") {
    return(create_empty_result(abstract_id, predicted_label))
  }

  # Extract candidate names with preprocessing caching
  candidate_result <- extract_candidate_names(text)
  plant_candidates <- candidate_result$candidates
  preprocessed_text <- candidate_result$preprocessed_text

  # Detect plant parts with simple string matching using cached preprocessed text
  # Optimization: Reuse preprocessed text for plant parts detection
  plant_parts_found <- character(0)
  for (part in plant_parts_keywords) {
    # Use word boundary pattern to match whole words only
    if (grepl(paste0("\\b", part, "\\b"), preprocessed_text)) {
      plant_parts_found <- c(plant_parts_found, part)
    }
  }

  plant_parts_indicator <- setNames(
    as.integer(plant_parts_keywords %in% plant_parts_found),
    plant_parts_keywords
  )

  # Validate candidate names with improved function
  valid_species <- batch_validate_names(plant_candidates, lookup_tables)

  # Process matches with improved function, passing cached preprocessed text
  all_rows <- process_taxonomic_matches(
    valid_species, lookup_tables, text,
    abstract_id, predicted_label, preprocessed_text
  )
  
  # Combine results
  final_df <- bind_rows(all_rows)
  if (nrow(final_df) == 0) {
    final_df <- create_empty_result(abstract_id, predicted_label)
  }
  
  # Add plant parts information
  plant_parts_df <- as.data.frame(t(plant_parts_indicator))
  final_df <- bind_cols(final_df, plant_parts_df)
  
  return(final_df)
}

# Helper function to create empty result
create_empty_result <- function(abstract_id, predicted_label) {
  tibble(
    id = abstract_id,
    predicted_label = predicted_label,
    match_type = "none",
    canonicalName = NA_character_,
    kingdom = NA_character_,
    phylum = NA_character_,
    family = NA_character_,
    genus = NA_character_,
    status = "NO_MATCH",
    resolved_name = NA_character_,
    acceptedScientificName = NA_character_
  )
}

# Enhanced parallel processing with streaming and optimized batching
process_abstracts_parallel <- function(abstracts, species_path, plant_parts_keywords,
                                        batch_size = NULL, workers = NULL, use_streaming = TRUE) {
  # Set up parallel processing
  setup_parallel(workers)

  # Total number of abstracts
  total_abstracts <- nrow(abstracts)
  message("Processing ", total_abstracts, " abstracts sequentially")

  # Optimization: Increase batch size for sequential processing to reduce overhead
  if (is.null(batch_size)) {
    batch_size <- ifelse(total_abstracts > 1000, 200, 500)
  }

  # Process in batches for better memory management
  batches <- split(1:total_abstracts, ceiling(seq_along(1:total_abstracts) / batch_size))

  all_results <- list()
  temp_files <- character(0)

  for (i in seq_along(batches)) {
    batch_indices <- batches[[i]]
    message("Processing batch ", i, " of ", length(batches),
             " (abstracts ", min(batch_indices), " to ", max(batch_indices), ")")

    # Process batch sequentially, loading lookup_tables inside worker to reduce serialization overhead
    batch_results <- future_map(batch_indices, function(idx) {
      # Load species data and create lookup tables inside worker function
      species <- readRDS(species_path)
      lookup_tables <- create_lookup_tables(species)

      extract_plant_info(
        text = abstracts$abstract[idx],
        abstract_id = abstracts$id[idx],
        predicted_label = abstracts$predicted_label[idx],
        lookup_tables = lookup_tables,
        plant_parts_keywords = plant_parts_keywords
      )
    }, .options = furrr_options(seed = TRUE))

    # Memory optimization: Stream partial results to temp files
    if (use_streaming && length(batch_results) > 0) {
      batch_df <- bind_rows(batch_results)
      temp_file <- tempfile(pattern = paste0("batch_", i, "_"), fileext = ".rds")
      saveRDS(batch_df, temp_file)
      temp_files <- c(temp_files, temp_file)

      # Clear batch results from memory
      batch_results <- NULL
      batch_df <- NULL
    } else {
      # Combine batch results in memory if not streaming
      all_results <- c(all_results, batch_results)
    }

    # Optimization: More frequent garbage collection for memory management
    gc()
  }

  # Aggregate results from temp files or in-memory results
  if (use_streaming && length(temp_files) > 0) {
    message("Aggregating results from ", length(temp_files), " temp files")
    final_results <- map_dfr(temp_files, readRDS)

    # Clean up temp files
    file.remove(temp_files)
  } else {
    final_results <- bind_rows(all_results)
  }

  message("Processing complete! Processed ", total_abstracts, " abstracts")
  return(final_results)
}


# Test function to demonstrate synonym handling
test_synonym_handling <- function() {
  # Load species data
  species_path <- "C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review/models/species.rds"
  if (!file.exists(species_path)) {
    stop("species.rds file not found at: ", species_path)
  }
  species <- readRDS(species_path)
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  # Find some known synonyms for testing
  if (nrow(lookup_tables$synonyms) == 0) {
    message("No synonyms found for testing")
    return(NULL)
  }
  
  # Make sure we have the right column names before joining
  synonyms <- lookup_tables$synonyms
  
  if ("canonicalName" %in% colnames(synonyms)) {
    # Join with accepted species to get the accepted names
    synonyms <- synonyms %>%
      left_join(
        lookup_tables$accepted_species %>% select(taxonID, canonicalName),
        by = c("acceptedNameUsageID" = "taxonID")
      ) %>%
      rename(acceptedName = canonicalName.y, synonymName = canonicalName.x) %>%
      head(5)
  } else {
    # If the column structure is different, adapt accordingly
    message("Using alternative synonym lookup method")
    
    # Use the pre-computed synonym resolution table
    synonyms <- lookup_tables$synonym_resolution %>% head(5)
  }
  
  if (nrow(synonyms) == 0) {
    message("No valid synonyms found for testing")
    return(NULL)
  }
  
  # Test cases with synonyms
  test_cases <- list()
  for (i in 1:nrow(synonyms)) {
    # Get synonym name based on available columns
    if ("synonymName" %in% colnames(synonyms)) {
      synonym_name <- synonyms$synonymName[i]
    } else if ("canonicalName" %in% colnames(synonyms)) {
      synonym_name <- synonyms$canonicalName[i]
    } else {
      # If we can't find a suitable column, skip this synonym
      next
    }
    
    # Skip if synonym name is NA
    if (is.na(synonym_name)) next
    
    test_cases[[paste0("Synonym ", i)]] <- paste0(
      "This study examines the endophytic fungi associated with ", 
      synonym_name, 
      " in the Pacific Northwest."
    )
  }
  
  # If no test cases could be created, return
  if (length(test_cases) == 0) {
    message("Could not create test cases from synonyms")
    return(NULL)
  }
  
  # Define plant parts for testing
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  # Run tests
  results <- list()
  
  cat("\n=== Testing Synonym Handling ===\n")
  
  for (name in names(test_cases)) {
    cat("\n---", name, "---\n")
    text <- test_cases[[name]]
    cat("Text:", text, "\n")
    
    # Extract candidate names
    candidate_result <- extract_candidate_names(text)
    candidates <- candidate_result$candidates
    cat("Extracted candidates:", paste(candidates, collapse = ", "), "\n")

    # Validate names
    valid_species <- batch_validate_names(candidates, lookup_tables)
    cat("Valid species found:", nrow(valid_species), "\n")
    if (nrow(valid_species) > 0) {
      print(valid_species %>% select(user_supplied_name, resolved_name, status))
    }
    
    # Process full abstract
    plant_info <- extract_plant_info(
      text = text,
      abstract_id = which(names(test_cases) == name),
      predicted_label = "Presence",
      lookup_tables = lookup_tables,
      plant_parts_keywords = plant_parts_keywords
    )
    
    cat("\nFinal detected taxa:\n")
    print(plant_info %>% select(match_type, resolved_name, status))
    
    results[[name]] <- plant_info
  }
  
  # Summary
  cat("\n=== Summary ===\n")
  success_count <- sum(sapply(results, function(r) any(r$match_type == "species")))
  cat("Successfully detected species in", success_count, "out of", length(test_cases), "test cases\n")
  
  # Check specifically for synonym resolution
  synonym_count <- sum(sapply(results, function(r) any(r$status == "SYNONYM")))
  cat("Successfully resolved synonyms in", synonym_count, "out of", length(test_cases), "test cases\n")
  
  return(results)
}

# Run the test if this script is executed directly
if (interactive() && run_examples) {
  cat("Testing optimized taxa detection with synonym handling...\n")
  test_results <- test_synonym_handling()
}

if (interactive() && run_examples) {
  cat("\n=== Running Function Tests ===\n")

  # Minimal mock species data
  mock_species <- tibble(
    taxonID = c(1, 2),
    canonicalName = c("Quercus robur", "Fagus sylvatica"),
    taxonomicStatus = c("accepted", "accepted"),
    acceptedNameUsageID = c(NA, NA),
    kingdom = c("Plantae", "Plantae"),
    phylum = c("Tracheophyta", "Tracheophyta"),
    family = c("Fagaceae", "Fagaceae"),
    genus = c("Quercus", "Fagus")
  )

  # Test create_lookup_tables
  lookup_tables <- create_lookup_tables(mock_species)
  print(names(lookup_tables))

  # Test extract_candidate_names
  candidate_result <- extract_candidate_names("Quercus robur and Fagus sylvatica are common trees.")
  candidates <- candidate_result$candidates
  print(candidates)

  # Test batch_validate_names
  validated <- batch_validate_names(candidates, lookup_tables)
  print(validated)

  # Test process_taxonomic_matches
  matches <- process_taxonomic_matches(validated, lookup_tables, "Quercus robur and Fagus sylvatica", 1, "Presence")
  print(matches)

  # Test extract_plant_info
  plant_parts_keywords <- c("leaf", "root")
  plant_info <- extract_plant_info("Quercus robur leaf and Fagus sylvatica root", 1, "Presence", lookup_tables, plant_parts_keywords)
  print(plant_info)

  # Test process_abstracts_parallel (with only one abstract for speed)
  abstracts <- tibble(
    abstract = c("Quercus robur leaf and Fagus sylvatica root"),
    id = 1,
    predicted_label = "Presence"
  )
  # Save mock species to temp file for testing
  temp_species_path <- tempfile(fileext = ".rds")
  saveRDS(mock_species, temp_species_path)
  parallel_results <- process_abstracts_parallel(abstracts, temp_species_path, plant_parts_keywords, batch_size = 1, workers = 1)
  print(parallel_results)
  # Clean up temp file
  file.remove(temp_species_path)
}
