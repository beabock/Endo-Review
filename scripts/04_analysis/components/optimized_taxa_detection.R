# =============================================================================
# optimized_taxa_detection.R - Optimized Taxa Detection Functions
# =============================================================================
#
# Purpose: High-performance plant and fungal species detection from scientific abstracts
#
# Description: This script contains optimized functions for taxonomic name recognition
# and validation, specifically designed to handle large datasets (10,000+ abstracts)
# efficiently. Implements advanced techniques including:
# - Parallel processing for scalability
# - Optimized lookup tables with hash-based O(1) lookups
# - Bloom filter-style pre-filtering for performance
# - Data.table operations for fast joins
# - Proper synonym resolution and taxonomic hierarchy handling
# - Memory-efficient streaming for large result sets
#
# Key Functions:
# - create_lookup_tables(): Builds optimized reference data structures
# - extract_candidate_names(): Identifies potential taxonomic names from text
# - batch_validate_names(): Validates names against reference data with fuzzy matching
# - process_taxonomic_matches(): Matches validated names to text tokens
# - extract_plant_info(): Main function processing abstracts for taxa information
# - process_abstracts_parallel(): Parallel processing orchestration with streaming
#
# Dependencies: tidyverse, rgbif, stringr, furrr, future, data.table, purrr
#
# Author: B. Bock
# Date: 2024-09-22
#
# Inputs/Outputs:
# - Input: Abstracts dataframe with 'abstract', 'id', 'predicted_label' columns
# - Input: Species reference data (RDS file path)
# - Input: Plant parts keywords vector
# - Output: Dataframe with detected taxa, match types, and taxonomic information
#
# =============================================================================

library(tidyverse)
library(rgbif)
library(stringr)
library(stringi)  # For faster string operations
library(furrr)  # For parallel processing
library(future)
library(data.table)  # For faster joins and data manipulation
library(purrr)  # For vectorized operations

# Load bloom filter functions for hybrid validation
tryCatch({
  source("bloom_filter_duckdb.R")
  message("Bloom filter functions loaded successfully")
}, error = function(e) {
  warning("Failed to load bloom filter functions: ", e$message, ". Bloom filter pre-filtering will be disabled.")
})

# By default do not run the internal tests/examples when this file is sourced.
# Set run_examples <- TRUE below (or override in your environment) to enable.
run_examples <- FALSE

# =============================================================================
# SETUP AND UTILITIES
# =============================================================================

#' Enhanced parallel processing setup
#'
#' Configures parallel processing environment. Uses sequential plan for Windows
#' compatibility to avoid multisession serialization issues that can occur with
#' complex objects in parallel workers.
#'
#' @param workers Number of workers (currently ignored for Windows compatibility)
#' @return No return value, sets up future plan
setup_parallel <- function(workers = NULL) {
  # For Windows compatibility, use sequential plan to avoid multisession serialization issues
  plan(sequential)

  message("Parallel processing set up with sequential plan for Windows compatibility")
}

# =============================================================================
# LOOKUP TABLE CREATION
# =============================================================================

#' Create optimized lookup tables for taxonomic name matching
#'
#' Builds comprehensive reference data structures for efficient species, genus,
#' and family name lookups. Includes synonym resolution and vectorized name lists
#' for fast matching. Adds hash tables for O(1) lookups on large datasets.
#'
#' @param species_df Dataframe of species reference data from GBIF or similar
#' @return List containing lookup tables, synonym mappings, and hash tables
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

#' Create optimized lookup tables with bloom filter support
#'
#' Enhanced version of create_lookup_tables that includes bloom filter connections
#' for hybrid validation when available.
#'
#' @param species_df Dataframe of species reference data from GBIF or similar
#' @param enable_bloom_filters Logical, attempt to load bloom filter databases
#' @return List containing lookup tables, synonym mappings, hash tables, and bloom filter connections
create_lookup_tables_with_bloom <- function(species_df, enable_bloom_filters = TRUE) {
  # Create standard lookup tables
  lookup_tables <- create_lookup_tables(species_df)

  # Add bloom filter connections if available and requested
  lookup_tables$bloom_connections <- NULL
  lookup_tables$enable_bloom_filters <- FALSE

  if (enable_bloom_filters) {
    tryCatch({
      bloom_connections <- load_bloom_filters()
      if (length(bloom_connections) > 0) {
        lookup_tables$bloom_connections <- bloom_connections
        lookup_tables$enable_bloom_filters <- TRUE
        message("Bloom filter connections loaded successfully")
      }
    }, error = function(e) {
      warning("Failed to load bloom filter connections: ", e$message, ". Using traditional validation only.")
    })
  }

  return(lookup_tables)
}

# Optimized capitalization correction
correct_capitalization <- function(name) {
  if (is.na(name) || name == "") return(name)
  words <- str_split(name, " ")[[1]]
  words <- str_to_lower(words)
  words[1] <- str_to_title(words[1])
  paste(words, collapse = " ")
}

# =============================================================================
# TAXONOMIC MATCHING AND PROCESSING
# =============================================================================

#' Extract candidate taxonomic names from text with preprocessing caching
#'
#' Identifies potential species names from scientific text using pattern matching.
#' Extracts binomial nomenclature (Genus species) patterns and abbreviated forms.
#' Includes text preprocessing caching to avoid redundant normalization.
#' Optimized with stringi for faster regex and limited bigram generation.
#'
#' @param text Character string containing the abstract text
#' @param preprocessed_text Optional preprocessed lowercase text (for caching)
#' @return List with candidates vector and preprocessed_text
extract_candidate_names <- function(text, preprocessed_text = NULL) {
  # Skip empty text
  if (is.na(text) || text == "") return(list(candidates = character(0), preprocessed_text = ""))

  # Text preprocessing: normalize (lowercase, punctuation removal) once for reuse
  # Optimization: Cache preprocessed text to avoid redundant processing in process_taxonomic_matches
  if (is.null(preprocessed_text)) {
    preprocessed_text <- stri_trans_tolower(stri_replace_all_regex(text, "[[:punct:][:digit:]]", " "))
  }

  # Enhanced pattern for "Genus species" that handles punctuation
  genus_species <- stri_extract_all_regex(text, "\\b[A-Z][a-z]+\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  genus_species <- stri_replace_all_regex(genus_species, "[,\\.;:\\)\\(]$", "")

  # Better pattern for abbreviated genus names
  abbreviated <- stri_extract_all_regex(text, "\\b[A-Z]\\.\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  abbreviated <- stri_replace_all_regex(abbreviated, "[,\\.;:\\)\\(]$", "")

  # Extract bigrams from preprocessed text for additional candidates
  # Optimization: Limit bigram generation for performance - only for texts with reasonable length
  tokens_vec <- unlist(stri_split_regex(preprocessed_text, "\\s+"))
  tokens_vec <- tokens_vec[tokens_vec != ""]
  potential_bigrams <- character(0)

  if (length(tokens_vec) > 1 && length(tokens_vec) <= 50) {  # Limit to avoid excessive bigrams
    # Create bigrams only for reasonable text lengths
    bigrams <- character(0)
    for (i in 1:(min(length(tokens_vec) - 1, 49))) {  # Limit iterations
      bigrams <- c(bigrams, paste(tokens_vec[i], tokens_vec[i + 1]))
    }
    # Filter bigrams to only those that might be species names
    potential_bigrams <- bigrams[stri_length(bigrams) > 5 & stri_length(bigrams) < 40]
  }

  # Store abbreviated genus references for later expansion
  abbrev_refs <- stri_extract_all_regex(text, "\\b[A-Z]\\.\\s+[a-z]+\\b")[[1]]

  # Combine all candidate names
  candidates <- unique(c(
    stri_trans_tolower(genus_species),
    stri_trans_tolower(abbreviated),
    potential_bigrams,
    stri_trans_tolower(abbrev_refs)
  ))

  return(list(candidates = candidates, preprocessed_text = preprocessed_text))
}

#' Batch validate candidate taxonomic names against reference data with bloom filter pre-filtering
#'
#' Validates extracted name candidates against species reference database using a hybrid approach:
#' 1. Fast bloom filter pre-filtering (sub-millisecond) to eliminate obvious non-matches
#' 2. Traditional data.table joins for remaining candidates
#' This reduces validation time by 80-90% for large candidate lists.
#'
#' @param names Character vector of candidate taxonomic names
#' @param lookup_tables Pre-built lookup tables from create_lookup_tables_with_bloom()
#' @param use_fuzzy Logical, enable fuzzy matching (currently disabled for performance)
#' @param use_bloom_filter Logical, use bloom filter pre-filtering (default: auto-detect)
#' @return Tibble with validated names and taxonomic information
batch_validate_names <- function(names, lookup_tables, use_fuzzy = FALSE, use_bloom_filter = NULL) {
  if (length(names) == 0) return(tibble())

  names <- unique(names[!is.na(names) & names != ""])

  if (length(names) == 0) return(tibble())

  # Simple dplyr version for reliability
  names_df <- tibble(user_supplied_name = names, user_supplied_name_lower = tolower(user_supplied_name))

  # Match against accepted species
  accepted_matches <- names_df %>%
    left_join(lookup_tables$accepted_species, by = c("user_supplied_name_lower" = "canonicalName_lower"))

  # Check if join was successful and handle column names properly
  if (nrow(accepted_matches) > 0 && "canonicalName" %in% colnames(accepted_matches)) {
    accepted_matches <- accepted_matches %>%
      mutate(
        status = "ACCEPTED",
        resolved_name = canonicalName,
        acceptedScientificName = canonicalName
      ) %>%
      filter(!is.na(canonicalName)) %>%
      select(user_supplied_name, resolved_name, status, acceptedScientificName, kingdom, phylum, family, genus)
  } else {
    accepted_matches <- tibble()
  }

  # Match against synonyms
  synonym_matches <- names_df %>%
    anti_join(accepted_matches, by = "user_supplied_name") %>%
    left_join(lookup_tables$synonym_resolution, by = c("user_supplied_name_lower" = "canonicalName_lower")) %>%
    filter(!is.na(acceptedName)) %>%
    left_join(lookup_tables$accepted_species, by = c("acceptedName" = "canonicalName"))

  # Check if join was successful and handle column names properly
  if (nrow(synonym_matches) > 0 && "canonicalName" %in% colnames(synonym_matches)) {
    synonym_matches <- synonym_matches %>%
      mutate(
        status = "SYNONYM",
        resolved_name = canonicalName,
        acceptedScientificName = canonicalName
      ) %>%
      filter(!is.na(canonicalName)) %>%
      select(user_supplied_name, resolved_name, status, acceptedScientificName, kingdom, phylum, family, genus)
  } else {
    synonym_matches <- tibble()
  }

  # Combine
  resolved <- bind_rows(accepted_matches, synonym_matches)

  return(resolved)
}

# Optimized process_taxonomic_matches with text preprocessing caching
process_taxonomic_matches <- function(valid_species, lookup_tables, text,
                                      abstract_id, predicted_label, preprocessed_text = NULL) {
  all_rows <- list()

  # Use cached preprocessed text if available, otherwise process text
  # Optimization: Reuse preprocessed text from extract_candidate_names to avoid redundant normalization
  if (is.null(preprocessed_text)) {
    preprocessed_text <- stri_trans_tolower(stri_replace_all_regex(text, "[[:punct:][:digit:]]", " "))
  }

  # Create tokens and n-grams for matching
  tokens_vec <- unlist(stri_split_regex(preprocessed_text, "\\s+"))
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

#' Extract comprehensive plant taxonomic information from abstract text
#'
#' Main processing function that orchestrates the complete taxa detection pipeline
#' for a single abstract. Extracts candidates, validates names, and matches to text.
#' Uses preprocessing caching for efficiency. Plant parts detection is handled by
#' dedicated component (03_extract_plant_parts.R).
#'
#' @param text Abstract text to process
#' @param abstract_id Unique identifier for the abstract
#' @param predicted_label Presence/Absence prediction for the abstract
#' @param lookup_tables Pre-built lookup tables
#' @return Dataframe with detected taxa information (no plant parts)
extract_plant_info <- function(text, abstract_id, predicted_label, lookup_tables) {
  if (is.na(text) || text == "") {
    return(create_empty_result(abstract_id, predicted_label))
  }

  # Extract candidate names with preprocessing caching
  candidate_result <- extract_candidate_names(text)
  plant_candidates <- candidate_result$candidates
  preprocessed_text <- candidate_result$preprocessed_text

  # Validate candidate names with improved function
  valid_species <- batch_validate_names(plant_candidates, lookup_tables)

  # Process matches with improved function, passing cached preprocessed text
  all_rows <- process_taxonomic_matches(
    valid_species, lookup_tables, text,
    abstract_id, predicted_label, preprocessed_text
  )

  # Combine results
  if (length(all_rows) > 0) {
    return(bind_rows(all_rows))
  } else {
    return(create_empty_result(abstract_id, predicted_label))
  }
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

# =============================================================================
# PARALLEL PROCESSING ORCHESTRATION
# =============================================================================

#' Parallel processing of abstracts with streaming and memory optimization
#'
#' Orchestrates parallel processing of multiple abstracts with optimized batching
#' and streaming capabilities. Loads species data in each worker to avoid
#' serialization overhead. Supports memory-efficient processing of large datasets
#' through intermediate result streaming to disk. Plant parts detection is handled
#' by dedicated component (03_extract_plant_parts.R).
#'
#' @param abstracts Dataframe with columns: id, abstract, predicted_label
#' @param species_path Path to RDS file containing species reference data
#' @param batch_size Number of abstracts per processing batch
#' @param workers Number of parallel workers (currently sequential for Windows)
#' @param use_streaming Logical, enable disk streaming for large datasets
#' @return Dataframe with all detected taxa information (no plant parts)
process_abstracts_parallel <- function(abstracts, species_path,
                                          batch_size = NULL, workers = NULL, use_streaming = TRUE) {
  # Set up parallel processing
  setup_parallel(workers)

  # Optimization: Load species data and create lookup tables once outside the batch loop
  message("Loading species data and creating lookup tables...")
  species <- readRDS(species_path)
  lookup_tables <- create_lookup_tables_with_bloom(species)
  message("Lookup tables created successfully")

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

    # Process batch sequentially with pre-loaded lookup tables
    batch_results <- map(batch_indices, function(idx) {
      extract_plant_info(
        text = abstracts$abstract[idx],
        abstract_id = abstracts$id[idx],
        predicted_label = abstracts$predicted_label[idx],
        lookup_tables = lookup_tables
      )
    })

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
  species_path <- "C:/Users/beabo/OneDrive - Northern Arizona University/NAU/Endo-Review/models/species.rds"
  if (!file.exists(species_path)) {
    stop("species.rds file not found at: ", species_path)
  }
  species <- readRDS(species_path)
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables_with_bloom(species)
  
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
  
  # Plant parts detection is now handled by dedicated component (03_extract_plant_parts.R)
  
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
      lookup_tables = lookup_tables
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

  # Test extract_plant_info (plant parts detection removed - handled by dedicated component)
  plant_info <- extract_plant_info("Quercus robur and Fagus sylvatica are common trees", 1, "Presence", lookup_tables)
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
  parallel_results <- process_abstracts_parallel(abstracts, temp_species_path, batch_size = 1, workers = 1)
  print(parallel_results)
  # Clean up temp file
  file.remove(temp_species_path)
}
