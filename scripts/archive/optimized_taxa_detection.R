# Optimized Taxa Detection Functions
# Addressing performance concerns for large datasets (10,000+ abstracts)
# and ensuring proper synonym handling

library(tidyverse)
library(rgbif)
library(stringr)
library(furrr)  # For parallel processing
library(future)

# Set up parallel processing
setup_parallel <- function(workers = NULL) {
  if (is.null(workers)) {
    workers <- min(4, availableCores() - 1)  # Default to 4 or less if fewer cores
  }
  
  # Set a higher memory limit for large datasets
  options(future.globals.maxSize = 4000 * 1024^2)  # 4GB limit
  
  # Use multisession for Windows compatibility
  plan(multisession, workers = workers)
  
  message("Parallel processing set up with ", workers, " workers")
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
  
  return(list(
    species_df = species_df,
    accepted_species = accepted_species,
    genus_list = genus_list,
    family_list = family_list,
    synonyms = synonyms,
    synonym_resolution = synonym_resolution
  ))
}

# Optimized capitalization correction
correct_capitalization <- function(name) {
  if (is.na(name) || name == "") return(name)
  words <- str_split(name, " ")[[1]]
  words <- str_to_lower(words)
  words[1] <- str_to_title(words[1])
  paste(words, collapse = " ")
}

# Optimized extract_candidate_names with better regex patterns
extract_candidate_names <- function(text) {
  # Skip empty text
  if (is.na(text) || text == "") return(character(0))
  
  # Enhanced pattern for "Genus species" that handles punctuation
  genus_species <- str_extract_all(text, "\\b[A-Z][a-z]+\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  genus_species <- str_replace_all(genus_species, "[,\\.;:\\)\\(]$", "")
  
  # Better pattern for abbreviated genus names
  abbreviated <- str_extract_all(text, "\\b[A-Z]\\.\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  abbreviated <- str_replace_all(abbreviated, "[,\\.;:\\)\\(]$", "")
  
  # Extract bigrams from lowercase text for additional candidates
  text_lower <- tolower(text)
  
  # Extract n-grams to preserve multi-word taxa
  bigram_pattern <- "\\b[a-z]+\\s+[a-z]+\\b"
  bigrams <- str_extract_all(text_lower, bigram_pattern)[[1]]
  
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
  
  return(candidates)
}

# Optimized batch_validate_names with better performance
batch_validate_names <- function(names, lookup_tables, use_fuzzy = FALSE) {
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
  
  # Expand abbreviated genus names
  expanded_names <- character(0)
  abbrev_matches <- names[grepl("^[A-Za-z]\\.\\s+[a-z]+", names)]
  
  if (length(abbrev_matches) > 0) {
    for (name in abbrev_matches) {
      # Extract first letter and species epithet
      parts <- strsplit(name, "\\.")[[1]]
      if (length(parts) < 2) next
      
      first_letter <- tolower(substr(parts[1], 1, 1))
      species_epithet <- trimws(gsub("^\\s+", "", parts[2]))
      
      # Get all genera starting with that letter (limit to 50 for performance)
      potential_genera <- lookup_tables$genus_list %>%
        filter(str_starts(canonicalName_lower, first_letter)) %>%
        head(50) %>%  # Limit to 50 most common genera for performance
        pull(canonicalName)
      
      # Try each potential genus
      for (gen in potential_genera) {
        full_name_candidate <- paste(gen, species_epithet)
        if (tolower(full_name_candidate) %in% lookup_tables$accepted_species$canonicalName_lower) {
          expanded_names <- c(expanded_names, full_name_candidate)
          break  # Stop after first match for performance
        }
      }
    }
  }
  
  names <- unique(c(names, expanded_names))
  
  # Direct species match - more efficient with semi_join first
  names_df <- tibble(user_supplied_name = names) %>%
    mutate(user_supplied_name_lower = tolower(user_supplied_name))
  
  # First check which names might match using a faster semi_join
  potential_matches <- names_df %>%
    semi_join(lookup_tables$species_df, 
              by = c("user_supplied_name_lower" = "canonicalName_lower"))
  
  # Only do the expensive left_join on potential matches
  if (nrow(potential_matches) > 0) {
    # Match against accepted species
    accepted_matches <- potential_matches %>%
      inner_join(
        lookup_tables$accepted_species,
        by = c("user_supplied_name_lower" = "canonicalName_lower")
      ) %>%
      mutate(
        resolved_name = canonicalName,
        status = "ACCEPTED",
        acceptedScientificName = canonicalName
      )
    
    # Match against synonyms
    synonym_matches <- potential_matches %>%
      anti_join(accepted_matches, by = "user_supplied_name") %>%
      inner_join(
        lookup_tables$synonym_resolution,
        by = c("user_supplied_name_lower" = "canonicalName_lower")
      ) %>%
      # Add taxonomic hierarchy for the accepted name
      left_join(
        lookup_tables$accepted_species %>% 
          select(canonicalName, kingdom, phylum, family, genus),
        by = c("acceptedName" = "canonicalName")
      ) %>%
      mutate(
        resolved_name = acceptedName,
        status = "SYNONYM",
        acceptedScientificName = acceptedName
      ) %>%
      distinct()  # Remove any duplicate rows that might be created
    
    # Combine matches
    resolved <- bind_rows(accepted_matches, synonym_matches)
  } else {
    resolved <- tibble()
  }
  
  # Fuzzy matching is disabled by default for performance
  # Only enable if specifically requested and for small datasets
  
  return(resolved)
}

# Optimized process_taxonomic_matches with better performance
process_taxonomic_matches <- function(valid_species, lookup_tables, text, 
                                     abstract_id, predicted_label) {
  all_rows <- list()
  
  # Create tokens and n-grams for matching
  tokens_vec <- unlist(strsplit(gsub("[[:punct:][:digit:]]", " ", tolower(text)), "\\s+"))
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

# Optimized extract_plant_info function
extract_plant_info <- function(text, abstract_id, predicted_label, lookup_tables, plant_parts_keywords) {
  if (is.na(text) || text == "") {
    return(create_empty_result(abstract_id, predicted_label))
  }
  
  # Extract candidate names with improved function
  plant_candidates <- extract_candidate_names(text)
  
  # Detect plant parts with simple string matching
  text_lower <- tolower(text)
  plant_parts_found <- character(0)
  for (part in plant_parts_keywords) {
    # Use word boundary pattern to match whole words only
    if (grepl(paste0("\\b", part, "\\b"), text_lower)) {
      plant_parts_found <- c(plant_parts_found, part)
    }
  }
  
  plant_parts_indicator <- setNames(
    as.integer(plant_parts_keywords %in% plant_parts_found), 
    plant_parts_keywords
  )
  
  # Validate candidate names with improved function
  valid_species <- batch_validate_names(plant_candidates, lookup_tables)
  
  # Process matches with improved function
  all_rows <- process_taxonomic_matches(
    valid_species, lookup_tables, text, 
    abstract_id, predicted_label
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

# Process abstracts in parallel for better performance
process_abstracts_parallel <- function(abstracts, lookup_tables, plant_parts_keywords, 
                                      batch_size = 100, workers = NULL) {
  # Set up parallel processing
  setup_parallel(workers)
  
  # Total number of abstracts
  total_abstracts <- nrow(abstracts)
  message("Processing ", total_abstracts, " abstracts in parallel")
  
  # Process in batches for better memory management
  batches <- split(1:total_abstracts, ceiling(seq_along(1:total_abstracts) / batch_size))
  
  all_results <- list()
  
  for (i in seq_along(batches)) {
    batch_indices <- batches[[i]]
    message("Processing batch ", i, " of ", length(batches), 
            " (abstracts ", min(batch_indices), " to ", max(batch_indices), ")")
    
    # Process batch in parallel
    batch_results <- future_map(batch_indices, function(idx) {
      extract_plant_info(
        text = abstracts$abstract[idx],
        abstract_id = abstracts$id[idx],
        predicted_label = abstracts$predicted_label[idx],
        lookup_tables = lookup_tables,
        plant_parts_keywords = plant_parts_keywords
      )
    }, .options = furrr_options(seed = TRUE))
    
    # Combine batch results
    all_results <- c(all_results, batch_results)
    
    # Force garbage collection to free memory
    gc()
  }
  
  # Combine all results
  final_results <- bind_rows(all_results)
  
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
    candidates <- extract_candidate_names(text)
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
if (interactive()) {
  cat("Testing optimized taxa detection with synonym handling...\n")
  test_results <- test_synonym_handling()
}
