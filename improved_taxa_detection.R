# Improved Taxa Detection Functions
# Addressing issues with species detection, particularly for "Acer macrophyllum"

library(tidyverse)
library(rgbif)
library(stringr)
library(stringdist)  # For fuzzy matching

# Create lookup tables with improved structure
create_lookup_tables <- function(species_df) {
  species_df <- species_df %>%
    mutate(canonicalName_lower = tolower(canonicalName))
  
  accepted_species <- species_df %>%
    filter(taxonomicStatus == "accepted") %>%
    select(taxonID, canonicalName) %>%
    rename(canonicalName_accepted = canonicalName)
  
  genus_list <- species_df %>%
    filter(taxonomicStatus == "accepted", !is.na(genus)) %>%
    distinct(genus) %>%
    rename(canonicalName = genus) %>%
    mutate(canonicalName_lower = tolower(canonicalName))
  
  family_list <- species_df %>%
    filter(taxonomicStatus == "accepted", !is.na(family)) %>%
    distinct(family) %>%
    rename(canonicalName = family) %>%
    mutate(canonicalName_lower = tolower(canonicalName))
  
  return(list(
    species_df = species_df,
    accepted_species = accepted_species,
    genus_list = genus_list,
    family_list = family_list
  ))
}

# Improved capitalization correction
correct_capitalization <- function(name) {
  if (is.na(name) || name == "") return(name)
  words <- str_split(name, " ")[[1]]
  words <- str_to_lower(words)
  words[1] <- str_to_title(words[1])
  paste(words, collapse = " ")
}

# Improved extract_candidate_names with better regex patterns
extract_candidate_names <- function(text) {
  # IMPROVEMENT 1: Enhanced pattern for "Genus species" that handles punctuation
  # This pattern captures species names even when followed by punctuation
  genus_species <- str_extract_all(text, "\\b[A-Z][a-z]+\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  genus_species <- str_replace_all(genus_species, "[,\\.;:\\)\\(]$", "")  # Remove trailing punctuation
  
  # IMPROVEMENT 2: Better pattern for abbreviated genus names
  abbreviated <- str_extract_all(text, "\\b[A-Z]\\.\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
  abbreviated <- str_replace_all(abbreviated, "[,\\.;:\\)\\(]$", "")  # Remove trailing punctuation
  
  # Extract bigrams from lowercase text for additional candidates
  text_lower <- tolower(text)
  
  # IMPROVEMENT 3: Extract n-grams to preserve multi-word taxa
  # First extract bigrams (two-word combinations)
  bigram_pattern <- "\\b[a-z]+\\s+[a-z]+\\b"
  bigrams <- str_extract_all(text_lower, bigram_pattern)[[1]]
  
  # Filter bigrams to only those that might be species names
  potential_bigrams <- bigrams[nchar(bigrams) > 5 & nchar(bigrams) < 40]
  
  # IMPROVEMENT 4: Look for taxonomic indicators
  # Extract phrases with taxonomic indicators
  taxonomic_indicators <- c("sp\\.", "spp\\.", "var\\.", "subsp\\.", "f\\.", "cf\\.")
  indicator_pattern <- paste0("\\b[a-z]+\\s+", paste(taxonomic_indicators, collapse = "|"), "\\b")
  indicator_matches <- str_extract_all(text_lower, indicator_pattern)[[1]]
  
  # IMPROVEMENT 5: Store abbreviated genus references for later expansion
  # This will help with cases like "A. macrophyllum" by storing the abbreviation pattern
  abbrev_refs <- str_extract_all(text, "\\b[A-Z]\\.\\s+[a-z]+\\b")[[1]]
  
  # Combine all candidate names
  candidates <- unique(c(
    tolower(genus_species), 
    tolower(abbreviated), 
    potential_bigrams,
    indicator_matches,
    tolower(abbrev_refs)  # Add abbreviated references
  ))
  
  return(candidates)
}

# Improved batch_validate_names with fuzzy matching
batch_validate_names <- function(names, lookup_tables, use_fuzzy = TRUE, max_distance = 2) {
  if (length(names) == 0) return(tibble())
  
  # Filter out invalid names
  names <- unique(names[!is.na(names) & names != ""])
  
  # Skip processing if too many names (likely noise)
  if (length(names) > 200) {
    message("Large number of candidate names (", length(names), "), filtering to most likely candidates")
    # Keep only names that look like species names (contain a space)
    names <- names[grepl(" ", names)]
    # Further limit if still too many
    if (length(names) > 100) {
      names <- names[1:100]
    }
  }
  
  # IMPROVEMENT 5: Better abbreviation expansion
  expanded_names <- character(0)
  
  # Find abbreviated genus patterns like "A. macrophyllum"
  abbrev_matches <- names[grepl("^[A-Za-z]\\.\\s+[a-z]+", names)]
  
  if (length(abbrev_matches) > 0) {
    for (name in abbrev_matches) {
      # Extract first letter and species epithet
      parts <- strsplit(name, "\\.")[[1]]
      if (length(parts) < 2) next
      
      first_letter <- tolower(substr(parts[1], 1, 1))
      species_epithet <- trimws(gsub("^\\s+", "", parts[2]))
      
      # Get all genera starting with that letter
      potential_genera <- lookup_tables$genus_list %>%
        filter(str_starts(canonicalName_lower, first_letter)) %>%
        pull(canonicalName)
      
      # Try each potential genus
      for (gen in potential_genera) {
        full_name_candidate <- paste(gen, species_epithet)
        if (tolower(full_name_candidate) %in% lookup_tables$species_df$canonicalName_lower) {
          expanded_names <- c(expanded_names, full_name_candidate)
          # Don't break after first match - collect all possible matches
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
    validated <- potential_matches %>%
      left_join(lookup_tables$species_df, 
                by = c("user_supplied_name_lower" = "canonicalName_lower"), 
                keep = TRUE)
    
    # Handle synonyms
    syns <- validated %>%
      filter(taxonomicStatus != "accepted" & !is.na(acceptedNameUsageID)) %>%
      left_join(lookup_tables$accepted_species, 
                by = c("acceptedNameUsageID" = "taxonID")) %>%
      mutate(canonicalName = coalesce(canonicalName_accepted, user_supplied_name)) %>%
      select(-canonicalName_accepted)
    
    reals <- validated %>%
      filter(taxonomicStatus == "accepted")
    
    resolved <- bind_rows(syns, reals) %>%
      filter(!is.na(canonicalName)) %>%
      mutate(
        resolved_name = canonicalName,
        status = ifelse(!is.na(kingdom), "ACCEPTED", "NO_MATCH"),
        acceptedScientificName = ifelse(status == "ACCEPTED", canonicalName, NA)
      )
  } else {
    resolved <- tibble()
  }
  
  # IMPROVEMENT 6: Fuzzy matching for unresolved names
  if (use_fuzzy && length(names) > 0) {
    unresolved <- names_df %>%
      filter(!user_supplied_name %in% resolved$user_supplied_name) %>%
      filter(nchar(user_supplied_name) >= 5)  # Skip short names
    
    if (nrow(unresolved) > 0 && nrow(unresolved) <= 50) {  # Limit fuzzy matching to reasonable number
      # Get sample of species names to compare against
      species_sample <- lookup_tables$species_df %>%
        filter(taxonomicStatus == "accepted") %>%
        sample_n(min(5000, nrow(.))) %>%  # Limit to 5000 for performance
        pull(canonicalName_lower)
      
      fuzzy_matches <- tibble()
      
      for (i in 1:nrow(unresolved)) {
        name_to_check <- unresolved$user_supplied_name_lower[i]
        
        # Only check names with a space (likely species names)
        if (!grepl(" ", name_to_check)) next
        
        # Calculate string distances
        distances <- stringdist(name_to_check, species_sample, method = "jw")
        
        # Find closest matches
        min_dist_idx <- which(distances == min(distances))
        if (length(min_dist_idx) > 0 && min(distances) <= max_distance) {
          closest_match <- species_sample[min_dist_idx[1]]
          
          # Look up the full species info
          match_info <- lookup_tables$species_df %>%
            filter(canonicalName_lower == closest_match) %>%
            mutate(
              user_supplied_name = unresolved$user_supplied_name[i],
              user_supplied_name_lower = name_to_check,
              resolved_name = canonicalName,
              status = "FUZZY_MATCH",
              acceptedScientificName = canonicalName,
              match_distance = min(distances)
            )
          
          fuzzy_matches <- bind_rows(fuzzy_matches, match_info)
        }
      }
      
      # Add fuzzy matches to resolved names
      if (nrow(fuzzy_matches) > 0) {
        resolved <- bind_rows(resolved, fuzzy_matches)
      }
    }
  }
  
  return(resolved)
}

# Improved process_taxonomic_matches with n-gram approach
process_taxonomic_matches <- function(valid_species, lookup_tables, text, 
                                     abstract_id, predicted_label) {
  all_rows <- list()
  
  # IMPROVEMENT 7: Create tokens and n-grams for better matching
  # Create basic tokens
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
  
  # Species matches - add better error handling
  if (is.data.frame(valid_species) && nrow(valid_species) > 0) {
    # Match against both individual tokens and bigrams
    species_matches <- valid_species %>%
      filter(tolower(resolved_name) %in% all_tokens)
    
    if (nrow(species_matches) > 0) {
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
        lookup_tables$species_df %>% 
          filter(taxonomicStatus == "accepted") %>% 
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
        lookup_tables$species_df %>% 
          filter(taxonomicStatus == "accepted") %>% 
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

# Improved extract_plant_info function
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
  valid_species <- batch_validate_names(plant_candidates, lookup_tables, use_fuzzy = TRUE)
  
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

# Test function to demonstrate the improvements
test_improved_detection <- function() {
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  # Test cases
  test_cases <- list(
    "Standard" = "This study examines the endophytic fungi associated with Acer macrophyllum (bigleaf maple) in the Pacific Northwest.",
    "With punctuation" = "Acer macrophyllum, commonly known as bigleaf maple, hosts diverse fungal communities.",
    "End of sentence" = "The primary tree species in our study area was Acer macrophyllum.",
    "Abbreviated" = "After initial identification, A. macrophyllum was found to host over 20 fungal species.",
    "Multiple species" = "We compared Acer macrophyllum and Pseudotsuga menziesii in our study area."
  )
  
  # Define plant parts for testing
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  # Run tests
  results <- list()
  
  for (name in names(test_cases)) {
    cat("\n===", name, "===\n")
    text <- test_cases[[name]]
    cat("Text:", text, "\n")
    
    # Extract and validate names
    candidates <- extract_candidate_names(text)
    cat("Extracted candidates:", paste(candidates, collapse = ", "), "\n")
    
    valid_species <- batch_validate_names(candidates, lookup_tables)
    cat("Valid species found:", nrow(valid_species), "\n")
    if (nrow(valid_species) > 0) {
      print(valid_species %>% select(user_supplied_name, resolved_name, status))
    }
    
    # Process matches
    matches <- process_taxonomic_matches(
      valid_species, lookup_tables, text, 
      abstract_id = 1, predicted_label = "Presence"
    )
    
    # Show results
    if (length(matches) > 0) {
      final_df <- bind_rows(matches)
      cat("Final matches:", nrow(final_df), "\n")
      print(final_df %>% select(match_type, resolved_name, status))
      results[[name]] <- final_df
    } else {
      cat("No matches found!\n")
      results[[name]] <- NULL
    }
  }
  
  return(results)
}

# Run the test if this script is executed directly
if (interactive()) {
  cat("Running test of improved taxa detection...\n")
  test_results <- test_improved_detection()
}
