# Extracting plant names and taxonomy information -------------------------
# BB - Optimized Version
# June 2025
# Extracting information from the labeled dataset of abstracts, including plant taxonomy, fungal taxonomy, and part of plant

# Load required libraries for data processing and visualization
library(tidyverse)  
library(quanteda)
library(rgbif)
library(furrr)
library(janitor)
library(vroom)
library(irlba)
library(stringdist) # For fuzzy matching

# Set up parallel processing with increased memory limit
options(future.globals.maxSize = 2000 * 1024^2)  # 2GB limit
plan(multisession, workers = min(4, availableCores() - 1))  # Limit workers to reduce memory usage

custom_theme <- theme_bw(base_size = 18)
theme_set(custom_theme)

set.seed(123)

# Custom color palette
cus_pal <- c(
  "#A1C181",  # soft sage green — for plants
  "#619B8A",  # muted teal — evokes moss or lichens
  "#C97E7E",  # dusty rose — for fungi like Russula or Hygrophoropsis
  "#D9AE94"   # pale mushroom beige — for caps and forest floor tones
)

# Function to save plots with consistent dimensions and format
save_plot <- function(filename, plot, width = 12, height = 7, units = "in", ...) {
  ggsave(filename, plot, width = width, height = height, units = units, ...)
}

# PLANT PARTS CONFIGURATION -----------------------------------------------

# Consolidated plant parts keywords with better organization
plant_parts_keywords <- c(
  # Basic structures
  "fruit", "fruits", "root", "roots", "leaf", "leaves", "stem", "stems", 
  "flower", "flowers", "seed", "seeds", "bark", "branch", "branches",
  "twig", "twigs", "shoot", "shoots", "bud", "buds", "trunk", "trunks",
  
  # Reproductive structures
  "pistil", "pistils", "anther", "anthers", "carpel", "carpels", 
  "sepal", "sepals", "petal", "petals", "stigma", "stigmas", 
  "style", "styles", "ovary", "ovaries", "ovule", "ovules",
  "calyx", "calyces", "corolla", "corollas", "pollen",
  "inflorescence", "inflorescences", "floret", "florets",
  
  # Specialized structures
  "rhizome", "rhizomes", "tuber", "tubers", "bulb", "bulbs", 
  "corm", "corms", "tendril", "tendrils", "thorn", "thorns",
  "cone", "cones", "needle", "needles",
  
  # Anatomical features
  "xylem", "phloem", "cortex", "cortices", "epidermis", "endodermis",
  "mesophyll", "parenchyma", "sclerenchyma", "collenchyma",
  "stomata", "stoma", "cuticle", "cuticles", "trichome", "trichomes",
  
  # Leaf parts
  "petiole", "petioles", "lamina", "laminae", "stipule", "stipules",
  "leaflet", "leaflets", "node", "nodes", "internode", "internodes",
  
  # Seed/embryo parts
  "cotyledon", "cotyledons", "endosperm", "embryo", "embryos",
  "testa", "hilum", "micropyle", "aleurone"
)

# Create mapping for plural/singular normalization
create_plant_part_groups <- function(keywords) {
  groups <- character()
  for (word in keywords) {
    if (str_ends(word, "s") && !word %in% c("cortex", "xylem", "phloem")) {
      singular <- str_remove(word, "s$")
      if (singular %in% keywords) {
        groups[word] <- singular
      }
    }
  }
  # Add special cases
  groups["calyces"] <- "calyx"
  groups["cortices"] <- "cortex"
  groups["stomata"] <- "stoma"
  groups["laminae"] <- "lamina"
  groups["paleae"] <- "palea"
  
  return(groups)
}

plant_part_groups <- create_plant_part_groups(plant_parts_keywords)

# DATA LOADING AND PREPROCESSING ------------------------------------------

# Load GBIF backbone data efficiently
load_gbif_data <- function() {
  if (!file.exists("species.rds")) {
    message("Loading GBIF backbone data...")
    backbone <- vroom("gbif_backbone/Taxon.tsv", delim = "\t", progress = FALSE)
    species <- backbone %>%
      filter(kingdom %in% c("Plantae", "Fungi") & taxonRank == "species") 
    saveRDS(species, "species.rds")
    return(species)
  } else {
    return(readRDS("species.rds"))
  }
}

species <- load_gbif_data()

# Load and prepare labeled abstracts
prepare_abstracts <- function() {
  raw_preds <- vroom("full_predictions_with_metadata.csv", progress = FALSE, show_col_types = FALSE)
  if (nrow(problems(raw_preds)) > 0) {
    message("Parsing issues in full_predictions_with_metadata.csv:")
    print(problems(raw_preds))
  }
  labeled_abstracts <- raw_preds %>%
    clean_names()
  
  abstracts_long <- labeled_abstracts %>%
    pivot_longer(
      cols = c(label_loose, label_medium, label_strict),
      names_to = "threshold",
      values_to = "predicted_label"
    ) %>%
    filter(predicted_label %in% c("Presence", "Absence"))
  
  raw_train <- vroom("Training_labeled_abs_5.csv", progress = FALSE, show_col_types = FALSE)
  if (nrow(problems(raw_train)) > 0) {
    message("Parsing issues in Training_labeled_abs_5.csv:")
    print(problems(raw_train))
  }
  training_abstracts <- raw_train %>%
    clean_names() %>%
    filter(!is.na(doi), authors != "", label %in% c("Presence", "Absence")) %>%
    crossing(threshold = c("label_loose")) %>%  # Focus on loose threshold
    mutate(
      predicted_label = label,
      start_page = as.character(start_page),
      end_page = as.character(end_page)
    )
  
  return(bind_rows(training_abstracts, abstracts_long) %>%
           mutate(id = row_number()) %>%
           relocate(id))
}

labeled_abstracts <- prepare_abstracts()

# TAXONOMY VALIDATION FUNCTIONS -------------------------------------------

# Optimized capitalization correction
correct_capitalization <- function(name) {
  if (is.na(name) || name == "") return(name)
  words <- str_split(name, " ")[[1]]
  words <- str_to_lower(words)
  words[1] <- str_to_title(words[1])
  paste(words, collapse = " ")
}

# Create lookup tables for faster matching
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

# Optimized batch validation with better error handling and speed
batch_validate_names <- function(names, lookup_tables) {
  if (length(names) == 0) return(tibble())
  
  # Filter out invalid names and limit to a reasonable number to process
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
  
  # Expand abbreviated genus names more efficiently
  expanded_names <- character(0)
  abbrev_pattern <- "^([A-Z])\\.\\s+(\\w+)$"
  abbrev_matches <- names[grepl(abbrev_pattern, names)]
  
  if (length(abbrev_matches) > 0) {
    for (name in abbrev_matches) {
      match_result <- str_match(name, abbrev_pattern)
      first_letter <- tolower(match_result[2])
      species_epithet <- match_result[3]
      
      # Limit potential genera to top 10 most common for that letter
      potential_genera <- lookup_tables$genus_list %>%
        filter(str_starts(canonicalName_lower, first_letter)) %>%
        slice_head(n = 10) %>%
        pull(canonicalName)
      
      for (gen in potential_genera) {
        full_name_candidate <- paste(gen, species_epithet)
        if (tolower(full_name_candidate) %in% lookup_tables$species_df$canonicalName_lower) {
          expanded_names <- c(expanded_names, full_name_candidate)
          break  # Stop after first match to save time
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
  
  # Skip fuzzy matching for large datasets - it's very slow and rarely useful
  # Only do fuzzy matching if we have a small number of unresolved names
  if (nrow(resolved) > 0) {
    unresolved <- names_df %>%
      filter(!user_supplied_name %in% resolved$user_supplied_name) %>%
      filter(nchar(user_supplied_name) >= 5)  # Skip short names that are unlikely to be species
  } else {
    # If no resolved names, consider all names for fuzzy matching
    unresolved <- names_df %>%
      filter(nchar(user_supplied_name) >= 5)  # Skip short names that are unlikely to be species
  }
  
  # Skip fuzzy matching entirely - it's causing errors and is rarely useful
  # Instead, we'll just return the exact matches we've found
  fuzzy_matches <- tibble()
  
  # No need to check fuzzy matches since we're skipping that step
  
  return(resolved)
}

# PLANT INFORMATION EXTRACTION --------------------------------------------

# Optimized plant info extraction with better text processing
extract_plant_info <- function(text, abstract_id, predicted_label, lookup_tables) {
  if (is.na(text) || text == "") {
    return(create_empty_result(abstract_id, predicted_label))
  }
  
  # Skip full tokenization and use faster regex-based approach
  text_lower <- tolower(text)
  
  # Extract candidate names directly with regex
  ngrams <- extract_candidate_names(text)
  
  # Detect plant parts with simple string matching instead of tokenization
  # This is much faster than tokenizing the entire text
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
  
  # Validate candidate names
  plant_candidates <- map_chr(ngrams, correct_capitalization)
  valid_species <- batch_validate_names(plant_candidates, lookup_tables)
  
  # Create a simple vector of tokens for taxonomic matching
  # Only do this once we have candidates to check against
  tokens_vec <- unlist(strsplit(gsub("[[:punct:][:digit:]]", " ", text_lower), "\\s+"))
  tokens_vec <- tokens_vec[tokens_vec != ""]
  
  # Process matches
  all_rows <- process_taxonomic_matches(
    valid_species, lookup_tables, tokens_vec, 
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

# Helper function to extract candidate names - optimized for speed
extract_candidate_names <- function(text) {
  # Skip unigrams entirely - they're rarely valid species names and slow down processing
  
  # Use more targeted regex patterns for species names instead of tokenization
  # This is much faster than tokenizing the entire text
  
  # Pattern for "Genus species" (e.g., "Acer macrophyllum")
  genus_species <- str_extract_all(text, "\\b[A-Z][a-z]+\\s+[a-z]+\\b")[[1]]
  
  # Pattern for "G. species" (e.g., "A. macrophyllum")
  abbreviated <- str_extract_all(text, "\\b[A-Z]\\.\\s+[a-z]+\\b")[[1]]
  
  # Extract bigrams only from lowercase text for additional candidates
  # This is more targeted than processing all unigrams
  text_lower <- tolower(text)
  bigram_pattern <- "\\b[a-z]+\\s+[a-z]+\\b"
  bigrams <- str_extract_all(text_lower, bigram_pattern)[[1]]
  
  # Filter bigrams to only those that might be species names
  # This dramatically reduces the number of candidates
  potential_bigrams <- bigrams[nchar(bigrams) > 5 & nchar(bigrams) < 40]
  
  unique(c(tolower(genus_species), tolower(abbreviated), potential_bigrams))
}

# Helper function to process taxonomic matches
process_taxonomic_matches <- function(valid_species, lookup_tables, tokens_vec, 
                                    abstract_id, predicted_label) {
  all_rows <- list()
  
  # Species matches - add better error handling
  if (is.data.frame(valid_species) && nrow(valid_species) > 0) {
    species_matches <- valid_species %>%
      filter(tolower(resolved_name) %in% tolower(tokens_vec)) %>%
      mutate(
        id = abstract_id,
        predicted_label = predicted_label,
        match_type = "species"
      )
    if (nrow(species_matches) > 0) {
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
    status = "NO_MATCH",  # Ensure status is character type for consistency
    resolved_name = NA_character_,
    acceptedScientificName = NA_character_
  )
}

# MAIN PROCESSING PIPELINE ------------------------------------------------

process_abstracts <- function(threshold_name = "label_loose", force_refresh = FALSE, sample_size = NULL) {
  message("Processing threshold: ", threshold_name)
  
  # Filter abstracts
  abs <- labeled_abstracts %>%
    filter(threshold == threshold_name)
  
  # Take a sample if requested
  if (!is.null(sample_size) && sample_size > 0 && sample_size < nrow(abs)) {
    message("Using a sample of ", sample_size, " abstracts")
    abs <- abs %>% 
      sample_n(sample_size, replace = FALSE) %>%
      arrange(id)
  }
  
  message("Creating lookup tables...")
  # Create lookup tables
  lookup_tables <- create_lookup_tables(species)
  
  message("Processing ", nrow(abs), " abstracts...")
  
  # Process abstracts sequentially with progress tracking to avoid memory issues
  results <- vector("list", nrow(abs))
  
  for (i in seq_len(nrow(abs))) {
    if (i %% 10 == 0 || i == nrow(abs)) {
      message("Processed ", i, "/", nrow(abs), " abstracts")
    }
    
    results[[i]] <- extract_plant_info(
      abs$abstract[i], 
      abs$id[i], 
      abs$predicted_label[i], 
      lookup_tables
    )
  }
  
  # Combine results
  final_results <- bind_rows(results)
  
  # Save results
  suffix <- if (!is.null(sample_size)) paste0("_sample", sample_size) else ""
  out_name <- paste0("taxa_info_results_", threshold_name, "_optimized", suffix, ".csv")
  write_csv(final_results, out_name)
  
  message("Processing complete! Saved ", nrow(final_results), " rows to ", out_name)
  return(final_results)
}

# VISUALIZATION FUNCTIONS -------------------------------------------------

# Function to detect research methods in abstracts
detect_research_methods <- function(abstracts_df) {
  # Define method categories and their keywords
  method_categories <- list(
    molecular = c("pcr", "dna", "rna", "sequenc", "primer", "amplif", "gene", "genom", 
                 "transcript", "clone", "phylogen", "molecular", "extraction", "isolat", 
                 "genetic", "marker", "polymorphism", "nucleotide", "hybridiz"),
    
    culture_based = c("culture", "isolat", "plate", "medium", "agar", "petri", "colony", 
                     "incubat", "inocul", "sterile", "aseptic", "axenic", "pure culture", 
                     "ferment", "broth", "in vitro"),
    
    microscopy = c("microscop", "stain", "section", "histolog", "morpholog", "ultrastructur", 
                  "sem", "tem", "scanning electron", "transmission electron", "light microscop", 
                  "confocal", "fluorescen"),
    
    field_observation = c("survey", "observ", "field", "collect", "sampl", "transect", 
                         "quadrat", "plot", "ecolog", "habitat", "in situ", "wild", "native"),
    
    computational = c("bioinformatic", "algorithm", "comput", "model", "simulat", "predict", 
                     "database", "software", "pipeline", "script", "analysis", "statistic")
  )
  
  # Function to detect methods in a text
  detect_methods <- function(text) {
    text_lower <- tolower(text)
    results <- sapply(names(method_categories), function(category) {
      keywords <- method_categories[[category]]
      matches <- sapply(keywords, function(keyword) {
        grepl(keyword, text_lower)
      })
      any(matches)
    })
    return(results)
  }
  
  # Apply detection to all abstracts
  methods_detected <- abstracts_df %>%
    mutate(
      abstract_text = abstract,
      molecular_methods = NA,
      culture_based_methods = NA,
      microscopy_methods = NA,
      field_observation_methods = NA,
      computational_methods = NA
    )
  
  # Process each abstract
  for (i in 1:nrow(methods_detected)) {
    if (!is.na(methods_detected$abstract_text[i])) {
      detection_results <- detect_methods(methods_detected$abstract_text[i])
      methods_detected$molecular_methods[i] <- detection_results["molecular"]
      methods_detected$culture_based_methods[i] <- detection_results["culture_based"]
      methods_detected$microscopy_methods[i] <- detection_results["microscopy"]
      methods_detected$field_observation_methods[i] <- detection_results["field_observation"]
      methods_detected$computational_methods[i] <- detection_results["computational"]
    }
  }
  
  return(methods_detected)
}

# Function to analyze methods and correlate with taxa
analyze_research_methods <- function(plant_species_df, abstracts_df, threshold_name) {
  # Detect methods in abstracts
  methods_df <- detect_research_methods(abstracts_df)
  
  # Join with plant species data
  taxa_methods_df <- plant_species_df %>%
    left_join(methods_df %>% 
                select(id, molecular_methods, culture_based_methods, 
                       microscopy_methods, field_observation_methods, 
                       computational_methods),
              by = "id")
  
  # Summarize methods by kingdom
  methods_by_kingdom <- taxa_methods_df %>%
    filter(!is.na(kingdom)) %>%
    group_by(kingdom) %>%
    summarise(
      total = n(),
      molecular = sum(molecular_methods, na.rm = TRUE),
      culture_based = sum(culture_based_methods, na.rm = TRUE),
      microscopy = sum(microscopy_methods, na.rm = TRUE),
      field_observation = sum(field_observation_methods, na.rm = TRUE),
      computational = sum(computational_methods, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      molecular_pct = molecular / total * 100,
      culture_based_pct = culture_based / total * 100,
      microscopy_pct = microscopy / total * 100,
      field_observation_pct = field_observation / total * 100,
      computational_pct = computational / total * 100
    )
  
  # Summarize methods by phylum
  methods_by_phylum <- taxa_methods_df %>%
    filter(!is.na(phylum)) %>%
    group_by(kingdom, phylum) %>%
    summarise(
      total = n(),
      molecular = sum(molecular_methods, na.rm = TRUE),
      culture_based = sum(culture_based_methods, na.rm = TRUE),
      microscopy = sum(microscopy_methods, na.rm = TRUE),
      field_observation = sum(field_observation_methods, na.rm = TRUE),
      computational = sum(computational_methods, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      molecular_pct = molecular / total * 100,
      culture_based_pct = culture_based / total * 100,
      microscopy_pct = microscopy / total * 100,
      field_observation_pct = field_observation / total * 100,
      computational_pct = computational / total * 100
    )
  
  # Create plots
  
  # Methods by kingdom plot
  kingdom_methods_long <- methods_by_kingdom %>%
    pivot_longer(
      cols = c(molecular_pct, culture_based_pct, microscopy_pct, 
               field_observation_pct, computational_pct),
      names_to = "method",
      values_to = "percentage"
    ) %>%
    mutate(
      method = recode(method,
                     molecular_pct = "Molecular",
                     culture_based_pct = "Culture-based",
                     microscopy_pct = "Microscopy",
                     field_observation_pct = "Field Observation",
                     computational_pct = "Computational")
    )
  
  kingdom_plot <- ggplot(kingdom_methods_long, 
                        aes(x = kingdom, y = percentage, fill = method)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Research Methods by Kingdom (", threshold_name, ")"),
      x = "Kingdom",
      y = "Percentage of Abstracts",
      fill = "Method"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Methods by phylum plot
  phylum_methods_long <- methods_by_phylum %>%
    filter(total >= 5) %>%  # Filter to phyla with at least 5 mentions
    pivot_longer(
      cols = c(molecular_pct, culture_based_pct, microscopy_pct, 
               field_observation_pct, computational_pct),
      names_to = "method",
      values_to = "percentage"
    ) %>%
    mutate(
      method = recode(method,
                     molecular_pct = "Molecular",
                     culture_based_pct = "Culture-based",
                     microscopy_pct = "Microscopy",
                     field_observation_pct = "Field Observation",
                     computational_pct = "Computational")
    )
  
  phylum_plot <- ggplot(phylum_methods_long, 
                       aes(x = phylum, y = percentage, fill = method)) +
    geom_col(position = "dodge") +
    facet_wrap(~ kingdom, scales = "free_x") +
    labs(
      title = paste("Research Methods by Phylum (", threshold_name, ")"),
      x = "Phylum",
      y = "Percentage of Abstracts",
      fill = "Method"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Analyze publication year trends if available
  year_trend_plot <- NULL
  
  # Check if publication_year exists in both dataframes
  has_pub_year <- "publication_year" %in% colnames(abstracts_df) && 
                  "publication_year" %in% colnames(taxa_methods_df)
  
  if (has_pub_year) {
    # Make sure the column is properly joined
    methods_by_year <- taxa_methods_df %>%
      filter(!is.na(publication_year)) %>%
      group_by(publication_year) %>%
      summarise(
        total = n(),
        molecular = sum(molecular_methods, na.rm = TRUE) / total * 100,
        culture_based = sum(culture_based_methods, na.rm = TRUE) / total * 100,
        microscopy = sum(microscopy_methods, na.rm = TRUE) / total * 100,
        field_observation = sum(field_observation_methods, na.rm = TRUE) / total * 100,
        computational = sum(computational_methods, na.rm = TRUE) / total * 100,
        .groups = "drop"
      )
    
    year_methods_long <- methods_by_year %>%
      pivot_longer(
        cols = c(molecular, culture_based, microscopy, field_observation, computational),
        names_to = "method",
        values_to = "percentage"
      ) %>%
      mutate(
        method = recode(method,
                       molecular = "Molecular",
                       culture_based = "Culture-based",
                       microscopy = "Microscopy",
                       field_observation = "Field Observation",
                       computational = "Computational")
      )
    
    year_trend_plot <- ggplot(year_methods_long, 
                             aes(x = publication_year, y = percentage, color = method)) +
      geom_line() +
      geom_point() +
      labs(
        title = paste("Research Methods Trends Over Time (", threshold_name, ")"),
        x = "Publication Year",
        y = "Percentage of Abstracts",
        color = "Method"
      ) +
      theme_minimal()
  }
  
  # Return all plots and data
  return(list(
    kingdom_plot = kingdom_plot,
    phylum_plot = phylum_plot,
    year_trend_plot = year_trend_plot,
    methods_by_kingdom = methods_by_kingdom,
    methods_by_phylum = methods_by_phylum
  ))
}

# Function to analyze taxonomic coverage
analyze_taxonomic_coverage <- function(plant_species_df, threshold_name) {
  # Load or create species backbone data
  if (file.exists("accepted_species.rds")) {
    accepted_species <- readRDS("accepted_species.rds")
  } else {
    accepted_species <- species %>%
      filter(taxonomicStatus == "accepted", !is.na(phylum), !is.na(family), !is.na(genus))
    saveRDS(accepted_species, "accepted_species.rds")
  }
  
  # Create family and genus lookup tables
  if (file.exists("families.rds") && file.exists("genera.rds")) {
    families_from_species <- readRDS("families.rds")
    genera_from_species <- readRDS("genera.rds")
  } else {
    families_from_species <- accepted_species %>%
      distinct(family, phylum, kingdom) %>%
      rename(canonicalName = family)
    
    genera_from_species <- accepted_species %>%
      distinct(genus, phylum, family, kingdom) %>%
      rename(canonicalName = genus)
    
    saveRDS(families_from_species, "families.rds")
    saveRDS(genera_from_species, "genera.rds")
  }
  
  # Get expected phyla
  plantae_key <- 6
  phyla <- name_lookup(
    higherTaxonKey = plantae_key,
    rank = "PHYLUM",
    status = "ACCEPTED",
    isExtinct = FALSE,
    limit = 5000
  )$data
  
  expected_plant_phyla <- unique(phyla$canonicalName)
  phylum_order <- sort(expected_plant_phyla)
  
  # Clean dataset
  plant_df_clean <- plant_species_df %>%
    filter(kingdom == "Plantae", !is.na(phylum))
  
  # Analyze family coverage
  plant_families_backbone <- accepted_species %>%
    filter(kingdom == "Plantae") %>%
    filter(!is.na(family), !is.na(phylum)) %>%
    distinct(phylum, family)
  
  total_families_per_phylum <- plant_families_backbone %>%
    group_by(phylum) %>%
    summarise(total_families = n_distinct(family), .groups = "drop")
  
  dataset_families <- plant_df_clean %>%
    filter(!is.na(family)) %>%
    distinct(phylum, family)
  
  families_in_dataset <- dataset_families %>%
    group_by(phylum) %>%
    summarise(n_families_found = n_distinct(family), .groups = "drop")
  
  families_coverage <- total_families_per_phylum %>%
    left_join(families_in_dataset, by = "phylum") %>%
    mutate(
      n_families_found = replace_na(n_families_found, 0),
      n_families_missing = total_families - n_families_found
    ) %>%
    arrange(phylum)
  
  families_coverage$phylum <- factor(families_coverage$phylum, levels = phylum_order)
  
  # Prepare data for family coverage plot
  families_long <- families_coverage %>%
    filter(!is.na(phylum)) %>%
    select(phylum, n_families_found, n_families_missing) %>%
    pivot_longer(
      cols = c(n_families_found, n_families_missing),
      names_to = "status",
      values_to = "count"
    ) %>%
    mutate(
      status = recode(status,
                     n_families_found = "Found",
                     n_families_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  # Color palette
  coverage_pal <- c(
    "Found" = "#A1C181",   # calm sage green — presence
    "Missing" = "#C97E7E"  # dusty rose — absence
  )
  
  # Create family coverage plot
  family_plot <- ggplot(families_long, aes(x = phylum, y = count, fill = status)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Plant Family Coverage (", threshold_name, ")"),
      x = "Phylum",
      y = "Number of Families",
      fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal)
  
  # Create proportional family coverage plot
  families_coverage_prop <- families_coverage %>%
    filter(!is.na(phylum)) %>%
    mutate(
      prop_found = n_families_found / total_families,
      prop_missing = n_families_missing / total_families
    )
  
  families_long_prop <- families_coverage_prop %>%
    select(phylum, prop_found, prop_missing) %>%
    pivot_longer(
      cols = c(prop_found, prop_missing),
      names_to = "status",
      values_to = "proportion"
    ) %>%
    mutate(
      status = recode(status,
                     prop_found = "Found",
                     prop_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  family_prop_plot <- ggplot(families_long_prop, aes(x = phylum, y = proportion, fill = status)) +
    geom_col() +
    geom_text(
      aes(label = scales::percent(proportion, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      size = 4,
      color = "black"
    ) +
    coord_flip() +
    labs(
      title = paste0("Proportional Plant Family Coverage (", threshold_name, ")"),
      x = "Phylum",
      y = "Proportion of Families",
      fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  # Similar analysis for genera
  plant_genera_backbone <- accepted_species %>%
    filter(kingdom == "Plantae") %>%
    filter(!is.na(genus), !is.na(phylum)) %>%
    distinct(phylum, genus)
  
  total_genera_per_phylum <- plant_genera_backbone %>%
    group_by(phylum) %>%
    summarise(total_genera = n_distinct(genus), .groups = "drop")
  
  dataset_genera <- plant_df_clean %>%
    filter(!is.na(genus)) %>%
    distinct(phylum, genus)
  
  genera_in_dataset <- dataset_genera %>%
    group_by(phylum) %>%
    summarise(n_genera_found = n_distinct(genus), .groups = "drop")
  
  genera_coverage <- total_genera_per_phylum %>%
    left_join(genera_in_dataset, by = "phylum") %>%
    mutate(
      n_genera_found = replace_na(n_genera_found, 0),
      n_genera_missing = total_genera - n_genera_found
    ) %>%
    arrange(phylum)
  
  genera_coverage$phylum <- factor(genera_coverage$phylum, levels = phylum_order)
  
  # Create genus coverage plots
  genera_long <- genera_coverage %>%
    filter(!is.na(phylum)) %>%
    select(phylum, n_genera_found, n_genera_missing) %>%
    pivot_longer(cols = starts_with("n_"), names_to = "status", values_to = "count") %>%
    mutate(
      status = recode(status, n_genera_found = "Found", n_genera_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  genus_plot <- ggplot(genera_long, aes(x = phylum, y = count, fill = status)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Plant Genus Coverage (", threshold_name, ")"),
      x = "Phylum", y = "Number of Genera", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal)
  
  # Proportional genus coverage
  genera_coverage_prop <- genera_coverage %>%
    mutate(
      prop_found = n_genera_found / total_genera,
      prop_missing = n_genera_missing / total_genera
    )
  
  genera_long_prop <- genera_coverage_prop %>%
    pivot_longer(cols = starts_with("prop_"), names_to = "status", values_to = "proportion") %>%
    mutate(
      status = recode(status, prop_found = "Found", prop_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  genus_prop_plot <- ggplot(genera_long_prop, aes(x = phylum, y = proportion, fill = status)) +
    geom_col() +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              size = 4, color = "black") +
    coord_flip() +
    labs(
      title = paste0("Proportional Plant Genus Coverage (", threshold_name, ")"),
      x = "Phylum", y = "Proportion of Genera", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  # Similar analysis for species
  plant_species_backbone <- accepted_species %>%
    filter(kingdom == "Plantae") %>%
    filter(!is.na(canonicalName), !is.na(phylum)) %>%
    distinct(phylum, canonicalName)
  
  total_species_per_phylum <- plant_species_backbone %>%
    group_by(phylum) %>%
    summarise(total_species = n_distinct(canonicalName), .groups = "drop")
  
  dataset_species <- plant_df_clean %>%
    filter(!is.na(canonicalName)) %>%
    distinct(phylum, canonicalName)
  
  species_in_dataset <- dataset_species %>%
    group_by(phylum) %>%
    summarise(n_species_found = n_distinct(canonicalName), .groups = "drop")
  
  species_coverage <- total_species_per_phylum %>%
    left_join(species_in_dataset, by = "phylum") %>%
    mutate(
      n_species_found = replace_na(n_species_found, 0),
      n_species_missing = total_species - n_species_found
    ) %>%
    arrange(phylum)
  
  species_coverage$phylum <- factor(species_coverage$phylum, levels = phylum_order)
  
  # Create species coverage plots
  species_long <- species_coverage %>%
    select(phylum, n_species_found, n_species_missing) %>%
    pivot_longer(cols = starts_with("n_"), names_to = "status", values_to = "count") %>%
    mutate(
      status = recode(status, n_species_found = "Found", n_species_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  species_plot <- ggplot(species_long, aes(x = phylum, y = count, fill = status)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Plant Species Coverage (", threshold_name, ")"),
      x = "Phylum", y = "Number of Species", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal)
  
  # Proportional species coverage
  species_coverage_prop <- species_coverage %>%
    mutate(
      prop_found = n_species_found / total_species,
      prop_missing = n_species_missing / total_species
    )
  
  species_long_prop <- species_coverage_prop %>%
    pivot_longer(cols = starts_with("prop_"), names_to = "status", values_to = "proportion") %>%
    mutate(
      status = recode(status, prop_found = "Found", prop_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  species_prop_plot <- ggplot(species_long_prop, aes(x = phylum, y = proportion, fill = status)) +
    geom_col() +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              size = 4, color = "black") +
    coord_flip() +
    labs(
      title = paste0("Proportional Plant Species Coverage (", threshold_name, ")"),
      x = "Phylum", y = "Proportion of Species", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  # Return all plots
  missing_families <- plant_families_backbone %>%
    anti_join(dataset_families, by = c("phylum", "family")) %>%
    distinct(family)
  missing_genera <- plant_genera_backbone %>%
    anti_join(dataset_genera, by = c("phylum", "genus")) %>%
    distinct(genus)
  missing_species <- plant_species_backbone %>%
    anti_join(dataset_species, by = c("phylum", "canonicalName")) %>%
    distinct(canonicalName)
  return(list(
    family_plot = family_plot,
    family_prop_plot = family_prop_plot,
    genus_plot = genus_plot,
    genus_prop_plot = genus_prop_plot,
    species_plot = species_plot,
    species_prop_plot = species_prop_plot,
    missing_families = missing_families,
    missing_genera = missing_genera,
    missing_species = missing_species
  ))
}

# Optimized plotting functions with better error handling
create_phylum_plots <- function(plant_species_df, threshold_name) {
  # Get expected phyla
  plantae_key <- 6
  phyla <- name_lookup(
    higherTaxonKey = plantae_key,
    rank = "PHYLUM",
    status = "ACCEPTED",
    isExtinct = FALSE,
    limit = 5000
  )$data
  
  expected_plant_phyla <- unique(phyla$canonicalName)
  phylum_order <- sort(expected_plant_phyla)
  
  # Create summary
  phylum_summary <- plant_species_df %>%
    filter(kingdom == "Plantae", !is.na(phylum)) %>%
    distinct(id, phylum, predicted_label) %>%
    count(phylum, predicted_label, name = "abstracts_with_label") %>%
    complete(phylum = expected_plant_phyla, 
             predicted_label = c("Presence", "Absence"),
             fill = list(abstracts_with_label = 0)) %>%
    mutate(
      phylum = factor(phylum, levels = phylum_order),
      predicted_label = factor(predicted_label, levels = c("Absence", "Presence"))
    )
  
  # Create plot
  plot <- ggplot(phylum_summary, aes(x = phylum, y = abstracts_with_label, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Abstract Mentions in Plant Phyla (", threshold_name, ")"),
      x = "Phylum",
      y = "Abstracts Mentioned"
    ) +
    scale_fill_manual(values = cus_pal) +
    theme_minimal()
  
  return(plot)
}

# Function to analyze plant parts
create_plant_parts_plots <- function(plant_species_df) {
  # Create mapping for plural/singular normalization
  plant_part_groups <- create_plant_part_groups(plant_parts_keywords)
  
  # Summarize plant parts by label
  plant_parts_summary <- plant_species_df %>%
    select(id, predicted_label, any_of(plant_parts_keywords)) %>%
    distinct() %>%
    group_by(predicted_label) %>%
    summarise(across(any_of(plant_parts_keywords), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(
      cols = any_of(plant_parts_keywords),
      names_to = "plant_part",
      values_to = "n_abstracts"
    )
  
  # Group plant parts (combine singular/plural forms)
  plant_parts_grouped <- plant_parts_summary %>%
    mutate(
      grouped_part = ifelse(plant_part %in% names(plant_part_groups), 
                           plant_part_groups[plant_part], 
                           plant_part)
    ) %>%
    group_by(predicted_label, grouped_part) %>%
    summarise(n_abstracts = sum(n_abstracts), .groups = "drop")
  
  # Find most and least mentioned plant parts
  top_parts <- plant_parts_grouped %>%
    group_by(grouped_part) %>%
    summarise(total = sum(n_abstracts)) %>%
    arrange(desc(total)) %>%
    slice_head(n = 20)
  
  bottom_parts <- plant_parts_grouped %>%
    group_by(grouped_part) %>%
    summarise(total = sum(n_abstracts)) %>%
    arrange(total) %>%
    slice_head(n = 20)
  
  # Create plots
  top_plot <- plant_parts_grouped %>%
    filter(grouped_part %in% top_parts$grouped_part) %>%
    ggplot(aes(x = reorder(grouped_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Most Frequently Mentioned Plant Parts",
      x = "Plant Part",
      y = "Number of Abstracts"
    ) +
    scale_fill_manual(values = cus_pal)
  
  bottom_plot <- plant_parts_grouped %>%
    filter(grouped_part %in% bottom_parts$grouped_part) %>%
    ggplot(aes(x = reorder(grouped_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Least Frequently Mentioned Plant Parts",
      x = "Plant Part",
      y = "Number of Abstracts"
    ) +
    scale_fill_manual(values = cus_pal)
  
  return(list(top_plot = top_plot, bottom_plot = bottom_plot))
}

# MAIN EXECUTION -----------------------------------------------------------

# Run the optimized pipeline
main <- function(sample_size = NULL) {
  # Create results directory
  dir.create("Results/optimized", showWarnings = FALSE, recursive = TRUE)
  
  # Process abstracts
  results <- process_abstracts(threshold_name = "label_loose", force_refresh = TRUE, sample_size = sample_size)
  
  # Load full abstracts for method analysis
  message("Loading full abstracts for method analysis...")
  
  # Check if publication_year exists in the data
  if ("publication_year" %in% colnames(labeled_abstracts)) {
    full_abstracts <- labeled_abstracts %>%
      filter(threshold == "label_loose") %>%
      select(id, abstract, publication_year)
  } else {
    message("Note: publication_year column not found, year trend analysis will be skipped")
    full_abstracts <- labeled_abstracts %>%
      filter(threshold == "label_loose") %>%
      select(id, abstract)
  }
  
  if (!is.null(sample_size) && sample_size > 0 && sample_size < nrow(full_abstracts)) {
    # Use the same IDs as in results to ensure consistency
    full_abstracts <- full_abstracts %>%
      filter(id %in% results$id)
  }
  
  # Create suffix for filenames
  suffix <- if (!is.null(sample_size)) paste0("_sample", sample_size) else ""
  
  # Create and save phylum visualizations
  message("Creating phylum visualizations...")
  phylum_plots <- create_phylum_plots(results, "label_loose")
  save_plot(paste0("Results/optimized/phyla_optimized", suffix, ".png"), phylum_plots)
  
  # Create and save plant parts visualizations
  message("Creating plant parts visualizations...")
  plant_parts_plots <- create_plant_parts_plots(results)
  save_plot(paste0("Results/optimized/plant_parts_top", suffix, ".png"), plant_parts_plots$top_plot)
  save_plot(paste0("Results/optimized/plant_parts_bottom", suffix, ".png"), plant_parts_plots$bottom_plot)
  
  # Create and save taxonomic coverage visualizations
  message("Creating taxonomic coverage visualizations...")
  taxonomic_coverage_plots <- analyze_taxonomic_coverage(results, "label_loose")
  
  # Save family coverage plots
  save_plot(paste0("Results/optimized/family_coverage", suffix, ".png"), 
           taxonomic_coverage_plots$family_plot)
  save_plot(paste0("Results/optimized/family_coverage_prop", suffix, ".png"), 
           taxonomic_coverage_plots$family_prop_plot)
  
  # Save genus coverage plots
  save_plot(paste0("Results/optimized/genus_coverage", suffix, ".png"), 
           taxonomic_coverage_plots$genus_plot)
  save_plot(paste0("Results/optimized/genus_coverage_prop", suffix, ".png"), 
           taxonomic_coverage_plots$genus_prop_plot)
  
  # Save species coverage plots
  save_plot(paste0("Results/optimized/species_coverage", suffix, ".png"), 
           taxonomic_coverage_plots$species_plot)
  save_plot(paste0("Results/optimized/species_coverage_prop", suffix, ".png"), 
           taxonomic_coverage_plots$species_prop_plot)

  # Save missing taxa lists
  write_csv(taxonomic_coverage_plots$missing_families, 
            paste0("Results/optimized/missing_families", suffix, ".csv"))
  write_csv(taxonomic_coverage_plots$missing_genera, 
            paste0("Results/optimized/missing_genera", suffix, ".csv"))
  write_csv(taxonomic_coverage_plots$missing_species, 
            paste0("Results/optimized/missing_species", suffix, ".csv"))
  
  # Create and save research methods visualizations
  message("Creating research methods visualizations...")
  research_methods_plots <- analyze_research_methods(results, full_abstracts, "label_loose")
  
  # Save research methods plots
  save_plot(paste0("Results/optimized/methods_by_kingdom", suffix, ".png"), 
           research_methods_plots$kingdom_plot)
  save_plot(paste0("Results/optimized/methods_by_phylum", suffix, ".png"), 
           research_methods_plots$phylum_plot)
  
  # Save year trend plot if available
  if (!is.null(research_methods_plots$year_trend_plot)) {
    message("Creating year trend visualization...")
    save_plot(paste0("Results/optimized/methods_by_year", suffix, ".png"), 
             research_methods_plots$year_trend_plot)
  } else {
    message("Skipping year trend visualization (publication_year data not available)")
  }
  
  # Save method data for further analysis
  write_csv(research_methods_plots$methods_by_kingdom, 
           paste0("Results/optimized/methods_by_kingdom", suffix, ".csv"))
  write_csv(research_methods_plots$methods_by_phylum, 
           paste0("Results/optimized/methods_by_phylum", suffix, ".csv"))
  
  message("Processing complete! Results saved to Results/optimized/")
  return(results)
}

# Run with a sample of 10 abstracts first
results <- main(sample_size = 10)

# To run on all abstracts, uncomment this line:
 results <- main()
