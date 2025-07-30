# Taxa Detection Functions
# Optimized for large datasets (10,000+ abstracts) with proper synonym handling
# and improved detection of abbreviated genus names (e.g., "A. macrophyllum")

library(tidyverse)
library(rgbif)
library(stringr)
library(furrr)  # For parallel processing
library(future)

# Set up parallel processing with memory constraints
setup_parallel <- function(workers = NULL, memory_limit = 4000) {
  if (is.null(workers)) {
    workers <- min(4, availableCores() - 1)  # Default to 4 or less if fewer cores
  }
  
  # Set memory limit based on parameter (in MB)
  options(future.globals.maxSize = memory_limit * 1024^2)
  
  # Use multisession for Windows compatibility
  plan(multisession, workers = workers)
  
  message("Parallel processing set up with ", workers, " workers and ", memory_limit, "MB memory limit")
}

# Monitor memory usage
get_memory_usage <- function() {
  # This works on most systems but might need adjustments
  if (Sys.info()["sysname"] == "Windows") {
    mem_info <- tryCatch({
      mem <- system("wmic OS get FreePhysicalMemory,TotalVisibleMemorySize /Value", intern = TRUE)
      mem <- mem[grepl("=", mem)]
      mem <- gsub("\\s", "", mem)
      mem <- strsplit(mem, "=")
      mem <- setNames(as.numeric(sapply(mem, `[`, 2)), sapply(mem, `[`, 1))
      list(
        total = mem["TotalVisibleMemorySize"] / 1024,  # Convert to MB
        free = mem["FreePhysicalMemory"] / 1024,       # Convert to MB
        used = (mem["TotalVisibleMemorySize"] - mem["FreePhysicalMemory"]) / 1024  # Convert to MB
      )
    }, error = function(e) {
      list(total = NA, free = NA, used = NA)
    })
  } else {
    # For Linux/Mac
    mem_info <- tryCatch({
      mem <- system("free -m", intern = TRUE)
      mem <- mem[2]
      mem <- strsplit(mem, "\\s+")[[1]]
      mem <- mem[mem != ""]
      list(
        total = as.numeric(mem[2]),
        used = as.numeric(mem[3]),
        free = as.numeric(mem[4])
      )
    }, error = function(e) {
      list(total = NA, free = NA, used = NA)
    })
  }
  
  # Get R process memory usage
  r_mem <- tryCatch({
    gc_stats <- gc(full = TRUE)
    sum(gc_stats[, 2]) / 1024  # Convert to MB
  }, error = function(e) {
    NA
  })
  
  mem_info$r_used <- r_mem
  
  return(mem_info)
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
    select(synonymName, acceptedName, canonicalName_lower)
  
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
      mutate(
        resolved_name = acceptedName,
        status = "SYNONYM",
        acceptedScientificName = acceptedName
      )
    
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
    species_matches <- valid_species %>%
      filter(tolower(resolved_name) %in% all_tokens)
    
    if (nrow(species_matches) > 0) {
      # Add metadata
      species_matches <- species_matches %>%
        left_join(
          lookup_tables$accepted_species %>% 
            select(canonicalName, kingdom, phylum, family, genus),
          by = c("resolved_name" = "canonicalName")
        ) %>%
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

# Process abstracts in parallel for better performance with memory optimization
process_abstracts_parallel <- function(abstracts, lookup_tables, plant_parts_keywords, 
                                      batch_size = 100, workers = NULL, memory_limit = 4000,
                                      low_memory_mode = FALSE) {
  # Set up parallel processing with memory constraints
  setup_parallel(workers, memory_limit)
  
  # Total number of abstracts
  total_abstracts <- nrow(abstracts)
  message("Processing ", total_abstracts, " abstracts in parallel")
  
  # Process in batches for better memory management
  batches <- split(1:total_abstracts, ceiling(seq_along(1:total_abstracts) / batch_size))
  
  # In low memory mode, we'll write intermediate results to disk
  temp_files <- character(0)
  if (low_memory_mode) {
    message("Running in low memory mode - intermediate results will be saved to disk")
    temp_dir <- tempdir()
    message("Using temporary directory: ", temp_dir)
  } else {
    all_results <- list()
  }
  
  # Monitor initial memory usage
  mem_usage <- get_memory_usage()
  message("Initial memory usage: ", round(mem_usage$r_used, 1), " MB (R process)")
  
  for (i in seq_along(batches)) {
    batch_indices <- batches[[i]]
    message("Processing batch ", i, " of ", length(batches), 
            " (abstracts ", min(batch_indices), " to ", max(batch_indices), ")")
    
    # Process batch in parallel
    batch_results <- future_map(batch_indices, function(idx) {
      result <- extract_plant_info(
        text = abstracts$abstract[idx],
        abstract_id = abstracts$id[idx],
        predicted_label = abstracts$predicted_label[idx],
        lookup_tables = lookup_tables,
        plant_parts_keywords = plant_parts_keywords
      )
      
      # Force garbage collection within each worker
      gc(full = TRUE)
      
      return(result)
    }, .options = furrr_options(seed = TRUE))
    
    # Combine batch results
    batch_df <- bind_rows(batch_results)
    
    # Clear batch_results to free memory
    rm(batch_results)
    
    if (low_memory_mode) {
      # Save intermediate results to disk
      temp_file <- tempfile(pattern = "batch_", tmpdir = temp_dir, fileext = ".rds")
      saveRDS(batch_df, temp_file)
      temp_files <- c(temp_files, temp_file)
      
      # Clear batch_df to free memory
      rm(batch_df)
    } else {
      # Add to in-memory results
      all_results <- c(all_results, list(batch_df))
      
      # Clear batch_df to free memory
      rm(batch_df)
    }
    
    # Force aggressive garbage collection
    gc(full = TRUE, reset = TRUE)
    
    # Monitor memory usage
    mem_usage <- get_memory_usage()
    message("Memory usage after batch ", i, ": ", round(mem_usage$r_used, 1), " MB (R process)")
    
    # If memory usage is getting high, switch to low memory mode
    if (!low_memory_mode && mem_usage$r_used > (memory_limit * 0.8)) {
      message("Memory usage is high (", round(mem_usage$r_used, 1), " MB) - switching to low memory mode")
      
      # Save current results to disk
      for (j in seq_along(all_results)) {
        temp_file <- tempfile(pattern = "batch_", tmpdir = temp_dir, fileext = ".rds")
        saveRDS(all_results[[j]], temp_file)
        temp_files <- c(temp_files, temp_file)
      }
      
      # Clear all_results to free memory
      rm(all_results)
      low_memory_mode <- TRUE
      
      # Force garbage collection
      gc(full = TRUE, reset = TRUE)
    }
  }
  
  # Combine all results
  if (low_memory_mode) {
    message("Combining results from temporary files...")
    final_results <- bind_rows(lapply(temp_files, readRDS))
    
    # Clean up temporary files
    message("Cleaning up temporary files...")
    unlink(temp_files)
  } else {
    final_results <- bind_rows(all_results)
  }
  
  # Final garbage collection
  gc(full = TRUE)
  
  message("Processing complete! Processed ", total_abstracts, " abstracts")
  message("Final memory usage: ", round(get_memory_usage()$r_used, 1), " MB (R process)")
  
  return(final_results)
}

# Function to run the optimized taxa detection on a dataset
run_taxa_detection <- function(input_file, output_file, sample_size = NULL, 
                              batch_size = NULL, workers = NULL, 
                              memory_limit = NULL, low_memory_mode = NULL,
                              auto_optimize = TRUE) {
  # Load abstracts
  message("Loading abstracts from ", input_file)
  abstracts <- read_csv(input_file, show_col_types = FALSE)
  
  # Take a sample if requested
  if (!is.null(sample_size) && sample_size > 0 && sample_size < nrow(abstracts)) {
    message("Using a sample of ", sample_size, " abstracts")
    abstracts <- abstracts %>% 
      sample_n(sample_size) %>%
      arrange(id)
  }
  
  # Auto-optimize settings based on dataset size if requested
  if (auto_optimize) {
    message("Auto-optimizing settings for ", nrow(abstracts), " abstracts...")
    optimal_settings <- get_optimal_settings(dataset_size = nrow(abstracts))
    
    # Only use optimal settings if parameters weren't explicitly provided
    if (is.null(batch_size)) batch_size <- optimal_settings$batch_size
    if (is.null(workers)) workers <- optimal_settings$workers
    if (is.null(memory_limit)) memory_limit <- optimal_settings$memory_limit
    if (is.null(low_memory_mode)) low_memory_mode <- optimal_settings$low_memory_mode
    
    message("Using optimized settings:")
    message("  - Batch size: ", batch_size)
    message("  - Workers: ", workers)
    message("  - Memory limit: ", memory_limit, " MB")
    message("  - Low memory mode: ", low_memory_mode)
  } else {
    # Use defaults if not provided
    if (is.null(batch_size)) batch_size <- 50
    if (is.null(memory_limit)) memory_limit <- 4000
    if (is.null(low_memory_mode)) low_memory_mode <- FALSE
  }
  
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create optimized lookup tables
  message("Creating optimized lookup tables...")
  lookup_tables <- create_lookup_tables(species)
  
  # Check memory usage after loading data
  mem_usage <- get_memory_usage()
  message("Memory usage after loading data: ", round(mem_usage$r_used, 1), " MB (R process)")
  
  # If memory usage is already high, suggest low memory mode
  if (mem_usage$r_used > (memory_limit * 0.5) && !low_memory_mode) {
    message("Memory usage is already high. Consider using low_memory_mode=TRUE")
    message("Current memory usage: ", round(mem_usage$r_used, 1), " MB, limit: ", memory_limit, " MB")
  }
  
  # Define plant parts keywords
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
    "cone", "cones", "needle", "needles"
  )
  
  # Process abstracts in parallel
  message("Processing ", nrow(abstracts), " abstracts with batch size ", batch_size)
  if (low_memory_mode) {
    message("Running in low memory mode - intermediate results will be saved to disk")
  }
  start_time <- Sys.time()
  
  results <- process_abstracts_parallel(
    abstracts, 
    lookup_tables, 
    plant_parts_keywords,
    batch_size = batch_size,
    workers = workers,
    memory_limit = memory_limit,
    low_memory_mode = low_memory_mode
  )
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  message("Processed ", nrow(abstracts), " abstracts in ", round(elapsed / 60, 2), " minutes")
  message("Average time per abstract: ", round(elapsed / nrow(abstracts), 4), " seconds")
  
  # Save results
  message("Saving results to ", output_file)
  write_csv(results, output_file)
  
  # Generate summary
  summary <- results %>%
    group_by(match_type) %>%
    summarise(
      count = n(),
      unique_taxa = n_distinct(resolved_name),
      abstracts = n_distinct(id)
    )
  
  message("\n=== Results Summary ===")
  print(summary)
  
  # Check synonym handling
  synonym_count <- sum(results$status == "SYNONYM", na.rm = TRUE)
  message("\nSynonyms resolved: ", synonym_count)
  
  if (synonym_count > 0) {
    synonym_examples <- results %>%
      filter(status == "SYNONYM") %>%
      select(id, match_type, resolved_name, status) %>%
      head(5)
    
    message("Example synonyms resolved:")
    print(synonym_examples)
  }
  
  return(results)
}

# Function to benchmark different batch sizes
benchmark_batch_sizes <- function(input_file, sample_size = 100) {
  # Load abstracts
  message("Loading abstracts from ", input_file)
  abstracts <- read_csv(input_file, show_col_types = FALSE)
  
  # Take a sample for benchmarking
  if (nrow(abstracts) > sample_size) {
    message("Using a sample of ", sample_size, " abstracts for benchmarking")
    abstracts <- abstracts %>% 
      sample_n(sample_size) %>%
      arrange(id)
  }
  
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the main script first.")
  }
  species <- readRDS("species.rds")
  
  # Create optimized lookup tables
  message("Creating optimized lookup tables...")
  lookup_tables <- create_lookup_tables(species)
  
  # Define simple plant parts keywords for benchmarking
  plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
  
  # Test different batch sizes
  batch_sizes <- c(10, 25, 50, 100)
  results <- list()
  
  message("\n=== Benchmarking Batch Sizes ===")
  message("Testing with ", nrow(abstracts), " abstracts")
  
  for (batch_size in batch_sizes) {
    message("\nTesting batch size: ", batch_size)
    
    # Time the processing
    start_time <- Sys.time()
    
    # Process abstracts
    batch_results <- process_abstracts_parallel(
      abstracts, 
      lookup_tables, 
      plant_parts_keywords,
      batch_size = batch_size
    )
    
    # Record time
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    message("Processed ", nrow(abstracts), " abstracts in ", round(elapsed, 2), " seconds")
    message("Average time per abstract: ", round(elapsed / nrow(abstracts), 4), " seconds")
    
    # Store results
    results[[as.character(batch_size)]] <- list(
      batch_size = batch_size,
      elapsed_time = elapsed,
      avg_time_per_abstract = elapsed / nrow(abstracts),
      detected_taxa = nrow(batch_results),
      abstracts_with_species = length(unique(batch_results$id[batch_results$match_type == "species"]))
    )
  }
  
  # Summarize results
  summary <- bind_rows(results)
  
  message("\n=== Benchmark Summary ===")
  print(summary)
  
  # Recommend optimal batch size
  optimal_batch <- summary %>%
    arrange(avg_time_per_abstract) %>%
    slice(1)
  
  message("\nRecommended batch size for your system: ", optimal_batch$batch_size)
  message("Estimated time for 10,000 abstracts: ", 
      round(optimal_batch$avg_time_per_abstract * 10000 / 60, 1), " minutes")
  
  return(optimal_batch$batch_size)
}

# Function to optimize settings for specific hardware
get_optimal_settings <- function(dataset_size = NULL) {
  # Get system information
  cpu_cores <- availableCores()
  mem_info <- get_memory_usage()
  total_ram <- mem_info$total
  
  # Default settings
  settings <- list(
    workers = 4,
    batch_size = 50,
    memory_limit = 4000,
    low_memory_mode = FALSE
  )
  
  # Detect Intel i7-1255U or similar high-performance CPUs
  is_high_performance_cpu <- FALSE
  cpu_info <- tryCatch({
    if (Sys.info()["sysname"] == "Windows") {
      cpu <- system("wmic cpu get name", intern = TRUE)
      cpu <- cpu[grepl("i7|i9|Xeon|Ryzen", cpu)]
      if (length(cpu) > 0) {
        is_high_performance_cpu <- TRUE
        cpu
      } else {
        NULL
      }
    } else {
      cpu <- system("cat /proc/cpuinfo | grep 'model name' | head -1", intern = TRUE)
      if (grepl("i7|i9|Xeon|Ryzen", cpu)) {
        is_high_performance_cpu <- TRUE
        cpu
      } else {
        NULL
      }
    }
  }, error = function(e) {
    NULL
  })
  
  # Check if it's specifically an i7-1255U (which has efficiency cores)
  is_efficiency_core_cpu <- FALSE
  if (is_high_performance_cpu) {
    if (any(grepl("1255U", cpu_info))) {
      is_efficiency_core_cpu <- TRUE
    }
  }
  
  # Optimize for high-performance CPU
  if (is_high_performance_cpu) {
    if (is_efficiency_core_cpu) {
      # For i7-1255U with efficiency cores, use fewer workers but more efficiently
      settings$workers <- 6  # Use 6 workers (2 performance cores + 4 efficiency cores)
    } else {
      # For other high-performance CPUs
      settings$workers <- min(8, cpu_cores - 2)  # Leave 2 cores for system
    }
  } else if (cpu_cores >= 6) {
    # For decent multi-core systems
    settings$workers <- min(6, cpu_cores - 1)
  }
  
  # Optimize batch size based on dataset size and available RAM
  if (!is.null(dataset_size)) {
    if (dataset_size > 5000) {
      # For large datasets, use smaller batches for more frequent progress updates
      settings$batch_size <- 50
    } else if (dataset_size > 1000) {
      settings$batch_size <- 75
    } else {
      settings$batch_size <- 100
    }
  } else {
    # Default batch sizes based on RAM
    if (!is.na(total_ram)) {
      if (total_ram >= 14000) {
        settings$batch_size <- 75  # Reduced from 150 for better performance
      } else if (total_ram >= 8000) {
        settings$batch_size <- 50
      } else {
        settings$batch_size <- 25
      }
    }
  }
  
  # Optimize for available RAM
  if (!is.na(total_ram)) {
    if (total_ram >= 14000) {
      # High RAM (14GB+)
      settings$memory_limit <- 10000  # Reduced from 12000 to avoid swapping
    } else if (total_ram >= 8000) {
      # Medium RAM (8-14GB)
      settings$memory_limit <- total_ram - 2000
    } else {
      # Low RAM (<8GB)
      settings$memory_limit <- total_ram - 1000
      settings$low_memory_mode <- TRUE
    }
  }
  
  return(settings)
}

# Example usage
if (interactive()) {
  message("To run with a small sample (10 abstracts):")
  message('results <- run_taxa_detection("full_predictions_with_metadata.csv", "taxa_info_results_sample.csv", sample_size = 10)')
  
  message("\nTo find the optimal batch size for your system:")
  message('optimal_batch_size <- benchmark_batch_sizes("full_predictions_with_metadata.csv", sample_size = 100)')
  
  message("\nTo run with the full dataset using the optimal batch size:")
  message('optimal_settings <- get_optimal_settings()')
  message('results <- run_taxa_detection(')
  message('  input_file = "full_predictions_with_metadata.csv",')
  message('  output_file = "taxa_info_results_optimized.csv",')
  message('  batch_size = optimal_settings$batch_size,')
  message('  workers = optimal_settings$workers,')
  message('  memory_limit = optimal_settings$memory_limit')
  message(')')
  
  message("\nFor your i7-1255U with 15.7GB RAM, recommended settings are:")
  optimal_settings <- get_optimal_settings()
  message(paste0("  - Workers: ", optimal_settings$workers))
  message(paste0("  - Batch size: ", optimal_settings$batch_size))
  message(paste0("  - Memory limit: ", optimal_settings$memory_limit, " MB"))
  message(paste0("  - Low memory mode: ", optimal_settings$low_memory_mode))
}
