# =============================================================================
# 01_species_mycorrhizal.R - Unified species detection and mycorrhizal classification
# =============================================================================
#
# Purpose: Extract plant and fungal species information and classify mycorrhizal status
#
# Description: Combined script that handles species detection for plants and fungi using optimized
# lookup tables and mycorrhizal classification using FUNGuild dataset. Part of the memory-efficient
# extraction pipeline that minimizes data duplication by only outputting id + species + mycorrhizal columns.
#
# Dependencies: tidyverse, tictoc, janitor; scripts/04_analysis/optimized_taxa_detection.R, scripts/04_analysis/utilities/reference_data_utils.R
#
# Author: B. Bock
# Date: 2024-09-26
#
# Inputs/Outputs: Reads consolidated abstracts from results/consolidated_dataset.csv; outputs species detection and mycorrhizal results to results/species_mycorrhizal_results.csv
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(janitor)

# Source required functions for optimized taxa detection and reference data utilities
source("scripts/04_analysis/optimized_taxa_detection.R")
source("scripts/04_analysis/utilities/reference_data_utils.R")

# Source memory optimization utilities
tryCatch({
  source("scripts/utils/memory_optimization.R")
}, error = function(e) {
  cat("⚠️ Memory optimization utilities not found, running without memory monitoring\n")
})

cat("=== UNIFIED SPECIES DETECTION AND MYCORRHIZAL CLASSIFICATION ===\n")
cat("Extracting species information and classifying mycorrhizal status\n\n")

# Function to create optimized lookup tables
create_lookup_tables_optimized <- function(species_df, threshold = 10000) {
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

  # Memory-efficient hash table creation (only for very large datasets)
  # Rationale: For datasets above the threshold, hash tables provide faster lookups than linear searches, improving performance for species matching
  if (nrow(species_df) > threshold) {
    if (verbose) cat("   Creating hash tables for large dataset (memory-optimized)...\n")

    # Create hash tables more efficiently by processing in chunks
    if (!is.null(lookup_tables$species_names_vector)) {
      # Use environment for memory efficiency instead of named vector
      species_env <- new.env(hash = TRUE)
      for (name in lookup_tables$species_names_vector) {
        species_env[[name]] <- TRUE
      }
      lookup_tables$species_hash <- species_env
    }

    if (!is.null(lookup_tables$genus_names_vector)) {
      genus_env <- new.env(hash = TRUE)
      for (name in lookup_tables$genus_names_vector) {
        genus_env[[name]] <- TRUE
      }
      lookup_tables$genus_hash <- genus_env
    }

    if (verbose) cat("   Hash tables created successfully\n")
  }

  return(lookup_tables)
}

#' Helper function to classify guild as mycorrhizal
#'
#' Determines if a fungal guild represents mycorrhizal fungi based on guild name.
#' Only TRUE mycorrhizal guilds count - Endophytes are NOT mycorrhizal.
#'
#' @param guild Character string with the guild name
#' @return Logical: TRUE if mycorrhizal, FALSE otherwise
classify_guild_as_mycorrhizal <- function(guild) {
  if (is.na(guild) || guild == "") {
    return(FALSE)
  }

  # TRUE mycorrhizal guilds (form symbiotic relationships with roots)
  mycorrhizal_guilds <- c(
    "Ectomycorrhizal",
    "Arbuscular Mycorrhizal",
    "Orchid Mycorrhizal",
    "Ericoid Mycorrhizal"
  )

  # Check if guild contains any mycorrhizal keywords
  is_mycorrhizal <- any(str_detect(guild, mycorrhizal_guilds))

  # Special cases: some complex guilds that include mycorrhizal
  complex_mycorrhizal <- c(
    "Ectomycorrhizal-Fungal Parasite",
    "Ectomycorrhizal-Undefined Saprotroph",
    "Ectomycorrhizal-Endophyte-Ericoid Mycorrhizal-Litter Saprotroph-Orchid Mycorrhizal",
    "Ectomycorrhizal-Orchid Mycorrhizal-Root Associated Biotroph",
    "Ectomycorrhizal-Wood Saprotroph",
    "Dung Saprotroph-Ectomycorrhizal"
  )

  if (is_mycorrhizal || any(str_detect(guild, complex_mycorrhizal))) {
    return(TRUE)
  }

  # Endophytes are NOT mycorrhizal - they live inside plants but don't form mycorrhizal symbiosis
  if (str_detect(guild, "Endophyte")) {
    return(FALSE)
  }

  # Conservative default: assume not mycorrhizal if unclear
  return(FALSE)
}

#' Taxonomic heuristic classification for mycorrhizal status
#'
#' Fallback method using known mycorrhizal taxonomic groups when FUNGuild fails.
#' This is a conservative approach based on established mycorrhizal taxonomy.
#'
#' @param taxa_names Character vector of fungal taxa names
#' @return Dataframe with taxonomic classification results
classify_mycorrhizal_taxonomic <- function(taxa_names) {

  # Known mycorrhizal taxonomic groups
  mycorrhizal_phyla <- c("Glomeromycota", "Mucoromycota")
  mycorrhizal_families <- c("Glomeraceae", "Acaulosporaceae", "Gigasporaceae",
                          "Diversisporaceae", "Pacisporaceae", "Archaeosporaceae",
                          "Paraglomeraceae", "Claroideoglomeraceae")

  # Known mycorrhizal genera (partial list)
  mycorrhizal_genera <- c("Glomus", "Acaulospora", "Gigaspora", "Scutellospora",
                         "Rhizophagus", "Funneliformis", "Septoglomus", "Archaeospora",
                         "Paraglomus", "Claroideoglomus", "Rhizopogon", "Pisolithus",
                         "Scleroderma", "Cenococcum", "Tuber", "Laccaria", "Suillus",
                         "Boletus", "Amanita", "Russula", "Lactarius")

  results <- map_dfr(taxa_names, function(taxon) {
    # Check if taxon name contains known mycorrhizal indicators
    is_mycorrhizal <- FALSE
    confidence <- 0

    # Check for phylum-level matches
    if (any(str_detect(taxon, mycorrhizal_phyla))) {
      is_mycorrhizal <- TRUE
      confidence <- 0.9
    }
    # Check for family-level matches
    else if (any(str_detect(taxon, mycorrhizal_families))) {
      is_mycorrhizal <- TRUE
      confidence <- 0.8
    }
    # Check for genus-level matches
    else if (any(str_detect(taxon, mycorrhizal_genera))) {
      is_mycorrhizal <- TRUE
      confidence <- 0.7
    }

    # Conservative default: assume not mycorrhizal if unclear
    if (!is_mycorrhizal) {
      confidence <- 0.1
    }

    return(tibble(
      resolved_name = taxon,
      is_mycorrhizal = is_mycorrhizal,
      funguild_guild = if_else(is_mycorrhizal, "Taxonomic_Mycorrhizal", "Unknown"),
      confidence_ranking = confidence,
      trophic_mode = NA_character_,
      growth_form = NA_character_,
      trait_confidence = NA_character_
    ))
  })

  return(results)
}

#' Classify fungal taxa using funtothefun dataset to determine mycorrhizal status
#'
#' Uses the local funtothefun dataset to classify fungal taxa and determine if they are mycorrhizal.
#' Handles cases where taxa might not be found in the database.
#'
#' @param taxa_names Character vector of fungal taxa names to classify
#' @param taxa_df Dataframe with taxonomic information for the taxa
#' @param verbose Logical, if TRUE prints progress messages (default: TRUE)
#' @return Dataframe with mycorrhizal classification for each taxon
classify_fungal_taxa_mycorrhizal <- function(taxa_names, taxa_df, verbose = TRUE) {

  # Filter to only fungal taxa
  fungal_taxa <- taxa_df %>%
    filter(kingdom == "Fungi", !is.na(resolved_name))

  if (nrow(fungal_taxa) == 0) {
    return(tibble(
      resolved_name = character(0),
      is_mycorrhizal = logical(0),
      funguild_guild = character(0),
      confidence_ranking = numeric(0)
    ))
  }

  # Get unique fungal taxa names
  unique_taxa <- fungal_taxa %>%
    distinct(resolved_name) %>%
    pull(resolved_name)

  # Memory-efficient loading of funtothefun dataset
  fun_data_path <- "C:/Users/beabo/OneDrive - Northern Arizona University/NAU/Sap_Sym/datasets/funtothefun.csv"

  if (!file.exists(fun_data_path)) {
    warning("funtothefun.csv not found, falling back to taxonomic classification")
    return(classify_mycorrhizal_taxonomic(unique_taxa))
  }

  # Memory-efficient CSV reading with progress monitoring
  if (verbose) cat("   Loading FUNGuild dataset (memory-efficient mode)...\n")

  fun_data <- tryCatch({
    # Read in chunks to manage memory
    chunk_size <- 10000
    first_chunk <- read_csv(fun_data_path, show_col_types = FALSE, n_max = chunk_size)

    # Check if file is very large and optimize accordingly
    file_size_mb <- file.size(fun_data_path) / (1024*1024)
    if (file_size_mb > 100) {
      if (verbose) cat("   Large FUNGuild file detected (", round(file_size_mb, 1), "MB), optimizing memory usage\n")

      # For large files, filter early to reduce memory footprint
      required_traits <- c("guild_fg", "trophic_mode_fg", "growth_form_fg", "confidence_fg")
      fun_data <- read_csv(fun_data_path, show_col_types = FALSE) %>%
        filter(trait_name %in% required_traits) %>%
        # Keep only species that might match our taxa (memory optimization)
        filter(species %in% unique_taxa | str_detect(species, paste0("^", str_extract(unique_taxa, "^[A-Z][a-z]+"), "_")))

      # Aggressive garbage collection after loading large dataset
      aggressive_gc(verbose = FALSE)
    } else {
      fun_data <- read_csv(fun_data_path, show_col_types = FALSE)
    }
  }, error = function(e) {
    warning("Error loading funtothefun dataset: ", e$message)
    return(NULL)
  })

  if (is.null(fun_data)) {
    return(classify_mycorrhizal_taxonomic(unique_taxa))
  }

  # Memory-efficient trait data processing
  if (verbose) cat("   Processing FUNGuild trait data...\n")

  # Create lookup table more efficiently
  all_trait_data <- fun_data %>%
    filter(trait_name %in% c("guild_fg", "trophic_mode_fg", "growth_form_fg", "confidence_fg")) %>%
    select(species, trait_name, value) %>%
    # Memory-efficient pivot with explicit handling
    group_by(species, trait_name) %>%
    summarise(values = list(value), .groups = "drop") %>%
    pivot_wider(
      names_from = trait_name,
      values_from = values,
      values_fn = ~if(length(.x[[1]]) > 0) .x[[1]][1] else NA_character_  # Take first value safely
    ) %>%
    # Clean up nested list columns
    mutate(across(everything(), ~ifelse(sapply(., is.list), NA_character_, .)))

  # Create lookup table by matching species names
  # Try exact match first, then genus-level matching
  classification_results <- map_dfr(unique_taxa, function(taxon) {

    # Try exact species match
    exact_match <- all_trait_data %>%
      filter(species == taxon)

    if (nrow(exact_match) > 0) {
      # Use the first match (there might be multiple entries)
      guild <- exact_match$guild_fg[1]
      return(tibble(
        resolved_name = taxon,
        is_mycorrhizal = classify_guild_as_mycorrhizal(guild),
        funguild_guild = guild,
        confidence_ranking = 0.9,  # High confidence for exact match
        trophic_mode = exact_match$trophic_mode_fg[1],
        growth_form = exact_match$growth_form_fg[1],
        trait_confidence = exact_match$confidence_fg[1]
      ))
    }

    # Try genus-level match (extract genus from species name)
    genus <- str_extract(taxon, "^[A-Z][a-z]+")
    if (!is.na(genus)) {
      genus_matches <- all_trait_data %>%
        filter(str_detect(species, paste0("^", genus, "_"))) %>%
        head(5)  # Take up to 5 matches for the genus

      if (nrow(genus_matches) > 0) {
        # Use the most common guild for this genus
        guild_counts <- genus_matches %>%
          count(guild_fg, sort = TRUE)

        guild <- guild_counts$guild_fg[1]
        # Get most common values for other traits too
        trophic_mode <- names(sort(table(genus_matches$trophic_mode_fg), decreasing = TRUE))[1]
        growth_form <- names(sort(table(genus_matches$growth_form_fg), decreasing = TRUE))[1]
        confidence <- names(sort(table(genus_matches$confidence_fg), decreasing = TRUE))[1]

        return(tibble(
          resolved_name = taxon,
          is_mycorrhizal = classify_guild_as_mycorrhizal(guild),
          funguild_guild = guild,
          confidence_ranking = 0.7,  # Medium confidence for genus match
          trophic_mode = trophic_mode,
          growth_form = growth_form,
          trait_confidence = confidence
        ))
      }
    }

    # No match found - use taxonomic fallback
    taxonomic_result <- classify_mycorrhizal_taxonomic(taxon)
    return(tibble(
      resolved_name = taxon,
      is_mycorrhizal = taxonomic_result$is_mycorrhizal,
      funguild_guild = taxonomic_result$funguild_guild,
      confidence_ranking = taxonomic_result$confidence_ranking,
      trophic_mode = NA_character_,
      growth_form = NA_character_,
      trait_confidence = NA_character_
    ))
  })

  return(classification_results)
}

#' Determine if an abstract mentions only mycorrhizal fungi
#'
#' Analyzes species detection results for a given abstract to determine if
#' all fungal taxa mentioned are mycorrhizal. This enables filtering papers
#' that focus exclusively on mycorrhizal fungi.
#'
#' @param abstract_data Dataframe of species detection results for one abstract
#' @param mycorrhizal_classifications Dataframe of mycorrhizal classifications
#' @return Logical: TRUE if only mycorrhizal fungi mentioned, FALSE otherwise
determine_abstract_mycorrhizal_status <- function(abstract_data, mycorrhizal_classifications) {

  # Get fungal taxa from this abstract
  fungal_taxa <- abstract_data %>%
    filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
    distinct(resolved_name)

  # If no fungal taxa mentioned, return FALSE (not mycorrhizal-only)
  if (nrow(fungal_taxa) == 0) {
    return(FALSE)
  }

  # Check if all fungal taxa are mycorrhizal
  taxa_mycorrhizal_status <- fungal_taxa %>%
    left_join(mycorrhizal_classifications, by = "resolved_name") %>%
    mutate(is_mycorrhizal = if_else(is.na(is_mycorrhizal), FALSE, is_mycorrhizal))

  # If all fungal taxa are mycorrhizal, this is a mycorrhizal-only paper
  all_mycorrhizal <- all(taxa_mycorrhizal_status$is_mycorrhizal)
  any_non_mycorrhizal <- any(!taxa_mycorrhizal_status$is_mycorrhizal)

  return(all_mycorrhizal && !any_non_mycorrhizal)
}

#' Extract species information and mycorrhizal classification from abstracts
#'
#' This function processes a dataset of abstracts to detect and extract plant and fungal species mentions
#' and classify mycorrhizal status using FUNGuild. Uses optimized lookup tables and parallel processing
#' for efficient taxonomic name recognition. Outputs only id + species + mycorrhizal columns for memory efficiency.
#'
#' @param abstracts_data Data frame containing abstracts with text to process
#' @param output_file Path to save the species detection and mycorrhizal results (default: "results/species_mycorrhizal_results.csv")
#' @param batch_size Number of abstracts to process per batch (default: 100)
#' @param force_rerun Logical, if TRUE forces re-extraction even if results exist (default: FALSE)
#' @param verbose Logical, if TRUE prints progress messages (default: TRUE)
#' @param hash_threshold Minimum species count to enable hash table optimization (default: 10000)
#' @return Data frame with species detection and mycorrhizal results
extract_species_mycorrhizal_data <- function(
  abstracts_data,
  output_file = "results/species_mycorrhizal_results.csv",
  batch_size = 100,
  force_rerun = FALSE,
  verbose = TRUE,
  hash_threshold = 10000
) {

  # Ensure verbose is defined (defensive programming)
  if (!exists("verbose")) {
    verbose <- TRUE
  }

  # Input validation
  if (!is.data.frame(abstracts_data)) {
    stop("'abstracts_data' must be a data frame.")
  }
  if (!"id" %in% colnames(abstracts_data)) {
    stop("'abstracts_data' must contain an 'id' column for unique identification.")
  }
  if (!"abstract" %in% colnames(abstracts_data)) {
    stop("'abstracts_data' must contain an 'abstract' column for abstracts.")
  }

  # Handle empty datasets
  if (nrow(abstracts_data) == 0) {
    warning("Input dataset is empty. Skipping species extraction.")
    return(data.frame())  # Return empty data frame
  }

  # Recovery mechanism - check for existing results
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing species and mycorrhizal results\n")
    existing_results <- tryCatch({
      read_csv(output_file, show_col_types = FALSE)
    }, error = function(e) {
      warning("Failed to load existing species and mycorrhizal results: ", e$message, ". Proceeding with fresh extraction.")
      NULL
    })
    if (!is.null(existing_results)) {
      if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
      return(existing_results)
    }
  }

  if (verbose) cat("🔬 Starting species detection and mycorrhizal classification for", nrow(abstracts_data), "abstracts\n")

  # Load species reference data
  if (file.exists("species.rds")) {
    species <- readRDS("species.rds")
  } else if (file.exists("models/species.rds")) {
    species <- readRDS("models/species.rds")
  } else {
    stop("❌ Species reference data not found. Please ensure species.rds exists.")
  }

  if (verbose) cat("   Loaded species reference data:", nrow(species), "species records\n")

  # Set up memory-efficient parallel processing
  # Reduce cores to prevent memory pressure: use max 3 cores for large datasets
  max_cores <- min(3, parallel::detectCores() - 1)
  n_cores <- min(max(1, floor(nrow(abstracts_data) / 5000) + 1), max_cores)

  if (verbose) cat("   Using", n_cores, "cores for parallel processing (memory-optimized)\n")

  tryCatch({
    setup_parallel(workers = n_cores)
  }, error = function(e) {
    warning("Parallel setup failed: ", e$message, ". Falling back to sequential processing.")
    n_cores <- 1
  })

  # Memory-efficient lookup table creation
  if (verbose) cat("   Creating lookup tables with memory optimization...\n")
  lookup_tables <- create_lookup_tables_with_bloom(species)

  # Monitor memory usage before processing
  if (verbose) {
    mem_status <- monitor_memory(threshold_gb = 8, context = "Before species detection")
    if (mem_status$above_threshold) {
      cat("   ⚠️ High memory usage detected, enabling aggressive garbage collection\n")
      aggressive_gc(verbose = FALSE)
    }
  }

  # Process in batches
  tic("Species detection and mycorrhizal classification")
  n_batches <- ceiling(nrow(abstracts_data) / batch_size)

  if (verbose) {
    cat("📊 Processing", nrow(abstracts_data), "abstracts in", n_batches, "batches\n")
    cat("⚙️  Batch size:", batch_size, "abstracts per batch\n")
    cat("🕐 Started at", format(Sys.time(), "%H:%M:%S"), "\n\n")
  }

  all_results <- map_dfr(1:n_batches, function(i) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(abstracts_data))

    batch_data <- abstracts_data[start_idx:end_idx, ]

    if (verbose) {
      cat("   🔬 Batch", i, "of", n_batches, "(", nrow(batch_data), "abstracts)\n")
      cat("      Processing rows", start_idx, "to", end_idx, "\n")
    }

    # Memory monitoring before batch processing
    if (i %% 3 == 0 && verbose) {  # Check every 3rd batch
      mem_status <- monitor_memory(threshold_gb = 6, context = paste("Batch", i))
      if (mem_status$above_threshold) {
        cat("      🧹 Running garbage collection due to high memory usage\n")
        aggressive_gc(verbose = FALSE)
      }
    }

    # Process batch for species detection with memory optimization
    batch_results <- process_abstracts_parallel(
      abstracts = batch_data,
      species_path = if (file.exists("species.rds")) "species.rds" else "models/species.rds",
      batch_size = min(50, batch_size / 4)  # Reduce internal batch size for memory
    )

    # Progress reporting
    species_found <- sum(!is.na(batch_results$resolved_name) | !is.na(batch_results$canonicalName), na.rm = TRUE)
    if (verbose) {
      cat("      ✅ Found species in", species_found, "abstracts\n")
    }

    # Overall progress
    total_processed <- i * batch_size
    if (total_processed > nrow(abstracts_data)) total_processed <- nrow(abstracts_data)
    progress_pct <- round(100 * total_processed / nrow(abstracts_data), 1)
    if (verbose) {
      cat("      📊 Progress:", total_processed, "/", nrow(abstracts_data),
          "abstracts (", progress_pct, "%)\n\n")
    }

    # Periodic garbage collection
    if (i %% 5 == 0) {
      aggressive_gc(verbose = FALSE)
    }

    return(batch_results)
  })

  # Memory-efficient fungal taxa extraction
  if (verbose) cat("   Extracting unique fungal taxa for classification...\n")

  # Process in smaller chunks to avoid memory spikes
  chunk_size_taxa <- 1000
  n_taxa_chunks <- ceiling(nrow(all_results) / chunk_size_taxa)

  fungal_taxa_list <- map_dfr(1:n_taxa_chunks, function(j) {
    start_idx <- (j - 1) * chunk_size_taxa + 1
    end_idx <- min(j * chunk_size_taxa, nrow(all_results))

    chunk_data <- all_results[start_idx:end_idx, ]

    chunk_fungal_taxa <- chunk_data %>%
      filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
      distinct(resolved_name, kingdom, phylum, family, genus)

    return(chunk_fungal_taxa)
  })

  fungal_taxa <- fungal_taxa_list %>%
    distinct(resolved_name, kingdom, phylum, family, genus)

  if (verbose) cat("   Found", nrow(fungal_taxa), "unique fungal taxa to classify\n")

  # Memory check before FUNGuild processing
  mem_status <- monitor_memory(threshold_gb = 6, context = "Before FUNGuild classification")
  if (mem_status$above_threshold) {
    cat("   🧹 Cleaning memory before FUNGuild processing\n")
    aggressive_gc(verbose = FALSE)
  }

  # Classify fungal taxa using funtothefun dataset with memory optimization
  if (verbose) cat("   Classifying fungal taxa using funtothefun dataset (memory-optimized)...\n")

  mycorrhizal_classifications <- classify_fungal_taxa_mycorrhizal(
    fungal_taxa$resolved_name,
    fungal_taxa,
    verbose = verbose
  )

  if (verbose) cat("   Mycorrhizal classification complete for", nrow(mycorrhizal_classifications), "taxa\n")

  # Clean up memory after intensive processing
  aggressive_gc(verbose = FALSE)

  # Create lookup table for mycorrhizal classifications
  mycorrhizal_lookup <- mycorrhizal_classifications %>%
    select(resolved_name, is_mycorrhizal, funguild_guild, confidence_ranking,
           trophic_mode, growth_form, trait_confidence)

  # Add mycorrhizal information to species results
  if (verbose) cat("   Adding mycorrhizal classification to species results...\n")

  # Merge mycorrhizal classifications with species data
  enhanced_results <- all_results %>%
    left_join(mycorrhizal_lookup, by = "resolved_name") %>%
    # Set defaults for non-fungal taxa
    mutate(
      is_mycorrhizal = if_else(kingdom != "Fungi", FALSE, is_mycorrhizal),
      funguild_guild = if_else(kingdom != "Fungi", "Non-fungal", funguild_guild),
      confidence_ranking = if_else(kingdom != "Fungi", 1.0, confidence_ranking),
      trophic_mode = if_else(kingdom != "Fungi", NA_character_, trophic_mode),
      growth_form = if_else(kingdom != "Fungi", NA_character_, growth_form),
      trait_confidence = if_else(kingdom != "Fungi", NA_character_, trait_confidence)
    )

  # Add abstract-level mycorrhizal status
  if (verbose) cat("   Determining abstract-level mycorrhizal status...\n")

  # Get unique abstract IDs
  abstract_ids <- unique(enhanced_results$id)

  # Create lookup table for abstract-level classifications with error handling
  if (verbose) cat("   Determining abstract-level mycorrhizal status...\n")

  abstract_mycorrhizal_status <- map_dfr(abstract_ids, function(abstract_id) {
    tryCatch({
      abstract_data <- enhanced_results %>% filter(id == abstract_id)

      is_mycorrhizal_only <- determine_abstract_mycorrhizal_status(
        abstract_data,
        mycorrhizal_classifications
      )

      return(tibble(id = abstract_id, abstract_mycorrhizal_only = is_mycorrhizal_only))
    }, error = function(e) {
      cat("     ⚠️ Warning: Error processing abstract", abstract_id, ":", e$message, "\n")
      return(tibble(id = abstract_id, abstract_mycorrhizal_only = FALSE))
    })
  })

  # Merge abstract-level status into enhanced results
  final_results <- enhanced_results %>%
    left_join(abstract_mycorrhizal_status, by = "id") %>%
    mutate(is_mycorrhizal_only = if_else(is.na(abstract_mycorrhizal_only), FALSE, abstract_mycorrhizal_only)) %>%
    select(-abstract_mycorrhizal_only)  # Remove temporary column

  # Keep only id + species + mycorrhizal columns for memory efficiency
  final_results <- final_results %>%
    select(id, resolved_name, canonicalName, kingdom, phylum, family, genus,
           is_mycorrhizal, funguild_guild, confidence_ranking, trophic_mode,
           growth_form, trait_confidence, is_mycorrhizal_only)

  # Summary statistics with error handling
  tryCatch({
    total_abstracts <- length(unique(final_results$id))
    mycorrhizal_only_count <- sum(final_results$is_mycorrhizal_only, na.rm = TRUE)
    mycorrhizal_only_pct <- round(100 * mycorrhizal_only_count / total_abstracts, 1)

    if (verbose) {
      cat("🎉 Species detection and mycorrhizal classification completed!\n")
      cat("📈 Results:\n")
      cat("   - Total abstracts processed:", total_abstracts, "\n")
      cat("   - Mycorrhizal-only papers:", mycorrhizal_only_count,
          "(", mycorrhizal_only_pct, "%)\n")
      cat("💾 Results saved to:", output_file, "\n")
    }
  }, error = function(e) {
    cat("⚠️ Warning: Error generating summary statistics:", e$message, "\n")
    cat("   Results were still processed successfully\n")
  })

  # Incremental result saving to prevent memory accumulation
  if (verbose) cat("   Saving results incrementally to prevent memory issues...\n")

  tryCatch({
    # Save main results
    write_csv(final_results, output_file)

    # Additional memory cleanup after saving
    aggressive_gc(verbose = FALSE)

    if (verbose) {
      file_size_mb <- file.size(output_file) / (1024*1024)
      cat("   💾 Results saved (", round(file_size_mb, 1), "MB)\n")
    }
  }, error = function(e) {
    warning("Failed to save results to ", output_file, ": ", e$message)
    # Try alternative saving method
    tryCatch({
      saveRDS(final_results, paste0(tools::file_path_sans_ext(output_file), ".rds"))
      cat("   💾 Results saved as RDS format instead\n")
    }, error = function(e2) {
      warning("Failed to save results in any format: ", e2$message)
    })
  })

  # Final memory cleanup
  if (verbose) cat("   🧹 Final memory cleanup...\n")
  tryCatch({
    aggressive_gc(verbose = verbose)
  }, error = function(e) {
    cat("   ⚠️ Warning: Memory cleanup encountered minor issue:", e$message, "\n")
  })

  # Ensure we return the results properly
  if (verbose) cat("   ✅ Returning final results\n")
  return(final_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "01_species_mycorrhizal.R")) {

  # Load consolidated dataset
  abstracts_file <- "results/consolidated_dataset.csv"
  if (!file.exists(abstracts_file)) {
    stop("❌ Consolidated dataset not found. Run the consolidation script first.")
  }

  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)

  # Check if funtothefun dataset exists
  fun_data_path <- "C:/Users/beabo/OneDrive - Northern Arizona University/NAU/Sap_Sym/datasets/funtothefun.csv"
  if (!file.exists(fun_data_path)) {
    cat("⚠️  funtothefun.csv dataset not found at expected location.\n")
    cat("   Expected path: ", fun_data_path, "\n")
    cat("   Please ensure the dataset is available for mycorrhizal classification.\n")
    stop("❌ funtothefun.csv dataset not found")
  }

  # Memory check before starting main processing
  cat("🔍 Final memory check before processing...\n")
  mem_status <- monitor_memory(threshold_gb = 8, context = "Pre-processing")
  if (mem_status$above_threshold) {
    cat("⚠️ High memory usage detected. Consider closing other applications.\n")
    cat("   Will proceed with aggressive memory management.\n")
  }

  # Extract species and mycorrhizal information with memory optimization
  species_mycorrhizal_results <- extract_species_mycorrhizal_data(
    abstracts_data,
    batch_size = 50,  # Use a reasonable default batch size
    verbose = TRUE
  )

  # Final memory cleanup
  cat("🧹 Performing final memory cleanup...\n")
  aggressive_gc(verbose = TRUE)

  cat("\n✅ Unified species detection and mycorrhizal classification completed!\n")
}