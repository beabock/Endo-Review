# =============================================================================
# 01b_mycorrhizal_check.R - Mycorrhizal taxa identification component
# =============================================================================
#
# Purpose: Identify papers that only mention mycorrhizal fungi using FUNGuild dataset
#
# Description: This script analyzes species detection results to determine which
# papers mention only mycorrhizal fungal taxa, enabling later filtering for
# analyses that need to include/exclude mycorrhizal-only papers.
#
# Dependencies: tidyverse (uses local funtothefun.csv dataset)
#
# Author: B. Bock
# Date: 2024-09-25
#
# Inputs/Outputs:
# - Input: results/species_detection_results.csv (from 01_extract_species.R)
# - Input: results/consolidated_dataset.csv (original dataset with all metadata)
# - Output: results/species_detection_results_mycorrhizal_enhanced.csv (with all original columns plus mycorrhizal classification)
#
# =============================================================================

library(tidyverse)

cat("=== MYCORRHIZAL TAXA IDENTIFICATION COMPONENT ===\n")
cat("Identifying papers with only mycorrhizal fungi using funtothefun dataset\n\n")

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

#' Classify fungal taxa using funtothefun dataset to determine mycorrhizal status
#'
#' Uses the local funtothefun dataset to classify fungal taxa and determine if they are mycorrhizal.
#' Handles cases where taxa might not be found in the database.
#'
#' @param taxa_names Character vector of fungal taxa names to classify
#' @param taxa_df Dataframe with taxonomic information for the taxa
#' @return Dataframe with mycorrhizal classification for each taxon
classify_fungal_taxa_mycorrhizal <- function(taxa_names, taxa_df) {

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

  # Load the funtothefun dataset
  fun_data_path <- "C:/Users/beabo/OneDrive/Documents/NAU/Sap_Sym/datasets/funtothefun.csv"

  if (!file.exists(fun_data_path)) {
    warning("funtothefun.csv not found, falling back to taxonomic classification")
    return(classify_mycorrhizal_taxonomic(unique_taxa))
  }

  fun_data <- tryCatch({
    read_csv(fun_data_path, show_col_types = FALSE)
  }, error = function(e) {
    warning("Error loading funtothefun dataset: ", e$message)
    return(NULL)
  })

  if (is.null(fun_data)) {
    return(classify_mycorrhizal_taxonomic(unique_taxa))
  }

  # Filter for guild information and create lookup table
  guild_data <- fun_data %>%
    filter(trait_name == "guild_fg") %>%
    select(species, value) %>%
    rename(guild = value) %>%
    distinct()

  # Create comprehensive lookup table with all available trait data
  # First, get all trait data for species in our dataset
  all_trait_data <- fun_data %>%
    filter(trait_name %in% c("guild_fg", "trophic_mode_fg", "growth_form_fg", "confidence_fg")) %>%
    select(species, trait_name, value) %>%
    pivot_wider(
      names_from = trait_name,
      values_from = value,
      values_fn = list  # Allow multiple values if they exist
    ) %>%
    unnest(cols = everything())  # Flatten the lists

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

#' Main function to identify mycorrhizal-only papers
#'
#' Merges species detection results with consolidated dataset to preserve all original metadata columns,
#' then adds mycorrhizal classification and funtothefun metadata.
#' Creates enhanced output with all original columns from consolidated dataset plus mycorrhizal classification and trait data.
#'
#' @param input_file Path to species detection results CSV
#' @param output_file Path to save results with mycorrhizal classification
#' @param force_rerun Logical, if TRUE forces re-processing even if output exists
#' @param verbose Logical, if TRUE prints progress messages
#' @return Dataframe with all original metadata columns plus mycorrhizal classification and funtothefun metadata
identify_mycorrhizal_papers <- function(
  input_file = "results/species_detection_results.csv",
  output_file = "results/species_detection_results_mycorrhizal_enhanced.csv",
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Check if output already exists
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing mycorrhizal classification results\n")
    return(read_csv(output_file, show_col_types = FALSE))
  }

  if (verbose) cat("🔍 Starting mycorrhizal classification analysis\n")

  # Load species detection results
  if (!file.exists(input_file)) {
    stop("❌ Species detection results not found. Please run 01_extract_species.R first.")
  }

  species_data <- read_csv(input_file, show_col_types = FALSE)

  if (verbose) cat("   Loaded", nrow(species_data), "species detection records\n")

  # Load consolidated dataset to preserve all original columns
  consolidated_file <- "results/consolidated_dataset.csv"
  if (!file.exists(consolidated_file)) {
    warning("⚠️  Consolidated dataset not found. Proceeding without original metadata columns.")
  } else {
    if (verbose) cat("   Loading consolidated dataset to preserve original columns...\n")

    consolidated_data <- read_csv(consolidated_file, show_col_types = FALSE)

    if (verbose) cat("   Loaded consolidated dataset with", nrow(consolidated_data), "records and", ncol(consolidated_data), "columns\n")

    # Merge consolidated data with species data using id column
    # Use left_join to preserve all species detection records
    merged_data <- species_data %>%
      left_join(consolidated_data, by = "id")

    if (verbose) cat("   Merged data now has", ncol(merged_data), "columns (added", ncol(merged_data) - ncol(species_data), "original columns)\n")

    # Update species_data to the merged version
    species_data <- merged_data
  }

  # Get unique fungal taxa for classification
  fungal_taxa <- species_data %>%
    filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
    distinct(resolved_name, kingdom, phylum, family, genus)

  if (verbose) cat("   Found", nrow(fungal_taxa), "unique fungal taxa to classify\n")

  # Classify fungal taxa using funtothefun dataset
  if (verbose) cat("   Classifying taxa using funtothefun dataset...\n")

  mycorrhizal_classifications <- classify_fungal_taxa_mycorrhizal(
    fungal_taxa$resolved_name,
    fungal_taxa
  )

  if (verbose) cat("   Classification complete for", nrow(mycorrhizal_classifications), "taxa\n")

  # Create enhanced results with all original columns plus mycorrhizal info
  if (verbose) cat("   Enhancing results with funtothefun metadata...\n")

  # Create lookup table for mycorrhizal classifications
  mycorrhizal_lookup <- mycorrhizal_classifications %>%
    select(resolved_name, is_mycorrhizal, funguild_guild, confidence_ranking,
           trophic_mode, growth_form, trait_confidence)

  # Preserve all original columns before joining
  original_columns <- names(species_data)

  # Merge mycorrhizal classifications back to original data
  enhanced_results <- species_data %>%
    left_join(mycorrhizal_lookup, by = "resolved_name") %>%
    # Keep all original columns, add new ones
    mutate(
      is_mycorrhizal_only = case_when(
        kingdom != "Fungi" ~ FALSE,  # Non-fungal taxa are not mycorrhizal-only
        is.na(is_mycorrhizal) ~ FALSE,  # Unknown taxa are not mycorrhizal-only
        TRUE ~ is_mycorrhizal  # Use classification for fungal taxa
      )
    ) %>%
    # Ensure all expected columns exist (fill NAs for non-fungal taxa)
    mutate(
      is_mycorrhizal = if_else(kingdom != "Fungi", FALSE, is_mycorrhizal),
      funguild_guild = if_else(kingdom != "Fungi", "Non-fungal", funguild_guild),
      confidence_ranking = if_else(kingdom != "Fungi", 1.0, confidence_ranking),
      trophic_mode = if_else(kingdom != "Fungi", NA_character_, trophic_mode),
      growth_form = if_else(kingdom != "Fungi", NA_character_, growth_form),
      trait_confidence = if_else(kingdom != "Fungi", NA_character_, trait_confidence)
    ) %>%
    # Reorder columns to put original columns first, then new ones at the end
    select(
      all_of(original_columns),  # Preserve all original columns in original order
      # New mycorrhizal and trait columns
      is_mycorrhizal, funguild_guild, confidence_ranking, trophic_mode,
      growth_form, trait_confidence, is_mycorrhizal_only
    )

  # Add abstract-level mycorrhizal status to enhanced results
  if (verbose) cat("   Determining abstract-level mycorrhizal status...\n")

  # Get unique abstract IDs
  abstract_ids <- unique(species_data$id)

  # Create lookup table for abstract-level classifications
  abstract_mycorrhizal_status <- map_dfr(abstract_ids, function(abstract_id) {
    abstract_data <- species_data %>% filter(id == abstract_id)

    is_mycorrhizal_only <- determine_abstract_mycorrhizal_status(
      abstract_data,
      mycorrhizal_classifications
    )

    return(tibble(id = abstract_id, abstract_mycorrhizal_only = is_mycorrhizal_only))
  })

  # Merge abstract-level status into enhanced results
  final_results <- enhanced_results %>%
    left_join(abstract_mycorrhizal_status, by = "id") %>%
    mutate(is_mycorrhizal_only = if_else(is.na(abstract_mycorrhizal_only), FALSE, abstract_mycorrhizal_only)) %>%
    select(-abstract_mycorrhizal_only)  # Remove temporary column

  # Summary statistics
  total_abstracts <- length(unique(final_results$id))
  mycorrhizal_only_count <- sum(final_results$is_mycorrhizal_only, na.rm = TRUE)
  mycorrhizal_only_pct <- round(100 * mycorrhizal_only_count / total_abstracts, 1)

  if (verbose) {
    cat("🎉 Mycorrhizal classification completed!\n")
    cat("📈 Results:\n")
    cat("   - Total abstracts processed:", total_abstracts, "\n")
    cat("   - Mycorrhizal-only papers:", mycorrhizal_only_count,
        "(", mycorrhizal_only_pct, "%)\n")
    cat("💾 Results saved to:", output_file, "\n")
  }

  # Save results
  tryCatch({
    write_csv(final_results, output_file)
  }, error = function(e) {
    warning("Failed to save results to ", output_file, ": ", e$message)
  })

  return(final_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "01b_mycorrhizal_check.R")) {

  # Check if funtothefun dataset exists
  fun_data_path <- "C:/Users/beabo/OneDrive/Documents/NAU/Sap_Sym/datasets/funtothefun.csv"
  if (!file.exists(fun_data_path)) {
    cat("⚠️  funtothefun.csv dataset not found at expected location.\n")
    cat("   Expected path: ", fun_data_path, "\n")
    cat("   Please ensure the dataset is available for mycorrhizal classification.\n")
    stop("❌ funtothefun.csv dataset not found")
  }

  # Run the analysis
  results <- identify_mycorrhizal_papers()

  cat("\n✅ Mycorrhizal checking component completed!\n")
}