# =============================================================================
# 01b_mycorrhizal_check.R - Mycorrhizal taxa identification component
# =============================================================================
#
# Purpose: Identify papers that only mention mycorrhizal fungi using FUNGuild
#
# Description: This script analyzes species detection results to determine which
# papers mention only mycorrhizal fungal taxa, enabling later filtering for
# analyses that need to include/exclude mycorrhizal-only papers.
#
# Dependencies: tidyverse, funguildr (FUNGuild package)
#
# Author: B. Bock
# Date: 2024-09-25
#
# Inputs/Outputs:
# - Input: results/species_detection_results.csv (from 01_extract_species.R)
# - Output: results/species_detection_results_mycorrhizal.csv (with is_mycorrhizal_only column)
#
# =============================================================================

library(tidyverse)
library(funguildr)

cat("=== MYCORRHIZAL TAXA IDENTIFICATION COMPONENT ===\n")
cat("Identifying papers with only mycorrhizal fungi using FUNGuild\n\n")

#' Classify fungal taxa using FUNGuild to determine mycorrhizal status
#'
#' Uses the FUNGuild database to classify fungal taxa and determine if they are mycorrhizal.
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

  # Use FUNGuild to classify taxa
  # Note: This may take some time for large datasets
  funguild_results <- tryCatch({
    funguildr::fungi_classify(unique_taxa)
  }, error = function(e) {
    warning("Error using FUNGuild classification: ", e$message)
    return(NULL)
  })

  if (is.null(funguild_results)) {
    # Fallback: use taxonomic heuristics for common mycorrhizal groups
    return(classify_mycorrhizal_taxonomic(unique_taxa))
  }

  # Process FUNGuild results
  # IMPORTANT: Only TRUE mycorrhizal guilds count - Endophytes are NOT mycorrhizal
  # This is a critical distinction for the analysis
  funguild_classified <- funguild_results %>%
    mutate(
      resolved_name = taxon,
      is_mycorrhizal = case_when(
        # TRUE mycorrhizal guilds (form symbiotic relationships with roots)
        guild %in% c("Ectomycorrhizal", "Arbuscular Mycorrhizal", "Orchid Mycorrhizal",
                    "Ericoid Mycorrhizal") ~ TRUE,
        # Endophytes are NOT mycorrhizal - they live inside plants but don't form mycorrhizal symbiosis
        guild == "Endophyte" ~ FALSE,
        # Unknown/missing guild - conservative approach
        is.na(guild) | guild == "" ~ FALSE,
        # All other guilds are not mycorrhizal
        TRUE ~ FALSE
      )
    ) %>%
    select(resolved_name, is_mycorrhizal, guild, confidenceRanking) %>%
    rename(funguild_guild = guild, confidence_ranking = confidenceRanking)

  return(funguild_classified)
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
      confidence_ranking = confidence
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
#' Processes species detection results to add mycorrhizal classification.
#' Creates a new column indicating if each abstract mentions only mycorrhizal fungi.
#'
#' @param input_file Path to species detection results CSV
#' @param output_file Path to save results with mycorrhizal classification
#' @param force_rerun Logical, if TRUE forces re-processing even if output exists
#' @param verbose Logical, if TRUE prints progress messages
#' @return Dataframe with mycorrhizal classification added
identify_mycorrhizal_papers <- function(
  input_file = "results/species_detection_results.csv",
  output_file = "results/species_detection_results_mycorrhizal.csv",
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

  # Get unique fungal taxa for classification
  fungal_taxa <- species_data %>%
    filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
    distinct(resolved_name, kingdom, phylum, family, genus)

  if (verbose) cat("   Found", nrow(fungal_taxa), "unique fungal taxa to classify\n")

  # Classify fungal taxa using FUNGuild
  if (verbose) cat("   Classifying taxa using FUNGuild...\n")

  mycorrhizal_classifications <- classify_fungal_taxa_mycorrhizal(
    fungal_taxa$resolved_name,
    fungal_taxa
  )

  if (verbose) cat("   Classification complete for", nrow(mycorrhizal_classifications), "taxa\n")

  # Determine mycorrhizal status for each abstract
  if (verbose) cat("   Determining abstract-level mycorrhizal status...\n")

  # Get unique abstract IDs
  abstract_ids <- unique(species_data$id)

  # Process each abstract
  mycorrhizal_results <- map_dfr(abstract_ids, function(abstract_id) {
    abstract_data <- species_data %>% filter(id == abstract_id)

    is_mycorrhizal_only <- determine_abstract_mycorrhizal_status(
      abstract_data,
      mycorrhizal_classifications
    )

    # Add the classification to all rows for this abstract
    abstract_data %>%
      mutate(is_mycorrhizal_only = is_mycorrhizal_only)
  })

  # Summary statistics
  total_abstracts <- length(unique(mycorrhizal_results$id))
  mycorrhizal_only_count <- sum(mycorrhizal_results$is_mycorrhizal_only, na.rm = TRUE)
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
    write_csv(mycorrhizal_results, output_file)
  }, error = function(e) {
    warning("Failed to save results to ", output_file, ": ", e$message)
  })

  return(mycorrhizal_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "01b_mycorrhizal_check.R")) {

  # Check if FUNGuild package is installed
  if (!require("funguildr", quietly = TRUE)) {
    cat("⚠️  FUNGuild package not installed. Installing now...\n")
    tryCatch({
      install.packages("funguildr")
      library(funguildr)
    }, error = function(e) {
      stop("❌ Failed to install funguildr package: ", e$message)
    })
  }

  # Run the analysis
  results <- identify_mycorrhizal_papers()

  cat("\n✅ Mycorrhizal checking component completed!\n")
}