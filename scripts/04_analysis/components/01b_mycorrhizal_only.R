# =============================================================================
# 01b_mycorrhizal_only.R - Assessment of mycorrhizal-only abstracts
# =============================================================================
#
# Purpose: Assess whether each abstract (grouped by id) contains only mycorrhizal fungi
#
# Description: Takes the output from 01_species_mycorrhizal_hpc_sequential.R and determines
# for each abstract whether ALL fungal taxa detected are mycorrhizal. This enables 
# identification of papers that focus exclusively on mycorrhizal relationships.
#
# Input: species_mycorrhizal_results_sequential.csv
# Output: mycorrhizal_only_abstracts.csv
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(janitor)

cat("🔍 MYCORRHIZAL-ONLY ABSTRACT ASSESSMENT\n")
cat("=====================================\n\n")

# Check if input file exists
input_file <- "results/species_mycorrhizal_results_sequential.csv"
output_file <- "results/mycorrhizal_only_abstracts.csv"

if (!file.exists(input_file)) {
  stop("❌ Input file not found: ", input_file, "\nPlease run 01_species_mycorrhizal_hpc_sequential.R first.")
}

cat("📁 Loading species detection results...\n")
tic("Loading data")

# Load the sequential results
species_results <- read_csv(input_file, show_col_types = FALSE)

toc()

cat("📊 Data summary:\n")
cat("   - Total rows: ", nrow(species_results), "\n")
cat("   - Unique abstracts: ", length(unique(species_results$id)), "\n")
cat("   - Fungal detections: ", sum(species_results$kingdom == "Fungi", na.rm = TRUE), "\n")

# Source the mycorrhizal classification functions
cat("\n🔄 Loading mycorrhizal classification functions...\n")

# Check if the functions exist and source them
if (file.exists("scripts/04_analysis/components/01_species_mycorrhizal.R")) {
  source("scripts/04_analysis/components/01_species_mycorrhizal.R")
} else {
  # Define the classification functions inline if the source file isn't available
  
  #' Classify a FUNGuild guild as mycorrhizal or not
  #' 
  #' @param guild Character string of the guild from FUNGuild
  #' @return Logical: TRUE if mycorrhizal, FALSE otherwise
  classify_guild_as_mycorrhizal <- function(guild) {
    if (is.na(guild) || guild == "" || guild == "Unknown") {
      return(FALSE)
    }
    
    # True mycorrhizal guilds - only these are considered mycorrhizal
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
    
    
    # Conservative default: assume not mycorrhizal if unclear
    return(FALSE)
  }
  
  #' Classify fungal taxa using FUNGuild dataset to determine mycorrhizal status
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
      warning("funtothefun.csv not found, using conservative taxonomic classification")
      # Return conservative classification - assume non-mycorrhizal unless clearly mycorrhizal
      return(tibble(
        resolved_name = unique_taxa,
        is_mycorrhizal = FALSE,
        funguild_guild = "Unknown",
        confidence_ranking = 0.1
      ))
    }
    
    # Load FUNGuild dataset
    if (verbose) cat("   Loading FUNGuild dataset...\n")
    
    fun_data <- tryCatch({
      read_csv(fun_data_path, show_col_types = FALSE)
    }, error = function(e) {
      warning("Error loading funtothefun dataset: ", e$message)
      return(NULL)
    })
    
    if (is.null(fun_data)) {
      return(tibble(
        resolved_name = unique_taxa,
        is_mycorrhizal = FALSE,
        funguild_guild = "Unknown", 
        confidence_ranking = 0.1
      ))
    }
    
    # Process trait data
    if (verbose) cat("   Processing FUNGuild trait data...\n")
    
    # Create lookup table for guild information
    guild_lookup <- fun_data %>%
      filter(trait_name == "guild_fg") %>%
      select(species, guild = value) %>%
      distinct()
      
    # Create lookup table for higher clade information
    clade_lookup <- fun_data %>%
      filter(trait_name == "higher_clade") %>%
      select(species, higher_clade = value) %>%
      distinct()
    
    # Classify each taxon
    classification_results <- map_dfr(unique_taxa, function(taxon) {
      
      # Get all info for the taxon first
      guild_info <- guild_lookup %>% filter(species == taxon)
      clade_info <- clade_lookup %>% filter(species == taxon)

      # Rule A: Glomeromycota is always mycorrhizal
      if (nrow(clade_info) > 0 && clade_info$higher_clade[1] == "Glomeromycota") {
          return(tibble(
              resolved_name = taxon,
              is_mycorrhizal = TRUE,
              funguild_guild = if (nrow(guild_info) > 0 && guild_info$guild[1] != "Unknown") guild_info$guild[1] else "Unknown (Glomeromycota)",
              confidence_ranking = 0.95 # Very high confidence
          ))
      }

      # Rule B: Exact guild match (for non-Glomeromycota)
      if (nrow(guild_info) > 0) {
          guild <- guild_info$guild[1]
          return(tibble(
              resolved_name = taxon,
              is_mycorrhizal = classify_guild_as_mycorrhizal(guild),
              funguild_guild = guild,
              confidence_ranking = 0.9
          ))
      }
      
      # Try genus-level match (extract genus from species name)
      genus <- str_extract(taxon, "^[A-Z][a-z]+")
      if (!is.na(genus)) {
        genus_matches <- guild_lookup %>%
          filter(str_detect(species, paste0("^", genus, "_"))) %>%
          head(5)  # Take up to 5 matches for the genus
        
        if (nrow(genus_matches) > 0) {
          # Use the most common guild for this genus
          guild_counts <- genus_matches %>%
            count(guild, sort = TRUE)
          
          guild <- guild_counts$guild[1]
          
          return(tibble(
            resolved_name = taxon,
            is_mycorrhizal = classify_guild_as_mycorrhizal(guild),
            funguild_guild = guild,
            confidence_ranking = 0.7  # Medium confidence for genus match
          ))
        }
      }
      
      # No match found - conservative approach
      return(tibble(
        resolved_name = taxon,
        is_mycorrhizal = FALSE,
        funguild_guild = "Unknown",
        confidence_ranking = 0.1
      ))
    })
    
    return(classification_results)
  }
}

cat("✅ Functions loaded successfully\n\n")

# =============================================================================
# MYCORRHIZAL CLASSIFICATION
# =============================================================================

cat("🔬 Classifying fungal taxa for mycorrhizal status...\n")
tic("Mycorrhizal classification")

# Get all unique fungal taxa that need classification
fungal_taxa_to_classify <- species_results %>%
  filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
  select(resolved_name, kingdom, phylum, family, genus) %>%
  distinct()

cat("   - Unique fungal taxa to classify: ", nrow(fungal_taxa_to_classify), "\n")

# Classify mycorrhizal status
if (nrow(fungal_taxa_to_classify) > 0) {
  mycorrhizal_classifications <- classify_fungal_taxa_mycorrhizal(
    taxa_names = fungal_taxa_to_classify$resolved_name,
    taxa_df = fungal_taxa_to_classify,
    verbose = TRUE
  )
} else {
  cat("   ⚠️ No fungal taxa found to classify\n")
  mycorrhizal_classifications <- tibble(
    resolved_name = character(0),
    is_mycorrhizal = logical(0),
    funguild_guild = character(0),
    confidence_ranking = numeric(0)
  )
}

toc()

# =============================================================================
# ABSTRACT-LEVEL ASSESSMENT
# =============================================================================

cat("\n📋 Assessing mycorrhizal-only status by abstract...\n")
tic("Abstract assessment")

# Get unique abstract IDs
abstract_ids <- unique(species_results$id)
abstract_ids <- abstract_ids[!is.na(abstract_ids)]

cat("   - Total abstracts to assess: ", length(abstract_ids), "\n")

# Assess each abstract
abstract_assessments <- map_dfr(abstract_ids, function(abstract_id) {
  
  # Get all taxa detected in this abstract
  abstract_data <- species_results %>%
    filter(id == abstract_id)
  
  # Get fungal taxa from this abstract
  fungal_taxa <- abstract_data %>%
    filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
    distinct(resolved_name)
  
  # If no fungal taxa mentioned, return FALSE (not mycorrhizal-only)
  if (nrow(fungal_taxa) == 0) {
    return(tibble(
      id = abstract_id,
      total_taxa_detected = nrow(abstract_data %>% filter(!is.na(resolved_name))),
      fungal_taxa_detected = 0,
      mycorrhizal_fungi_detected = 0,
      non_mycorrhizal_fungi_detected = 0,
      is_mycorrhizal_only = FALSE,
      assessment_notes = "No fungal taxa detected"
    ))
  }
  
  # Join with mycorrhizal classifications
  taxa_mycorrhizal_status <- fungal_taxa %>%
    left_join(mycorrhizal_classifications, by = "resolved_name") %>%
    mutate(is_mycorrhizal = if_else(is.na(is_mycorrhizal), FALSE, is_mycorrhizal))
  
  # Count mycorrhizal vs non-mycorrhizal fungi
  mycorrhizal_count <- sum(taxa_mycorrhizal_status$is_mycorrhizal, na.rm = TRUE)
  non_mycorrhizal_count <- sum(!taxa_mycorrhizal_status$is_mycorrhizal, na.rm = TRUE)
  
  # Determine if this abstract mentions only mycorrhizal fungi
  is_mycorrhizal_only <- (mycorrhizal_count > 0) && (non_mycorrhizal_count == 0)
  
  # Create assessment notes
  if (mycorrhizal_count == 0) {
    notes <- "Only non-mycorrhizal fungi detected"
  } else if (non_mycorrhizal_count == 0) {
    notes <- "Only mycorrhizal fungi detected"
  } else {
    notes <- "Mixed: mycorrhizal and non-mycorrhizal fungi detected"
  }
  
  return(tibble(
    id = abstract_id,
    total_taxa_detected = nrow(abstract_data %>% filter(!is.na(resolved_name))),
    fungal_taxa_detected = nrow(fungal_taxa),
    mycorrhizal_fungi_detected = mycorrhizal_count,
    non_mycorrhizal_fungi_detected = non_mycorrhizal_count,
    is_mycorrhizal_only = is_mycorrhizal_only,
    assessment_notes = notes
  ))
})

toc()

# =============================================================================
# DETAILED RESULTS WITH TAXA INFORMATION
# =============================================================================

cat("\n📝 Creating detailed results with taxa information...\n")
tic("Detailed results")

# Create detailed results that include the taxa detected in each abstract
detailed_results <- species_results %>%
  # Add mycorrhizal classification for fungal taxa
  left_join(
    mycorrhizal_classifications %>% select(resolved_name, is_mycorrhizal, funguild_guild),
    by = "resolved_name"
  ) %>%
  # Add abstract-level assessment
  left_join(
    abstract_assessments %>% select(id, is_mycorrhizal_only, assessment_notes),
    by = "id"
  ) %>%
  # Clean up mycorrhizal status for non-fungal taxa
  mutate(
    is_mycorrhizal = if_else(kingdom != "Fungi", NA, is_mycorrhizal),
    funguild_guild = if_else(kingdom != "Fungi", NA_character_, funguild_guild)
  )

toc()

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n📊 SUMMARY STATISTICS\n")
cat("==================\n")

# Overall summary
total_abstracts <- length(unique(abstract_assessments$id))
mycorrhizal_only_abstracts <- sum(abstract_assessments$is_mycorrhizal_only, na.rm = TRUE)
abstracts_with_fungi <- sum(abstract_assessments$fungal_taxa_detected > 0, na.rm = TRUE)

cat("Abstract-level summary:\n")
cat("   - Total abstracts assessed: ", total_abstracts, "\n")
cat("   - Abstracts with fungal taxa: ", abstracts_with_fungi, " (", round(100 * abstracts_with_fungi / total_abstracts, 1), "%)\n")
cat("   - Mycorrhizal-only abstracts: ", mycorrhizal_only_abstracts, " (", round(100 * mycorrhizal_only_abstracts / total_abstracts, 1), "%)\n")
cat("   - Mixed/non-mycorrhizal abstracts: ", abstracts_with_fungi - mycorrhizal_only_abstracts, "\n")

# Fungal taxa summary
total_fungal_detections <- sum(species_results$kingdom == "Fungi", na.rm = TRUE)
mycorrhizal_detections <- sum(detailed_results$is_mycorrhizal == TRUE, na.rm = TRUE)

cat("\nFungal taxa summary:\n")
cat("   - Total fungal detections: ", total_fungal_detections, "\n")
cat("   - Mycorrhizal fungal detections: ", mycorrhizal_detections, " (", round(100 * mycorrhizal_detections / total_fungal_detections, 1), "%)\n")
cat("   - Non-mycorrhizal fungal detections: ", total_fungal_detections - mycorrhizal_detections, "\n")

# Assessment categories
assessment_summary <- abstract_assessments %>%
  count(assessment_notes, sort = TRUE)

cat("\nAssessment categories:\n")
for (i in 1:nrow(assessment_summary)) {
  cat("   - ", assessment_summary$assessment_notes[i], ": ", assessment_summary$n[i], " abstracts\n")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n💾 Saving results...\n")

# Save the abstract-level assessment
write_csv(abstract_assessments, output_file)
cat("   ✅ Abstract assessment saved to: ", output_file, "\n")

# Save detailed results with taxa information
detailed_output_file <- "mycorrhizal_only_detailed_results.csv"
write_csv(detailed_results, detailed_output_file)
cat("   ✅ Detailed results saved to: ", detailed_output_file, "\n")

# Save summary statistics
summary_file <- "mycorrhizal_only_summary.txt"
sink(summary_file)
cat("MYCORRHIZAL-ONLY ABSTRACT ASSESSMENT SUMMARY\n")
cat("==========================================\n")
cat("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("INPUT FILE: ", input_file, "\n")
cat("OUTPUT FILES: \n")
cat("   - ", output_file, "\n")
cat("   - ", detailed_output_file, "\n\n")

cat("ABSTRACT-LEVEL SUMMARY:\n")
cat("   - Total abstracts assessed: ", total_abstracts, "\n")
cat("   - Abstracts with fungal taxa: ", abstracts_with_fungi, " (", round(100 * abstracts_with_fungi / total_abstracts, 1), "%)\n")
cat("   - Mycorrhizal-only abstracts: ", mycorrhizal_only_abstracts, " (", round(100 * mycorrhizal_only_abstracts / total_abstracts, 1), "%)\n")
cat("   - Mixed/non-mycorrhizal abstracts: ", abstracts_with_fungi - mycorrhizal_only_abstracts, "\n\n")

cat("FUNGAL TAXA SUMMARY:\n")
cat("   - Total fungal detections: ", total_fungal_detections, "\n")
cat("   - Mycorrhizal fungal detections: ", mycorrhizal_detections, " (", round(100 * mycorrhizal_detections / total_fungal_detections, 1), "%)\n")
cat("   - Non-mycorrhizal fungal detections: ", total_fungal_detections - mycorrhizal_detections, "\n\n")

cat("ASSESSMENT CATEGORIES:\n")
for (i in 1:nrow(assessment_summary)) {
  cat("   - ", assessment_summary$assessment_notes[i], ": ", assessment_summary$n[i], " abstracts\n")
}
sink()

cat("   ✅ Summary saved to: ", summary_file, "\n")

cat("\n🎉 MYCORRHIZAL-ONLY ASSESSMENT COMPLETED!\n")
cat("=====================================\n\n")

cat("📋 Next steps:\n")
cat("   1. Review the abstract-level assessment in: ", output_file, "\n")
cat("   2. Examine detailed results in: ", detailed_output_file, "\n")
cat("   3. Check summary statistics in: ", summary_file, "\n")
cat("   4. Filter mycorrhizal-only abstracts with: filter(is_mycorrhizal_only == TRUE)\n\n")