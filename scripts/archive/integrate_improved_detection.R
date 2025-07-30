# Integration Script for Improved Taxa Detection
# This script demonstrates how to integrate the improved taxa detection functions
# into your existing workflow

library(tidyverse)
library(rgbif)
library(furrr)
library(janitor)
library(vroom)

# Source the improved functions
source("improved_taxa_detection.R")

# Set up parallel processing
options(future.globals.maxSize = 2000 * 1024^2)  # 2GB limit
plan(multisession, workers = min(4, availableCores() - 1))

# Load species data
if (file.exists("species.rds")) {
  species <- readRDS("species.rds")
} else {
  message("Loading GBIF backbone data...")
  backbone <- vroom("gbif_backbone/Taxon.tsv", delim = "\t", progress = FALSE)
  species <- backbone %>%
    filter(kingdom %in% c("Plantae", "Fungi") & taxonRank == "species") 
  saveRDS(species, "species.rds")
}

# Load labeled abstracts
prepare_abstracts <- function() {
  raw_preds <- vroom("full_predictions_with_metadata.csv", progress = FALSE, show_col_types = FALSE)
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

# Create lookup tables
lookup_tables <- create_lookup_tables(species)

# Process abstracts with improved functions
process_abstracts_improved <- function(threshold_name = "label_loose", sample_size = NULL) {
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
  
  message("Processing ", nrow(abs), " abstracts...")
  
  # Process abstracts with progress tracking
  results <- vector("list", nrow(abs))
  
  for (i in seq_len(nrow(abs))) {
    if (i %% 10 == 0 || i == nrow(abs)) {
      message("Processed ", i, "/", nrow(abs), " abstracts")
    }
    
    # Use the improved extract_plant_info function
    results[[i]] <- extract_plant_info(
      text = abs$abstract[i], 
      abstract_id = abs$id[i], 
      predicted_label = abs$predicted_label[i], 
      lookup_tables = lookup_tables,
      plant_parts_keywords = plant_parts_keywords
    )
  }
  
  # Combine results
  final_results <- bind_rows(results)
  
  # Save results
  suffix <- if (!is.null(sample_size)) paste0("_sample", sample_size) else ""
  out_name <- paste0("taxa_info_results_improved_", threshold_name, suffix, ".csv")
  write_csv(final_results, out_name)
  
  message("Processing complete! Saved ", nrow(final_results), " rows to ", out_name)
  return(final_results)
}

# Alternative: Process abstracts in parallel with furrr
process_abstracts_parallel <- function(threshold_name = "label_loose", sample_size = NULL) {
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
  
  message("Processing ", nrow(abs), " abstracts in parallel...")
  
  # Process abstracts in parallel
  results <- future_map2_dfr(
    abs$abstract, abs$id,
    function(text, id) {
      extract_plant_info(
        text = text,
        abstract_id = id,
        predicted_label = abs$predicted_label[abs$id == id],
        lookup_tables = lookup_tables,
        plant_parts_keywords = plant_parts_keywords
      )
    },
    .options = furrr_options(seed = TRUE)
  )
  
  # Save results
  suffix <- if (!is.null(sample_size)) paste0("_sample", sample_size) else ""
  out_name <- paste0("taxa_info_results_improved_parallel_", threshold_name, suffix, ".csv")
  write_csv(results, out_name)
  
  message("Processing complete! Saved ", nrow(results), " rows to ", out_name)
  return(results)
}

# Compare original vs improved detection
compare_detection <- function(sample_size = 10) {
  # Load original results if available
  original_file <- paste0("taxa_info_results_label_loose_sample", sample_size, ".csv")
  if (file.exists(original_file)) {
    original_results <- read_csv(original_file, show_col_types = FALSE)
  } else {
    message("Original results file not found. Run your original process_abstracts function first.")
    return(NULL)
  }
  
  # Process with improved functions
  improved_results <- process_abstracts_improved("label_loose", sample_size)
  
  # Compare species detection
  original_species <- original_results %>%
    filter(match_type == "species") %>%
    group_by(id) %>%
    summarise(
      species_count_original = n(),
      species_names_original = paste(resolved_name, collapse = ", "),
      .groups = "drop"
    )
  
  improved_species <- improved_results %>%
    filter(match_type == "species") %>%
    group_by(id) %>%
    summarise(
      species_count_improved = n(),
      species_names_improved = paste(resolved_name, collapse = ", "),
      .groups = "drop"
    )
  
  # Join results
  comparison <- full_join(
    original_species, 
    improved_species,
    by = "id"
  ) %>%
    mutate(
      species_count_original = replace_na(species_count_original, 0),
      species_count_improved = replace_na(species_count_improved, 0),
      species_names_original = replace_na(species_names_original, ""),
      species_names_improved = replace_na(species_names_improved, ""),
      difference = species_count_improved - species_count_original
    )
  
  # Add abstract text for reference
  comparison <- comparison %>%
    left_join(
      labeled_abstracts %>% select(id, abstract),
      by = "id"
    )
  
  # Save comparison
  write_csv(comparison, paste0("detection_comparison_sample", sample_size, ".csv"))
  
  # Summary
  message("\nDetection Comparison Summary:")
  message("Total abstracts: ", nrow(comparison))
  message("Abstracts with more species detected in improved version: ", 
          sum(comparison$difference > 0))
  message("Abstracts with same number of species detected: ", 
          sum(comparison$difference == 0))
  message("Abstracts with fewer species detected in improved version: ", 
          sum(comparison$difference < 0))
  
  return(comparison)
}

# Run with a small sample first
if (interactive()) {
  message("Running improved detection on a small sample...")
  results_sample <- process_abstracts_improved(sample_size = 10)
  
  message("\nTo process all abstracts, run:")
  message("results <- process_abstracts_improved()")
  
  message("\nTo compare with original results, run:")
  message("comparison <- compare_detection(sample_size = 10)")
}
