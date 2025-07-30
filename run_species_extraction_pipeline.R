# Species Extraction Pipeline for Classification Results
# B. Bock
# July 30, 2025
# 
# This script runs species detection on the relevant abstracts identified by 
# the relevance and presence/absence classification pipeline.
# 
# Input: results/relevant_abstracts_with_pa_predictions.csv
# Output: species extraction results with detected taxa information

library(tidyverse)
library(tictoc)
library(janitor)

# Source the optimized taxa detection functions
source("scripts/optimized_taxa_detection.R")

cat("=== ENDOPHYTE SYSTEMATIC REVIEW: SPECIES EXTRACTION PIPELINE ===\n")
cat("Processing abstracts from ML classification pipeline\n")
cat("Input: relevant_abstracts_with_pa_predictions.csv\n\n")

# Configuration ---------------------------------------------------------------

# Set up parallel processing for species detection
setup_parallel(workers = 4)  # Adjust based on your system

# Define which predictions to use for species extraction
prediction_methods <- c(
  "weighted_ensemble",    # Best overall performance (89.8% accuracy)
  "threshold_ensemble",   # Alternative approach
  "final_classification"  # Combined relevance + P/A pipeline
)

# Filter criteria for species extraction
filter_criteria <- list(
  all_relevant = "all relevant abstracts",
  presence_only = "presence predictions only", 
  high_confidence = "high confidence predictions only"
)

# Load and prepare data -------------------------------------------------------

cat("Step 1: Loading classification results...\n")
tic("Data loading")

# Load the classification results
classification_results <- read_csv("results/relevant_abstracts_with_pa_predictions.csv") %>%
  clean_names()

cat("  Loaded", nrow(classification_results), "relevant abstracts\n")

# Load species reference data
if (file.exists("models/species.rds")) {
  species <- readRDS("models/species.rds")
  cat("  Loaded species reference data:", nrow(species), "species records\n")
} else {
  stop("Species reference data not found at models/species.rds")
}

# Create optimized lookup tables for species detection
cat("  Creating species lookup tables...\n")
lookup_tables <- create_lookup_tables(species)

toc()

# Define plant parts keywords for context detection
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

# Run species extraction for different prediction methods -------------------

for (method in prediction_methods) {
  cat("\n=== Processing", method, "predictions ===\n")
  
  # Filter abstracts based on prediction method
  if (method == "final_classification") {
    # Use final classification (includes relevance + P/A pipeline)
    abstracts_to_process <- classification_results %>%
      filter(final_classification %in% c("Presence", "Absence")) %>%
      mutate(prediction = final_classification)
  } else {
    # Use specific prediction method
    abstracts_to_process <- classification_results %>%
      filter(!is.na(.data[[method]])) %>%
      mutate(prediction = .data[[method]])
  }
  
  cat("  Found", nrow(abstracts_to_process), "abstracts for", method, "\n")
  
  if (nrow(abstracts_to_process) == 0) {
    cat("  No abstracts found for", method, "- skipping\n")
    next
  }
  
  # Run species detection for different filtering approaches
  for (filter_name in names(filter_criteria)) {
    cat("\n--- Processing filter:", filter_criteria[[filter_name]], "---\n")
    tic(paste("Species extraction -", method, filter_name))
    
    # Apply filter criteria
    if (filter_name == "all_relevant") {
      final_abstracts <- abstracts_to_process
    } else if (filter_name == "presence_only") {
      final_abstracts <- abstracts_to_process %>%
        filter(prediction == "Presence")
    } else if (filter_name == "high_confidence") {
      # High confidence: use strict P/A thresholds
      final_abstracts <- abstracts_to_process %>%
        filter(pa_strict %in% c("Presence", "Absence")) %>%
        mutate(prediction = pa_strict)
    }
    
    cat("    Processing", nrow(final_abstracts), "abstracts\n")
    
    if (nrow(final_abstracts) == 0) {
      cat("    No abstracts meet criteria - skipping\n")
      next
    }
    
    # Prepare data in the format expected by taxa detection functions
    prepared_abstracts <- final_abstracts %>%
      select(id, article_title, abstract, authors, source_title, 
             publication_year, doi, prediction) %>%
      rename(
        title = article_title,
        label = prediction
      ) %>%
      # Add required columns
      mutate(
        threshold = paste0(method, "_", filter_name),
        predicted_label = label
      )
    
    # Run species detection with improved functions
    cat("    Running species detection...\n")
    
    tryCatch({
      # Process in batches for memory efficiency
      batch_size <- 50
      n_batches <- ceiling(nrow(prepared_abstracts) / batch_size)
      
      all_taxa_results <- map_dfr(1:n_batches, function(batch_num) {
        start_idx <- (batch_num - 1) * batch_size + 1
        end_idx <- min(batch_num * batch_size, nrow(prepared_abstracts))
        
        batch_abstracts <- prepared_abstracts[start_idx:end_idx, ]
        
        cat("      Processing batch", batch_num, "of", n_batches, 
            "(rows", start_idx, "to", end_idx, ")\n")
        
        # Run improved taxa detection on batch
        batch_results <- process_abstracts_parallel(
          abstracts = batch_abstracts,
          lookup_tables = lookup_tables,
          plant_parts_keywords = plant_parts_keywords,
          batch_size = 10,  # Small batches for memory efficiency
          max_workers = 2   # Reduced workers for nested parallelism
        )
        
        return(batch_results)
      })
      
      # Save results
      output_filename <- paste0("results/species_extraction_", method, "_", filter_name, ".csv")
      write_csv(all_taxa_results, output_filename)
      
      cat("    Saved results to:", output_filename, "\n")
      cat("    Detected species in", nrow(all_taxa_results), "records\n")
      
      # Quick summary
      species_summary <- all_taxa_results %>%
        filter(!is.na(species_detected)) %>%
        count(kingdom, name = "abstracts_with_species") %>%
        arrange(desc(abstracts_with_species))
      
      if (nrow(species_summary) > 0) {
        cat("    Species detection summary:\n")
        print(species_summary)
      }
      
    }, error = function(e) {
      cat("    ERROR in species detection for", method, filter_name, ":", e$message, "\n")
    })
    
    toc()
  }
}

# Create summary report -------------------------------------------------------

cat("\n=== Creating Species Extraction Summary ===\n")
tic("Summary creation")

# Find all species extraction result files
result_files <- list.files("results", pattern = "species_extraction_.*\\.csv", full.names = TRUE)

if (length(result_files) > 0) {
  # Create comprehensive summary
  summary_stats <- map_dfr(result_files, function(file) {
    filename <- basename(file)
    
    tryCatch({
      data <- read_csv(file, show_col_types = FALSE)
      
      summary <- data %>%
        summarise(
          method = str_extract(filename, "(?<=species_extraction_)[^_]+"),
          filter = str_extract(filename, "(?<=_)[^_]+(?=\\.csv)"),
          total_abstracts = n(),
          abstracts_with_species = sum(!is.na(species_detected)),
          unique_species = n_distinct(species_detected, na.rm = TRUE),
          plant_species = sum(kingdom == "Plantae", na.rm = TRUE),
          fungal_species = sum(kingdom == "Fungi", na.rm = TRUE),
          .groups = "drop"
        )
      
      return(summary)
    }, error = function(e) {
      return(NULL)
    })
  }) %>%
  filter(!is.na(method))
  
  # Save summary
  write_csv(summary_stats, "results/species_extraction_summary.csv")
  
  cat("Species extraction summary:\n")
  print(summary_stats)
  
  # Create detailed summary report
  capture.output({
    cat("=== ENDOPHYTE SYSTEMATIC REVIEW: SPECIES EXTRACTION SUMMARY ===\n\n")
    cat("Generated:", Sys.time(), "\n\n")
    
    cat("OVERVIEW:\n")
    cat("Total classification methods processed:", n_distinct(summary_stats$method), "\n")
    cat("Total filter approaches:", n_distinct(summary_stats$filter), "\n")
    cat("Total result files generated:", nrow(summary_stats), "\n\n")
    
    cat("DETAILED RESULTS:\n")
    for (i in 1:nrow(summary_stats)) {
      row <- summary_stats[i, ]
      cat(sprintf("Method: %s | Filter: %s\n", row$method, row$filter))
      cat(sprintf("  - Abstracts processed: %d\n", row$total_abstracts))
      cat(sprintf("  - Abstracts with species: %d (%.1f%%)\n", 
                  row$abstracts_with_species, 
                  100 * row$abstracts_with_species / row$total_abstracts))
      cat(sprintf("  - Unique species detected: %d\n", row$unique_species))
      cat(sprintf("  - Plant species: %d | Fungal species: %d\n\n", 
                  row$plant_species, row$fungal_species))
    }
    
    cat("RECOMMENDATIONS:\n")
    cat("1. Focus on 'weighted_ensemble' results - best ML performance (89.8% accuracy)\n")
    cat("2. Use 'presence_only' filter for targeted species identification\n")
    cat("3. Use 'high_confidence' filter for conservative species lists\n")
    cat("4. Manually review species detections from 'Absence' classifications\n\n")
    
    cat("FILES GENERATED:\n")
    for (file in result_files) {
      cat("- ", basename(file), "\n")
    }
  }, file = "results/species_extraction_report.txt")
  
  cat("Detailed report saved to: results/species_extraction_report.txt\n")
  
} else {
  cat("No species extraction result files found.\n")
}

toc()

# Final summary ---------------------------------------------------------------

cat("\n=== SPECIES EXTRACTION PIPELINE COMPLETE ===\n")
cat("Results saved to results/ directory\n")
cat("Key files:\n")
cat("- species_extraction_*.csv: Detailed species detection results\n")
cat("- species_extraction_summary.csv: Summary statistics\n")
cat("- species_extraction_report.txt: Comprehensive report\n\n")

cat("Next steps:\n")
cat("1. Review species extraction results for quality\n")
cat("2. Use scripts/visualize_taxa_results.R for analysis and visualization\n")
cat("3. Focus on weighted_ensemble results for best accuracy\n")
cat("4. Consider manual review of uncertain classifications\n\n")

cat("Species extraction pipeline complete! 🧬\n")
