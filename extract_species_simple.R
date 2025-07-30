# Simple Species Extraction for Classification Results
# B. Bock  
# July 30, 2025
#
# Simplified script to run species detection on your best classification results
# Focuses on the weighted ensemble (89.8% accuracy) predictions

library(tidyverse)
library(tictoc)

# Source the detection functions
source("scripts/run_taxa_detection.R")

cat("=== SIMPLE SPECIES EXTRACTION PIPELINE ===\n")
cat("Processing weighted ensemble results (best ML performance)\n\n")

# Step 1: Prepare the data ------------------------------------------------

cat("Step 1: Preparing classification results for species detection...\n")

# Load the relevant abstracts with predictions
classification_results <- read_csv("results/relevant_abstracts_with_pa_predictions.csv")

# Focus on the weighted ensemble predictions (best performance)
abstracts_for_species <- classification_results %>%
  filter(!is.na(weighted_ensemble)) %>%
  select(
    id, article_title, abstract, authors, source_title, 
    publication_year, doi, weighted_ensemble, 
    glmnet_prob_presence, glmnet_prob_absence
  ) %>%
  # Rename to match expected format
  rename(
    title = article_title,
    predicted_label = weighted_ensemble
  ) %>%
  # Add confidence score
  mutate(
    confidence = pmax(glmnet_prob_presence, glmnet_prob_absence)
  )

cat("  Prepared", nrow(abstracts_for_species), "abstracts with weighted ensemble predictions\n")

# Create breakdown by prediction type
prediction_summary <- abstracts_for_species %>%
  count(predicted_label, name = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 1))

cat("  Prediction breakdown:\n")
print(prediction_summary)

# Step 2: Run species detection -------------------------------------------

cat("\nStep 2: Running species detection...\n")

# Save prepared data in the format expected by the detection script
temp_input_file <- "temp_abstracts_for_species_detection.csv"
write_csv(abstracts_for_species, temp_input_file)

# Run taxa detection using your existing optimized function
tryCatch({
  tic("Species detection")
  
  # Run detection on all abstracts
  results <- run_taxa_detection(
    input_file = temp_input_file,
    output_file = "results/species_detection_weighted_ensemble.csv",
    sample_size = NULL,  # Process all abstracts
    batch_size = 50     # Process in batches for memory efficiency
  )
  
  toc()
  
  cat("Species detection completed successfully!\n")
  cat("Results saved to: results/species_detection_weighted_ensemble.csv\n")
  
}, error = function(e) {
  cat("Error in species detection:", e$message, "\n")
  cat("Trying alternative approach...\n")
  
  # Alternative: Run detection manually with smaller batches
  if (file.exists("species.rds")) {
    species <- readRDS("species.rds")
  } else if (file.exists("models/species.rds")) {
    species <- readRDS("models/species.rds")
  } else {
    stop("Species reference data not found")
  }
  
  # Source the optimized functions directly
  source("scripts/optimized_taxa_detection.R")
  
  # Set up processing
  setup_parallel(workers = 2)
  lookup_tables <- create_lookup_tables(species)
  
  plant_parts_keywords <- c(
    "fruit", "fruits", "root", "roots", "leaf", "leaves", "stem", "stems", 
    "flower", "flowers", "seed", "seeds", "bark", "branch", "branches",
    "twig", "twigs", "shoot", "shoots", "bud", "buds", "trunk", "trunks",
    "pistil", "pistils", "anther", "anthers", "carpel", "carpels", 
    "sepal", "sepals", "petal", "petals", "stigma", "stigmas", 
    "style", "styles", "ovary", "ovaries", "ovule", "ovules",
    "calyx", "calyces", "corolla", "corollas", "pollen",
    "inflorescence", "inflorescences", "floret", "florets",
    "rhizome", "rhizomes", "tuber", "tubers", "bulb", "bulbs", 
    "corm", "corms", "tendril", "tendrils", "thorn", "thorns",
    "cone", "cones", "needle", "needles"
  )
  
  # Process in small batches
  batch_size <- 25
  n_batches <- ceiling(nrow(abstracts_for_species) / batch_size)
  
  cat("Processing", nrow(abstracts_for_species), "abstracts in", n_batches, "batches...\n")
  
  all_results <- map_dfr(1:n_batches, function(i) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(abstracts_for_species))
    
    batch_data <- abstracts_for_species[start_idx:end_idx, ]
    
    cat("  Processing batch", i, "of", n_batches, "(rows", start_idx, "to", end_idx, ")\n")
    
    # Process this batch
    batch_results <- process_abstracts_parallel(
      abstracts = batch_data,
      lookup_tables = lookup_tables,
      plant_parts_keywords = plant_parts_keywords,
      batch_size = 10,
      max_workers = 1
    )
    
    return(batch_results)
  })
  
  # Save results
  write_csv(all_results, "results/species_detection_weighted_ensemble.csv")
  cat("Alternative processing completed. Results saved.\n")
})

# Clean up temporary file
if (file.exists(temp_input_file)) {
  file.remove(temp_input_file)
}

# Step 3: Analyze results -------------------------------------------------

cat("\nStep 3: Analyzing species detection results...\n")

if (file.exists("results/species_detection_weighted_ensemble.csv")) {
  
  # Load and analyze results
  species_results <- read_csv("results/species_detection_weighted_ensemble.csv", show_col_types = FALSE)
  
  cat("Analysis of species detection results:\n")
  cat("  Total abstracts processed:", nrow(species_results), "\n")
  
  # Count abstracts with species detected
  abstracts_with_species <- species_results %>%
    filter(!is.na(species_detected)) %>%
    nrow()
  
  cat("  Abstracts with species detected:", abstracts_with_species, 
      "(", round(100 * abstracts_with_species / nrow(species_results), 1), "%)\n")
  
  # Count unique species
  unique_species <- species_results %>%
    filter(!is.na(species_detected)) %>%
    distinct(species_detected) %>%
    nrow()
  
  cat("  Unique species detected:", unique_species, "\n")
  
  # Breakdown by kingdom
  kingdom_summary <- species_results %>%
    filter(!is.na(species_detected)) %>%
    count(kingdom, name = "abstracts") %>%
    arrange(desc(abstracts))
  
  if (nrow(kingdom_summary) > 0) {
    cat("  Species by kingdom:\n")
    for (i in 1:nrow(kingdom_summary)) {
      cat("    ", kingdom_summary$kingdom[i], ":", kingdom_summary$abstracts[i], "abstracts\n")
    }
  }
  
  # Breakdown by prediction type
  prediction_species_summary <- species_results %>%
    filter(!is.na(species_detected)) %>%
    count(predicted_label, kingdom, name = "abstracts") %>%
    arrange(predicted_label, desc(abstracts))
  
  if (nrow(prediction_species_summary) > 0) {
    cat("  Species detections by prediction type:\n")
    print(prediction_species_summary)
  }
  
  # Save summary
  summary_data <- list(
    total_abstracts = nrow(species_results),
    abstracts_with_species = abstracts_with_species,
    unique_species = unique_species,
    kingdom_summary = kingdom_summary,
    prediction_summary = prediction_species_summary
  )
  
  # Create summary report
  capture.output({
    cat("=== SPECIES DETECTION SUMMARY REPORT ===\n")
    cat("Generated:", Sys.time(), "\n")
    cat("Input: Weighted ensemble predictions (89.8% ML accuracy)\n\n")
    
    cat("OVERVIEW:\n")
    cat("Total abstracts processed:", summary_data$total_abstracts, "\n")
    cat("Abstracts with species:", summary_data$abstracts_with_species, 
        "(", round(100 * summary_data$abstracts_with_species / summary_data$total_abstracts, 1), "%)\n")
    cat("Unique species detected:", summary_data$unique_species, "\n\n")
    
    cat("SPECIES BY KINGDOM:\n")
    if (nrow(kingdom_summary) > 0) {
      for (i in 1:nrow(kingdom_summary)) {
        cat(kingdom_summary$kingdom[i], ":", kingdom_summary$abstracts[i], "abstracts\n")
      }
    }
    cat("\n")
    
    cat("SPECIES BY PREDICTION TYPE:\n")
    if (nrow(prediction_species_summary) > 0) {
      print(prediction_species_summary)
    }
    cat("\n")
    
    cat("RECOMMENDATIONS:\n")
    cat("1. Review species detected in 'Absence' predictions - may indicate misclassification\n")
    cat("2. Focus manual curation on 'Presence' predictions with species detected\n")
    cat("3. Use species detection confidence for prioritizing review\n")
    cat("4. Consider running visualization scripts for deeper analysis\n")
  }, file = "results/species_detection_summary.txt")
  
  cat("Summary report saved to: results/species_detection_summary.txt\n")
  
} else {
  cat("No species detection results found.\n")
}

cat("\n=== SPECIES EXTRACTION COMPLETE ===\n")
cat("Key output files:\n")
cat("- results/species_detection_weighted_ensemble.csv: Main results\n")
cat("- results/species_detection_summary.txt: Summary report\n\n")
cat("Next steps:\n")
cat("1. Review the species detection results\n")
cat("2. Run scripts/visualize_taxa_results.R for detailed analysis\n")
cat("3. Consider manual review of high-confidence species detections\n")
cat("4. Use results for systematic review data extraction\n\n")

cat("Species extraction pipeline complete! 🧬🔬\n")
