# Simple test to show species detection results
library(tidyverse)

# Load data
abstracts_data <- read_csv("results/consolidated_dataset.csv", show_col_types = FALSE)

# Test multiple abstracts to find ones with species
cat("=== SPECIES DETECTION TEST - MULTIPLE ABSTRACTS ===\n")

# Load pre-computed lookup tables
lookup_tables <- readRDS("models/lookup_tables.rds")

# Source the detection functions
source("scripts/04_analysis/optimized_taxa_detection.R")

# Test first 15 abstracts to find species
results_found <- FALSE

for (i in 1:15) {
  test_abstract <- abstracts_data$abstract[i]
  test_id <- abstracts_data$id[i]

  cat("\n--- Abstract", i, "(ID:", test_id, ") ---\n")
  cat("Text (first 300 chars):", substr(test_abstract, 1, 300), "...\n")

  # Step 1: Extract candidate names
  candidate_result <- extract_candidate_names(test_abstract)
  candidates <- candidate_result$candidates

  cat("Candidates found (", length(candidates), "): ")
  if (length(candidates) > 0 && !all(is.na(candidates))) {
    valid_candidates <- candidates[!is.na(candidates) & candidates != "NA"]
    cat(paste(head(valid_candidates, 5), collapse = ", "))
    if (length(valid_candidates) > 5) cat(" ...")
    cat("\n")

    # Step 2: Validate candidates
    valid_species <- batch_validate_names(valid_candidates, lookup_tables)

    if (nrow(valid_species) > 0) {
      cat("✅ SPECIES FOUND!\n")
      print(valid_species %>% select(user_supplied_name, resolved_name, status, kingdom, phylum, family, genus))

      # Step 3: Process matches
      all_rows <- process_taxonomic_matches(
        valid_species, lookup_tables, test_abstract,
        test_id, "Presence"
      )

      if (length(all_rows) > 0) {
        final_results <- bind_rows(all_rows)
        cat("\nFinal matches in text:\n")
        print(final_results %>% select(match_type, resolved_name, status, kingdom, phylum, family, genus))
        results_found <- TRUE
      }
    } else {
      cat("❌ No valid species found\n")
    }
  } else {
    cat("No candidates found\n")
  }
}

if (!results_found) {
  cat("\n⚠️ No species found in first 15 abstracts.\n")
  cat("This could mean:\n")
  cat("1. Abstracts don't contain explicit species names\n")
  cat("2. Species names are in a different format than expected\n")
  cat("3. Need to adjust candidate extraction patterns\n")
  cat("4. Check if abstracts are mycorrhizal-related (some might be general plant studies)\n")
} else {
  cat("\n✅ Species detection is working correctly!\n")
}