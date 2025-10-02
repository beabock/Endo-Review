# Test script for the enhanced mycorrhizal check with funtothefun metadata
library(tidyverse)

cat("=== Testing Enhanced Mycorrhizal Check Output ===\n\n")

# Source the modified functions
source("scripts/04_analysis/components/01b_mycorrhizal_check.R")

cat("1. Testing enhanced classification function...\n")

# Test with sample fungal taxa
test_taxa <- c(
  "Amanita_muscaria",      # Should match and get full trait data
  "Rhizophagus_irregularis", # Should match and get full trait data
  "Oidiodendron_maius",    # Should match and get full trait data
  "Tulasnella_calospora",  # Should match and get full trait data
  "Aspergillus_niger",     # Should match and get full trait data
  "Unknown_species",       # Should use taxonomic fallback
  "Glomus_sp"             # Should match genus
)

test_taxa_df <- data.frame(
  resolved_name = test_taxa,
  kingdom = "Fungi",
  phylum = "Basidiomycota",
  family = "Amanitaceae",
  genus = "Amanita"
)

# Test enhanced classification
enhanced_results <- classify_fungal_taxa_mycorrhizal(test_taxa, test_taxa_df)

cat("\n2. Enhanced classification results:\n")
if (nrow(enhanced_results) > 0) {
  print(enhanced_results)
} else {
  cat("   No enhanced classification results returned\n")
}

cat("\n3. Column names in enhanced results:\n")
cat("   ", paste(names(enhanced_results), collapse = ", "), "\n")

cat("\n4. Testing with sample species detection data...\n")

# Create sample species detection data that mimics the real format
sample_species_data <- data.frame(
  id = c(1, 1, 2, 2, 3),
  predicted_label = c(NA, NA, NA, NA, NA),
  match_type = c("species", "genus", "species", "family", "species"),
  canonicalName = c("Amanita muscaria", "Penicillium", "Rhizophagus irregularis", "Clavicipitaceae", "Oidiodendron maius"),
  kingdom = c("Fungi", "Fungi", "Fungi", "Fungi", "Fungi"),
  phylum = c("Basidiomycota", "Ascomycota", "Glomeromycota", "Ascomycota", "Ascomycota"),
  family = c("Amanitaceae", "Aspergillaceae", "Glomeraceae", "Clavicipitaceae", "Myxotrichaceae"),
  genus = c("Amanita", "Penicillium", "Rhizophagus", NA, "Oidiodendron"),
  status = c("accepted", "accepted", "accepted", "accepted", "accepted"),
  resolved_name = c("Amanita_muscaria", "Penicillium", "Rhizophagus_irregularis", "Clavicipitaceae", "Oidiodendron_maius"),
  acceptedScientificName = c("Amanita muscaria", "Penicillium sp.", "Rhizophagus irregularis", "Clavicipitaceae", "Oidiodendron maius"),
  user_supplied_name = c("Amanita muscaria", "Penicillium", "Rhizophagus irregularis", "Clavicipitaceae", "Oidiodendron maius"),
  canonicalName_lower = c("amanita muscaria", "penicillium", "rhizophagus irregularis", "clavicipitaceae", "oidiodendron maius")
)

# Create results directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
  cat("   Created results directory\n")
}

# Save sample data to test file
write_csv(sample_species_data, "results/test_species_input.csv")

cat("   Created test species data file with", nrow(sample_species_data), "records\n")

# Test the enhanced identification function
enhanced_mycorrhizal_results <- identify_mycorrhizal_papers(
  input_file = "results/test_species_input.csv",
  output_file = "results/test_enhanced_mycorrhizal_output.csv",
  verbose = TRUE
)

cat("\n5. Enhanced mycorrhizal results structure:\n")
cat("   Dimensions: ", dim(enhanced_mycorrhizal_results), "\n")
cat("   Column names: ", paste(names(enhanced_mycorrhizal_results), collapse = ", "), "\n")

cat("\n6. Sample of enhanced results:\n")
print(head(enhanced_mycorrhizal_results, 3))

cat("\n7. Summary of mycorrhizal classifications:\n")
if ("is_mycorrhizal" %in% names(enhanced_mycorrhizal_results)) {
  mycorrhizal_summary <- enhanced_mycorrhizal_results %>%
    filter(kingdom == "Fungi") %>%
    group_by(is_mycorrhizal) %>%
    summarise(count = n(), .groups = "drop")
  print(mycorrhizal_summary)
}

cat("\n8. Checking that all original columns are preserved:\n")
original_cols <- c("id", "predicted_label", "match_type", "canonicalName", "kingdom",
                   "phylum", "family", "genus", "status", "resolved_name",
                   "acceptedScientificName", "user_supplied_name", "canonicalName_lower")

missing_cols <- setdiff(original_cols, names(enhanced_mycorrhizal_results))
extra_cols <- setdiff(names(enhanced_mycorrhizal_results), c(original_cols, "is_mycorrhizal", "funguild_guild", "confidence_ranking", "trophic_mode", "growth_form", "trait_confidence", "is_mycorrhizal_only"))

print(names(enhanced_mycorrhizal_results))

if (length(missing_cols) == 0) {
  cat("   ✅ All original columns preserved\n")
} else {
  cat("   ❌ Missing columns: ", paste(missing_cols, collapse = ", "), "\n")
}

if (length(extra_cols) == 0) {
  cat("   ✅ Only expected new columns added\n")
} else {
  cat("   ❌ Unexpected columns: ", paste(extra_cols, collapse = ", "), "\n")
}

cat("\n9. Mycorrhizal-only summary:\n")
if ("is_mycorrhizal_only" %in% names(enhanced_mycorrhizal_results)) {
  mycorrhizal_only_summary <- enhanced_mycorrhizal_results %>%
    group_by(is_mycorrhizal_only) %>%
    summarise(count = n(), .groups = "drop")
  print(mycorrhizal_only_summary)
}

# Clean up test files
file.remove("results/test_species_input.csv")
file.remove("results/test_enhanced_mycorrhizal_output.csv")

cat("\n=== Enhanced Mycorrhizal Check Testing Complete ===\n")