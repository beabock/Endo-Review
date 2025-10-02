# Test script for the modified mycorrhizal check functionality
library(tidyverse)

cat("=== Testing Modified Mycorrhizal Check ===\n\n")

# Source the modified functions
source("scripts/04_analysis/components/01b_mycorrhizal_check.R")

cat("1. Testing guild classification function...\n")

# Test the helper function
test_guilds <- c(
  "Ectomycorrhizal",
  "Arbuscular Mycorrhizal",
  "Ericoid Mycorrhizal",
  "Orchid Mycorrhizal",
  "Endophyte",
  "Plant Pathogen",
  "Wood Saprotroph",
  "Ectomycorrhizal-Fungal Parasite",
  "Animal Pathogen-Endophyte-Plant Pathogen-Wood Saprotroph",
  NA,
  ""
)

for (guild in test_guilds) {
  is_mycorrhizal <- classify_guild_as_mycorrhizal(guild)
  cat("   Guild: ", ifelse(is.na(guild), "NA", paste0('"', guild, '"')),
      " -> Mycorrhizal: ", is_mycorrhizal, "\n")
}

cat("\n2. Testing main classification function with sample data...\n")

# Create test species data
test_taxa <- c(
  "Amanita_muscaria",      # Should be Ectomycorrhizal
  "Rhizophagus_irregularis", # Should be Arbuscular Mycorrhizal
  "Oidiodendron_maius",    # Should be Ericoid Mycorrhizal
  "Tulasnella_calospora",  # Should be Orchid Mycorrhizal
  "Aspergillus_niger",     # Should be saprotroph (no match)
  "Unknown_species",       # Should use taxonomic fallback
  "Glomus_sp"             # Should match genus Glomus
)

test_taxa_df <- data.frame(
  resolved_name = test_taxa,
  kingdom = "Fungi",
  phylum = "Basidiomycota",
  family = "Amanitaceae",
  genus = "Amanita"
)

cat("   Test taxa: ", paste(test_taxa, collapse = ", "), "\n")

# Test classification
classification_results <- classify_fungal_taxa_mycorrhizal(test_taxa, test_taxa_df)

cat("\n3. Classification results:\n")
if (nrow(classification_results) > 0) {
  print(classification_results %>%
    select(resolved_name, is_mycorrhizal, funguild_guild, confidence_ranking))
} else {
  cat("   No classification results returned\n")
}

cat("\n4. Testing with actual species detection data...\n")

# Look for existing species detection results
species_file <- "results/species_detection_results.csv"

if (file.exists(species_file)) {
  cat("   Found species detection results, loading...\n")
  species_data <- read_csv(species_file, show_col_types = FALSE)

  # Get sample of fungal taxa
  fungal_sample <- species_data %>%
    filter(kingdom == "Fungi", !is.na(resolved_name)) %>%
    select(resolved_name, kingdom, phylum, family, genus) %>%
    distinct() %>%
    head(10)

  cat("   Testing with ", nrow(fungal_sample), " fungal taxa from actual data\n")

  sample_results <- classify_fungal_taxa_mycorrhizal(
    fungal_sample$resolved_name,
    fungal_sample
  )

  cat("\n5. Sample classification results:\n")
  print(sample_results %>%
    select(resolved_name, is_mycorrhizal, funguild_guild, confidence_ranking))

  # Summary statistics
  cat("\n6. Summary statistics:\n")
  cat("   - Total taxa tested: ", nrow(sample_results), "\n")
  cat("   - Mycorrhizal taxa: ", sum(sample_results$is_mycorrhizal, na.rm = TRUE), "\n")
  cat("   - Non-mycorrhizal taxa: ", sum(!sample_results$is_mycorrhizal, na.rm = TRUE), "\n")
  cat("   - Average confidence: ", round(mean(sample_results$confidence_ranking, na.rm = TRUE), 3), "\n")

} else {
  cat("   No species detection results found at ", species_file, "\n")
  cat("   Run 01_extract_species.R first to generate test data\n")
}

cat("\n7. Testing full pipeline (if data available)...\n")

if (file.exists(species_file)) {
  cat("   Running identify_mycorrhizal_papers function...\n")

  # Run with a small sample for testing
  test_results <- identify_mycorrhizal_papers(
    input_file = species_file,
    output_file = "results/test_mycorrhizal_results.csv",
    verbose = TRUE
  )

  cat("   Pipeline test completed successfully!\n")
  cat("   Results saved to: results/test_mycorrhizal_results.csv\n")

} else {
  cat("   Skipping pipeline test - no input data available\n")
}

cat("\n=== Mycorrhizal Check Testing Complete ===\n")