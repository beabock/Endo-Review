# Test script for the modified mycorrhizal check functionality
library(tidyverse)

cat("=== Testing Modified Mycorrhizal Check ===\n\n")

# Initialize scoring
total_tests <- 0
passed_tests <- 0
failed_tests <- 0

# Helper function for scoring
test_result <- function(passed, test_name, details = "") {
  total_tests <<- total_tests + 1
  if (passed) {
    passed_tests <<- passed_tests + 1
    cat("   ✅ PASS:", test_name, "\n")
  } else {
    failed_tests <<- failed_tests + 1
    cat("   ❌ FAIL:", test_name, "\n")
    if (details != "") cat("      Details:", details, "\n")
  }
}

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

expected_mycorrhizal <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

for (i in 1:length(test_guilds)) {
  guild <- test_guilds[i]
  is_mycorrhizal <- classify_guild_as_mycorrhizal(guild)
  expected <- expected_mycorrhizal[i]
  passed <- is_mycorrhizal == expected
  
  test_result(passed, paste0("Guild classification: ", ifelse(is.na(guild), "NA", paste0('"', guild, '"'))),
              paste0("Expected: ", expected, ", Got: ", is_mycorrhizal))
  
  cat("      Guild: ", ifelse(is.na(guild), "NA", paste0('"', guild, '"')),
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
  
  # Score the classification accuracy
  expected_mycorrhizal <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)  # Amanita, Rhizophagus, Oidiodendron, Tulasnella, Aspergillus, Unknown, Glomus
  actual_mycorrhizal <- classification_results$is_mycorrhizal
  
  correct <- sum(actual_mycorrhizal == expected_mycorrhizal, na.rm = TRUE)
  total_classified <- sum(!is.na(actual_mycorrhizal))
  accuracy <- correct / length(expected_mycorrhizal)
  
  test_result(accuracy >= 0.8, "Main classification accuracy", 
              paste0("Accuracy: ", round(accuracy, 3), " (", correct, "/", length(expected_mycorrhizal), ")"))
  
} else {
  test_result(FALSE, "Main classification function", "No results returned")
  cat("   No classification results returned\n")
}

cat("\n4. Accuracy Testing with Known Mycorrhizal/Non-Mycorrhizal Taxa:\n")
cat("==========================================================\n")

# Test accuracy with a larger set of known taxa
accuracy_test_taxa <- c(
  # Definitely mycorrhizal
  "Amanita muscaria", "Laccaria bicolor", "Tuber melanosporum", "Suillus bovinus",
  "Rhizophagus irregularis", "Glomus mosseae", "Gigaspora margarita", 
  "Paxillus involutus", "Hebeloma crustuliniforme", "Cenococcum geophilum",
  
  # Definitely non-mycorrhizal (saprotrophs, pathogens)
  "Aspergillus niger", "Penicillium chrysogenum", "Trichoderma harzianum",
  "Fusarium oxysporum", "Botrytis cinerea", "Rhizoctonia solani",
  "Sclerotinia sclerotiorum", "Colletotrichum gloeosporioides",
  
  # Ericoid mycorrhizal
  "Oidiodendron maius", "Pezizella ericae", "Meliniomyces variabilis",
  
  # Orchid mycorrhizal
  "Tulasnella calospora", "Ceratorhiza goodyerae", "Epulorhiza repens",
  
  # Arbuscular mycorrhizal
  "Diversispora spurca", "Acaulospora laevis", "Scutellospora calospora"
)

accuracy_test_df <- data.frame(
  resolved_name = accuracy_test_taxa,
  kingdom = "Fungi",
  phylum = "Basidiomycota",  # Will be overridden by actual data
  family = NA,
  genus = NA
)

# Expected classifications (1 = mycorrhizal, 0 = non-mycorrhizal)
expected_classifications <- c(
  rep(1, 10),  # mycorrhizal
  rep(0, 8),   # non-mycorrhizal
  rep(1, 3),   # ericoid
  rep(1, 3),   # orchid
  rep(1, 3)    # arbuscular
)

cat("Testing ", length(accuracy_test_taxa), " taxa with known mycorrhizal status...\n")

accuracy_results <- classify_fungal_taxa_mycorrhizal(accuracy_test_taxa, accuracy_test_df)

if (nrow(accuracy_results) > 0) {
  # Calculate accuracy
  actual_classifications <- as.numeric(accuracy_results$is_mycorrhizal)
  correct_predictions <- sum(actual_classifications == expected_classifications, na.rm = TRUE)
  total_predictions <- sum(!is.na(actual_classifications))
  accuracy <- correct_predictions / total_predictions
  
  cat("Accuracy Results:\n")
  cat("  Total taxa tested: ", total_predictions, "\n")
  cat("  Correct predictions: ", correct_predictions, "\n")
  cat("  Accuracy: ", round(accuracy * 100, 1), "%\n\n")
  
  # Test specific categories
  mycorrhizal_accuracy <- sum(actual_classifications[expected_classifications == 1] == 1, na.rm = TRUE) / sum(expected_classifications == 1)
  non_mycorrhizal_accuracy <- sum(actual_classifications[expected_classifications == 0] == 0, na.rm = TRUE) / sum(expected_classifications == 0)
  
  cat("Category-specific accuracy:\n")
  cat("  Mycorrhizal detection: ", round(mycorrhizal_accuracy * 100, 1), "%\n")
  cat("  Non-mycorrhizal detection: ", round(non_mycorrhizal_accuracy * 100, 1), "%\n\n")
  
  test_result(accuracy >= 0.8, "Overall mycorrhizal classification accuracy", 
              paste0("Accuracy: ", round(accuracy * 100, 1), "% (", correct_predictions, "/", total_predictions, ")"))
  test_result(mycorrhizal_accuracy >= 0.8, "Mycorrhizal taxa detection accuracy")
  test_result(non_mycorrhizal_accuracy >= 0.8, "Non-mycorrhizal taxa detection accuracy")
  
} else {
  test_result(FALSE, "Accuracy testing", "No results returned from classification function")
}

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

# Final scoring summary
cat("\n" , rep("=", 50), "\n", sep = "")
cat("TEST SUMMARY\n")
cat(rep("=", 50), "\n", sep = "")
cat(sprintf("Total Tests:        %d\n", total_tests))
cat(sprintf("Passed:             %d (%.1f%%)\n", passed_tests, 100 * passed_tests / total_tests))
cat(sprintf("Failed:             %d (%.1f%%)\n", failed_tests, 100 * failed_tests / total_tests))
cat(rep("=", 50), "\n", sep = "")

cat("\n=== Mycorrhizal Check Testing Complete ===\n")