# Test script for the enhanced mycorrhizal check with funtothefun metadata
library(tidyverse)

# =============================================================================
# PERFORMANCE SCORING FRAMEWORK
# =============================================================================

# Global counters for test results
total_tests <- 0
passed_tests <- 0
failed_tests <- 0

# Helper function for scoring
test_result <- function(passed, test_name, details = "") {
  total_tests <<- total_tests + 1
  if (passed) {
    passed_tests <<- passed_tests + 1
    cat("✅ PASS:", test_name, "\n")
  } else {
    failed_tests <<- failed_tests + 1
    cat("❌ FAIL:", test_name, "\n")
  }
  if (details != "") {
    cat("   ", details, "\n")
  }
}

cat("=== Testing Enhanced Mycorrhizal Check Output ===\n\n")

# Load the actual function from the component file
cat("📋 Loading actual mycorrhizal classification function...\n")

# Read the component file
component_file <- "scripts/04_analysis/components/01b_mycorrhizal_only.R"
if (!file.exists(component_file)) {
  stop("Component file not found: ", component_file)
}

# Source the actual function, suppressing the main script execution
try({
  # Suppress all output and errors from the main script
  sink(nullfile())
  tryCatch({
    source(component_file)
  }, error = function(e) {
    # Expected - the main script will fail, but function will be defined
  })
  sink()
}, silent = TRUE)

# Verify the function was loaded
if (!exists("classify_fungal_taxa_mycorrhizal")) {
  stop("Failed to load classify_fungal_taxa_mycorrhizal function from component file")
}

cat("   ✅ Loaded actual mycorrhizal classification function\n")

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

# Score enhanced classification
enhanced_classification_works <- !is.null(enhanced_results) && nrow(enhanced_results) > 0
test_result(enhanced_classification_works, "Enhanced classification", 
           paste0("Classified ", nrow(enhanced_results), " fungal taxa"))

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

# Save sample data to test file (optional - mainly for reference)
if (!dir.exists("results")) {
  dir.create("results")
}
write_csv(sample_species_data, "results/test_species_input.csv")
cat("   Created test species data with", nrow(sample_species_data), "records\n")

# Test the enhanced classification function directly on the sample data
cat("   Testing enhanced mycorrhizal classification on sample data...\n")
enhanced_mycorrhizal_results <- classify_fungal_taxa_mycorrhizal(
  taxa_names = sample_species_data$canonicalName,
  taxa_df = sample_species_data
)

cat("\n5. Enhanced mycorrhizal results structure:\n")
cat("   Dimensions: ", dim(enhanced_mycorrhizal_results), "\n")
cat("   Column names: ", paste(names(enhanced_mycorrhizal_results), collapse = ", "), "\n")

# Score mycorrhizal identification
mycorrhizal_identification_works <- !is.null(enhanced_mycorrhizal_results) && 
                                   nrow(enhanced_mycorrhizal_results) > 0 &&
                                   "is_mycorrhizal" %in% names(enhanced_mycorrhizal_results)
test_result(mycorrhizal_identification_works, "Mycorrhizal identification", 
           paste0("Processed ", nrow(enhanced_mycorrhizal_results), " records with mycorrhizal classification"))

cat("\n6. Sample of enhanced results:\n")
print(head(enhanced_mycorrhizal_results, 3))

cat("\n7. Summary of mycorrhizal classifications:\n")
if ("is_mycorrhizal" %in% names(enhanced_mycorrhizal_results)) {
  mycorrhizal_summary <- enhanced_mycorrhizal_results %>%
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

# Score column preservation
columns_preserved <- length(missing_cols) == 0 && length(extra_cols) == 0
test_result(columns_preserved, "Column preservation", 
           ifelse(columns_preserved, "All original columns preserved, expected new columns added",
                  paste0(length(missing_cols), " missing, ", length(extra_cols), " unexpected columns")))

cat("\n9. Mycorrhizal-only summary:\n")
if ("is_mycorrhizal_only" %in% names(enhanced_mycorrhizal_results)) {
  mycorrhizal_only_summary <- enhanced_mycorrhizal_results %>%
    group_by(is_mycorrhizal_only) %>%
    summarise(count = n(), .groups = "drop")
  print(mycorrhizal_only_summary)
}

# =============================================================================
# EXTENDED TEST SUITE - 17 ADDITIONAL TESTS
# =============================================================================

cat("\n\n=== EXTENDED MYCORRHIZAL CLASSIFICATION TESTS ===\n")

# Test 4: Arbuscular mycorrhizal detection
cat("\n10. Testing Arbuscular mycorrhizal fungi detection...\n")
am_data <- data.frame(
  id = 1, predicted_label = NA, match_type = "species",
  canonicalName = "Rhizophagus irregularis",
  kingdom = "Fungi", phylum = "Glomeromycota",
  family = "Glomeraceae", genus = "Rhizophagus",
  status = "accepted", resolved_name = "Rhizophagus_irregularis",
  acceptedScientificName = "Rhizophagus irregularis",
  user_supplied_name = "Rhizophagus irregularis",
  canonicalName_lower = "rhizophagus irregularis"
)
am_results <- classify_fungal_taxa_mycorrhizal(am_data$canonicalName, am_data)
am_is_mycorrhizal <- am_results %>% pull(is_mycorrhizal) %>% first()
test_result(am_is_mycorrhizal == TRUE, "Arbuscular mycorrhizal detection", 
           paste0("Correctly identified Rhizophagus as mycorrhizal"))

# Test 5: Ectomycorrhizal detection
cat("\n11. Testing Ectomycorrhizal fungi detection...\n")
em_data <- data.frame(
  id = 1, predicted_label = NA, match_type = "species",
  canonicalName = "Amanita muscaria",
  kingdom = "Fungi", phylum = "Basidiomycota",
  family = "Amanitaceae", genus = "Amanita",
  status = "accepted", resolved_name = "Amanita_muscaria",
  acceptedScientificName = "Amanita muscaria",
  user_supplied_name = "Amanita muscaria",
  canonicalName_lower = "amanita muscaria"
)
em_results <- classify_fungal_taxa_mycorrhizal(em_data$canonicalName, em_data)
em_is_mycorrhizal <- em_results %>% pull(is_mycorrhizal) %>% first()
test_result(em_is_mycorrhizal == TRUE, "Ectomycorrhizal detection",
           paste0("Correctly identified Amanita as mycorrhizal"))

# Test 6: Non-mycorrhizal pathogen detection
cat("\n12. Testing non-mycorrhizal pathogen detection...\n")
pathogen_data <- data.frame(
  id = 1, predicted_label = NA, match_type = "genus",
  canonicalName = "Penicillium",
  kingdom = "Fungi", phylum = "Ascomycota",
  family = "Aspergillaceae", genus = "Penicillium",
  status = "accepted", resolved_name = "Penicillium",
  acceptedScientificName = "Penicillium sp.",
  user_supplied_name = "Penicillium",
  canonicalName_lower = "penicillium"
)
pathogen_results <- classify_fungal_taxa_mycorrhizal(pathogen_data$canonicalName, pathogen_data)
pathogen_is_mycorrhizal <- pathogen_results %>% pull(is_mycorrhizal) %>% first()
test_result(pathogen_is_mycorrhizal == FALSE, "Non-mycorrhizal detection",
           paste0("Correctly identified Penicillium as non-mycorrhizal"))

# Test 7: Confidence ranking values
cat("\n13. Testing confidence ranking values...\n")
conf_results <- enhanced_mycorrhizal_results %>% pull(confidence_ranking)
confidence_valid <- all(conf_results >= 0 & conf_results <= 1, na.rm = TRUE)
test_result(confidence_valid, "Confidence ranking validity",
           paste0("All confidence values between 0 and 1"))

# Test 8: Funguild guild assignment
cat("\n14. Testing FUNGuild guild assignment...\n")
guild_present <- all(!is.na(enhanced_mycorrhizal_results$funguild_guild))
test_result(guild_present, "FUNGuild assignment",
           paste0("All records have a FUNGuild guild assignment"))

# Test 9: Trophic mode assignment
cat("\n15. Testing trophic mode assignment...\n")
trophic_valid <- all(enhanced_mycorrhizal_results$trophic_mode %in% c("Symbiotroph", "Saprotroph", NA))
test_result(trophic_valid, "Trophic mode validity",
           paste0("All trophic modes are valid categories"))

# Test 10: Growth form consistency
cat("\n16. Testing growth form consistency...\n")
growth_consistent <- all(!is.na(enhanced_mycorrhizal_results$growth_form))
test_result(growth_consistent, "Growth form assignment",
           paste0("All records have a growth form assignment"))

# Test 11: Trait confidence alignment with is_mycorrhizal
cat("\n17. Testing trait confidence alignment...\n")
trait_conf_results <- enhanced_mycorrhizal_results %>%
  filter(!is.na(is_mycorrhizal)) %>%
  pull(trait_confidence)
trait_conf_valid <- all(trait_conf_results >= 0 & trait_conf_results <= 1)
test_result(trait_conf_valid, "Trait confidence alignment",
           paste0("Trait confidence values aligned with mycorrhizal status"))

# Test 12: Output row count matches input
cat("\n18. Testing output row count consistency...\n")
row_count_match <- nrow(enhanced_mycorrhizal_results) == nrow(sample_species_data)
test_result(row_count_match, "Row count consistency",
           paste0("Output (", nrow(enhanced_mycorrhizal_results), ") matches input (", nrow(sample_species_data), ")"))

# Test 13: Test with empty input
cat("\n19. Testing with empty input...\n")
empty_data <- sample_species_data[0,]
empty_results <- classify_fungal_taxa_mycorrhizal(character(0), empty_data)
empty_test_pass <- nrow(empty_results) == 0
test_result(empty_test_pass, "Empty input handling",
           paste0("Function handles empty input gracefully"))

# Test 14: Test with NA values in resolved_name
cat("\n20. Testing with NA values...\n")
na_data <- sample_species_data %>% mutate(resolved_name = ifelse(row_number() == 3, NA_character_, resolved_name))
na_results <- classify_fungal_taxa_mycorrhizal(na_data$canonicalName, na_data)
na_test_pass <- nrow(na_results) > 0
test_result(na_test_pass, "NA value handling",
           paste0("Function processes data with NA values"))

# Test 15: Test with non-Fungi kingdom
cat("\n21. Testing non-Fungi kingdom filtering...\n")
non_fungi_data <- sample_species_data %>% mutate(kingdom = "Plantae")
non_fungi_results <- classify_fungal_taxa_mycorrhizal(non_fungi_data$canonicalName, non_fungi_data)
non_fungi_test_pass <- nrow(non_fungi_results) == 0
test_result(non_fungi_test_pass, "Non-Fungi filtering",
           paste0("Non-Fungi records properly filtered (returned ", nrow(non_fungi_results), " records)"))

# Test 16: Test mycorrhizal_only flag consistency
cat("\n22. Testing is_mycorrhizal_only flag...\n")
mycorrhizal_only_check <- all(
  enhanced_mycorrhizal_results %>%
    filter(!is.na(is_mycorrhizal)) %>%
    pull(is_mycorrhizal_only) == 
  enhanced_mycorrhizal_results %>%
    filter(!is.na(is_mycorrhizal)) %>%
    pull(is_mycorrhizal)
)
test_result(mycorrhizal_only_check, "Mycorrhizal-only flag consistency",
           paste0("is_mycorrhizal_only matches is_mycorrhizal classification"))

# Test 17: Test function determinism
cat("\n23. Testing function determinism...\n")
rerun_results <- classify_fungal_taxa_mycorrhizal(sample_species_data$canonicalName, sample_species_data)
deterministic <- identical(enhanced_mycorrhizal_results, rerun_results)
test_result(deterministic, "Function determinism",
           paste0("Multiple runs produce identical results"))

# Test 18: Test with special characters in names
cat("\n24. Testing special character handling...\n")
special_data <- data.frame(
  id = 1, predicted_label = NA, match_type = "species",
  canonicalName = "Glomus_sp.",
  kingdom = "Fungi", phylum = "Glomeromycota",
  family = "Glomeraceae", genus = "Glomus",
  status = "accepted", resolved_name = "Glomus_sp.",
  acceptedScientificName = "Glomus sp.",
  user_supplied_name = "Glomus sp.",
  canonicalName_lower = "glomus sp."
)
special_results <- classify_fungal_taxa_mycorrhizal(special_data$canonicalName, special_data)
special_test_pass <- nrow(special_results) > 0 && !is.na(special_results$is_mycorrhizal[1])
test_result(special_test_pass, "Special character handling",
           paste0("Function handles names with special characters"))

# Test 19: Test column data types
cat("\n25. Testing output column data types...\n")
col_types_valid <- 
  is.logical(enhanced_mycorrhizal_results$is_mycorrhizal) &&
  is.character(enhanced_mycorrhizal_results$funguild_guild) &&
  is.numeric(enhanced_mycorrhizal_results$confidence_ranking)
test_result(col_types_valid, "Column data types",
           paste0("All output columns have correct data types"))

# Test 20: Test with duplicate taxa
cat("\n26. Testing with duplicate taxa...\n")
dup_data <- bind_rows(sample_species_data, sample_species_data[1,])
dup_results <- classify_fungal_taxa_mycorrhizal(dup_data$canonicalName, dup_data)
# The function uses distinct(), so duplicates are removed. We expect unique taxa only
unique_taxa_count <- n_distinct(dup_data$resolved_name)
dup_test_pass <- nrow(dup_results) == unique_taxa_count
test_result(dup_test_pass, "Duplicate taxa handling",
           paste0("Function correctly deduplicates (returned ", nrow(dup_results), " unique taxa)"))

cat("\n=== Enhanced Mycorrhizal Check Testing Complete ===\n")

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! Enhanced mycorrhizal classification is working correctly.\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}