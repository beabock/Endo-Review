# =============================================================================
# test_methods_deduplication.R - Test script to verify methods deduplication
# =============================================================================
#
# Purpose: Test that research methods are only counted once per abstract
#
# =============================================================================

library(tidyverse)
library(stringr)

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

# Source the methods extraction function
source("scripts/04_analysis/components/02_extract_methods.R")

cat("🧪 TESTING METHODS DEDUPLICATION\n")
cat("===============================\n\n")

# Create test cases with potential duplicate method detection scenarios
test_cases <- tibble(
  id = 1:6,
  abstract = c(
    # Test case 1: Multiple molecular techniques mentioned
    "We used PCR amplification and DNA sequencing for identification. The ITS regions were amplified by PCR and then sequenced using Sanger sequencing. Phylogenetic analysis was conducted using DNA sequences.",
    
    # Test case 2: Multiple culture-based methods
    "Fungi were isolated on PDA media and cultured in petri dishes. Pure cultures were obtained through serial dilution and plating. The isolates were maintained on agar plates and stored as cultures.",
    
    # Test case 3: Mixed methods with potential overlaps
    "We used light microscopy and electron microscopy for observations. Samples were prepared for microscopy and examined under different microscope settings. Microscopic examination revealed detailed structures.",
    
    # Test case 4: Bioactivity and physiological assays
    "Antimicrobial activity was tested using disk diffusion assays. We also conducted enzyme assays to determine cellulase and protease activity. The bioactivity tests included antifungal and antibacterial assays.",
    
    # Test case 5: Ecological studies with diversity analysis
    "We conducted field surveys to assess endophyte diversity. Species diversity was calculated using Shannon index and Simpson index. The diversity analysis showed high species richness in the community.",
    
    # Test case 6: Comprehensive study with multiple method categories
    "This study employed molecular techniques (PCR, DNA sequencing), culture-based isolation on agar media, light microscopy for morphological analysis, and bioactivity assays including antimicrobial testing. We also conducted field surveys for ecological diversity assessment."
  )
)

cat("📝 Test cases:\n")
for (i in 1:nrow(test_cases)) {
  cat("   ", i, ": ", substr(test_cases$abstract[i], 1, 100), "...\n")
}

cat("\n🔬 Running methods detection...\n")

# Test the methods detection function directly
results <- detect_research_methods_batch(test_cases$abstract)

# Add IDs for clarity
results_with_ids <- bind_cols(
  select(test_cases, id),
  results
)

cat("\n📊 RESULTS:\n")
cat("=========\n")

for (i in 1:nrow(results_with_ids)) {
  cat("\n🔍 Test case", i, ":\n")
  cat("   Abstract: ", substr(test_cases$abstract[i], 1, 120), "...\n")
  
  # Show detected method categories
  detected_methods <- c()
  if (results_with_ids$molecular_methods[i]) detected_methods <- c(detected_methods, "molecular")
  if (results_with_ids$culture_based_methods[i]) detected_methods <- c(detected_methods, "culture_based")
  if (results_with_ids$microscopy_methods[i]) detected_methods <- c(detected_methods, "microscopy")
  if (results_with_ids$inoculation_methods[i]) detected_methods <- c(detected_methods, "inoculation")
  if (results_with_ids$plant_microbe_interaction_methods[i]) detected_methods <- c(detected_methods, "plant_microbe_interaction")
  if (results_with_ids$bioactivity_assays_methods[i]) detected_methods <- c(detected_methods, "bioactivity_assays")
  if (results_with_ids$physiological_assays_methods[i]) detected_methods <- c(detected_methods, "physiological_assays")
  if (results_with_ids$ecological_studies_methods[i]) detected_methods <- c(detected_methods, "ecological_studies")
  if (results_with_ids$surface_sterilization_methods[i]) detected_methods <- c(detected_methods, "surface_sterilization")
  
  cat("   Method categories detected: ", paste(detected_methods, collapse = ", "), " (", length(detected_methods), " categories)\n")
  
  if (!is.na(results_with_ids$methods_summary[i])) {
    summary_methods <- str_split(results_with_ids$methods_summary[i], "; ")[[1]]
    cat("   Methods summary: ", paste(summary_methods, collapse = ", "), " (", length(summary_methods), " unique)\n")
  } else {
    cat("   Methods summary: None\n")
  }
}

cat("\n✅ VERIFICATION:\n")
cat("===============\n")

# Check for duplicates in methods_summary
duplicate_issues <- FALSE

for (i in 1:nrow(results_with_ids)) {
  # Check methods_summary for duplicates
  if (!is.na(results_with_ids$methods_summary[i])) {
    summary_methods <- str_split(results_with_ids$methods_summary[i], "; ")[[1]]
    if (length(summary_methods) != length(unique(summary_methods))) {
      cat("❌ Test case", i, ": Duplicate method categories found in methods_summary:", paste(summary_methods, collapse = ", "), "\n")
      cat("   Duplicates: ", paste(summary_methods[duplicated(summary_methods)], collapse = ", "), "\n")
      duplicate_issues <- TRUE
    }
  }
  
  # Check consistency between individual flags and summary
  detected_methods <- c()
  if (results_with_ids$molecular_methods[i]) detected_methods <- c(detected_methods, "molecular")
  if (results_with_ids$culture_based_methods[i]) detected_methods <- c(detected_methods, "culture_based")
  if (results_with_ids$microscopy_methods[i]) detected_methods <- c(detected_methods, "microscopy")
  if (results_with_ids$inoculation_methods[i]) detected_methods <- c(detected_methods, "inoculation")
  if (results_with_ids$plant_microbe_interaction_methods[i]) detected_methods <- c(detected_methods, "plant_microbe_interaction")
  if (results_with_ids$bioactivity_assays_methods[i]) detected_methods <- c(detected_methods, "bioactivity_assays")
  if (results_with_ids$physiological_assays_methods[i]) detected_methods <- c(detected_methods, "physiological_assays")
  if (results_with_ids$ecological_studies_methods[i]) detected_methods <- c(detected_methods, "ecological_studies")
  if (results_with_ids$surface_sterilization_methods[i]) detected_methods <- c(detected_methods, "surface_sterilization")
  
  if (!is.na(results_with_ids$methods_summary[i])) {
    summary_methods <- str_split(results_with_ids$methods_summary[i], "; ")[[1]]
    
    # Check if summary matches individual flags
    if (!setequal(detected_methods, summary_methods)) {
      cat("❌ Test case", i, ": Inconsistency between flags and summary\n")
      cat("   Flags detected: ", paste(detected_methods, collapse = ", "), "\n")
      cat("   Summary shows: ", paste(summary_methods, collapse = ", "), "\n")
      duplicate_issues <- TRUE
    }
  } else if (length(detected_methods) > 0) {
    cat("❌ Test case", i, ": Methods detected by flags but summary is NA\n")
    duplicate_issues <- TRUE
  }
}

if (!duplicate_issues) {
  test_result(TRUE, "Methods deduplication", "No duplicates found in all test cases")
  test_result(TRUE, "Methods summary consistency", "Summary matches individual flags")
  cat("✅ All test cases passed! No duplicates found.\n")
  cat("✅ Each method category is counted only once per abstract.\n")
  cat("✅ Methods summary is consistent with individual method flags.\n")
} else {
  test_result(FALSE, "Methods deduplication", "Duplicates or inconsistencies detected")
  cat("❌ Some test cases failed. Duplicates or inconsistencies detected.\n")
}

cat("\n4. Accuracy Testing with Expected Method Categories:\n")
cat("===================================================\n")

# Define expected method categories for each test case
expected_methods <- list(
  # Test case 1: PCR, sequencing, phylogenetic analysis -> molecular
  c("molecular"),
  
  # Test case 2: PDA media, petri dishes, serial dilution, plating -> culture_based
  c("culture_based"),
  
  # Test case 3: light microscopy, electron microscopy -> microscopy
  c("microscopy"),
  
  # Test case 4: disk diffusion assays, enzyme assays, bioactivity tests -> bioactivity_assays, physiological_assays
  c("bioactivity_assays", "physiological_assays"),
  
  # Test case 5: field surveys, diversity analysis (Shannon, Simpson) -> ecological_studies
  c("ecological_studies"),
  
  # Test case 6: PCR, sequencing, culture isolation, microscopy, bioactivity assays, field surveys -> multiple categories
  c("molecular", "culture_based", "microscopy", "bioactivity_assays", "ecological_studies")
)

cat("Testing accuracy against expected method categories...\n\n")

accuracy_results <- data.frame(
  test_case = 1:6,
  expected_categories = I(expected_methods),
  detected_categories = I(vector("list", 6)),
  correct_detections = 0,
  false_positives = 0,
  false_negatives = 0,
  precision = 0,
  recall = 0
)

for (i in 1:nrow(results_with_ids)) {
  # Get detected methods for this test case
  detected_methods <- c()
  if (results_with_ids$molecular_methods[i]) detected_methods <- c(detected_methods, "molecular")
  if (results_with_ids$culture_based_methods[i]) detected_methods <- c(detected_methods, "culture_based")
  if (results_with_ids$microscopy_methods[i]) detected_methods <- c(detected_methods, "microscopy")
  if (results_with_ids$inoculation_methods[i]) detected_methods <- c(detected_methods, "inoculation")
  if (results_with_ids$plant_microbe_interaction_methods[i]) detected_methods <- c(detected_methods, "plant_microbe_interaction")
  if (results_with_ids$bioactivity_assays_methods[i]) detected_methods <- c(detected_methods, "bioactivity_assays")
  if (results_with_ids$physiological_assays_methods[i]) detected_methods <- c(detected_methods, "physiological_assays")
  if (results_with_ids$ecological_studies_methods[i]) detected_methods <- c(detected_methods, "ecological_studies")
  if (results_with_ids$surface_sterilization_methods[i]) detected_methods <- c(detected_methods, "surface_sterilization")
  
  expected <- expected_methods[[i]]
  
  # Calculate accuracy metrics
  correct <- sum(detected_methods %in% expected)
  false_positives <- sum(!(detected_methods %in% expected))
  false_negatives <- sum(!(expected %in% detected_methods))
  
  precision <- if (length(detected_methods) > 0) correct / length(detected_methods) else 0
  recall <- if (length(expected) > 0) correct / length(expected) else 0
  
  accuracy_results$detected_categories[[i]] <- detected_methods
  accuracy_results$correct_detections[i] <- correct
  accuracy_results$false_positives[i] <- false_positives
  accuracy_results$false_negatives[i] <- false_negatives
  accuracy_results$precision[i] <- precision
  accuracy_results$recall[i] <- recall
  
  cat("Test case", i, ":\n")
  cat("  Expected: ", paste(expected, collapse = ", "), "\n")
  cat("  Detected: ", paste(detected_methods, collapse = ", "), "\n")
  cat("  Precision: ", round(precision * 100, 1), "%, Recall: ", round(recall * 100, 1), "%\n\n")
}

# Overall accuracy metrics
total_correct <- sum(accuracy_results$correct_detections)
total_detected <- sum(sapply(accuracy_results$detected_categories, length))
total_expected <- sum(sapply(accuracy_results$expected_categories, length))

overall_precision <- total_correct / total_detected
overall_recall <- total_correct / total_expected
overall_f1 <- 2 * (overall_precision * overall_recall) / (overall_precision + overall_recall)

cat("Overall Accuracy Metrics:\n")
cat("  Total expected categories: ", total_expected, "\n")
cat("  Total detected categories: ", total_detected, "\n")
cat("  Correct detections: ", total_correct, "\n")
cat("  Precision: ", round(overall_precision * 100, 1), "%\n")
cat("  Recall: ", round(overall_recall * 100, 1), "%\n")
cat("  F1 Score: ", round(overall_f1 * 100, 1), "%\n\n")

# Test results
test_result(overall_precision >= 0.8, "Methods detection precision", 
            paste0("Precision: ", round(overall_precision * 100, 1), "%"))
test_result(overall_recall >= 0.8, "Methods detection recall", 
            paste0("Recall: ", round(overall_recall * 100, 1), "%"))
test_result(overall_f1 >= 0.8, "Methods detection F1 score", 
            paste0("F1: ", round(overall_f1 * 100, 1), "%"))
cat("=============================\n")

# Show which method categories were detected across all test cases
all_categories <- c("molecular", "culture_based", "microscopy", "inoculation", 
                   "plant_microbe_interaction", "bioactivity_assays", 
                   "physiological_assays", "ecological_studies", "surface_sterilization")

category_counts <- sapply(all_categories, function(cat) {
  col_name <- paste0(cat, "_methods")
  if (col_name %in% names(results_with_ids)) {
    sum(results_with_ids[[col_name]], na.rm = TRUE)
  } else {
    0
  }
})

cat("Method category detection counts:\n")
for (i in seq_along(category_counts)) {
  cat("   - ", names(category_counts)[i], ": ", category_counts[i], " abstracts\n")
}

# Final scoring summary
cat("\n" , rep("=", 50), "\n", sep = "")
cat("TEST SUMMARY\n")
cat(rep("=", 50), "\n", sep = "")
cat(sprintf("Total Tests:        %d\n", total_tests))
cat(sprintf("Passed:             %d (%.1f%%)\n", passed_tests, 100 * passed_tests / total_tests))
cat(sprintf("Failed:             %d (%.1f%%)\n", failed_tests, 100 * failed_tests / total_tests))
cat(rep("=", 50), "\n", sep = "")

cat("\n📋 Test completed.\n")