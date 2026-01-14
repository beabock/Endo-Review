# =============================================================================
# test_plant_parts_deduplication.R - Test script to verify plant parts deduplication
# =============================================================================
#
# Purpose: Test that plant parts are only counted once per abstract
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

# Source the plant parts extraction function
source("scripts/04_analysis/components/03_extract_plant_parts.R")

cat("🧪 TESTING PLANT PARTS DEDUPLICATION\n")
cat("===================================\n\n")

# Create test cases with duplicate mentions
test_cases <- tibble(
  id = 1:6,
  abstract = c(
    # Test case 1: Same plant part mentioned multiple times in different forms
    "We studied root systems in detail. The roots were examined for endophytic fungi. Root tissues showed interesting patterns. We also analyzed the root tips and root hairs.",
    
    # Test case 2: Singular and plural forms of the same parts
    "This study focused on leaves and stems. We examined individual leaf structures and stem anatomy. The leaf tissues and stem tissues were analyzed separately.",
    
    # Test case 3: Compound terms that include basic parts
    "We analyzed root hair development and root tip growth. The root system included both fibrous roots and tap roots. Root tissues showed mycorrhizal colonization.",
    
    # Test case 4: Multiple mentions with synonyms and compounds
    "Foliar analysis was conducted on leaf blades and leaf surfaces. We examined leaves from different positions. The leaf margin and leaf tip morphology were documented.",
    
    # Test case 5: Mixed plant parts with repetition
    "Seeds, fruits, and flowers were collected. We analyzed seed coat structure, fruit tissues, and flower buds. The seeds showed variation in size while fruits varied in color.",
    
    # Test case 6: Technical terms that should normalize
    "Vascular bundles in stems were examined. We studied vascular tissue organization and vascular bundle arrangement. The stem vascular system showed clear patterns."
  )
)

cat("📝 Test cases:\n")
for (i in 1:nrow(test_cases)) {
  cat("   ", i, ": ", substr(test_cases$abstract[i], 1, 80), "...\n")
}

cat("\n🔬 Running plant parts detection...\n")

# Test the plant parts detection function directly
results <- detect_plant_parts_batch(test_cases$abstract)

# Add IDs for clarity
results_with_ids <- bind_cols(
  select(test_cases, id),
  results
)

cat("\n📊 RESULTS:\n")
cat("=========\n")

for (i in 1:nrow(results_with_ids)) {
  cat("\n🔍 Test case", i, ":\n")
  cat("   Abstract: ", substr(test_cases$abstract[i], 1, 100), "...\n")
  
  if (!is.na(results_with_ids$plant_parts_detected[i])) {
    parts <- str_split(results_with_ids$plant_parts_detected[i], "; ")[[1]]
    cat("   Plant parts detected: ", paste(parts, collapse = ", "), " (", length(parts), " unique)\n")
    cat("   Parts count: ", results_with_ids$parts_count[i], "\n")
  } else {
    cat("   Plant parts detected: None\n")
    cat("   Parts count: 0\n")
  }
  
  if (!is.na(results_with_ids$parts_normalized[i])) {
    normalized <- str_split(results_with_ids$parts_normalized[i], "; ")[[1]]
    cat("   Normalized parts: ", paste(normalized, collapse = ", "), " (", length(normalized), " unique)\n")
  } else {
    cat("   Normalized parts: None\n")
  }
}

cat("\n✅ VERIFICATION:\n")
cat("===============\n")

# Check for duplicates in results
duplicate_issues <- FALSE

for (i in 1:nrow(results_with_ids)) {
  # Check plant_parts_detected
  if (!is.na(results_with_ids$plant_parts_detected[i])) {
    parts <- str_split(results_with_ids$plant_parts_detected[i], "; ")[[1]]
    if (length(parts) != length(unique(parts))) {
      cat("❌ Test case", i, ": Duplicate plant parts found in plant_parts_detected:", paste(parts, collapse = ", "), "\n")
      cat("   Duplicates: ", paste(parts[duplicated(parts)], collapse = ", "), "\n")
      duplicate_issues <- TRUE
    }
  }
  
  # Check parts_normalized
  if (!is.na(results_with_ids$parts_normalized[i])) {
    normalized <- str_split(results_with_ids$parts_normalized[i], "; ")[[1]]
    if (length(normalized) != length(unique(normalized))) {
      cat("❌ Test case", i, ": Duplicate normalized parts found:", paste(normalized, collapse = ", "), "\n")
      cat("   Duplicates: ", paste(normalized[duplicated(normalized)], collapse = ", "), "\n")
      duplicate_issues <- TRUE
    }
  }
  
  # Check if parts_count matches actual count
  if (!is.na(results_with_ids$plant_parts_detected[i])) {
    parts <- str_split(results_with_ids$plant_parts_detected[i], "; ")[[1]]
    actual_count <- length(parts)
    reported_count <- results_with_ids$parts_count[i]
    
    if (actual_count != reported_count) {
      cat("❌ Test case", i, ": Parts count mismatch. Actual:", actual_count, "Reported:", reported_count, "\n")
      duplicate_issues <- TRUE
    }
  }
}

if (!duplicate_issues) {
  test_result(TRUE, "Plant parts deduplication", "No duplicates found in all test cases")
  test_result(TRUE, "Parts count accuracy", "Counts match actual unique parts detected")
  cat("✅ All test cases passed! No duplicates found.\n")
  cat("✅ Each plant part is counted only once per abstract.\n")
  cat("✅ Parts counts match actual unique parts detected.\n")
} else {
  test_result(FALSE, "Plant parts deduplication", "Duplicates or count mismatches detected")
  cat("❌ Some test cases failed. Duplicates or count mismatches detected.\n")
}

cat("\n4. Accuracy Testing with Expected Plant Parts:\n")
cat("=============================================\n")

# Define expected plant parts for each test case (normalized forms)
expected_parts <- list(
  # Test case 1: roots, root tips, root hairs -> root
  c("root"),
  
  # Test case 2: leaves, stems -> leaf, stem
  c("leaf", "stem"),
  
  # Test case 3: root hair, root tip, root system, fibrous roots, tap roots -> root
  c("root"),
  
  # Test case 4: foliar, leaf blades, leaf surfaces, leaves, leaf margin, leaf tip -> leaf
  c("leaf"),
  
  # Test case 5: seeds, fruits, flowers, seed coat, flower buds -> seed, fruit, flower
  c("seed", "fruit", "flower"),
  
  # Test case 6: vascular bundles, stems, vascular tissue, vascular bundle, stem vascular -> stem
  c("stem")
)

cat("Testing accuracy against expected plant parts...\n\n")

accuracy_results <- data.frame(
  test_case = 1:6,
  expected_parts = I(expected_parts),
  detected_parts = I(vector("list", 6)),
  correct_detections = 0,
  false_positives = 0,
  false_negatives = 0,
  precision = 0,
  recall = 0
)

for (i in 1:nrow(results_with_ids)) {
  # Get detected normalized parts for this test case
  detected_parts <- c()
  if (!is.na(results_with_ids$parts_normalized[i])) {
    detected_parts <- str_split(results_with_ids$parts_normalized[i], "; ")[[1]]
  }
  
  expected <- expected_parts[[i]]
  
  # Calculate accuracy metrics
  correct <- sum(detected_parts %in% expected)
  false_positives <- sum(!(detected_parts %in% expected))
  false_negatives <- sum(!(expected %in% detected_parts))
  
  precision <- if (length(detected_parts) > 0) correct / length(detected_parts) else 0
  recall <- if (length(expected) > 0) correct / length(expected) else 0
  
  accuracy_results$detected_parts[[i]] <- detected_parts
  accuracy_results$correct_detections[i] <- correct
  accuracy_results$false_positives[i] <- false_positives
  accuracy_results$false_negatives[i] <- false_negatives
  accuracy_results$precision[i] <- precision
  accuracy_results$recall[i] <- recall
  
  cat("Test case", i, ":\n")
  cat("  Expected: ", paste(expected, collapse = ", "), "\n")
  cat("  Detected: ", paste(detected_parts, collapse = ", "), "\n")
  cat("  Precision: ", round(precision * 100, 1), "%, Recall: ", round(recall * 100, 1), "%\n\n")
}

# Overall accuracy metrics
total_correct <- sum(accuracy_results$correct_detections)
total_detected <- sum(sapply(accuracy_results$detected_parts, length))
total_expected <- sum(sapply(accuracy_results$expected_parts, length))

overall_precision <- total_correct / total_detected
overall_recall <- total_correct / total_expected
overall_f1 <- 2 * (overall_precision * overall_recall) / (overall_precision + overall_recall)

cat("Overall Accuracy Metrics:\n")
cat("  Total expected parts: ", total_expected, "\n")
cat("  Total detected parts: ", total_detected, "\n")
cat("  Correct detections: ", total_correct, "\n")
cat("  Precision: ", round(overall_precision * 100, 1), "%\n")
cat("  Recall: ", round(overall_recall * 100, 1), "%\n")
cat("  F1 Score: ", round(overall_f1 * 100, 1), "%\n\n")

# Test results
test_result(overall_precision >= 0.8, "Plant parts detection precision", 
            paste0("Precision: ", round(overall_precision * 100, 1), "%"))
test_result(overall_recall >= 0.8, "Plant parts detection recall", 
            paste0("Recall: ", round(overall_recall * 100, 1), "%"))
test_result(overall_f1 >= 0.8, "Plant parts detection F1 score", 
            paste0("F1: ", round(overall_f1 * 100, 1), "%"))
cat("======================\n")

# Check for proper normalization (e.g., roots/root should be the same)
for (i in 1:nrow(results_with_ids)) {
  if (!is.na(results_with_ids$plant_parts_detected[i])) {
    parts <- str_split(results_with_ids$plant_parts_detected[i], "; ")[[1]]
    
    # Look for potential normalization issues (plurals that should be singular)
    potential_plurals <- parts[str_ends(parts, "s")]
    if (length(potential_plurals) > 0) {
      cat("   Test case", i, "- Potential plural forms detected: ", paste(potential_plurals, collapse = ", "), "\n")
    }
  }
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