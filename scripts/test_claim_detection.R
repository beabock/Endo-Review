# Test Script for Citation Claim Detection Patterns
# Purpose: Test the universality claim detection function on sample texts

# Source the main detection function
source("citation_analysis_phase1.R")

# Test cases for pattern validation
test_claim_detection <- function() {
  cat("=== TESTING UNIVERSALITY CLAIM DETECTION PATTERNS ===\n\n")
  
  # Test cases with expected results
  test_cases <- list(
    # Should be detected - Strong claims
    list(
      text = "All plants harbor endophytic fungi that contribute to plant health.",
      expected = TRUE,
      category = "Strong - explicit endophyte"
    ),
    list(
      text = "Every plant species contains fungi that live within plant tissues.",
      expected = TRUE,
      category = "Strong - general fungi"
    ),
    list(
      text = "Fungi are universally present in plants and play important ecological roles.",
      expected = TRUE,
      category = "Strong - universal fungi"
    ),
    list(
      text = "Endophytes are ubiquitous in plants across all ecosystems.",
      expected = TRUE,
      category = "Strong - ubiquitous endophytes"
    ),
    
    # Should be detected - Moderate claims
    list(
      text = "Most plants harbor endophytic fungi in their tissues.",
      expected = TRUE,
      category = "Moderate - most plants"
    ),
    list(
      text = "Virtually all plant species contain some form of endophytic microorganisms.",
      expected = TRUE,
      category = "Moderate - virtually all"
    ),
    
    # Should be detected - Weak claims
    list(
      text = "Endophytes are common and widespread in plant communities.",
      expected = TRUE,
      category = "Weak - common/widespread"
    ),
    list(
      text = "Fungi are found in many different plant species worldwide.",
      expected = TRUE,
      category = "Weak - many plants"
    ),
    
    # Should NOT be detected - False positives
    list(
      text = "All samples in this study contained endophytic fungi.",
      expected = FALSE,
      category = "Study-specific (should not detect)"
    ),
    list(
      text = "Some plants harbor endophytic fungi while others do not.",
      expected = FALSE,
      category = "Qualified statement (should not detect)"
    ),
    list(
      text = "All tropical rainforest plants contain endophytes.",
      expected = FALSE,
      category = "Geographically limited (should not detect)"
    ),
    list(
      text = "Endophytes were isolated from plant tissues using standard methods.",
      expected = FALSE,
      category = "Methodological (should not detect)"
    )
  )
  
  # Run tests
  results <- data.frame(
    test_case = character(),
    expected = logical(),
    detected = logical(),
    confidence = numeric(),
    matches = character(),
    category = character(),
    correct = logical(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(test_cases)) {
    test <- test_cases[[i]]
    detection_result <- detect_universality_claims(test$text)
    
    correct <- (detection_result$detected == test$expected)
    
    results <- rbind(results, data.frame(
      test_case = i,
      expected = test$expected,
      detected = detection_result$detected,
      confidence = detection_result$confidence,
      matches = paste(detection_result$matches, collapse = " | "),
      category = test$category,
      correct = correct,
      stringsAsFactors = FALSE
    ))
    
    # Print individual results
    status <- ifelse(correct, "✓ PASS", "✗ FAIL")
    cat(sprintf("%s Test %d (%s)\n", status, i, test$category))
    cat(sprintf("   Text: \"%s\"\n", substr(test$text, 1, 60)))
    if (nchar(test$text) > 60) cat("...\n")
    cat(sprintf("   Expected: %s, Detected: %s, Confidence: %.2f\n", 
                test$expected, detection_result$detected, detection_result$confidence))
    if (detection_result$detected) {
      cat(sprintf("   Matches: %s\n", paste(detection_result$matches, collapse = " | ")))
    }
    cat("\n")
  }
  
  # Summary
  cat("=== TEST SUMMARY ===\n")
  accuracy <- sum(results$correct) / nrow(results) * 100
  cat(sprintf("Overall Accuracy: %.1f%% (%d/%d correct)\n", 
              accuracy, sum(results$correct), nrow(results)))
  
  # Breakdown by expected result
  true_positives <- sum(results$expected == TRUE & results$detected == TRUE)
  false_negatives <- sum(results$expected == TRUE & results$detected == FALSE)
  true_negatives <- sum(results$expected == FALSE & results$detected == FALSE)
  false_positives <- sum(results$expected == FALSE & results$detected == TRUE)
  
  cat("\nConfusion Matrix:\n")
  cat(sprintf("True Positives:  %d (correctly detected claims)\n", true_positives))
  cat(sprintf("False Negatives: %d (missed claims)\n", false_negatives))
  cat(sprintf("True Negatives:  %d (correctly ignored non-claims)\n", true_negatives))
  cat(sprintf("False Positives: %d (incorrectly detected claims)\n", false_positives))
  
  if (true_positives + false_negatives > 0) {
    sensitivity <- true_positives / (true_positives + false_negatives) * 100
    cat(sprintf("Sensitivity (recall): %.1f%%\n", sensitivity))
  }
  
  if (true_negatives + false_positives > 0) {
    specificity <- true_negatives / (true_negatives + false_positives) * 100
    cat(sprintf("Specificity: %.1f%%\n", specificity))
  }
  
  return(results)
}

# Function to test on a small sample of real abstracts
test_on_sample_abstracts <- function(abstracts_file = "data/All_abstracts.csv", n_sample = 20) {
  cat("=== TESTING ON SAMPLE ABSTRACTS ===\n\n")
  
  if (!file.exists(abstracts_file)) {
    cat("Abstracts file not found. Skipping sample test.\n")
    return(NULL)
  }
  
  # Read and sample abstracts
  library(readr)
  library(dplyr)
  
  abstracts <- read_csv(abstracts_file, show_col_types = FALSE)
  
  # Take random sample
  set.seed(123)
  sample_abstracts <- abstracts %>%
    slice_sample(n = min(n_sample, nrow(abstracts))) %>%
    mutate(
      combined_text = paste(
        ifelse(is.na(Title), "", Title),
        ifelse(is.na(Abstract), "", Abstract),
        sep = " "
      )
    )
  
  cat(sprintf("Testing detection on %d random abstracts...\n\n", nrow(sample_abstracts)))
  
  # Apply detection
  detected_claims <- 0
  for (i in 1:nrow(sample_abstracts)) {
    result <- detect_universality_claims(sample_abstracts$combined_text[i])
    
    if (result$detected) {
      detected_claims <- detected_claims + 1
      cat(sprintf("CLAIM DETECTED in Abstract %d (Confidence: %.2f):\n", i, result$confidence))
      cat(sprintf("Title: %s\n", substr(sample_abstracts$Title[i], 1, 80)))
      if (nchar(sample_abstracts$Title[i]) > 80) cat("...\n")
      cat(sprintf("Matches: %s\n\n", paste(result$matches, collapse = " | ")))
    }
  }
  
  cat(sprintf("Summary: %d claims detected in %d abstracts (%.1f%%)\n", 
              detected_claims, nrow(sample_abstracts), 
              detected_claims/nrow(sample_abstracts)*100))
}

# Main test execution
if (!interactive()) {
  # Run pattern tests
  test_results <- test_claim_detection()
  
  # Test on sample abstracts
  test_on_sample_abstracts()
}
