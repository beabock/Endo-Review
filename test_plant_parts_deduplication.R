# =============================================================================
# test_plant_parts_deduplication.R - Test script to verify plant parts deduplication
# =============================================================================
#
# Purpose: Test that plant parts are only counted once per abstract
#
# =============================================================================

library(tidyverse)
library(stringr)

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
  cat("✅ All test cases passed! No duplicates found.\n")
  cat("✅ Each plant part is counted only once per abstract.\n")
  cat("✅ Parts counts match actual unique parts detected.\n")
} else {
  cat("❌ Some test cases failed. Duplicates or count mismatches detected.\n")
}

cat("\n📋 Additional Analysis:\n")
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

cat("\n📋 Test completed.\n")