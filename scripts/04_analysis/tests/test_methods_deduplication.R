# =============================================================================
# test_methods_deduplication.R - Test script to verify methods deduplication
# =============================================================================
#
# Purpose: Test that research methods are only counted once per abstract
#
# =============================================================================

library(tidyverse)
library(stringr)

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
  cat("✅ All test cases passed! No duplicates found.\n")
  cat("✅ Each method category is counted only once per abstract.\n")
  cat("✅ Methods summary is consistent with individual method flags.\n")
} else {
  cat("❌ Some test cases failed. Duplicates or inconsistencies detected.\n")
}

cat("\n📋 Method Categories Analysis:\n")
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

cat("\n📋 Test completed.\n")