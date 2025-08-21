# Manual Validation Sample Generator
# B. Bock
# July 31, 2025
#
# Creates a stratified sample of ML predictions for manual validation
# to assess accuracy and identify potential biases in the classification model

library(tidyverse)
library(janitor)

cat("=== MANUAL VALIDATION SAMPLE GENERATOR ===\n")
cat("Creating stratified sample for accuracy assessment\n\n")

# Load comprehensive extraction results
cat("Loading comprehensive extraction results...\n")
if (!file.exists("results/comprehensive_extraction_results.csv")) {
  stop("Please run extract_species_simple.R first to generate comprehensive results.")
}

comprehensive_data <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)

cat("Total abstracts available:", nrow(comprehensive_data), "\n")

# Define sampling strategy
# We want to validate across different dimensions:
# 1. Prediction confidence levels (high vs medium vs low)
# 2. Prediction types (Presence vs Absence)
# 3. Species detection (with vs without species)
# 4. Information completeness (high vs low info)

# Calculate sampling strata (ensuring unique abstracts)
validation_data <- comprehensive_data %>%
  # First group by id to get one row per abstract
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  # Add confidence categories
  mutate(
    confidence_level = case_when(
      confidence >= 0.9 ~ "High (≥0.9)",
      confidence >= 0.7 ~ "Medium (0.7-0.9)",
      confidence >= 0.5 ~ "Low (0.5-0.7)",
      TRUE ~ "Very Low (<0.5)"
    ),
    # Information completeness score
    info_score = (
      ifelse(!is.na(canonicalName), 1, 0) +
      ifelse(!is.na(methods_summary), 1, 0) +
      ifelse(!is.na(plant_parts_detected), 1, 0) +
      ifelse(!is.na(geographic_summary), 1, 0)
    ),
    info_completeness = case_when(
      info_score >= 3 ~ "High (3-4 types)",
      info_score >= 2 ~ "Medium (2 types)",
      info_score >= 1 ~ "Low (1 type)",
      TRUE ~ "None (0 types)"
    ),
    # Species detection status
    has_species = ifelse(!is.na(canonicalName), "Species detected", "No species"),
    # Create unique combination for stratification
    stratum = paste(predicted_label, confidence_level, has_species, sep = " | ")
  )

# Show stratum breakdown
cat("\nStratum breakdown for sampling:\n")
stratum_summary <- validation_data %>%
  count(stratum, name = "available") %>%
  arrange(desc(available))

print(stratum_summary)

# Define sample sizes per stratum
# Target: ~200 total abstracts for manual validation
# Priority: High confidence + species detected abstracts
# Balance: Ensure representation across all categories

set.seed(12345)  # For reproducible sampling

sample_strategy <- stratum_summary %>%
  mutate(
    # Determine sample size based on stratum importance and availability
    target_n = case_when(
      # High confidence with species (most important)
      str_detect(stratum, "High.*Species detected") ~ pmin(available, 25),
      # Medium confidence with species
      str_detect(stratum, "Medium.*Species detected") ~ pmin(available, 15),
      # High confidence without species (potential false negatives)
      str_detect(stratum, "High.*No species") ~ pmin(available, 10),
      # Low confidence with species (potential false positives)
      str_detect(stratum, "Low.*Species detected") ~ pmin(available, 8),
      # All others
      TRUE ~ pmin(available, 5)
    ),
    # Ensure we sample at least 1 from each stratum if available
    target_n = pmax(1, target_n),
    # Don't exceed available
    target_n = pmin(target_n, available)
  )

cat("\nSampling strategy:\n")
print(sample_strategy %>% select(stratum, available, target_n))
cat("Total target sample size:", sum(sample_strategy$target_n), "\n")

# Generate stratified sample
validation_sample <- map_dfr(1:nrow(sample_strategy), function(i) {
  stratum_name <- sample_strategy$stratum[i]
  n_sample <- sample_strategy$target_n[i]
  
  # Get abstracts from this stratum
  stratum_data <- validation_data %>%
    filter(stratum == stratum_name)
  
  # Sample randomly within stratum
  if (nrow(stratum_data) > 0 && n_sample > 0) {
    sampled <- stratum_data %>%
      slice_sample(n = min(n_sample, nrow(stratum_data)))
    return(sampled)
  } else {
    return(tibble())
  }
})

cat("\nValidation sample generated:", nrow(validation_sample), "abstracts\n")

# Add validation fields for manual review
validation_sample <- validation_sample %>%
  arrange(desc(confidence), predicted_label, has_species) %>%
  mutate(
    # Add fields for manual validation
    validator_name = "",
    validation_date = "",
    manual_label = "",  # Presence/Absence/Irrelevant
    manual_confidence = "",  # High/Medium/Low
    species_accuracy = "",  # Correct/Incorrect/Partial/NA
    notes = "",
    validation_id = paste0("VAL_", str_pad(row_number(), 3, pad = "0"))
  ) 

# Save validation sample
write_csv(validation_sample, "results/validation_sample_for_manual_review.csv")

# Create validation instructions
validation_instructions <- "
# MANUAL VALIDATION INSTRUCTIONS

## Overview
This file contains a stratified sample of {nrow(validation_sample)} abstracts for manual validation of the machine learning classification model. The sample is designed to assess model accuracy across different confidence levels and information types.

## Validation Process

### Step 1: Basic Classification
For each abstract, determine:
- **manual_label**: Is this abstract about endophytic fungi?
  - 'Presence': Clear evidence of endophytic fungi research
  - 'Absence': Not about endophytic fungi (different topic)
  - 'Irrelevant': Unclear, ambiguous, or insufficient information

### Step 2: Confidence Assessment
- **manual_confidence**: How confident are you in your classification?
  - 'High': Very clear, unambiguous
  - 'Medium': Reasonably clear with minor uncertainty
  - 'Low': Difficult to determine, ambiguous

### Step 3: Species Accuracy (if applicable)
- **species_accuracy**: If species were detected by the algorithm, are they correct?
  - 'Correct': Species mentioned are accurate and relevant
  - 'Incorrect': Species are wrong or not mentioned in text
  - 'Partial': Some species correct, some incorrect
  - 'NA': No species detected or not applicable

### Step 4: Notes
- **notes**: Any additional observations, issues, or context

## Validation Fields to Complete
1. **validator_name**: Your name/initials
2. **validation_date**: Date of validation (YYYY-MM-DD)
3. **manual_label**: Presence/Absence/Irrelevant
4. **manual_confidence**: High/Medium/Low
5. **species_accuracy**: Correct/Incorrect/Partial/NA
6. **notes**: Free text observations

## Sample Composition
{paste(capture.output(print(sample_strategy %>% select(stratum, target_n))), collapse='\n')}

## Quality Control
- Validate abstracts in random order to avoid bias
- Take breaks to maintain focus and consistency
- If uncertain, mark as 'Low' confidence and add detailed notes
- Consider having a second validator review uncertain cases

## Analysis Plan
After validation, we will:
1. Calculate overall model accuracy by confidence level
2. Identify systematic biases or error patterns
3. Assess species detection accuracy
4. Recommend model improvements or threshold adjustments

## Contact
For questions about specific abstracts or validation criteria, contact the research team.

Generated: {Sys.time()}
"

writeLines(
  str_glue(validation_instructions),
  "results/VALIDATION_INSTRUCTIONS.md"
)

# Create summary report
cat("\n=== VALIDATION SAMPLE SUMMARY ===\n")
cat("Sample composition by prediction:\n")
prediction_breakdown <- validation_sample %>%
  count(predicted_label, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(prediction_breakdown)

cat("\nSample composition by confidence:\n")
confidence_breakdown <- validation_sample %>%
  count(confidence_level, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(confidence_breakdown)

cat("\nSample composition by species detection:\n")
species_breakdown <- validation_sample %>%
  count(has_species, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(species_breakdown)

cat("\nInformation completeness in sample:\n")
info_breakdown <- validation_sample %>%
  count(info_completeness, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(info_breakdown)

# Create validation tracking template
validation_progress <- tibble(
  validation_status = c("Not Started", "In Progress", "Completed", "Reviewed"),
  count = c(nrow(validation_sample), 0, 0, 0),
  percentage = c(100, 0, 0, 0)
)

write_csv(validation_progress, "results/validation_progress_tracker.csv")

cat("\nFiles created:\n")
cat("✓ results/validation_sample_for_manual_review.csv (", nrow(validation_sample), " abstracts)\n")
cat("✓ results/VALIDATION_INSTRUCTIONS.md (detailed instructions)\n")
cat("✓ results/validation_progress_tracker.csv (progress tracking)\n")

cat("\n=== NEXT STEPS ===\n")
cat("1. Open validation_sample_for_manual_review.csv in Excel or similar\n")
cat("2. Read VALIDATION_INSTRUCTIONS.md for detailed guidelines\n")
cat("3. Complete validation fields for each abstract\n")
cat("4. Save completed validation file\n")
cat("5. Run validation analysis script (when available)\n")
cat("\nManual validation sample ready! 📋✅\n")
