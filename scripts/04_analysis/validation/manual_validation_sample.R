# Separate Absence/Presence Manual Validation Sample Generator
# B. Bock
# July 31, 2025
#
# PURPOSE:
# Creates separate validation samples for absence and presence abstracts to enable
# focused validation of detection methods and classification accuracy.
#
# This script generates:
# 1. ABSENCE SAMPLE: ALL ML absence predictions + high-confidence string method absences + training manual absences (with DOI)
# 2. PRESENCE SAMPLE: Stratified random sample of presence predictions
#
# The resulting samples enable validation of:
# - Absence detection accuracy and method comparison (ML vs string vs training manual)
# - Presence detection accuracy and false positive assessment
# - Confidence calibration for both classes
# - Overall classification accuracy and bias assessment
#
# INPUTS:
# - results/relevant_abstracts_with_pa_predictions.csv: ML predictions with Presence/Absence labels
# - results/all_papers_with_absence_matches.csv: Abstracts flagged by string-based absence detection (filtered to high confidence only)
# - Training_labeled_abs_6.csv: Manually labeled training absences (filtered to those with DOI)
#
# OUTPUTS:
# - results/absence_validation_sample_for_manual_review.csv: Complete absence sample (ML + string + training manual)
# - results/presence_validation_sample_for_manual_review.csv: Stratified presence sample
# - results/ABSENCE_VALIDATION_INSTRUCTIONS.md: Absence validation guidelines
# - results/PRESENCE_VALIDATION_INSTRUCTIONS.md: Presence validation guidelines
# - results/validation_progress_tracker.csv: Combined progress tracking template
#
# DEPENDENCIES:
# - tidyverse (for data manipulation)
# - janitor (for data cleaning)

library(tidyverse)
library(janitor)

cat("=== SEPARATE ABSENCE/PRESENCE MANUAL VALIDATION SAMPLE GENERATOR ===\n")
cat("Creating complete absence sample + stratified presence sample\n\n")

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================
# Load prediction data and absence matches to create separate validation datasets.
# This creates: 1) Complete absence dataset (ML + high-confidence string + training manual), 2) Presence dataset for stratified sampling.
cat("Loading prediction data and absence matches...\n")
if (!file.exists("results/relevant_abstracts_with_pa_predictions.csv")) {
  stop("Please ensure relevant_abstracts_with_pa_predictions.csv exists.")
}
if (!file.exists("results/all_papers_with_absence_matches.csv")) {
  stop("Please ensure all_papers_with_absence_matches.csv exists.")
}

# Load full ML predictions and split by class
full_predictions <- read_csv("results/relevant_abstracts_with_pa_predictions.csv", show_col_types = FALSE)

absence_predictions <- full_predictions %>%
  filter(final_classification == "Absence") %>%
  mutate(absence_source = "ML")

presence_predictions <- full_predictions %>%
  filter(final_classification == "Presence")

# Load string method absence matches and filter for high confidence only
string_absence_raw <- read_csv("results/all_papers_with_absence_matches.csv", show_col_types = FALSE)

# Calculate confidence for string method absences
string_absence_data <- string_absence_raw %>%
  # Filter for high confidence string method absences only
  filter(confidence_level %in% c("High", "Very High")) %>%
  mutate(absence_source = "String")

# Load manually labeled training absences (only those with DOIs)
training_absence_raw <- read_csv("Training_labeled_abs_6.csv", show_col_types = FALSE)

training_absence_data <- training_absence_raw %>%
  # Filter for rows with DOIs (real examples only)
  filter(!is.na(doi) & doi != "") %>%
  mutate(absence_source = "Training_Manual")

cat("ML Absence predictions:", nrow(absence_predictions), "\n")
cat("ML Presence predictions:", nrow(presence_predictions), "\n")
cat("String method absence matches (total):", nrow(string_absence_raw), "\n")
cat("String method absence matches (high confidence only):", nrow(string_absence_data), "\n")
cat("Training manual absences (with DOI):", nrow(training_absence_data), "\n")

# Sample presences randomly (target ~200 for balanced validation)
set.seed(1998)
presence_sample_size <- min(200, nrow(presence_predictions))
presence_sample <- presence_predictions %>%
  slice_sample(n = presence_sample_size) %>%
  mutate(absence_source = NA)  # Not applicable for presences

cat("Presence sample size:", nrow(presence_sample), "\n")

# =============================================================================
# ABSENCE VALIDATION SAMPLE PROCESSING
# =============================================================================
# Process absence data for complete validation coverage
absence_validation_raw <- bind_rows(
  absence_predictions,
  string_absence_data,
  training_absence_data
) %>%
  distinct(id, .keep_all = TRUE)  # Remove any duplicates by id

cat("Combined unique absence abstracts:", nrow(absence_validation_raw), "\n")

# =============================================================================
# PRESENCE VALIDATION SAMPLE PROCESSING
# =============================================================================
# Process presence data for stratified validation sampling
presence_validation_raw <- presence_predictions

cat("Presence abstracts available for sampling:", nrow(presence_validation_raw), "\n")

# =============================================================================
# ABSENCE SAMPLE STRATIFICATION AND PROCESSING
# =============================================================================
# Process absence validation data with stratification for complete coverage
absence_validation_data <- absence_validation_raw %>%
  # First group by id to get one row per abstract
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  # Add confidence categories based on available probability columns
  mutate(
    # Create confidence score from available probability columns
    confidence = pmax(
      coalesce(glmnet_prob_presence, 0),
      coalesce(svm_prob_presence, 0),
      na.rm = TRUE
    ),
    confidence_level = case_when(
      confidence >= 0.9 ~ "High (≥0.9)",
      confidence >= 0.7 ~ "Medium (0.7-0.9)",
      confidence >= 0.5 ~ "Low (0.5-0.7)",
      TRUE ~ "Very Low (<0.5)"),
    
    # Create stratification based on prediction and source
    predicted_label = final_classification,
    # Create unique combination for stratification
    stratum = paste(predicted_label, absence_source, confidence_level, sep = " | ")
  )


# Show absence stratum breakdown
cat("\n=== ABSENCE STRATIFICATION ===\n")
absence_stratum_summary <- absence_validation_data %>%
  count(stratum, name = "available") %>%
  arrange(desc(available))

print(absence_stratum_summary)

# For absences, take ALL available (complete coverage)
absence_sample_strategy <- absence_stratum_summary %>%
  mutate(target_n = available)  # Take all absences

cat("\nAbsence sample strategy (complete coverage):\n")
print(absence_sample_strategy %>% select(stratum, available, target_n))
cat("Total absence sample size:", sum(absence_sample_strategy$target_n), "\n")

# Generate absence validation sample (take all)
absence_validation_sample <- map_dfr(1:nrow(absence_sample_strategy), function(i) {
  stratum_name <- absence_sample_strategy$stratum[i]
  n_sample <- absence_sample_strategy$target_n[i]

  stratum_data <- absence_validation_data %>%
    filter(stratum == stratum_name)

  if (nrow(stratum_data) > 0 && n_sample > 0) {
    sampled <- stratum_data %>%
      slice_sample(n = min(n_sample, nrow(stratum_data)))
    return(sampled)
  } else {
    return(tibble())
  }
})

cat("\nAbsence validation sample generated:", nrow(absence_validation_sample), "abstracts\n")

# =============================================================================
# PRESENCE SAMPLE STRATIFICATION AND PROCESSING
# =============================================================================
# Process presence validation data with stratified sampling for balanced coverage
presence_validation_data <- presence_validation_raw %>%
  # First group by id to get one row per abstract
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  # Add confidence categories based on available probability columns
  mutate(
    # Create confidence score from available probability columns
    confidence = pmax(
      coalesce(glmnet_prob_presence, 0),
      coalesce(svm_prob_presence, 0),
      na.rm = TRUE
    ),
    confidence_level = case_when(
      confidence >= 0.9 ~ "High (≥0.9)",
      confidence >= 0.7 ~ "Medium (0.7-0.9)",
      confidence >= 0.5 ~ "Low (0.5-0.7)",
      TRUE ~ "Very Low (<0.5)"
    ),
    # Note: Information completeness and species detection columns not available in this dataset
    # Using confidence level as primary stratification dimension
    # Use final_classification as predicted_label for stratification
    predicted_label = final_classification,
    # Create unique combination for stratification
    stratum = paste(predicted_label, confidence_level, sep = " | ")
  )

# Show presence stratum breakdown
cat("\n=== PRESENCE STRATIFICATION ===\n")
presence_stratum_summary <- presence_validation_data %>%
  count(stratum, name = "available") %>%
  arrange(desc(available))

print(presence_stratum_summary)

# Define stratified sampling for presence data
set.seed(1998)
presence_sample_strategy <- presence_stratum_summary %>%
  mutate(
    # Determine sample size based on stratum importance and availability
    target_n = case_when(
      # High confidence Presence
      str_detect(stratum, "Presence.*High") ~ pmin(available, 40),
      # Medium confidence Presence
      str_detect(stratum, "Presence.*Medium") ~ pmin(available, 35),
      # Low confidence Presence
      str_detect(stratum, "Presence.*Low") ~ pmin(available, 25),
      # Very low confidence Presence
      str_detect(stratum, "Presence.*Very Low") ~ pmin(available, 15),
      # All other Presence strata
      str_detect(stratum, "^Presence") ~ pmin(available, 10),
      # Fallback
      TRUE ~ pmin(available, 5)
    ),
    # Ensure we sample at least 1 from each stratum if available
    target_n = pmax(1, target_n),
    # Don't exceed available
    target_n = pmin(target_n, available)
  )

cat("\nPresence sample strategy (stratified sampling):\n")
print(presence_sample_strategy %>% select(stratum, available, target_n))
cat("Total presence sample size:", sum(presence_sample_strategy$target_n), "\n")

# Generate presence validation sample
presence_validation_sample <- map_dfr(1:nrow(presence_sample_strategy), function(i) {
  stratum_name <- presence_sample_strategy$stratum[i]
  n_sample <- presence_sample_strategy$target_n[i]

  # Get abstracts from this stratum
  stratum_data <- presence_validation_data %>%
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

cat("\nPresence validation sample generated:", nrow(presence_validation_sample), "abstracts\n")

# =============================================================================
# ABSENCE VALIDATION TEMPLATE PREPARATION
# =============================================================================
# Add validation fields for absence-focused validation
absence_validation_sample <- absence_validation_sample %>%
  arrange(desc(confidence), absence_source, confidence_level) %>%
  mutate(
    # Add fields for manual absence validation
    validator_name = "",
    validation_date = "",
    manual_label = "",  # Presence/Absence/Irrelevant
    absence_source_confirmed = "",  # ML/String/Both/Neither
    manual_confidence = "",  # High/Medium/Low
    notes = "",
    validation_id = paste0("ABS_", str_pad(row_number(), 3, pad = "0"))
  )

# =============================================================================
# PRESENCE VALIDATION TEMPLATE PREPARATION
# =============================================================================
# Add validation fields for presence-focused validation
presence_validation_sample <- presence_validation_sample %>%
  arrange(desc(confidence), confidence_level) %>%
  mutate(
    # Add fields for manual presence validation
    validator_name = "",
    validation_date = "",
    manual_label = "",  # Presence/Absence/Irrelevant
    manual_confidence = "",  # High/Medium/Low
    notes = "",
    validation_id = paste0("PRS_", str_pad(row_number(), 3, pad = "0"))
  )

# =============================================================================
# OUTPUT FILE GENERATION
# =============================================================================
# Reorganize columns for better manual validation workflow
# Put validation fields first, then content columns, then metadata
absence_validation_sample <- absence_validation_sample %>%
  select(
    # Validation fields (leftmost)
    validation_id, validator_name, validation_date, manual_label,
    absence_source_confirmed, manual_confidence, notes,
    # Content fields
    abstract, article_title, authors,
    # Metadata fields
    id, doi, publication_year, source_title,
    # Prediction fields
    final_classification, confidence, confidence_level, absence_source,
    # All other original columns
    everything()
  )

presence_validation_sample <- presence_validation_sample %>%
  select(
    # Validation fields (leftmost)
    validation_id, validator_name, validation_date, manual_label,
    manual_confidence, notes,
    # Content fields
    abstract, article_title, authors,
    # Metadata fields
    id, doi, publication_year, source_title,
    # Prediction fields
    final_classification, confidence, confidence_level,
    # All other original columns
    everything()
  )

# Save separate validation samples for manual review
write_csv(absence_validation_sample, "results/absence_validation_sample_for_manual_review.csv")
write_csv(presence_validation_sample, "results/presence_validation_sample_for_manual_review.csv")

# Generate absence validation instructions
absence_instructions <- "
# ABSENCE MANUAL VALIDATION INSTRUCTIONS

## Overview
This file contains ALL {nrow(absence_validation_sample)} absence-tagged abstracts from ML predictions, high-confidence string method detections, and manually labeled training examples (with DOI). These abstracts were flagged as NOT finding fungal endophytes in plant taxa. The sample validates whether these research papers truly found no fungal endophytes.

## Validation Process

### Step 1: Confirm Absence Classification
For each abstract, determine:
- **manual_label**: Did this research find fungal endophytes in plant taxa?
  - 'Absence': Research found NO fungal endophytes in the plant taxon(s) studied
  - 'Presence': Research found fungal endophytes in the plant taxon(s) studied (false negative)
  - 'Irrelevant': Unclear, ambiguous, or insufficient information to determine findings

### Step 2: Absence Detection Source Confirmation
- **absence_source_confirmed**: Which method(s) correctly identified that this research found NO fungal endophytes?
  - 'ML': Only ML model correctly identified the absence finding
  - 'String': Only string method correctly identified the absence finding
  - 'Training_Manual': Only training manual labeling correctly identified the absence finding
  - 'Both': Both methods correctly identified the absence finding
  - 'Neither': Neither method correctly identified this as absence (false positive)

### Step 3: Confidence Assessment
- **manual_confidence**: How confident are you in your classification?
  - 'High': Very clear, unambiguous
  - 'Medium': Reasonably clear with minor uncertainty
  - 'Low': Difficult to determine, ambiguous

### Step 4: Notes
- **notes**: Any additional observations, issues, or context about the absence detection

## Validation Fields to Complete
1. **validator_name**: Your name/initials
2. **validation_date**: Date of validation (YYYY-MM-DD)
3. **manual_label**: Absence/Presence/Irrelevant
4. **absence_source_confirmed**: ML/String/Both/Neither
5. **manual_confidence**: High/Medium/Low
6. **notes**: Free text observations

## Sample Composition
{paste(capture.output(print(absence_sample_strategy %>% select(stratum, target_n))), collapse='\n')}

## Quality Control
- Validate abstracts in random order to avoid bias
- Take breaks to maintain focus and consistency
- If uncertain, mark as 'Low' confidence and add detailed notes
- Consider having a second validator review uncertain cases

## Analysis Plan
After validation, we will:
1. Calculate accuracy of absence detection (research finding no fungal endophytes)
2. Assess false positive/negative rates by method (ML vs high-confidence string)
3. Identify biases in absence detection across different research contexts
4. Compare detection performance across confidence levels
5. Recommend improvements to absence detection methods

## Contact
For questions about specific abstracts or validation criteria, contact the research team.

Generated: {Sys.time()}
"

# Generate presence validation instructions
presence_instructions <- "
# PRESENCE MANUAL VALIDATION INSTRUCTIONS

## Overview
This file contains a stratified sample of {nrow(presence_validation_sample)} presence-tagged abstracts from ML predictions. These abstracts were flagged as finding fungal endophytes in plant taxa. The sample validates whether these research papers truly found fungal endophytes.

## Validation Process

### Step 1: Confirm Presence Classification
For each abstract, determine:
- **manual_label**: Did this research find fungal endophytes in plant taxa?
  - 'Presence': Research found fungal endophytes in the plant taxon(s) studied
  - 'Absence': Research found NO fungal endophytes in the plant taxon(s) studied (false positive)
  - 'Irrelevant': Unclear, ambiguous, or insufficient information to determine findings

### Step 2: Confidence Assessment
- **manual_confidence**: How confident are you in your classification?
  - 'High': Very clear, unambiguous
  - 'Medium': Reasonably clear with minor uncertainty
  - 'Low': Difficult to determine, ambiguous

### Step 3: Notes
- **notes**: Any additional observations, issues, or context

## Validation Fields to Complete
1. **validator_name**: Your name/initials
2. **validation_date**: Date of validation (YYYY-MM-DD)
3. **manual_label**: Presence/Absence/Irrelevant
4. **manual_confidence**: High/Medium/Low
5. **notes**: Free text observations

## Sample Composition
{paste(capture.output(print(presence_sample_strategy %>% select(stratum, target_n))), collapse='\n')}

## Quality Control
- Validate abstracts in random order to avoid bias
- Take breaks to maintain focus and consistency
- If uncertain, mark as 'Low' confidence and add detailed notes
- Consider having a second validator review uncertain cases

## Analysis Plan
After validation, we will:
1. Calculate accuracy of presence detection (research finding fungal endophytes)
2. Assess false positive rates for presence classification
3. Identify biases in presence detection across research contexts
4. Compare detection performance across confidence levels
5. Recommend improvements to presence classification methods

## Contact
For questions about specific abstracts or validation criteria, contact the research team.

Generated: {Sys.time()}
"

writeLines(str_glue(absence_instructions), "results/ABSENCE_VALIDATION_INSTRUCTIONS.md")
writeLines(str_glue(presence_instructions), "results/PRESENCE_VALIDATION_INSTRUCTIONS.md")

# =============================================================================
# SAMPLE COMPOSITION SUMMARY AND REPORTING
# =============================================================================
# Generate detailed summary statistics for both validation samples

# Absence sample summary
cat("\n=== ABSENCE VALIDATION SAMPLE SUMMARY ===\n")
cat("Absence sample composition by prediction:\n")
absence_prediction_breakdown <- absence_validation_sample %>%
  count(predicted_label, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(absence_prediction_breakdown)

cat("\nAbsence sample composition by detection source:\n")
absence_source_breakdown <- absence_validation_sample %>%
  count(absence_source, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(absence_source_breakdown)

cat("\nAbsence sample composition by confidence:\n")
absence_confidence_breakdown <- absence_validation_sample %>%
  count(confidence_level, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(absence_confidence_breakdown)

# Presence sample summary
cat("\n=== PRESENCE VALIDATION SAMPLE SUMMARY ===\n")
cat("Presence sample composition by prediction:\n")
presence_prediction_breakdown <- presence_validation_sample %>%
  count(predicted_label, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(presence_prediction_breakdown)

cat("\nPresence sample composition by confidence:\n")
presence_confidence_breakdown <- presence_validation_sample %>%
  count(confidence_level, name = "n_abstracts") %>%
  mutate(percentage = round(100 * n_abstracts / sum(n_abstracts), 1))
print(presence_confidence_breakdown)

# Note: Species detection information not available in this dataset

# Create progress tracking templates
absence_progress <- tibble(
  sample_type = "Absence",
  validation_status = c("Not Started", "In Progress", "Completed", "Reviewed"),
  count = c(nrow(absence_validation_sample), 0, 0, 0),
  percentage = c(100, 0, 0, 0)
)

presence_progress <- tibble(
  sample_type = "Presence",
  validation_status = c("Not Started", "In Progress", "Completed", "Reviewed"),
  count = c(nrow(presence_validation_sample), 0, 0, 0),
  percentage = c(100, 0, 0, 0)
)

combined_progress <- bind_rows(absence_progress, presence_progress)

write_csv(combined_progress, "results/validation_progress_tracker.csv")

# =============================================================================
# SCRIPT COMPLETION AND NEXT STEPS
# =============================================================================
# Report successful completion and provide clear next steps for validation workflow
cat("\nFiles created:\n")
cat("✓ results/absence_validation_sample_for_manual_review.csv (", nrow(absence_validation_sample), " absence abstracts: all ML + high-confidence string + training manual)\n")
cat("✓ results/presence_validation_sample_for_manual_review.csv (", nrow(presence_validation_sample), " presence abstracts: stratified sample)\n")
cat("✓ results/ABSENCE_VALIDATION_INSTRUCTIONS.md (absence validation guidelines)\n")
cat("✓ results/PRESENCE_VALIDATION_INSTRUCTIONS.md (presence validation guidelines)\n")
cat("✓ results/validation_progress_tracker.csv (combined progress tracking)\n")

cat("\n=== NEXT STEPS ===\n")
cat("ABSENCE VALIDATION:\n")
cat("1. Open absence_validation_sample_for_manual_review.csv in Excel or similar\n")
cat("2. Read ABSENCE_VALIDATION_INSTRUCTIONS.md for detailed guidelines\n")
cat("3. Complete validation fields for each absence abstract\n")
cat("4. Save completed absence validation file\n")

cat("\nPRESENCE VALIDATION:\n")
cat("1. Open presence_validation_sample_for_manual_review.csv in Excel or similar\n")
cat("2. Read PRESENCE_VALIDATION_INSTRUCTIONS.md for detailed guidelines\n")
cat("3. Complete validation fields for each presence abstract\n")
cat("4. Save completed presence validation file\n")

cat("\n5. Run validation analysis script (when available)\n")
cat("\nSeparate absence and presence validation samples ready! 📋✅\n")
