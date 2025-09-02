# MANUAL VALIDATION INSTRUCTIONS

## Overview
This file contains a stratified sample of 35 abstracts for manual validation of the machine learning classification model. The sample is designed to assess model accuracy across different confidence levels and information types.

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
# A tibble: 2 × 2
  stratum                                   target_n
  <chr>                                        <dbl>
1 Presence | High (≥0.9) | Species detected       25
2 Presence | High (≥0.9) | No species             10

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

Generated: 2025-09-02 12:32:06.142039
