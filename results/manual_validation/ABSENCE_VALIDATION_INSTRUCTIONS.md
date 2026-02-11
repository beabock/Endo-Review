# ABSENCE MANUAL VALIDATION INSTRUCTIONS

## Overview
This file contains ALL 102 absence-tagged abstracts from ML predictions, high-confidence string method detections, and manually labeled training examples (with DOI). These abstracts were flagged as NOT finding fungal endophytes in plant taxa. The sample validates whether these research papers truly found no fungal endophytes.

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
# A tibble: 6 × 2
  stratum                                target_n
  <chr>                                     <int>
1 Absence | ML | High (≥0.9)                   71
2 Presence | String | Very Low (<0.5)          12
3 Absence | ML | Low (0.5-0.7)                  7
4 Absence | ML | Very Low (<0.5)                7
5 Absence | ML | Medium (0.7-0.9)               4
6 NA | Training_Manual | Very Low (<0.5)        1

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

Generated: 2025-09-23 10:48:24.986183
