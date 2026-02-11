# PRESENCE MANUAL VALIDATION INSTRUCTIONS

## Overview
This file contains a stratified sample of 77 presence-tagged abstracts from ML predictions. These abstracts were flagged as finding fungal endophytes in plant taxa. The sample validates whether these research papers truly found fungal endophytes.

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
# A tibble: 3 × 2
  stratum                     target_n
  <chr>                          <dbl>
1 Presence | High (≥0.9)            40
2 Presence | Medium (0.7-0.9)       35
3 Presence | Low (0.5-0.7)           2

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

Generated: 2025-09-23 10:48:25.003882
