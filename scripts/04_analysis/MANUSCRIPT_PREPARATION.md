# Manuscript Preparation

Authors: B. Bock, N. McKay, N.C. Johnson, C.A. Gehring
Goal journal: Nature

## Overview
This document is my first go at outlining the paper for this project. 

Concept: Do all plants have fungi inside them?
- This is often cited, but there is no paper that systematically assesses if this is true.
- It has not been assessed because there are ~19,000 papers about fungi inside plants, and only recent advances in ML methods have allowed us to approach such a large systematic review

# Methods
## Data Collection Update (September 22, 2025)
- **Updated**:   ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi" OR 
  "latent fungus" OR "latent fungi" OR "systemic fungus" OR "systemic fungi" OR 
  "internal fungi" OR "resident fungi" OR "seed-borne fungi" OR "seed-transmitted fungi" OR 
  "dark septate endophyte" OR "dark septate fungi" OR "DSE fungi")
  AND
  (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR 
  pteridophyte* OR tree* OR shrub* OR grass* OR "graminoid*" OR herb* OR 
  crop* OR seedling* OR sapling* OR seed* OR root* OR leaf* OR foliage OR shoot* OR 
  stem* OR twig* OR rhizome* OR thallus OR frond* OR algae OR "green alga*" OR macroalga* OR 
  cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
- **Rationale**: More precise fungal endophyte focus + expanded host organism coverage including bryophytes, algae, and lichen symbionts
- **Impact**: Enhanced literature coverage and reduced false positives
- PubMed search details in '01_data_processing/api_pull_abstracts.R'
- Abstracts and metadata from all three databases were joined and deduplicated in '01_data_processing/Combo_abstracts_pull2.R`

# Model Training

- Refer to '01_model_training/ML_compare_models_subset.R'
- Model 1: Relevance
- Trained to sort abstracts as either Relevant (discussing fungal endophytes in a plant) or Irrelevant (Review, bacterial-focused, other)
- Add numbers about how well this worked etc. and thresholds used, including figures as supplementary
- Model 2: Presence/Absence
- Discuss different models attempted, final ensemble approach, and numbers about how well it worked

# Model application

- Refer to '03_prediction/apply_models_to_full_dataset.R'
- Include numbers for both models (number of papers flagged as relevant, then number flagged as presence or absence)

# Validation of Model

- Refer to '04_analysis/validation' for three files:
   - absence_evidence_detection.R: Uses a string-based approach to look for papers in the database that explicitly say they found no fungal endophytes in a plant, so that we can validate the absence labels from the model
   - find_all_plants_statement.R: Uses a string-based approach to find abstracts in the database that explicitly make the statement that all plants have fungi inside them, to support the need for this project.
   - manual_validation_sample.R: Pulls all High confidence absence labels from the model and all absences labeled from the string approach, plus absences from the training dataset so that they can be manually checked. Pulls a stratified sample of Presences to check.

# Results

- Only one paper searched for fungal endophytes and did not find them (refer to 'results/manual_validation/absence_validation_sample_for_manual_review_BB.csv')
- Common that papers report endophyte-free plants (typically made so experimentally), but no examples of endophyte-free taxa in these cases
- Because there is potentially a bias in not publishing null results (e.g. not finding fungal endophytes in a plant), we also assessed the taxonomic coverage of both plants and fungi in the dataset to assess where we have and have not found examples of fungal endophytes
- Taxonomic biases: refer to files in the /results folder
- Geographic biases: which countries and regions have been over or understudied, include map from /plots
- Temporal biases: Probably in supplementary
