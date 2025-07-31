# Endo-Review Project Structure

## Overview
This project implements machine learning models for endophyte research literature classification and species extraction.

**Data Collection Update (July 31, 2025):**
- **Enhanced Search Strategy**: ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi") AND (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR pteridophyte* OR algae OR green alga* OR macroalga* OR cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
- **Database**: Web of Science Core Collection
- **Scope**: Comprehensive coverage of fungal endophytes across diverse host organisms
- **Status**: Current dataset for all analyses

## Directory Structure

```
Endo-Review/
├── data/                              # Data files
│   ├── raw/                          # Original, unmodified data
│   │   ├── All_abstracts.csv         # Complete abstract dataset
│   │   ├── Training_labeled_abs_5.csv # Labeled training data
│   │   └── pbdb_all.csv              # Paleobiology database
│   ├── processed/                    # Cleaned/transformed data
│   ├── gbif_backbone/               # Species reference data
│   ├── All_abstracts.csv           # Working copy (frequently accessed)
│   └── Training_labeled_abs_5.csv  # Working copy (frequently accessed)
│
├── scripts/                          # All R scripts organized by purpose
│   ├── 01_data_processing/          # Data cleaning and preparation
│   ├── 02_model_training/           # Machine learning model development
│   │   └── ML_compare_models_subset_optimized.R
│   ├── 03_prediction/               # Apply trained models to new data
│   │   ├── apply_models_to_full_dataset.R
│   │   └── apply_two_stage_models.R
│   ├── 04_analysis/                 # Species extraction and analysis
│   │   ├── extract_species_simple.R
│   │   └── run_species_extraction_pipeline.R
│   ├── archive/                     # Old script versions
│   ├── extracting_plant_names_optimized.R
│   ├── improved_taxa_detection.R
│   ├── integrate_improved_detection.R
│   ├── optimized_taxa_detection.R
│   ├── run_taxa_detection.R
│   ├── scale_taxa_detection.R
│   └── visualize_taxa_results.R
│
├── models/                          # Trained ML models and reference data
│   ├── archive/                     # Old model versions
│   ├── accepted_species.rds         # Species reference data
│   ├── best_model_*.rds            # Trained classification models
│   ├── families.rds                # Plant family data
│   ├── genera.rds                  # Plant genus data
│   ├── species.rds                 # Species lookup tables
│   └── valid_species_lookup.rds    # Validated species names
│
├── results/                         # All outputs and analysis results
│   ├── predictions/                 # Model prediction outputs
│   │   ├── absence_abstracts.csv
│   │   ├── all_abstracts_predictions_2025-07-22.csv
│   │   ├── full_predictions_with_metadata.csv
│   │   ├── high_confidence_absence_2025-07-22.csv
│   │   ├── high_confidence_presence_2025-07-22.csv
│   │   ├── irrelevant_uncertain_abstracts.csv
│   │   ├── relevance_preds.csv
│   │   ├── relevance_pa_preds_all_abstracts.csv
│   │   └── uncertain_predictions_2025-07-22.csv
│   ├── summaries/                   # Summary reports and analyses
│   │   ├── absences.csv
│   │   └── classification_summary.txt
│   ├── archive/                     # Old result versions
│   ├── comprehensive_extraction_results.csv
│   ├── comprehensive_extraction_report.txt
│   ├── species_detection_weighted_ensemble.csv
│   └── *.png                       # Analysis plots and visualizations
│
├── figures/                         # Publication-ready plots
│   ├── accuracy_comparison_*.png
│   ├── class_balanced_accuracy_*.png
│   ├── confusion_matrix_*.png
│   ├── PCA_optimized.png
│   └── Rplots.pdf
│
├── docs/                           # Documentation
│   ├── README.md
│   ├── README_taxa_detection.md
│   └── taxa_detection_improvements.md
│
├── archive/                        # Historical project versions
│   ├── All_Abstracts-11-18-24/
│   ├── dataset/
│   ├── Results/
│   └── *.csv                      # Old data files
│
├── README.md                       # Main project documentation
├── PROJECT_STRUCTURE.md            # This file
├── LICENSE
├── Endo-Review.Rproj               # RStudio project file
└── .gitignore
```

## Key Files

### Main Analysis Scripts
- `scripts/04_analysis/extract_species_simple.R` - Comprehensive species extraction pipeline
- `scripts/03_prediction/apply_models_to_full_dataset.R` - Production prediction pipeline
- `scripts/02_model_training/ML_compare_models_subset_optimized.R` - Model training and comparison

### Model Performance
- **Best Model**: Weighted ensemble (GLMNet + SVM) - 89.8% accuracy
- **Presence Recall**: 91.6%
- **Absence Recall**: 82.6%

### Key Outputs
- `results/comprehensive_extraction_results.csv` - Complete species and metadata extraction
- `results/predictions/` - All model predictions on full dataset
- `results/summaries/` - Analysis summaries and reports

## Workflow

1. **Data Preparation**: Raw data in `data/raw/`, working copies in `data/`
2. **Model Training**: Scripts in `scripts/02_model_training/`
3. **Prediction**: Scripts in `scripts/03_prediction/` apply models to full dataset
4. **Analysis**: Scripts in `scripts/04_analysis/` extract species and metadata
5. **Results**: All outputs organized in `results/` with subdirectories

## Usage

### Running Species Extraction
```bash
cd scripts/04_analysis
Rscript extract_species_simple.R
```

### Running Model Predictions
```bash
cd scripts/03_prediction
Rscript apply_models_to_full_dataset.R
```

## Data Sources
- **Abstracts**: Web of Science literature search
- **Species Data**: GBIF backbone taxonomy
- **Training Labels**: Manual annotation of endophyte presence/absence

## Dependencies
- R packages: tidyverse, tictoc, caret, glmnet, e1071, xgboost, randomForest
- Species detection functions in `scripts/optimized_taxa_detection.R`

## Output Formats
- **CSV**: Structured data for further analysis
- **PNG**: Visualizations and performance plots
- **TXT**: Summary reports and documentation
