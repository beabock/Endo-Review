# Endo-Review Project Structure

## Overview
This project implements machine learning models for endophyte research literature classification and species extraction, organized in a clean modular architecture.

**Data Collection Update (July 31, 2025):**
- **Enhanced Search Strategy**: ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi" OR "latent fungus" OR "latent fungi" OR "systemic fungus" OR "systemic fungi" OR "internal fungi" OR "resident fungi" OR "seed-borne fungi" OR "seed-transmitted fungi" OR "dark septate endophyte" OR "dark septate fungi" OR "DSE fungi") AND (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR pteridophyte* OR tree* OR shrub* OR grass* OR "graminoid*" OR herb* OR crop* OR seedling* OR sapling* OR seed* OR root* OR leaf* OR foliage OR shoot* OR stem* OR twig* OR rhizome* OR thallus OR frond* OR algae OR "green alga*" OR macroalga* OR cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
- **Database**: Web of Science Core Collection, PubMed, Scopus
- **Scope**: Comprehensive coverage of fungal endophytes across diverse host organisms
- **Status**: Current dataset for all analyses

## Directory Structure

```
Endo-Review/
├── docs/                              # Documentation and project guides
│   ├── README.md                     # Main project documentation
│   └── PROJECT_STRUCTURE.md          # This file - current structure guide
│
├── models/                           # Trained ML models and reference data
│   ├── accepted_species.rds          # Species reference data
│   ├── best_model_presence_*.rds     # Trained classification models
│   ├── families.rds                  # Plant family taxonomic data
│   ├── genera.rds                    # Plant genus taxonomic data
│   ├── species_*.rds                 # Species lookup tables
│   └── plant_parts_grouped.rds       # Plant part groupings
│
├── plots/                            # Visualization outputs and summaries
│   ├── families_per_phylum_summary.csv
│   ├── genera_per_phylum_summary.csv
│   └── visualization_summary.csv
│
├── results/                          # Analysis outputs and processed data
│   ├── comprehensive_extraction_results.csv
│   ├── consolidated_dataset.csv
│   ├── full_dataset_predictions.csv
│   ├── manuscript_*.csv              # Manuscript-specific analysis files
│   ├── geographic_*.csv              # Geographic analysis outputs
│   ├── species_detection_trends.csv
│   ├── research_*.csv                # Research pattern analyses
│   ├── evaluation_table_*.csv        # Model evaluation results
│   ├── temp_species_batch_*.csv      # Batch processing outputs
│   ├── *.txt                         # Analysis reports and summaries
│   └── manual_validation/            # Manual validation samples and instructions
│       ├── ABSENCE_VALIDATION_INSTRUCTIONS.md
│       ├── PRESENCE_VALIDATION_INSTRUCTIONS.md
│       └── validation_*.csv
│
├── scripts/                          # Modular R scripts organized by function
│   ├── 01_data_processing/           # Data acquisition and preprocessing
│   │   ├── api_pull_abstracts.R
│   │   └── Combo_abstracts_pull2.R
│   ├── 02_model_training/            # Machine learning model development
│   │   └── ML_compare_models_subset.R
│   ├── 03_prediction/                # Apply trained models to datasets
│   │   ├── apply_models_to_full_dataset.R
│   │   └── consolidate_datasets.R
│   ├── 04_analysis/                  # Species extraction and analysis pipeline
│   │   ├── components/               # Core extraction components
│   │   │   ├── 01_extract_species.R
│   │   │   ├── 01b_mycorrhizal_check.R
│   │   │   ├── 02_extract_methods.R
│   │   │   ├── 03_extract_plant_parts.R
│   │   │   ├── 04_extract_geography.R
│   │   │   ├── 05_merge_results.R
│   │   │   └── README_mycorrhizal_check.md
│   │   ├── workflows/                # Main pipeline workflows
│   │   │   ├── analysis_workflow.R
│   │   │   ├── run_extraction_pipeline.R
│   │   │   └── run_pipeline.R
│   │   ├── validation/               # Validation and quality control
│   │   │   ├── absence_evidence_detection.R
│   │   │   ├── find_all_plants_statement.R
│   │   │   └── manual_validation_sample.R
│   │   ├── visualization/            # Data visualization scripts
│   │   │   ├── run_taxa_visualizations.R
│   │   │   ├── temporal_trend_analysis.R
│   │   │   ├── visualize_extraction_results.R
│   │   │   └── visualize_taxa_results.R
│   │   ├── utilities/                # Helper functions and utilities
│   │   │   ├── build_bloom_filters.R
│   │   │   ├── create_species_subsets.R
│   │   │   └── reference_data_utils.R
│   │   ├── tests/                    # Testing and validation scripts
│   │   │   ├── test_*.R
│   │   │   └── profile_species.R
│   │   ├── temporal/                 # Temporal analysis functions
│   │   │   └── temporal_trend_analysis.R
│   │   ├── archive/                  # Legacy analysis scripts
│   │   │   ├── extract_species_simple.R
│   │   │   └── run_taxa_visualizations.R
│   │   ├── MANUSCRIPT_PREPARATION.md
│   │   ├── README.md
│   │   └── *.R                       # Main analysis scripts
│   ├── archive/                      # Organized archive of legacy scripts
│   │   ├── ml_models/               # Previous ML model versions
│   │   ├── pipeline_versions/        # Previous pipeline implementations
│   │   ├── taxa_detection/           # Previous species detection methods
│   │   └── deprecated/              # Deprecated functionality
│   ├── config/                       # Configuration files
│   │   └── pipeline_config.R
│   └── utils/                        # Shared utility functions
│       ├── error_handling.R
│       ├── memory_optimization.R
│       └── plot_utils.R
│
├── test_data/                        # Test datasets for validation
│   └── test_subset_random_*.csv     # Various test set sizes
│
├── test_results/                     # Test execution outputs
│   ├── comparison_report.txt
│   ├── test_results_*.txt
│   └── test_subset_random_*/        # Individual test run results
│       ├── comprehensive_results.csv
│       ├── analysis/                 # Detailed analysis outputs
│       └── *.csv                     # Component-specific results
│
├── LICENSE
├── Endo-Review.Rproj                # RStudio project file
└── .gitignore
```

## Modular Architecture Overview

### Scripts Organization by Function

**Phase 1: Data Processing (`01_data_processing/`)**
- Raw data acquisition from APIs and external sources
- Data cleaning and preprocessing workflows
- Abstract collection and preparation

**Phase 2: Model Training (`02_model_training/`)**
- Machine learning model development and comparison
- Training pipeline optimization
- Model evaluation and selection

**Phase 3: Prediction (`03_prediction/`)**
- Application of trained models to full datasets
- Result consolidation and formatting
- Batch processing workflows

**Phase 4: Analysis (`04_analysis/`)**
- **Components**: Modular extraction functions for species, methods, geography
- **Workflows**: Main pipeline orchestration
- **Validation**: Quality control and evidence detection
- **Visualization**: Data visualization and analysis plotting
- **Utilities**: Helper functions and data processing tools
- **Tests**: Comprehensive testing framework
- **Archive**: Legacy analysis implementations

### Archive Organization
The archive system is organized by functionality:
- `ml_models/`: Previous machine learning model versions
- `pipeline_versions/`: Earlier pipeline implementations
- `taxa_detection/`: Legacy species detection methods
- `deprecated/`: Functionality no longer in use

## Key Files by Purpose

### Core Analysis Pipeline
- `scripts/04_analysis/components/01_extract_species.R` - Main species extraction engine
- `scripts/04_analysis/workflows/run_extraction_pipeline.R` - Primary pipeline orchestrator
- `scripts/04_analysis/components/05_merge_results.R` - Result consolidation

### Model Development
- `scripts/02_model_training/ML_compare_models_subset.R` - Model training and comparison
- `scripts/03_prediction/apply_models_to_full_dataset.R` - Production prediction pipeline

### Quality Control
- `scripts/04_analysis/validation/absence_evidence_detection.R` - Absence evidence validation
- `scripts/04_analysis/validation/manual_validation_sample.R` - Manual validation sampling

### Utilities and Testing
- `scripts/04_analysis/utilities/build_bloom_filters.R` - Species filtering optimization
- `scripts/04_analysis/tests/test_pipeline_workflow.R` - Comprehensive pipeline testing

## Results Organization

### Primary Analysis Outputs
- `results/comprehensive_extraction_results.csv` - Complete species and metadata extraction
- `results/consolidated_dataset.csv` - Consolidated analysis dataset
- `results/full_dataset_predictions.csv` - Model predictions on complete dataset

### Specialized Analyses
- `results/manuscript_*.csv` - Publication-ready analysis tables
- `results/geographic_*.csv` - Geographic distribution analyses
- `results/research_*.csv` - Research pattern and gap analyses

### Validation and Quality Control
- `results/manual_validation/` - Manual validation samples and instructions
- `results/evaluation_table_*.csv` - Model performance evaluations

## Navigation Guide

### For Data Scientists/Analysts
1. Start with `scripts/04_analysis/README.md` for pipeline overview
2. Use `scripts/04_analysis/workflows/run_extraction_pipeline.R` for main analysis
3. Check `results/` for outputs and `test_results/` for validation

### For Model Developers
1. Review `scripts/02_model_training/` for model development
2. Use `scripts/03_prediction/` for model application
3. Evaluate results in `results/evaluation_table_*.csv`

### For Quality Assurance
1. Use `scripts/04_analysis/validation/` for validation workflows
2. Review `results/manual_validation/` for manual review samples
3. Check `scripts/04_analysis/tests/` for testing framework

### For Manuscript Preparation
1. See `scripts/04_analysis/MANUSCRIPT_PREPARATION.md`
2. Use `results/manuscript_*.csv` files for publication tables
3. Review `results/manuscript_*.txt` for analysis summaries

## Workflow

1. **Data Preparation**: Use `scripts/01_data_processing/` for data acquisition
2. **Model Training**: Develop models in `scripts/02_model_training/`
3. **Prediction**: Apply models via `scripts/03_prediction/`
4. **Analysis**: Run extraction pipeline in `scripts/04_analysis/`
5. **Results**: Access all outputs in organized `results/` structure

## Usage Examples

### Running Full Analysis Pipeline
```bash
cd scripts/04_analysis
Rscript workflows/run_extraction_pipeline.R
```

### Running Model Predictions
```bash
cd scripts/03_prediction
Rscript apply_models_to_full_dataset.R
```

### Running Specific Component
```bash
cd scripts/04_analysis/components
Rscript 01_extract_species.R
```

## Data Sources
- **Abstracts**: Web of Science literature search (current dataset)
- **Species Data**: GBIF backbone taxonomy and custom validation
- **Training Labels**: Manual annotation of endophyte presence/absence
- **Geographic Data**: Country and ecosystem classifications

## Dependencies
- R packages: tidyverse, caret, glmnet, e1071, xgboost, randomForest, duckdb
- Custom utilities in `scripts/utils/` and `scripts/04_analysis/utilities/`
- Configuration management via `scripts/config/pipeline_config.R`

## Output Formats
- **CSV**: Structured data for analysis and publication
- **RDS**: R model objects and processed data
- **TXT**: Analysis reports and documentation
- **PNG**: Visualizations (when applicable)
