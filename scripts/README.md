# Scripts Directory

This directory contains all R scripts for the Endo-Review analysis pipeline, organized by function and development stage.

## Directory Structure

### Analysis Pipeline (`04_analysis/`)
Core analysis scripts organized into specialized subdirectories:

#### `components/` - Pipeline Components
Individual processing modules that form the analysis pipeline:
- `01_extract_species.R` - Species name detection and taxonomic classification
- `02_extract_methods.R` - Research methodology identification
- `03_extract_plant_parts.R` - Plant tissue/organ detection
- `04_extract_geography.R` - Geographic location extraction
- `05_merge_results.R` - Result consolidation and integration

#### `utilities/` - Specialized Utilities
Performance and utility functions:
- `reference_data_utils.R` - Data normalization and reference management
- `build_bloom_filters.R` - Probabilistic filtering algorithms
- `create_species_subsets.R` - Test data generation

#### `tests/` - Test Suite
Comprehensive testing and validation:
- `test_extract_species.R` - Species extraction validation
- `test_pipeline_workflow.R` - Complete pipeline testing
- `quick_start_testing.R` - One-click pipeline validation

#### `validation/` - Quality Assurance
Result validation and quality control:
- `absence_evidence_detection.R` - Absence claim validation
- `find_all_plants_statement.R` - Ubiquity statement analysis
- `manual_validation_sample.R` - Expert validation sample generation

#### `visualization/` - Data Visualization
Analysis result visualization:
- `visualize_taxa_results.R` - Taxonomic distribution plots
- `visualize_extraction_results.R` - Geographic and method analysis
- `run_taxa_visualizations.R` - Visualization orchestration

#### `workflows/` - Workflow Orchestration
High-level pipeline coordination:
- `analysis_workflow.R` - Main analysis workflow
- `run_pipeline.R` - Complete pipeline execution
- `run_extraction_pipeline.R` - Data extraction pipeline

#### `temporal/` - Temporal Analysis
Time-series analysis of research trends:
- `temporal_trend_analysis.R` - Publication and method evolution analysis

### Model Training (`02_model_training/`)
Machine learning model development:
- `ML_compare_models_subset.R` - Model comparison and optimization

### Prediction (`03_prediction/`)
Model application and prediction:
- `apply_models_to_full_dataset.R` - Large-scale model application
- `consolidate_datasets.R` - Data consolidation and preparation

### Data Processing (`01_data_processing/`)
Initial data acquisition and preprocessing:
- `api_pull_abstracts.R` - Literature database API queries
- `Combo_abstracts_pull2.R` - Data consolidation and cleaning

### Configuration (`config/`)
Pipeline configuration and settings:
- `pipeline_config.R` - Centralized configuration management

### Utilities (`utils/`)
Shared utility functions:
- `error_handling.R` - Robust error handling and logging
- `memory_optimization.R` - Memory management utilities
- `plot_utils.R` - Plotting and visualization utilities

### Archive (`archive/`)
Historical scripts and alternative implementations:
- `ml_models/` - Previous ML model versions
- `taxa_detection/` - Alternative taxa detection approaches
- `pipeline_versions/` - Complete pipeline implementations
- `deprecated/` - Retired but preserved scripts

## Usage Guidelines

### Pipeline Execution Order
1. **Data Processing**: `01_data_processing/` scripts for data acquisition
2. **Model Training**: `02_model_training/` for ML model development
3. **Prediction**: `03_prediction/` for applying trained models
4. **Analysis**: `04_analysis/` components for comprehensive analysis

### Quick Start
```r
# Quick pipeline test
source("scripts/04_analysis/quick_start_testing.R")

# Full pipeline execution
source("scripts/04_analysis/workflows/run_pipeline.R")

# Individual component testing
source("scripts/04_analysis/tests/test_pipeline_workflow.R")
```

### Dependencies
All scripts require R and the tidyverse ecosystem. Specific dependencies are documented in individual script headers.

## Development Notes

- **Modular Design**: Components can be run independently or as part of the full pipeline
- **Error Handling**: Robust error handling with logging to `results/logs/`
- **Memory Optimization**: Large dataset processing with chunking and garbage collection
- **Testing**: Comprehensive test suite for validation and debugging
- **Documentation**: Each script includes detailed headers with usage instructions

## Maintenance

When adding new scripts:
1. Follow established naming conventions
2. Include comprehensive header documentation
3. Add appropriate error handling
4. Create corresponding tests in `tests/`
5. Update this README

For questions about specific components, refer to individual script headers or component README files.