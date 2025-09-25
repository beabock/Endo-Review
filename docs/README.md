# Endo-Review - Comprehensive Endophyte Research Analysis Pipeline

This repository implements a complete machine learning and analysis pipeline for endophyte research literature classification and species extraction. The project addresses the fundamental question: **"Do all plants host fungal endophytes?"** through systematic analysis of scientific literature.

## Overview

The pipeline combines machine learning classification models with advanced species extraction, geographic analysis, temporal trend analysis, and validation workflows to provide comprehensive insights into fungal endophyte research patterns and gaps.

## Key Features

### 🔬 **Machine Learning Classification**
- **Relevance Classification**: Identifies literature relevant to fungal endophytes
- **Presence/Absence Detection**: Determines if studies report endophyte presence or absence
- **Ensemble Models**: Weighted ensemble of GLMNet and SVM models (89.8% accuracy)

### 🌿 **Species and Taxa Analysis**
- **Comprehensive Extraction**: Species, genus, and family identification from scientific abstracts
- **Mycorrhizal Filtering**: Advanced detection using FUNGuild database
- **Taxonomic Validation**: Cross-referencing with GBIF backbone taxonomy
- **Multi-level Analysis**: Plant parts, research methods, and geographic patterns

### 🌍 **Geographic and Temporal Analysis**
- **Global Research Patterns**: Analysis of geographic biases and research equity
- **Temporal Trends**: Publication trends and methodology evolution over time
- **Research Gap Identification**: Identifies understudied regions and taxa
- **North-South Equity Analysis**: Comprehensive assessment of research distribution

### ✅ **Quality Control and Validation**
- **Stratified Sampling**: Manual validation with confidence-based stratification
- **Absence Evidence Detection**: Identification of negative results reporting
- **Error Handling**: Robust pipeline with comprehensive error management
- **Progress Tracking**: Detailed execution reports and quality metrics

## Modular Architecture

The project is organized into a clean modular structure with specialized components:

### Core Pipeline Components
- [`scripts/04_analysis/components/01_extract_species.R`](scripts/04_analysis/components/01_extract_species.R) - Main species extraction and information processing
- [`scripts/04_analysis/components/01b_mycorrhizal_check.R`](scripts/04_analysis/components/01b_mycorrhizal_check.R) - Mycorrhizal taxa identification and filtering
- [`scripts/04_analysis/components/02_extract_methods.R`](scripts/04_analysis/components/02_extract_methods.R) - Research methodology analysis
- [`scripts/04_analysis/components/03_extract_plant_parts.R`](scripts/04_analysis/components/03_extract_plant_parts.R) - Plant tissue identification
- [`scripts/04_analysis/components/04_extract_geography.R`](scripts/04_analysis/components/04_extract_geography.R) - Geographic data extraction
- [`scripts/04_analysis/components/05_merge_results.R`](scripts/04_analysis/components/05_merge_results.R) - Results consolidation and quality checks

### Analysis Workflows
- [`scripts/04_analysis/workflows/run_pipeline.R`](scripts/04_analysis/workflows/run_pipeline.R) - Master pipeline orchestrator
- [`scripts/04_analysis/workflows/analysis_workflow.R`](scripts/04_analysis/workflows/analysis_workflow.R) - Core analysis workflow
- [`scripts/04_analysis/workflows/run_extraction_pipeline.R`](scripts/04_analysis/workflows/run_extraction_pipeline.R) - Species extraction orchestration

### Supporting Infrastructure
- [`scripts/04_analysis/utilities/reference_data_utils.R`](scripts/04_analysis/utilities/reference_data_utils.R) - Centralized reference data and standardization
- [`scripts/04_analysis/utilities/build_bloom_filters.R`](scripts/04_analysis/utilities/build_bloom_filters.R) - High-performance species lookup
- [`scripts/04_analysis/utilities/create_species_subsets.R`](scripts/04_analysis/utilities/create_species_subsets.R) - Species subset creation for testing

### Documentation and Validation
- [`scripts/04_analysis/README.md`](scripts/04_analysis/README.md) - Detailed analysis workflow documentation
- [`docs/README_analysis_workflow.md`](docs/README_analysis_workflow.md) - Analysis pipeline documentation
- [`docs/README_modular_pipeline.md`](docs/README_modular_pipeline.md) - Modular architecture overview
- [`results/VALIDATION_INSTRUCTIONS.md`](results/VALIDATION_INSTRUCTIONS.md) - Manual validation guidelines

## Quick Start

### Complete Pipeline Execution
```r
# Run the complete endophyte analysis pipeline
source("scripts/04_analysis/workflows/run_pipeline.R")
result <- run_endophyte_pipeline()
```

### Modular Component Execution
```r
# Run individual components
source("scripts/04_analysis/components/01_extract_species.R")
source("scripts/04_analysis/components/01b_mycorrhizal_check.R")
source("scripts/04_analysis/geographic_bias_analysis.R")
```

### Analysis-Only Pipeline (Skip Model Training)
```r
# Run analysis stages assuming models are already trained
source("scripts/04_analysis/workflows/run_pipeline.R")
result <- run_analysis_only()
```

## Pipeline Stages

1. **Model Training** (`scripts/02_model_training/`)
   - Train relevance and presence/absence classification models
   - Compare multiple algorithms and select best performers

2. **Full Dataset Prediction** (`scripts/03_prediction/`)
   - Apply trained models to complete literature dataset
   - Generate predictions for relevance and endophyte presence

3. **Species Extraction** (`scripts/04_analysis/components/`)
   - Extract species, methods, geography, and plant parts
   - Apply mycorrhizal filtering and taxonomic validation

4. **Geographic Analysis** (`scripts/04_analysis/`)
   - Analyze global research patterns and equity
   - Identify geographic biases and research gaps

5. **Temporal Analysis** (`scripts/04_analysis/`)
   - Track research trends and methodology evolution
   - Analyze publication patterns over time

6. **Validation and Quality Control** (`scripts/04_analysis/`)
   - Generate stratified validation samples
   - Detect absence evidence and negative results

## Data Requirements

### Input Data
- `data/All_abstracts.csv` - Complete literature dataset with abstracts
- `data/Training_labeled_abs_6.csv` - Manually labeled training data
- `models/species.rds` - GBIF species reference data
- `models/accepted_species.rds` - Validated species names

### Reference Data
- **Species Data**: GBIF backbone taxonomy for validation
- **Geographic Data**: Country classifications and biodiversity hotspots
- **Method Keywords**: Standardized research methodology terms
- **Plant Parts**: Comprehensive plant tissue terminology

## Output Structure

### Primary Results
- `results/comprehensive_extraction_results.csv` - Complete species and metadata extraction
- `results/relevant_abstracts_with_pa_predictions.csv` - Model predictions with confidence scores
- `results/species_detection_results.csv` - Species detection and validation results

### Analysis Reports
- `results/geographic_bias_analysis_report.txt` - Geographic equity analysis
- `results/temporal_trends_summary.csv` - Temporal trend summaries
- `results/absence_evidence_analysis.csv` - Absence evidence detection results

### Validation and Quality Control
- `results/manual_validation/` - Manual validation samples and instructions
- `results/evaluation_table_*.csv` - Model performance evaluations

## Dependencies

### R Packages
```r
required_packages <- c(
  "tidyverse", "tidytext", "caret", "Matrix", "text", "tm", "recipes", 
  "themis", "janitor", "tictoc", "maps", "ggplot2", "viridis", "scales",
  "lubridate", "stringr", "rgbif", "furrr", "stringdist", "irlba"
)
```

### System Requirements
- R 4.0 or higher
- 16GB+ RAM recommended for large dataset processing
- Parallel processing capabilities for optimal performance

## Performance Metrics

### Model Performance
- **Weighted Ensemble Accuracy**: 89.8%
- **Presence Recall**: 91.6%
- **Absence Recall**: 82.6%
- **Cross-validation**: 5-fold stratified validation

### Processing Speed
- **Full dataset prediction**: 10-20 minutes
- **Species extraction**: 30-60 minutes
- **Geographic analysis**: 5-10 minutes
- **Complete pipeline**: 60-120 minutes

## Documentation

### Core Documentation
- [`docs/PROJECT_STRUCTURE.md`](docs/PROJECT_STRUCTURE.md) - Complete project organization
- [`docs/METHODS.md`](docs/METHODS.md) - Detailed methodology description
- [`docs/RESEARCH_OBJECTIVES.md`](docs/RESEARCH_OBJECTIVES.md) - Research goals and scope

### Workflow Documentation
- [`docs/README_analysis_workflow.md`](docs/README_analysis_workflow.md) - Analysis pipeline details
- [`docs/README_modular_pipeline.md`](docs/README_modular_pipeline.md) - Modular architecture guide
- [`docs/README_PIPELINE_IMPROVEMENTS.md`](docs/README_PIPELINE_IMPROVEMENTS.md) - Pipeline enhancements

### Analysis Guides
- [`docs/README_taxa_detection.md`](docs/README_taxa_detection.md) - Species detection methodology
- [`docs/README_testing_workflow.md`](docs/README_testing_workflow.md) - Testing and validation
- [`docs/PROCESSING_WORKFLOW.md`](docs/PROCESSING_WORKFLOW.md) - Data processing workflow

## Contributing

### Code Organization
- Follow modular structure in `scripts/04_analysis/`
- Use reference data utilities for consistency
- Include comprehensive error handling
- Document all functions and parameters

### Quality Standards
- Maintain 80%+ test coverage for new components
- Use standardized data formats
- Follow existing naming conventions
- Include usage examples in documentation

## Archive Structure

### Historical Versions
- `scripts/archive/` - Legacy script versions and deprecated code
- `archive/data/` - Historical data versions and backups
- `models/archive/` - Previous model iterations
- `results/archive/` - Previous analysis results

### Version Control
- All major changes tracked with version numbers
- Deprecated functions clearly marked
- Migration guides provided for breaking changes
- Archive documentation maintained for reference

## License

See the LICENSE file for details.

## Citation

For research using this pipeline, please cite the associated publication and methodology documentation in `docs/CITATION_ANALYSIS_FRAMEWORK.md`.
