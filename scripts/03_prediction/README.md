# Prediction Directory

This directory contains scripts for applying trained machine learning models to large-scale datasets and consolidating prediction results.

## Scripts Overview

### `apply_models_to_full_dataset.R` - Large-Scale Model Application
Primary script for applying trained ML models to full literature datasets:

#### Model Application Pipeline
1. **Data Loading**: Loads consolidated abstract datasets and trained models
2. **Relevance Classification**: Filters abstracts relevant to endophyte research questions
3. **Presence/Absence Classification**: Classifies relevant abstracts by fungal findings
4. **Ensemble Predictions**: Combines GLMNet and SVM predictions with optimized weights
5. **Confidence Scoring**: Generates prediction probabilities and confidence metrics

#### Key Features
- **Batch Processing**: Handles datasets of 20,000+ abstracts efficiently
- **Memory Management**: Chunked processing with garbage collection
- **Error Recovery**: Robust error handling with partial result preservation
- **Progress Monitoring**: Real-time progress tracking for long-running predictions
- **Quality Assurance**: Validation of prediction outputs and data integrity

#### Prediction Strategy
- **Two-Stage Classification**: Relevance filtering followed by presence/absence classification
- **Ensemble Approach**: Weighted combination of complementary models
- **Threshold Optimization**: Configurable decision boundaries for classification
- **Uncertainty Quantification**: Probability outputs for prediction confidence

### `consolidate_datasets.R` - Data Integration and Preparation
Dataset consolidation and preparation script:

#### Data Integration
- **Multi-source Merging**: Combines predictions with original abstract metadata
- **Column Standardization**: Harmonizes column names and data types
- **Missing Data Handling**: Robust handling of incomplete records
- **Duplicate Management**: Identifies and resolves duplicate entries

#### Quality Control
- **Data Validation**: Checks for required columns and data integrity
- **Consistency Verification**: Ensures prediction results match input data
- **Metadata Preservation**: Maintains all original abstract information
- **Audit Trail**: Logs all data transformations and merges

## Prediction Workflow

### Complete Pipeline Execution
```r
# Apply models to full dataset
source("scripts/03_prediction/apply_models_to_full_dataset.R")

# Consolidate results with original data
source("scripts/03_prediction/consolidate_datasets.R")
```

### Output Files

#### Model Application Results
- `results/full_dataset_predictions.csv` - Raw prediction outputs
- `results/relevant_abstracts_with_pa_predictions.csv` - Filtered relevant abstracts with P/A classifications

#### Consolidated Datasets
- `results/consolidated_dataset.csv` - Complete dataset with predictions and metadata
- `results/prepared_abstracts_for_extraction.csv` - Cleaned data for analysis pipeline

#### Quality Assurance
- `results/prediction_summary.txt` - Prediction statistics and performance metrics
- `results/data_consolidation_log.txt` - Consolidation process documentation

## Performance Considerations

### Scalability Features
- **Memory Efficient**: Processes data in configurable batches (default 1000 abstracts)
- **Parallel Processing**: Utilizes multiple CPU cores for prediction
- **Disk I/O Optimization**: Minimizes memory usage through streaming approaches
- **Progress Tracking**: Real-time monitoring with ETA calculations

### Error Handling
- **Graceful Failures**: Continues processing when individual predictions fail
- **Data Recovery**: Preserves partial results and provides recovery mechanisms
- **Logging**: Comprehensive logging to `results/logs/` directory
- **Validation**: Post-processing validation of prediction quality

## Dependencies

### Required Models
- `models/best_model_relevance_glmnet.rds` - Relevance classification model
- `models/best_model_presence_glmnet_ensemble.rds` - P/A GLMNet component
- `models/best_model_presence_svmLinear_ensemble.rds` - P/A SVM component

### Data Requirements
- `results/consolidated_dataset.csv` - Input abstracts for prediction
- `scripts/config/pipeline_config.R` - Model paths and parameters

### R Packages
- `caret`, `glmnet`, `e1071` (SVM), `dplyr`, `readr`, `progress`

## Prediction Quality Metrics

### Relevance Classification
- **Accuracy**: ~91% on relevant abstracts
- **Recall**: High sensitivity for endophyte-related content
- **Precision**: Minimizes false positives in irrelevant literature

### Presence/Absence Classification
- **Overall Accuracy**: 89.8% on relevant abstracts
- **Absence Recall**: 87% (prioritized for conservative endophyte ubiquity claims)
- **Presence Recall**: 95.8% (complements absence detection)
- **F1 Score**: Balanced performance across classes

## Configuration Options

### Ensemble Weights (from config)
```r
ENSEMBLE_WEIGHTS <- list(
  svm_weight_for_presence = 0.6,    # SVM weight for presence predictions
  glm_weight_for_absence = 0.8      # GLMNet weight for absence predictions
)
```

### Processing Parameters
- **Batch Size**: Configurable chunk size for memory management
- **Confidence Thresholds**: Decision boundaries for classification
- **Parallel Workers**: CPU cores for parallel prediction

## Validation and Monitoring

### Prediction Validation
- **Cross-referencing**: Compares predictions against known labeled data
- **Consistency Checks**: Verifies prediction probabilities are reasonable
- **Edge Case Testing**: Validates handling of ambiguous abstracts

### Performance Monitoring
- **Timing Analysis**: Tracks processing speed and bottlenecks
- **Memory Usage**: Monitors resource consumption
- **Error Rates**: Logs prediction failures and recovery actions

## Usage in Research Pipeline

This prediction stage bridges model training and analysis:

1. **Input**: Raw or consolidated abstract datasets
2. **Processing**: ML-based classification and filtering
3. **Output**: Classified abstracts ready for taxonomic/geographic analysis
4. **Integration**: Feeds directly into `04_analysis/` pipeline components

The prediction pipeline transforms raw literature data into structured, classified datasets that enable systematic analysis of endophyte research patterns and ubiquity claims.