# Model Training Directory

This directory contains scripts for training and optimizing machine learning models used in the Endo-Review abstract classification pipeline.

## Scripts Overview

### `ML_compare_models_subset.R` - Primary Model Training Pipeline
Comprehensive machine learning pipeline for training relevance and presence/absence classification models:

#### Two-Stage Classification Approach
1. **Relevance Classification**: Identifies abstracts relevant to endophyte research questions
2. **Presence/Absence Classification**: Classifies relevant abstracts by reported fungal findings

#### Model Training Features
- **Ensemble Methods**: Combines GLMNet (regularized logistic regression) and SVM for optimal performance
- **Class Balancing**: SMOTE (Synthetic Minority Over-sampling Technique) for imbalanced datasets
- **Cross-Validation**: 5-fold repeated cross-validation with 3 repeats for robust evaluation
- **Hyperparameter Tuning**: Automated parameter optimization using caret framework
- **Feature Engineering**: TF-IDF weighting on unigrams and bigrams from text preprocessing

#### Model Evaluation
- **Performance Metrics**: Accuracy, precision, recall, specificity, F1-score
- **Confusion Matrices**: Detailed classification performance analysis
- **ROC Curves**: Receiver Operating Characteristic analysis
- **Feature Importance**: Analysis of predictive text features
- **Threshold Optimization**: Decision boundary tuning for ensemble predictions

#### Manuscript-Ready Outputs
- **Publication Figures**: ROC curves, confusion matrices, feature importance plots
- **Model Documentation**: Complete parameter settings and training details
- **Performance Tables**: Formatted results for academic publication

## Model Architecture

### Relevance Classification Model
- **Algorithm**: Regularized Logistic Regression (GLMNet)
- **Features**: Term frequency from unigrams + bigrams
- **Class Balancing**: SMOTE with 2:1 weight for relevant class
- **Performance**: ~91% recall on relevant abstracts

### Presence/Absence Ensemble Model
- **GLMNet Component**: Optimized for absence detection (87% recall)
- **SVM Component**: Optimized for presence detection (95.8% recall)
- **Ensemble Weights**: SVM 0.6, GLMNet 0.8 (prioritizing absence detection)
- **Combined Performance**: 89.8% overall accuracy

## Usage

### Complete Model Training
```r
source("scripts/02_model_training/ML_compare_models_subset.R")
```

### Model Application
After training, apply models using:
```r
source("scripts/03_prediction/apply_models_to_full_dataset.R")
```

## Dependencies

- **R Packages**: `tidyverse`, `caret`, `glmnet`, `themis`, `tidytext`, `tm`, `recipes`, `pROC`, `PRROC`
- **Training Data**: Labeled abstracts from `scripts/config/pipeline_config.R`
- **Reference Data**: Preprocessed text corpora and lookup tables

## Output Files

### Model Files
- `models/best_model_relevance_glmnet.rds` - Trained relevance classifier
- `models/best_model_presence_glmnet_ensemble.rds` - GLMNet ensemble component
- `models/best_model_presence_svmLinear_ensemble.rds` - SVM ensemble component

### Evaluation Results
- `results/evaluation_table_relevance.csv` - Relevance model performance
- `results/evaluation_table_pa.csv` - Presence/absence model performance
- `results/manuscript_*.csv/txt` - Publication-ready model documentation

### Visualization Outputs
- `plots/manuscript_*.png` - Model performance plots (ROC curves, confusion matrices, feature importance)
- `plots/manuscript_class_distribution_*.png` - Training data class distributions

## Training Data Requirements

- **Labeled Abstracts**: ~1,000 manually classified abstracts for training
- **Class Distribution**: Balanced relevance labels, imbalanced presence/absence (favoring presence)
- **Text Features**: Title + abstract text with preprocessing (stop words, lowercase, numeric removal)

## Performance Optimization

- **Memory Efficient**: Processes training data in batches with garbage collection
- **Parallel Training**: Utilizes multiple CPU cores for cross-validation
- **Early Stopping**: Prevents overfitting with regularization parameters
- **Feature Selection**: Automatic selection of most predictive text features

## Model Interpretability

- **Feature Importance**: Top predictive words/phrases identified
- **Decision Boundaries**: Configurable classification thresholds
- **Confidence Scores**: Probability outputs for uncertainty quantification
- **Error Analysis**: Detailed breakdown of misclassifications

## Validation and Testing

Models are validated through:
- **Cross-validation**: Internal validation during training
- **Holdout Testing**: Performance on unseen data
- **Manual Review**: Expert validation of predictions
- **Consistency Checks**: Reproducibility across training runs

## Maintenance Notes

- **Random Seeds**: Set to 1998 for reproducible results
- **Version Control**: Models saved with timestamps for tracking
- **Backward Compatibility**: New models can be compared with archived versions
- **Performance Monitoring**: Regular retraining recommended as new data becomes available

This training pipeline produces high-accuracy classifiers specifically optimized for the challenging task of identifying fungal endophyte absence in scientific literature, a critical component of the Endo-Review project's systematic review methodology.