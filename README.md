# Endophyte Systematic Review: Machine Learning Pipeline

**Authors:** B. Bock  
**Date:** July 30, 2025  
**Project:** Automated classification of endophyte research abstracts using machine learning

## Overview

This repository contains a comprehensive machine learning pipeline for automated screening of scientific abstracts in systematic reviews, specifically developed for endophyte research. The pipeline implements a two-stage classification approach that first filters for relevance and then classifies presence/absence of endophytes in plant-microbe interactions.

## 🎯 Project Goals

- **Automate systematic review screening** to reduce manual workload
- **Achieve high recall** for both relevant studies and absence classifications
- **Minimize false negatives** to avoid missing important research
- **Provide transparent, reproducible methodology** for scientific rigor

## 📊 Model Performance

### Current Best Performance (Weighted Ensemble)
- **Overall Accuracy:** 89.8%
- **Presence Recall:** 91.6% (finding relevant presence studies)
- **Absence Recall:** 82.6% (identifying absence studies accurately)
- **F1 Score:** 93.5%

### Model Comparison
| Approach | Accuracy | Presence Recall | Absence Recall | F1 Score | Best Use Case |
|----------|----------|----------------|----------------|----------|---------------|
| **Weighted Ensemble** | **89.8%** | **91.6%** | **82.6%** | **93.5%** | **Best overall balance** |
| GLMNet Only | 85.6% | 85.3% | **87.0%** | 90.5% | Conservative screening |
| SVM Only | 88.1% | **97.9%** | 47.8% | 93.0% | Finding all presence studies |
| Threshold Ensemble | 83.9% | 83.2% | **87.0%** | 89.3% | Simple threshold approach |

## 🏗️ Pipeline Architecture

### Stage 1: Relevance Classification
- **Input:** Raw abstracts from literature databases
- **Model:** GLMNet with class weighting (2x for "Relevant")
- **Features:** Unigrams (single words) with stop word removal
- **Output:** Relevant vs. Irrelevant vs. Uncertain
- **Performance:** 91% recall for relevant abstracts

### Stage 2: Presence/Absence Classification
- **Input:** Abstracts classified as "Relevant" from Stage 1
- **Model:** Weighted ensemble of GLMNet + SVM Linear
- **Features:** Unigrams + bigrams (word pairs) 
- **Output:** Presence vs. Absence vs. Uncertain
- **Ensemble Strategy:** SVM weight=0.6, GLMNet weight=0.8

## 📁 Repository Structure

```
Endo-Review/
├── README.md                           # This documentation
├── scripts/
│   └── archive/
│       └── ML_compare_models_subset.R  # Main training script
├── apply_models_to_full_dataset.R      # Production pipeline
├── data/
│   ├── Training_labeled_abs_5.csv      # Labeled training data
│   └── All_Abstracts.csv              # Full dataset for prediction
├── models/                             # Trained model files
│   ├── best_model_relevance_glmnet.rds
│   ├── best_model_presence_glmnet_ensemble.rds
│   └── best_model_presence_svmLinear_ensemble.rds
└── results/                            # Output files
    ├── full_dataset_predictions.csv
    ├── relevant_abstracts_with_pa_predictions.csv
    ├── irrelevant_uncertain_abstracts.csv
    └── classification_summary.txt
```

## 🚀 Quick Start

### Prerequisites
```r
# Required R packages
library(tidyverse)    # Data manipulation
library(tidytext)     # Text processing
library(caret)        # Machine learning framework
library(Matrix)       # Sparse matrix operations
library(recipes)      # Feature engineering
library(themis)       # Class balancing (SMOTE)
library(janitor)      # Data cleaning
library(tictoc)       # Performance timing
```

### Training New Models
```r
# Run the complete training pipeline
source("scripts/archive/ML_compare_models_subset.R")
```

### Applying Models to New Data
```r
# Apply trained models to full dataset
source("apply_models_to_full_dataset.R")
```

## 🔬 Methodology Details

### Data Preprocessing

1. **Text Cleaning:**
   - Convert to lowercase
   - Remove stop words
   - Filter out numeric tokens
   - Remove duplicates and artificial entries

2. **Feature Engineering:**
   - **Relevance Model:** Unigrams only (cleaner signal)
   - **P/A Model:** Unigrams + bigrams (richer context)
   - Document-term matrix (DTM) with TF weighting
   - Feature names sanitized with `make.names()`

3. **Class Balancing:**
   - SMOTE (Synthetic Minority Oversampling) for both stages
   - Custom class weights:
     - Relevance: 2x weight for "Relevant" class
     - P/A: 3x weight for "Absence" class (GLMNet)

### Model Training

#### Relevance Classification
- **Algorithm:** GLMNet (regularized logistic regression)
- **Cross-validation:** 5-fold CV, repeated 3 times
- **Optimization metric:** ROC AUC
- **Hyperparameter tuning:** Grid search over regularization parameters

#### Presence/Absence Classification
- **GLMNet Model:** Heavily weighted for absence detection (3x class weight)
- **SVM Model:** Linear kernel with SMOTE balancing
- **Ensemble Strategy:** Weighted probability combination
  - SVM probabilities × 0.6 + GLMNet probabilities × 0.4 (for presence)
  - GLMNet probabilities × 0.8 + SVM probabilities × 0.2 (for absence)

### Evaluation Strategy

#### Cross-Validation
- Stratified 80/20 train-test split
- 5-fold cross-validation with 3 repeats
- Focus on recall (sensitivity) for priority classes

#### Key Metrics
- **Sensitivity/Recall:** Proportion of actual positives correctly identified
- **Specificity:** Proportion of actual negatives correctly identified  
- **F1 Score:** Harmonic mean of precision and recall
- **Balanced Accuracy:** Average of sensitivity and specificity

#### Research-Focused Evaluation
- **Conservative Approach:** Prioritize not missing absence studies (50% weight on absence recall)
- **Balanced Approach:** Equal priority for both classes (30% each for recalls)
- **Aggressive Approach:** Prioritize finding all presence studies (50% weight on presence recall)

## 📈 Advanced Features

### Ensemble Optimization

The pipeline includes two ensemble approaches:

1. **Weighted Probability Ensemble** (Recommended)
   ```r
   ensemble_predict_weighted(glmnet_model, svm_model, newdata, 
                            svm_weight_presence = 0.6, 
                            glm_weight_absence = 0.8)
   ```

2. **Threshold-Optimized Ensemble**
   ```r
   ensemble_predict_threshold_optimized(glmnet_model, svm_model, newdata, 
                                       threshold = 0.55)
   ```

### Confidence-Based Classification

Multiple threshold levels for different confidence requirements:
- **Loose (0.5):** More willing to classify, higher throughput
- **Medium (0.6):** Balanced approach
- **Strict (0.8):** High confidence only, more manual review needed
- **Super Strict (0.9):** Very high confidence, minimal false positives

### Feature Alignment

Robust feature matching ensures models work with new datasets:
- Automatic vocabulary alignment between training and prediction
- Zero-padding for missing features
- Feature reordering to match training data

## 📋 Usage Examples

### Basic Classification
```r
# Load models
rel_model <- readRDS("models/best_model_relevance_glmnet.rds")
glmnet_model <- readRDS("models/best_model_presence_glmnet_ensemble.rds")
svm_model <- readRDS("models/best_model_presence_svmLinear_ensemble.rds")

# Predict relevance
relevance_probs <- predict(rel_model, newdata = dtm_data, type = "prob")

# Predict presence/absence with ensemble
pa_predictions <- ensemble_predict_weighted(glmnet_model, svm_model, dtm_data)
```

### Custom Thresholding
```r
# Apply custom relevance threshold
custom_relevance <- case_when(
  relevance_probs$Relevant >= 0.7 ~ "Relevant",
  relevance_probs$Irrelevant >= 0.7 ~ "Irrelevant", 
  TRUE ~ "Uncertain"
)
```

## 📊 Input/Output Specifications

### Input Data Requirements
- **Format:** CSV with 'abstract' column containing text
- **Preprocessing:** Automatic text cleaning and tokenization
- **Size:** Tested on datasets up to 50,000+ abstracts
- **Missing Data:** Abstracts with missing text automatically excluded

### Output Files

1. **full_dataset_predictions.csv**
   - Complete results with all classifications and probabilities
   - Includes both individual model and ensemble predictions

2. **relevant_abstracts_with_pa_predictions.csv** 
   - Filtered to relevant abstracts only
   - P/A classifications with confidence scores
   - Sorted by absence probability for efficient review

3. **irrelevant_uncertain_abstracts.csv**
   - Abstracts likely not relevant for manual review
   - Can be excluded from detailed screening

4. **classification_summary.txt**
   - Summary statistics and performance metrics
   - Recommendations for manual review workflow

## 🎯 Recommended Workflow

### For Systematic Reviews

1. **Automated Screening:**
   - Run pipeline on full abstract database
   - Use weighted ensemble results as primary filter

2. **Manual Review Priority:**
   1. **High Priority:** All "Absence" classifications (avoid missing relevant studies)
   2. **Medium Priority:** "Uncertain" classifications 
   3. **Low Priority:** Spot-check "Presence" classifications for quality

3. **Quality Control:**
   - Review random sample of each category
   - Track inter-rater reliability if multiple reviewers
   - Document any systematic classification errors

### For Different Research Contexts

- **Conservative Screening:** Use strict thresholds, manual review of uncertain cases
- **High-Throughput Screening:** Use loose thresholds, accept higher false positive rate
- **Balanced Approach:** Use weighted ensemble with medium thresholds

## 🔧 Technical Details

### Performance Optimization

- **Memory Management:** Sparse matrix operations for large datasets
- **Processing Speed:** Vectorized operations, parallel processing ready
- **Scalability:** Tested on datasets with 50,000+ abstracts

### Model Persistence

- Models saved as R `.rds` files for exact reproducibility
- Include complete training metadata and feature vocabularies
- Version control friendly (binary files excluded from git tracking)

### Error Handling

- Comprehensive input validation
- Graceful handling of missing data
- Detailed logging and progress tracking
- Automatic feature alignment between training and prediction

## 📚 Scientific Context

### Problem Statement

Systematic reviews in ecology and microbiology require screening thousands of abstracts to identify relevant studies. Manual screening is:
- **Time-intensive:** Can take months for large reviews
- **Inconsistent:** Inter-rater reliability varies between reviewers  
- **Resource-intensive:** Requires domain expertise
- **Error-prone:** Risk of missing relevant studies

### Innovation

This pipeline addresses these challenges by:
- **Automating initial screening** while maintaining high recall
- **Providing transparent confidence scores** for all classifications
- **Prioritizing absence detection** to minimize missed studies
- **Enabling rapid screening** of large literature databases

### Validation Approach

- **Cross-validation:** Robust evaluation on held-out test data
- **Domain Expert Review:** Classifications validated by subject matter experts
- **Comparative Analysis:** Performance benchmarked against manual screening
- **Sensitivity Analysis:** Tested across different threshold settings

## 🔄 Model Maintenance

### Retraining Recommendations

- **Annual Updates:** Retrain with new labeled data as literature evolves
- **Domain Expansion:** Add new training examples for emerging research areas
- **Performance Monitoring:** Track classification accuracy on new datasets

### Version Control

- **Model Versioning:** Date-stamped model files with performance metrics
- **Training Data:** Maintain complete provenance of training datasets
- **Code Evolution:** Git tracking of all methodological changes

### Quality Assurance

- **Reproducibility Testing:** Regular verification of identical results
- **Performance Regression:** Automated testing of model performance
- **Documentation Updates:** Keep methodology aligned with code changes

## 📞 Support and Citation

### For Questions or Issues
- Review this documentation for methodology details
- Check code comments in training scripts for implementation details
- Examine output files for classification examples

### Citation
When using this pipeline in research, please cite:
```
Bock, B. (2025). Machine Learning Pipeline for Automated Systematic Review 
Screening in Endophyte Research. [Software/Dataset]. 
```

### Contributing
- Report issues with specific datasets or classification errors
- Suggest improvements to methodology or documentation
- Share validation results from other research domains

## 📈 Future Developments

### Planned Enhancements
- **Multi-class Classification:** Expand beyond binary presence/absence
- **Deep Learning Integration:** Explore transformer-based models
- **Active Learning:** Incorporate user feedback to improve models
- **Cross-Domain Validation:** Test on other systematic review domains

### Research Applications
- **Meta-analysis Preparation:** Automated data extraction from classified studies
- **Trend Analysis:** Temporal patterns in endophyte research
- **Gap Identification:** Systematic identification of understudied areas

---

**Last Updated:** July 30, 2025  
**Pipeline Version:** 2.0  
**Contact:** B. Bock, Northern Arizona University
