# Processing Workflow Documentation

## Project: Testing the Universality of Fungal Endophytes in Plants
**Complete step-by-step analysis pipeline for reproducibility**

---

## 🔄 **Pipeline Overview**

The analysis pipeline consists of 5 main stages executed sequentially:

```
Raw Literature → Classification → Species Detection → Information Extraction → Analysis
     ↓              ↓               ↓                    ↓                   ↓
Stage 1         Stage 2         Stage 3              Stage 4            Stage 5
```

---

## 📊 **Stage 1: Literature Search and Data Preparation**

### Input
- Web of Science search results (July 31, 2025)
- Search query: [Documented in SEARCH_STRATEGY.md]

### Processing Steps

#### 1.1 Data Import and Cleaning
```r
# File: scripts/01_data_processing/clean_abstracts.R
source("scripts/01_data_processing/clean_abstracts.R")

# Key operations:
# - Import CSV from Web of Science export
# - Standardize column names (janitor::clean_names())
# - Handle encoding issues (UTF-8 conversion)
# - Validate DOI formats
# - Remove exact duplicates
# - Handle missing abstracts/titles
```

**Output**: `data/raw/All_abstracts.csv` (cleaned dataset)

#### 1.2 Training Data Preparation
```r
# File: data/raw/Training_labeled_abs_6.csv
# Manual labeling completed by B. Bock
# Labels: "Presence", "Absence", "Irrelevant"
# Quality control: 10% double-labeled for consistency check
```

### Quality Control Checks
- [x] Duplicate detection and removal
- [x] DOI format validation  
- [x] Year range validation (1900-2025)
- [x] Text encoding standardization
- [ ] Manual verification of edge cases

---

## 🤖 **Stage 2: Machine Learning Classification**

### Objective
Classify abstracts into Presence/Absence/Irrelevant categories with high accuracy

### Processing Steps

#### 2.1 Feature Engineering
```r
# File: scripts/02_model_training/ML_compare_models_subset_optimized.R

# Text preprocessing pipeline:
prepare_text_features <- function(text_data) {
  # 1. Text cleaning
  text_cleaned <- text_data %>%
    str_to_lower() %>%                      # Lowercase conversion
    str_replace_all("[^a-zA-Z0-9\\s]", " ") %>%  # Remove special chars
    str_replace_all("\\s+", " ") %>%        # Normalize whitespace
    str_trim()                              # Trim edges
  
  # 2. TF-IDF vectorization
  create_dtm(
    text = text_cleaned,
    method = "tfidf",
    ngram_max = 2,        # Unigrams + bigrams
    max_features = 5000,  # Top 5000 features
    min_doc_freq = 5,     # Minimum document frequency
    max_doc_freq = 0.95   # Maximum document frequency
  )
}
```

#### 2.2 Model Training and Selection
```r
# Cross-validation approach
cv_control <- trainControl(
  method = "cv",
  number = 10,           # 10-fold CV
  summaryFunction = multiClassSummary,
  classProbs = TRUE,
  savePredictions = "final"
)

# Models trained:
models <- list(
  glmnet = train(x = features, y = labels, method = "glmnet", trControl = cv_control),
  svm    = train(x = features, y = labels, method = "svmLinear", trControl = cv_control)
)
```

#### 2.3 Ensemble Model Creation
```r
# Weighted ensemble (optimal performance)
create_weighted_ensemble <- function(glmnet_pred, svm_pred) {
  # Optimal weights determined via grid search
  glmnet_weight <- 0.8
  svm_weight <- 0.6
  
  # Weighted probability combination
  ensemble_prob <- (glmnet_pred * glmnet_weight + svm_pred * svm_weight) / 
                   (glmnet_weight + svm_weight)
  
  return(ensemble_prob)
}
```

### Model Performance (Validation Set)
- **Overall Accuracy**: 89.8%
- **Presence Recall**: 91.6%
- **Absence Recall**: 82.6%
- **F1 Score**: 93.5%

### Output Files
- `models/best_model_glmnet.rds` - Trained GLMNet model
- `models/best_model_svm.rds` - Trained SVM model  
- `results/model_evaluation_metrics.csv` - Performance statistics

---

## 🔍 **Stage 3: Full Dataset Classification**

### Processing Steps

#### 3.1 Apply Trained Models
```r
# File: scripts/03_prediction/apply_models_to_full_dataset.R

# Load trained models and apply to full dataset
source("scripts/03_prediction/apply_models_to_full_dataset.R")

# Process:
# 1. Load all abstracts (All_abstracts.csv)
# 2. Apply text preprocessing pipeline
# 3. Generate predictions with confidence scores
# 4. Apply ensemble weighting
# 5. Assign final classifications with thresholds
```

#### 3.2 Classification Thresholds
```r
# Conservative thresholds to minimize false negatives
thresholds <- list(
  presence_threshold = 0.3,    # Lower threshold for high recall
  absence_threshold = 0.7,     # Higher threshold for precision
  uncertain_range = c(0.3, 0.7) # Uncertain classification zone
)
```

### Output Files
- `results/relevant_abstracts_with_pa_predictions.csv` - All predictions
- `results/classification_summary.txt` - Summary statistics
- `results/high_confidence_presence.csv` - High-confidence presence studies
- `results/high_confidence_absence.csv` - High-confidence absence studies

---

## 🧬 **Stage 4: Species Detection and Information Extraction**

### Objective
Extract comprehensive information from classified abstracts

### Processing Steps

#### 4.1 Species Detection Pipeline
```r
# File: scripts/04_analysis/extract_species_simple.R (OPTIMIZED VERSION)

# Main extraction function
run_comprehensive_extraction <- function() {
  
  # Step 1: Load classification results
  abstracts <- read_csv("results/relevant_abstracts_with_pa_predictions.csv")
  
  # Step 2: Species detection (parallel processing)
  species_results <- detect_species_parallel(
    abstracts = abstracts,
    batch_size = 100,      # Optimized batch size
    workers = 6            # Parallel workers
  )
  
  # Step 3: Additional information extraction (vectorized)
  additional_info <- extract_additional_info_batch(
    abstracts = abstracts,
    batch_size = 1000      # Large batches for vectorized processing
  )
  
  # Step 4: Combine results
  comprehensive_results <- combine_extraction_results(
    species_results, 
    additional_info, 
    abstracts
  )
  
  return(comprehensive_results)
}
```

#### 4.2 Information Categories Extracted

##### 4.2.1 Species Information
```r
# Plant and fungal species detection
species_detection_categories <- list(
  plant_species = "Host plant identification",
  fungal_species = "Endophyte species identification", 
  taxonomic_level = "Identification precision (species/genus/family)",
  kingdom = "Plant vs fungal classification"
)
```

##### 4.2.2 Plant Parts Studied
```r
# Vectorized plant parts detection
plant_parts_keywords <- c(
  "root", "roots", "leaf", "leaves", "stem", "stems", 
  "shoot", "shoots", "flower", "flowers", "seed", "seeds",
  "bark", "wood", "needle", "needles", "rhizome", "tuber"
)
```

##### 4.2.3 Research Methods
```r
# Research methodology detection
method_categories <- list(
  molecular = c("dna", "rna", "pcr", "sequencing", "its", "18s", "ribosomal"),
  culture = c("cultur", "isolat", "media", "agar", "petri", "incubat"),
  microscopy = c("microscop", "microtom", "stain", "histolog", "light microscop")
)
```

##### 4.2.4 Geographic Information
```r
# Geographic detection with special handling
geographic_categories <- list(
  countries = "Country-level location detection",
  continents = "Continental classification", 
  regions = "Biogeographic regions and ecosystems",
  coordinates = "Latitude/longitude coordinates",
  global_classification = "Global North vs Global South"
)
```

### Performance Optimizations
- **Vectorized processing**: Process 1000+ abstracts simultaneously
- **Parallel species detection**: Multi-core processing for taxonomic matching
- **Memory management**: Batch processing to handle large datasets
- **Progress reporting**: Real-time progress and results summary

### Output Files
- `results/species_detection_weighted_ensemble.csv` - Species detection results
- `results/comprehensive_extraction_results.csv` - Complete extraction
- `results/comprehensive_extraction_report.txt` - Analysis summary

---

## 📈 **Stage 5: Statistical Analysis and Reporting**

### Analysis Categories

#### 5.1 Descriptive Analysis
```r
# Key summary statistics
descriptive_analysis <- function(data) {
  list(
    total_studies = nrow(data),
    presence_studies = sum(data$prediction == "Presence"),
    absence_studies = sum(data$prediction == "Absence"),
    species_detected = length(unique(data$species_name[!is.na(data$species_name)])),
    countries_represented = length(unique(data$country[!is.na(data$country)])),
    years_covered = range(data$publication_year, na.rm = TRUE),
    methods_coverage = table(data$research_methods)
  )
}
```

#### 5.2 Bias Analysis
```r
# Geographic bias assessment  
geographic_bias_analysis <- function(data) {
  # Global North vs South representation
  north_south_comparison <- data %>%
    group_by(global_region, prediction) %>%
    summarise(count = n(), .groups = "drop")
  
  # Country-level research intensity
  country_research_intensity <- data %>%
    count(country, sort = TRUE)
    
  # Biodiversity vs research effort correlation
  biodiversity_research_correlation()
}

# Taxonomic bias assessment
taxonomic_bias_analysis <- function(data) {
  # Family-level representation
  family_representation <- analyze_family_coverage(data)
  
  # Phylogenetic sampling bias
  phylogenetic_bias_assessment(data)
}

# Temporal bias assessment
temporal_bias_analysis <- function(data) {
  # Research trends over time
  temporal_trends <- data %>%
    group_by(publication_year, prediction) %>%
    summarise(count = n(), .groups = "drop")
    
  # Method evolution over time
  method_temporal_trends(data)
}
```

#### 5.3 Citation Analysis Integration
```r
# Citation impact assessment (Framework in CITATION_ANALYSIS_FRAMEWORK.md)
citation_analysis <- function(data) {
  # Extract DOIs for citation lookup
  dois <- data$doi[!is.na(data$doi)]
  
  # Get citation metrics from multiple sources
  citation_metrics <- get_citation_data(dois)
  
  # Analyze citation patterns by study type
  citation_bias_analysis(citation_metrics, data)
}
```

### Statistical Testing Framework
```r
# Hypothesis testing for key claims
statistical_tests <- list(
  # H1: Absence studies are under-represented
  absence_underrepresentation = prop.test(
    x = c(absence_count, presence_count),
    n = c(total_studies, total_studies)
  ),
  
  # H2: Geographic bias exists
  geographic_bias_test = chisq.test(geographic_contingency_table),
  
  # H3: Taxonomic sampling is biased
  taxonomic_bias_test = compare_expected_vs_observed_families(),
  
  # H4: Citation bias affects absence studies
  citation_bias_test = wilcox.test(
    citations_absence_studies, 
    citations_presence_studies
  )
)
```

---

## 🔧 **Technical Implementation Details**

### Computing Environment
```r
# Required R packages
required_packages <- c(
  # Data manipulation
  "tidyverse", "janitor", "stringr",
  
  # Machine learning  
  "caret", "glmnet", "e1071",
  
  # Parallel processing
  "furrr", "future", "parallel",
  
  # Species detection
  "rgbif", "taxize",
  
  # Statistical analysis
  "broom", "car", "effsize",
  
  # Visualization
  "ggplot2", "plotly", "ggmap",
  
  # Reproducibility
  "here", "renv", "knitr"
)
```

### Memory and Performance Requirements
- **RAM**: Minimum 16GB (32GB recommended for full dataset)
- **Storage**: ~10GB for full pipeline with intermediate files
- **CPU**: Multi-core processor recommended (6+ cores optimal)
- **Runtime**: ~4-6 hours for complete pipeline execution

### Error Handling and Recovery
```r
# Checkpoint system for long-running processes
create_checkpoint <- function(data, stage_name) {
  checkpoint_file <- paste0("results/checkpoint_", stage_name, "_", Sys.Date(), ".rds")
  saveRDS(data, checkpoint_file)
  cat("Checkpoint saved:", checkpoint_file, "\n")
}

# Recovery from checkpoints
recover_from_checkpoint <- function(stage_name) {
  checkpoint_pattern <- paste0("results/checkpoint_", stage_name, "_")
  checkpoint_files <- list.files("results", pattern = checkpoint_pattern, full.names = TRUE)
  
  if (length(checkpoint_files) > 0) {
    latest_checkpoint <- checkpoint_files[which.max(file.mtime(checkpoint_files))]
    cat("Recovering from checkpoint:", latest_checkpoint, "\n")
    return(readRDS(latest_checkpoint))
  } else {
    return(NULL)
  }
}
```

---

## 📋 **Quality Control and Validation**

### Automated Quality Checks
```r
# Data validation pipeline
validate_pipeline_stage <- function(data, stage_name) {
  validation_results <- list(
    # Basic data integrity
    no_missing_ids = all(!is.na(data$id)),
    no_duplicate_ids = length(unique(data$id)) == nrow(data),
    valid_years = all(data$publication_year >= 1900 & data$publication_year <= 2025, na.rm = TRUE),
    
    # Stage-specific validations
    stage_specific_checks = switch(stage_name,
      "classification" = validate_classification_stage(data),
      "species_detection" = validate_species_detection_stage(data),
      "information_extraction" = validate_extraction_stage(data)
    )
  )
  
  # Log validation results
  log_validation_results(validation_results, stage_name)
  
  return(all(unlist(validation_results)))
}
```

### Manual Validation Protocol
1. **Sample Selection**: Random stratified sampling (100 per category)
2. **Independent Review**: Secondary reviewer for subset validation
3. **Discrepancy Resolution**: Consensus procedure for disagreements
4. **Documentation**: Complete validation log with decisions

---

## 🔄 **Reproducibility Framework**

### Version Control and Documentation
- **Git Repository**: Complete version history
- **Zenodo DOI**: Permanent archive upon publication
- **Container**: Docker image with complete environment
- **Documentation**: This workflow + inline code comments

### Data Sharing Compliance
- **Public Components**: Search strategies, code, processed results
- **Restricted Components**: Raw abstracts (copyright), DOI lists available upon request
- **Reference Data**: GBIF-derived species lists (fully open)

### Computational Reproducibility
```r
# Complete environment capture
renv::snapshot()  # Package versions
sessionInfo()     # R version and system info
here::here()      # Path management
```

---

**Last Updated**: August 26, 2025  
**Pipeline Status**: Stage 4 in progress (Species detection)  
**Next Milestone**: Complete Stage 4, begin Stage 5 analysis  
**Estimated Completion**: September 5, 2025
