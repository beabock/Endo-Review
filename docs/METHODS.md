# Methods Documentation for Endophyte Research Systematic Review

## Overview

This document provides comprehensive methodology for the machine learning-assisted systematic review of endophytic fungi research literature. The approach combines automated classification with comprehensive information extraction to analyze research patterns, geographic distribution, and methodological trends.

## 1. Literature Search and Data Collection

### 1.1 Search Strategy
- **Database**: Web of Science Core Collection
- **Search Terms**: 
  - Updated search (July 31, 2025): ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi") AND (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR pteridophyte* OR algae OR green alga* OR macroalga* OR cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
  - Previous search: Endophyt* AND (fung* OR mycolog*)
- **Date Range**: All years to present (search conducted July 31, 2025)
- **Publication Types**: Journal articles, conference proceedings
- **Language**: English publications

### 1.2 Initial Screening
- **Inclusion Criteria**:
  - Studies investigating endophytic fungi in plants
  - Research on fungal endophytes in any plant tissue
  - Culture-based, molecular, or microscopy approaches
  - Field studies, laboratory experiments, or reviews

- **Exclusion Criteria**:
  - Plant pathology studies without endophyte focus
  - Bacterial endophyte studies (unless also including fungi)
  - Purely methodological papers without biological context
  - Duplicate publications

### 1.3 Data Export and Preprocessing
- Bibliographic data exported in BibTeX/CSV format
- Fields extracted: Title, Abstract, Authors, Journal, Year, DOI
- Data cleaning: Removal of duplicates, standardization of fields
- Quality control: Manual review of problematic entries

## 2. Machine Learning Classification Pipeline

### 2.1 Training Data Preparation

#### Manual Labeling Protocol:
- **Sample Size**: 1,000+ abstracts randomly selected
- **Labels**: 
  - "Presence": Clear endophytic fungi research
  - "Absence": Not endophytic fungi research
  - "Irrelevant": Insufficient information or unclear

#### Labeling Guidelines:
- **Presence Indicators**: 
  - Explicit mention of "endophyte" or "endophytic"
  - Discussion of fungi living inside plant tissues
  - Internal fungal communities or microbiomes
  - Non-pathogenic plant-fungal associations

- **Absence Indicators**:
  - Plant pathology focus
  - Soil fungi without plant association
  - Bacterial microbiome studies
  - Non-biological topics

#### Quality Control:
- Inter-rater reliability assessment (κ > 0.8)
- Consensus labeling for uncertain cases
- Balanced representation across years and journals

### 2.2 Feature Engineering

#### Text Preprocessing:
```r
# Text cleaning pipeline
clean_text <- function(text) {
  text %>%
    tolower() %>%                          # Convert to lowercase
    str_replace_all("[^a-z0-9\\s]", " ") %>%  # Remove special characters
    str_replace_all("\\s+", " ") %>%        # Normalize whitespace
    str_trim()                             # Remove leading/trailing whitespace
}
```

#### Feature Extraction:
- **TF-IDF Vectorization**: Term frequency-inverse document frequency
  - Max features: 5,000 terms
  - N-gram range: 1-2 (unigrams and bigrams)
  - Min document frequency: 5
  - Max document frequency: 0.95

- **Document-Term Matrix**: Sparse matrix representation
- **Feature Selection**: Chi-squared test for feature importance

### 2.3 Model Training and Selection

#### Algorithms Evaluated:
1. **Elastic Net (glmnet)**
   - Regularization parameters: α ∈ {0.1, 0.5, 0.9}
   - Lambda: Cross-validation optimized

2. **Support Vector Machine (svmLinear)**
   - Cost parameter: Grid search {0.1, 1, 10}
   - Linear kernel for interpretability

3. **Random Forest (ranger)**
   - Trees: 500
   - mtry: √(number of features)
   - Min node size: Cross-validation optimized

4. **Extreme Gradient Boosting (xgbTree)**
   - Learning rate: 0.1
   - Max depth: 6
   - Subsample: 0.8

#### Training Protocol:
- **Cross-Validation**: 10-fold stratified CV
- **Performance Metrics**: 
  - Accuracy
  - Precision/Recall
  - F1-score
  - Area Under ROC Curve (AUC)

- **Model Selection**: Best performing on validation set
- **Ensemble Approach**: Weighted combination of top models

### 2.4 Model Validation

#### Validation Strategy:
- **Hold-out Test Set**: 20% of labeled data
- **Stratified Sampling**: Maintains class distribution
- **Temporal Validation**: Recent publications for external validation

#### Performance Assessment:
- **Confusion Matrix Analysis**
- **Precision-Recall Curves**
- **Feature Importance Analysis**
- **Error Analysis**: Systematic review of misclassifications

## 3. Comprehensive Information Extraction

### 3.1 Species Detection Pipeline

#### Reference Database:
- **Plant Species**: Global Biodiversity Information Facility (GBIF)
- **Fungal Species**: Index Fungorum, MycoBank
- **Taxonomic Resolution**: Species, genus, family levels
- **Synonyms**: Alternative nomenclature included

#### Detection Algorithm:
```r
# Species detection with fuzzy matching
detect_species <- function(text, species_db) {
  # Exact matching
  exact_matches <- str_extract_all(text, species_patterns)
  
  # Fuzzy matching for partial names
  fuzzy_matches <- agrep(species_db$names, text, 
                        max.distance = 0.1, value = TRUE)
  
  # Validate biological context
  validated_species <- validate_context(matches, text)
  
  return(validated_species)
}
```

#### Validation Criteria:
- **Context Filtering**: Species names in biological context only
- **False Positive Reduction**: Exclude author names, locations
- **Taxonomic Verification**: Cross-reference with accepted names

### 3.2 Plant Parts Analysis

#### Comprehensive Keyword List:
- **Basic Structures**: roots, stems, leaves, flowers, fruits, seeds
- **Reproductive Parts**: pistils, stamens, ovaries, pollen
- **Specialized Structures**: rhizomes, tubers, bulbs, tendrils
- **Anatomical Features**: xylem, phloem, cortex, epidermis
- **Pathological Structures**: galls, cankers, tumors, hyperplasia
- **Surface Features**: trichomes, cuticles, lenticels

#### Detection Method:
- **Exact String Matching**: Case-insensitive
- **Word Boundary Detection**: Avoid partial matches
- **Frequency Analysis**: Count occurrences per abstract
- **Deduplication**: Remove redundant detections

### 3.3 Research Methods Classification

#### Method Categories:
1. **Molecular Methods**:
   - Keywords: PCR, DNA, RNA, sequencing, primers, phylogeny
   - Indicators: Genetic analysis, molecular identification

2. **Culture-Based Methods**:
   - Keywords: culture, isolation, medium, agar, incubation
   - Indicators: Fungal cultivation, pure cultures

3. **Microscopy Methods**:
   - Keywords: microscopy, SEM, TEM, histology, staining
   - Indicators: Morphological analysis, imaging

#### Classification Algorithm:
```r
detect_methods <- function(text) {
  method_scores <- sapply(method_categories, function(keywords) {
    matches <- str_detect(tolower(text), keywords)
    sum(matches) / length(keywords)  # Normalized score
  })
  
  return(method_scores > threshold)
}
```

### 3.4 Geographic Information Extraction

#### Geographic Entity Recognition:
- **Countries**: Complete list of world countries and territories
- **Development Classification**: Global North/South categorization
- **Regions**: Continents, biomes, ecosystems
- **Coordinates**: Latitude/longitude pattern detection

#### Global North/South Classification:
- **Global North**: OECD countries + high-income nations
- **Global South**: Developing and emerging economies
- **Mixed Studies**: Multi-country research spanning both categories

#### Validation Protocol:
- **Ambiguity Resolution**: Manual review of uncertain cases
- **Coordinate Validation**: Geographic plausibility checks
- **Regional Consistency**: Cross-reference country-continent assignments

## 4. Quality Control and Validation

### 4.1 Automated Quality Checks

#### Data Integrity:
- **Missing Value Analysis**: Systematic pattern detection
- **Outlier Detection**: Statistical analysis of extracted features
- **Consistency Checks**: Cross-field validation
- **Duplicate Detection**: Fuzzy matching for near-duplicates

#### Extraction Validation:
- **Species Name Verification**: Taxonomic database cross-reference
- **Geographic Consistency**: Country-continent alignment
- **Method Logic Checks**: Impossible method combinations

### 4.2 Manual Validation Protocol

#### Stratified Sampling:
- **Sample Size**: ~200 abstracts (target 95% CI ±7%)
- **Stratification Variables**:
  - ML prediction confidence (high/medium/low)
  - Prediction class (presence/absence)
  - Information completeness (high/low)
  - Geographic representation

#### Validation Procedure:
1. **Independent Review**: Trained reviewers assess sample
2. **Consensus Building**: Resolve disagreements through discussion
3. **Error Pattern Analysis**: Systematic bias identification
4. **Accuracy Metrics**: Calculate precision, recall, F1-score
5. **Calibration Assessment**: Confidence vs accuracy relationship

### 4.3 Reproducibility Framework

#### Code Documentation:
- **Version Control**: Git repository with tagged releases
- **Dependency Management**: R package versions recorded
- **Containerization**: Docker images for computational environment
- **Testing Suite**: Automated tests for key functions

#### Data Provenance:
- **Processing Logs**: Complete audit trail of data transformations
- **Parameter Documentation**: All hyperparameters and thresholds recorded
- **Random Seed Management**: Reproducible random number generation
- **Intermediate Results**: Preservation of key processing stages

## 5. Statistical Analysis Framework

### 5.1 Descriptive Statistics

#### Distribution Analysis:
- **Temporal Patterns**: Publication frequency over time
- **Geographic Distribution**: Study locations and concentrations
- **Method Usage**: Frequency and combinations of research approaches
- **Species Diversity**: Taxonomic representation and coverage

#### Summary Metrics:
- **Central Tendencies**: Means, medians for continuous variables
- **Variability Measures**: Standard deviations, interquartile ranges
- **Categorical Summaries**: Frequencies and proportions
- **Missing Data Patterns**: Systematic analysis of data gaps

### 5.2 Comparative Analysis

#### Group Comparisons:
- **Geographic Equity**: Global North vs South research characteristics
- **Temporal Trends**: Method adoption and geographic expansion over time
- **Quality Assessment**: Information completeness by region/time
- **Method Sophistication**: Technology access and usage patterns

#### Statistical Tests:
- **Chi-square Tests**: Categorical variable associations
- **T-tests/ANOVA**: Continuous variable group differences
- **Trend Analysis**: Time series analysis for temporal patterns
- **Effect Size Calculation**: Practical significance assessment

### 5.3 Bias Detection and Assessment

#### Geographic Bias Analysis:
- **Research Intensity Mapping**: Studies per unit area/biodiversity
- **Accessibility Factors**: Correlation with infrastructure, political stability
- **Capacity Indicators**: Method sophistication by economic development
- **Hotspot Coverage**: Research in biodiversity priority areas

#### Temporal Bias Assessment:
- **Publication Lag**: Time from research to publication
- **Method Adoption Rates**: Technology diffusion patterns
- **Language Bias**: Non-English publication representation
- **Journal Impact**: Venue prestige and geographic representation

## 6. Visualization and Reporting

### 6.1 Figure Creation Protocol

#### Core Manuscript Figures:
1. **PRISMA Flow Diagram**: Study selection and classification process
2. **Global Distribution Map**: Research locations with method overlays
3. **Temporal Trends**: Publication volume and method adoption over time
4. **Species Analysis**: Taxonomic diversity and geographic patterns
5. **Quality Assessment**: ML performance and validation results

#### Technical Specifications:
- **Resolution**: 300 DPI minimum for publication
- **Color Schemes**: Colorblind-friendly palettes
- **Font Consistency**: Uniform typography across figures
- **Size Standards**: Journal-specific dimension requirements

### 6.2 Report Generation

#### Automated Reports:
- **Processing Summaries**: Key statistics and quality metrics
- **Extraction Results**: Comprehensive findings tables
- **Validation Outcomes**: Accuracy and bias assessment
- **Temporal Analysis**: Trend identification and significance testing

#### Documentation Standards:
- **Methodology Details**: Step-by-step processing documentation
- **Parameter Settings**: Complete configuration recording
- **Data Dictionary**: Variable definitions and coding schemes
- **Limitation Discussion**: Known issues and constraints

## 7. Ethical Considerations and Best Practices

### 7.1 Data Usage Ethics

#### Copyright Compliance:
- **Fair Use Principles**: Systematic review exemptions
- **Attribution Requirements**: Proper citation of source databases
- **Access Restrictions**: Respect for paywall and subscription barriers
- **Data Sharing**: Balance openness with publisher agreements

### 7.2 Bias Mitigation

#### Geographic Representation:
- **Inclusive Search**: Multiple databases and languages where possible
- **Southern Scholarship**: Recognition of non-English publications
- **Capacity Building**: Collaborative partnerships for validation
- **Equitable Citation**: Balanced geographic representation in references

### 7.3 Open Science Practices

#### Data Availability:
- **Processed Datasets**: Public repository deposition
- **Code Sharing**: GitHub repository with documentation
- **Methodology Transparency**: Complete protocol publication
- **Reproducibility Materials**: Containerized computational environments

## 8. Software and Computational Environment

### 8.1 Primary Software

#### R Environment:
- **Version**: R 4.3.0 or later
- **Key Packages**:
  - `tidyverse` (data manipulation)
  - `caret` (machine learning)
  - `glmnet` (elastic net regression)
  - `randomForest` (ensemble methods)
  - `xgboost` (gradient boosting)
  - `tm` (text mining)
  - `maps` (geographic visualization)

### 8.2 Computational Resources

#### Hardware Requirements:
- **Memory**: 16GB RAM minimum (32GB recommended)
- **Storage**: 100GB available space for intermediate files
- **Processing**: Multi-core CPU for parallel processing
- **Graphics**: Dedicated GPU optional for deep learning extensions

#### Performance Optimization:
- **Parallel Processing**: Multi-core utilization for large datasets
- **Memory Management**: Efficient handling of sparse matrices
- **Batch Processing**: Chunked analysis for memory constraints
- **Caching Strategy**: Intermediate result preservation

## 9. Limitations and Future Directions

### 9.1 Current Limitations

#### Search Strategy:
- **Database Coverage**: Limited to Web of Science
- **Language Bias**: English-language focus
- **Gray Literature**: Minimal inclusion of reports, theses
- **Temporal Coverage**: Emphasis on recent publications

#### Method Constraints:
- **Text-Based Analysis**: Limited to abstract information
- **Automated Extraction**: Potential for false positives/negatives
- **Taxonomic Resolution**: Species-level identification challenges
- **Geographic Precision**: Country-level rather than coordinate-based

### 9.2 Future Enhancements

#### Methodological Improvements:
- **Deep Learning**: Transformer models for better text understanding
- **Multi-Language Support**: Non-English literature inclusion
- **Full-Text Analysis**: Extension beyond abstracts
- **Expert Integration**: Hybrid human-AI validation systems

#### Scope Expansion:
- **Multiple Databases**: Scopus, PubMed, regional databases
- **Broader Taxonomy**: All microbial endophytes
- **Longitudinal Analysis**: Research impact and citation patterns
- **Policy Integration**: Connection to conservation and management

---

*This methodology document provides the foundation for reproducible, systematic analysis of endophyte research literature. Regular updates reflect methodological improvements and validation outcomes.*
