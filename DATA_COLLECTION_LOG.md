# Data Collection Log

## Project: Testing the Universality of Fungal Endophytes in Plants
**Date Range**: November 2024 - August 2025

## Search History and Data Sources

### Final Comprehensive Search (July 31, 2025)
**Status**: CURRENT DATASET - Used for all analyses

**Database**: Web of Science Core Collection
**Search String**: 
```
("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi")
AND 
(plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR 
 lycophyte* OR pteridophyte* OR algae OR green alga* OR macroalga* OR 
 cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
```

**Search Results**:
- **Total Records Retrieved**: [TO BE FILLED DURING PROCESSING]
- **Date Range**: All years to July 31, 2025
- **Languages**: English
- **Document Types**: Articles, Conference Proceedings, Reviews

**Files Generated**:
- `data/raw/All_abstracts.csv` - Complete bibliographic data
- Fields: Title, Abstract, Authors, Journal, Year, DOI, Keywords

### Data Quality Control

#### Duplicate Removal
- **Method**: DOI-based deduplication + title similarity matching
- **Duplicates Removed**: [TO BE DOCUMENTED]
- **Final Unique Records**: [TO BE DOCUMENTED]

#### Data Completeness Check
- **Records with abstracts**: [TO BE DOCUMENTED]
- **Records with DOIs**: [TO BE DOCUMENTED]
- **Records with complete metadata**: [TO BE DOCUMENTED]

### Training Data Collection

#### Manual Labeling Process (Updated August 2025)
**Training File**: `data/raw/Training_labeled_abs_6.csv`

**Sample Selection**:
- **Method**: Random stratified sampling across years and journals
- **Total Labeled**: [TO BE DOCUMENTED FROM FILE]
- **Label Distribution**:
  - Presence: [COUNT] abstracts
  - Absence: [COUNT] abstracts
  - Uncertain: [COUNT] abstracts

**Quality Control**:
- **Labeler**: B. Bock (Primary researcher)
- **Guidelines**: Documented in METHODS.md
- **Consistency Check**: Double-labeling on 10% subset
- **Inter-rater Agreement**: κ = [TO BE CALCULATED]

### Reference Data Sources

#### Species Taxonomic Database
- **Source**: GBIF Backbone Taxonomy
- **File**: `models/species.rds`
- **Coverage**: [NUMBER] plant species, [NUMBER] fungal species
- **Last Updated**: [DATE]

#### Geographic Reference Data
- **Countries**: ISO standard country list + regional classifications
- **Regions**: Biogeographic realms, biomes, hotspots
- **Source**: Centralized in `scripts/04_analysis/reference_data_utils.R`

## Data Processing Pipeline

### Step 1: Raw Data Import
- ✅ **File**: All_abstracts.csv imported
- ✅ **Cleaning**: Text standardization, encoding fixes
- ✅ **Validation**: DOI format checks, year range validation

### Step 2: Machine Learning Classification
- ✅ **Training Set**: 6th iteration training data used
- ✅ **Model Performance**: Weighted ensemble (89.8% accuracy)
- ✅ **Classification**: Applied to full dataset
- ✅ **Output**: `results/relevant_abstracts_with_pa_predictions.csv`

### Step 3: Information Extraction
- 🔄 **Current**: Running species detection and comprehensive extraction
- 📋 **Target Output**: `results/comprehensive_extraction_results.csv`
- 📋 **Expected Completion**: August 26, 2025

## Data Validation and Quality Assurance

### Automated Checks
- ✅ **DOI Validation**: Format and uniqueness
- ✅ **Year Range**: 1900-2025 bounds checking
- ✅ **Text Quality**: Encoding and length validation
- ✅ **Classification Confidence**: Prediction probability distributions

### Manual Validation (Planned)
- 📋 **Sample Size**: 100 abstracts per prediction category
- 📋 **Validator**: Independent reviewer
- 📋 **Metrics**: Precision, recall, F1 per category
- 📋 **Documentation**: `results/validation_sample_for_manual_review.csv`

## Reproducibility Documentation

### Data Provenance
- **Search Date**: July 31, 2025
- **Database Version**: Web of Science Core Collection (current)
- **Export Format**: Plain text CSV with full bibliographic fields
- **Processing Software**: R 4.3+, tidyverse ecosystem

### Version Control
- **Repository**: GitHub (private during review)
- **Key Snapshots**: 
  - Initial search (Nov 2024)
  - Training data v6 (Aug 2025)
  - Final classification (Aug 2025)

### Computational Environment
- **Platform**: Windows 11, R 4.3+
- **Key Packages**: tidyverse, caret, glmnet, e1071, furrr
- **Hardware**: Multi-core processing for species detection
- **Memory**: Batch processing for large dataset handling

## Data Sharing and Archival

### Open Data Components (Post-Publication)
- ✅ **Search Strategy**: Fully documented and reproducible
- ✅ **Classification Pipeline**: All code on GitHub
- ✅ **Reference Data**: GBIF-derived species lists
- ❌ **Raw Abstracts**: Copyright restrictions (Web of Science)

### Restricted Access Components
- **Full Abstract Text**: Available upon reasonable request
- **DOI Lists**: Can be shared for replication
- **Supplementary Files**: Will accompany publication

---

**Last Updated**: August 26, 2025  
**Next Review**: Before manuscript submission  
**Responsible**: B. Bock
