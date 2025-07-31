# Data Collection Log

## Overview
This document tracks all data collection activities for the endophyte research systematic review project.

## Data Collection History

### Initial Data Pull
- **Date**: [Original date - to be determined from project history]
- **Search Strategy**: Endophyt* AND (fung* OR mycolog*)
- **Database**: Web of Science Core Collection
- **Results**: [Number of abstracts - to be determined]
- **Status**: Used for initial model training and validation

### Updated Comprehensive Data Pull
- **Date**: July 31, 2025
- **Search Strategy**: 
  ```
  ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi")
  AND
  (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR pteridophyte* OR algae OR green alga* OR macroalga* OR cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
  ```
- **Database**: Web of Science Core Collection
- **Rationale**: 
  - More specific terminology for fungal endophytes
  - Expanded host organism coverage including bryophytes, algae, and lichens
  - Improved precision and recall for comprehensive systematic review
- **Results**: [To be updated after processing]
- **Status**: **Current dataset for all analyses and manuscript**

## Search Strategy Rationale

### Updated Search Terms (July 31, 2025)

**Fungal Endophyte Terms:**
- "fungal endophyte" / "fungal endophytes" - Direct terminology
- "endophytic fungus" / "endophytic fungi" - Alternative phrasing
- Rationale: More precise than broad "endophyt*" which can capture bacterial endophytes

**Host Organism Terms:**
- **Vascular Plants**: plant* (captures plant, plants, Plantae)
- **Bryophytes**: moss*, bryophyte*, liverwort*, hornwort*
- **Pteridophytes**: fern*, lycophyte*, pteridophyte*
- **Algae**: algae, green alga*, macroalga*
- **Symbiotic Systems**: cyanobacteria, cyanobiont*, photobiont*, lichen*

**Advantages of Updated Search:**
1. **Increased Specificity**: Focuses specifically on fungal endophytes
2. **Expanded Host Range**: Includes non-vascular plants and algal systems
3. **Reduced False Positives**: More targeted terminology
4. **Better Recall**: Captures alternative terminology used in the field
5. **Comprehensive Coverage**: Includes emerging research areas (lichen symbionts, algal endophytes)

## Data Processing Pipeline

### Current Workflow (Using July 31, 2025 Data)
1. **Raw Data Import**: Web of Science export files
2. **Preprocessing**: Duplicate removal, field standardization
3. **ML Classification**: Presence/Absence prediction using trained models
4. **Information Extraction**: Species, methods, geography, plant parts
5. **Quality Control**: Manual validation of stratified sample
6. **Analysis**: Temporal trends, geographic patterns, research gaps

### File Locations
- **Raw Data**: `data/raw/` (July 31, 2025 search results)
- **Processed Data**: `data/processed/`
- **ML Results**: `results/predictions/`
- **Final Extractions**: `results/comprehensive_extraction_results.csv`

## Quality Assurance

### Search Validation
- [x] Search terms reviewed for comprehensiveness
- [x] Database coverage confirmed (Web of Science Core Collection)
- [x] Date range verified (all years to July 31, 2025)
- [ ] Manual validation of sample results
- [ ] Comparison with previous search results (if needed)

### Data Processing Validation
- [ ] Duplicate detection and removal
- [ ] Field completeness assessment
- [ ] ML model performance on new data
- [ ] Extraction accuracy validation

## Impact on Analysis

### Changes from Previous Search
1. **Scope Expansion**: Now includes bryophytes, algae, and lichen systems
2. **Terminology Precision**: Focus on fungal (not bacterial) endophytes
3. **Literature Coverage**: More comprehensive capture of recent research
4. **Analysis Framework**: All existing scripts compatible with new data structure

### Manuscript Implications
- **Methods Section**: Updated search strategy documentation
- **Results Section**: Analysis based on comprehensive July 31, 2025 dataset
- **Discussion**: Enhanced coverage of diverse host-endophyte systems
- **Limitations**: Acknowledgment of search strategy evolution

## Notes
- All analysis scripts updated to reference July 31, 2025 data collection
- Previous search results retained for methodological comparison if needed
- ML models trained on previous data remain valid for classification
- Extraction pipelines enhanced to handle expanded organism diversity

---

**Current Status**: All analyses use July 31, 2025 comprehensive search results
**Last Updated**: July 31, 2025
**Next Review**: Upon manuscript completion
