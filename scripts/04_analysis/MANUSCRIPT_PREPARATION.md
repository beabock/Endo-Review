# Manuscript Preparation Recommendations

## Overview
This document provides comprehensive recommendations for preparing the endophyte research systematic review manuscript based on the machine learning pipeline and extraction results.

## Current Status ✅
- [x] Updated comprehensive literature search conducted (July 31, 2025)
- [x] New search strategy implemented with expanded host organism coverage
- [x] Machine learning models trained and validated (89.8% accuracy)
- [x] Comprehensive species extraction pipeline implemented
- [x] Geographic, methods, and plant parts analysis completed
- [x] Workspace organized with clear structure
- [x] Visualization framework established
- [x] Duplicate keyword issues resolved

## Data Collection Update (July 31, 2025)
**New Comprehensive Search Strategy:**
- **Previous**: Endophyt* AND (fung* OR mycolog*)
- **Updated**: ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi") AND (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR pteridophyte* OR algae OR green alga* OR macroalga* OR cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
- **Rationale**: More precise fungal endophyte focus + expanded host organism coverage including bryophytes, algae, and lichen symbionts
- **Impact**: Enhanced literature coverage and reduced false positives

## Next Steps for Manuscript Preparation

### 1. Manual Validation and Quality Control 🔍

#### Priority Actions:
- **Run manual validation script**: `manual_validation_sample.R`
  - Creates stratified sample of ~200 abstracts for human review
  - Assesses ML model accuracy across confidence levels
  - Identifies systematic biases or errors

#### Implementation:
```r
# Execute from scripts/04_analysis/
source("manual_validation_sample.R")
```

#### Deliverables:
- Validation sample with instructions
- Model accuracy metrics by confidence level
- Error pattern analysis
- Bias detection results

### 2. Temporal and Geographic Analysis 📊

#### Scripts to Run:
1. **Temporal trends**: `temporal_trend_analysis.R`
   - Publication volume evolution
   - Research method adoption over time
   - Geographic research patterns by decade
   - Species detection trends

2. **Geographic bias analysis**: `geographic_bias_analysis.R`
   - Global North/South research equity
   - Understudied biodiversity hotspots
   - Research capacity disparities
   - Ecosystem coverage gaps

#### Expected Insights:
- Field development trajectory
- Methodological evolution
- Research concentration vs biodiversity
- Priority regions for future study

### 3. Core Manuscript Figures 📈

#### Essential Figures to Create:
1. **PRISMA Flow Diagram**
   - Literature search and screening process
   - ML classification results
   - Final included studies

2. **Global Research Distribution Map**
   - Study locations color-coded by method type
   - Biodiversity hotspot overlay
   - Global North/South representation

3. **Temporal Research Trends**
   - Publication volume over time
   - Method adoption curves
   - Geographic expansion patterns

4. **Species and Methods Analysis**
   - Most studied plant families
   - Research method combinations
   - Plant parts frequency analysis

5. **Research Quality Assessment**
   - Information completeness metrics
   - ML prediction confidence distribution
   - Validation accuracy results

#### Implementation:
- Use existing `visualize_extraction_results.R` as foundation
- Create manuscript-specific plotting script
- Ensure high-resolution, publication-ready outputs

### 4. Statistical Analysis Framework 📋

#### Key Analyses Needed:

**Descriptive Statistics:**
- Total studies included
- Geographic distribution summary
- Temporal distribution
- Method usage frequencies
- Species detection rates

**Comparative Analysis:**
- Global North vs South research characteristics
- Method sophistication by region/time
- Information completeness trends
- Publication venue analysis

**Quality Metrics:**
- ML model performance validation
- Inter-rater reliability (if multiple validators)
- Data completeness assessment
- Bias detection results

### 5. Data Management and Reproducibility 🔄

#### Create Finalized Datasets:
1. **Master dataset**: All classified abstracts with extraction results
2. **Included studies**: Final set for systematic review
3. **Validation results**: Manual review outcomes
4. **Summary statistics**: Key metrics for manuscript

#### Documentation Requirements:
- Detailed methods documentation (see `METHODS.md`)
- Data dictionary for all variables
- Code availability statement
- Reproducibility checklist

### 6. Manuscript Structure Recommendations 📝

#### Suggested Sections:

**Abstract:**
- Background: Endophyte research growth and systematic review need
- Methods: ML-assisted screening + comprehensive extraction
- Results: Key patterns in geographic, temporal, methodological trends
- Conclusions: Research gaps and future priorities

**Introduction:**
- Endophyte ecological importance
- Previous review limitations
- Need for comprehensive, unbiased analysis
- Study objectives

**Methods:**
- Literature search strategy
- ML classification approach (reference detailed methods)
- Extraction methodology
- Validation procedures
- Statistical analysis

**Results:**
- Literature search and screening results
- Geographic distribution patterns
- Temporal trends in research
- Methodological evolution
- Species and ecosystem coverage
- Research quality assessment
- Validation results

**Discussion:**
- Major findings and implications
- Geographic and methodological biases
- Research capacity issues
- Biodiversity conservation implications
- Limitations and future directions

**Supplementary Materials:**
- Detailed methodology
- Complete species lists
- Additional figures and tables
- Data availability information

### 7. Target Journal Considerations 🎯

#### Potential Venues:
- **Mycologia** (mycological focus)
- **Applied and Environmental Microbiology** (methodological emphasis)
- **Biological Reviews** (comprehensive reviews)
- **Global Ecology and Biogeography** (geographic patterns)
- **Frontiers in Microbiology** (open access, broad scope)

#### Journal-Specific Preparation:
- Review author guidelines for figure specifications
- Check word limits and structure requirements
- Consider open access policies
- Plan for data sharing requirements

### 8. Collaboration and Review Process 👥

#### Internal Review:
- Co-author review of results and interpretations
- Statistical analysis verification
- Figure and table review
- Methods documentation review

#### External Input:
- Expert consultation on species identification validation
- Geographic representation review
- Methodological expertise input
- Writing and editing support

### 9. Timeline and Milestones 📅

#### Immediate (1-2 weeks):
- [ ] Run manual validation analysis
- [ ] Execute temporal and geographic scripts
- [ ] Create core manuscript figures
- [ ] Draft methods section

#### Short-term (2-4 weeks):
- [ ] Complete statistical analyses
- [ ] Draft results section
- [ ] Create supplementary materials
- [ ] Internal co-author review

#### Medium-term (1-2 months):
- [ ] Complete manuscript draft
- [ ] External expert review
- [ ] Revisions and refinements
- [ ] Journal submission preparation

### 10. Quality Assurance Checklist ✔️

#### Data Quality:
- [ ] Manual validation completed
- [ ] Error rates documented
- [ ] Bias assessment performed
- [ ] Statistical assumptions checked

#### Reproducibility:
- [ ] All code documented and tested
- [ ] Data processing steps recorded
- [ ] Version control maintained
- [ ] Dependencies documented

#### Manuscript Quality:
- [ ] Methods clearly described
- [ ] Results comprehensively presented
- [ ] Limitations acknowledged
- [ ] Implications discussed

#### Ethical Considerations:
- [ ] Proper attribution of data sources
- [ ] Open science practices followed
- [ ] Conflicts of interest declared
- [ ] Author contributions clarified

## Immediate Next Steps

1. **Execute analysis scripts**:
   ```r
   source("manual_validation_sample.R")
   source("temporal_trend_analysis.R") 
   source("geographic_bias_analysis.R")
   ```

2. **Review generated reports** for key findings and insights

3. **Create manuscript-specific figures** using visualization framework

4. **Begin drafting methods section** using `METHODS.md` as foundation

5. **Plan validation timeline** and assign responsibilities

## Expected Outcomes

This systematic approach will produce:
- Comprehensive, unbiased view of endophyte research landscape
- Identification of research gaps and priorities
- Methodological advancement in systematic review approaches
- Foundation for future research planning and funding decisions
- High-impact publication in appropriate venue

## Support Resources

- **Technical**: All analysis scripts in `scripts/04_analysis/`
- **Documentation**: Methods and structure documents
- **Data**: Comprehensive extraction results and validation tools
- **Visualization**: Flexible plotting framework for manuscript figures

---

*This document serves as a roadmap for completing the manuscript preparation process. Update checklist items as tasks are completed and adjust timeline as needed based on validation results and analysis outcomes.*
