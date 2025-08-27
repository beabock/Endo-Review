# Manuscript Preparation Checklist

## Project: Testing the Universality of Fungal Endophytes in Plants
**Target Submission**: November 2025  
**Primary Target**: Nature (Tier 1), PNAS (Tier 2)

---

## 📋 **Pre-Writing Phase (August-September 2025)**

### ✅ **COMPLETED - Core Analysis Pipeline**
- [x] Machine learning classification (89.8% accuracy)
- [x] Species detection and extraction pipeline
- [x] Geographic and methodological information extraction
- [x] Research framework and objectives documentation
- [x] Search strategy documentation and reproducibility
- [x] Publication strategy and target journal analysis

### 🔄 **IN PROGRESS - Data Processing**
- [ ] Complete species detection on full dataset
- [ ] Finalize comprehensive extraction results
- [ ] Generate final summary statistics and reports
- [ ] Data quality validation and inter-rater reliability

### 📋 **IMMEDIATE PRIORITIES (Next 2 Weeks)**

#### 1. Complete Current Analysis Pipeline
- [ ] **Species Detection**: Finish running on full dataset
- [ ] **Results Integration**: Combine all extraction components
- [ ] **Quality Control**: Validate species detection accuracy
- [ ] **Summary Statistics**: Generate manuscript-ready numbers

#### 2. Data Documentation for Reproducibility
- [x] ✅ **DATA_COLLECTION_LOG.md**: Complete data provenance
- [x] ✅ **CITATION_ANALYSIS_FRAMEWORK.md**: Framework for literature impact analysis
- [ ] **PROCESSING_WORKFLOW.md**: Step-by-step analysis pipeline
- [ ] **VALIDATION_PROTOCOL.md**: Quality control and validation methods

#### 3. Statistical Analysis Framework
- [ ] **Power Analysis**: Sample size justification for conclusions
- [ ] **Bias Assessment**: Geographic, taxonomic, temporal bias quantification
- [ ] **Confidence Intervals**: Uncertainty quantification for key findings
- [ ] **Sensitivity Analysis**: Robustness testing of main conclusions

---

## 📊 **Analysis Phase (September 2025)**

### Core Research Questions - Data Analysis Status

#### Q1: Evidence of Endophyte Absence
- [ ] **Species-level analysis**: Plant taxa without detected endophytes
- [ ] **Tissue-level analysis**: Plant tissues consistently endophyte-free
- [ ] **Ecosystem-level patterns**: Environments with low endophyte prevalence
- [ ] **Methodological adequacy**: Quality of negative result studies

#### Q2: Research Bias Assessment  
- [ ] **Geographic bias**: Global North vs South research representation
- [ ] **Taxonomic bias**: Over/under-represented plant families
- [ ] **Temporal bias**: Research trends over time
- [ ] **Methodological bias**: Detection method preferences and capabilities

#### Q3: Citation and Knowledge Propagation Analysis
- [x] ✅ **Framework established**: Citation analysis methodology
- [ ] **Data collection**: Citation metrics for all classified papers
- [ ] **Network analysis**: Research community structure and influence
- [ ] **Claim validation**: Evidence supporting universality assertions

### Statistical Analyses Required

#### Descriptive Statistics
```r
# Key numbers needed for manuscript
total_abstracts_analyzed <- [TO BE CALCULATED]
presence_studies <- [COUNT] 
absence_studies <- [COUNT]
uncertain_studies <- [COUNT]
species_detected <- [COUNT]
countries_represented <- [COUNT]
years_covered <- [RANGE]
```

#### Inferential Statistics
- [ ] **Chi-square tests**: Association between variables
- [ ] **Logistic regression**: Predictors of study outcomes  
- [ ] **Time series analysis**: Temporal trends in research patterns
- [ ] **Spatial analysis**: Geographic clustering of research

#### Effect Sizes and Confidence Intervals
- [ ] **Proportion estimates**: % studies finding absence with 95% CI
- [ ] **Geographic coverage**: Representation gaps with uncertainty
- [ ] **Taxonomic coverage**: Family/genus representation analysis
- [ ] **Method effectiveness**: Detection rate by methodology

---

## 📝 **Writing Phase Structure (October 2025)**

### Target Journal: Nature Format
**Word Limits**: 
- Main text: ~3,000 words
- Methods: ~800 words  
- References: ~50 citations
- Figures: 4 main + extended data

### Manuscript Outline (Nature-style)

#### **Title Options**
1. "Systematic analysis reveals limited evidence for universal fungal endophytes in plants"
2. "Machine learning analysis challenges assumption of universal plant-fungal endophyte associations"  
3. "Comprehensive literature review questions universality of fungal endophytes across plant species"

#### **Abstract (150 words)**
- [ ] **Background**: Universality claim and its importance (30 words)
- [ ] **Methods**: ML classification + comprehensive extraction (40 words)  
- [ ] **Results**: Key quantitative findings (50 words)
- [ ] **Conclusion**: Implications for endophyte ecology (30 words)

#### **Main Text Structure**

##### **Introduction (~400 words)**
- [ ] Endophyte ecological importance and current paradigms
- [ ] The universality claim and its prevalence in literature
- [ ] Evidence gaps and methodological considerations
- [ ] Study objectives and approach

##### **Results (~1,200 words)**
- [ ] **Literature landscape**: Dataset characteristics and scope
- [ ] **Evidence for absence**: Studies documenting endophyte-free plants  
- [ ] **Research biases**: Geographic, taxonomic, methodological patterns
- [ ] **Citation analysis**: Knowledge propagation and claim validation

##### **Discussion (~800 words)**
- [ ] **Interpretation**: What absence evidence tells us about universality
- [ ] **Implications**: For endophyte ecology, evolution, and plant biology
- [ ] **Limitations**: Study constraints and future research needs
- [ ] **Conclusion**: Revised framework for endophyte distribution

##### **Methods (~600 words)**  
- [ ] Search strategy and data collection
- [ ] Machine learning classification approach
- [ ] Information extraction and species detection
- [ ] Statistical analysis and bias assessment

### **Figures (4 Main + Extended Data)**

#### **Figure 1**: Study Flow and Classification
- [ ] PRISMA-style flow diagram
- [ ] Classification accuracy metrics
- [ ] Dataset characteristics overview

#### **Figure 2**: Evidence of Absence
- [ ] Geographic distribution of absence studies
- [ ] Taxonomic representation of absence findings
- [ ] Methodological adequacy assessment

#### **Figure 3**: Research Bias Analysis  
- [ ] Geographic bias (Global North/South)
- [ ] Taxonomic bias (family/genus representation)
- [ ] Temporal trends in research focus

#### **Figure 4**: Citation Network and Knowledge Propagation
- [ ] Citation network of influential papers
- [ ] Propagation of universality claims
- [ ] Evidence quality vs citation impact

#### **Extended Data Figures**
- [ ] Detailed methodological validation
- [ ] Comprehensive species detection results
- [ ] Supplementary bias analyses
- [ ] Complete statistical results

---

## 🔬 **Methods Documentation (Critical for Reproducibility)**

### Required Methods Sections

#### **Literature Search and Selection**
- [x] ✅ Search strategy documented (SEARCH_STRATEGY.md)
- [x] ✅ Database selection justified  
- [ ] PRISMA compliance checklist
- [ ] Selection criteria application workflow

#### **Machine Learning Classification**
- [x] ✅ Training data preparation documented (METHODS.md)
- [x] ✅ Model selection and validation approach
- [ ] Cross-validation procedure details
- [ ] Performance metrics calculation methods

#### **Information Extraction Pipeline**  
- [x] ✅ Species detection methodology (docs/README_taxa_detection.md)
- [x] ✅ Geographic detection approach
- [ ] Method detection algorithms
- [ ] Validation and quality control procedures

#### **Statistical Analysis Framework**
- [ ] **Bias assessment methods**: Geographic, taxonomic, temporal
- [ ] **Confidence interval calculations**: Proportions and effect sizes
- [ ] **Sensitivity analysis**: Robustness testing approach
- [ ] **Missing data handling**: Treatment of incomplete records

---

## 📋 **Submission Preparation (November 2025)**

### Journal-Specific Requirements

#### **Nature Submission Checklist**
- [ ] **Cover letter**: Significance and broad interest justification
- [ ] **Main manuscript**: 3,000 word limit compliance
- [ ] **Methods**: Detailed methodology (online-only section)
- [ ] **Extended data**: Comprehensive supplementary results
- [ ] **Source data**: Processed datasets for figures
- [ ] **Code availability**: GitHub repository with DOI

#### **PNAS Backup Preparation**
- [ ] **Significance statement**: 120-word scientific significance
- [ ] **Classification**: Biological Sciences - Ecology
- [ ] **Format adjustment**: PNAS-specific structure requirements
- [ ] **Length adjustment**: 6 pages including figures

### Supporting Materials

#### **Data Availability Statement**
```
Search strategies, classification code, and processed datasets are available at [GitHub repository]. 
Raw abstracts cannot be shared due to copyright restrictions but are available upon reasonable 
request for replication purposes. Species reference data are derived from publicly available 
GBIF backbone taxonomy.
```

#### **Code Availability Statement**  
```
All analysis code is available at https://github.com/[username]/Endo-Review with permanent DOI 
from Zenodo. The repository includes complete machine learning pipeline, species detection 
algorithms, and statistical analysis scripts for full reproducibility.
```

#### **Funding and Acknowledgments**
- [ ] NSF funding acknowledgment
- [ ] Institutional support (NAU)
- [ ] Software and database acknowledgments
- [ ] Peer review and feedback acknowledgments

---

## ✅ **Quality Control and Validation**

### Pre-Submission Validation Tasks

#### **Data Validation**
- [ ] **Manual validation**: 100 abstracts per classification category
- [ ] **Inter-rater reliability**: Independent classification verification  
- [ ] **Species detection accuracy**: Botanical validation of identifications
- [ ] **Geographic accuracy**: Country/region assignment verification

#### **Statistical Validation**
- [ ] **Power analysis**: Sample size adequacy for conclusions
- [ ] **Assumption testing**: Statistical test requirements
- [ ] **Multiple comparison corrections**: Family-wise error rate control
- [ ] **Sensitivity analysis**: Robustness to parameter changes

#### **Reproducibility Validation**
- [ ] **Code testing**: Independent reproduction of all analyses
- [ ] **Documentation review**: Completeness of methodological description
- [ ] **Data accessibility**: Verification of data sharing compliance
- [ ] **Version control**: Stable release with permanent identifiers

### Internal Review Process

#### **Technical Review (September 2025)**
- [ ] Computational methods review by statistical collaborator
- [ ] Botanical species identification review by taxonomist
- [ ] Geographic analysis review by biogeographer
- [ ] Machine learning approach review by ML specialist

#### **Scientific Review (October 2025)**  
- [ ] Ecological interpretation review by endophyte researcher
- [ ] Systematic review methodology assessment
- [ ] Literature review completeness evaluation
- [ ] Implications and significance assessment

---

## 📅 **Timeline Summary**

| Phase | Date Range | Key Deliverables | Status |
|-------|------------|------------------|---------|
| **Data Processing** | Aug 20-Sep 5 | Complete analysis pipeline | 🔄 In Progress |
| **Statistical Analysis** | Sep 5-20 | All quantitative results | 📋 Planned |
| **Citation Analysis** | Sep 20-Oct 5 | Knowledge propagation study | 📋 Planned |
| **Writing Phase** | Oct 5-25 | Complete manuscript draft | 📋 Planned |
| **Internal Review** | Oct 25-Nov 5 | Validation and revision | 📋 Planned |
| **Submission** | Nov 5-15 | Journal submission | 📋 Planned |

---

**Last Updated**: August 26, 2025  
**Next Milestone**: Complete species detection pipeline  
**Critical Path**: Data processing → Statistical analysis → Writing  
**Risk Management**: PNAS backup prepared if Nature timeline slips
