# Manuscript Preparation

Authors: B. Bock, N. McKay, N.C. Johnson, C.A. Gehring
Goal journal: Nature

## Title

## Abstract

## Introduction
- **Problem Statement**: The assumption that "all plants have fungi inside them" is a cornerstone of plant-microbe ecology but has never been systematically tested
- **Approach**: First comprehensive assessment using machine learning to analyze >21,000 scientific abstracts
- **Key Findings**: 91% accuracy in relevance filtering, 87% in presence/absence detection; 99.5% of studies report presence, and no taxon-level absences of fungal endophytes reported.
- **Implications**: Supports universality paradigm, reveals methodological and geographical biases, establishes automated literature mining as a new tool for microbial ecology

# Results
- **Primary Finding**: Fungal endophytes extraordinarily widespread - 99.5% of 19,447 relevant abstracts report presence in plant tissues (Fig. 2)
- **Absence Analysis**: Only 89 abstracts (0.5%) report absence; validation confirms 1 genuine natural case vs. experimental artifacts. However, other abstracts report finding fungal endophytes in that plant taxon (Phragmites australis), so it does not count as a taxon-level absence.
- **Geographic Bias**: Strong over-representation from USA vs. under-representation from Global South (Fig. 3)
- **Taxonomic Bias**: Comprehensive coverage of model organisms but gaps in diverse lineages.
- **Implication**: Current universality claims may be premature due to methodological and sampling biases

# Discussion
- **Ecological Significance**: Near-universal presence (99.5%) supports fundamental role in plant ecology and evolution
- **Paradigm Challenge**: Single confirmed natural absence questions universality assumption
- **Bias Analysis**: Geographic and taxonomic sampling biases undermine global generalizations
- **Methodological Innovation**: ML-powered literature mining provides scalable framework for large ecological datasets
- **Future Directions**: Systematic surveys of understudied taxa/regions needed; re-evaluation of universality paradigm
- **Broader Impact**: Methodology applicable to other long-standing questions in microbial ecology

# Methods
## Literature Mining and Classification
- **Database Search**: Web of Science, PubMed, Scopus using comprehensive fungal endophyte search terms
- **Data Processing**: 21,429 abstracts harmonized and deduplicated across databases
- **Classification Pipeline**: Two-stage ML approach - relevance filtering (19,447 retained) → presence/absence classification (19,358 presence, 89 absence)
- **Technical Implementation**: Memory-optimized processing for large-scale analysis, sparse matrix operations

## Model Development
- **Pipeline Overview**: Two-stage classification system (Fig. 1)
- **Stage 1 - Relevance Model**: Regularized logistic regression achieving 91% accuracy; trained on 3,500 manually labeled abstracts
- **Stage 2 - Presence/Absence Model**: Ensemble of logistic regression + SVM (optimized weights: SVM 0.6 for presence, GLM 0.8 for absence); 89.8% accuracy, 91.6% presence recall, 82.6% absence recall
- **Validation**: 5-fold cross-validation; full specifications in Supplementary Methods

## Model Validation
- **Expert Review**: Manual validation of 102 high-confidence absence predictions
- **Key Finding**: Only 1 genuine natural absence confirmed (Lambert & Casagrande 2006); 99 were methodological artifacts
- **String Detection**: Universal claims ("all plants have fungi") appear in 2.3% of abstracts, showing assumption pervasiveness

# References

# Acknowledgements

# Author contributions

# Competing interests

# Data availability

# Code availability