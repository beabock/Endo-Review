# Manuscript Preparation

Authors: B. Bock, N. McKay, N.C. Johnson, C.A. Gehring
Goal journal: Nature

## Title
Ubiquity of fungal endophytes in plants globally assessed with a systematic review performed with machine learning

## Abstract

Fungal endophytes are widely believed to be ubiquitous in plants, yet this paradigm has never been comprehensively tested. Here we use machine learning to analyze over 21,000 scientific abstracts from 1926-2025, achieving 89.8% accuracy in classifying endophyte presence/absence. Our analysis reveals that 99.5% of studies report fungal endophyte presence in plant tissues, with only 0.5% documenting absence. However, expert validation shows that nearly all 'absence' cases are methodological artifacts rather than genuine absences. Strikingly, research biases create an illusion of universality: only 0.8% of global plant species are represented, with extreme geographic concentration in the Global North (76.7% of studies). This work challenges the universality assumption, highlights critical sampling biases, and establishes automated literature mining as a powerful tool for large-scale ecological synthesis.

## Introduction

### Background
The paradigm that "all plants have fungi inside them" represents a foundational assumption in plant-microbe ecology, with fungal endophytes recognized as ubiquitous symbionts that influence plant health, stress tolerance, and ecosystem functioning (1-3). This universality claim appears in 0.2% of abstracts in our dataset.

### Research Gaps and Objectives
While individual studies have documented endophyte presence in specific plant taxa, the assumption of universal occurrence across all plants has never been comprehensively tested. Critical knowledge gaps include:
- Taxonomic coverage: Only 0.8% of global plant species represented in endophyte literature. Massive gaps in certain plant phyla like Bryophyta (0.2% species represented).
- Geographic bias: Strong over-representation from Global North (76.7%) vs. South (28.8%), specifically dominated by the United States.
- Methodological evolution: Transition from culture-based (pre-2000) to molecular detection (47% in recent studies)
- Absence evidence: Limited systematic documentation of endophyte-free plants

### Study Approach
We conducted the first comprehensive assessment using machine learning analysis of >21,000 scientific abstracts from Web of Science, PubMed, and Scopus. Our automated pipeline achieved 89.8% overall accuracy in classifying relevance and endophyte presence/absence, enabling systematic literature mining at unprecedented scale.

### Key Findings
Analysis of 19,071 relevant abstracts (excluding studies which only mentioned mycorrhizal fungal species) revealed that 99.5% report fungal endophyte presence, with only 89 studies (0.5%) labeled as reporting absence. Manual validation confirmed 1 genuine reported natural absence case versus 99 methodological artifacts, where plants were experimentally rendered endophyte-free, but are not endophyte-free on a taxonomic level. The 1 genuine absence-report (Lambert and Casagrande) reported no fungal endophytes in *Phragmites australis*, but other researchers have found fungal endophytes in this taxon. These findings support the universality paradigm while highlighting substantial research biases that may undermine global generalizations.

### Implications
Our results establish automated literature mining as a powerful tool for large-scale ecological synthesis, reveal critical sampling biases in endophyte research, and provide empirical foundation for re-evaluating the universality assumption. The methodological framework developed here is applicable to other long-standing questions in microbial ecology.

# Results

## Literature Landscape and Dataset Characteristics

Our systematic search identified 21,429 abstracts from Web of Science, PubMed, and Scopus (1926-2025). After deduplication and relevance filtering using a two-stage machine learning pipeline (89.2% relevance accuracy, 87.3% presence/absence accuracy), we retained 19,071 relevant abstracts for analysis (excluding 376 studied which only mentioned mycorrhizal fungal species).

The dataset spans a 1350-fold increase in publication volume, from 3 studies in the 1920s to 7,048 in 2020-2024. Research methodology evolved dramatically, with molecular detection increasing from 0% pre-2000 to 47% in recent periods. Geographic coverage shows strong bias toward the Global North (76.7% of studies) versus South (28.8%).

## Evidence for Endophyte Absence

Among 19,071 relevant studies, 18,982 (99.5%) report fungal endophyte presence in plant tissues, while 89 (0.5%) document absence. Expert validation of high-confidence absence predictions revealed only 1 genuine natural case (experimental sterility in Phragmites australis) versus 99 false negatives (mostly cases where plants were experimentally rendered endophyte-free but the plant taxon itself is not naturally endophyte-free).

Notably, other studies report endophyte presence in P. australis, indicating this single case does not constitute taxon-level absence. 

## Research Bias Analysis

### Geographic Bias
Endophyte research exhibits extreme geographic concentration, with the United States contributing 45% of all studies despite representing only 5% of global land area. European countries (UK, Germany, Netherlands) account for 23% combined. In contrast, biodiversity-rich regions show severe under-representation: Brazil (2.8%), China (3.2%), India (1.1%), and Indonesia (0.3%). This North-South disparity (76.7% vs. 28.8%) undermines claims of universal endophyte occurrence.

### Taxonomic Bias
Only 0.8% of global plant species (approximately 39,000 of 4.9 million described species) are represented in endophyte literature. Coverage varies dramatically by lineage:
- Tracheophyta (vascular plants): 0.9% species coverage, but no family fully represented
- Bryophyta (mosses): 0.2% coverage
- Glaucophyta: 0% species represented
- Marchantiophyta (liverworts): 0.1% coverage

The most studied families (Poaceae: 12%, Fabaceae: 8%, Asteraceae: 6%) represent only 15% of global plant diversity but 50% of endophyte research.

### Temporal and Methodological Trends
Publication volume increased exponentially, with 83% of all studies published since 2000. Detection methods evolved from exclusively culture-based (pre-1980) to molecular-dominated (47% post-2010). Information completeness improved over time, with recent studies providing more detailed methodological reporting.

## Species Detection and Biodiversity Insights

Automated species extraction identified fungal endophytes in 394 unique plant species in recent periods (2020-2024), with 83.7% of abstracts containing detectable species information. Detection accuracy improved from 33% pre-2000 to 83% in recent studies, reflecting both better methodology and more explicit species reporting.

These patterns reveal that current universality claims are premature, as research biases create an illusion of comprehensive coverage while leaving 99.2% of global plant diversity unexplored.

# Discussion

## Ecological and Evolutionary Implications

The universal presence of fungal endophytes in the published literature provides strong empirical support for their fundamental role in plant ecology and evolution. This extraordinarily widespread symbiosis suggests endophytes are not niche associates but core components of plant microbiomes, potentially influencing:
- Plant stress tolerance and adaptation to environmental change
- Competitive interactions and community assembly
- Evolutionary trajectories of plant lineages
- Ecosystem-level nutrient cycling and carbon sequestration

## Research Bias Implications

The extreme sampling biases revealed here fundamentally undermine current generalizations about endophyte universality:
- Geographic bias (76.7% North vs. 28.8% South) creates a skewed view of global patterns
- Taxonomic bias (0.8% species coverage) leaves 99.2% of plant diversity unexplored
- Temporal bias (83% of studies post-2000) may miss historical patterns

These biases create an illusion of comprehensive evidence while masking critical knowledge gaps, particularly in biodiversity hotspots of the Global South and understudied plant lineages.

## Methodological Innovation and Validation

Our machine learning pipeline (89.8% overall accuracy) demonstrates the power of automated literature mining for large-scale ecological synthesis. The systematic manual validation of absence claims establishes rigorous quality control standards. This approach provides:
- Scalable framework for evidence synthesis across disciplines
- Objective bias quantification through systematic data collection
- Reproducible methodology for addressing long-standing ecological questions

## Future Research Directions

The identified biases demand targeted research efforts:
- Systematic surveys of understudied taxa (Glaucophyta, Marchantiophyta, under-represented families)
- Geographic expansion to biodiversity-rich regions (Brazil, Indonesia, China)
- Methodological standardization for comparable absence detection
- Long-term studies examining endophyte dynamics under environmental change

## Broader Applications

The ML-powered literature mining framework developed here extends beyond endophyte ecology to address other fundamental questions in microbial ecology, such as:
- Bacterial endophyte distribution patterns
- Mycorrhizal association universality
- Pathogen-host interaction networks
- Climate change impacts on plant-microbe symbioses

This approach establishes automated systematic review as a cornerstone methodology for evidence-based microbial ecology.

# Methods

## Literature Search and Data Acquisition

We conducted a comprehensive literature search across three major databases, with collections occuring on 14 August 2025:
- Web of Science Core Collection (1926-present): 14,855 abstracts
- PubMed (1946-present): 10,873 abstracts
- Scopus (1930-present): 15,427 abstracts

Search terms combined fungal endophyte terminology with plant hosts:

```
("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi" OR
  "latent fungus" OR "latent fungi" OR "systemic fungus" OR "systemic fungi" OR
  "internal fungi" OR "resident fungi" OR "seed-borne fungi" OR "seed-transmitted fungi" OR
  "dark septate endophyte" OR "dark septate fungi" OR "DSE fungi")
  AND
  (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR
  pteridophyte* OR tree* OR shrub* OR grass* OR "graminoid*" OR herb* OR
  crop* OR seedling* OR sapling* OR seed* OR root* OR leaf* OR foliage OR shoot* OR
  stem* OR twig* OR rhizome* OR thallus OR frond* OR algae OR "green alga*" OR macroalga* OR
  cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
```

Terms such as "latent fungi" were used to capture older terms for fungal endophytes.

Search was also restricted to only articles, excluding review articles.

## Data Processing and Harmonization

Raw abstracts underwent systematic processing:
1. **Combination**: Combining all abstracts from all three sources resulted in a dataset of 40,776 abstracts.
2. **Deduplication**: Exact matching of DOIs removed 17,455 abstracts. Exact matching of abstracts removed 314 abstracts.  Filtering to articles only removed 420 abstracts. Removing exact title matches removed 696 abstracts, leaving the final dataset row count at 21,891 abstracts, with 1,630 missing DOIs, 1 missing author, 0 missing titles, and 0 missing abstracts. 
3. **Text Cleaning**: Standardization of Unicode characters, removal of HTML artifacts
4. **Metadata Extraction**: DOI, publication year, journal, author affiliations

Final processed dataset contained 20,967 abstracts with complete metadata and text.

## Machine Learning Classification Pipeline

### Two-Stage Classification System
We implemented a memory-optimized, two-stage classification pipeline using R (version 4.3.0) with glmnet, caret, and themis packages.

**Stage 1 - Relevance Filtering**: Regularized logistic regression (glmnet) distinguished endophyte-relevant from irrelevant abstracts. Model trained on 3,500 manually labeled abstracts (2:1 relevant:irrelevant ratio). Achieved 89.2% accuracy (91.3% precision, 88.7% recall) on 5-fold cross-validation.

**Stage 2 - Presence/Absence Classification**: Weighted ensemble of logistic regression and SVM-Linear, optimized for imbalanced data. Trained on 2,800 relevant abstracts. Overall accuracy 87.3% (91.6% presence recall, 82.6% absence recall). Pipeline accuracy: 89.8%.

## Model Performance Summary

| Model Component | Description | Accuracy | Key Features | Training Data | Strengths |
| --- | --- | --- | --- | --- | --- |
| Relevance Classification | Regularized Logistic Regression (glmnet) | 89.2% | Text unigrams/bigrams from abstracts | ~3,500 labeled abstracts | Effective at filtering irrelevant studies |
| Presence/Absence Ensemble | Weighted Ensemble (glmnet + SVM-Linear) | 87.3% | Text unigrams/bigrams, weighted by model strengths | ~2,800 relevant abstracts | Balances precision/recall trade-offs |
| Overall Pipeline (Relevance + P/A) | Two-stage classification pipeline | 89.8% | Combined relevance filtering + P/A detection | ~3,500 labeled abstracts (two-stage) | Comprehensive endophyte literature screening |

### Feature Engineering
- Text preprocessing: tokenization, stop-word removal, lowercase conversion
- Feature representation: unigrams and bigrams with TF weighting
- Class balancing: SMOTE oversampling for minority absence class
- Memory optimization: sparse matrices for large-scale processing

## Information Extraction and Analysis

### Species Detection Pipeline
Automated extraction of plant species names using:
- Dictionary-based matching against GBIF taxonomic backbone (4.9 million species)
- Morphological variant handling (e.g., "P. australis" → Phragmites australis)
- Contextual validation to reduce false positives
- Achieved 83.7% detection rate in recent abstracts

### Geographic and Methodological Extraction
- Country/region detection using geonames database
- Research method classification (culture, microscopy, molecular)
- Temporal trend analysis with 5-year aggregation

### Mycorrhizal Filtering
Excluded 376 mycorrhizal-only abstracts (studies detecting only mycorrhizal fungi) to focus on non-mycorrhizal endophyte patterns, resulting in 19,071 relevant abstracts for final analysis. Supplememtary figures include figures replicated to incldue the mycorrhizal-only abstracts, and results are not noticeably different due to the small numnber (376) of mycorrhizal-only abstracts.

## Validation and Quality Control

### Manual Validation of Presence/Absence Determinations
Manual review of 102 high-confidence absence predictions by domain experts. Classification criteria:
- Genuine natural absence: confirmed endophyte-free plants in natural conditions
- Methodological artifacts: detection failures, inappropriate methods, developmental stages
- Result: 1 genuine natural absence confirmed vs. 99 artifacts

Presence validation was conducted on 77 high-confidence presence predictions to assess specificity. Manual review confirmed that 65% represented true detections of fungal endophytes, 15% were false positives, and 20% were irrelevant (e.g., reviews, studies of bacterial endophytes).

Automated text mining for absence evidence identified explicit statements of fungal endophyte absence in 10 high-confidence cases and suggestive absence patterns in 304 medium-confidence cases across 19,071 relevant abstracts. This text mining approach used keyword patterns and contextual analysis to validate the model's absence predictions, revealing that most ML-classified absence cases actually contained implicit or methodological absence claims rather than explicit natural absences.

### Universality Claim Detection
Text mining identified explicit universality claims ("all plants have fungi", "universal endophytes") in 0.2% of abstracts, quantifying assumption pervasiveness.

## Statistical Analysis

### Bias Quantification
- Geographic representation: proportion analysis with 95% confidence intervals
- Taxonomic coverage: species count against global estimates (GBIF backbone)

### Code

All analyses conducted in R with reproducible random seed (1998). Complete code available at [GitHub repository].

# Figures

## Figure 1: Machine Learning Pipeline and Classification Performance
- **Panel A**: PRISMA-style flow diagram showing literature screening pipeline
- **Panel B**: Confusion matrices and performance metrics for relevance and P/A models
- **Panel C**: ROC curves demonstrating classification accuracy
- **Panel D**: Feature importance analysis for model interpretability

## Figure 2: Evidence of Endophyte Presence and Absence
- **Panel A**: Global map showing geographic distribution of studies (presence vs absence)
- **Panel B**: Taxonomic distribution of absence evidence by plant family
- **Panel C**: Methodological breakdown of absence studies (culture vs molecular)
- **Panel D**: Temporal trends in absence reporting (1926-2025)

## Figure 3: Research Bias Analysis
- **Panel A**: Geographic bias - North vs South research representation (% publications vs % land area)
- **Panel B**: Taxonomic bias - plant family coverage vs global diversity
- **Panel C**: Temporal evolution of research volume (5-year periods)
- **Panel D**: Method adoption trends (culture → molecular transition)


## Extended Data Figures

### Extended Data Figure 1: Detailed Model Validation
- Cross-validation performance across different thresholds
- Calibration plots for probability estimates
- Feature selection analysis and model stability

### Extended Data Figure 2: Comprehensive Species Detection Results
- Species accumulation curves over time
- Geographic distribution of detected species
- Taxonomic resolution trends (genus vs species level)

### Extended Data Figure 3: Supplementary Bias Analyses
- Country-level research contribution maps
- Journal publication patterns
- Author affiliation network analysis

### Extended Data Figure 4: Complete Statistical Results
- Full regression analyses for temporal trends
- Confidence intervals for all proportion estimates
- Sensitivity analysis results

# References

1. Arnold, A. E. et al. Fungal endophytes limit pathogen damage in a tropical tree. Proc. Natl Acad. Sci. USA 100, 15649-15654 (2003).

2. Rodriguez, R. J. et al. Stress tolerance in plants via habitat-adapted symbiosis. ISME J. 2, 404-416 (2008).

3. Porras-Alfaro, A. & Bayman, P. Hidden fungi, emergent properties: endophytes and microbiomes. Annu. Rev. Microbiol. 65, 207-226 (2011).

4. Tedersoo, L. et al. Global diversity and geography of soil fungi. Science 346, 1256688 (2014).

5. Caruso, T. et al. Relative roles of niche and neutral processes in structuring a soil microbial community. ISME J. 8, 1947-1959 (2014).

6. Xu, L. et al. Soil fungal communities respond to grassland plant community richness and soil edaphics. New Phytol. 198, 859-868 (2013).

# Acknowledgements

# Author contributions

# Competing interests

# Data availability

# Code availability