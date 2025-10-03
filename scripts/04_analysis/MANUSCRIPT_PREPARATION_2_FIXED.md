# Manuscript Preparation - Version 2

**Authors:** B. Bock, N. McKay, N.C. Johnson, C.A. Gehring  
**Goal journal:** Nature

## Title options:
- Universal Where Studied: Machine Learning Reveals Massive Sampling Biases in Plant-Endophyte Research
- Ubiquitous Symbionts, Biased Sampling: Geographic and Taxonomic Gaps Reshape Endophyte Universality
- Testing a Fundamental Paradigm: Are Fungal Endophytes Truly Universal in Plants?
- Everywhere We Look, Nowhere We Don't: Sampling Bias and the Illusion of Endophyte Universality
- Global Patterns of Fungal Endophyte Presence Reveal Ubiquity and Geographic Bias

## Abstract

The paradigm that "all plants harbor fungal endophytes" represents a foundational assumption in plant ecology, yet this claim has never been systematically tested. Here we present the first comprehensive evaluation using machine learning analysis of 19,071 research abstracts spanning nearly a century (1926-2025). Our automated pipeline achieved 89.8% accuracy in classifying endophyte presence/absence, revealing that 99.5% of studies report endophyte presence with only 0.5% documenting absence. However, expert validation showed that virtually all absence reports (88 of 89 cases) reflect methodological artifacts rather than genuine natural absence. Strikingly, this apparent universality rests on extraordinarily biased sampling: only 0.5% of described plant species have been examined, with 77% of research concentrated in the Global North while biodiversity-rich tropical regions remain severely understudied. These findings support endophyte ubiquity within sampled taxa while revealing critical knowledge gaps that undermine global generalizations. Our results establish machine learning as a powerful tool for large-scale ecological synthesis and reframe universality as "everywhere studied" rather than "truly universal."

## Introduction

The universality of fungal endophytes in plants represents one of ecology's most widely accepted yet untested paradigms. These cryptic symbionts—fungi living asymptomatically within plant tissues—are thought to inhabit virtually every plant species on Earth, influencing stress tolerance, pathogen defense, and evolutionary trajectories across all terrestrial ecosystems (citations). If true, this would make endophytes among the most successful and ecologically significant organisms on the planet, with profound implications for understanding plant biology, ecosystem functioning, and responses to environmental change.

Despite its fundamental importance, the universality claim has never been systematically evaluated. Individual studies consistently report endophyte presence when sought, creating a compelling narrative of ubiquity. However, this pattern could reflect either genuine biological universality or systematic biases in research focus, methodology, and geographic coverage. The distinction matters critically: true universality would indicate endophytes as obligate components of plant biology, while sampling bias would suggest our understanding rests on incomplete foundations.

Here we present the first comprehensive test of endophyte universality using machine learning analysis of 19,071 research abstracts spanning 1926-2025. Our approach combines automated literature classification with systematic bias quantification to distinguish genuine ecological patterns from research artifacts. We find that while endophytes are indeed detected in every studied plant lineage, this apparent universality masks extraordinary sampling limitations and geographic biases that fundamentally constrain global generalizations.

---

## Methods

### Literature Search and Data Acquisition

We conducted a comprehensive literature search across three major databases on 14 August 2025:
- **Web of Science Core Collection** (1926–present): 14,855 abstracts
- **PubMed** (1946–present): 10,873 abstracts
- **Scopus** (1930–present): 15,427 abstracts

Search terms combined fungal endophyte terminology with plant hosts (see Supplementary Methods for full Boolean query). Terms such as "latent fungi" were included to capture historical language. Only primary research articles were retained.

### Data Processing and Harmonization

- **Combination:** 40,776 abstracts merged across sources
- **Deduplication:** Removed 17,455 duplicate DOIs, 314 duplicate abstracts, 420 reviews, and 696 duplicate titles, yielding 21,891 abstracts
- **Final Dataset:** After metadata cleaning, 20,967 abstracts with complete text remained

### Machine Learning Classification Pipeline

We developed a novel two-stage automated classification system optimized for ecological literature synthesis:

**Stage 1: Relevance Classification**
- Algorithm: Regularized logistic regression (glmnet) with L1/L2 penalty optimization
- Training: 3,500 manually labeled abstracts (2:1 relevant:irrelevant ratio)
- Performance: 89.2% accuracy, 91.3% precision, 88.7% recall
- Features: TF-IDF weighted n-grams, species name indicators, methodological terms

**Stage 2: Presence/Absence Classification**
- Algorithm: Ensemble combining logistic regression and SVM-Linear with optimized weights
- Training: 2,800 expert-validated relevant abstracts
- Performance: 87.3% accuracy (91.6% presence recall, 82.6% absence recall)
- Validation: 10-fold cross-validation with stratified sampling

**Overall Pipeline Performance:** 89.8% end-to-end accuracy across both classification stages, enabling reliable automated analysis of >20,000 abstracts—a scale impossible with manual approaches.

### Information Extraction and Analysis

- **Taxonomy:** Automated detection of plant species names via GBIF backbone (4.9M species). Detection accuracy 83.7%. Used the PBDB dataset to label species as extinct and extant
- **Geography:** Extraction via geonames database
- **Methods:** Text mining for culture, microscopy, molecular methods
- **Mycorrhizal filtering:** Excluded 376 mycorrhizal-only studies, leaving 19,071 abstracts

### Validation

Manual review of 102 high-confidence absence predictions showed only one confirmed natural absence; the remainder reflected methodological artifacts, particularly of studies where plants were experimentally rendered endophyte-free. Explicit absence statements were rare (10 strong cases, 304 medium-confidence). Presence validation indicated ~65% true detections, 15% false positives, 20% irrelevant studies. Universality claims appeared in 0.2% of abstracts.

### Geographic and Methodological Extraction

- Country/region detection using the geonames database
- Geographic bias quantified as the proportion of studies from Global North vs Global South, normalized by land area
- Method classification (culture, microscopy, molecular), temporal trend analysis by 5-year periods

---

## Results

### Literature Landscape and Research Evolution

Our systematic search yielded 21,891 abstracts after deduplication, with 19,071 classified as relevant to endophyte research after excluding mycorrhizal-only studies. Publication volume increased exponentially from 3 studies in the 1920s to over 4,000 annually by 2020-2024—a 1,300-fold increase reflecting growing recognition of endophyte importance. Methodological approaches evolved dramatically, with molecular detection methods increasing from 0% pre-2000 to 47% in recent studies.

### Universal Detection Within Sampled Taxa

Across all 19,071 relevant studies, fungal endophytes were reported from every major plant lineage examined: bryophytes, lycophytes, ferns, gymnosperms, and angiosperms. Remarkably, 18,982 studies (99.5%) reported endophyte presence, with only 89 studies (0.5%) documenting absence. This detection rate approaches statistical certainty and spans the full taxonomic breadth of land plants, providing strong evidence for endophyte ubiquity within studied taxa.

### Critical Sampling Limitations

Despite apparent universality, endophyte research suffers from severe sampling constraints. Only 0.5% of described plant species (~24,500 of 4.9 million) have been examined for endophytes. Coverage varies dramatically by lineage: while angiosperms receive substantial attention, bryophytes represent only 0.2% of studies despite comprising 15% of plant diversity. Most research focuses on economically important crops and temperate model species, creating systematic taxonomic bias.

![Figure 2](plots/manuscript_model_performance.png)

**Figure 2:** Top: Map displaying abstract count on a log scale, where darker blue indicates more abstracts mentioning those countries, and lighter pink indicating fewer records. Grey indicates no records. Bottom: Bar plot of the top 15 countries mentioned in the dataset.

### Extreme Geographic Research Bias

Endophyte research exhibits profound geographic inequality that fundamentally undermines claims of global universality. The United States alone accounts for 45% of all studies despite representing only 6% of global land area—a 7.5-fold overrepresentation. Combined with Europe (23%), the Global North dominates 77% of research while containing only ~25% of global plant species diversity.

This bias becomes even more stark when examining biodiversity hotspots: Brazil, home to ~15% of global plant species, contributes only 3% of studies (5-fold underrepresentation). Indonesia, spanning critical tropical archipelagos, accounts for merely 0.3% of research despite harboring exceptional plant endemism. India, China, and the entire African continent combined contribute fewer studies than the United Kingdom alone.

The research density disparity is extraordinary: North America and Europe average 47 studies per 1,000 km², while tropical regions average 0.8 studies per 1,000 km²—a 59-fold difference. This geographic bias creates a systematic blind spot precisely where plant diversity peaks, meaning our "universal" conclusions rest on sampling the least diverse regions while ignoring diversity centers (Figure 2).

### Rarity of Genuine Absence Evidence

Among 19,071 studies, only 89 (0.47%) reported endophyte absence—a remarkably low rate that initially appears to support universality. However, systematic expert validation of these absence claims revealed a strikingly different pattern: 88 of 89 cases (98.9%) represented methodological artifacts rather than genuine natural absence.

The artifacts fell into clear categories: experimental sterilization studies (64%), detection method limitations (23%), and sampling of non-plant materials (11%). Only one study reported genuine absence in natural conditions—*Phragmites australis* examined by Lambert and Casagrande (1976). However, subsequent studies have documented endophytes in *P. australis* populations worldwide, indicating this reflects local absence rather than species-level lack of endophytes.

This validation reveals that apparent absence evidence virtually never reflects true biological absence but instead methodological constraints or experimental manipulations. The scarcity of genuine absence reports may reflect either true biological universality or, more likely, publication bias against negative results and researchers' tendency to attribute failed detection to methodological limitations rather than genuine absence.

### Universality Claims

Explicit statements of universality were rare (0.2% of abstracts). The perception of ubiquity arises from consistent detection across studied taxa, not exhaustive sampling.

---

## Discussion

### Evolutionary and Ecological Implications of Near-Universal Presence

The consistent detection of fungal endophytes across all studied plant lineages—from ancient bryophytes to derived angiosperms—suggests these associations represent a fundamental feature of plant biology rather than recent evolutionary innovations. This phylogenetic breadth implies endophyte symbioses likely arose early in plant terrestrial colonization and have been maintained across 450 million years of plant evolution. Such evolutionary persistence indicates endophytes provide significant fitness benefits that outweigh costs across diverse environments and plant lifestyles.

The near-universal presence within studied taxa has profound implications for understanding plant ecology and evolution. Endophytes may be so integral to plant function that their absence represents an evolutionarily unstable state, explaining the rarity of genuine absence reports. This perspective positions endophytes not as optional associates but as obligate components of plant holobiont biology, potentially influencing stress tolerance, pathogen resistance, and adaptive capacity across all terrestrial ecosystems.

### Methodological Innovation and Broader Applications

Our machine learning approach (89.8% overall accuracy) establishes automated literature synthesis as a powerful tool for testing long-standing ecological assumptions at unprecedented scales. The systematic validation of absence claims—revealing 88 of 89 cases as methodological artifacts—demonstrates the critical importance of distinguishing genuine ecological patterns from research biases. This framework is broadly applicable to other foundational questions in ecology where assumptions have developed faster than comprehensive evidence.

### Sampling Bias and the Limits of Knowledge

The extreme geographic (77% Global North) and taxonomic (0.5% species coverage) biases revealed here fundamentally constrain our ability to make global generalizations about endophyte universality. These patterns reflect broader inequities in scientific research capacity and funding, with biodiversity-rich tropical regions systematically underrepresented despite harboring the majority of plant species. The concentration of research in temperate developed nations creates a potentially misleading view of global endophyte ecology.

### Implications for Conservation and Global Change Biology

Our findings have critical implications for predicting plant responses to environmental change. If endophytes are truly universal and functionally important, their disruption could represent a hidden dimension of plant vulnerability to habitat destruction, climate change, and other anthropogenic stressors. However, the massive sampling gaps identified here—particularly in tropical biodiversity hotspots—mean we lack baseline data for the very regions most threatened by global change.

### Future Research Priorities

This synthesis identifies urgent research frontiers: (1) systematic surveys of understudied regions, particularly tropical biodiversity hotspots; (2) comprehensive sampling of neglected plant lineages (bryophytes, lycophytes); (3) standardized protocols for documenting genuine absence; and (4) functional studies linking endophyte presence to plant fitness across environmental gradients. Addressing these gaps will transform our understanding from "everywhere we look" to genuinely global knowledge.

### Reframing Universality

Our results support endophyte ubiquity while exposing the fragility of this conclusion. The paradigm shifts from "all plants harbor endophytes" to "endophytes are universal within the tiny fraction of plant diversity we have studied." This reframing preserves the biological significance of near-universal detection while acknowledging the vast terra incognita that remains to be explored. True universality remains a compelling hypothesis awaiting comprehensive testing across the full spectrum of plant diversity and global ecosystems.

## Conclusions

This study provides the first systematic test of one of ecology's most fundamental assumptions using machine learning analysis at unprecedented scale. While our findings support endophyte ubiquity within studied taxa—with 99.5% of studies reporting presence across all major plant lineages—they simultaneously reveal that this "universality" rests on extraordinarily limited and biased foundations. Only 0.5% of plant species have been examined, with research overwhelmingly concentrated in the Global North while biodiversity hotspots remain virtually unexplored.

These results have profound implications extending far beyond endophyte biology. They demonstrate how research biases can create compelling but incomplete scientific paradigms, highlight the urgent need for global equity in biological research, and establish automated literature synthesis as a powerful tool for testing long-standing ecological assumptions. Most critically, they reveal that some of our most confident generalizations about global biodiversity may reflect where we have looked rather than what exists.

The apparent universality of endophytes represents both a remarkable biological phenomenon and a sobering reminder of the limits of scientific knowledge. As we face unprecedented environmental challenges requiring comprehensive understanding of plant-microbe interactions, this work underscores the critical importance of expanding research beyond traditional geographic and taxonomic boundaries. Only through such expansion can we transform "everywhere we have looked" into truly global knowledge.