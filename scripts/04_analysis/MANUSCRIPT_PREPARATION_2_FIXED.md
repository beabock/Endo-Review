---
output:
  word_document: default
  html_document: default
---
# Manuscript Preparation

**Authors:** B. M. Bock, N. McKay, N.C. Johnson, C.A. Gehring

**Goal journal:** Nature

**Title:** The Sparsely Sampled Ubiquity of Global Fungal Endophytes


## Abstract
The paradigm that "all plants harbor fungal endophytes" is a foundational assumption in plant ecology, yet this claim has never been systematically tested at a global scale. Here, we use a machine learning pipeline to analyze **19,199** research abstracts spanning nearly a century. We developed a high-sensitivity screening tool to find and manually validate all potential claims of endophyte absence. This comprehensive validation revealed zero verifiable examples of an endophyte-free plant taxon within the entire scientific literature surveyed. This finding confirms that endophyte ubiquity is a robust pattern wherever plants have been studied. However, we show that this "ubiquitous" paradigm is a generalization from an extraordinarily biased sample: only 0.8% of described plant species have been examined for fungal endophytes, with 77% of all research concentrated in the Global North. Our analysis reframes a foundational paradigm, revealing that our understanding of a "global" symbiosis is based on a tiny fraction of biodiversity. This sampling bias represents a critical bottleneck in biodiversity research, limiting our discovery of novel biotechnologies and climate-resilient symbioses in the world's most threatened ecosystems.

## Introduction
Fungal endophytes are fungi that inhabit plant tissues without causing apparent disease (Petrini, 1991). This symbiosis, considered one of the most abundant on earth, influences plant ecology and evolution and enhances plant stress tolerance (Rodriguez et al., 2009). Foundational literature has established the paradigm that this symbiosis is ubiquitous. Influential book chapters (e.g., Stone et al., 2000) and the introductions of highly-cited research articles (e.g., Arnold et al., 2000) state this claim explicitly. This idea has become a central, defining assumption in ecology, which is restated in contemporary reviews (e.g., Cosner et al., 2025) noting they are "found in nearly all plants," and underpins research on endophyte function, diversity, and transmission.

However, this assumption of ubiquity remains unvalidated, as a comprehensive, systematic evaluation of fungal endophyte distribution across plant taxa and regions is lacking. Previous reviews have been qualitative or focused on specific lineages (e.g., Schulz & Boyle, 2005; U’Ren et al., 2012), leaving the global pattern untested. Crucially, testing this paradigm requires distinguishing between a "study-level absence" (a failure to detect endophytes in a specific sample or experiment) and a "taxon-level absence" (a plant taxon that always lacks endophytes). While study-level absences are expected due to detection limits, a confirmed taxon-level absence would fundamentally challenge the ubiquity paradigm.

Here, we leverage a **novel machine learning pipeline** to conduct the first **large-scale, quantitative meta-ecology test** of endophyte ubiquity. We classify **19,199** abstracts focused on fungal endophytes to map the taxonomic and geographic patterns of research and systematically test the claim of ubiquity against the evidence.

## Results
Our analysis of **19,199** abstracts focused on fungal endophytes confirms that the literature is overwhelmingly a record of presence. Given that "true absence" is an exceptionally rare event, we designed a high-sensitivity screening pipeline (see Methods) to find and manually validate all potential absence candidates.

ML-Assisted Identification of Putative Absence Studies: The final weighted ensemble (SVM, ×0.6 Presence; glmnet, ×0.8 Absence) was applied to the entire corpus of 19,447 relevant abstracts. This high-sensitivity screening tool identified 89 abstracts classified as Absence, while 19,358 were classified as Presence. These 89 abstracts, representing 0.46% of the relevant literature, were manually curated to validate claims of endophyte absence, which ultimately confirmed zero verifiable instances of endophyte-free taxa.

This pipeline reduced the **19,199** abstracts to a manageable set of 102 candidates. We then manually validated 100% of these cases. This validation revealed that none of these candidates represented a true, taxon-level absence. While we identified individual studies that failed to find endophytes (study-level absences), further investigation revealed that other studies published on the same plant taxon do report presence. Therefore, our comprehensive analysis found zero verifiable examples of an endophyte-free plant taxon in our entire **19,199**-abstract dataset.

### A Biased View of Ubiquity
Despite this confirmed ubiquity, our analysis reveals that this conclusion rests on a tiny and deeply biased sample of plant biodiversity. We find that only 0.8% (3,226 of 390,101 species) of described plant species have been examined for endophytes. This sparsity is consistent across all taxonomic ranks: only 6.7% (1,506 of 22,441) of described plant genera and only 28.9% (343 of 1,185) of described plant families have been studied for endophytes. The corresponding study of the fungal symbionts has only reached 3.5% of described fungal species (**5,064** of 143,957 species).

![Figure 1: Plant species representation by phylum in the analyzed literature. Bars show the percentage of described species within each phylum present in the 19,199-abstract dataset. Overall coverage = 0.8% (3,226 of 390,101 described plant species).](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/plantae_species_representation_main_by_phylum_percent.png)

This bias systematically overlooks critical evolutionary lineages. For example, Charophyta (Stoneworts), the closest algal relatives to land plants, have no coverage (0 of 2,012 species) in our dataset. This is a critical knowledge gap, as it leaves the aquatic origins of plant-fungal symbiosis almost completely untested. Similarly, Bryophyta (Mosses), representing some of the earliest land plants, have only 0.2% coverage (21 of 10,508 species). This gap obscures our understanding of how these symbioses evolved after plants first colonized land.

Even within the relatively well-studied Tracheophyta (Vascular Plants), which encompasses familiar groups like ferns, conifers, and flowering plants, coverage remains exceptionally low at 0.9% (3,150 of 363,445 species). This demonstrates that our knowledge of endophyte ubiquity is largely derived from a thin sliver of plant diversity.

This taxonomic bias is mirrored by an extreme geographic bias (Figure 2; Supp. Figs. 3-5). Research is concentrated in high-income countries (the Global North), which account for 77% of all geographically-coded studies (World Bank, 2025). This concentration is driven almost entirely by just two regions: North America and Europe alone account for 70.3% of all studies. Conversely, entire nations with unique biodiversity, such as Tonga, had zero studies detected in our dataset. A temporal analysis shows that these geographic and taxonomic biases are not historical artifacts; they have remained largely consistent over the past three decades, even as molecular methods have become standard (used in 36.8% of all studies; Figure 3; Supp. Fig. 6).

![Figure 2: Country-level counts of geographically-coded studies on a log10 scale and Robinson projection (countries with zero studies shown in light gray).](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/world_map_countries_log.png)

Finally, analysis of plant parts reported in the abstracts shows that research has covered a range of host tissues, with **roots, leaves, seeds, stem, shoot, and fruit** being the most frequently studied structures (Supp. Fig. 9). This demonstrates thorough sampling across tissue types within the limited taxonomic breadth.

---
## Discussion
Our analysis provides the first large-scale, data-driven confirmation of the fungal endophyte ubiquity paradigm. By demonstrating that there is no evidence for a single endophyte-free plant taxon in the literature, we confirm that ubiquity is a robust pattern wherever plants have been studied.

However, the central finding of this paper is the stark contradiction between this observed ubiquity and the severe sampling biases that define the field. The 0.8% of plant species and 77% concentration in the Global North are not minor gaps; they are systematic biases that fundamentally limit our understanding. Our "ubiquitous" paradigm is a generalization from a non-random, convenience-based sample of the plant kingdom.

The stark under-sampling of plant taxa (0.8% coverage) has a direct, critical consequence for our understanding of fungal biodiversity. Given that endophytes are often host-specific and nearly ubiquitous across taxa, our data strongly suggest that the true biodiversity of fungal endophytes is severely underestimated. This conclusion is reinforced by the limited scope of fungal discovery itself: despite the **19,199** abstracts focusing on endophytes, we only find evidence for 3.5% of described fungal species (**5,064** of 143,957 species) and **16.5%** of fungal genera (**1,832** of 11,108 genera) being reported in the literature. Our current understanding of endophyte diversity is therefore a non-random, minimal subset, constrained by the taxonomic convenience of the sampled plant species. 

![Figure 3: Temporal trends in methods reported for detecting fungal endophytes across the 19,199‑abstract corpus. Molecular approaches were negligible before the 1990s and rose sharply after 2000; overall, 36.8% of studies in the corpus report molecular methods. Data include only abstracts that explicitly described methods; see Methods for extraction and classification details.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/supplementary/temporal_method_trends.png)

### From "Study-Level Negatives" to "Taxon-Level Ubiquity"
A key challenge in testing ubiquity is the interpretation of "absence" reports. Our screening pipeline flagged 102 abstracts that mentioned absence. Manual validation clarified that these were either irrelevant (40), simple false positives (46), or reports of mixed results (15) where endophytes were found in some tissues but not others or in some plant individuals but not others.

We identified only a single genuine "study-level absence" in the literature: a study reporting no fungal endophytes in *Phragmites australis* (Lambert & Casagrande, 2006). However, this absence is contradicted by numerous other studies that confirm diverse fungal endophyte communities in the same host species (e.g., Wirsel et al., 2001; Angelini et al., 2012; Clay et al., 2016). This specific case perfectly illustrates our core distinction: while individual studies may fail to detect endophytes ("study-level negatives"), "taxon-level ubiquity" holds true. We found no evidence of a plant taxon that is consistently free of fungal endophytes.

### Limitations of Scope
We acknowledge that this analysis is based on data extracted from scientific abstracts, not full-text articles. This is a standard trade-off for a review of this scale. Crucially, this means our data on species counts and geographic regions represent a **conservative minimum (an undercount)**, as abstracts may not explicitly list every plant species and research site studied. This inherent under-counting reinforces the conclusion that the true sampling bias is likely even more severe than the 0.8% coverage reported. Additionally, we acknowledge the potential for publication bias, often called the "file drawer problem" (Rosenthal, 1979), where negative results (researchers looking for but failing to find endophytes) may be less likely to be published. This potential publication bias may itself be a consequence of the ubiquity paradigm; studies failing to detect endophytes might be dismissed by authors or reviewers as methodological failures rather than true biological negatives, thus reinforcing the very paradigm they would otherwise challenge. Therefore, our findings represent the state of the published scientific record, which is itself shaped by this assumption.

### Conclusions
This concentration of research has profound practical consequences. By prioritizing the Global North, we are effectively ignoring the tropical and arid biodiversity hotspots of the Global South (Myers et al., 2000), regions where plants face the most extreme environmental pressures (Seneviratne et al., 2021). This reframes the research bias from a simple academic gap to a significant bottleneck in global bioprospecting. This is not a trivial omission; a significant percentage of modern pharmaceuticals are derived from or inspired by natural products (Strobel, 2003; Newman & Cragg, 2012; El-Shora, 2023). 
The profound impact of this sampling bias is perhaps best illustrated by the ongoing bioprospecting success in extreme, under-sampled environments. For example, research into the endophytic fungal community of the Antarctic angiosperm *Colobanthus quitensis*, a plant that thrives in one of the planet’s harshest environments, has yielded highly novel compounds (Bertini et al. 2022). Isolates, primarily Basidiomycetes fungi, have demonstrated significant, multi-faceted biotechnological value: their extracts exhibit dose-dependent antitumor activity and, critically, show the ability to inhibit the formation of amyloid fibrils of $\alpha$-synuclein, a process linked to neurodegenerative disorders like Parkinson's disease. This discovery confirms that unique metabolic pathways—developed by endophytes for survival in extreme conditions—represent a vast, untapped source of novel biotechnologies currently excluded from our limited, Global North-biased sampling efforts.

Beyond simply quantifying bias, our analysis yields an essential product: a curated list of the plant taxa, geographic regions, and fungal phyla that are currently under-represented in the published record. This resource, provided in the Supplementary Information, acts as a **roadmap for targeted future research**, identifying priority taxa and regions (e.g., Charophyta, Bryophyta, and nations like Tonga) where directed bioprospecting is most likely to yield novel ecological and biotechnological discoveries.

---
## Methods
Our methodology consisted of a three-stage computational pipeline: (1) ML-based abstract classification, (2) a multi-step validation of "Absence" classifications, and (3) a validated data extraction pipeline for taxonomic and geographic information. Full details, from data sourcing to validation, are provided in the Supplementary Methods.

### 1. ML Classification as a High-Sensitivity Screen
We developed a two-stage machine learning pipeline. The first stage screened for relevance and filtered out mycorrhizal-only abstracts, resulting in the **19,199** abstract dataset. Although mycorrhizal fungi inhabit plant tissues, they represent a distinct, well-characterized functional guild separate from the general "endophyte" paradigm tested here (Cosner et al., 2025); therefore, abstracts focusing exclusively on mycorrhizal fungi were excluded to avoid confounding the results on endophytic fungi.

For the primary classification, we faced an extreme imbalanced class problem: our manually-curated 876-abstract training dataset contained 346 'Presence' examples but only a single 'Absence' example (0.11%). To address this scarcity, we employed data augmentation techniques, generating synthetic abstracts representing plausible absence claims (both manually and using large language models). These synthetic data were used solely to define the linguistic boundaries for the rare non-Presence classes ('Absence', 'Both', and 'Other') during training and were not part of the analyzed dataset.

We designed a high-sensitivity screening tool, not a high-precision classifier. We used a weighted ensemble of two machine learning models: a Support Vector Machine (a model that finds the optimal boundary separating two groups) and a regularized logistic regression (a model that automatically selects the most important predictive words). As documented in our model testing, we intentionally weighted the model to prioritize absence detection, creating a screen that would flag all potential candidates for manual review.

### 2. Pipeline Validation: Absence and Manual Review
We implemented a rigorous, multi-step process to validate all potential "Absence" classifications. First, our ML screen flagged 89 candidates. Second, an independent rule-based string detection algorithm flagged 12 additional unique candidates. This created a final set of 102 abstracts for manual validation.

We manually reviewed 100% of these 102 abstracts. The results, detailed in the Supplementary Methods, confirmed that zero represented a true, taxon-level absence.

### 3. Data Extraction Pipeline and Validation
We built a modular data extraction pipeline to parse taxonomic, geographic, and methodological information. The robustness of every component, from species name extraction to geographic coordinate parsing, was verified using a comprehensive, automated test suite. This suite evaluated core functionality, accuracy, synonym resolution, and error handling, ensuring high fidelity in the extracted data.

### 4. Data Aggregation
Taxonomic names were standardized and synonyms were resolved against the Global Biodiversity Information Facility (GBIF) backbone (GBIF.org, 2025) to ensure accurate aggregation of species counts. Geographic information was aggregated at the country and region level. For regional analysis, countries were classified as 'Global North' or 'Global South' based on the World Bank's income group classifications (e.g., 'High income' vs. 'Low/Middle income') (World Bank, 2025).

---

## References
Angelini, P. et al. The endophytic fungal communities associated with the leaves and roots of the common reed (Phragmites australis) in Lake Trasimeno (Perugia, Italy) in declining and healthy stands. *Fungal Ecology* 5, 683–693 (2012).

Arnold, A. E., Maynard, Z., Gilbert, G. S., Coley, P. D., & Kursar, T. A. (2000). Are tropical fungal endophytes hyperdiverse?. *Ecology Letters*, 3(4), 267-274.

Bertini, L.; Perazzolli, M.;
Proietti, S.; Capaldi, G.; Savatin, D.V.;
Bigini, V.; Longa, C.M.O.; Basaglia,
M.; Favaro, L.; Casella, S.; et al.
Biodiversity and Bioprospecting of
Fungal Endophytes from the
Antarctic Plant Colobanthus quitensis.
J. Fungi 2022, 8, 979. https://doi.org/
10.3390/jof8090979

Clay, K., Shearin, Z., Bourke, K., Bickford, W. & Kowalski, K. Diversity of fungal endophytes in non-native Phragmites australis in the Great Lakes. *Biological Invasions* 18, 2703–2716 (2016).

Cosner, J., Pandharikar, G., Tremble, K., Nash, J., Rush, T. A., Vilgalys, R., & Veneault-Fourrey, C. (2025). Fungal endophytes. *Current Biology*, 35(19), R904-R910.

El-Shora, H. M., & El-Sayed, M. (2023). Endophytic fungi: a treasure trove of novel bioactive compounds for pharmaceutical and agricultural applications. *Symbiosis*, 79(3), 295-322.

GBIF.org (2025). *GBIF Home Page*. Retrieved from https://www.gbif.org

Lambert, A. & Casagrande, R. No evidence of fungal endophytes in native and exotic Phragmites australis. *Northeastern Naturalist* 13, 561–568 (2006).

Myers, N., Mittermeier, R. A., Mittermeier, C. G., da Fonseca, G. A. & Kent, J. Biodiversity hotspots for conservation priorities. *Nature* 403, 853–858 (2000).

Newman, D. J. & Cragg, G. M. Natural products as sources of new drugs over the 30 years from 1981 to 2010. *Journal of Natural Products* 75, 311–335 (2012).

Petrini, O. Fungal Endophytes of Tree Leaves. in *Microbial Ecology of Leaves* (eds Andrews, J. H. & Hirano, S. S.) 179–197 (Springer New York, New York, NY, 1991).

Rodriguez, R. J., White, J. F., Arnold, A. E., & Redman, R. S. (2009). Fungal endophytes: diversity and functional roles. *New Phytologist*, 182(2), 314-330.

Rosenthal, R. The “file drawer problem” and tolerance for null results. *Psychological Bulletin* 86, 638–641 (1979).

Royal Botanic Gardens, Kew. (2016). *State of the World's Plants Report - 2016*.

Schulz, B., & Boyle, C. (2005). The endophytic continuum. *Mycological Research*, 109(6), 661-86.

Seneviratne, S.I., et al. Weather and Climate Extreme Events in a Changing Climate. in *Climate Change 2021: The Physical Science Basis. Contribution of Working Group I to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change* (eds. Masson-Delmotte, V. et al.) 1513–1766 (Cambridge University Press, 2021).

Stone, J. K., Bacon, C. W. & White, F. An Overview of Endophytic Microbes: Endophytism Defined. in *Microbial Endophytes* 1–28 (CRC Press, 2000).

Strobel, G. A. Endophytes as sources of bioactive products. *Microbes and Infection* 5, 535–544 (2003).

U’Ren, J. M., Lutzoni, F., Miadlikowska, J., Laetsch, A. D. & Arnold, A. E. Host and geographic structure of endophytic and endolichenic fungi at a continental scale. *American Journal of Botany* 99, 898–914 (2012).

Wirsel, S. G. R., Leibinger, W., Ernst, M. & Mendgen, K. Genetic diversity of fungi closely associated with common reed. *New Phytologist* 149, 589–598 (2001).

World Bank. (2025). World Bank Country and Lending Groups. Retrieved from https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

---

## Author Contributions
B.B. conceived the study, designed the methodology, performed the data analysis and machine learning, and wrote the manuscript. N.M., N.C.J. and C.A.G. contributed to the final manuscript text. All authors read and approved the final manuscript.

## Competing Interests
The authors declare no competing interests.

## Funding Statement
This work was supported by...

## Data and Code Availability
The full code for the data analysis pipeline, including all R scripts used for machine learning, validation, and figure generation, is available at [GitHub/Zenodo DOI]. The final abstract dataset and model outputs generated during this study are available at [Zenodo/Dryad DOI].

---

## Supplementary Methods

### 1. Data Sourcing and Deduplication
The initial abstract corpus was generated by querying three major bibliographic databases: Web of Science (WoS), Scopus, and PubMed on 14 August, 2025. The search string was designed to capture fungal endophyte research broadly. Search string: `("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi" OR 
  "latent fungus" OR "latent fungi" OR "systemic fungus" OR "systemic fungi" OR 
  "internal fungi" OR "resident fungi" OR "seed-borne fungi" OR "seed-transmitted fungi" OR 
  "dark septate endophyte" OR "dark septate fungi" OR "DSE fungi")
  AND
  (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR lycophyte* OR 
  pteridophyte* OR tree* OR shrub* OR grass* OR "graminoid*" OR herb* OR 
  crop* OR seedling* OR sapling* OR seed* OR root* OR leaf* OR foliage OR shoot* OR 
  stem* OR twig* OR rhizome* OR thallus OR frond* OR algae OR "green alga*" OR macroalga* OR 
  cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)`

This process yielded a combined dataset of 40,076 abstracts. This corpus was subjected to a rigorous, multi-stage deduplication pipeline:

1.  Standardization of column names (e.g., Title, Abstract, DOI) across sources.

2.  Deduplication by DOI, reducing the set to 23,321 abstracts.

3.  Deduplication by normalized (to lowercase) abstract text, reducing the set to 23,007 abstracts.

4.  Filtering to include "Article" document types only, reducing the set to 22,587 abstracts.

5.  Final deduplication by normalized (to lowercase) title, resulting in the 21,891 abstract dataset that was fed into the ML pipeline.

### 2. ML Classification Pipeline
The 21,891 abstracts were processed by a two-stage machine learning pipeline.

* **Relevance Classification:** The initial relevance classification stage used a regularized logistic regression (glmnet) to filter the corpus of 21,891 abstracts for primary literature relevant to fungal endophytes. Using 5-fold cross-validation on the manually curated training set, the model achieved an Overall Accuracy of 0.895 and a Sensitivity (Recall) for the 'Relevant' class of 0.976. This high sensitivity was critical to ensure the first filter retained the vast majority of true endophyte-focused studies, preventing the loss of relevant data from the corpus before the final analysis. The Specificity (correctly identifying 'Irrelevant' abstracts) was 0.674, which is an acceptable trade-off for maximizing the recall of the primary, 'Relevant' literature. An irrelevant abstract was defined as a non-primary literature article that did not discuss fungal endophytes. Common examples were abstracts that discussed only bacterial endophytes or review papers.

* **Mycorrhizal Filtering:** A dedicated component filtered abstracts focusing exclusively on mycorrhizal fungi. Starting with **19,380** unique abstracts, we systematically removed abstracts that reported mycorrhizal associations to avoid confounding the endophytic signal. This involved a multi-step filter targeting rows classified as `is_mycorrhizal` or containing the phylum `Glomeromycota`. This rigorous filtering resulted in the final corpus of **19,199** relevant, non-mycorrhizal-only abstracts used for all subsequent analyses.

* **Presence/Absence Classification:** The training dataset for the second stage, which classified abstracts as Presence or Absence, consisted of 505 relevant abstracts (80% split). The class distribution was highly imbalanced (Presence: 411, Absence: 94). To mitigate classification bias toward the dominant Presence class, we applied the Synthetic Minority Oversampling Technique (SMOTE) to the training data, resulting in a balanced training set of 822 abstracts (411 for each class).The models were trained on a Document-Term Matrix (DTM) composed of 7,492 unique features (Unigrams and Bigrams) extracted from the abstract text.
This corpus was classified using a custom-weighted ensemble of a `glmnet` model and a linear-kernel Support Vector Machine (`svmLinear`). As documented in our model testing, this ensemble allowed us to prioritize absence detection. The final model applied a weight of 0.6 to the SVM's "Presence" prediction and 0.8 to the `glmnet` "Absence" prediction. All models were validated using 5-fold cross-validation. Two distinct models were trained and cross-validated on the balanced dataset: Support Vector Machine with a Linear Kernel (svmLinear) and Regularized Logistic Regression (glmnet). The svmLinear model demonstrated superior ability to detect studies reporting fungal presence, achieving a Presence Recall of 0.980 (Sensitivity). Conversely, the glmnet model achieved a superior Absence Recall of 0.913 (Specificity). To optimize performance for the rare Absence class while retaining high confidence in Presence classification, we implemented a weighted probability ensemble. This strategy combines the prediction probabilities of the two models using the following weights:

    Weight for svmLinear Presence prediction: 0.6

    Weight for glmnet Absence prediction: 0.8

This approach ensures that a high probability of Absence from the glmnet model, which excels at this detection, is strongly favored, thus creating a high-confidence screening tool for our critical class of interest. The optimized threshold for the combined ensemble achieved an Overall Accuracy of 0.872 with a Presence Recall of 0.882 and an Absence Recall of 0.826 on the test set (at a threshold of 0.7).

### 3. Training Data Composition
The models were trained on a manually curated dataset of 876 abstracts. This dataset was characterized by extreme class imbalance, with 346 "Presence" examples (39.5%) but only one genuine "Absence" example from the literature (0.11%). To compensate, data augmentation was used to create synthetic examples of "Both" (n=117) and "Other" (n=180) classes to help the model learn the linguistic boundaries of ambiguous, negative, and irrelevant claims. The publication year distribution of the non-augmented training abstracts mirrors that of the full 19,199-abstract corpus, confirming the training data are not temporally biased (Supp. Fig. 7).

### 4. Ubiquity Statement Analysis
To quantify the nature of the "ubiquity paradigm," we ran a separate analysis to find explicit statements of ubiquity (e.g., "endophytes are ubiquitous") within the relevant literature. The search terms were applied to 19,447 abstracts. This analysis found that only 38 abstracts (0.2%) contained such a statement, confirming the paradigm functions largely as an implicit assumption in the primary literature.

### 5. Computational Optimization & Pipeline
The pipeline was engineered for high-throughput and robust processing of large text datasets.

* **Error Handling:** The pipeline featured a robust error handling system, including "safe execution wrappers" and data recovery from backups, to prevent catastrophic failures during long-running processes.

* **Memory Optimization:** We employed memory-efficient techniques, such as chunked processing of large files, data structure optimization (e.g., converting strings to factors), and aggressive garbage collection, to ensure the pipeline could run on standard hardware.

* **Optimized Lookups:** We used hash-table optimization and pre-computed lookup tables for O(1) (instant) lookups of species names.

* **Bloom Filters:** We implemented DuckDB-based bloom filters for probabilistic pre-filtering, which allowed the pipeline to instantly reject 80-90% of non-matching taxonomic strings, dramatically reducing expensive validation queries.

### 6. Modular Extraction Pipeline
The data extraction pipeline was built as a series of modular, independent components.

The extraction components demonstrated high fidelity, successfully detecting plant and fungal species in **$82.2\%$** ($\text{15,940}$) of abstracts and plant part information in **$75.5\%$** of abstracts.

1.  **Species & Mycorrhizal Extraction:** The first component extracted all plant and fungal taxonomic names and classified fungi for mycorrhizal status (Supp. Fig. 8).

2.  **Mycorrhizal-Only Filtering:** A dedicated script assessed abstracts and flagged those containing *only* mycorrhizal fungi for exclusion.

3.  **Component Extraction:** Subsequent components ran on the filtered abstract list to extract research methods (e.g. culture, molecular), plant parts (e.g. stem, leaf, bark, root) (Supp. Fig. 9), and geographic locations.

4.  **Final Aggregation:** A final script merged the outputs from all components into a single comprehensive dataset.

### 7. Pipeline Performance and Visualization

* **Profiling:** The pipeline's performance was comprehensively profiled, confirming a high throughput of ~100-500 abstracts/second.

* **Visualization:** All data visualizations were generated in R using `ggplot2`, employing custom, colorblind-friendly palettes and themes to ensure accessibility and consistency.

### 8. Code Reproducibility
The codebase includes a comprehensive framework to ensure validation and reproducibility, including scripts to generate representative test subsets and "quick start" test scripts to automatically execute the entire pipeline on these subsets.

### 9. Detailed Validation Protocols

* **Extraction Validation Suite:** The data extraction components were validated using a comprehensive, automated test suite (`test_extract_species.R`) that evaluated "core functionality, accuracy, performance, and robustness." The pipeline achieved a weighted "Overall score" of 91.4%, passing all 10 test categories.

* **Manual Classification Validation:** We validated our ML classification in two ways. First, we reviewed 100% of all 'Absence' candidates. Second, to estimate the 'Absence' False Negative Rate (i.e., 'Absence' abstracts misclassified as 'Presence'), we manually reviewed a stratified random sample of 77 abstracts that the model had classified as 'Presence'. This review confirmed that 0 of the 77 abstracts were genuine 'Absence' cases, supporting the robustness of our primary finding.

### 10. Detailed Absence Validation Results
The high-sensitivity screening pipeline identified 102 candidate abstracts for manual validation (89 from the ML model, 12 from the string search, 1 from training). Manual expert review of these 102 abstracts yielded the following:

* 46 'Presence': Simple false positives (the abstract only reported presence).

* 15 'Both': Abstracts that reported mixed results (e.g., presence in leaves, absence in roots). These are considered 'Presence' at the taxon level.

* 40 'Irrelevant': Abstracts irrelevant to the study.

* 1 'Absence': A single study-level report of non-detection (Lambert & Casagrande, 2006).
This validation confirmed zero verifiable taxon-level absences in the corpus.

## Supplementary Figures

![Supp. Fig. 1: Genus representation by plant phylum. Proportion of described genera represented in the corpus, broken down by plant phylum.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/plantae_genus_representation_main_by_phylum_percent.png)

![Supp. Fig. 2: Family representation by phylum (plants). Proportion of described families represented in the corpus, by plant phylum.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/plantae_family_representation_main_by_phylum_percent.png)

![Supp. Fig. 4: Top countries by study count. Bar plot of the top 20 countries by number of geographically-coded studies; United States, China, South Africa, India, and Brazil are highest.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/top_countries.png)

![Supp. Fig. 5: Plant phylum geographic distribution. Geographic distribution of studies stratified by plant phylum to show regional sampling biases across major plant lineages.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/geographic/plant_phylum_geographic_distribution.png)

![Supp. Fig. 6: Combined detection methods overview. Relative and absolute counts of method types (molecular, culture-based, microscopy/visual) across studies that reported methods.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/methods_combined_core.png)

![Supp. Fig. 7: Distribution of publication years in the training dataset.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/training_publication_years.png")

![Supp. Fig. 8: Fungal species representation by phylum. Percent of described fungal species represented in the dataset by fungal phylum.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/fungi_species_representation_main_by_phylum_percent.png)

![Supp. Fig. 9: Plant parts frequency in the dataset. Frequency distribution of plant parts (leaf, root, stem, etc.) reported across the 19,199-abstract corpus.](C:/Users/beabo/OneDrive%20-%20Northern%20Arizona%20University/NAU/Endo-Review/plots/main/plant_parts_frequency.png)