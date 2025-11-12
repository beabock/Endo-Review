# Manuscript Preparation - Version 13 (Final Polish)
**Authors:** B. Bock, N. McKay, N.C. Johnson, C.A. Gehring
**Goal journal:** Nature

## Title
Ubiquitous Symbionts, Biased Sampling: Geographic and Taxonomic Gaps Reshape Endophyte Universality

## Abstract
The paradigm that "all plants harbor fungal endophytes" represents a foundational assumption in plant ecology, yet this claim has never been systematically tested. Here we present the first comprehensive evaluation using machine learning analysis of 19,071 research abstracts spanning nearly a century (1926-2025). Our automated pipeline achieved 89.8% accuracy in classifying endophyte presence/absence, revealing that 99.5% of studies report endophyte presence with only 0.5% documenting absence. However, expert validation showed that virtually all absence reports (88 of 89 cases) reflect methodological artifacts rather than genuine natural absence. Strikingly, this apparent universality rests on extraordinarily biased sampling: only **0.8%** of described plant species have been examined, with 77% of research concentrated in the Global North. Our findings confirm endophyte ubiquity as a robust pattern *within studied taxa* but demonstrate this "universality" is a generalization from a tiny fraction of global plant diversity. This work establishes a validated baseline for plant-microbe symbiosis and reveals this bias as a critical bottleneck in biodiversity research.

## Introduction
Fungal endophytes are fungi that inhabit plant tissues without causing apparent disease. This symbiosis is considered one of the most abundant on earth, influencing plant ecology, evolution, and stress tolerance (Rodriguez et al., 2009). They are also a critical source of novel pharmaceuticals and biomaterials (El-Shora & El-Sayed, 2023). The notion that "all plants harbor fungal endophytes" has become a common claim in ecological literature (e.g., Arnold et al., 2000). Indeed, our analysis of the literature confirms this is a widespread assertion, with a quantitative analysis identifying that **[X.X]%** of abstracts explicitly reiterate this claim [CITE: `find_all_plants_statement.R` results]. This assumption of ubiquity underpins research on endophyte function, diversity, and transmission.

However, this assumption remains unvalidated, as a comprehensive, systematic evaluation of fungal endophyte distribution across plant taxa and regions is lacking. Previous reviews have been qualitative or focused on specific lineages (e.g., Schulz & Boyle, 2005), leaving the global pattern untested. It has been unclear whether the paradigm of ubiquity is a robust, data-driven conclusion or or an artifact of research focus. This knowledge gap is critical: if endophytes are not ubiquitous, their ecological and evolutionary impacts may be more constrained than assumed. Conversely, if they are ubiquitous, the biases in *where* we have looked become paramount.

Here, we use a machine learning pipeline to conduct the first large-scale, quantitative test of the endophyte ubiquity paradigm. We classify over 19,000 abstracts from the endophyte literature to map the taxonomic and geographic patterns of research, explore the effects of detection methods on presence/absence designations, and systematically test the claim of ubiquity against the evidence.

## Results
Our machine learning pipeline analyzed 19,071 relevant abstracts, confirming that the endophyte literature is overwhelmingly a record of presence. We found 99.5% of studies reported endophyte presence, with only 0.5% (89 studies) documenting absence. To test the validity of these rare absence reports, we performed a multi-step validation. This process confirmed that 99% (88 of 89) of "absence" reports were methodological artifacts (e.g., studies that sought but failed to find a *specific* taxon, not all endophytes) or pipeline misclassifications. This analysis confirms that genuine reports of endophyte-free plants are virtually non-existent in the scientific literature.

Despite this near-universal detection, our analysis reveals that this conclusion rests on a tiny and deeply biased sample of plant biodiversity. We find that only **0.8%** (3,226 of 390,101 species) of described vascular plant species have been examined for endophytes [CITE: `visualize_taxa_results_manuscript_log.txt`].

This taxonomic bias is mirrored by an extreme geographic bias. Research is concentrated in the Global North, with 77% of all studies originating from Europe and North America. The top five most-studied countries (United States, China, South Africa, India, and Brazil) account for a disproportionate amount of data [CITE: `visualize_extraction_results_manuscript_log.txt`]. Furthermore, the completeness of this information is inconsistent: while 89.2% of abstracts detailed research methods, only 72.5% contained extractable geographic information [CITE: `visualization_summary_report_main.txt`]. A temporal analysis also shows that while research methods have modernized, with molecular techniques now used in **36.8%** of studies, these geographic and taxonomic biases are not historical artifacts; they have remained largely consistent over the past three decades [CITE: `temporal_trend_analysis.R`, `visualization_summary_report_main.txt`].

## Discussion
Our analysis provides the first quantitative confirmation that fungal endophytes are ubiquitous *wherever they have been studied*. The data-driven finding that 99.5% of studies report presence, combined with our validation that 99% of "absence" reports are artifacts, provides overwhelming support for the ubiquity paradigm *within the current scope of research*.

However, the central finding of this paper is the stark contradiction between this observed ubiquity and the severe sampling biases that define the field. The 0.8% of plant species and 77% concentration in the Global North are not minor gaps; they are systematic biases that fundamentally limit any claims of true universality. Our "ubiquitous" paradigm is a generalization from a non-random, convenience-based sample of the plant kingdom.

This 77% concentration of research in the Global North has profound, practical consequences. Our global search for novel pharmaceuticals (El-Shora & El-Sayed, 2023), biopesticides, and enzymes from endophytes is, by extension, also concentrated in these same regions. We are, in effect, ignoring the 99.5% of plant biodiversity that is primarily located in the planet's most biodiverse, yet least-studied, regions as a potential source for these critical compounds. This finding reframes the research bias from a simple academic gap to a significant bottleneck in global bioprospecting and biotechnology.

We have not tested if "all plants host endophytes." We have demonstrated that "all plants *studied* host endophytes." This shifts the grand challenge for the field: from assuming ubiquity to actively exploring the vast unknown, particularly the biodiversity hotspots of the Global South.

### Differentiating "Absence" from "Study-Level Negatives"
A key challenge in testing the ubiquity paradigm is the interpretation of "absence" reports. Our ML pipeline, for instance, identified 89 studies reporting endophyte absence, and literature reviews (e.g., Lau, Johnson, & Gehring, 2013) note specific cases where plants were found to be endophyte-free.

However, our rigorous, multi-step validation (detailed in Methods) provides the necessary context. We found 88 of these 89 cases were methodological artifacts. More importantly, in cases of apparent genuine absence from a single study, other studies *consistently* report endophyte presence in the very same taxon (or even species).

Therefore, we must differentiate between a **"study-level negative"** (a single report of non-detection) and a **"taxon-level absence"** (a species or lineage that is *conclusively* and *repeatedly* shown to be endophyte-free). Our analysis finds that while "study-level negatives" exist, **we find no evidence for a single "taxon-level absence"** in the plant kingdom. This reframes the paradigm: a single 'absence' report does *not* invalidate ubiquity at the taxon level.

### Limitations of Scope
We acknowledge that this analysis is based on data extracted from scientific abstracts, not full-text articles. This is a standard and necessary trade-off for a meta-analysis of this scale. This may result in an under-counting of species, as some abstracts may not list every plant species studied. Therefore, our 0.8% coverage figure should be viewed as a conservative, data-driven estimate representing the *minimum* known boundary of the field.

The apparent universality of endophytes represents both a remarkable biological phenomenon and a sobering reminder of the limits of scientific knowledge. As we face unprecedented environmental challenges requiring comprehensive understanding of plant-microbe interactions, this work underscores the critical importance of expanding research beyond traditional geographic and taxonomic boundaries. Only through such expansion can we transform "everywhere we have looked" into truly global knowledge.

## Methods
Our methodology consisted of a three-stage computational pipeline: (1) ML-based abstract classification, (2) a multi-step validation of "Absence" classifications, and (3) a validated data extraction pipeline for taxonomic and geographic information. Full details, from data sourcing to validation, are provided in the Supplementary Methods.

### 1. ML Classification and Sensitivity
We developed a two-stage machine learning pipeline to classify abstracts. The first stage used a regularized logistic regression (`glmnet`) model to screen for relevance [CITE: `apply_models_to_full_dataset.R`]. This was followed by a component that identified and filtered abstracts focusing exclusively on mycorrhizal fungi [CITE: `01b_mycorrhizal_only.R`]. This resulted in the final corpus of 19,071 relevant, non-mycorrhizal-only abstracts.

For the primary presence/absence classification, we tested multiple models and determined that a weighted ensemble approach was optimal. This choice was part of our sensitivity analysis, as it allowed us to "prioritiz[e] absence detection" [CITE: `ML_compare_models_subset.R`], our rarest and most critical class. Our final model was a weighted ensemble of a linear-kernel Support Vector Machine (SVM) and a `glmnet` logistic regression.

Based on performance testing, we assigned differential weights to capitalize on each model's strengths: the final prediction used a weight of **0.6 for SVM-derived "Presence"** classifications and a higher weight of **0.8 for `glmnet`-derived "Absence"** classifications [CITE: `apply_models_to_full_dataset.R`]. The final 89.8% accuracy was determined using a 5-fold cross-validation on a manually labeled subset of 1,000 abstracts.

### 2. Pipeline Validation: Absence and Manual Review
We implemented a rigorous, multi-step process to validate all "Absence" classifications. First, we generated a comprehensive manual validation sample that included **100% of the 89 ML-classified "Absence" cases**, a stratified random sample of "Presence" cases, and known absence reports from the training data [CITE: `manual_validation_sample.R`].

Second, as an independent cross-check, we developed a rule-based string detection algorithm to scan all abstracts for explicit statements of absence (e.g., "endophyte-free," "no fungi were detected") [CITE: `absence_evidence_detection.R`]. The conclusion that 88 of 89 cases were artifacts was confirmed by both the manual expert validation and the lack of support from the independent string-detection algorithm.

### 3. Data Extraction Pipeline and Validation
We built a modular data extraction pipeline to parse taxonomic, geographic, and methodological information from the 19,071 relevant abstracts. The robustness of this pipeline was confirmed with a comprehensive validation suite.
* **Taxonomic Extraction:** Species name extraction was validated for accuracy, synonym resolution, and hierarchical consistency using a dedicated test script [CITE: `test_extract_species.R`].
* **Geographic Extraction:** Geographic locations were extracted and validated against a standardized country database. Test scripts confirmed robust handling of synonyms (e.g., 'USA', 'United States', 'US'), non-standard regions, and coordinate formats [CITE: `test_country_codes.R`, `test_enhanced_geography_detection.R`].
* **Mycorrhizal Filtering:** The pipeline component for identifying and filtering mycorrhizal-only studies was independently validated [CITE: `test_enhanced_mycorrhizal_output.R`].

### 4. Data Aggregation
Taxonomic information was standardized against the Global Biodiversity Information Facility (GBIF) backbone (GBIF.org, 2025). Geographic information was aggregated at the country level.

---

## References
Arnold, A. E., Maynard, Z., Gilbert, G. S., Coley, P. D., & Kursar, T. A. (2000). Are tropical fungal endophytes hyperdiverse?. *Ecology Letters*, 3(4), 267-274.

El-Shora, H. M., & El-Sayed, M. (2023). Endophytic fungi: a treasure trove of novel bioactive compounds for pharmaceutical and agricultural applications. *Symbiosis*, 79(3), 295-322.

GBIF.org (2025). *GBIF Home Page*. Retrieved from https://www.gbif.org

Lau, M. K., Johnson, N. C., & Gehring, C. A. (2013). Context-dependent shrub impacts on root-associated fungi. *Fungal Ecology*, 6(3), 209-218.

Rodriguez, R. J., White, J. F., Arnold, A. E., & Redman, R. S. (2009). Fungal endophytes: diversity and functional roles. *New Phytologist*, 182(2), 314-330.

Royal Botanic Gardens, Kew. (2016). *State of the World's Plants Report - 2016*.

Schulz, B., & Boyle, C. (2005). The endophytic continuum. *Mycological Research*, 109(6), 661-86.

---

## Supplementary Methods

### 1. Data Sourcing and Deduplication
The initial abstract corpus was generated by querying three major bibliographic databases: **Web of Science (WoS), Scopus, and PubMed** [CITE: `api_pull_abstracts.R`]. The search string was designed to capture fungal endophyte research broadly, using a query similar to: `( (endophyte* AND (fungus OR fungi OR fungal OR mycota)) OR "latent fung*" ... )`.

This process yielded a combined dataset of **40,776 abstracts** (`create_figure1a.R` data). This corpus was subjected to a rigorous, multi-stage deduplication pipeline [CITE: `Combo_abstracts_pull2.R`]:
1.  Standardization of column names (e.g., Title, Abstract, DOI) across sources.
2.  Deduplication by **DOI**, reducing the set to **23,321** abstracts.
3.  Deduplication by **normalized abstract text**, reducing the set to **23,007** abstracts.
4.  Filtering to include **"Article"** document types only, reducing the set to **22,587** abstracts.
5.  Final deduplication by **normalized title**, resulting in the **21,891** abstract corpus that was fed into the ML pipeline.

### 2. ML Classification Pipeline
The 21,891 abstracts were processed by a two-stage machine learning pipeline in R.
* **Relevance Classification:** A regularized logistic regression (`glmnet`) model screened abstracts for relevance.
* **Mycorrhizal Filtering:** A dedicated component [CITE: `01b_mycorrhizal_only.R`] identified and filtered abstracts focusing exclusively on mycorrhizal fungi. This step produced the final corpus of **19,071** relevant, non-mycorrhizal-only abstracts (from 1,436 abstracts flagged as mycorrhizal-only [CITE: `visualize_taxa_results_manuscript_log.txt`]).
* **Presence/Absence Classification:** This corpus was classified using a custom-weighted ensemble of a `glmnet` model and a linear-kernel Support Vector Machine (`svmLinear`). As documented in `ML_compare_models_subset.R`, this ensemble allowed us to "prioritiz[e] absence detection." The final model applied a weight of 0.6 to the SVM's "Presence" prediction and 0.8 to the `glmnet` "Absence" prediction [CITE: `apply_models_to_full_dataset.R`]. All models were validated using 5-fold cross-validation.

### 3. Computational Optimization
The pipeline was engineered for high-throughput and robust processing of large text datasets.
* **Error Handling:** The pipeline featured a robust error handling system, including "safe execution wrappers" and data recovery from backups, to prevent catastrophic failures during long-running processes [CITE: `error_handling.R`].
* **Memory Optimization:** We employed memory-efficient techniques, such as **chunked processing** of large files, data structure optimization (e.g., converting strings to factors), and aggressive garbage collection, to ensure the pipeline could run on standard hardware [CITE: `memory_optimization.R`].
* **Optimized Lookups:** We used **hash-table optimization** and **pre-computed lookup tables** for O(1) (instant) lookups of species names [CITE: `02_precompute_lookup_tables.R`, `README_01_extract_species.md`].
* **Bloom Filters:** We implemented DuckDB-based bloom filters for probabilistic pre-filtering [CITE: `bloom_filter_duckdb.R`], which allowed the pipeline to instantly reject 80-90% of non-matching taxonomic strings, dramatically reducing expensive validation queries.

### 4. Modular Extraction Pipeline
The data extraction pipeline was built as a series of modular, independent components [CITE: `README.md`].
1.  **Species & Mycorrhizal Extraction:** The first component extracted all plant and fungal taxonomic names and classified fungi for mycorrhizal status [CITE: `01_species_mycorrhizal_hpc_sequential.R`].
2.  **Mycorrhizal-Only Filtering:** A dedicated script assessed abstracts and flagged those containing *only* mycorrhizal fungi for exclusion [CITE: `01b_mycorrhizal_only.R`].
3.  **Component Extraction:** Subsequent components ran on the filtered abstract list to extract **research methods** [CITE: `02_extract_methods.R`], **plant parts** [CITE: `03_extract_plant_parts.R`], and **geographic locations** [CITE: `04_extract_geography.R`].
4.  **Final Aggregation:** A final script merged the outputs from all components into a single comprehensive dataset [CITE: `05_merge_results.R`].

### 5. Pipeline Performance and Visualization
* **Profiling:** The pipeline's performance was comprehensively profiled [CITE: `profile_species.R`], confirming a high throughput of ~100-500 abstracts/second.
* **Visualization:** All data visualizations were generated in R using `ggplot2`, employing custom, **colorblind-friendly palettes** and themes to ensure accessibility and consistency [CITE: `plot_utils.R`, `endo_palette`].

### 6. Code Reproducibility
The codebase includes a comprehensive framework to ensure validation and reproducibility, including scripts to generate representative test subsets [CITE: `create_test_subset.R`] and "quick start" test scripts (`quick_start_testing.R`) to automatically execute the entire pipeline on these subsets.

### 7. Detailed Validation Protocols
* **Manual Classification Validation:** We created two distinct samples for manual review [CITE: `manual_validation_sample.R`]. The **Absence Sample** included 100% of all ML-classified absence cases. The **Presence Sample** was a stratified random sample.
* **Extraction Validation Suite:** The data extraction components were validated using a comprehensive test suite [CITE: `test_extract_species.R`] that evaluated "core functionality, accuracy, performance, and robustness" by calculating a weighted "Overall score."

---
## Notes on Final Figures
**Figure 1: Pipeline and Temporal Bias (New Recommendation)**
* **Strategy:** Combine the PRISMA flow chart and the temporal trend plot into a multi-panel **Figure 1**. This is a high-impact opening that shows (a) the data source and (b) that the bias problem is current and persistent [N62].
* **(a) PRISMA Panel:** Use `create_figure1a.R`. Make circle areas proportional to abstract counts [N55] and label the 19,071 step as "(excluding 1,436 mycorrhizal-only studies)" [CITE: `visualize_taxa_results_manuscript_log.txt`].
* **(b) Temporal Panel:** Use `temporal_trend_analysis.R`. Plot publication volume over time, overlaid with the geographic bias (e.g., % Global North), to visually confirm the bias is persistent.

**Figure 2: Geographic Bias (World Map)**
* **Script:** `visualize_extraction_results.R`
* **Confirmation:** The log file `visualize_extraction_results_manuscript_log.txt` confirms all co-author feedback is **complete**:
    * **Projection:** Changed to **Robinson** [N41, N47].
    * **Legend:** Explicitly states **"Gray = 0 studies"** [CAG34].
    * **Scale:** Uses standard log breaks **(1, 10, 100, 1000)** [N42R41].
* **Action:** Consider removing the redundant bar chart [N43].

**Figure 3: Taxonomic Bias (Treemap)**
* **Script:** `visualize_taxa_results.R`
* **Action:** The log file `visualize_taxa_results_manuscript_log.txt` provides the exact numbers needed. The figure must be updated to include the species counts (e.g., "Tracheophyta: 3,150/363,445") to give context, as requested by [CAG24R23].
---