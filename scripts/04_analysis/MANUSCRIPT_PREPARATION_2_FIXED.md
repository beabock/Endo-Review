# Manuscript Preparation - Version 21 (Final Polish)

**Authors:** B. Bock, N. McKay, N.C. Johnson, C.A. Gehring
**Goal journal:** Nature
**Title:** The Sparsely Sampled Ubiquity of Global Fungal Endophytes

## Abstract
The paradigm that "all plants harbor fungal endophytes" is a foundational assumption in plant ecology, yet this claim has never been systematically tested at a global scale. Here, we use a machine learning pipeline to analyze 19,071 research abstracts spanning nearly a century. We developed a high-sensitivity screening tool to find and manually validate all potential claims of endophyte absence. This comprehensive validation revealed zero verifiable examples of an endophyte-free plant taxon within the entire scientific literature. This finding confirms that endophyte ubiquity is a robust pattern *wherever* plants have been studied. However, we show that this "ubiquitous" paradigm is a generalization from an extraordinarily biased sample: only 0.8% of described plant species have been examined, with 77% of all research concentrated in the Global North. Our analysis reframes a foundational paradigm, revealing that our understanding of a "global" symbiosis is based on a tiny fraction of biodiversity. This sampling bias represents a critical bottleneck in biodiversity research and our search for novel biotechnologies.

## Introduction
Fungal endophytes are fungi that inhabit plant tissues without causing apparent disease. This symbiosis, considered one of the most abundant on earth, influences plant ecology and evolution and enhances plant stress tolerance (Rodriguez et al., 2009). Foundational reviews have established the paradigm that this symbiosis is ubiquitous, stating that "all plants" host fungal endophytes (e.g., Arnold et al., 2000; Stone et al., 2000). This idea has become a central, defining assumption in ecology, underpinning research on endophyte function, diversity, and transmission.

However, this assumption of ubiquity remains unvalidated, as a comprehensive, systematic evaluation of fungal endophyte distribution across plant taxa and regions is lacking. Previous reviews have been qualitative or focused on specific lineages, leaving the global pattern untested. Crucially, testing this paradigm requires distinguishing between a "study-level absence" (a failure to detect endophytes in a specific sample or experiment) and a "taxon-level absence" (a plant lineage that consistently lacks endophytes across multiple studies). While study-level absences are expected due to detection limits, a confirmed taxon-level absence would fundamentally challenge the ubiquity paradigm.

Here, we use a machine learning pipeline to conduct the first large-scale, quantitative test of endophyte ubiquity. We classify over 19,000 abstracts from the endophyte literature to map the taxonomic and geographic patterns of research and systematically test the claim of ubiquity against the evidence.

## Results
Our analysis of 19,071 relevant abstracts confirms that the endophyte literature is overwhelmingly a record of presence. Given that "true absence" is an exceptionally rare event, we designed a high-sensitivity screening pipeline (see Methods) to flag all potential absence candidates for manual review.

This pipeline reduced the 19,071 abstracts to a manageable set of 102 candidates. We then manually validated 100% of these cases. This validation revealed that none of these candidates represented a true, taxon-level absence. While we identified individual studies that failed to find endophytes (study-level absences), further investigation revealed that other studies published on the same plant taxon *do* report presence. Therefore, our comprehensive analysis found zero verifiable examples of an endophyte-free plant taxon in the entire corpus.

### A Biased View of Ubiquity
Despite this confirmed ubiquity, our analysis reveals that this conclusion rests on a tiny and deeply biased sample of plant biodiversity. We find that only 0.8% (3,226 of 390,101 species) of described vascular plant species have been examined for endophytes [CITE: `visualize_taxa_results_manuscript_log.txt`].

This bias systematically overlooks critical evolutionary lineages. For example, Charophyta (Stoneworts), the closest algal relatives to land plants, have 0.0% coverage (0 of 2,012 species studied) in our dataset. Similarly, Bryophyta (Mosses), representing some of the earliest land plants, have only 0.2% coverage (21 of 10,508 species).

Even within the relatively well-studied Tracheophyta (Vascular Plants)—which encompasses familiar groups like ferns, conifers, and flowering plants—coverage remains exceptionally low at 0.9% (3,150 of 363,445 species). This demonstrates that our knowledge of endophyte ubiquity is largely derived from a thin sliver of plant diversity.

This taxonomic bias is mirrored by an extreme geographic bias. Research is concentrated in the Global North, with 77% of all studies originating from Europe and North America. The top five most-studied countries (United States, China, South Africa, India, and Brazil) account for a disproportionate amount of data. A temporal analysis shows that these geographic and taxonomic biases are not historical artifacts; they have remained largely consistent over the past three decades, even as molecular methods have become standard [CITE: `temporal_trend_analysis.R`].

## Discussion
Our analysis provides the first large-scale, data-driven confirmation of the fungal endophyte ubiquity paradigm. By demonstrating that there is no evidence for a single endophyte-free plant taxon in the literature, we confirm that ubiquity is a robust pattern *wherever plants have been studied*.

However, the central finding of this paper is the stark contradiction between this observed ubiquity and the severe sampling biases that define the field. The 0.8% of plant species and 77% concentration in the Global North are not minor gaps; they are systematic biases that fundamentally limit our understanding. Our "ubiquitous" paradigm is a generalization from a non-random, convenience-based sample of the plant kingdom.

This concentration of research has profound practical consequences. Our global search for novel pharmaceuticals (El-Shora & El-Sayed, 2023), biopesticides, and enzymes is, by extension, also concentrated in these same regions. We are, in effect, ignoring the 99.2% of plant biodiversity—including evolutionarily distinct groups like Stoneworts and Mosses—as potential sources for these critical compounds.

### From "Study-Level Negatives" to "Taxon-Level Ubiquity"
A key challenge in testing ubiquity is the interpretation of "absence" reports. Our screening pipeline flagged 102 abstracts that mentioned absence. Manual validation clarified that these were either irrelevant (40), simple false positives (46), or reports of mixed results (15) where endophytes were found in some tissues but not others.

We identified only a single genuine "study-level absence" in the literature: a study reporting no fungal endophytes in *Phragmites australis* (Lambert & Casagrande, 2006). However, this absence is contradicted by numerous other studies that confirm diverse fungal endophyte communities in the same host species (e.g., Wirsel et al., 2001; Angelini et al., 2012; Clay et al., 2016). This specific case perfectly illustrates our core distinction: while individual studies may fail to detect endophytes due to methodology, season, or tissue sampling ("study-level negatives"), "taxon-level ubiquity" holds true. We found no evidence of a plant taxon that is consistently free of fungal endophytes.

### Limitations of Scope
We acknowledge that this analysis is based on data extracted from scientific abstracts, not full-text articles. This is a standard trade-off for a meta-analysis of this scale. This may result in an under-counting of species, as some abstracts may not list every plant species studied. Therefore, our 0.8% coverage figure should be viewed as a conservative, data-driven estimate representing the *minimum* known boundary of the field.

## Methods
Our methodology consisted of a three-stage computational pipeline: (1) ML-based abstract classification, (2) a multi-step validation of "Absence" classifications, and (3) a validated data extraction pipeline for taxonomic and geographic information. Full details, from data sourcing to validation, are provided in the Supplementary Methods.

### 1. ML Classification as a High-Sensitivity Screen
We developed a two-stage machine learning pipeline. The first stage screened for relevance and filtered mycorrhizal-only abstracts [CITE: `01b_mycorrhizal_only.R`], resulting in the 19,071 abstract corpus.

For the primary classification, we faced an extreme imbalanced class problem: "Presence" was near-universal, and "Absence" was nearly non-existent. Therefore, we designed a high-sensitivity screening tool, not a high-precision classifier. We used a weighted ensemble of a linear-kernel SVM and a `glmnet` logistic regression. As documented in our model testing [CITE: `ML_compare_models_subset.R`], we intentionally weighted the model to "prioritiz[e] absence detection," creating a screen that would flag all potential candidates for manual review.

### 2. Pipeline Validation: Absence and Manual Review
We implemented a rigorous, multi-step process to validate all potential "Absence" classifications. First, our ML screen flagged 89 candidates. Second, an independent rule-based string detection algorithm [CITE: `absence_evidence_detection.R`] flagged 12 additional unique candidates. This created a final set of 102 abstracts for manual validation.

We manually reviewed 100% of these 102 abstracts. The results, detailed in the Supplementary Methods and documented in our `absence_validation_report.md` file, confirmed that zero represented a true, taxon-level absence.

### 3. Data Extraction Pipeline and Validation
We built a modular data extraction pipeline to parse taxonomic, geographic, and methodological information.
* Taxonomic Extraction: Species name extraction was validated for accuracy using a dedicated test suite [CITE: `test_extract_species.R`].
* Geographic Extraction: Geographic locations were extracted and validated against a standardized country database [CITE: `test_country_codes.R`].
* Mycorrhizal Filtering: The pipeline component for identifying and filtering mycorrhizal-only studies was independently validated [CITE: `test_enhanced_mycorrhizal_output.R`].

### 4. Data Aggregation
Taxonomic information was standardized against the Global Biodiversity Information Facility (GBIF) backbone (GBIF.org, 2025). Geographic information was aggregated at the country level.

---

## References
Angelini, P. et al. The endophytic fungal communities associated with the leaves and roots of the common reed (Phragmites australis) in Lake Trasimeno (Perugia, Italy) in declining and healthy stands. *Fungal Ecology* 5, 683–693 (2012).

Arnold, A. E., Maynard, Z., Gilbert, G. S., Coley, P. D., & Kursar, T. A. (2000). Are tropical fungal endophytes hyperdiverse?. *Ecology Letters*, 3(4), 267-274.

Bacon, C. W. & White, J. F. *Microbial Endophytes*. (M. Dekker, New York, 2000).

Clay, K., Shearin, Z., Bourke, K., Bickford, W. & Kowalski, K. Diversity of fungal endophytes in non-native Phragmites australis in the Great Lakes. *Biological Invasions* 18, 2703–2716 (2016).

El-Shora, H. M., & El-Sayed, M. (2023). Endophytic fungi: a treasure trove of novel bioactive compounds for pharmaceutical and agricultural applications. *Symbiosis*, 79(3), 295-322.

GBIF.org (2025). *GBIF Home Page*. Retrieved from https://www.gbif.org

Lambert, A. & Casagrande, R. No evidence of fungal endophytes in native and exotic Phragmites australis. *Northeastern Naturalist* 13, 561–568 (2006).

Lau, M. K., Johnson, N. C., & Gehring, C. A. (2013). Context-dependent shrub impacts on root-associated fungi. *Fungal Ecology*, 6(3), 209-218.

Rodriguez, R. J., White, J. F., Arnold, A. E., & Redman, R. S. (2009). Fungal endophytes: diversity and functional roles. *New Phytologist*, 182(2), 314-330.

Royal Botanic Gardens, Kew. (2016). *State of the World's Plants Report - 2016*.

Schulz, B., & Boyle, C. (2005). The endophytic continuum. *Mycological Research*, 109(6), 661-86.

Stone, J. K., Bacon, C. W. & White, F. An Overview of Endophytic Microbes: Endophytism Defined. in *Microbial Endophytes* 1–28 (CRC Press, 2000).

Strobel, G. A. Endophytes as sources of bioactive products. *Microbes and Infection* 5, 535–544 (2003).

U’Ren, J. M., Lutzoni, F., Miadlikowska, J., Laetsch, A. D. & Arnold, A. E. Host and geographic structure of endophytic and endolichenic fungi at a continental scale. *American Journal of Botany* 99, 898–914 (2012).

Wirsel, S. G. R., Leibinger, W., Ernst, M. & Mendgen, K. Genetic diversity of fungi closely associated with common reed. *New Phytologist* 149, 589–598 (2001).

---

## Supplementary Methods

### 1. Absence Validation Results
The high-sensitivity screening pipeline identified 102 candidate abstracts for manual validation (89 from the ML model, 12 from the string search, 1 from training). Manual expert review of these 102 abstracts yielded the following:
* 46 'Presence': Simple false positives (the abstract only reported presence).
* 15 'Both': Abstracts that reported mixed results (e.g., presence in leaves, absence in roots). These are considered 'Presence' at the taxon level.
* 40 'Irrelevant': Abstracts irrelevant to the study.
* 1 'Absence': A single study-level report of non-detection (Lambert & Casagrande, 2006).
This validation confirmed zero verifiable taxon-level absences in the corpus.

### 2. Ubiquity Statement Analysis
To quantify the nature of the "ubiquity paradigm," we ran a separate analysis to find explicit statements of ubiquity (e.g., "endophytes are ubiquitous") within the relevant literature [CITE: `find_all_plants_statement.R`]. The search terms were applied to 19,447 abstracts. This analysis found that only 38 abstracts (0.2%) contained such a statement, confirming the paradigm functions largely as an implicit assumption in the primary literature [CITE: `all_plants_statement_analysis.txt`].

### 3. Computational Optimization
The pipeline was engineered for high-throughput and robust processing of large text datasets.
* Error Handling: The pipeline featured a robust error handling system, including "safe execution wrappers" and data recovery from backups, to prevent catastrophic failures during long-running processes [CITE: `error_handling.R`].
* Memory Optimization: We employed memory-efficient techniques, such as chunked processing of large files, data structure optimization (e.g., converting strings to factors), and aggressive garbage collection, to ensure the pipeline could run on standard hardware [CITE: `memory_optimization.R`].
* Optimized Lookups: We used hash-table optimization and pre-computed lookup tables for O(1) (instant) lookups of species names [CITE: `02_precompute_lookup_tables.R`, `README_01_extract_species.md`].
* Bloom Filters: We implemented DuckDB-based bloom filters for probabilistic pre-filtering [CITE: `bloom_filter_duckdb.R`], which allowed the pipeline to instantly reject 80-90% of non-matching taxonomic strings, dramatically reducing expensive validation queries.

### 4. Modular Extraction Pipeline
The data extraction pipeline was built as a series of modular, independent components [CITE: `README.md`].
1.  Species & Mycorrhizal Extraction: The first component extracted all plant and fungal taxonomic names and classified fungi for mycorrhizal status [CITE: `01_species_mycorrhizal_hpc_sequential.R`].
2.  Mycorrhizal-Only Filtering: A dedicated script assessed abstracts and flagged those containing *only* mycorrhizal fungi for exclusion [CITE: `01b_mycorrhizal_only.R`].
3.  Component Extraction: Subsequent components ran on the filtered abstract list to extract research methods [CITE: `02_extract_methods.R`], plant parts [CITE: `03_extract_plant_parts.R`], and geographic locations [CITE: `04_extract_geography.R`].
4.  Final Aggregation: A final script merged the outputs from all components into a single comprehensive dataset [CITE: `05_merge_results.R`].

### 5. Pipeline Performance and Visualization
* Profiling: The pipeline's performance was comprehensively profiled [CITE: `profile_species.R`], confirming a high throughput of ~100-500 abstracts/second.
* Visualization: All data visualizations were generated in R using `ggplot2`, employing custom, colorblind-friendly palettes and themes to ensure accessibility and consistency [CITE: `plot_utils.R`, `endo_palette`].

### 6. Code Reproducibility
The codebase includes a comprehensive framework to ensure validation and reproducibility, including scripts to generate representative test subsets [CITE: `create_test_subset.R`] and "quick start" test scripts (`quick_start_testing.R`) to automatically execute the entire pipeline on these subsets.

### 7. Detailed Validation Protocols
* Manual Classification Validation: We created two distinct samples for manual review [CITE: `manual_validation_sample.R`]. The Absence Sample included 100% of all ML-classified absence cases. The Presence Sample was a stratified random sample.
* Extraction Validation Suite: The data extraction components were validated using a comprehensive test suite [CITE: `test_extract_species.R`] that evaluated "core functionality, accuracy, performance, and robustness" by calculating a weighted "Overall score."

---
## Notes on Final Figures

**Figure 1: Pipeline and Temporal Bias (New Recommendation)**
* Strategy: Combine the PRISMA flow chart and the temporal trend plot into a multi-panel Figure 1. This is a high-impact opening that shows (a) the data source and (b) that the bias problem is current and persistent [N62].
* (a) PRISMA Panel: Use `create_figure1a.R`. Make circle areas proportional to abstract counts [N55] and label the 19,071 step as "(excluding 1,436 mycorrhizal-only studies)" [CITE: `visualize_taxa_results_manuscript_log.txt`].
* (b) Temporal Panel: Use `temporal_trend_analysis.R`. Plot publication volume over time, overlaid with the geographic bias (e.g., % Global North), to visually confirm the bias is persistent.

**Figure 2: Geographic Bias (World Map)**
* Script: `visualize_extraction_results.R`
* Confirmation: The log file `visualize_extraction_results_manuscript_log.txt` confirms all co-author feedback is complete:
    * Projection: Changed to Robinson [N41, N47].
    * Legend: Explicitly states "Gray = 0 studies" [CAG34].
    * Scale: Uses standard log breaks (1, 10, 100, 1000) [N42R41].
* Action: Consider removing the redundant bar chart [N43].

**Figure 3: Taxonomic Bias (Treemap)**
* Script: `visualize_taxa_results.R`
* Action: The log file `visualize_taxa_results_manuscript_log.txt` provides the exact numbers needed. The figure must be updated to include the species counts (e.g., "Tracheophyta: 3,150/363,445") to give context, as requested by [CAG24R23].