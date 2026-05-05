# Manuscript Outline (Structure Only)

## Working Title Options
1. Global Sampling Bias in Fungal Endophyte Research: Where Ecology Is Missing
2. Mapping the Blind Spots of Endophyte Ecology Across Biomes, Hosts, and Economies
3. Fungal Endophyte Research Is Global in Claim but Local in Coverage

## Target Journal
New Phytologist

## Central Framing (1-2 sentences for Abstract and Introduction)
- Fungal endophytes are frequently described as ubiquitous, but that claim has rarely been evaluated with a global, quantitative synthesis.
- This manuscript is not a claim that endophytes are newly discovered as widespread; it is a first-pass global test of the ubiquity claim and a quantitative map of where ecological inference is currently credible versus where evidence is missing.
- The key advance is an integrated, reproducible gap analysis linking host taxonomy, biome context, geography, and economic asymmetry to define a priority sampling roadmap.

## Core Research Questions
1. Is the commonly repeated claim of fungal endophyte ubiquity supported by globally aggregated evidence?
1. Where are studies concentrated geographically, and where are major gaps?
2. How is sampling distributed across biomes and host plant lineages?
3. How strongly is research effort associated with national economic capacity?
4. Which host taxa and regions should be prioritized to maximize ecological discovery?

## Novelty Claims to Make Explicit
- Directly evaluates a field-wide, often repeated ubiquity claim with global-scale evidence aggregation.
- First synthesis that jointly analyzes geographic, biome, host taxonomic, and macroeconomic bias in fungal endophyte literature at this scale.
- Produces actionable priority lists (understudied taxa and countries) rather than only descriptive bibliometrics.
- Reframes the ecological question from "are endophytes ubiquitous" to "where can we currently test ecological mechanisms with sufficient evidence?"

## Manuscript Structure

## Abstract (Outline)
- Background: Fungal endophytes are often described as ubiquitous, yet this has not been tested with a truly global synthesis.
- Aim: Quantify and integrate geographic, biome, host taxonomic, and GDP-linked sampling bias.
- Methods: Reproducible literature-mining and metadata standardization workflow; bias metrics across country, biome, and host taxonomy.
- Main findings: Strong concentration in a limited set of countries/biomes; study effort tracks national wealth; substantial blind spots remain.
- Ecological implication: Current ecological generalizations are likely conditioned by sampling geography and host coverage.
- Deliverable: Priority roadmap of understudied countries and host taxa for future field and sequencing efforts.

## Introduction (Outline)
1. Why endophyte ecology needs global synthesis now.
2. The ubiquity claim as a central assumption in the literature, and why it needs explicit global testing.
2. Problem statement: ecological conclusions are vulnerable to sampling bias.
3. What prior work has done (regional/taxon-specific reviews) and what is still missing.
4. Gap this manuscript fills: first global test framing + integrated bias analysis + prioritization outputs.
5. Study objectives and predictions:
	- Prediction U: Current evidence is insufficiently global to support an unqualified ubiquity claim.
	- Prediction A: Research effort is geographically clustered.
	- Prediction B: Biome and host representation are non-random.
	- Prediction C: Study intensity increases with GDP.
6. Brief statement of scope limits (metadata/abstract-driven evidence) and why this is still informative.

## Methods (Outline)
1. Corpus Assembly and Inclusion Criteria
	- Data sources, deduplication, screening, final corpus definition.
2. Metadata Extraction and Standardization
	- Country assignment, host taxonomy harmonization, biome mapping.
3. Bias Metrics and Statistical Analyses
	- Country-level counts and concentration measures.
	- GDP-study relationship model.
	- Biome x country and host taxonomy representation analyses.
4. Sensitivity and Quality Checks
	- Steps taken to reduce false positives/metadata noise.
	- Caveat language for abstract-level inference.
5. Reproducibility
	- Scripts, outputs, and versioned pipeline summary.

## Results (Outline)
1. Evidence base for evaluating the ubiquity claim
	- Quantify scope and coverage limits of the globally aggregated record.
	- Position the rest of the Results as tests of where the ubiquity claim can and cannot currently be supported.

### Estimated prevalence of explicit ubiquity claims in the sampled literature

- 380 htis on google sholar on 5/5/26 with: "endophytic fungi are ubiquitous" OR "ubiquity of fungal endophytes" OR "fungal endophytes are ubiquitous" OR "all plants host fung*" OR "every plant species studied to date hosts"
- 7,600 hits on scholar on 5/5/26 with: ("all plants" OR "most plants" OR "every plant") AND "host" AND ("fungal endophytes" OR "endophytic fungi") 


1. Global geography of study effort
	- Show strong spatial concentration and zero/near-zero regions.
	- Figure: `study_count_by_country_robinson.png`

2. Biome-by-country imbalance
	- Show that study effort is not only geographically clustered, but biome-skewed within and across countries.
	- Figure: `biome_country_heatmap.png`

3. Economic gradient in research intensity
	- Quantify association between GDP and number of studies.
	- Figure: `country_study_count_vs_gdp.png`

4. Host taxonomy representation gaps
	- Identify concentrated sampling of certain host groups and missing branches.
	- Figure: `13_compound_taxonomy_heatmap.png`

5. Priority outputs for future sampling
	- Summarize top understudied countries and host taxa from pipeline outputs.
	- Tables: unstudied species/genera/families and unstudied countries.

## Discussion (Outline)
1. Main interpretation
	- The ubiquity claim may be plausible, but current evidence is too uneven for strong global generalization.

2. Ecological meaning (address this directly)
	- Biased sampling limits inference on host specificity, biome filtering, and broad functional claims.
	- Overrepresented geographies may inflate confidence in generality.

3. Why this is needed even if broad bias is expected
	- Quantification, explicit testing of a core field assumption, integration, and prioritization are the contribution, not merely noting bias exists.
	- Converts an intuitive claim into a testable resource for future work.

4. Reliability and scope limits
	- Abstract-level and indexing constraints.
	- Taxonomic and nomenclatural uncertainty handling.
	- Country-level aggregation caveat and biome context as partial remedy.

5. Practical roadmap
	- Propose targeted sampling design: underrepresented countries x underrepresented host lineages x under-sampled biomes.
	- Suggest minimum metadata standards for future synthesis-ready studies.

6. Conclusion
	- Endophyte ecology needs strategic expansion of sampling domains before stronger global ecological generalizations are made.

## Figure Plan (Main)
1. `study_count_by_country_robinson.png`
	- Role: Global baseline map of where evidence exists.
2. `biome_country_heatmap.png`
	- Role: Demonstrates ecological context imbalance beyond country counts.
3. `country_study_count_vs_gdp.png`
	- Role: Quantifies structural inequity in knowledge production.
4. `13_compound_taxonomy_heatmap.png`
	- Role: Shows host lineage concentration and missing taxonomic space.

## Figure Plan (Supplementary)
- `biome_trends_over_time.png`: temporal shifts in biome focus.
- `biome_family_heatmap.png`: family-level concentration within biomes.
- `14_family_trends_over_time.png`: temporal taxonomic dynamics.
- `tissue_trends_over_time.png`: changing tissue emphasis through time.
- `top_tissue_parts_by_study.png`: dominant tissue categories.
- `top_countries_ranked.png`: ranked concentration summary.

## Optional Web Supplement
- `interactive_study_density.html`
  - Keep as online supplementary exploration tool, not a core figure.
  - Mention briefly in Data/Code Availability and Supplementary Methods.

## Reviewer-Facing Positioning Notes (for cover letter and Discussion)
- Explicitly state that this is a bias-and-prioritization synthesis, not a full ecological mechanism paper.
- Emphasize conservative interpretation: outputs identify evidence distribution and missingness.
- Add a short paragraph on how future full-text and non-English expansion can refine estimates.

## Writing Guardrails for Drafting
- Avoid overclaims such as "global ubiquity is proven" from sparse host coverage.
- Prefer language like "evidence-supported in sampled domains" and "currently untested domains."
- Tie each major claim to one figure and one concrete quantitative result.
- Do not use em-dashes (--)
- Do not randomly quote or bold things.