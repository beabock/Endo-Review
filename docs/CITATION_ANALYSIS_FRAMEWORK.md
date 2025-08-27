# Citation Analysis Framework

## Project: Testing the Universality of Fungal Endophytes in Plants
**Framework for analyzing citation patterns in endophyte literature**

## Research Context

### The "Universality Claim" Problem
The claim that "all plants harbor fungal endophytes" appears frequently in endophyte literature but often:
- Lacks proper citation to supporting evidence
- References studies with limited taxonomic scope
- Perpetuates through circular citation patterns
- May influence research bias toward "presence" findings

### Citation Analysis Objectives
1. **Trace the origin** of universality claims in the literature
2. **Identify highly cited papers** that shape field assumptions
3. **Analyze citation patterns** around absence vs presence studies
4. **Document knowledge claims** and their evidential support

## Citation Data Sources

### Bibliometric Databases
- **Web of Science**: Citation counts, citing papers, reference lists
- **Scopus**: Alternative citation metrics, broader journal coverage
- **Google Scholar**: Additional citation coverage, gray literature

### Data Fields to Extract
- **Forward Citations**: Papers citing each study
- **Backward Citations**: References cited by each study
- **Citation Context**: How the study is cited (methods, results, claims)
- **Self-Citations**: Author overlap between citing/cited papers

## Key Papers for Citation Analysis

### Foundational Reviews (High Impact)
1. **Rodriguez et al. (2009)** - "Fungal endophytes: diversity and functional roles"
2. **Porras-Alfaro & Bayman (2011)** - "Hidden fungi, emergent properties"  
3. **Hardoim et al. (2015)** - "The hidden world within plants"
4. **Vandenkoornhuyse et al. (2015)** - "The importance of the microbiome"

### Potential "Universality" Sources
- Early papers making broad claims about endophyte distribution
- Review papers that may have amplified limited evidence
- Highly cited methodological papers that could bias detection

## Citation Analysis Methods

### Quantitative Citation Metrics
```r
# Example analysis framework
citation_analysis <- function(paper_list) {
  # Extract citation counts by category
  presence_papers %>% summarise(mean_citations = mean(citation_count))
  absence_papers %>% summarise(mean_citations = mean(citation_count))
  
  # Temporal citation patterns
  citations_over_time %>% 
    group_by(year, study_type) %>%
    summarise(total_citations = sum(citations))
    
  # Citation impact by result type
  impact_factor_by_result_type()
}
```

### Qualitative Citation Context Analysis
- **Methods Citations**: Studies cited for methodology only
- **Results Citations**: Studies cited for their findings
- **Claims Citations**: Studies cited to support broad claims
- **Counter-Evidence**: How absence studies are cited

### Citation Network Analysis
- **Co-citation patterns**: Papers frequently cited together
- **Citation clusters**: Research communities and schools of thought
- **Influential nodes**: Papers that bridge different research areas
- **Isolation analysis**: Whether absence studies form separate citation networks

## Specific Research Questions

### Q1: Origin of Universality Claims
- Which papers first made claims about endophyte universality?
- How has the strength of these claims changed over time?
- What evidence was originally cited to support universal claims?

### Q2: Citation Bias Patterns
- Do presence studies receive more citations than absence studies?
- Are absence studies more likely to be ignored or dismissed?
- How do citation patterns vary by journal impact factor?

### Q3: Knowledge Propagation
- How do unsupported claims spread through the literature?
- Which review papers have been most influential in shaping field beliefs?
- Are there citation cascades that amplify weak evidence?

### Q4: Geographic/Taxonomic Citation Bias
- Are studies from certain regions or taxa cited more frequently?
- Do Global North vs Global South studies show citation disparities?
- How do model organism studies influence broader claims?

## Analysis Workflow

### Phase 1: Citation Data Collection
```r
# Extract citation data for all classified abstracts
citation_data <- abstracts_classified %>%
  filter(prediction == "Presence" | prediction == "Absence") %>%
  pull(doi) %>%
  map_dfr(get_citation_metrics)
```

### Phase 2: Citation Impact Analysis
```r
# Compare citation patterns by study outcome
citation_comparison <- citation_data %>%
  group_by(study_outcome) %>%
  summarise(
    mean_citations = mean(citation_count),
    median_citations = median(citation_count),
    citation_inequality = gini_coefficient(citation_count)
  )
```

### Phase 3: Temporal Citation Trends
```r
# Track how citation patterns change over time
temporal_citation_analysis <- function() {
  # Citation accumulation curves
  # Peak citation years
  # Half-life of citation impact
  # Emerging vs declining research areas
}
```

### Phase 4: Network Analysis
```r
# Build citation networks
library(igraph)
citation_network <- graph_from_data_frame(
  edges = citation_relationships,
  vertices = paper_metadata
)

# Network metrics
- Centrality measures (betweenness, closeness, eigenvector)
- Community detection
- Bridge papers connecting different research streams
```

## Expected Outcomes

### For Manuscript Writing
1. **Quantitative evidence** of citation bias patterns
2. **Historical analysis** of claim propagation  
3. **Network visualization** of research communities
4. **Documentation** of evidential gaps in universal claims

### Key Figures for Publication
- **Citation inequality plots**: Presence vs absence studies
- **Citation network diagram**: Research community structure  
- **Temporal trends**: How universality claims have evolved
- **Geographic citation bias**: Map of citation patterns

### Supporting Analyses
- **H-index comparison**: Presence vs absence researchers
- **Journal impact factors**: Where different study types publish
- **Funding acknowledgments**: Resource availability patterns
- **Author geographic affiliations**: Global research capacity

## Tools and Software

### R Packages for Citation Analysis
```r
library(bibliometrix)    # Bibliometric analysis
library(igraph)         # Network analysis  
library(ggplot2)        # Visualization
library(tidytext)       # Text analysis of citation contexts
library(rcrossref)      # CrossRef API access
library(roadoi)         # Open access data
```

### External Tools
- **VOSviewer**: Citation network visualization
- **Gephi**: Advanced network analysis
- **Zotero/Mendeley**: Reference management and organization

## Timeline for Citation Analysis

### Immediate (Current Sprint - August 2025)
- Extract DOIs from classified abstracts
- Begin citation data collection via APIs
- Identify top-cited papers in dataset

### Phase 2 (September 2025)  
- Complete citation network construction
- Quantitative citation bias analysis
- Temporal trend analysis

### Phase 3 (October 2025)
- Citation context analysis (how papers are cited)
- Network visualization and community detection
- Integration with geographic/taxonomic analyses

### Manuscript Integration (November 2025)
- Citation analysis results in Methods section
- Key findings in Results
- Citation bias discussion in Discussion
- Network figures in main text or supplement

## Quality Control and Validation

### Citation Data Validation
- **Cross-check** citation counts across databases
- **Manual verification** of high-impact papers
- **Temporal consistency** checks for citation accumulation

### Bias Mitigation
- **Multiple databases** to avoid single-source bias
- **Self-citation exclusion** for impact calculations
- **Journal impact factor normalization** for fair comparison

---

**Framework Status**: Ready for implementation  
**Expected Completion**: October 2025  
**Integration Target**: Manuscript submission November 2025
