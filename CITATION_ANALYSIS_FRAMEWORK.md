# Citation Analysis Framework: Tracking the "Universal Endophyte" Claim

## Project: Testing the Universality of Fungal Endophytes in Plants
**Principal Investigator**: B. Bock  
**Institution**: Northern Arizona University  
**Date**: July 31, 2025

---

## Objective

Systematically track how the claim that "all plants harbor fungal endophytes" propagates through the scientific literature, assess its evidential basis, and document how this assertion has evolved over time.

---

## Research Questions

### Primary Questions
1. **Origin**: What are the original source(s) of the universality claim?
2. **Evidence**: What empirical evidence actually supports the universality assertion?
3. **Propagation**: How has the claim spread through the literature over time?
4. **Evolution**: Has the claim become stronger, weaker, or more nuanced through citation?

### Secondary Questions
1. **Citation Patterns**: How often is the claim properly cited vs stated without reference?
2. **Context Shifts**: How does the claim change meaning across different research contexts?
3. **Counter-Evidence**: How is contradictory evidence (absence findings) handled in relation to the claim?
4. **Disciplinary Differences**: Does the claim vary across different research fields or journals?

---

## Methodology

### Phase 1: Claim Identification and Extraction

#### Automated Text Mining Approach
**Search Targets in Abstracts/Full Text:**
- "all plants" + "endophyte*"
- "every plant*" + "endophyte*" 
- "universal*" + "endophyte*"
- "ubiquitous*" + "endophyte*"
- "plants universally" + "endophyte*"
- "all plant species" + "endophyte*"
- "without exception" + "plant*" + "endophyte*"

**Pattern Matching for Common Formulations:**
- "All plants harbor endophytic fungi"
- "Endophytes are found in all plants"
- "Every plant species contains endophytes"
- "Fungal endophytes are universally present in plants"
- "No plant species lacks endophytes"

#### Manual Review Protocol
1. **Sample Validation**: Manual review of automated extraction results (sample size: 200 abstracts)
2. **Context Assessment**: Determine if statement is:
   - Direct assertion of universality
   - Qualified statement (e.g., "most plants")
   - Citation of universality claim
   - Counter-statement questioning universality
3. **Evidence Status**: Classify as:
   - Supported by citation
   - Supported by study data
   - Unsupported assertion
   - Contradicted by evidence

### Phase 2: Citation Network Analysis

#### Forward Citation Tracking
**Starting Points**: Identify papers making strong universality claims
**Method**: 
1. Use Web of Science/Scopus citation databases
2. Track all papers citing universality-claiming papers
3. Assess how citing papers use/modify the claim
4. Create citation network maps

#### Backward Citation Tracking
**Starting Points**: Papers making universality claims
**Method**:
1. Examine all references cited in support of universality
2. Assess whether cited papers actually support the claim
3. Identify original empirical sources
4. Document citation accuracy and context

#### Key Metrics to Track
- **Citation Frequency**: How often is each source cited for universality?
- **Citation Accuracy**: Do citations actually support the universality claim?
- **Claim Strength**: Does the claim become stronger/weaker through citation chains?
- **Evidence Decay**: Distance between empirical evidence and current claims

### Phase 3: Temporal Analysis

#### Historical Progression
1. **Timeline Construction**: Map when universality claims first appeared
2. **Strength Evolution**: Track how claim certainty changes over time
3. **Evidence Accumulation**: Assess if supporting evidence increases over time
4. **Challenge Documentation**: Identify when/how the claim is questioned

#### Periodization Analysis
- **Pre-molecular era** (pre-1990s): Claims based on culture studies
- **Early molecular era** (1990s-2000s): PCR-based detection expansion
- **High-throughput era** (2000s-2010s): Increased detection sensitivity
- **Microbiome era** (2010s-present): Community-level perspectives

### Phase 4: Evidence Synthesis

#### Claim vs Evidence Matrix
**Structure**: Cross-tabulate universality claims against empirical support
- **Strong Claims + Strong Evidence**: Well-supported assertions
- **Strong Claims + Weak Evidence**: Potentially unsupported claims
- **Weak Claims + Strong Evidence**: Conservative interpretations
- **Strong Claims + Counter-Evidence**: Problematic assertions

#### Quality Assessment
**Citation Quality Metrics**:
- Accuracy (does citation support claim?)
- Specificity (how precise is the supporting evidence?)
- Recency (age of supporting evidence)
- Independence (circular citation detection)

---

## Implementation Strategy

### Phase 1 Implementation (Immediate)
1. **Text Mining Setup**: Develop search patterns for automated claim detection
2. **Manual Validation**: Review and refine automated detection accuracy
3. **Database Preparation**: Set up tracking database for claims and citations
4. **Pilot Testing**: Run on subset of literature to refine methods

### Phase 2 Implementation (Short-term)
1. **Citation Database Access**: Ensure access to Web of Science/Scopus citation data
2. **Network Analysis Tools**: Set up software for citation network mapping
3. **Systematic Tracking**: Begin forward/backward citation analysis
4. **Data Integration**: Link citation data with claim extraction results

### Phase 3 Implementation (Medium-term)
1. **Temporal Database**: Organize data by publication year for trend analysis
2. **Statistical Analysis**: Apply time-series analysis to claim evolution
3. **Visualization**: Create timelines and network diagrams
4. **Pattern Recognition**: Identify key inflection points in claim propagation

---

## Expected Outputs

### Quantitative Results
- **Claim Frequency**: Number of papers asserting universality vs questioning it
- **Citation Accuracy**: Proportion of citations that actually support universality claims
- **Network Metrics**: Key papers/authors driving claim propagation
- **Temporal Trends**: Changes in claim frequency and strength over time

### Qualitative Insights
- **Original Sources**: Identification of foundational papers making universality claims
- **Evidence Gaps**: Areas where claims exceed empirical support
- **Context Shifts**: How claims change meaning across research contexts
- **Counter-Narratives**: Documentation of papers questioning universality

### Visualizations
- **Citation Networks**: Maps showing claim propagation pathways
- **Timeline Plots**: Evolution of claim strength over time
- **Evidence-Claim Matrix**: Relationship between assertions and supporting data
- **Geographic/Institutional Patterns**: Where universality claims originate and spread

---

## Integration with Main Research

### Connection to Absence Analysis
- **Claim Context**: How do papers reporting absence handle universality claims?
- **Evidence Tension**: How are negative results reconciled with universality assertions?
- **Methodological Implications**: Do universality claims affect study design or interpretation?

### Bias Documentation
- **Publication Bias**: Does belief in universality discourage publication of negative results?
- **Confirmation Bias**: Do universality claims affect how results are interpreted?
- **Citation Bias**: Are papers questioning universality cited less frequently?

### Research Impact Assessment
- **Field Influence**: How has the universality assumption shaped endophyte research?
- **Funding Implications**: Has belief in universality affected research priorities?
- **Educational Impact**: How is the universality claim transmitted in textbooks and teaching?

---

## Quality Control and Validation

### Automated Extraction Validation
- **Manual Review**: Expert validation of 10% of automatically extracted claims
- **Inter-rater Reliability**: Multiple reviewers assess ambiguous cases
- **Context Verification**: Ensure claims are extracted with appropriate context

### Citation Analysis Validation
- **Accuracy Checking**: Verify that cited papers actually support universality claims
- **Completeness Assessment**: Ensure comprehensive capture of citation networks
- **Temporal Validation**: Cross-check publication dates and citation relationships

### Bias Mitigation
- **Multiple Databases**: Use both Web of Science and Scopus for broader coverage
- **Language Considerations**: Acknowledge English-language bias in citation analysis
- **Field Representation**: Ensure coverage across different research areas and journals

---

## Timeline and Resources

### Phase 1 (Months 1-2): Claim Detection
- [ ] Develop and test automated extraction methods
- [ ] Manual validation of extraction accuracy
- [ ] Create claim database and classification system

### Phase 2 (Months 2-4): Citation Analysis
- [ ] Systematic forward/backward citation tracking
- [ ] Network analysis and visualization
- [ ] Evidence-claim relationship assessment

### Phase 3 (Months 4-5): Temporal Analysis
- [ ] Historical progression mapping
- [ ] Trend analysis and inflection point identification
- [ ] Integration with main research findings

### Phase 4 (Months 5-6): Synthesis and Reporting
- [ ] Comprehensive analysis of claim propagation patterns
- [ ] Integration with absence evidence analysis
- [ ] Manuscript preparation and figure creation

---

## Success Metrics

### Quantitative Targets
- Identify >100 papers making explicit universality claims
- Map citation networks for top 20 most-cited universality sources
- Achieve >90% accuracy in automated claim detection
- Document temporal trends across >30 years of literature

### Qualitative Goals
- Identify original source(s) of universality claims
- Document evidence gaps in claim support
- Assess impact of claims on research direction
- Provide framework for evidence-based claim evaluation

---

## Ethical Considerations

### Responsible Critique
- **Constructive Analysis**: Focus on improving scientific understanding, not attacking individuals
- **Context Sensitivity**: Acknowledge that universality claims may have been reasonable given historical evidence
- **Balanced Reporting**: Present both supporting and contradicting evidence fairly

### Transparency
- **Methodology Documentation**: Full transparency in claim identification and analysis methods
- **Data Availability**: Make claim database available for verification and extension
- **Limitation Acknowledgment**: Clear discussion of analysis limitations and potential biases

---

**Document Status**: Implementation framework  
**Last Updated**: July 31, 2025  
**Implementation Start**: August 2025 (proposed)  
**Review Schedule**: Monthly during implementation  
**Approval**: B. Bock, Principal Investigator

---

*This framework provides a systematic approach to understanding how scientific claims propagate through literature and assessing the relationship between assertions and empirical evidence in the field of endophyte research.*
