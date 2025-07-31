# Citation Analysis Phase 1: Automated Universality Claim Detection
# Author: Systematic Review Analysis Team
# Date: January 2025
# Purpose: Identify papers that make claims about universal endophyte presence in plants

# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Function to create structured validation template
create_validation_template <- function(output_dir) {
  template_file <- file.path(output_dir, "validation_template.md")
  
  template_content <- '# Manual Validation Template: Universality Claims

## Instructions for Reviewers

### Overview
This template provides a structured approach to manually validate automatically detected universality claims about endophytes in plants.

### Validation Categories

#### 1. Manual Validation Status
- **CONFIRMED**: Clear universality claim detected correctly
- **FALSE_POSITIVE**: Automated detection error, no real universality claim
- **UNCLEAR**: Ambiguous statement requiring discussion

#### 2. Claim Strength Assessment
- **STRONG**: Unequivocal statements (e.g., "all plants harbor endophytes")
- **MODERATE**: Strong but slightly qualified (e.g., "virtually all plants")
- **WEAK**: Broad generalizations (e.g., "endophytes are ubiquitous")

#### 3. Claim Type Classification
- **fungal_only**: Specifically about fungal endophytes
- **bacterial_only**: Specifically about bacterial endophytes  
- **general_endophyte**: About endophytes in general
- **specific_context**: Limited to specific plants/environments

#### 4. Evidence Assessment
- **STRONG**: Multiple studies, comprehensive data
- **MODERATE**: Some supporting evidence cited
- **WEAK**: Limited or outdated evidence
- **NONE**: No supporting evidence provided

#### 5. Citation Context
- **introduction**: Background/literature review context
- **background**: Setting up study rationale
- **results**: Presenting study findings
- **discussion**: Interpreting results
- **conclusion**: Summary statements

### Review Workflow

1. **Read title and abstract carefully**
2. **Examine detected claim matches for accuracy**
3. **Assess claim in context of full abstract**
4. **Look for supporting citations or evidence**
5. **Fill in validation fields**
6. **Add detailed notes for unclear cases**

### Common False Positives to Watch For

- Study-specific results ("all our samples contained endophytes")
- Qualified statements ("many plants", "most species")
- Geographic/taxonomic limitations ("all tropical plants")
- Methodological statements ("all techniques detected endophytes")

### Quality Control

- Review high-confidence detections first
- Flag uncertain cases for secondary review
- Note potential citation chains for follow-up
- Document unusual claim formulations

---

## Example Validation Entry

**Paper ID**: 123  
**Title**: "Endophytic fungi in medicinal plants..."  
**Detected Claim**: "all plants harbor endophytic fungi"  

**Validation**:
- Manual Validation Status: CONFIRMED
- Claim Strength: STRONG  
- Claim Type: fungal_only
- Evidence Provided: WEAK
- Citation Context: introduction
- Supporting Citations: [None provided in abstract]
- Reviewer Notes: "Clear universality statement but no supporting evidence visible in abstract"
- Validation Date: 2025-07-31
- Reviewer Initials: BB

---

*Template Version: 1.0*  
*Last Updated: July 31, 2025*
'
  
  writeLines(template_content, template_file)
  cat("Validation template created:", template_file, "\n")
}

# Define function to detect universality claims
detect_universality_claims <- function(text) {
  if (is.na(text) || text == "") return(list(detected = FALSE, matches = character(0), confidence = 0))
  
  # Define universality claim patterns with confidence weights
  # NOTE: Includes both explicit "endophyte" references and general "fungi" statements
  # that may be referring to endophytic fungi in context
  universality_patterns <- list(
    # Strong universality claims (high confidence)
    strong = list(
      patterns = c(
        # Explicit endophyte claims
        "all plants (have|harbor|contain|host) endophyt",
        "every plant (has|harbors|contains|hosts) endophyt",
        "all plant species (have|harbor|contain|host) endophyt",
        "plants universally (have|harbor|contain|host) endophyt",
        "endophytes (are|occur) universally in plants",
        "endophytes (are|occur) ubiquitous in plants",
        "endophytes are found in all plants",
        "all plants are colonized by endophyt",
        "endophytes colonize all plants",
        "universal presence of endophytes",
        "ubiquitous presence of endophytes",
        
        # General fungi claims (may refer to endophytes)
        "all plants (have|harbor|contain|host) fungi",
        "every plant (has|harbors|contains|hosts) fungi",
        "all plant species (have|harbor|contain|host) fungi",
        "plants universally (have|harbor|contain|host) fungi",
        "fungi (are|occur) universally in plants",
        "fungi (are|occur) ubiquitous in plants",
        "fungi are found in all plants",
        "all plants are colonized by fungi",
        "fungi colonize all plants",
        "universal presence of fungi",
        "ubiquitous presence of fungi",
        
        # Fungal endophyte specific
        "all plants (have|harbor|contain|host) fungal endophyt",
        "every plant (has|harbors|contains|hosts) fungal endophyt",
        "fungal endophytes (are|occur) universally",
        "fungal endophytes are ubiquitous",
        
        # Alternative phrasings
        "no plant (is|exists) without endophyt",
        "no plant (is|exists) without fungi",
        "all plant.{0,20}(contain|harbor|host).{0,20}endophyt",
        "all plant.{0,20}(contain|harbor|host).{0,20}fungi",
        "endophyt.{0,30}present in all plant",
        "fungi.{0,30}present in all plant"
      ),
      weight = 1.0
    ),
    
    # Moderate universality claims (medium confidence)
    moderate = list(
      patterns = c(
        # Endophyte-specific moderate claims
        "most plants (have|harbor|contain|host) endophyt",
        "nearly all plants (have|harbor|contain|host) endophyt",
        "virtually all plants (have|harbor|contain|host) endophyt",
        "widespread presence of endophytes",
        "endophytes are common in all",
        "endophytes are prevalent in all",
        "endophytes are widespread in plants",
        "endophytes occur in most plants",
        "widespread occurrence of endophytes",
        
        # General fungi moderate claims
        "most plants (have|harbor|contain|host) fungi",
        "nearly all plants (have|harbor|contain|host) fungi",
        "virtually all plants (have|harbor|contain|host) fungi",
        "widespread presence of fungi in plants",
        "fungi are common in all plants",
        "fungi are prevalent in all plants",
        "fungi are widespread in plants",
        "fungi occur in most plants",
        "widespread occurrence of fungi",
        
        # Context-specific universal claims
        "all terrestrial plants (have|harbor|contain|host)",
        "all vascular plants (have|harbor|contain|host)",
        "all flowering plants (have|harbor|contain|host)"
      ),
      weight = 0.7
    ),
    
    # Weak universality claims (low confidence)
    weak = list(
      patterns = c(
        # General endophyte claims
        "endophytes are common",
        "endophytes are widespread",
        "endophytes are ubiquitous",
        "common occurrence of endophytes",
        "widespread distribution of endophytes",
        "endophytes are found in many",
        "endophytes occur in various",
        "diverse endophytic",
        "broad distribution of endophytes",
        
        # General fungi claims (potentially endophytic)
        "fungi are common in plants",
        "fungi are widespread in plants",
        "fungi are ubiquitous in plants",
        "common occurrence of fungi",
        "widespread distribution of fungi",
        "fungi are found in many plants",
        "fungi occur in various plants",
        "diverse fungal communities",
        "broad distribution of fungi",
        
        # Contextual indicators
        "fungi.{0,50}(internal|within|inside).{0,50}plant",
        "plant.{0,50}(associated|symbiotic).{0,50}fungi",
        "symbiotic fungi.{0,30}plants"
      ),
      weight = 0.3
    )
  )
  
  # Convert text to lowercase for matching
  text_lower <- tolower(text)
  
  # Check for matches
  all_matches <- character(0)
  total_confidence <- 0
  
  for (strength in names(universality_patterns)) {
    patterns <- universality_patterns[[strength]]$patterns
    weight <- universality_patterns[[strength]]$weight
    
    for (pattern in patterns) {
      if (str_detect(text_lower, pattern)) {
        all_matches <- c(all_matches, paste0("[", strength, "] ", pattern))
        total_confidence <- total_confidence + weight
      }
    }
  }
  
  # Calculate final confidence score (max 1.0)
  confidence_score <- min(total_confidence, 1.0)
  
  return(list(
    detected = length(all_matches) > 0,
    matches = all_matches,
    confidence = confidence_score
  ))
}

# Function to analyze abstracts for universality claims
analyze_universality_claims <- function(abstracts_file) {
  cat("Reading abstracts data...\n")
  abstracts <- read_csv(abstracts_file, show_col_types = FALSE)
  
  cat("Total papers in database:", nrow(abstracts), "\n")
  
  # Combine title and abstract text for analysis
  abstracts$combined_text <- paste(
    ifelse(is.na(abstracts$Title), "", abstracts$Title),
    ifelse(is.na(abstracts$Abstract), "", abstracts$Abstract),
    sep = " "
  )
  
  cat("Analyzing texts for universality claims...\n")
  
  # Apply claim detection to each paper
  claim_results <- abstracts %>%
    rowwise() %>%
    mutate(
      universality_analysis = list(detect_universality_claims(combined_text))
    ) %>%
    ungroup()
  
  # Extract results
  cat("Extracting and organizing results...\n")
  
  results <- claim_results %>%
    mutate(
      has_universality_claim = map_lgl(universality_analysis, ~ .x$detected),
      claim_matches = map_chr(universality_analysis, ~ paste(.x$matches, collapse = " | ")),
      confidence_score = map_dbl(universality_analysis, ~ .x$confidence),
      paper_id = row_number()
    ) %>%
    select(
      paper_id, Title, Authors, Year, 
      has_universality_claim, claim_matches, confidence_score,
      Abstract, DOI, `Times.Cited`, Source.title
    )
  
  # Filter papers with universality claims
  papers_with_claims <- results %>%
    filter(has_universality_claim == TRUE) %>%
    arrange(desc(confidence_score), desc(`Times.Cited`))
  
  # Summary statistics
  cat("\n=== UNIVERSALITY CLAIM DETECTION RESULTS ===\n")
  cat("Total papers analyzed:", nrow(results), "\n")
  cat("Papers with universality claims:", nrow(papers_with_claims), "\n")
  cat("Percentage with claims:", round(nrow(papers_with_claims)/nrow(results)*100, 2), "%\n")
  
  # Confidence score distribution
  cat("\nConfidence Score Distribution:\n")
  confidence_summary <- papers_with_claims %>%
    mutate(
      confidence_category = case_when(
        confidence_score >= 0.8 ~ "High (≥0.8)",
        confidence_score >= 0.5 ~ "Medium (0.5-0.79)",
        TRUE ~ "Low (<0.5)"
      )
    ) %>%
    count(confidence_category) %>%
    mutate(percentage = round(n/sum(n)*100, 1))
  
  print(confidence_summary)
  
  # Top cited papers with claims
  cat("\nTop 10 Most Cited Papers with Universality Claims:\n")
  top_cited <- papers_with_claims %>%
    filter(!is.na(`Times.Cited`)) %>%
    arrange(desc(`Times.Cited`)) %>%
    slice_head(n = 10) %>%
    select(Title, Authors, Year, `Times.Cited`, confidence_score)
  
  print(top_cited)
  
  return(list(
    all_results = results,
    papers_with_claims = papers_with_claims,
    summary_stats = list(
      total_papers = nrow(results),
      papers_with_claims = nrow(papers_with_claims),
      percentage_with_claims = round(nrow(papers_with_claims)/nrow(results)*100, 2),
      confidence_distribution = confidence_summary
    )
  ))
}

# Function to export results for manual validation
export_for_validation <- function(analysis_results, output_dir = "outputs") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Exporting results for manual validation...\n")
  
  # Export main results as CSV
  claims_csv <- file.path(output_dir, "universality_claims_detected.csv")
  
  # Prepare main data for validation
  validation_data <- analysis_results$papers_with_claims %>%
    mutate(
      # Add columns for manual validation
      manual_validation_status = "", # "CONFIRMED", "FALSE_POSITIVE", "UNCLEAR"
      claim_strength = "", # "STRONG", "MODERATE", "WEAK"
      claim_type = "", # "fungal_only", "bacterial_only", "general_endophyte", "specific_context"
      evidence_provided = "", # "STRONG", "MODERATE", "WEAK", "NONE"
      citation_context = "", # "introduction", "background", "results", "discussion", "conclusion"
      supporting_citations = "", # DOIs or references supporting the claim
      reviewer_notes = "",
      validation_date = "",
      reviewer_initials = ""
    ) %>%
    select(
      paper_id, Title, Authors, Year, Source.title, DOI, `Times.Cited`,
      confidence_score, claim_matches, 
      manual_validation_status, claim_strength, claim_type, evidence_provided, 
      citation_context, supporting_citations, reviewer_notes, 
      validation_date, reviewer_initials, Abstract
    )
  
  write_csv(validation_data, claims_csv)
  
  # Export summary statistics
  summary_csv <- file.path(output_dir, "detection_summary.csv")
  summary_stats <- data.frame(
    metric = c("total_papers", "papers_with_claims", "percentage_with_claims", 
               "high_confidence", "medium_confidence", "low_confidence"),
    value = c(
      analysis_results$summary_stats$total_papers,
      analysis_results$summary_stats$papers_with_claims,
      analysis_results$summary_stats$percentage_with_claims,
      nrow(filter(analysis_results$papers_with_claims, confidence_score >= 0.8)),
      nrow(filter(analysis_results$papers_with_claims, confidence_score >= 0.5 & confidence_score < 0.8)),
      nrow(filter(analysis_results$papers_with_claims, confidence_score < 0.5))
    )
  )
  write_csv(summary_stats, summary_csv)
  
  # Export high-priority papers for immediate review
  high_priority_csv <- file.path(output_dir, "high_priority_for_review.csv")
  high_priority <- analysis_results$papers_with_claims %>%
    filter(confidence_score >= 0.7) %>%
    arrange(desc(confidence_score), desc(`Times.Cited`)) %>%
    mutate(
      priority_rank = row_number(),
      review_priority = case_when(
        confidence_score >= 0.8 & `Times.Cited` >= 100 ~ "URGENT",
        confidence_score >= 0.8 ~ "HIGH", 
        TRUE ~ "MEDIUM"
      )
    ) %>%
    select(priority_rank, review_priority, Title, Authors, Year, 
           confidence_score, `Times.Cited`, claim_matches, DOI)
  
  write_csv(high_priority, high_priority_csv)
  
  # Create structured validation template
  create_validation_template(output_dir)
  
  cat("Results exported to:\n")
  cat("- Main results CSV:", claims_csv, "\n")
  cat("- Summary statistics:", summary_csv, "\n") 
  cat("- High priority papers:", high_priority_csv, "\n")
  cat("- Validation template: See 'validation_template.md'\n")
  
  return(claims_csv)
}

# Pilot analysis function for smaller subset
pilot_citation_analysis <- function(abstracts_file = "data/All_abstracts.csv", sample_size = 1000) {
  cat("=== CITATION ANALYSIS PILOT: UNIVERSALITY CLAIM DETECTION ===\n")
  cat("Starting pilot analysis at:", Sys.time(), "\n")
  cat("Sample size:", sample_size, "papers\n\n")
  
  # Check if abstracts file exists
  if (!file.exists(abstracts_file)) {
    stop("Abstracts file not found: ", abstracts_file)
  }
  
  # Read and sample data
  cat("Reading and sampling data...\n")
  all_abstracts <- read_csv(abstracts_file, show_col_types = FALSE)
  
  # Take random sample for pilot
  set.seed(42) # For reproducibility
  if (nrow(all_abstracts) > sample_size) {
    sampled_abstracts <- all_abstracts %>% 
      slice_sample(n = sample_size)
    cat("Randomly sampled", sample_size, "papers from", nrow(all_abstracts), "total papers\n")
  } else {
    sampled_abstracts <- all_abstracts
    cat("Using all", nrow(all_abstracts), "papers (less than requested sample size)\n")
  }
  
  # Temporarily save sampled data
  temp_file <- "data/pilot_sample.csv"
  write_csv(sampled_abstracts, temp_file)
  
  # Run analysis on sample
  results <- analyze_universality_claims(temp_file)
  
  # Export pilot results
  export_file <- export_for_validation(results, output_dir = "outputs/pilot")
  
  # Clean up temp file
  if (file.exists(temp_file)) file.remove(temp_file)
  
  cat("\n=== PILOT ANALYSIS COMPLETE ===\n")
  cat("Next steps:\n")
  cat("1. Review pilot results to refine detection patterns\n")
  cat("2. Manually validate flagged papers using structured template\n")
  cat("3. Adjust confidence thresholds and patterns based on pilot\n")
  cat("4. Run full analysis after pilot validation\n")
  cat("5. Pilot results exported to:", export_file, "\n")
  
  return(results)
}

# Main execution function
main_citation_analysis <- function(abstracts_file = "data/All_abstracts.csv") {
  cat("=== CITATION ANALYSIS PHASE 1: UNIVERSALITY CLAIM DETECTION ===\n")
  cat("Starting analysis at:", Sys.time(), "\n\n")
  
  # Check if abstracts file exists
  if (!file.exists(abstracts_file)) {
    stop("Abstracts file not found: ", abstracts_file)
  }
  
  # Run analysis
  results <- analyze_universality_claims(abstracts_file)
  
  # Export for validation
  export_file <- export_for_validation(results)
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Next steps:\n")
  cat("1. Review the exported CSV files for manual validation\n")
  cat("2. Focus on high-confidence papers first\n")
  cat("3. After validation, run Phase 2 analysis for citation patterns\n")
  cat("4. Export file:", export_file, "\n")
  
  return(results)
}

# Function to create structured validation template
create_validation_template <- function(output_dir) {
  template_file <- file.path(output_dir, "validation_template.md")
  
  template_content <- '# Manual Validation Template: Universality Claims

## Instructions for Reviewers

### Overview
This template provides a structured approach to manually validate automatically detected universality claims about endophytes in plants.

### Validation Categories

#### 1. Manual Validation Status
- **CONFIRMED**: Clear universality claim detected correctly
- **FALSE_POSITIVE**: Automated detection error, no real universality claim
- **UNCLEAR**: Ambiguous statement requiring discussion

#### 2. Claim Strength Assessment
- **STRONG**: Unequivocal statements (e.g., "all plants harbor endophytes")
- **MODERATE**: Strong but slightly qualified (e.g., "virtually all plants")
- **WEAK**: Broad generalizations (e.g., "endophytes are ubiquitous")

#### 3. Claim Type Classification
- **fungal_only**: Specifically about fungal endophytes
- **bacterial_only**: Specifically about bacterial endophytes  
- **general_endophyte**: About endophytes in general
- **specific_context**: Limited to specific plants/environments

#### 4. Evidence Assessment
- **STRONG**: Multiple studies, comprehensive data
- **MODERATE**: Some supporting evidence cited
- **WEAK**: Limited or outdated evidence
- **NONE**: No supporting evidence provided

#### 5. Citation Context
- **introduction**: Background/literature review context
- **background**: Setting up study rationale
- **results**: Presenting study findings
- **discussion**: Interpreting results
- **conclusion**: Summary statements

### Review Workflow

1. **Read title and abstract carefully**
2. **Examine detected claim matches for accuracy**
3. **Assess claim in context of full abstract**
4. **Look for supporting citations or evidence**
5. **Fill in validation fields**
6. **Add detailed notes for unclear cases**

### Common False Positives to Watch For

- Study-specific results ("all our samples contained endophytes")
- Qualified statements ("many plants", "most species")
- Geographic/taxonomic limitations ("all tropical plants")
- Methodological statements ("all techniques detected endophytes")

### Quality Control

- Review high-confidence detections first
- Flag uncertain cases for secondary review
- Note potential citation chains for follow-up
- Document unusual claim formulations

---

## Example Validation Entry

**Paper ID**: 123  
**Title**: "Endophytic fungi in medicinal plants..."  
**Detected Claim**: "all plants harbor endophytic fungi"  

**Validation**:
- Manual Validation Status: CONFIRMED
- Claim Strength: STRONG  
- Claim Type: fungal_only
- Evidence Provided: WEAK
- Citation Context: introduction
- Supporting Citations: [None provided in abstract]
- Reviewer Notes: "Clear universality statement but no supporting evidence visible in abstract"
- Validation Date: 2025-07-31
- Reviewer Initials: BB

---

*Template Version: 1.0*  
*Last Updated: July 31, 2025*
'
  
  writeLines(template_content, template_file)
  cat("Validation template created:", template_file, "\n")
}
