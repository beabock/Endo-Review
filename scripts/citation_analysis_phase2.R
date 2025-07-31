# Citation Analysis Phase 2: Citation Network Analysis
# Author: Systematic Review Analysis Team
# Date: July 31, 2025
# Purpose: Analyze citation networks and track universality claim propagation

# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(igraph)
library(ggplot2)
library(networkD3)
library(visNetwork)

# Source Phase 1 functions
source("citation_analysis_phase1.R")

# Function to parse reference lists from database exports
parse_reference_data <- function(abstracts_file) {
  cat("Parsing reference data from abstracts file...\n")
  
  # Read the abstracts data (assuming it now includes reference lists)
  abstracts <- read_csv(abstracts_file, show_col_types = FALSE)
  
  # Check if reference data is available
  ref_columns <- c("References", "Cited.References", "reference_list", "citations")
  available_ref_cols <- intersect(names(abstracts), ref_columns)
  
  if (length(available_ref_cols) == 0) {
    stop("No reference data found. Expected columns: ", paste(ref_columns, collapse = ", "))
  }
  
  cat("Found reference data in column(s):", paste(available_ref_cols, collapse = ", "), "\n")
  
  # Use the first available reference column
  ref_col <- available_ref_cols[1]
  
  # Parse references into a long format
  references_long <- abstracts %>%
    select(paper_id = row_number(), DOI, Title, Authors, Year, 
           reference_text = !!sym(ref_col)) %>%
    filter(!is.na(reference_text) & reference_text != "") %>%
    mutate(
      # Split references (assuming they're separated by semicolons or newlines)
      references = str_split(reference_text, "[;\n]|(?<=\\d{4})\\.\\s+")
    ) %>%
    unnest(references) %>%
    filter(str_trim(references) != "") %>%
    mutate(
      references = str_trim(references),
      ref_id = row_number()
    )
  
  cat("Parsed", nrow(references_long), "individual references from", 
      length(unique(references_long$paper_id)), "papers\n")
  
  return(references_long)
}

# Function to extract citation information from reference strings
extract_citation_info <- function(reference_text) {
  # Basic patterns to extract key information
  # This is a simplified parser - may need refinement based on actual reference formats
  
  citation_info <- tibble(
    full_reference = reference_text,
    # Extract year (4 digits, often in parentheses or after comma)
    year = str_extract(reference_text, "\\b(19|20)\\d{2}\\b"),
    # Extract DOI if present
    doi = str_extract(reference_text, "10\\.\\d+/[^\\s,;]+"),
    # Extract first author (before comma or et al)
    first_author = str_extract(reference_text, "^[^,;]+(?=,|\\s+et\\s+al)"),
    # Check if it's likely a journal article vs book/other
    is_journal = str_detect(reference_text, "\\b\\d+\\s*:\\s*\\d+|\\bpp?\\.")
  )
  
  return(citation_info)
}

# Function to identify papers in our dataset that are cited by other papers
find_internal_citations <- function(references_long, abstracts_data) {
  cat("Identifying internal citations within dataset...\n")
  
  # Extract citation info from references
  ref_with_info <- references_long %>%
    rowwise() %>%
    mutate(
      citation_info = list(extract_citation_info(references))
    ) %>%
    unnest(citation_info)
  
  # Try to match references to papers in our dataset
  # Method 1: DOI matching (most reliable)
  doi_matches <- ref_with_info %>%
    filter(!is.na(doi)) %>%
    inner_join(
      abstracts_data %>% 
        filter(!is.na(DOI)) %>%
        select(cited_paper_id = row_number(), cited_doi = DOI),
      by = c("doi" = "cited_doi")
    ) %>%
    select(citing_paper_id = paper_id, cited_paper_id, match_method = "DOI")
  
  # Method 2: Author-year matching (less reliable but broader)
  # Clean and standardize author names and years for matching
  author_year_matches <- ref_with_info %>%
    filter(!is.na(first_author) & !is.na(year)) %>%
    mutate(
      clean_author = str_to_upper(str_remove_all(first_author, "[^A-Za-z\\s]")),
      clean_author = str_trim(clean_author)
    ) %>%
    inner_join(
      abstracts_data %>%
        mutate(
          clean_author = str_to_upper(str_extract(Authors, "^[^,;]+")),
          clean_author = str_remove_all(clean_author, "[^A-Za-z\\s]"),
          clean_author = str_trim(clean_author),
          year_char = as.character(Year)
        ) %>%
        filter(!is.na(clean_author) & !is.na(year_char)) %>%
        select(cited_paper_id = row_number(), clean_author, year_char),
      by = c("clean_author", "year" = "year_char")
    ) %>%
    anti_join(doi_matches, by = c("citing_paper_id", "cited_paper_id")) %>%
    select(citing_paper_id, cited_paper_id, match_method = "author_year")
  
  # Combine all matches
  all_internal_citations <- bind_rows(doi_matches, author_year_matches)
  
  cat("Found", nrow(all_internal_citations), "internal citations:\n")
  cat("- DOI matches:", nrow(doi_matches), "\n")
  cat("- Author-year matches:", nrow(author_year_matches), "\n")
  
  return(all_internal_citations)
}

# Function to build citation network
build_citation_network <- function(internal_citations, abstracts_data, claim_results) {
  cat("Building citation network...\n")
  
  # Add claim information to papers
  papers_with_claims <- abstracts_data %>%
    mutate(paper_id = row_number()) %>%
    left_join(
      claim_results$papers_with_claims %>%
        select(paper_id, has_universality_claim, confidence_score, claim_matches),
      by = "paper_id"
    ) %>%
    mutate(
      has_universality_claim = replace_na(has_universality_claim, FALSE),
      confidence_score = replace_na(confidence_score, 0),
      node_type = case_when(
        has_universality_claim & confidence_score >= 0.8 ~ "Strong Claim",
        has_universality_claim & confidence_score >= 0.5 ~ "Moderate Claim", 
        has_universality_claim ~ "Weak Claim",
        TRUE ~ "No Claim"
      )
    )
  
  # Create network edges with citation relationships
  citation_edges <- internal_citations %>%
    left_join(
      papers_with_claims %>% select(paper_id, citing_title = Title, citing_year = Year, 
                                   citing_claim = has_universality_claim, citing_confidence = confidence_score),
      by = c("citing_paper_id" = "paper_id")
    ) %>%
    left_join(
      papers_with_claims %>% select(paper_id, cited_title = Title, cited_year = Year,
                                   cited_claim = has_universality_claim, cited_confidence = confidence_score),
      by = c("cited_paper_id" = "paper_id")
    ) %>%
    mutate(
      edge_type = case_when(
        citing_claim & cited_claim ~ "Claim to Claim",
        citing_claim & !cited_claim ~ "Claim to Non-claim",
        !citing_claim & cited_claim ~ "Non-claim to Claim",
        TRUE ~ "Non-claim to Non-claim"
      )
    )
  
  # Create igraph network object
  g <- graph_from_data_frame(
    d = citation_edges %>% select(citing_paper_id, cited_paper_id, edge_type),
    vertices = papers_with_claims %>% select(paper_id, Title, Year, node_type, 
                                           has_universality_claim, confidence_score),
    directed = TRUE
  )
  
  cat("Network created with", vcount(g), "nodes and", ecount(g), "edges\n")
  
  return(list(
    graph = g,
    edges = citation_edges,
    nodes = papers_with_claims
  ))
}

# Function to analyze citation patterns
analyze_citation_patterns <- function(network_data, claim_results) {
  cat("Analyzing citation patterns...\n")
  
  g <- network_data$graph
  edges <- network_data$edges
  nodes <- network_data$nodes
  
  # Basic network metrics
  network_metrics <- list(
    total_papers = vcount(g),
    total_citations = ecount(g),
    papers_with_claims = sum(nodes$has_universality_claim, na.rm = TRUE),
    density = edge_density(g),
    reciprocity = reciprocity(g),
    transitivity = transitivity(g)
  )
  
  # Citation patterns by claim status
  citation_patterns <- edges %>%
    count(edge_type) %>%
    mutate(percentage = round(n/sum(n)*100, 1))
  
  # Most cited papers with claims
  claim_citations <- edges %>%
    filter(cited_claim == TRUE) %>%
    count(cited_paper_id, cited_title, cited_confidence, sort = TRUE) %>%
    head(20) %>%
    rename(times_cited_internally = n)
  
  # Papers citing the most claim-making papers
  claim_citers <- edges %>%
    filter(cited_claim == TRUE) %>%
    count(citing_paper_id, citing_title, citing_claim, sort = TRUE) %>%
    head(20) %>%
    rename(claims_cited = n)
  
  # Temporal analysis
  temporal_analysis <- edges %>%
    filter(!is.na(citing_year) & !is.na(cited_year)) %>%
    mutate(
      citation_lag = citing_year - cited_year,
      era_citing = case_when(
        citing_year < 1990 ~ "Pre-molecular (<1990)",
        citing_year < 2000 ~ "Early molecular (1990s)",
        citing_year < 2010 ~ "High-throughput (2000s)", 
        TRUE ~ "Microbiome (2010+)"
      ),
      era_cited = case_when(
        cited_year < 1990 ~ "Pre-molecular (<1990)",
        cited_year < 2000 ~ "Early molecular (1990s)",
        cited_year < 2010 ~ "High-throughput (2000s)",
        TRUE ~ "Microbiome (2010+)"
      )
    )
  
  # Cross-era citation patterns
  cross_era_patterns <- temporal_analysis %>%
    count(era_cited, era_citing, edge_type) %>%
    pivot_wider(names_from = edge_type, values_from = n, values_fill = 0)
  
  return(list(
    network_metrics = network_metrics,
    citation_patterns = citation_patterns,
    most_cited_claims = claim_citations,
    top_claim_citers = claim_citers,
    temporal_analysis = temporal_analysis,
    cross_era_patterns = cross_era_patterns
  ))
}

# Function to detect citation accuracy issues
analyze_citation_accuracy <- function(network_data, references_long) {
  cat("Analyzing citation accuracy...\n")
  
  edges <- network_data$edges
  
  # Identify potential citation accuracy issues
  accuracy_issues <- edges %>%
    filter(citing_claim == TRUE) %>%  # Focus on papers making claims
    mutate(
      # Flag potential issues
      cites_non_claim = !cited_claim,  # Claim paper cites non-claim paper
      older_cites_newer = citing_year < cited_year,  # Temporal inconsistency
      self_citation = citing_paper_id == cited_paper_id  # Self-citations
    ) %>%
    filter(cites_non_claim | older_cites_newer | self_citation)
  
  # Summary of accuracy issues
  accuracy_summary <- accuracy_issues %>%
    summarise(
      total_problematic_citations = n(),
      cites_non_claim_count = sum(cites_non_claim, na.rm = TRUE),
      temporal_issues = sum(older_cites_newer, na.rm = TRUE),
      self_citations = sum(self_citation, na.rm = TRUE)
    )
  
  return(list(
    accuracy_issues = accuracy_issues,
    accuracy_summary = accuracy_summary
  ))
}

# Function to create network visualizations
create_network_visualizations <- function(network_data, output_dir = "outputs/phase2") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating network visualizations...\n")
  
  g <- network_data$graph
  
  # Prepare data for visNetwork
  nodes_vis <- network_data$nodes %>%
    filter(paper_id %in% c(network_data$edges$citing_paper_id, network_data$edges$cited_paper_id)) %>%
    mutate(
      id = paper_id,
      label = paste0(str_trunc(Title, 40), "\n(", Year, ")"),
      group = node_type,
      color = case_when(
        node_type == "Strong Claim" ~ "#d62728",    # Red
        node_type == "Moderate Claim" ~ "#ff7f0e",  # Orange  
        node_type == "Weak Claim" ~ "#ffbb78",      # Light orange
        TRUE ~ "#1f77b4"                           # Blue
      ),
      size = case_when(
        has_universality_claim ~ 20,
        TRUE ~ 10
      )
    )
  
  edges_vis <- network_data$edges %>%
    mutate(
      from = citing_paper_id,
      to = cited_paper_id,
      color = case_when(
        edge_type == "Claim to Claim" ~ "#d62728",
        edge_type == "Claim to Non-claim" ~ "#ff7f0e", 
        edge_type == "Non-claim to Claim" ~ "#2ca02c",
        TRUE ~ "#1f77b4"
      ),
      arrows = "to"
    )
  
  # Create interactive network
  vis_network <- visNetwork(nodes_vis, edges_vis) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 42) %>%
    visNodes(font = list(size = 12)) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>%
    visLegend()
  
  # Save interactive network
  visSave(vis_network, file = file.path(output_dir, "citation_network_interactive.html"))
  
  cat("Network visualization saved to:", file.path(output_dir, "citation_network_interactive.html"), "\n")
  
  return(vis_network)
}

# Function to export Phase 2 results
export_phase2_results <- function(citation_analysis, network_data, output_dir = "outputs/phase2") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Exporting Phase 2 results...\n")
  
  # Export citation network data
  write_csv(network_data$edges, file.path(output_dir, "citation_edges.csv"))
  write_csv(network_data$nodes, file.path(output_dir, "citation_nodes.csv"))
  
  # Export citation patterns
  write_csv(citation_analysis$citation_patterns, file.path(output_dir, "citation_patterns.csv"))
  write_csv(citation_analysis$most_cited_claims, file.path(output_dir, "most_cited_claims.csv"))
  write_csv(citation_analysis$top_claim_citers, file.path(output_dir, "top_claim_citers.csv"))
  write_csv(citation_analysis$cross_era_patterns, file.path(output_dir, "cross_era_patterns.csv"))
  
  # Export network metrics summary
  metrics_df <- data.frame(
    metric = names(citation_analysis$network_metrics),
    value = unlist(citation_analysis$network_metrics)
  )
  write_csv(metrics_df, file.path(output_dir, "network_metrics.csv"))
  
  cat("Phase 2 results exported to:", output_dir, "\n")
}

# Main Phase 2 analysis function
phase2_citation_analysis <- function(abstracts_file = "data/All_abstracts.csv", 
                                   phase1_results = NULL) {
  cat("=== CITATION ANALYSIS PHASE 2: CITATION NETWORK ANALYSIS ===\n")
  cat("Starting Phase 2 analysis at:", Sys.time(), "\n\n")
  
  # If Phase 1 results not provided, run Phase 1 first
  if (is.null(phase1_results)) {
    cat("Phase 1 results not provided. Running Phase 1 first...\n")
    phase1_results <- analyze_universality_claims(abstracts_file)
  }
  
  # Read abstracts data
  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)
  
  # Parse reference data
  references_long <- parse_reference_data(abstracts_file)
  
  # Find internal citations
  internal_citations <- find_internal_citations(references_long, abstracts_data)
  
  # Build citation network
  network_data <- build_citation_network(internal_citations, abstracts_data, phase1_results)
  
  # Analyze citation patterns
  citation_analysis <- analyze_citation_patterns(network_data, phase1_results)
  
  # Analyze citation accuracy
  accuracy_analysis <- analyze_citation_accuracy(network_data, references_long)
  
  # Create visualizations
  network_viz <- create_network_visualizations(network_data)
  
  # Export results
  export_phase2_results(citation_analysis, network_data)
  
  # Print summary
  cat("\n=== PHASE 2 ANALYSIS COMPLETE ===\n")
  cat("Network Summary:\n")
  cat("- Total papers:", citation_analysis$network_metrics$total_papers, "\n")
  cat("- Total internal citations:", citation_analysis$network_metrics$total_citations, "\n")
  cat("- Papers with claims:", citation_analysis$network_metrics$papers_with_claims, "\n")
  cat("- Network density:", round(citation_analysis$network_metrics$density, 4), "\n")
  
  cat("\nCitation Patterns:\n")
  print(citation_analysis$citation_patterns)
  
  cat("\nAccuracy Issues:\n")
  print(accuracy_analysis$accuracy_summary)
  
  return(list(
    network_data = network_data,
    citation_analysis = citation_analysis,
    accuracy_analysis = accuracy_analysis,
    references_data = references_long,
    internal_citations = internal_citations
  ))
}

# Run Phase 2 if script is executed directly
if (!interactive()) {
  # Run Phase 2 analysis
  phase2_results <- phase2_citation_analysis()
}
