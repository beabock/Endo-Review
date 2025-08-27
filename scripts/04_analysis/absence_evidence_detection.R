# Absence Evidence Detection for Endophyte Research
# B. Bock
# July 31, 2025
#
# Specialized script to identify and analyze studies reporting absence of fungal endophytes
# Part of the systematic investigation of endophyte universality in plants


library(tidyverse)
library(stringr)

cat("=== ABSENCE EVIDENCE DETECTION ===\n")
cat("Identifying studies that searched for but did not find fungal endophytes\n\n")

# Load relevant abstracts with species detection results
if (!file.exists("results/relevant_abstracts_with_pa_predictions.csv")) {
  stop("Please run apply_models_to_full_dataset.R first to generate species detection results.")
}

relevant_data <- read_csv("results/relevant_abstracts_with_pa_predictions.csv", show_col_types = FALSE)

cat("Total abstracts for absence analysis:", nrow(relevant_data), "\n")

# Define absence detection function
detect_absence_evidence <- function(text) {
  # Keywords indicating absence or negative results
  absence_indicators <- c(
    # Direct absence statements
    "no endophyte", "no endophytes", "endophyte-free", "endophytes absent", 
    "absence of endophyte", "absence of endophytes", "lacking endophyte", 
    "lacking endophytes", "without endophyte", "without endophytes",
    "devoid of endophyte", "devoid of endophytes", "free of endophyte",
    "free of endophytes", "not contain endophyte", "not contain endophytes",
    
    # Negative culture results
    "no fungi isolated", "no fungal isolates", "no growth", "sterile", 
    "failed to isolate", "no colonization", "not colonized", "uncolonized",
    "no infection", "not infected", "culture negative", "negative culture",
    "no viable fungi", "no culturable fungi",
    
    # Negative molecular results  
    "no amplification", "no pcr product", "no fungal dna", "no fungal sequences",
    "below detection limit", "not detected", "negative pcr", "pcr negative",
    "no fungal taxa", "no fungal otus", "no reads", "empty samples",
    
    # Negative microscopy results
    "no fungal structures", "no hyphae observed", "no fungal hyphae",
    "no internal fungi", "no intercellular fungi", "no intracellular fungi",
    "tissues free", "clean tissues", "no fungal presence",
    
    # Quantitative absence
    "zero percent", "0%", "zero colonization", "colonization rate of 0",
    "frequency of 0", "prevalence of 0", "incidence of 0"
  )
  
  # Method-specific search terms
  search_methods <- c(
    # Culture methods
    "cultured", "isolated", "isolation", "culture", "plated", "incubated",
    "medium", "agar", "petri", "colony", "sterile", "aseptic",
    
    # Molecular methods
    "pcr", "amplified", "sequenced", "dna extracted", "primers",
    "18s", "28s", "rrna", "barcode", "metagenom*", "amplicon",
    
    # Microscopy methods
    "sectioned", "stained", "microscopy", "examined", "observed",
    "histological", "cleared", "mounted", "visualized"
  )
  
  text_lower <- tolower(text)
  
  # Check for absence indicators
  absence_matches <- sapply(absence_indicators, function(term) {
    grepl(paste0("\\b", term, "\\b"), text_lower, fixed = FALSE)
  })
  
  # Check for search methods (indicates active looking)
  method_matches <- sapply(search_methods, function(term) {
    grepl(paste0("\\b", term, "\\b"), text_lower, fixed = FALSE)
  })
  
  # Calculate scores
  absence_score <- sum(absence_matches)
  method_score <- sum(method_matches)
  
  # Identify specific absence terms found
  absence_terms_found <- names(absence_matches)[absence_matches]
  method_terms_found <- names(method_matches)[method_matches]
  
  return(list(
    potential_absence = absence_score > 0 & method_score > 0,  # Both absence terms AND methods
    absence_score = absence_score,
    method_score = method_score,
    absence_terms = if(length(absence_terms_found) > 0) paste(absence_terms_found, collapse = "; ") else NA,
    method_terms = if(length(method_terms_found) > 0) paste(method_terms_found, collapse = "; ") else NA,
    confidence_level = case_when(
      absence_score >= 3 & method_score >= 2 ~ "High",
      absence_score >= 2 & method_score >= 1 ~ "Medium", 
      absence_score >= 1 & method_score >= 1 ~ "Low",
      TRUE ~ "None"
    )
  ))
}


# Apply absence detection to all abstracts
cat("Applying absence detection algorithm...\n")

absence_results <- map_dfr(1:nrow(relevant_data), function(i) {
  if (i %% 500 == 0) cat("  Processed", i, "of", nrow(relevant_data), "abstracts\n")
  
  abstract_text <- relevant_data$abstract[i]
  
  # Use only abstract for analysis since title column is not available
  combined_text <- ifelse(is.na(abstract_text), "", abstract_text)
  
  if (nchar(combined_text) < 10) {
    # Skip very short texts
    return(data.frame(
      id = relevant_data$id[i],
      potential_absence = FALSE,
      absence_score = 0,
      method_score = 0,
      absence_terms = NA,
      method_terms = NA,
      confidence_level = "None"
    ))
  }
  
  # Apply detection function
  absence_analysis <- detect_absence_evidence(combined_text)

  return(data.frame(
    id = relevant_data$id[i],
    potential_absence = absence_analysis$potential_absence,
    absence_score = absence_analysis$absence_score,
    method_score = absence_analysis$method_score,
    absence_terms = absence_analysis$absence_terms,
    method_terms = absence_analysis$method_terms,
    confidence_level = absence_analysis$confidence_level
  ))
})

cat("Absence detection completed.\n")

# Combine with original data
absence_analysis_results <- relevant_data %>%
  left_join(absence_results, by = "id") %>%
  mutate(
    # Add additional classification variables - check for any species information
    has_species = if(all(c("canonicalName", "resolved_name", "acceptedScientificName") %in% names(.))) {
      !is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName)
    } else {
      FALSE
    },
    # Check if methods_summary exists, if not check for individual method columns
    has_methods = if("methods_summary" %in% names(.)) {
      !is.na(methods_summary)
    } else if(any(c("molecular_methods", "culture_based_methods", "microscopy_methods") %in% names(.))) {
      !is.na(molecular_methods) | !is.na(culture_based_methods) | !is.na(microscopy_methods)
    } else {
      FALSE
    },
    predicted_relevant = final_classification == "Presence",
    predicted_absence = final_classification == "Absence",
    weighted_ensemble_presence = weighted_ensemble == "Presence",
    conservative_presence = conservative_classification == "Presence"
  )

# Analysis of absence evidence
cat("\n=== ABSENCE EVIDENCE ANALYSIS ===\n")

# Calculate totals properly - check for duplicates first
cat("Checking for duplicate IDs in dataset...\n")
duplicate_check <- absence_analysis_results %>%
  count(id, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(duplicate_check) > 0) {
  cat("Warning:", nrow(duplicate_check), "IDs have multiple rows. Using distinct() to handle duplicates safely.\n")
  
  # Use distinct() instead of slice(1) to handle duplicates safely
  total_abstracts <- absence_analysis_results %>% 
    distinct(id, .keep_all = TRUE) %>%
    nrow()
    
  high_confidence_absence <- absence_analysis_results %>% 
    group_by(id) %>% 
    summarise(high_confidence = any(confidence_level == "High", na.rm = TRUE), .groups = "drop") %>% 
    pull(high_confidence) %>% 
    sum(na.rm = TRUE)
    
  medium_confidence_absence <- absence_analysis_results %>% 
    group_by(id) %>% 
    summarise(medium_confidence = any(confidence_level == "Medium", na.rm = TRUE), .groups = "drop") %>% 
    pull(medium_confidence) %>% 
    sum(na.rm = TRUE)
} else {
  cat("No duplicate IDs found. Proceeding with full dataset.\n")
  
  total_abstracts <- nrow(absence_analysis_results)
  high_confidence_absence <- sum(absence_analysis_results$confidence_level == "High", na.rm = TRUE)
  medium_confidence_absence <- sum(absence_analysis_results$confidence_level == "Medium", na.rm = TRUE)
}

cat("Total abstracts analyzed:", total_abstracts, "\n")
cat("High confidence absence:", high_confidence_absence, "(", round(100 * high_confidence_absence / total_abstracts, 2), "%)\n")
cat("Medium confidence absence:", medium_confidence_absence, "(", round(100 * medium_confidence_absence / total_abstracts, 2), "%)\n")

# Detailed analysis by prediction type
absence_by_prediction <- absence_analysis_results %>%
  # Handle potential duplicates safely
  distinct(id, final_classification, confidence_level, .keep_all = TRUE) %>%
  group_by(final_classification) %>%
  summarise(
    total = n(),
    high_confidence = sum(confidence_level == "High", na.rm = TRUE),
    medium_confidence = sum(confidence_level == "Medium", na.rm = TRUE),
    high_confidence_rate = round(100 * high_confidence / total, 2),
    medium_confidence_rate = round(100 * medium_confidence / total, 2),
    .groups = "drop"
  )

cat("\nAbsence evidence by ML prediction (final_classification):\n")
print(absence_by_prediction)

# Cross-tabulation of ML predictions vs detected absence evidence
absence_crosstab <- absence_analysis_results %>%
  # Handle potential duplicates safely by keeping unique combinations
  distinct(id, final_classification, confidence_level, .keep_all = TRUE) %>%
  mutate(
    detected_absence = confidence_level %in% c("High", "Medium"),
    prediction_category = case_when(
      final_classification == "Presence" & detected_absence ~ "ML:Presence, Evidence:Absence",
      final_classification == "Presence" & !detected_absence ~ "ML:Presence, Evidence:None", 
      final_classification == "Absence" & detected_absence ~ "ML:Absence, Evidence:Absence",
      final_classification == "Absence" & !detected_absence ~ "ML:Absence, Evidence:None",
      TRUE ~ "Other"
    )
  ) %>%
  count(prediction_category, name = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 2))

cat("\nAgreement between ML predictions and detected absence evidence:\n")
print(absence_crosstab)


# Analysis by research methods
if (any(c("molecular_methods", "culture_based_methods", "microscopy_methods") %in% names(absence_analysis_results))) {
  absence_by_methods <- absence_analysis_results %>%
    filter(confidence_level %in% c("High", "Medium")) %>%
    summarise(
      total_absence = n(),
      with_molecular = if("molecular_methods" %in% names(.)) sum(molecular_methods, na.rm = TRUE) else 0,
      with_culture = if("culture_based_methods" %in% names(.)) sum(culture_based_methods, na.rm = TRUE) else 0,
      with_microscopy = if("microscopy_methods" %in% names(.)) sum(microscopy_methods, na.rm = TRUE) else 0,
      molecular_pct = round(100 * with_molecular / total_absence, 1),
      culture_pct = round(100 * with_culture / total_absence, 1),
      microscopy_pct = round(100 * with_microscopy / total_absence, 1)
    )
  
  cat("\nMethod usage in absence studies:\n")
  print(absence_by_methods)
} else {
  cat("\nMethod usage analysis skipped - method columns not available in dataset\n")
}

# Geographic patterns in absence evidence
if ("countries_detected" %in% names(absence_analysis_results)) {
  absence_geographic <- absence_analysis_results %>%
    filter(confidence_level %in% c("High", "Medium"), !is.na(countries_detected)) %>%
    # First consolidate information per abstract
    group_by(id) %>%
    summarise(
      countries_detected = paste(unique(na.omit(countries_detected)), collapse = "; "),
      .groups = "drop"
    ) %>%
    separate_rows(countries_detected, sep = "; ") %>%
    count(countries_detected, name = "absence_studies") %>%
    arrange(desc(absence_studies)) %>%
    head(15)
} else {
  absence_geographic <- data.frame()
  cat("\nGeographic analysis skipped - countries_detected column not available\n")
}

if (nrow(absence_geographic) > 0) {
  cat("\nTop countries with absence evidence:\n")
  print(absence_geographic)
}

# Plant parts analysis for absence studies
if ("plant_parts_detected" %in% names(absence_analysis_results)) {
  absence_plant_parts <- absence_analysis_results %>%
    filter(confidence_level %in% c("High", "Medium"), !is.na(plant_parts_detected)) %>%
    # First consolidate information per abstract
    group_by(id) %>%
    summarise(
      plant_parts_detected = paste(unique(na.omit(plant_parts_detected)), collapse = "; "),
      .groups = "drop"
    ) %>%
    separate_rows(plant_parts_detected, sep = "; ") %>%
    count(plant_parts_detected, name = "absence_studies") %>%
    arrange(desc(absence_studies)) %>%
    head(15)
} else {
  absence_plant_parts <- data.frame()
  cat("\nPlant parts analysis skipped - plant_parts_detected column not available\n")
}

if (nrow(absence_plant_parts) > 0) {
  cat("\nPlant parts most commonly reported as endophyte-free:\n")
  print(absence_plant_parts)
}

# Temporal analysis
absence_temporal <- absence_analysis_results %>%
  # Handle potential duplicates safely
  distinct(id, publication_year, confidence_level, .keep_all = TRUE) %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  filter(!is.na(publication_year)) %>%
  mutate(
    decade = floor(publication_year / 10) * 10,
    five_year_period = floor(publication_year / 5) * 5
  ) %>%
  group_by(five_year_period) %>%
  summarise(
    total_studies = n(),
    high_confidence_absence = sum(confidence_level == "High", na.rm = TRUE),
    medium_confidence_absence = sum(confidence_level == "Medium", na.rm = TRUE),
    high_confidence_rate = round(100 * high_confidence_absence / total_studies, 2),
    .groups = "drop"
  ) %>%
  arrange(five_year_period)

cat("\nTemporal trends in absence reporting:\n")
print(absence_temporal)

# Save results
write_csv(absence_analysis_results, "results/absence_evidence_analysis.csv")

# Create comprehensive CSV of ALL papers with absence statement matches
cat("Creating comprehensive CSV of all papers with absence statement matches...\n")

all_absence_matches <- absence_analysis_results %>%
  filter(potential_absence == TRUE | confidence_level %in% c("High", "Medium", "Low")) %>%
  arrange(desc(absence_score), desc(method_score), confidence_level) %>%
  select(
    id, article_title, abstract, authors, source_title, publication_year, doi,
    # ML predictions
    final_classification, weighted_ensemble, conservative_classification,
    Relevant, Irrelevant, confidence,
    # Absence detection results
    potential_absence, absence_score, method_score, confidence_level,
    absence_terms, method_terms,
    # Species information (if available)
    any_of(c("canonicalName", "resolved_name", "acceptedScientificName")),
    # Methods information (if available)
    any_of(c("methods_summary", "molecular_methods", "culture_based_methods", "microscopy_methods")),
    # Geographic information (if available)
    any_of(c("countries_detected", "geographic_summary")),
    # Plant parts (if available)
    any_of(c("plant_parts_detected")),
    # Note: Universality statement detection removed from this script
  )

write_csv(all_absence_matches, "results/all_papers_with_absence_matches.csv")
cat("✓ Created results/all_papers_with_absence_matches.csv (", nrow(all_absence_matches), " papers with absence matches)\n")

# Create high-confidence absence subset for manual review
high_confidence_subset <- absence_analysis_results %>%
  filter(confidence_level %in% c("High", "Medium")) %>%
  arrange(desc(absence_score), desc(method_score)) %>%
  select(
    id, abstract, publication_year, final_classification, weighted_ensemble,
    canonicalName, resolved_name, acceptedScientificName, methods_summary, plant_parts_detected,
    countries_detected, absence_score, method_score, confidence_level,
    absence_terms, method_terms
  )

write_csv(high_confidence_subset, "results/high_confidence_absence_evidence.csv")

# Generate summary report
capture.output({
  cat("=== ABSENCE EVIDENCE DETECTION REPORT ===\n")
  cat("Generated:", Sys.time(), "\n")
  cat("Purpose: Identify studies reporting absence of fungal endophytes\n")
  cat("Method: Automated text analysis with manual validation recommended\n\n")
  
  cat("SUMMARY STATISTICS:\n")
  cat("Total abstracts analyzed:", total_abstracts, "\n")
  cat("High confidence absence:", high_confidence_absence, "\n")
  cat("Medium confidence absence:", medium_confidence_absence, "\n\n")
  
  cat("ABSENCE BY PREDICTION TYPE:\n")
  print(absence_by_prediction)
  cat("\n")
  
  cat("METHODOLOGICAL CONTEXT:\n")
  print(absence_by_methods)
  cat("\n")
  
  cat("TEMPORAL TRENDS:\n")
  print(absence_temporal)
  cat("\n")
  
  if (nrow(absence_geographic) > 0) {
    cat("GEOGRAPHIC PATTERNS:\n")
    print(absence_geographic)
    cat("\n")
  }
  
  if (nrow(absence_plant_parts) > 0) {
    cat("PLANT PARTS PATTERNS:\n")
    print(absence_plant_parts)
    cat("\n")
  }
  
  cat("QUALITY CONTROL RECOMMENDATIONS:\n")
  cat("1. Manual review of high-confidence absence cases recommended\n")
  cat("2. Verify methodological adequacy for studies claiming absence\n")
  cat("3. Check for potential false positives in absence detection\n")
  cat("4. Consider context of absence claims (e.g., specific conditions)\n\n")
  
  cat("NEXT STEPS:\n")
  cat("1. Expert review of high_confidence_absence_evidence.csv\n")
  cat("2. Quality assessment of methodological rigor\n")
  cat("3. Integration with species-level absence patterns\n")
  cat("4. Use absence_evidence_analysis.csv for further analysis\n")
  
}, file = "results/absence_evidence_report.txt")

cat("\nFiles created:\n")
cat("✓ results/absence_evidence_analysis.csv (complete results)\n")
cat("✓ results/all_papers_with_absence_matches.csv (ALL papers with absence statements)\n")
cat("✓ results/high_confidence_absence_evidence.csv (priority cases for review)\n")
cat("✓ results/absence_evidence_report.txt (detailed analysis report)\n")


cat("\n=== ABSENCE DETECTION COMPLETE ===\n")
cat("Key findings:\n")
cat("📊 ", high_confidence_absence, " high-confidence absence cases found\n")
cat("📋 ", medium_confidence_absence, " medium-confidence absence cases found\n")
cat("⏰ Temporal trends in absence reporting analyzed\n")
cat("🌍 Geographic patterns in absence evidence mapped\n")
cat("🌱 Plant parts commonly reported as endophyte-free documented\n")
cat("\nRecommendation: Manual review of high-confidence cases to validate absence claims!\n")
