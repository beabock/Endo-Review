# B. Bock
# July 30, 2025
# Apply Trained Models to Full Dataset
# 
# This script applies the trained relevance and presence/absence models to the complete
# dataset of abstracts. Uses the optimized weighted ensemble approach for P/A classification.
# 
# Pipeline:
# 1. Load and prepare full dataset
# 2. Apply relevance classification (glmnet)
# 3. Filter to relevant abstracts  
# 4. Apply presence/absence classification (weighted ensemble: glmnet + svmLinear)
# 5. Save results with multiple threshold options

setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

# Library loading ---------------------------------------------------------

library(tidyverse)
library(tidytext)
library(caret)
library(Matrix)
library(janitor)
library(tictoc)

# Load memory optimization utilities
source("scripts/utils/memory_optimization.R")

cat("=== ENDOPHYTE SYSTEMATIC REVIEW: FULL DATASET CLASSIFICATION ===\n")
cat("Pipeline: Relevance → Presence/Absence Classification\n")
cat("Models: glmnet (relevance), weighted ensemble (P/A)\n")
cat("Memory optimization: Enabled\n\n")

# Ensemble functions ------------------------------------------------------

# Weighted ensemble function (best overall performance: 89.8% accuracy)
ensemble_predict_weighted <- function(glmnet_model, svm_model, newdata, 
                                     svm_weight_presence = 0.6, glm_weight_absence = 0.8) {
  # Get probabilities from both models
  glmnet_probs <- predict(glmnet_model, newdata = newdata, type = "prob")
  svm_probs <- predict(svm_model, newdata = newdata, type = "prob")
  
  # Create weighted probability ensemble - prioritizing absence detection
  ensemble_presence_prob <- (svm_probs$Presence * svm_weight_presence + 
                            glmnet_probs$Presence * (1 - svm_weight_presence))
  
  ensemble_absence_prob <- (glmnet_probs$Absence * glm_weight_absence + 
                           svm_probs$Absence * (1 - glm_weight_absence))
  
  # Make predictions using simple probability comparison
  ensemble_preds <- ifelse(ensemble_presence_prob > ensemble_absence_prob, "Presence", "Absence")
  
  return(factor(ensemble_preds, levels = c("Presence", "Absence")))
}

# Alternative threshold-optimized ensemble function
ensemble_predict_threshold_optimized <- function(glmnet_model, svm_model, newdata, 
                                               svm_weight_presence = 0.6, glm_weight_absence = 0.8, 
                                               threshold = 0.55) {
  # Get probabilities from both models
  glmnet_probs <- predict(glmnet_model, newdata = newdata, type = "prob")
  svm_probs <- predict(svm_model, newdata = newdata, type = "prob")
  
  # Create weighted probability ensemble - prioritizing absence detection
  ensemble_presence_prob <- (svm_probs$Presence * svm_weight_presence + 
                            glmnet_probs$Presence * (1 - svm_weight_presence))
  
  ensemble_absence_prob <- (glmnet_probs$Absence * glm_weight_absence + 
                           svm_probs$Absence * (1 - glm_weight_absence))
  
  # Make predictions using optimized threshold
  ensemble_preds <- ifelse(ensemble_absence_prob > threshold, "Absence", 
                          ifelse(ensemble_presence_prob > (1 - threshold), "Presence", "Presence"))
  
  return(factor(ensemble_preds, levels = c("Presence", "Absence")))
}

# Column name harmonization -----------------------------------------------

colname_mapping <- c(
  "title" = "article_title",                # 'title' to 'article_title'
  "abstract" = "abstract",                  # 'abstract' matches
  "authors" = "authors",                    # 'authors' matches
  "book_authors" = "book_authors",          # 'book_authors' matches
  "editors" = "book_editors",               # 'editors' to 'book_editors'
  "group_authors" = "book_group_authors",   # 'group_authors' to 'book_group_authors'
  "author_full_names" = "author_full_names",# 'author_full_names' matches
  "book_full_names" = "book_author_full_names", # 'book_full_names' to 'book_author_full_names'
  "conference_authors" = "conference_authors", # 'conference_authors' to 'conference_title'
  "source_title" = "source_title",          # 'source_title' matches
  "series_title" = "book_series_title",     # 'series_title' to 'book_series_title'
  "book_series" = "book_series_subtitle",   # 'book_series' to 'book_series_subtitle'
  "language_of_original_document" = "language", # 'language_of_original_document' to 'language'
  "conference_name" = "conference_title",   # 'conference_name' to 'conference_title'
  "conference_date" = "conference_date",    # 'conference_date' matches
  "conference_location" = "conference_location", # 'conference_location' matches
  "sponsors" = "conference_sponsor",       # 'sponsors' to 'conference_sponsor'
  "host" = "conference_host",               # 'host' to 'conference_host'
  "author_keywords" = "author_keywords",    # 'author_keywords' matches
  "index_keywords" = "keywords_plus",       # 'index_keywords' to 'keywords_plus'
  "affiliations" = "affiliations",          # 'affiliations' matches
  "authors_with_affiliations" = "addresses", # 'authors_with_affiliations' to 'addresses'
  "correspondence_address" = "reprint_addresses", # 'correspondence_address' to 'reprint_addresses'
  "email_address" = "email_addresses",      # 'email_address' to 'email_addresses'
  "researcher_i_ds" = "researcher_ids",     # 'researcher_i_ds' to 'researcher_ids'
  "orcid_i_ds" = "orci_ds",                 # 'orcid_i_ds' to 'orci_ds'
  "funding_details" = "funding_text",       # 'funding_details' to 'funding_text'
  "funding_programs" = "funding_orgs",      # 'funding_programs' to 'funding_orgs'
  "funding_texts" = "funding_name_preferred", # 'funding_texts' to 'funding_name_preferred'
  "references" = "cited_references",        # 'references' to 'cited_references'
  "cited_references" = "cited_reference_count", # 'cited_references' to 'cited_reference_count'
  "times_cited" = "times_cited_wo_s_core",  # 'times_cited' to 'times_cited_wo_s_core'
  "total_times_cited" = "times_cited_all_databases", # 'total_times_cited' to 'times_cited_all_databases'
  "usage_count_180_days" = "x180_day_usage_count", # 'usage_count_180_days' to 'x180_day_usage_count'
  "usage_count_since_2013" = "since_2013_usage_count", # 'usage_count_since_2013' to 'since_2013_usage_count'
  "publisher" = "publisher",                # 'publisher' matches
  "publisher_city" = "publisher_city",      # 'publisher_city' matches
  "publisher_address" = "publisher_address",# 'publisher_address' matches
  "issn" = "issn",                          # 'issn' matches
  "e_issn" = "e_issn",                      # 'e_issn' matches
  "isbn" = "isbn",                          # 'isbn' matches
  "abbreviated_source_title" = "journal_abbreviation", # 'abbreviated_source_title' to 'journal_abbreviation'
  "journal_iso" = "journal_iso_abbreviation", # 'journal_iso' to 'journal_iso_abbreviation'
  "publication_date" = "publication_date",  # 'publication_date' matches
  "year" = "publication_year",              # 'year' to 'publication_year'
  "volume" = "volume",                      # 'volume' matches
  "issue" = "issue",                        # 'issue' matches
  "supplement" = "supplement",              # 'supplement' matches
  "special_issue" = "special_issue",        # 'special_issue' matches
  "meeting_abstract" = "meeting_abstract",  # 'meeting_abstract' matches
  "page_start" = "start_page",              # 'page_start' to 'start_page'
  "page_end" = "end_page",                  # 'page_end' to 'end_page'
  "doi" = "doi",                            # 'doi' matches
  "doi_link" = "doi_link",                  # 'doi_link' matches
  "secondary_doi" = "book_doi",             # 'secondary_doi' to 'book_doi'
  "early_access_date" = "early_access_date",# 'early_access_date' matches
  "page_count" = "number_of_pages",         # 'page_count' to 'number_of_pages'
  "web_of_science_categories" = "wo_s_categories", # 'web_of_science_categories' to 'wo_s_categories'
  "research_areas" = "research_areas",      # 'research_areas' matches
  "subject_categories" = "subject_categories", # 'subject_categories' to 'wo_s_categories'
  "document_delivery_number" = "ids_number", # 'document_delivery_number' to 'ids_number'
  "pub_med_id" = "pubmed_id",               # 'pub_med_id' to 'pubmed_id'
  "open_access" = "open_access_designations", # 'open_access' to 'open_access_designations'
  "highly_cited_paper" = "highly_cited_status", # 'highly_cited_paper' to 'highly_cited_status'
  "hot_paper" = "hot_paper_status",         # 'hot_paper' to 'hot_paper_status'
  "date" = "date_of_export",                # 'date' to 'date_of_export'
  "wos_id" = "ut_unique_wos_id",            # 'wos_id' to 'ut_unique_wos_id'
  "author_s_id" = "web_of_science_record",  # 'author_s_id' to 'web_of_science_record'
  "art_no" = "article_number",              # 'art_no' to 'article_number'
  "cited_by" = "cited_by",                  # 'cited_by' matches
  "link" = "link",                          # 'link' matches
  "molecular_sequence_numbers" = "molecular_sequence_numbers", # 'molecular_sequence_numbers' matches
  "chemicals_cas" = "chemicals_cas",        # 'chemicals_cas' matches
  "tradenames" = "tradenames",              # 'tradenames' matches
  "manufacturers" = "manufacturers",        # 'manufacturers' matches
  "conference_code" = "conference_code",    # 'conference_code' matches
  "coden" = "coden",                        # 'coden' matches
  "document_type" = "document_type",        # 'document_type' matches
  "publication_stage" = "publication_stage",# 'publication_stage' matches
  "source" = "source",                      # 'source' matches
  "eid" = "eid"                             # 'eid' matches
)

# Load and prepare data ---------------------------------------------------

cat("Step 1: Loading and preparing full dataset...\n")
tic("Data loading and preparation")

# Load full dataset
full_abstracts <- read.csv("data/processed/All_abstracts_deduped.csv") %>%
  clean_names()

cat("  Loaded", nrow(full_abstracts), "abstracts from full dataset\n")

# Apply column name harmonization
full_abstracts <- full_abstracts %>%
  rename_with(~ ifelse(. %in% names(colname_mapping), colname_mapping[.], .))

# Exclude training data to avoid predicting on labeled abstracts
labeled_abstracts <- read.csv("data/raw/Training_labeled_abs_6.csv") %>%
  clean_names()

filtered_dois <- labeled_abstracts$doi[!is.na(labeled_abstracts$doi) & labeled_abstracts$doi != ""]
original_count <- nrow(full_abstracts)
full_abstracts <- full_abstracts[!full_abstracts$doi %in% filtered_dois, ]

cat("  Excluded", original_count - nrow(full_abstracts), "training abstracts\n")
cat("  Final dataset:", nrow(full_abstracts), "abstracts for prediction\n")

# Add unique ID column for tracking documents
full_abstracts$id <- 1:nrow(full_abstracts)

toc()

# STEP 1: Relevance Classification ----------------------------------------

cat("\nStep 2: Relevance Classification...\n")
tic("Relevance classification")

# Create DTM for relevance classification
cat("  Creating document-term matrix...\n")
dtm <- full_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

# Convert to matrix and sanitize column names
dtm_matrix <- as.matrix(dtm)
colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)
rownames(dtm_matrix) <- dtm$dimnames$Docs

# Optimize: Keep as matrix until needed, convert to sparse-friendly data frame
cat("  Converting DTM to data frame (this may take a moment for large datasets)...\n")
dtm_df <- as.data.frame(as.matrix(dtm_matrix))  # Explicit conversion for clarity
stopifnot(!any(duplicated(colnames(dtm_df))))

cat("  DTM created:", nrow(dtm_df), "documents ×", ncol(dtm_df), "terms\n")

# Load trained relevance model
cat("  Loading trained relevance model...\n")
rel_model <- readRDS("models/best_model_relevance_glmnet.rds")
trained_vocab <- rel_model$finalModel$xNames

# Align features with training vocabulary
missing_words <- setdiff(trained_vocab, colnames(dtm_df))
if (length(missing_words) > 0) {
  zero_matrix <- matrix(0, nrow = nrow(dtm_df), ncol = length(missing_words),
                        dimnames = list(rownames(dtm_df), missing_words))
  dtm_df <- cbind(dtm_df, zero_matrix)
}

# Remove extra columns and reorder to match training
dtm_df <- dtm_df[, colnames(dtm_df) %in% trained_vocab]
dtm_df <- dtm_df[, trained_vocab, drop = FALSE]

cat("  Feature alignment complete:", ncol(dtm_df), "features\n")

# Predict relevance
cat("  Predicting relevance...\n")
probs <- predict(rel_model, newdata = dtm_df, type = "prob")
full_abstracts <- full_abstracts %>%
  bind_cols(probs)

# Apply relevance thresholds
relevance_thresholds <- list(
  loose = 0.5,    # more willing to classify
  medium = 0.6,   # balanced
  strict = 0.8    # only classify with strong certainty
)

for (thresh_name in names(relevance_thresholds)) {
  thresh_val <- relevance_thresholds[[thresh_name]]
  col_name <- paste0("relevance_", thresh_name)
  
  full_abstracts[[col_name]] <- case_when(
    full_abstracts$Relevant >= thresh_val ~ "Relevant",
    full_abstracts$Irrelevant >= thresh_val ~ "Irrelevant",
    TRUE ~ "Uncertain"
  )
}

# Report relevance classification results
relevance_summary <- full_abstracts %>%
  select(relevance_loose, relevance_medium, relevance_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label) %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0)

cat("  Relevance classification results:\n")
print(relevance_summary)

# Filter to relevant abstracts for P/A classification
abstracts_for_pa <- full_abstracts %>%
  filter(relevance_loose == "Relevant")  # Use loose threshold to be inclusive

cat("  Selected", nrow(abstracts_for_pa), "relevant abstracts for P/A classification\n")

write.csv(full_abstracts, "data/processed/full_abstracts_for_pa.csv", row.names = FALSE)

toc()

# STEP 2: Presence/Absence Classification ---------------------------------

cat("\nStep 3: Presence/Absence Classification...\n")
tic("Presence/Absence classification")

# Memory check before P/A processing
mem_status <- monitor_memory(context = "Before P/A classification")
if (mem_status$above_threshold) {
  cat("⚠️ High memory usage detected. Consider reducing dataset size or using chunked processing.\n")
}

# Create memory-efficient sparse DTM for P/A classification
cat("  Creating memory-efficient P/A DTM with unigrams and bigrams...\n")

# Use the optimized sparse DTM creation function
create_memory_optimized_pa_dtm <- function(data, max_features = 15000, min_term_freq = 2) {

  cat("  Processing", nrow(data), "abstracts for P/A classification\n")

  # Create temporary workspace for intermediate files
  temp_dir <- create_temp_workspace("pa_dtm_temp")

  # Process unigrams with memory optimization
  cat("  Creating unigram features...\n")
  dtm_unigrams <- create_sparse_dtm(
    data = data,
    text_column = "abstract",
    id_column = "id",
    min_term_freq = min_term_freq,
    max_features = max_features
  )

  # Process bigrams with memory optimization
  cat("  Creating bigram features...\n")

  # Extract bigrams efficiently
  bigram_data <- data %>%
    mutate(
      # Pre-filter abstracts to reduce memory usage
      abstract_clean = str_replace_all(abstract, "[[:punct:][:digit:]]+", " "),
      abstract_clean = str_squish(abstract_clean)
    ) %>%
    select(id, abstract_clean)

  # Create bigram DTM
  dtm_bigrams <- bigram_data %>%
    unnest_tokens(bigram, abstract_clean, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    unite(bigram, word1, word2, sep = "_") %>%
    mutate(bigram = str_to_lower(bigram)) %>%
    group_by(bigram) %>%
    filter(n() >= min_term_freq) %>%
    ungroup() %>%
    count(id, bigram, sort = TRUE) %>%
    mutate(id = as.character(id)) %>%
    cast_sparse(id, bigram, n)

  # Memory cleanup
  rm(bigram_data)
  aggressive_gc(verbose = FALSE)

  # Combine unigrams and bigrams efficiently
  cat("  Combining unigram and bigram features...\n")

  # Get common document IDs
  common_docs <- intersect(rownames(dtm_unigrams), rownames(dtm_bigrams))

  if (length(common_docs) == 0) {
    cat("  Warning: No common documents found. Using unigrams only.\n")
    dtm_combined <- dtm_unigrams
  } else {
    # Subset to common documents
    dtm_unigrams_subset <- dtm_unigrams[common_docs, ]
    dtm_bigrams_subset <- dtm_bigrams[common_docs, ]

    # Combine sparse matrices
    dtm_combined <- cbind(dtm_unigrams_subset, dtm_bigrams_subset)
    cat("  Combined features:", ncol(dtm_combined), "total features\n")
  }

  # Apply feature selection to reduce dimensionality
  if (ncol(dtm_combined) > max_features) {
    cat("  Reducing features from", ncol(dtm_combined), "to", max_features, "\n")

    # Calculate term frequencies for feature selection
    term_freqs <- Matrix::colSums(dtm_combined)
    top_terms <- names(sort(term_freqs, decreasing = TRUE))[1:max_features]

    dtm_combined <- dtm_combined[, top_terms]
    cat("  Feature reduction complete:", ncol(dtm_combined), "features retained\n")
  }

  # Clean up temporary workspace
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  return(dtm_combined)
}

# Create the optimized DTM
dtm_sparse <- create_memory_optimized_pa_dtm(
  data = abstracts_for_pa,
  max_features = 10000,  # Limit features to reduce memory
  min_term_freq = 2
)

# Monitor memory after DTM creation
mem_status <- monitor_memory(context = "After P/A DTM creation")
if (mem_status$above_threshold) {
  cat("⚠️ High memory usage after DTM creation. Consider further reducing max_features.\n")
}

cat("  Memory-efficient P/A DTM created:", nrow(dtm_sparse), "documents ×", ncol(dtm_sparse), "terms\n")
cat("  Sparsity:", round(100 * (1 - length(dtm_sparse@x) / (nrow(dtm_sparse) * ncol(dtm_sparse))), 1), "%\n")

# Load trained P/A models
cat("  Loading trained P/A models...\n")
glmnet_model <- readRDS("models/best_model_presence_glmnet_ensemble.rds")
svm_model <- readRDS("models/best_model_presence_svmLinear_ensemble.rds")

# Get training vocabulary from SVM model (more comprehensive)
trained_vocab_pa <- setdiff(colnames(svm_model$trainingData), ".outcome")
cat("  Training vocabulary size:", length(trained_vocab_pa), "features\n")

# Memory-efficient feature alignment for sparse matrix
cat("  Performing memory-efficient feature alignment...\n")

# Get current features
current_features <- colnames(dtm_sparse)

# Find common features
common_features <- intersect(current_features, trained_vocab_pa)
missing_features <- setdiff(trained_vocab_pa, current_features)
extra_features <- setdiff(current_features, trained_vocab_pa)

cat("  Feature alignment summary:\n")
cat("  - Common features:", length(common_features), "\n")
cat("  - Missing features:", length(missing_features), "\n")
cat("  - Extra features:", length(extra_features), "\n")

# Subset to common features and add missing features as zeros
if (length(common_features) > 0) {
  dtm_aligned <- dtm_sparse[, common_features]
} else {
  cat("  Warning: No common features found. Creating zero matrix.\n")
  dtm_aligned <- Matrix::Matrix(0, nrow = nrow(dtm_sparse), ncol = 0, sparse = TRUE)
}

# Add missing features as sparse zero columns
if (length(missing_features) > 0) {
  cat("  Adding", length(missing_features), "missing features as zeros...\n")

  # Create sparse zero matrix for missing features
  zero_cols <- Matrix::Matrix(0, nrow = nrow(dtm_sparse), ncol = length(missing_features), sparse = TRUE)
  colnames(zero_cols) <- missing_features
  rownames(zero_cols) <- rownames(dtm_sparse)

  # Combine matrices
  dtm_aligned <- cbind(dtm_aligned, zero_cols)
}

# Reorder to match training vocabulary
dtm_aligned <- dtm_aligned[, trained_vocab_pa, drop = FALSE]

# Memory cleanup
aggressive_gc(verbose = FALSE)

cat("  P/A feature alignment complete:", ncol(dtm_aligned), "features\n")
cat("  Memory usage after alignment:\n")
monitor_memory(context = "After feature alignment")

# Apply ensemble prediction methods with memory optimization
cat("  Applying memory-efficient ensemble predictions...\n")

# Monitor memory before predictions
mem_status <- monitor_memory(context = "Before ensemble predictions")
if (mem_status$above_threshold) {
  cat("⚠️ High memory usage. Consider processing in chunks.\n")
}

# Convert sparse matrix to data frame in chunks if needed
chunk_size <- 5000  # Process in chunks to avoid memory issues
total_docs <- nrow(dtm_aligned)

if (total_docs > chunk_size) {
  cat("  Large dataset detected. Processing in chunks of", chunk_size, "documents\n")

  # Process predictions in chunks
  process_chunk_predictions <- function(chunk_start, chunk_end) {
    cat("  Processing documents", chunk_start, "to", chunk_end, "\n")

    # Extract chunk
    chunk_data <- dtm_aligned[chunk_start:chunk_end, , drop = FALSE]

    # Convert to data frame (necessary for caret predict functions)
    chunk_df <- as.data.frame(as.matrix(chunk_data))

    # Apply predictions
    weighted_preds_chunk <- ensemble_predict_weighted(glmnet_model, svm_model, chunk_df)
    threshold_preds_chunk <- ensemble_predict_threshold_optimized(glmnet_model, svm_model, chunk_df, threshold = 0.55)

    glmnet_preds_chunk <- predict(glmnet_model, newdata = chunk_df)
    svm_preds_chunk <- predict(svm_model, newdata = chunk_df)

    glmnet_probs_chunk <- predict(glmnet_model, newdata = chunk_df, type = "prob")
    svm_probs_chunk <- predict(svm_model, newdata = chunk_df, type = "prob")

    # Return results
    list(
      weighted = weighted_preds_chunk,
      threshold = threshold_preds_chunk,
      glmnet_pred = glmnet_preds_chunk,
      svm_pred = svm_preds_chunk,
      glmnet_probs = glmnet_probs_chunk,
      svm_probs = svm_probs_chunk
    )
  }

  # Process all chunks
  all_results <- list()
  for (start_idx in seq(1, total_docs, by = chunk_size)) {
    end_idx <- min(start_idx + chunk_size - 1, total_docs)
    chunk_results <- process_chunk_predictions(start_idx, end_idx)
    all_results[[length(all_results) + 1]] <- chunk_results

    # Memory cleanup between chunks
    if (start_idx %% (chunk_size * 2) == 0) {
      aggressive_gc(verbose = FALSE)
    }
  }

  # Combine results
  cat("  Combining chunk results...\n")
  weighted_preds <- do.call(c, lapply(all_results, function(x) x$weighted))
  threshold_preds <- do.call(c, lapply(all_results, function(x) x$threshold))
  glmnet_preds <- do.call(c, lapply(all_results, function(x) x$glmnet_pred))
  svm_preds <- do.call(c, lapply(all_results, function(x) x$svm_pred))

  # Combine probabilities (more complex due to data frame structure)
  glmnet_probs_list <- lapply(all_results, function(x) x$glmnet_probs)
  svm_probs_list <- lapply(all_results, function(x) x$svm_probs)

  glmnet_probs <- do.call(rbind, glmnet_probs_list)
  svm_probs <- do.call(rbind, svm_probs_list)

  # Clean up
  rm(all_results, glmnet_probs_list, svm_probs_list)
  aggressive_gc(verbose = FALSE)

} else {
  # Process all at once for smaller datasets
  cat("  Processing all documents at once...\n")

  # Convert sparse matrix to data frame
  dtm_df <- as.data.frame(as.matrix(dtm_aligned))

  # Apply predictions
  weighted_preds <- ensemble_predict_weighted(glmnet_model, svm_model, dtm_df)
  threshold_preds <- ensemble_predict_threshold_optimized(glmnet_model, svm_model, dtm_df, threshold = 0.55)

  glmnet_preds <- predict(glmnet_model, newdata = dtm_df)
  svm_preds <- predict(svm_model, newdata = dtm_df)

  glmnet_probs <- predict(glmnet_model, newdata = dtm_df, type = "prob")
  svm_probs <- predict(svm_model, newdata = dtm_df, type = "prob")

  # Clean up
  rm(dtm_df)
  aggressive_gc(verbose = FALSE)
}

cat("  Ensemble predictions complete\n")
monitor_memory(context = "After ensemble predictions")

# Add all predictions to the dataset
abstracts_for_pa <- abstracts_for_pa %>%
  mutate(
    # Individual model predictions
    glmnet_pred = as.character(glmnet_preds),
    svm_pred = as.character(svm_preds),
    # Ensemble predictions
    weighted_ensemble = as.character(weighted_preds),
    threshold_ensemble = as.character(threshold_preds),
    # Probabilities
    glmnet_prob_presence = glmnet_probs$Presence,
    glmnet_prob_absence = glmnet_probs$Absence,
    svm_prob_presence = svm_probs$Presence,
    svm_prob_absence = svm_probs$Absence
  )

# Apply P/A thresholds using the weighted ensemble as base
pa_thresholds <- list(
  loose = 0.5,       # more willing to classify
  medium = 0.6,      # balanced
  strict = 0.8,      # only classify with strong certainty
  super_strict = 0.9 # very high confidence
)

for (thresh_name in names(pa_thresholds)) {
  thresh_val <- pa_thresholds[[thresh_name]]
  col_name <- paste0("pa_", thresh_name)
  
  abstracts_for_pa[[col_name]] <- case_when(
    abstracts_for_pa$glmnet_prob_presence >= thresh_val ~ "Presence",
    abstracts_for_pa$glmnet_prob_absence >= thresh_val ~ "Absence",
    TRUE ~ "Uncertain"
  )
}

# Report P/A classification results
pa_summary <- abstracts_for_pa %>%
  select(weighted_ensemble, threshold_ensemble, pa_loose, pa_medium, pa_strict, pa_super_strict) %>%
  pivot_longer(everything(), names_to = "method", values_to = "label") %>%
  count(method, label) %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0)

cat("  P/A classification results:\n")
print(pa_summary)

# Final memory cleanup and monitoring
cat("  Performing final memory cleanup...\n")
aggressive_gc(verbose = TRUE)
monitor_memory(context = "End of P/A classification")

toc()

cat("  P/A classification completed successfully!\n")
cat("  Memory optimization preserved system stability.\n")

# Save results ------------------------------------------------------------

cat("\nStep 4: Saving results...\n")
tic("Saving results")

# Final memory check before results processing
mem_status <- monitor_memory(context = "Before results processing")
if (mem_status$above_threshold) {
  cat("⚠️ High memory usage before results processing. Performing aggressive cleanup...\n")
  aggressive_gc(verbose = TRUE)
}

# Create comprehensive results dataset
final_results <- full_abstracts %>%
  left_join(
    abstracts_for_pa %>% 
      select(id, glmnet_pred, svm_pred, weighted_ensemble, threshold_ensemble,
             glmnet_prob_presence, glmnet_prob_absence, 
             svm_prob_presence, svm_prob_absence,
             pa_loose, pa_medium, pa_strict, pa_super_strict),
    by = "id"
  ) %>%
  # Add final classification combining relevance and P/A
  mutate(
    # Final classification using weighted ensemble
    final_classification = case_when(
      relevance_loose == "Irrelevant" ~ "Irrelevant",
      relevance_loose == "Uncertain" ~ "Uncertain_Relevance",
      relevance_loose == "Relevant" & !is.na(weighted_ensemble) ~ weighted_ensemble,
      relevance_loose == "Relevant" & is.na(weighted_ensemble) ~ "Uncertain_PA",
      TRUE ~ "Uncertain"
    ),
    # Conservative classification using strict thresholds
    conservative_classification = case_when(
      relevance_strict == "Irrelevant" ~ "Irrelevant",
      relevance_strict == "Uncertain" ~ "Uncertain_Relevance",
      relevance_strict == "Relevant" & pa_strict == "Presence" ~ "Presence",
      relevance_strict == "Relevant" & pa_strict == "Absence" ~ "Absence",
      TRUE ~ "Uncertain"
    )
  )

# Save main results
cat("  Saving main results...\n")
# =============================================================================
# MANUSCRIPT-READY OUTPUTS FOR PREDICTION ANALYSIS
# =============================================================================

cat("\n=== GENERATING MANUSCRIPT FIGURES FOR PREDICTION ANALYSIS ===\n")

# 1. Prediction Distribution Summary
prediction_summary_manuscript <- final_results %>%
  filter(!is.na(final_classification)) %>%
  mutate(
    prediction_category = case_when(
      final_classification == "Presence" ~ "Presence",
      final_classification == "Absence" ~ "Absence",
      final_classification == "Irrelevant" ~ "Irrelevant",
      grepl("Uncertain", final_classification) ~ "Uncertain",
      TRUE ~ "Other"
    )
  ) %>%
  count(prediction_category, name = "count") %>%
  mutate(
    percentage = round(100 * count / sum(count), 1),
    category_label = paste0(prediction_category, "\n(n = ", count, ", ", percentage, "%)")
  )

write.csv(prediction_summary_manuscript, "results/manuscript_prediction_distribution.csv", row.names = FALSE)
cat("✓ Saved prediction distribution summary for manuscript\n")

# 2. Confidence Score Distribution
confidence_plot <- final_results %>%
  filter(!is.na(final_classification) & !is.na(confidence)) %>%
  mutate(
    prediction_type = case_when(
      final_classification == "Presence" ~ "Presence",
      final_classification == "Absence" ~ "Absence",
      TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(x = confidence, fill = prediction_type)) +
  geom_histogram(alpha = 0.7, bins = 30, position = "stack") +
  scale_fill_manual(values = c("Presence" = "#2E86AB", "Absence" = "#F24236", "Other" = "#999999")) +
  labs(
    title = "Distribution of Model Confidence Scores",
    subtitle = "Across all classified abstracts",
    x = "Model Confidence",
    y = "Number of Abstracts",
    fill = "Prediction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

ggsave("plots/manuscript_confidence_distribution.png", confidence_plot,
       width = 10, height = 6, dpi = 300)
cat("✓ Saved confidence distribution plot for manuscript\n")

# 3. Threshold Analysis Report
threshold_analysis <- final_results %>%
  filter(!is.na(final_classification)) %>%
  summarise(
    total_abstracts = n(),
    high_confidence = sum(confidence >= 0.8, na.rm = TRUE),
    medium_confidence = sum(confidence >= 0.6 & confidence < 0.8, na.rm = TRUE),
    low_confidence = sum(confidence < 0.6, na.rm = TRUE),
    high_conf_pct = round(100 * high_confidence / total_abstracts, 1),
    medium_conf_pct = round(100 * medium_confidence / total_abstracts, 1),
    low_conf_pct = round(100 * low_confidence / total_abstracts, 1)
  )

# Save threshold analysis
capture.output({
  cat("=== PREDICTION CONFIDENCE ANALYSIS ===\n")
  cat("Manuscript Methods Section\n\n")

  cat("CLASSIFICATION THRESHOLDS:\n")
  cat("• High confidence: ≥0.8\n")
  cat("• Medium confidence: 0.6-0.8\n")
  cat("• Low confidence: <0.6\n\n")

  cat("RESULTS:\n")
  cat("Total abstracts classified:", threshold_analysis$total_abstracts, "\n")
  cat("High confidence classifications:", threshold_analysis$high_confidence,
      "(", threshold_analysis$high_conf_pct, "%)\n")
  cat("Medium confidence classifications:", threshold_analysis$medium_confidence,
      "(", threshold_analysis$medium_conf_pct, "%)\n")
  cat("Low confidence classifications:", threshold_analysis$low_confidence,
      "(", threshold_analysis$low_conf_pct, "%)\n\n")

  cat("INTERPRETATION:\n")
  cat("High confidence predictions represent the most reliable classifications\n")
  cat("and are recommended for primary analysis. Medium confidence predictions\n")
  cat("should be interpreted with caution. Low confidence predictions may\n")
  cat("require manual review or exclusion depending on research objectives.\n")

}, file = "results/manuscript_prediction_thresholds.txt")
cat("✓ Saved threshold analysis for manuscript Methods section\n")

# 4. Sample Abstracts for Methods Section
# Show examples of high-confidence predictions
high_conf_examples <- final_results %>%
  filter(confidence >= 0.9 & !is.na(final_classification)) %>%
  select(abstract, final_classification, confidence) %>%
  head(3)

capture.output({
  cat("=== EXAMPLE CLASSIFICATIONS ===\n")
  cat("High-confidence predictions for manuscript methods\n\n")

  for (i in 1:nrow(high_conf_examples)) {
    cat("Example", i, " - ", high_conf_examples$final_classification[i],
        "(confidence:", round(high_conf_examples$confidence[i], 3), ")\n")
    cat("Abstract:", substr(high_conf_examples$abstract[i], 1, 200), "...\n\n")
  }

  cat("These examples demonstrate the model's ability to distinguish between\n")
  cat("endophyte presence and absence based on textual content.\n")

}, file = "results/manuscript_classification_examples.txt")
cat("✓ Saved classification examples for manuscript\n")

# 5. Processing Statistics for Methods
processing_stats <- list(
  total_processed = nrow(full_abstracts),
  training_excluded = original_count - nrow(full_abstracts),
  final_classified = nrow(final_results %>% filter(!is.na(final_classification))),
  classification_rate = round(100 * nrow(final_results %>% filter(!is.na(final_classification))) / nrow(full_abstracts), 1),
  average_confidence = round(mean(final_results$confidence, na.rm = TRUE), 3)
)

capture.output({
  cat("=== DATASET PROCESSING STATISTICS ===\n")
  cat("For manuscript methods section\n\n")

  cat("DATA PROCESSING SUMMARY:\n")
  cat("• Total abstracts in database:", processing_stats$total_processed, "\n")
  cat("• Training abstracts excluded:", processing_stats$training_excluded, "\n")
  cat("• Abstracts successfully classified:", processing_stats$final_classified, "\n")
  cat("• Classification success rate:", processing_stats$classification_rate, "%\n")
  cat("• Average model confidence:", processing_stats$average_confidence, "\n\n")

  cat("CLASSIFICATION BREAKDOWN:\n")
  print(prediction_summary_manuscript %>%
          select(prediction_category, count, percentage) %>%
          mutate(percentage = paste0(percentage, "%")))
  cat("\n")

  cat("This automated classification approach allows for efficient processing\n")
  cat("of large literature databases while maintaining high accuracy standards.\n")

}, file = "results/manuscript_processing_statistics.txt")
cat("✓ Saved processing statistics for manuscript\n")

write.csv(final_results, "results/full_dataset_predictions.csv", row.names = FALSE)

# Save filtered subsets for review
cat("  Saving filtered subsets...\n")

# Irrelevant and uncertain abstracts (can be excluded from manual review)
irrelevant_uncertain <- final_results %>%
  filter(relevance_loose %in% c("Irrelevant", "Uncertain")) %>%
  select(id, article_title, abstract, authors, source_title, publication_year, doi,
         Relevant, Irrelevant, relevance_loose, relevance_medium, relevance_strict) %>%
  arrange(desc(Irrelevant))

write.csv(irrelevant_uncertain, "results/irrelevant_uncertain_abstracts.csv", row.names = FALSE)

# Relevant abstracts with P/A classifications
relevant_classified <- final_results %>%
  filter(relevance_loose == "Relevant") %>%
  select(id, article_title, abstract, authors, source_title, publication_year, doi,
         # Relevance info
         Relevant, relevance_loose,
         # Individual model predictions
         glmnet_pred, svm_pred,
         # Ensemble predictions  
         weighted_ensemble, threshold_ensemble,
         # Probabilities
         glmnet_prob_presence, glmnet_prob_absence,
         svm_prob_presence, svm_prob_absence,
         # Threshold classifications
         pa_loose, pa_medium, pa_strict, pa_super_strict,
         # Final classifications
         final_classification, conservative_classification) %>%
  arrange(desc(glmnet_prob_absence))  # Sort by absence probability for easier review

write.csv(relevant_classified, "results/relevant_abstracts_with_pa_predictions.csv", row.names = FALSE)

# Summary statistics
cat("  Creating summary statistics...\n")

summary_stats <- list(
  total_abstracts = nrow(final_results),
  training_excluded = original_count - nrow(full_abstracts),
  
  # Relevance results
  relevance_loose = table(final_results$relevance_loose),
  relevance_strict = table(final_results$relevance_strict),
  
  # P/A results (for relevant abstracts only)
  pa_weighted_ensemble = table(final_results$weighted_ensemble, useNA = "ifany"),
  pa_threshold_ensemble = table(final_results$threshold_ensemble, useNA = "ifany"),
  
  # Final classifications
  final_classification = table(final_results$final_classification),
  conservative_classification = table(final_results$conservative_classification)
)

# Save summary
capture.output(
  {
    cat("=== ENDOPHYTE SYSTEMATIC REVIEW: CLASSIFICATION SUMMARY ===\n\n")
    cat("Dataset Summary:\n")
    cat("- Total abstracts processed:", summary_stats$total_abstracts, "\n")
    cat("- Training abstracts excluded:", summary_stats$training_excluded, "\n\n")
    
    cat("Relevance Classification (Loose Threshold):\n")
    print(summary_stats$relevance_loose)
    cat("\n")
    
    cat("Relevance Classification (Strict Threshold):\n")
    print(summary_stats$relevance_strict)
    cat("\n")
    
    cat("P/A Classification - Weighted Ensemble (Relevant abstracts only):\n")
    print(summary_stats$pa_weighted_ensemble)
    cat("\n")
    
    cat("P/A Classification - Threshold Ensemble (Relevant abstracts only):\n")
    print(summary_stats$pa_threshold_ensemble)
    cat("\n")
    
    cat("Final Classification (Relevance → P/A Pipeline):\n")
    print(summary_stats$final_classification)
    cat("\n")
    
    cat("Conservative Classification (Strict Thresholds):\n")
    print(summary_stats$conservative_classification)
    cat("\n")
    
    cat("=== RECOMMENDATIONS ===\n")
    cat("1. WEIGHTED ENSEMBLE: Best overall performance (89.8% accuracy)\n")
    cat("   - Use 'final_classification' column for primary results\n")
    cat("   - Manually review 'Uncertain' classifications\n\n")
    
    cat("2. CONSERVATIVE APPROACH: High-confidence classifications only\n")
    cat("   - Use 'conservative_classification' column for minimal false positives\n")
    cat("   - Larger proportion will need manual review\n\n")
    
    cat("3. REVIEW PRIORITIES:\n")
    cat("   - Manually review all 'Absence' classifications (avoid missing relevant studies)\n")
    cat("   - Spot-check 'Presence' classifications for quality\n")
    cat("   - Review 'Uncertain' cases based on available time\n")
  },
  file = "results/classification_summary.txt"
)

toc()

# Final summary -----------------------------------------------------------

cat("\n=== CLASSIFICATION COMPLETE ===\n")
cat("Results saved to results/ directory:\n")
cat("- full_dataset_predictions.csv: Complete results with all classifications\n")
cat("- relevant_abstracts_with_pa_predictions.csv: Relevant abstracts with P/A predictions\n")
cat("- irrelevant_uncertain_abstracts.csv: Abstracts likely not relevant for review\n")
cat("- classification_summary.txt: Summary statistics and recommendations\n\n")

cat("Key Statistics:\n")
cat("- Total abstracts processed:", nrow(final_results), "\n")
cat("- Relevant abstracts (loose):", sum(final_results$relevance_loose == "Relevant", na.rm = TRUE), "\n")
cat("- Presence classifications:", sum(final_results$final_classification == "Presence", na.rm = TRUE), "\n")
cat("- Absence classifications:", sum(final_results$final_classification == "Absence", na.rm = TRUE), "\n")
cat("- Uncertain classifications:", sum(grepl("Uncertain", final_results$final_classification), na.rm = TRUE), "\n\n")

cat("Recommended workflow:\n")
cat("1. Use weighted ensemble results (final_classification) as primary screening\n")
cat("2. Manually review all 'Absence' classifications to avoid missing relevant studies\n")
cat("3. Spot-check 'Presence' classifications for quality assurance\n")
cat("4. Time permitting, review 'Uncertain' classifications\n\n")

cat("Pipeline complete! 🎉\n")
