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

cat("=== ENDOPHYTE SYSTEMATIC REVIEW: FULL DATASET CLASSIFICATION ===\n")
cat("Pipeline: Relevance → Presence/Absence Classification\n")
cat("Models: glmnet (relevance), weighted ensemble (P/A)\n\n")

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

# Load trained relevance model first to get vocabulary
cat("  Loading trained relevance model...\n")
rel_model <- readRDS("models/best_model_relevance_glmnet.rds")
trained_vocab <- rel_model$finalModel$xNames
cat("  Training vocabulary size:", length(trained_vocab), "terms\n")

# Process in batches to manage memory
batch_size <- 2000  # Adjust based on available RAM
n_batches <- ceiling(nrow(full_abstracts) / batch_size)
cat("  Processing", nrow(full_abstracts), "abstracts in", n_batches, "batches\n")

all_predictions <- list()

for(i in 1:n_batches) {
  cat("  Processing batch", i, "of", n_batches, "...\n")
  
  # Get batch indices
  start_idx <- (i-1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(full_abstracts))
  
  # Extract batch data
  batch_data <- full_abstracts[start_idx:end_idx, ]
  
  # Create DTM for this batch - filter to training vocabulary early
  dtm_batch <- batch_data %>%
    unnest_tokens(word, abstract, token = "words") %>%
    anti_join(stop_words, by = "word") %>%
    mutate(word = str_to_lower(word)) %>%
    filter(!str_detect(word, "\\d")) %>%
    # Only keep words that were in training vocabulary to reduce memory
    filter(word %in% trained_vocab) %>%
    count(id, word, sort = TRUE) %>%
    ungroup() %>%
    mutate(id = as.character(id)) %>%
    cast_dtm(document = id, term = word, value = n)
  
  # Convert to matrix and sanitize
  dtm_matrix_batch <- as.matrix(dtm_batch)
  colnames(dtm_matrix_batch) <- make.names(colnames(dtm_matrix_batch), unique = TRUE)
  dtm_df_batch <- as.data.frame(dtm_matrix_batch)
  
  # Add missing columns (words in training but not in this batch)
  missing_words <- setdiff(trained_vocab, colnames(dtm_df_batch))
  if (length(missing_words) > 0) {
    zero_matrix <- matrix(0, nrow = nrow(dtm_df_batch), ncol = length(missing_words),
                          dimnames = list(rownames(dtm_df_batch), missing_words))
    dtm_df_batch <- cbind(dtm_df_batch, zero_matrix)
  }
  
  # Reorder columns to match training
  dtm_df_batch <- dtm_df_batch[, trained_vocab, drop = FALSE]
  
  # Predict for this batch
  batch_probs <- predict(rel_model, newdata = dtm_df_batch, type = "prob")
  batch_results <- batch_data %>%
    bind_cols(batch_probs)
  
  # Store results
  all_predictions[[i]] <- batch_results
  
  # Clean up memory
  rm(dtm_batch, dtm_matrix_batch, dtm_df_batch, batch_probs, batch_results)
  gc()
}

# Combine all batch results
cat("  Combining batch results...\n")
full_abstracts <- bind_rows(all_predictions)
rm(all_predictions)
gc()

cat("  Relevance classification complete for", nrow(full_abstracts), "abstracts\n")

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

# STEP 2: Presence/Absence Classification ---------------------------------

cat("\nStep 3: Presence/Absence Classification...\n")
tic("Presence/Absence classification")

# Load P/A models first to get vocabulary
cat("  Loading P/A models...\n")
glmnet_model <- readRDS("models/best_model_presence_glmnet_ensemble.rds")
svm_model <- readRDS("models/best_model_presence_svmLinear_ensemble.rds")

# Get training vocabulary from one of the models
pa_trained_vocab <- setdiff(colnames(svm_model$trainingData), ".outcome")
cat("  P/A training vocabulary size:", length(pa_trained_vocab), "terms\n")

# Process P/A classification in batches
pa_batch_size <- 1000  # Smaller batches for P/A due to bigrams
n_pa_batches <- ceiling(nrow(abstracts_for_pa) / pa_batch_size)
cat("  Processing", nrow(abstracts_for_pa), "relevant abstracts in", n_pa_batches, "batches\n")

all_pa_predictions <- list()

for(i in 1:n_pa_batches) {
  cat("  Processing P/A batch", i, "of", n_pa_batches, "...\n")
  
  # Get batch indices
  start_idx <- (i-1) * pa_batch_size + 1
  end_idx <- min(i * pa_batch_size, nrow(abstracts_for_pa))
  
  # Extract batch data
  batch_data <- abstracts_for_pa[start_idx:end_idx, ]
  
  # Create unigrams for this batch
  dtm_unigrams_batch <- batch_data %>%
    unnest_tokens(word, abstract, token = "words") %>%
    anti_join(stop_words, by = "word") %>%
    mutate(word = str_to_lower(word)) %>%
    filter(!str_detect(word, "\\d")) %>%
    # Filter to training vocabulary early
    filter(word %in% pa_trained_vocab) %>%
    count(id, word, sort = TRUE) %>%
    ungroup() %>%
    mutate(id = as.character(id))
  
  # Create bigrams for this batch
  dtm_bigrams_batch <- batch_data %>%
    unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    unite(bigram, word1, word2, sep = "_") %>%
    mutate(bigram = str_to_lower(bigram)) %>%
    # Filter to training vocabulary early
    filter(bigram %in% pa_trained_vocab) %>%
    count(id, bigram, sort = TRUE) %>%
    ungroup() %>%
    mutate(id = as.character(id)) %>%
    rename(word = bigram)
  
  # Combine and create DTM
  if(nrow(dtm_unigrams_batch) > 0 || nrow(dtm_bigrams_batch) > 0) {
    dtm_combined_batch <- bind_rows(dtm_unigrams_batch, dtm_bigrams_batch) %>%
      cast_dtm(document = id, term = word, value = n)
    
    # Convert to matrix and sanitize
    dtm_matrix_batch <- as.matrix(dtm_combined_batch)
    colnames(dtm_matrix_batch) <- make.names(colnames(dtm_matrix_batch), unique = TRUE)
    dtm_df_batch <- as.data.frame(dtm_matrix_batch)
    
    # Add missing columns
    missing_words <- setdiff(pa_trained_vocab, colnames(dtm_df_batch))
    if (length(missing_words) > 0) {
      zero_matrix <- matrix(0, nrow = nrow(dtm_df_batch), ncol = length(missing_words),
                            dimnames = list(rownames(dtm_df_batch), missing_words))
      dtm_df_batch <- cbind(dtm_df_batch, zero_matrix)
    }
    
    # Reorder columns to match training
    dtm_df_batch <- dtm_df_batch[, pa_trained_vocab, drop = FALSE]
    
    # Predict using ensemble
    glmnet_probs <- predict(glmnet_model, newdata = dtm_df_batch, type = "prob")
    svm_probs <- predict(svm_model, newdata = dtm_df_batch, type = "prob")
    
    # Apply ensemble weights (matching training configuration)
    svm_weight_presence <- 0.6
    glm_weight_absence <- 0.8
    
    ensemble_presence_prob <- (svm_probs$Presence * svm_weight_presence + 
                              glmnet_probs$Presence * (1 - svm_weight_presence))
    ensemble_absence_prob <- (glmnet_probs$Absence * glm_weight_absence + 
                             svm_probs$Absence * (1 - glm_weight_absence))
    
    # Make ensemble predictions
    ensemble_preds <- ifelse(ensemble_presence_prob > ensemble_absence_prob, "Presence", "Absence")
    ensemble_preds <- factor(ensemble_preds, levels = c("Presence", "Absence"))
    
    # Combine results
    batch_results <- batch_data %>%
      bind_cols(
        Presence = svm_probs$Presence,
        Absence = svm_probs$Absence,
        ensemble_presence_prob = ensemble_presence_prob,
        ensemble_absence_prob = ensemble_absence_prob,
        ensemble_prediction = ensemble_preds
      )
  } else {
    # Handle empty batch
    batch_results <- batch_data %>%
      mutate(
        Presence = 0.5,
        Absence = 0.5,
        ensemble_presence_prob = 0.5,
        ensemble_absence_prob = 0.5,
        ensemble_prediction = factor("Presence", levels = c("Presence", "Absence"))
      )
  }
  
  # Store results
  all_pa_predictions[[i]] <- batch_results
  
  # Clean up memory
  rm(dtm_unigrams_batch, dtm_bigrams_batch, dtm_df_batch)
  if(exists("dtm_combined_batch")) rm(dtm_combined_batch, dtm_matrix_batch)
  if(exists("glmnet_probs")) rm(glmnet_probs, svm_probs)
  gc()
}

# Combine all P/A batch results
cat("  Combining P/A batch results...\n")
abstracts_for_pa <- bind_rows(all_pa_predictions)
rm(all_pa_predictions)
gc()

cat("  P/A classification complete for", nrow(abstracts_for_pa), "abstracts\n")

# Apply P/A thresholds using the ensemble probabilities from batch processing
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
    abstracts_for_pa$ensemble_presence_prob >= thresh_val ~ "Presence",
    abstracts_for_pa$ensemble_absence_prob >= thresh_val ~ "Absence",
    TRUE ~ "Uncertain"
  )
}

# Report P/A classification results
pa_summary <- abstracts_for_pa %>%
  select(ensemble_prediction, pa_loose, pa_medium, pa_strict, pa_super_strict) %>%
  pivot_longer(everything(), names_to = "method", values_to = "label") %>%
  count(method, label) %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0)

cat("  P/A classification results:\n")
print(pa_summary)

toc()

# Save results ------------------------------------------------------------

cat("\nStep 4: Saving results...\n")
tic("Saving results")

# Create comprehensive results dataset
final_results <- full_abstracts %>%
  left_join(
    abstracts_for_pa %>% 
      select(id, ensemble_prediction, ensemble_presence_prob, ensemble_absence_prob,
             pa_loose, pa_medium, pa_strict, pa_super_strict),
    by = "id"
  ) %>%
  # Add final classification combining relevance and P/A
  mutate(
    # Final classification using ensemble prediction
    final_classification = case_when(
      relevance_loose == "Irrelevant" ~ "Irrelevant",
      relevance_loose == "Uncertain" ~ "Uncertain_Relevance",
      relevance_loose == "Relevant" & !is.na(ensemble_prediction) ~ as.character(ensemble_prediction),
      relevance_loose == "Relevant" & is.na(ensemble_prediction) ~ "Uncertain_PA",
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
         # Ensemble prediction
         ensemble_prediction,
         # Probabilities
         ensemble_presence_prob, ensemble_absence_prob,
         # Threshold classifications
         pa_loose, pa_medium, pa_strict, pa_super_strict,
         # Final classifications
         final_classification, conservative_classification) %>%
  arrange(desc(ensemble_absence_prob))  # Sort by absence probability for easier review

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
  pa_ensemble = table(final_results$ensemble_prediction, useNA = "ifany"),
  
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
    
    cat("P/A Classification - Ensemble (Relevant abstracts only):\n")
    print(summary_stats$pa_ensemble)
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
