#B. Bock
#6/19/25
#ML Approach: this is adapted from ML_bigger.R. 
#This time, I want to subset my training dataset, and compare different ML approaches on them. See what works best then run those on bigger training datasets and run again.


# Library loading ---------------------------------------------------------



# Load necessary libraries

library(tidyverse)
library(tidytext)
library(caret)
library(Matrix)
library(text)
library(tm)
library(recipes)
library(themis)
library(janitor)
library(tictoc)

getwd()

#setwd("Endo_Review")

cus_pal <- c(
  "#A1C181",  # soft sage green — for plants
  "#619B8A",  # muted teal — evokes moss or lichens
  "#C97E7E",  # dusty rose — for fungi like Russula or Hygrophoropsis
  "#D9AE94"   # pale mushroom beige — for caps and forest floor tones
)

# Functions ---------------------------------------------------------------
save_plot <- function(filename, plot, width = 12, height = 7, units = "in", ...) {
  ggsave(filename, plot, width = width, height = height, units = units, ...)
}

train_and_evaluate <- function(train_df, test_df, method_name, tune_len = 10, ...) {
  train_control <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    classProbs = TRUE,
    summaryFunction = multiClassSummary,
    savePredictions = "final"
  )
  
  model <- train(
    label ~ ., 
    data = train_df,
    method = method_name,
    trControl = train_control,
    tuneLength = tune_len,
    ...
  )
  
  predictions <- predict(model, newdata = test_df)
  cm <- confusionMatrix(predictions, test_df$label)
  
  list(
    model = model,
    confusion_matrix = cm,
    accuracy = cm$overall["Accuracy"]
  )
}


# Presence/Absence --------------------------------------------------------


set.seed(1998)

labeled_abstracts <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(label %in% c("Presence", "Absence")) %>%
  mutate(id = row_number())


# Remove artificial or duplicate Presence examples
rows_to_remove <- labeled_abstracts %>%
  filter(
    is.na(doi) | doi %in% c("", "<NA>", "NA"),
    label == "Presence",
    is.na(authors) | authors == ""
  )

labeled_abstracts <- labeled_abstracts %>%
  anti_join(rows_to_remove, by = "id")%>%
  group_by(label)%>%
 # slice_sample(n=30)%>%
  ungroup()

# Check label balance
labeled_abstracts %>%
  count(label) 

# # Metadata columns
# target <- "label"
# predictor <- "abstract"
# metadata_columns <- setdiff(names(labeled_abstracts), c(target, predictor)) %>%
#   discard(~ .x %in% c("publication_type", "group_authors", "part_number", "web_of_science_index"))
# 
# labeled_abstracts <- labeled_abstracts %>%
#   select(all_of(c(target, predictor, metadata_columns, "id")))

dtm <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%  # Ensure IDs are character
  cast_dtm(document = id, term = word, value = n)

# Preserve row names BEFORE converting
rownames_dtm <- dtm$dimnames$Docs  # Extract doc IDs from DTM

# Convert to sparse matrix and assign row names
dtm_matrix <- as.matrix(dtm)

valid_ids <- as.integer(rownames_dtm)  # convert back to integer

labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% valid_ids) %>%
  mutate(id = as.character(id)) %>%         # match rownames (character)
  arrange(match(id, rownames_dtm))  


rownames(dtm_matrix) <- rownames_dtm

labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% rownames(dtm_matrix)) %>%
  mutate(label = factor(label))

# Train-test split
train_index <- createDataPartition(labeled_abstracts$label, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

train_ids <- as.character(train_data$id)
test_ids <- as.character(test_data$id)

train_matrix <- dtm_matrix[train_ids, ]
test_matrix <- dtm_matrix[test_ids, ]

train_dtm_matrix <- train_matrix

# Convert to data frame for caret
train_df <- as.data.frame(as.matrix(train_matrix)) %>% mutate(label = train_data$label)
test_df <- as.data.frame(as.matrix(test_matrix)) %>% mutate(label = test_data$label)


# Testing models ----------------------------------------------------------


#glmnet and svmLinear are super fast.

train_recipe <- recipe(label ~ ., data = train_df) %>%
  step_smote(label) %>%
  prep()
balanced_train <- juice(train_recipe)

# Train Control
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Models to compare
models_to_try <- c("glmnet", "svmLinear") #Can add rf back in but it kind of sucks
results <- list()

for (method in models_to_try) {
  cat("\nTraining:", method, "\n")
  tic(paste("Time for", method))
  result <- tryCatch({
    train(
      label ~ ., 
      data = balanced_train,
      method = method,
      metric = "ROC",
      trControl = train_control,
      tuneLength = 10
    )
  }, error = function(e) {
    message("Model ", method, " failed: ", e$message)
    return(NULL)
  })
  toc()
  results[[method]] <- result
}

#Ran on whole training dataset this time.

confusion_matrices <- list() 
# Evaluate
accuracy_table <- tibble(
  model = names(results),
  accuracy = sapply(names(results), function(m) {
    model <- results[[m]]
    if (is.null(model)) return(NA)
    
    preds <- predict(model, newdata = test_df)
    cm <- confusionMatrix(preds, test_df$label)
    
    # Save confusion matrix to list for later inspection
    confusion_matrices[[m]] <<- cm
    
    cm$overall["Accuracy"]
  })
) %>% filter(!is.na(accuracy)) %>% arrange(desc(accuracy))

print(accuracy_table)

#98%!!!! holy hell!!!

for (m in names(confusion_matrices)) {
  cat("\nConfusion Matrix for model:", m, "\n")
  print(confusion_matrices[[m]]$table)  # Just the table
  cat("\nDetailed stats:\n")
  print(confusion_matrices[[m]]$byClass)  # Per-class metrics (Sensitivity, Specificity, etc.)
  cat("\n")
}

accuracy_table %>%
  ggplot(aes(x = reorder(model, accuracy), y = accuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Model Accuracy Comparison",
    x = "Model",
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 14)


# Save best model
best_model <- results[[accuracy_table$model[1]]]
#saveRDS(best_model, file = paste0("best_model_", accuracy_table$model[1], ".rds"))


best_model
#svmLinear it is!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Doing some pca stuff here
library(irlba)

# Use training matrix (only rownames with labels)
svd_result <- irlba(train_matrix, nv = 2)  # 2 PCs for plotting

# Project into PC space
pca_coords <- svd_result$u %*% diag(svd_result$d)

# Recreate labels
train_labels <- train_data$label

# Data frame for plotting
pca_df <- data.frame(
  PC1 = pca_coords[, 1],
  PC2 = pca_coords[, 2],
  label = train_labels
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = label)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "PCA of Abstract Word Features (Training Set)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  scale_color_manual(values = c("Presence" = "#0072B2", "Absence" = "#D55E00"))

save_plot(plot = last_plot(), "PCA_Label.png")

preds <- predict(best_model, newdata = train_df)

# Add prediction info to PCA plot
pca_df$predicted <- preds
pca_df$correct <- ifelse(pca_df$label == pca_df$predicted, "Correct", "Incorrect")

ggplot(pca_df, aes(x = PC1, y = PC2, color = correct)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "SVM Classification Accuracy in PCA Space",
    x = "PC1", y = "PC2"
  ) +
  scale_color_manual(values = c("Correct" = "forestgreen", "Incorrect" = "firebrick")) 

save_plot(plot = last_plot(), "PCA_correct.png")

# Whole dataset -----------------------------------------------------------


best_model <- readRDS("best_model_svmLinear.rds")
# Step 8: Predict on the Full Dataset
full_abstracts <- read.csv("All_Abstracts.csv") %>%
  clean_names()



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


full_abstracts <- full_abstracts %>%
  rename_with(~ ifelse(. %in% names(colname_mapping), colname_mapping[.], .))


metadata_columns <- metadata_columns[!metadata_columns %in% c("id", "predicted_label")]


full_abstracts <- full_abstracts %>%
  select(c(predictor, all_of(metadata_columns)))


filtered_dois <- labeled_abstracts$doi[!is.na(labeled_abstracts$doi) & labeled_abstracts$doi != ""]
full_abstracts <- full_abstracts[!full_abstracts$doi %in% filtered_dois, ]

# Add ID column for consistent row tracking
full_abstracts$id <- 1:nrow(full_abstracts)

dtm <- full_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%  # Ensure IDs are character
  cast_dtm(document = id, term = word, value = n)


dtm_matrix <- as.matrix(dtm)
rownames(dtm_matrix) <- dtm$dimnames$Docs  # Ensure IDs as rownames

# Load vocabulary from training data (should already exist in your script)
trained_vocab <- colnames(train_dtm_matrix)

# Add missing words (zero columns) to full DTM
missing_words <- setdiff(trained_vocab, colnames(dtm_matrix))
if (length(missing_words) > 0) {
  zero_matrix <- matrix(0, nrow = nrow(dtm_matrix), ncol = length(missing_words),
                        dimnames = list(rownames(dtm_matrix), missing_words))
  dtm_matrix <- cbind(dtm_matrix, zero_matrix)
}

# Reorder columns to match training vocab
dtm_matrix <- dtm_matrix[, trained_vocab, drop = FALSE]

# Predict labels using best model
# Ensure model is loaded and ready
train_vars <- setdiff(colnames(train_df), "label")

# 2. Convert DTM to dataframe
full_df <- as.data.frame(dtm_matrix)

missing_cols <- setdiff(train_vars, colnames(full_df))

if (length(missing_cols) > 0) {
  full_df <- bind_cols(
    full_df,
    missing_cols %>%
      set_names() %>%
      purrr::map_dfc(~ rep(0, nrow(full_df)))
  )
}

# 4. Reorder to match training variable order
full_df <- full_df %>% select(all_of(train_vars))

probs <- predict(best_model, newdata = full_df, type = "prob")

full_abstracts <- full_abstracts %>%
  bind_cols(probs)

# Define thresholds
loose_thresh <- 0.5   # more willing to classify
medium_thresh <- 0.6  # balanced
strict_thresh <- 0.8  # only classify with strong certainty

# Apply each thresholding scheme
full_abstracts <- full_abstracts %>%
  mutate(
    label_loose = case_when(
      Presence >= loose_thresh ~ "Presence",
      Absence >= loose_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    ),
    label_medium = case_when(
      Presence >= medium_thresh ~ "Presence",
      Absence >= medium_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    ),
    label_strict = case_when(
      Presence >= strict_thresh ~ "Presence",
      Absence >= strict_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    )
  )

full_abstracts %>%
  select(label_loose, label_medium, label_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label)

# Save the results
write.csv(full_abstracts, "full_predictions_with_metadata.csv", row.names = FALSE)



