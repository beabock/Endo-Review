#B. Bock
#6/19/25
#ML Approach: this is adapted from ML_bigger.R. 
#This time, I want to subset my training dataset, and compare different ML approaches on them. See what works best then run those on bigger training datasets and run again.


#7/23/25 Coming back to this since this seemed like the best approach. Adding in relevance filtering step.

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

# Relevance Classification (Relevant vs Irrelevant) ----------------------


set.seed(1998)

labeled_abstracts <- read.csv("data/Training_labeled_abs_5.csv") %>%
  clean_names() %>%
 # filter(label %in% c("Presence", "Absence")) %>%
  mutate(id = row_number())%>%
  filter(label != "")%>%
  mutate(
    relevance = case_when(
      label %in% c("Presence", "Absence", "Both") ~ "Relevant",
      label %in% c("Review", "Other") ~ "Irrelevant",
      TRUE ~ NA_character_
    ),
    relevance = factor(relevance)
  )%>%
  mutate(
    presence_both_absence = case_when(
      label == "Presence" ~ "Presence",
      label == "Absence" ~ "Absence",
      label == "Both" ~ "Presence",
      TRUE ~ NA_character_
    ),
    presence_both_absence = factor(presence_both_absence, levels = c("Presence", "Absence"))
  )


# Remove artificial or duplicate Presence examples
rows_to_remove <- labeled_abstracts %>%
  filter(
    is.na(doi) | doi %in% c("", "<NA>", "NA"),
    label == "Presence",
    is.na(authors) | authors == ""
  )

labeled_abstracts <- labeled_abstracts %>%
  anti_join(rows_to_remove, by = "id") %>%
  ungroup()

# DTM creation and alignment
dtm <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

dtm_matrix <- as.matrix(dtm)
colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)
rownames_dtm <- dtm$dimnames$Docs
rownames(dtm_matrix) <- rownames_dtm
dtm_df <- as.data.frame(dtm_matrix)
stopifnot(!any(duplicated(colnames(dtm_df))))

# Align abstracts and DTM
labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% rownames(dtm_matrix)) %>%
  mutate(relevance = factor(relevance)) %>%
  arrange(match(id, rownames(dtm_matrix)))

# Train-test split
train_index <- createDataPartition(labeled_abstracts$relevance, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

train_ids <- as.character(train_data$id)
test_ids <- as.character(test_data$id)

train_matrix <- dtm_matrix[train_ids, ]
test_matrix <- dtm_matrix[test_ids, ]

# Convert to data frame for caret
train_df <- as.data.frame(as.matrix(train_matrix)) %>% 
  mutate(relevance = train_data$relevance)

test_df <- as.data.frame(as.matrix(test_matrix)) %>% 
  mutate(relevance = test_data$relevance)

# Apply SMOTE for class balancing
train_recipe <- recipe(relevance ~ ., data = train_df) %>%
  step_smote(relevance) %>%
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
models_to_try <- c("glmnet") # Can add rf back in but it kind of sucks. can also add svmLinear back if wanted, but glmnet performs the best.
results <- list()

for (method in models_to_try) {
  cat("\nTraining:", method, "\n")
  tic(paste("Time for", method))
  result <- tryCatch({
    train(
      relevance ~ ., 
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


confusion_matrices <- list() 
# Evaluate
accuracy_table <- tibble(
  model = names(results),
  accuracy = sapply(names(results), function(m) {
    model <- results[[m]]
    if (is.null(model)) return(NA)
    
    preds <- predict(model, newdata = test_df)
    cm <- confusionMatrix(preds, test_df$relevance)
    
    # Save confusion matrix to list for later inspection
    confusion_matrices[[m]] <<- cm
    
    cm$overall["Accuracy"]
  })
) %>% filter(!is.na(accuracy)) %>% arrange(desc(accuracy))

print(accuracy_table)

#91% on glmnet, pretty good. 

for (m in names(confusion_matrices)) {
  cat("\nConfusion Matrix for model:", m, "\n")
  print(confusion_matrices[[m]]$table)  # Just the table
  cat("\nDetailed stats:\n")
  print(confusion_matrices[[m]]$byClass)  # Per-class metrics (Sensitivity, Specificity, etc.)
  cat("\n")
}

# Save best model
best_model <- results[[accuracy_table$model[1]]]

setdiff(colnames(best_model$trainingData), best_model$finalModel$xNames)
#xnames <- best_model$finalModel$xNames
#best_model$trainingData <- best_model$trainingData[, c(xnames, ".outcome"), drop = FALSE]



saveRDS(best_model, file = paste0("models/best_model_relevance_", accuracy_table$model[1], ".rds"))


#svmLinear it is!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Same thing but now P/A --------------------------------------------

labeled_abstracts <- labeled_abstracts %>%
  filter(relevance == "Relevant")

labeled_abstracts %>%
  count(presence_both_absence)

dtm <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  #mutate(word = str_replace_all(word, "'s\\b", ""))  %>% # Remove possessives
  #filter(!str_detect(word, "'")) %>%  # Remove any word containing an apostrophe
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%  # Ensure IDs are character
  cast_dtm(document = id, term = word, value = n) #Think about this more.

# Preserve row names BEFORE converting
rownames_dtm <- dtm$dimnames$Docs  # Extract doc IDs from DTM

dtm_matrix <- as.matrix(dtm)
colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)

# Continue safely
dtm_df <- as.data.frame(dtm_matrix)

# Check
stopifnot(!any(duplicated(colnames(dtm_df))))



# Get column names ordered by decreasing column sum
order_cols <- names(sort(colSums(dtm_df), decreasing = TRUE))

# Reorder the columns
dtm_df <- dtm_df[, order_cols]

valid_ids <- as.integer(rownames_dtm)  # convert back to integer

labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% valid_ids) %>%
  mutate(id = as.character(id)) %>%         # match rownames (character)
  arrange(match(id, rownames_dtm))  


rownames(dtm_matrix) <- rownames_dtm

labeled_abstracts <- labeled_abstracts %>%
  filter(id %in% rownames(dtm_matrix)) %>%
  mutate(presence_both_absence = factor(presence_both_absence))

# Train-test split
train_index <- createDataPartition(labeled_abstracts$presence_both_absence, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

train_ids <- as.character(train_data$id)
test_ids <- as.character(test_data$id)

train_matrix <- dtm_matrix[train_ids, ]
test_matrix <- dtm_matrix[test_ids, ]

# Convert to data frame for caret
train_df <- as.data.frame(as.matrix(train_matrix)) %>% mutate(presence_both_absence = train_data$presence_both_absence)

test_df <- as.data.frame(as.matrix(test_matrix)) %>% mutate(presence_both_absence = test_data$presence_both_absence)


# Testing models ----------------------------------------------------------


#glmnet and svmLinear are super fast.

train_recipe <- recipe(presence_both_absence ~ ., data = train_df) %>%
  step_smote(presence_both_absence) %>%
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
      presence_both_absence ~ ., 
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

confusion_matrices <- list() 
# Evaluate
accuracy_table <- tibble(
  model = names(results),
  accuracy = sapply(names(results), function(m) {
    model <- results[[m]]
    if (is.null(model)) return(NA)
    
    preds <- predict(model, newdata = test_df)
    cm <- confusionMatrix(preds, test_df$presence_both_absence)
    
    # Save confusion matrix to list for later inspection
    confusion_matrices[[m]] <<- cm
    
    cm$overall["Accuracy"]
  })
) %>% filter(!is.na(accuracy)) %>% arrange(desc(accuracy))

print(accuracy_table)

#90%, pretty good.

for (m in names(confusion_matrices)) {
  cat("\nConfusion Matrix for model:", m, "\n")
  print(confusion_matrices[[m]]$table)  # Just the table
  cat("\nDetailed stats:\n")
  print(confusion_matrices[[m]]$byClass)  # Per-class metrics (Sensitivity, Specificity, etc.)
  cat("\n")
}



# Save best model
best_model <- results[[accuracy_table$model[1]]]


saveRDS(best_model, file = paste0("models/best_model_presence_", accuracy_table$model[1], ".rds"))


best_model
#This part is seemingly doing well...


# Whole dataset -----------------------------------------------------------



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

full_abstracts <- read.csv("data/All_Abstracts.csv") %>%
  clean_names()


# Apply your column name harmonization
full_abstracts <- full_abstracts %>%
  rename_with(~ ifelse(. %in% names(colname_mapping), colname_mapping[.], .))

# Filter out labeled DOIs (so we’re not predicting on training data)
filtered_dois <- labeled_abstracts$doi[!is.na(labeled_abstracts$doi) & labeled_abstracts$doi != ""]
full_abstracts <- full_abstracts[!full_abstracts$doi %in% filtered_dois, ]

# Add unique ID column for tracking documents
full_abstracts$id <- 1:nrow(full_abstracts)

# Construct DTM (no make.names!)
dtm <- full_abstracts %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
 # mutate(word = str_replace_all(word, "'s\\b", ""))  %>% # Remove possessives
 # filter(!str_detect(word, "'")) %>%  # Remove any word containing an apostrophe
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

# Convert to matrix and assign rownames
dtm_matrix <- as.matrix(dtm)

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)

# Continue safely

# Check

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix)) #Santize
rownames(dtm_matrix) <- dtm$dimnames$Docs  # these are the character ids

dtm_df <- as.data.frame(dtm_matrix)

stopifnot(!any(duplicated(colnames(dtm_df))))


# Load the trained model
rel_model <- readRDS("models/best_model_relevance_glmnet.rds")

# Get training feature names
trained_vocab <- rel_model$finalModel$xNames

# Add missing columns (words present in training but not here)
missing_words <- setdiff(trained_vocab, colnames(dtm_df))
if (length(missing_words) > 0) {
  zero_matrix <- matrix(0, nrow = nrow(dtm_df), ncol = length(missing_words),
                        dimnames = list(rownames(dtm_df), missing_words))
  dtm_df <- cbind(dtm_df, zero_matrix)
}

# Remove extra columns not in trained vocab
dtm_df <- dtm_df[, colnames(dtm_df) %in% trained_vocab]

# Reorder columns to match the trained model’s input order
dtm_df <- dtm_df[, trained_vocab, drop = FALSE]

# Convert to dataframe
full_df <- dtm_df

# Sanity check (should be empty):
 head(setdiff(rel_model$finalModel$xNames, colnames(full_df)))

head(setdiff(colnames(rel_model$trainingData), colnames(full_df)))

# Predict
probs <- predict(rel_model, newdata = full_df, type = "prob")

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
      Relevant >= loose_thresh ~ "Relevant",
      Irrelevant >= loose_thresh ~ "Irrelevant",
      TRUE ~ "Uncertain"
    ),
    label_medium = case_when(
      Relevant >= medium_thresh ~ "Relevant",
      Irrelevant >= medium_thresh ~ "Irrelevant",
      TRUE ~ "Uncertain"
    ),
    label_strict = case_when(
      Relevant >= strict_thresh ~ "Relevant",
      Irrelevant >= strict_thresh ~ "Irrelevant",
      TRUE ~ "Uncertain"
    )
  )

full_abstracts %>%
  select(label_loose, label_medium, label_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label)


# Save the results
write.csv(full_abstracts, "relevance_preds.csv", row.names = FALSE)

irr_un <- full_abstracts %>%
  filter(label_loose == "Irrelevant" | label_loose == "Uncertain")%>%
  relocate(label_loose, abstract)

write.csv(irr_un, "irrelevant_uncertain_abstracts.csv", row.names = FALSE)

abstracts_with_rel <- read.csv("relevance_preds.csv")%>%
  filter(label_loose == "Relevant")

abstracts_with_rel %>%
  select(label_loose, label_medium, label_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label)

#Now do the same with P/A

dtm <- abstracts_with_rel %>%
  unnest_tokens(word, abstract, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
 # mutate(word = str_replace_all(word, "'s\\b", ""))  %>% # Remove possessives
 # filter(!str_detect(word, "'")) %>%  # Remove any word containing an apostrophe
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  cast_dtm(document = id, term = word, value = n)

# Convert to matrix and assign rownames
dtm_matrix <- as.matrix(dtm)

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix), unique = TRUE)

# Continue safely

# Check

colnames(dtm_matrix) <- make.names(colnames(dtm_matrix)) #Santize
rownames(dtm_matrix) <- dtm$dimnames$Docs  # these are the character ids

dtm_df <- as.data.frame(dtm_matrix)

stopifnot(!any(duplicated(colnames(dtm_df))))


# Load the trained model
pa_model <- readRDS("models/best_model_presence_svmLinear.rds")

# Get training feature names
trained_vocab <- setdiff(colnames(pa_model$trainingData), ".outcome")

# Add missing columns (words present in training but not here)
missing_words <- setdiff(trained_vocab, colnames(dtm_df))
if (length(missing_words) > 0) {
  zero_matrix <- matrix(0, nrow = nrow(dtm_df), ncol = length(missing_words),
                        dimnames = list(rownames(dtm_df), missing_words))
  dtm_df <- cbind(dtm_df, zero_matrix)
}

# Remove extra columns not in trained vocab
dtm_df <- dtm_df[, colnames(dtm_df) %in% trained_vocab]

# Reorder columns to match the trained model’s input order
dtm_df <- dtm_df[, trained_vocab, drop = FALSE]

# Convert to dataframe
full_df <- dtm_df

# Sanity check (should be empty):
 head(setdiff(colnames(pa_model$finalModel), colnames(full_df)))

head(setdiff(colnames(pa_model$trainingData), colnames(full_df)))

# Predict
probs <- predict(pa_model, newdata = full_df, type = "prob")

abstracts_with_rel  <- abstracts_with_rel  %>%
  bind_cols(probs)

# Define thresholds
loose_thresh <- 0.5   # more willing to classify
medium_thresh <- 0.6  # balanced
strict_thresh <- 0.8  # only classify with strong certainty
super_strict_thresh <- 0.9  # very high confidence

# Apply each thresholding scheme
abstracts_with_rel <- abstracts_with_rel %>%
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
    ),
    label_super_strict = case_when(
      Presence >= super_strict_thresh ~ "Presence",
      Absence >= super_strict_thresh ~ "Absence",
      TRUE ~ "Uncertain"
    )
  )

abstracts_with_rel %>%
  select(label_loose, label_medium, label_strict, label_super_strict) %>%
  pivot_longer(everything(), names_to = "threshold", values_to = "label") %>%
  count(threshold, label)

# Save the results
write.csv(abstracts_with_rel, "relevance_pa_preds_all_abstracts.csv", row.names = FALSE)

#Go with strict and manually review uncertain and absence...