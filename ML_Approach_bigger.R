#B. Bock
#3/7/25
#ML Approach: changed from words to trigrams

#Could try stacking models too. Maybe try that next.

# Load necessary libraries

packages <- c("tidyverse", "tidytext", "caret", "randomForest", "DMwR2", "text", 
              "xgboost", "recipes", "themis", "janitor", 
              "ParBayesianOptimization", "rBayesianOptimization")

install.packages(setdiff(packages, installed.packages()[,"Package"]))

library(tidyverse)
library(tidytext)
library(caret)
library(randomForest)
library(DMwR2)
library(text) # For embeddings, optional
library(xgboost)
library(tm)
library(recipes)
library(themis)
library(janitor)
library(ParBayesianOptimization)
library(rBayesianOptimization)

getwd()

#setwd("Endo_Review")





# Presence/Absence --------------------------------------------------------





# Step 1: Label Data and Prepare Text. mmake sure to update with every nerw version of training ds
# Load and clean data
labeled_abstracts <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(label != "Other" & label != "") %>%
  mutate(id = row_number())

# Remove rows with missing DOI and authors for "Presence" label
rows_to_remove <- labeled_abstracts %>%
  filter(is.na(doi) | doi %in% c("", "<NA>", "NA"),
         label == "Presence",
         is.na(authors) | authors == "") %>%
  slice_sample(n = 90)

labeled_abstracts <- labeled_abstracts %>%
  anti_join(rows_to_remove, by = "id")


labeled_abstracts %>%
  group_by(label)%>%
  summarize(n = n())



# Process "Other" abstracts
other_abstracts <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  mutate(predicted_label = NA,
         early_access_date = as.character(early_access_date)) %>%
  select(-c(x, predicted_label, id)) %>%
  filter(label == "Other")

# Metadata columns
target <- "label"
predictor <- "abstract"
metadata_columns <- setdiff(names(labeled_abstracts), c(target, predictor)) %>%
  .[! . %in% c("publication_type", "group_authors", "part_number", "web_of_science_index")]

labeled_abstracts <- labeled_abstracts %>%
  select(c(target, predictor, metadata_columns))

# Tokenize and create Document-Term Matrix (DTM)
dtm <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "ngrams", n = 3) %>%
  anti_join(stop_words) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)

dtm_matrix <- as.matrix(dtm)

# Train-test split
set.seed(123)
train_index <- createDataPartition(labeled_abstracts$label, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

# Match DTM rows with train/test data
train_dtm_matrix <- dtm_matrix[rownames(dtm_matrix) %in% train_data$id, , drop = FALSE]
test_dtm_matrix <- dtm_matrix[rownames(dtm_matrix) %in% test_data$id, , drop = FALSE]

# Ensure consistent factor levels
train_data$label <- factor(train_data$label)
test_data$label <- factor(test_data$label, levels = levels(train_data$label))

# Convert DTM matrices to dataframes and bind labels
train_dtm_df <- as.data.frame(train_dtm_matrix) %>%
  bind_cols(select(train_data, label))
test_dtm_df <- as.data.frame(test_dtm_matrix) %>%
  bind_cols(select(test_data, label))

# # Train Random Forest model
# rf_model <- train(
#   label ~ ., 
#   data = train_dtm_df, 
#   method = "rf"
# )
# 
# # Save the model
# save(rf_model, file = "rf_model_no_Other9.RData")
# Uncomment the above two code lines if you want to rerun the model. For now, load the saved model.
#load("rf_model_no_Other7_balanced.RData") #7 is best. others are trash?
#predictions_rf <- predict(rf_model, newdata = test_dtm_df)
#confusionMatrix(predictions_rf, test_dtm_df$label)


## Same thing but gradient boosting


# Step 1: Prepare the training data by removing the label column and converting to numeric
train_labels <- as.numeric(factor(train_data$label)) - 1
test_labels <- as.numeric(factor(test_data$label)) - 1

# Step 4: Prepare DMatrix for XGBoost
xgb_train <- xgb.DMatrix(data = train_dtm_matrix, label = train_labels)
xgb_test <- xgb.DMatrix(data = test_dtm_matrix, label = test_labels)


bounds <- list(
  eta = c(0.001, 0.3),
  max_depth = c(3L, 15L),
  subsample = c(0.3, 1),
  colsample_bytree = c(0.3, 1),
  min_child_weight = c(1, 10),
  gamma = c(0, 10),
  nrounds = c(100, 1000) # Add rounds as an optimization parameter
)

set.seed(123) # For reproducibility

optimize_xgb <- function(eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample, nrounds) {
  params <- list(
    objective = "multi:softmax",
    num_class = length(unique(train_labels)), 
    eval_metric = "mlogloss",
    eta = eta,
    max_depth = as.integer(max_depth),
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample = subsample
  )
  
  cv_results <- xgb.cv(
    params = params,
    data = xgb_train,
    nrounds = nrounds,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = FALSE
  )
  
  best_accuracy <- max(1 - cv_results$evaluation_log$test_mlogloss_mean)  
  return(list(Score = best_accuracy, Pred = 0))
}

#Don't run the below locally
# opt_results <- bayesOpt(
#   FUN = optimize_xgb,
#   bounds = bounds,
#   initPoints = 10,     # Number of random initial points
#   iters.n = 30,        # Number of optimization iterations
#   acq = "ei",          # Acquisition function: Expected Improvement
#   verbose = 2
# )


#Ran until here 2/12/25

 ss <- opt_results$scoreSummary

best_params <- ss[which.min(ss$Score), ]
max_score <- ss[which.max(ss$Score), ]

final_params <- list(
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(levels(train_data$label)),
  eta = best_params$eta,
  max_depth = as.integer(best_params$max_depth),
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree
)

#save(final_params, file = "xgb_params_bayes.R")


#Run until here

load("xgb_params_bayes.R")

xgb_model <- xgb.train(
  params = final_params,
  data = xgb_train,
  nrounds = 300,
  nthread = parallel::detectCores(), # Use all available cores
  tree_method = "hist", # Faster training for large datasets
  verbosity = 1
)

xgb.save(xgb_model, "xgb_model_2.model")

xgb_model <- xgb.load("xgb_model.model")

#Problem here...

# Step 7: Predict and evaluate
predictions <- predict(xgb_model, xgb_test)
predicted_classes <- max.col(matrix(predictions, nrow = length(test_labels), byrow = TRUE)) - 1

original_class_labels <- levels(factor(train_data$label))

# Convert numeric predictions back to original class names
predicted_labels <- factor(original_class_labels[predicted_classes + 1], levels = original_class_labels)
true_labels <- factor(original_class_labels[test_labels + 1], levels = original_class_labels)

# Generate the confusion matrix with original labels
confusionMatrix(predicted_labels, true_labels)

#at 80%. trigrams is worse right now




#Test different models and delete the worst ones.



# Whole dataset -----------------------------------------------------------




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


# Exclude empty strings and NA values from labeled_abstracts$doi
filtered_dois <- labeled_abstracts$doi[!is.na(labeled_abstracts$doi) & labeled_abstracts$doi != ""]

# Remove matching rows from full_abstracts
full_abstracts <- full_abstracts[!full_abstracts$doi %in% filtered_dois, ]
full_abstracts <- full_abstracts[!full_abstracts$doi %in% other_abstracts$doi, ]

# Step 8: Prepare Full Dataset for Prediction
full_abstracts$id <- 1:nrow(full_abstracts)

dtm <- full_abstracts %>%
  unnest_tokens(word, abstract, token = "ngrams", n = 3) %>%
  anti_join(stop_words) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)


dtm_matrix <- as.matrix(dtm)


trained_vocab <- colnames(train_dtm_matrix)  # Replace with your actual training matrix

# Identify missing words in dtm_matrix
missing_words <- setdiff(trained_vocab, colnames(dtm_matrix))

extra_words <- setdiff(colnames(dtm_matrix), trained_vocab)

# Create a zero matrix for missing words
if (length(missing_words) > 0) {
  zero_matrix <- matrix(0, nrow = nrow(dtm_matrix), ncol = length(missing_words),
                        dimnames = list(rownames(dtm_matrix), missing_words))
  
  # Combine existing matrix with zero matrix
  dtm_matrix <- cbind(dtm_matrix, zero_matrix)
}

# Ensure columns are in the same order as train_vocab
dtm_matrix <- dtm_matrix[, trained_vocab, drop = FALSE]

dtm_matrix <- dtm_matrix[full_abstracts$id, , drop = FALSE]


# Predict labels for the full dataset using the trained model
full_predictions <- predict(xgb_model, newdata = dtm_matrix)
#Full predictions seems fine. More rows bc more items.
dim(dtm_matrix)
length(full_predictions)

#Double check that we want byrow to equal False here
predicted_classes <- max.col(matrix(full_predictions, ncol = 4, byrow = F)) - 1


original_class_labels <- levels(factor(train_data$label))

# Convert numeric predictions back to original class names
predicted_labels <- factor(original_class_labels[predicted_classes + 1], levels = original_class_labels)

full_abstracts$predicted_label <- predicted_labels


# Save the results with predictions and metadata
write.csv(full_abstracts, "full_predictions_with_metadata.csv", row.names = FALSE)

# Optional: View summary of results. Would have to look through 600 abtracts here.
full_abstracts %>%
  group_by(predicted_label) %>%
  summarize(n = n())


#When make final output, remember to include training dataset.

# Checking on labels ------------------------------------------------------


#Careful with running the below code. Adding labels to random samples within abstract subsets
labeled_abstracts %>%
  filter(doi != "")%>%
  group_by(doi, authors)%>%
  filter(n()>1)
#Good, no duplicates


# Perform anti_join based on multiple columns, then filter and slice
subsample <- full_abstracts %>%
  filter(predicted_label == "Absence")%>%
  relocate(c(predicted_label))%>%
  slice_sample(n = 44)%>%
  mutate(volume = as.integer(volume))


write.csv(subsample, "subsample.csv")

