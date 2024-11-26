#B. Bock
#11/11/24
#ML Approach

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(caret)
library(randomForest)
library(DMwR2)
library(text) # For embeddings, optional
library(xgboost)
library(recipes)
library(themis)
library(janitor)


setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")





# Presence/Absence --------------------------------------------------------





# Step 1: Label Data and Prepare Text. mmake sure to update with every nerw version of training ds

labeled_abstracts <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(label != "Other" & label != "") %>%
  mutate(
    id = row_number()  # Domain-specific feature
  )


other_abstracts <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names()%>%
  mutate(predicted_label = NA,
         early_access_date = as.character(early_access_date))%>%
  select(!any_of(c("x", "predicted_label", "id")))%>%
  filter(label == "Other")

#Make sure to add Others back in at end.

labeled_abstracts %>%
  group_by(label)%>%
  summarize(n = n())

#20 more absent, 20 more both, 20 more review

# Select relevant columns
target <- "label"
predictor <- "abstract"

metadata_columns <- setdiff(names(labeled_abstracts), c(target, predictor)) %>%
  .[!.%in% c("publication_type", "group_authors", "part_number", "web_of_science_index")]

# Keep the target, predictor, and metadata columns
labeled_abstracts <- labeled_abstracts %>%
  select(c(target, predictor, metadata_columns))

# Now, labeled_abstracts contains the target, predictor, and metadata columns


# Step 2: Tokenize Text and Create Document-Term Matrix (DTM)
# Tokenize and remove stop words for the entire dataset (before splitting)
text_tokens <- labeled_abstracts %>%
  unnest_tokens(word, abstract, token = "ngrams", n = 1) %>%
  anti_join(stop_words)

# Create Document-Term Matrix (DTM) for the entire dataset
dtm <- text_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)

dtm_matrix <- as.matrix(dtm)

# Step 3: Train-Test Split (80-20)
set.seed(123)
train_index <- createDataPartition(labeled_abstracts$label, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

# Create DTM for training and testing data (matching id)
train_dtm_matrix <- dtm_matrix[train_data$id, , drop = FALSE]
test_dtm_matrix <- dtm_matrix[test_data$id, , drop = FALSE]

# Step 4: Ensure Factor Levels Match for Labels in Both Training and Test Data
train_data$label <- as.factor(train_data$label)
test_data$label <- as.factor(test_data$label)

train_levels <- levels(train_data$label)

test_data$label <- factor(test_data$label, levels = train_levels)


#test_data <- test_data %>%
#  mutate(abstract = iconv(abstract, from = "latin1", to = "UTF-8", sub = ""))

# Now calculate abstract_length
#test_data <- test_data %>%
#  mutate(abstract_length = nchar(abstract))

# train_dtm_df <- as.data.frame(train_dtm_matrix) %>%
#   bind_cols(train_data %>% select(label, abstract_length, absence_indicator))
# test_dtm_df <- as.data.frame(test_dtm_matrix) %>%
#   bind_cols(test_data %>% select(label, abstract_length, absence_indicator))

train_dtm_df <- as.data.frame(train_dtm_matrix) %>%
  bind_cols(train_data %>% select(label))
test_dtm_df <- as.data.frame(test_dtm_matrix) %>%
  bind_cols(test_data %>% select(label))


# Step 5: Train the Model (Random Forest)
# Convert train DTM into a data frame and add labels
train_dtm_df$label <- as.factor(train_dtm_df$label)
test_dtm_df$label <- as.factor(test_dtm_df$label)


# Train Random Forest model.

rf_model <- train(
  label ~ ., 
  data = train_dtm_df, 
  method = "rf",
  trControl = trainControl(method = "cv", number = 2),
  tuneGrid = expand.grid(mtry = c(5)),  # Only include mtry here
  weights = ifelse(train_dtm_df$label == "Absence", 10, 1), # Set weights
  ntree = 100 # Specify ntree here
)
 #save(rf_model, file = "rf_model_no_Other2.RData")


# Uncomment the above two code lines if you want to rerun the model. For now, load the saved model.


#  smote_recipe <- recipe(label ~ ., data = train_dtm_df) %>%
#    step_smote(label, over_ratio = 1) %>%
#    prep()
#  
#  balanced_train_data <- bake(smote_recipe, new_data = NULL)
# dim(balanced_train_data)
#  
# print(object.size(balanced_train_data), units = "auto")
#  
# rf_model <- train(
# label ~ ., data = balanced_train_data, method = "rf",
# trControl = trainControl(method = "cv", number = 2),
# tuneGrid = expand.grid(.mtry = c(5), ntree=100),
# weights = ifelse(balanced_train_data$label == "Absence", 10, 1) #Might need to change these weights. Could increase the 3 number
# )
# save(rf_model, file = "rf_model_no_Other8_balanced.RData")

#missing_features <- setdiff(names(balanced_train_data), names(test_dtm_df))
#Comeback to thiss

load("rf_model_no_Other6.RData") #7 is best. others are trash?


predictions_rf <- predict(rf_model, newdata = test_dtm_df)
confusionMatrix(predictions_rf, test_dtm_df$label)

#2 gets 17%, 3 does too. 80% at 7


# Step 6: Gradient Boosting with XGBoost

# Step 1: Prepare the training data by removing the label column and converting to numeric
# Step 1: Prepare the training data by removing the label column and converting to numeric
xgb_train_data <- balanced_train_data[, -ncol(balanced_train_data)]  # Remove label column
xgb_train_data <- as.data.frame(apply(xgb_train_data, 2, function(x) as.numeric(as.character(x))))  # Convert all features to numeric and keep as data frame

# Step 2: Prepare the test data by removing the label column and converting to numeric
test_features <- test_dtm_df[, -ncol(test_dtm_df)]  # Remove label column
test_features <- as.data.frame(apply(test_features, 2, function(x) as.numeric(as.character(x))))  # Convert all features to numeric and keep as data frame

# Step 3: Align the columns in the training and test datasets
# Make sure both datasets have the same features
train_cols <- colnames(xgb_train_data)
test_cols <- colnames(test_features)

# Add missing columns to the test data with 0 values if necessary
missing_cols <- setdiff(train_cols, test_cols)
if(length(missing_cols) > 0) {
  test_features[missing_cols] <- 0
}

# Add missing columns to the train data with 0 values if necessary
missing_cols <- setdiff(test_cols, train_cols)
if(length(missing_cols) > 0) {
  xgb_train_data[missing_cols] <- 0
}

# Step 4: Ensure column order matches in both training and test datasets
xgb_train_data <- xgb_train_data[, train_cols]
test_features <- test_features[, train_cols]

# Step 5: Convert both datasets into DMatrix objects
xgb_train <- xgb.DMatrix(data = as.matrix(xgb_train_data), label = as.numeric(balanced_train_data$label) - 1)
xgb_test <- xgb.DMatrix(data = as.matrix(test_features), label = as.numeric(test_dtm_df$label) - 1)

# Step 2: Set the parameters for XGBoost
params <- list(
  objective = "multi:softmax",       # Multi-class classification
  num_class = length(unique(balanced_train_data$label)),  # Number of classes
  eta = 0.1,                         # Learning rate
  max_depth = 6,                     # Max depth of trees
  subsample = 0.8,                   # Fraction of samples for each tree
  colsample_bytree = 0.8,            # Fraction of features for each tree
  min_child_weight = 5,              # Minimum sum of instance weight for a child
  gamma = 0.1                        # Regularization parameter
)


xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 1000,  # Number of boosting rounds
  watchlist = list(train = xgb_train),  # Keep track of training progress
  verbose = 1,  # Show progress during training
  early_stopping_rounds = 10
)


#Need to tune this better.


save(xgb_model, file = "xgb_bigrams_1.RData")

# Convert the predictions to a factor and ensure the levels match the actual labels
original_levels <- levels(factor(train_dtm_df$label))


predictions_xgb <- predict(xgb_model, xgb_test)

# Map the numeric predictions back to the original class labels
predictions_xgb_labels <- factor(predictions_xgb, levels = 0:(length(original_levels) - 1), labels = original_levels)

# Now perform confusion matrix with consistent labels
confusionMatrix(predictions_xgb_labels, factor(test_dtm_df$label))



predicted_labels <- as.factor(predictions_xgb + 1)  # Convert predictions back to factor labels
confusion_matrix <- table(predicted_labels, test_dtm_df$label)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))




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
  "conference_authors" = "conference_title", # 'conference_authors' to 'conference_title'
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
  "subject_categories" = "wo_s_categories", # 'subject_categories' to 'wo_s_categories'
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


# Rename columns in full_abstracts using the mapping
colnames(full_abstracts) <- colname_mapping[colnames(full_abstracts)]

full_abstracts <- full_abstracts %>%
  select(c(predictor, all_of(metadata_columns)))%>%
  mutate(volume = as.integer(volume))


# Exclude empty strings and NA values from labeled_abstracts$doi
filtered_dois <- labeled_abstracts$doi[!is.na(labeled_abstracts$doi) & labeled_abstracts$doi != ""]

# Remove matching rows from full_abstracts
full_abstracts <- full_abstracts[!full_abstracts$doi %in% filtered_dois, ]
full_abstracts <- full_abstracts[!full_abstracts$doi %in% other_abstracts$doi, ]

# Step 8: Prepare Full Dataset for Prediction
full_abstracts$id <- 1:nrow(full_abstracts)

# Tokenize and clean text (same as before)
full_text_tokens <- full_abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words)

# Create DTM for full dataset
full_dtm <- full_text_tokens %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# Convert full DTM to a matrix
full_dtm_matrix <- as.matrix(full_dtm)

# Step 9: Align Full DTM with Training DTM Vocabulary
train_vocab <- colnames(train_dtm_matrix)

full_dtm_matrix_aligned <- matrix(0, nrow = nrow(full_dtm_matrix), ncol = length(train_vocab))
colnames(full_dtm_matrix_aligned) <- train_vocab

matching_words <- intersect(colnames(full_dtm_matrix), train_vocab)
full_dtm_matrix_aligned[, matching_words] <- full_dtm_matrix[, matching_words]

# Step 10: Predict on Full Dataset
full_dtm_df <- as.data.frame(full_dtm_matrix_aligned)

# Predict labels for the full dataset using the trained model
full_predictions <- predict(rf_model, newdata = full_dtm_df)

# Step 11: Relevel Predictions Based on Training Labels
full_predictions <- factor(full_predictions, levels = train_levels)

# Step 12: Save Predictions with Metadata
full_abstracts$predicted_label <- full_predictions

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
  slice_sample(n = 44)%>%
  mutate(volume = as.integer(volume))


write.csv(subsample, "subsample.csv")

subsample$abstract
#Both: 17
#Present: 30, 29, 28, 27, 26, 24, 23, 22, 20, 18, 16, 14
#Review: 
#Absent:
#Other: 25, 21, 19, 15


fixed_other <- full_abstracts %>%
  filter(id %in% subsample$id[c(25, 21, 19, 15)]) %>%
  mutate(label = "Other") %>%
  relocate(label) %>%
  relocate(id, .before = last_col())

fixed_both <- full_abstracts %>%
  filter(id %in% subsample$id[c(17)]) %>%  
  mutate(label = "Both") %>%  # Add a new label column with "Both"
  relocate(label) %>%  # Relocate label to the first position
  relocate(id, .before = last_col())  # Move 'id' to the second-to-last position

fixed_present <- full_abstracts %>%
  filter(id %in% subsample$id[c(30, 29, 28, 27, 26, 24, 23, 22, 20, 18, 16, 14)]) %>%
  mutate(label = "Presence") %>%
  relocate(label) %>%
  relocate(id, .before = last_col())

fixed_review <- full_abstracts %>%
  filter(id %in% subsample$id[c()]) %>%
  mutate(label = "Review")%>%
  relocate(label) %>%
  relocate(id, .before = last_col())


test <- labeled_abstracts %>%
  mutate(predicted_label = NA)

# Combine data ensuring there are no duplicates in the DOI column
test <- bind_rows(test, other_abstracts)
                  #fixed_present, fixed_other, fixed_both, fixed_review) 


# If there are any duplicates that should be manually resolved,
# you can review the following:
test %>%
       filter(!is.na(doi)) %>%
       filter(doi != "")%>%
       group_by(doi) %>%
       filter(n() > 1)


filepath <- "Training_labeled_abs_4.csv" 

i <- as.numeric(gsub(".*_(\\d+)\\.csv$", "\\1", filepath))

i <- i + 1

newname <- paste0("Training_labeled_abs_", i, ".csv")

write.csv(test, newname)

#Nice. fixed the probs.




# Trying to extract plant names -------------------------------------------

# Load necessary libraries

library(dplyr)
library(quanteda)
library(rgbif)
library(purrr)
library(janitor)
library(tidyverse)
library(furrr)


set.seed(123)

# Load and sample 5 abstracts randomly
labeled_abstracts <- read.csv("full_predictions.csv") %>%
  clean_names() %>%
  sample_n(size = 100)

plan(multisession)

# Function to correct capitalization (Genus uppercase, species lowercase)
correct_capitalization <- function(name) {
  words <- unlist(strsplit(name, " "))
  if (length(words) == 2) {
    words[1] <- paste0(toupper(substring(words[1], 1, 1)), tolower(substring(words[1], 2)))
    words[2] <- tolower(words[2])
    return(paste(words, collapse = " "))
  }
  return(name)
}

# Function to query GBIF and get additional fields (batch processing)
batch_validate_species <- function(names) {
  res <- name_backbone_checklist(names)  # Batch query
  
  # Ensure required columns exist before selecting
  required_columns <- c("canonicalName", "rank", "confidence", "matchType", "kingdom", "phylum", 
                        "class", "order", "family", "genus", "species", 
                        "kingdomKey", "phylumKey", "classKey", "orderKey", 
                        "familyKey", "genusKey", "speciesKey")
  
  valid_data <- res %>%
    filter(status == "ACCEPTED" & (kingdom == "Plantae" | kingdom == "Fungi") & rank != "KINGDOM") %>%
    select(any_of(required_columns)) %>%
    mutate(across(.cols = matches("Key$"), .fns = as.character))
  
  return(valid_data)
}

#Look into other options for premade dictionaries, in case I'm missing anything.
plant_parts_keywords <- c(
  "fruit", "fruits", "root", "roots", "rhizoid", "rhizoids", "leaf", "leaves", 
  "twig", "twigs", "branch", "branches", "bark", "stems", "stem", "flowers", 
  "flower", "shoot", "shoots", "seed", "seeds", "node", "nodes", "leaflet", 
  "leaflets", "pistil", "pistils", "anther", "anthers", "carpel", "carpels", 
  "sepal", "sepals", "petal", "petals", "stigma", "stigmas", "style", "styles", 
  "ovary", "ovaries", "calyx", "calyces", "corolla", "corollas", "peduncle", 
  "peduncles", "rachis", "rachises", "inflorescence", "inflorescences", "trunk", 
  "trunks", "cork", "buds", "bud", "pollen", "cones", "cone", "tuber", "tubers", 
  "bulb", "bulbs", "corm", "corms", "cladode", "cladodes", "vascular bundle", 
  "vascular bundles", "xylem", "phloem", "cortex", "cortices", "endosperm", 
  "cotyledon", "cotyledons", "hypocotyl", "hypocotyls", "epicotyl", "epicotyls", 
  "flowering stem", "flowering stems", "internode", "internodes", "leaf vein", 
  "leaf veins", "leaf blade", "leaf blades", "palmate", "palmatations", "needle", 
  "needles", "fascicle", "fascicles", "cuticle", "cuticles", "stomata", "stoma", 
  "vascular cambium", "vascular cambiums", "petiole", "petioles", "axil", "axils", 
  "phyllode", "phyllodes", "perianth", "perianths", "rachilla", "rachillas", 
  "pedicel", "pedicels", "lateral root", "lateral roots", "taproot", "taproots", 
  "root cap", "root caps", "root hair", "root hairs", "lignin", "pith", "pericycle", 
  "pericycles", "parenchyma", "colleter", "colleters", "scutellum", "scutella", 
  "coleoptile", "coleoptiles", "sporophyte", "sporophytes", "gametophyte", "gametophytes"
)

# Function to extract plant info
extract_plant_info <- function(text, abstract_id, predicted_label, valid_species_lookup) {
  # Tokenize text
  tokens <- tokens(text, remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower()
  
  # Extract plant parts
  plant_parts_found <- unlist(tokens) %>%
    .[. %in% plant_parts_keywords] %>%
    unique()
  
  # Create binary indicator for plant parts
  plant_parts_indicator <- setNames(as.integer(plant_parts_keywords %in% plant_parts_found), plant_parts_keywords)
  
  # Generate ngrams and correct capitalization
  plant_candidates <- tokens %>%
    tokens_ngrams(n = 2) %>%
    unlist() %>%
    gsub("_", " ", .) %>%
    sapply(correct_capitalization)
  
  # Validate genus-species combinations using precomputed lookup
  valid_species <- valid_species_lookup %>%
    filter(canonicalName %in% unique(plant_candidates))
  
  # Create the final output
  if (nrow(valid_species) > 0) {
    valid_species <- valid_species %>%
      mutate(id = abstract_id, predicted_label = predicted_label)
    final_df <- cbind(valid_species, as.data.frame(t(plant_parts_indicator)))
  } else {
    plant_parts_df <- as.data.frame(t(plant_parts_indicator))
    plant_parts_df <- cbind(
      data.frame(
        canonicalName = NA, rank = NA, confidence = NA, matchType = NA, kingdom = NA,
        phylum = NA, class = NA, order = NA, family = NA, genus = NA, species = NA,
        kingdomKey = NA, phylumKey = NA, classKey = NA, orderKey = NA, familyKey = NA,
        genusKey = NA, speciesKey = NA, id = abstract_id, predicted_label = predicted_label,
        stringsAsFactors = FALSE
      ),
      plant_parts_df
    )
    final_df <- plant_parts_df
  }
  
  return(final_df)
}

# Precompute ngrams and species validation
presence_abstracts <- labeled_abstracts %>%
  filter(predicted_label == "Presence") %>%
  mutate(ngrams = map(abstract, ~ {
    tokens(.x, remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_tolower() %>%
      tokens_ngrams(n = 2) %>%
      unlist() %>%
      sapply(function(name) gsub("_", " ", name))
  }))

# Collect all unique candidate names for validation
all_candidates <- presence_abstracts %>%
  pull(ngrams) %>%
  unlist() %>%
  unique() %>%
  sapply(correct_capitalization)

# Batch validate candidates
valid_species_lookup <- batch_validate_species(all_candidates)

# Apply the extraction function in parallel
plant_species_df <- future_pmap_dfr(
  list(
    presence_abstracts$abstract,
    presence_abstracts$id,
    presence_abstracts$predicted_label
  ),
  ~extract_plant_info(..1, ..2, ..3, valid_species_lookup)
)

# View the result
print(plant_species_df)

# Save the result as a CSV
write.csv(plant_species_df, "plant_info_results.csv", row.names = FALSE)

