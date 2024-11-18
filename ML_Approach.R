#B. Bock
#11/11/24
#ML Approach

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(caret)
library(readxl)
library(janitor)
library(visdat)


setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")





# Presence/Absence --------------------------------------------------------



library(rgbif)
library(dplyr)
library(tidyr)
library(tidytext)
library(caret)
library(randomForest)
library(readxl)
library(openxlsx)


# Step 1: Label Data and Prepare Text. mmake sure to update with every nerw version of training ds
labeled_abstracts <- read.csv("Training_labeled_abs_3.csv") %>%
  clean_names()%>%
  mutate(predicted_label = NA,
         early_access_date = as.character(early_access_date))%>%
  select(!any_of(c("x", "predicted_label", "id")))%>%
  filter(label != "Other")


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

# Assign ID
labeled_abstracts$id <- 1:nrow(labeled_abstracts)

# Step 2: Tokenize Text and Create Document-Term Matrix (DTM)
# Tokenize and remove stop words for the entire dataset (before splitting)
text_tokens <- labeled_abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words)

# Create Document-Term Matrix (DTM) for the entire dataset
dtm <- text_tokens %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

dtm_matrix <- as.matrix(dtm)

# Step 3: Train-Test Split (80% training, 20% testing)
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

# Step 5: Train the Model (Random Forest)
# Convert train DTM into a data frame and add labels
train_dtm_df <- as.data.frame(train_dtm_matrix)
train_dtm_df$label <- train_data$label  # Add labels to the DTM for training

# Train Random Forest model.

 rf_model <- train(label ~ ., data = train_dtm_df, method = "rf")
 #save(rf_model, file = "rf_model_no_Other3.RData")

# rf_model <- train(label ~ ., data = train_dtm_df, method = "rf")
# save(rf_model, file = "rf_model.RData")

# Uncomment the above two code lines if you want to rerun the model. For now, load the saved model.
load("rf_model_no_Other2.RData")

# Step 6: Evaluate the Model on Test Data
test_dtm_df <- as.data.frame(test_dtm_matrix)
test_data$label <- factor(test_data$label, levels = train_levels)  # Relevel test labels

# Make predictions
predictions <- predict(rf_model, newdata = test_dtm_df)

predictions <- factor(predictions, levels = train_levels)

# Evaluate performance
confusionMatrix(predictions, test_data$label)
#It's getting Presence and Review well. Not getting both very well, nor is it getting absence well.

#86% accurate with Other in it
#90% accurate with Other not in it. 
#up to 93%!!! (no Other cat)
#Down to 85%. retrain model again,...


#For now, use model without Other in it.


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

# Optional: View summary of results
test_summary <- full_abstracts %>%
  group_by(predicted_label) %>%
  summarize(n = n())


#check absences and both



#write.csv(test_summary, "labels_results_with_metadata.csv", row.names = FALSE)


#Pull full_abstracts that do not have matching dois in labeled_abstrats

# Checking on labels ------------------------------------------------------

#Come back to this:: removed a bunch of abstracts from the labeled dataset. we don't want that.



#Careful with running the below code. Adding labels to random samples within abstract subsets
labeled_abstracts %>%
  filter(doi != "")%>%
  group_by(doi, authors)%>%
  filter(n()>1)
#Good, no duplicates


# Perform anti_join based on multiple columns, then filter and slice
subsample <- full_abstracts %>%
  filter(predicted_label == "Absence")%>%
  slice_sample(n = 30)%>%
  mutate(volume = as.integer(volume))

write.csv(subsample, "subsample.csv")

subsample$abstract
#Both: 26, 12, 11
#Present: 30, 28, 25, 24, 23, 22, 21, 20, 18, 17, 14, 13, 10
#Review: 16
#Absent:
#Other: 29, 27, 19, 15


fixed_other <- full_abstracts %>%
  filter(id %in% subsample$id[c(29, 27, 19, 15)]) %>%
  mutate(label = "Other") %>%
  relocate(label) %>%
  relocate(id, .before = last_col())

fixed_both <- full_abstracts %>%
  filter(id %in% subsample$id[c(26, 12, 11)]) %>%  
  mutate(label = "Both") %>%  # Add a new label column with "Both"
  relocate(label) %>%  # Relocate label to the first position
  relocate(id, .before = last_col())  # Move 'id' to the second-to-last position

fixed_present <- full_abstracts %>%
  filter(id %in% subsample$id[c(30, 28, 25, 24, 23, 22, 21, 20, 18, 17, 14, 13, 10)]) %>%
  mutate(label = "Presence") %>%
  relocate(label) %>%
  relocate(id, .before = last_col())

fixed_review <- full_abstracts %>%
  filter(id %in% subsample$id[c(16)]) %>%
  mutate(label = "Review")%>%
  relocate(label) %>%
  relocate(id, .before = last_col())


test <- labeled_abstracts %>%
  mutate(predicted_label = NA)

# Combine data ensuring there are no duplicates in the DOI column
test <- bind_rows(test, fixed_present, fixed_other, fixed_both, fixed_review) 


# If there are any duplicates that should be manually resolved,
# you can review the following:
test %>%
       filter(!is.na(doi)) %>%
       filter(doi != "")%>%
       group_by(doi) %>%
       filter(n() > 1)


filepath <- "Training_labeled_abs_2.csv" 

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

set.seed(123)

# Load and sample 5 abstracts randomly
labeled_abstracts <- read.csv("full_predictions.csv") %>%
  clean_names() %>%
  sample_n(size = 5)

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
  
  # Ensure columns exist before attempting to select
  available_columns <- colnames(res)
  required_columns <- c("canonicalName", "rank", "confidence", "matchType", "kingdom", "phylum", 
                        "class", "order", "family", "genus", "species", 
                        "kingdomKey", "phylumKey", "classKey", "orderKey", 
                        "familyKey", "genusKey", "speciesKey")
  
  # Select only the columns that exist in the response
  valid_data <- res %>%
    filter(status == "ACCEPTED" & (kingdom == "Plantae" | kingdom == "Fungi") & rank != "KINGDOM") %>%
    select(any_of(required_columns))  # Select only the columns that exist
  
  # Only mutate columns if they exist in the available columns
  valid_data <- valid_data %>%
    mutate(across(.cols = intersect(names(valid_data), c("kingdomKey", "phylumKey", "classKey", 
                                                         "orderKey", "familyKey", "genusKey", "speciesKey")), 
                  .fns = ~as.character(.)))
  
  return(valid_data)
}

# Function to extract valid plant species from abstracts
extract_plant_info <- function(text, abstract_id, predicted_label) {
  # Keywords for plant parts
  plant_parts_keywords <- c("fruit", "root", "rhizoid", "leaf", "twig", "branch", "bark", "stem", "flower", 
                            "shoot", "seed", "node", "leaflet", "pistil", "anther", "carpel", "sepal", "petal", 
                            "stigma", "style", "ovary", "calyx", "corolla", "peduncle", "rachis", "inflorescence", 
                            "trunk", "cork", "bud", "pollen", "cone", "spore", "tuber", "bulb", "corm", "cladode", 
                            "vascular bundle", "xylem", "phloem", "cortex", "endosperm", "cotyledon", "hypocotyl", 
                            "epicotyl", "flowering stem", "internode", "leaf vein", "leaf blade", "palmate", "needle", 
                            "fascicle", "cuticle", "stomata", "vascular cambium", "petiole", "axil", "phyllode", 
                            "perianth", "rachilla", "pedicel", "lateral root", "taproot", "root cap", "root hair", 
                            "mycorrhiza", "lignin", "pith", "pericycle", "parenchyma", "colleter", "scutellum", "coleoptile",
                            "sporophyte", "gametophyte"
  )
  
  # Tokenize text and convert to lowercase
  tokens <- tokens(text, remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower()  # Convert to lowercase
  
  # Extract plant parts by checking if the tokens match any of the keywords
  plant_parts_found <- tokens %>%
    unlist() %>%
    .[ . %in% plant_parts_keywords ] %>%
    unique()
  
  # Tokenize text and create 2-word ngrams (potential genus-species combinations)
  bigrams <- tokens %>%
    tokens_ngrams(n = 2) %>%
    as.list()
  
  # Convert ngrams to readable format (replace underscores with spaces)
  plant_candidates <- sapply(unlist(bigrams), function(name) {
    gsub("_", " ", name)  # Replace underscores with spaces
  })
  
  # Correct capitalization
  plant_candidates <- sapply(plant_candidates, correct_capitalization)
  
  # Validate genus-species combinations (batch processing)
  valid_species <- batch_validate_species(unique(plant_candidates))  # Query GBIF once
  
  # Ensure id and predicted_label are character for consistency
  abstract_id <- as.character(abstract_id)
  predicted_label <- as.character(predicted_label)
  
  # Create a data frame with species, id, predicted label, and GBIF information
  if (nrow(valid_species) > 0) {
    valid_species <- valid_species %>%
      mutate(id = abstract_id, predicted_label = predicted_label)  # Add abstract_id and predicted_label to valid_species
    return(valid_species)
  } else {
    return(data.frame(canonicalName = character(), rank = character(), confidence = numeric(), 
                      matchType = character(), kingdom = character(), phylum = character(),
                      class = character(), order = character(), family = character(),
                      genus = character(), species = character(), 
                      kingdomKey = character(), phylumKey = character(), classKey = character(),
                      orderKey = character(), familyKey = character(), genusKey = character(),
                      speciesKey = character(), id = character(), predicted_label = character(), 
                      stringsAsFactors = FALSE))
  }
}

# Apply the function to all abstracts in the dataset
plant_species_df <- map2_dfr(labeled_abstracts$abstract, 
                             labeled_abstracts$id, 
                             ~extract_plant_info(.x, .y, labeled_abstracts$predicted_label[labeled_abstracts$id == .y]))


# View the result
print(plant_species_df)

# Save the result as a CSV
write.csv(plant_info_df, "plant_info_results.csv", row.names = FALSE)


#Maybe switch plant parts to a separate section. 