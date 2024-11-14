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

# Step 1: Label Data and Prepare Text
labeled_abstracts <- read.csv("Training_labeled_abs.csv") %>%
  clean_names()%>%
  mutate(predicted_label = NA,
         early_access_date = as.character(early_access_date))%>%
  select(!any_of(c("x", "predicted_label", "id")))%>%
  filter(label != "Other")

# Select relevant columns
target <- "label"
predictor <- "abstract"

# Dynamically select metadata columns (exclude target and predictor)
metadata_columns <- setdiff(names(labeled_abstracts), c(target, predictor))

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
 #rf_model <- train(label ~ ., data = train_dtm_df, method = "rf")
 #save(rf_model, file = "rf_model_no_Other2.RData")

# Uncomment the above two code lines if you want to rerun the model. For now, load the saved model.
load("rf_model_no_Other2.RData") #model with Other in it

# Step 6: Evaluate the Model on Test Data
test_dtm_df <- as.data.frame(test_dtm_matrix)
test_data$label <- factor(test_data$label, levels = train_levels)  # Relevel test labels

# Make predictions
predictions <- predict(rf_model, newdata = test_dtm_df)

predictions <- factor(predictions, levels = train_levels)

# Evaluate performance
confusionMatrix(predictions, test_data$label)

#86% accurate with Other in it
#90% accurate with Other not in it. 
#up to 93%!!!

#For now, use model without Other in it.




# Step 8: Predict on the Full Dataset
full_abstracts <- read.csv("wos-11-9-23_1000.csv") %>%
  clean_names() %>%
  select(c(predictor, all_of(metadata_columns)))%>%
  anti_join(labeled_abstracts, by = "doi")


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

# View summary of results
full_abstracts %>%
  group_by(predicted_label) %>%
  summarize(n = n())

#check absences and both



#write.csv(test_summary, "labels_results_with_metadata.csv", row.names = FALSE)


#Pull full_abstracts that do not have matching dois in labeled_abstrats

# Checking on labels ------------------------------------------------------

#Careful with running the below code. Adding labels to random samples within abstract subsets


# Optionally filter by a specific predicted label
subsample <- full_abstracts %>%
  anti_join(labeled_abstracts, by = "doi") %>%  # Remove matching DOI rows
  filter(predicted_label == "Absence")%>%
  slice_sample(n = 10)


#Present: 1, 2, 3, 7, 8
#Other: 6
#Review: 5, 4

subsample$id



fixed_other <- full_abstracts %>%
  filter(id %in% subsample$id[c(6)]) %>%
  mutate(label = "Other") %>%
  relocate(label) %>%
  relocate(id, .before = last_col())

fixed_both <- full_abstracts %>%
  filter(id %in% c(480, 506, 618)) %>%  
  mutate(label = "Both") %>%  # Add a new label column with "Both"
  relocate(label) %>%  # Relocate label to the first position
  relocate(id, .before = last_col())  # Move 'id' to the second-to-last position

fixed_present <- full_abstracts %>%
  filter(id %in% subsample$id[c(1, 2, 3, 7, 8)]) %>%
  mutate(label = "Presence") %>%
  relocate(label) %>%
  relocate(id, .before = last_col())

fixed_review <- full_abstracts %>%
  filter(id %in% subsample$id[c(4, 5)]) %>%
  mutate(label = "Review")%>%
  relocate(label) %>%
  relocate(id, .before = last_col())


test <- labeled_abstracts %>%
  mutate(predicted_label = NA)

test <- rbind(test, fixed_present, fixed_review, fixed_other)%>%
  distinct()

#Remove any duplicates by doi
View(test %>%
  filter(!is.na(doi))%>%
  group_by(doi)%>%
  filter(n()>1))

write.csv(test, "Training_labeled_abs.csv")

#Check that no duplicates are in here. 

#Pull leaf info too.


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
  sample_n(size = 500)

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
extract_plant_species <- function(text, abstract_id, predicted_label) {
  # Tokenize text and create 2-word ngrams (potential genus-species combinations)
  tokens <- tokens(text, remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower()  # Convert to lowercase
  
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
                             ~extract_plant_species(.x, .y, labeled_abstracts$predicted_label[labeled_abstracts$id == .y]))

# View the result
print(plant_species_df)

# Save the result as a CSV
write.csv(plant_species_df, "plant_species_results.csv", row.names = FALSE)
