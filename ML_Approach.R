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



# Step 1: Label Data and Prepare Text
labeled_abstracts <- read_xlsx("Training_labeled_abs.xlsx") %>%
  clean_names()

# Select relevant columns
target <- "label"
predictor <- "abstract"
labeled_abstracts <- labeled_abstracts %>%
  select(c(target, predictor))

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
# Ensure that factor levels in train and test data match

train_data$label <- as.factor(train_data$label)
test_data$label <- as.factor(test_data$label)

train_levels <- levels(train_data$label)

test_data$label <- factor(test_data$label, levels = train_levels)

# Step 5: Train the Model (Random Forest)
# Convert train DTM into a data frame and add labels
train_dtm_df <- as.data.frame(train_dtm_matrix)
train_dtm_df$label <- train_data$label  # Add labels to the DTM for training

# Train Random Forest model. takes a long time so beware before running.
#rf_model <- train(label ~ ., data = train_dtm_df, method = "rf")
#save(rf_model, file = "rf_model.RData")

#Uncomment the above two code lines if I want to rerun the model. Takes foreeeeeeever so be careful. For testing for now, just reload the below model.

load("rf_model.RData")

# Step 6: Evaluate the Model on Test Data
# Convert test DTM into a data frame and add labels
test_dtm_df <- as.data.frame(test_dtm_matrix)

train_levels <- levels(train_data$label)  # Levels from the training data
test_data$label <- factor(test_data$label, levels = train_levels)  # Relevel test labels

# Make predictions
predictions <- predict(rf_model, newdata = test_dtm_df)

predictions <- factor(predictions, levels = train_levels)


# Evaluate performance
confusionMatrix(predictions, test_data$label)

#86% accuracy on 50/50 split. try higher split
#95% accurate on 80/20 split. nice!
#Could improve training ds by including more examples of reviews and of other.



# Step 8: Predict on the Full Dataset
full_abstracts <- read.csv("wos-11-9-23_1000.csv") %>%
  clean_names()%>%
  select(c(predictor))

# Step 8: Prepare Full Dataset for Prediction
# Create Document-Term Matrix (DTM) for full dataset
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
# Get the words (column names) from the training DTM
train_vocab <- colnames(train_dtm_matrix)

# Align full dataset DTM with the training DTM vocabulary
# Ensure the full DTM has the same columns (words) as the training DTM
full_dtm_matrix_aligned <- matrix(0, nrow = nrow(full_dtm_matrix), ncol = length(train_vocab))
colnames(full_dtm_matrix_aligned) <- train_vocab

# Match existing words in full DTM with the training DTM
matching_words <- intersect(colnames(full_dtm_matrix), train_vocab)
full_dtm_matrix_aligned[, matching_words] <- full_dtm_matrix[, matching_words]

# Step 10: Predict on Full Dataset
# Convert aligned full DTM matrix to a data frame
full_dtm_df <- as.data.frame(full_dtm_matrix_aligned)

# Predict labels for the full dataset using the trained model
full_predictions <- predict(rf_model, newdata = full_dtm_df)

# Step 11: Relevel Predictions Based on Training Labels
# Relevel full predictions to match training label levels
full_predictions <- factor(full_predictions, levels = train_levels)

# Step 12: Save Predictions
# Add predictions to the original full_abstracts dataset
full_abstracts$predicted_label <- full_predictions

# Save the results with predictions
write.csv(full_abstracts, "full_predictions.csv", row.names = FALSE)

test <- full_abstracts %>%
  group_by(predicted_label)%>%
  summarize(n=n())
#not bad. check out the negatives.
#Maybe print this out to show K + N.

write.csv(test, "labels_results.csv", row.names = FALSE)

full_abstracts %>%
  filter(predicted_label == "Both")







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
  sample_n(size = 100)

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
                        "key", "kingdomKey", "phylumKey", "classKey", "orderKey", 
                        "familyKey", "genusKey", "speciesKey")
  
  # Select only available columns
  valid_data <- res %>%
    filter(status == "ACCEPTED" & (kingdom == "Plantae" | kingdom == "Fungi") & rank != "KINGDOM") %>%
    select(any_of(required_columns))  # Select only the columns that exist
  
  
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
write.csv(plant_species_df, "plant_species_results_with_keys.csv", row.names = FALSE)
