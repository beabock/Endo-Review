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
rf_model <- train(label ~ ., data = train_dtm_df, method = "rf")

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

full_abstracts %>%
  group_by(predicted_label)%>%
  summarize(n=n())
#not bad. check out the negatives. 

full_abstracts %>%
  filter(predicted_label == "Both")







# Trying to extract plant names -------------------------------------------

library(udpipe)
library(rgbif)
library(quanteda)

# Example text that contains potential plant names
text <- c("Acer rubrum and Quercus alba are common species in North America. Pinus sylvestris is also widely distributed.")

# Step 1: Tokenize the text more carefully, keeping two-word species names together
tokens <- tokens(text, remove_punct = TRUE) %>%
  tokens_tolower()

# Step 2: Extract potential two-word species names (ngrams) and ensure correct capitalization
plant_names <- tokens %>%
  tokens_ngrams(n = 2) %>%
  as.list()

# Convert tokens to strings with correct capitalization (Genus uppercase, species lowercase)
correct_capitalization <- function(name) {
  words <- unlist(strsplit(name, " "))
  if (length(words) == 2) {
    words[1] <- toupper(substring(words[1], 1, 1))  # Capitalize first letter of genus
    words[2] <- tolower(words[2])  # Ensure species is lowercase
    return(paste(words, collapse = " "))
  }
  return(name)
}

plant_candidates <- sapply(unlist(plant_names), correct_capitalization)

# Step 3: Query GBIF for species names
get_valid_species <- function(name) {
  res <- name_backbone(name = name)
  
  # Check if a species is returned and handle the case where species is missing
  if (!is.null(res$scientificName) && !is.na(res$scientificName)) {
    return(res$scientificName)
  } else {
    return(NA)
  }
}

# Apply the function to each candidate name
valid_species <- sapply(plant_candidates, get_valid_species)

# Filter out NAs to get valid plant names
valid_species <- valid_species[!is.na(valid_species)]

# Print the valid species names found
print(valid_species)
