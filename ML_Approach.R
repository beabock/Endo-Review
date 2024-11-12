#B. Bock
#11/11/24
#ML Approach

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(caret)


setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo Review Project")

# Step 1: Label Data Manually (This should be done before training the model)
# Let's assume we have a column 'label' with 'Presence', 'Absence', or 'Unclear'
# Label 100-500 abstracts to use as training data.

labeled_abstracts <- read.csv("wos-11-9-23_100_labeled_abstracts.csv")

# Step 2: Preprocess the text data
# Tokenize and remove stop words
text_tokens <- labeled_abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words)

# Step 3: Create Document-Term Matrix (DTM)
# We use a simple bag-of-words approach here
dtm <- text_tokens %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# Step 4: Train-Test Split
# 80% training, 20% testing
set.seed(123)
train_index <- createDataPartition(labeled_abstracts$label, p = 0.8, list = FALSE)
train_data <- labeled_abstracts[train_index, ]
test_data <- labeled_abstracts[-train_index, ]

# Step 5: Train a model (e.g., Naive Bayes, Random Forest, or SVM)
# Example using Random Forest:
rf_model <- train(label ~ ., data = train_data, method = "rf")

# Step 6: Evaluate the model
predictions <- predict(rf_model, test_data)
confusionMatrix(predictions, test_data$label)

# Step 7: Predict on the Full Dataset
full_abstracts <- read.csv("abstracts.csv")
# Preprocess full dataset as done in Step 2, then use the model for prediction
# (Ensure same text preprocessing pipeline for new data)
full_tokens <- full_abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words)

full_dtm <- full_tokens %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# Predict presence/absence for all abstracts
full_predictions <- predict(rf_model, full_dtm)

# Step 8: Combine predictions with plant family data for analysis
full_abstracts <- full_abstracts %>%
  mutate(predicted_label = full_predictions)

# Analyze plant families
families_with_endophytes <- full_abstracts %>%
  filter(predicted_label == "Presence") %>%
  select(plant_family) %>%
  distinct()

families_without_endophytes <- full_abstracts %>%
  filter(predicted_label == "Absence") %>%
  select(plant_family) %>%
  distinct()

# Identify gaps in plant families
all_families <- full_abstracts %>% select(plant_family) %>% distinct()
families_with_gaps <- all_families %>%
  filter(!plant_family %in% families_with_endophytes$plant_family &
           !plant_family %in% families_without_endophytes$plant_family)

# Output the results
list(
  Present_Families = families_with_endophytes,
  Absent_Families = families_without_endophytes,
  Families_with_Gaps = families_with_gaps
)
