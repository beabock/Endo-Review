#BB
#11/13/24
#Writing analysis code for the output of the machine learning dataset.
# Load necessary libraries

# Load the required libraries
library(dplyr)
library(rgbif)

# Set your working directory
setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

# Load your dataset. Note that this is only a subset of the whole dataset. Will need to re-run this one the whole thing.
ds <- read.csv("plant_info_results.csv")



# Define a function to query GBIF and retrieve taxonomic info for plants
get_gbif_taxonomy <- function(kingdom_key = 6, limit = 1000) {
  # Query GBIF occurrence data for plants within a certain kingdom
  occ_data <- occ_search(
    kingdomKey = kingdom_key,  # This is the key for Plantae, you can adjust if needed
    limit = limit
  )
  
  # Extract the taxonomic columns (phylum, class, order, family, genus, species)
  taxonomy <- occ_data$data %>%
    select(phylum, class, order, family, genus, species)  # Select desired taxonomic columns
  
  return(taxonomy)
}

# Define a function to find missing taxa for each category (taxonomic rank)
get_missing_taxa <- function(category, ds) {
  # Query GBIF for taxonomic data at the specified kingdom level
  gbif_taxonomy <- get_gbif_taxonomy()
  
  # Extract the unique values for the specified category from GBIF
  gbif_values <- unique(gbif_taxonomy[[category]])
  
  # Extract the unique values from the dataset (ds)
  dataset_values <- unique(ds[[category]])
  
  # Identify missing values by comparing GBIF values with the dataset values
  missing_values <- setdiff(gbif_values, dataset_values)
  
  return(missing_values)
}

# Define categories to check
#categories <- c("phylum", "class", "order", "family", "genus", "species") #Limit of 1000.

categories <- c("phylum", "class", "order")

# Create a list to store missing values for each category
missing_values_list <- list()

# Loop through each category to find missing values
for(i in 1:length(categories)) {
  category <- categories[i]
  missing_values_list[[category]] <- get_missing_taxa(category, ds)
}

# Print the missing values for each category
for(i in 1:length(categories)) {
  category <- categories[i]
  cat("\nMissing values for", category, ":\n")
  print(missing_values_list[[category]])
  cat("\n")
}

# Loop through missing_values_list and write a CSV for each
for (category in names(missing_values_list)) {
  # Create the missing values data frame
  missing_df <- data.frame(
    category = rep(category, length(missing_values_list[[category]])),
    missing_value = missing_values_list[[category]]
  )
  
  # Write the data frame to a CSV file
  write.csv(missing_df, paste0("missing_", tolower(category), ".csv"), row.names = FALSE)
}

ds %>%
  filter(kingdom == "Fungi")%>%
  group_by(phylum)%>%
  summarize(n=n())

total_rows <- nrow(ds %>% filter(kingdom == "Fungi"))
ds %>%
  filter(kingdom == "Fungi")%>%
  group_by(genus)%>%
  summarize(n=n(), total = total_rows)%>%
  arrange(desc(n))

ds %>%
  filter(kingdom == "Fungi") %>%
  pivot_longer(cols = c(colnames(ds)[21:92]), names_to = "plant_part", values_to = "presence") %>%
  filter(!is.na(presence) & presence != 0) %>%  # Only count non-NA and non-zero values
  group_by(plant_part) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

#Cool!

