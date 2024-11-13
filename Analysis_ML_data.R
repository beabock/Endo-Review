#BB
#11/13/24
#Writing analysis code for the output of the machine learning dataset.
# Load necessary libraries

# Load the required libraries
library(dplyr)
library(rgbif)

# Set your working directory
setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

# Load your dataset
ds <- read.csv("plant_species_results.csv")

# Define a function to query GBIF and retrieve taxonomic info for plants
get_gbif_taxonomy <- function(kingdom = "Plantae", rank = "species", limit = 1000) {
  # Query GBIF occurrence data for plants within a certain rank
  occ_data <- occ_search(
    kingdom = kingdom,
    limit = limit
  )
  
  # Extract the taxonomic column(s) based on the rank
  taxonomy <- occ_data$taxonKey %>%
    as.data.frame() %>%
    select(phylum, class, order, family, genus, species) # Select desired taxonomic columns
  
  return(taxonomy)
}

# Define a function to find missing taxa for each rank
get_missing_taxa <- function(category, rank) {
  # Query GBIF for taxonomic data at the specified rank
  gbif_taxonomy <- get_gbif_taxonomy(rank = rank)
  
  # Extract the unique values for the specified category
  gbif_values <- unique(gbif_taxonomy[[category]])
  
  # Extract the unique values from the dataset (ds)
  dataset_values <- unique(ds[[category]])
  
  # Identify missing values by comparing GBIF values with the dataset values
  missing_values <- setdiff(gbif_values, dataset_values)
  
  return(missing_values)
}

# Define categories and ranks to check
categories <- c("phylum", "class", "order", "family", "genus", "species")
ranks <- c("PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES")

# Create a list to store missing values for each category
missing_values_list <- list()

# Loop through each category and rank to find missing values
for(i in 1:length(categories)) {
  category <- categories[i]
  rank <- ranks[i]
  missing_values_list[[category]] <- get_missing_taxa(category, rank)
}

# Print the missing values for each category
for(i in 1:length(categories)) {
  category <- categories[i]
  cat("\nMissing values for", category, ":\n")
  print(missing_values_list[[category]])
  cat("\n")
}

# Optional: Save the missing values to a CSV file
missing_df <- data.frame(
  category = rep(categories, each = length(missing_values_list[[1]])),
  missing_value = unlist(missing_values_list)
)

write.csv(missing_df, "missing_values_from_GBIF.csv", row.names = FALSE)
