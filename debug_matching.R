# Simple test script to debug the issue
library(tidyverse)

# Load the species data
species_df <- readRDS("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review/models/species.rds")

# Source the functions
source("scripts/archive/optimized_taxa_detection.R")

# Create lookup tables
lookup_tables <- create_lookup_tables(species_df)

# Test the specific case
text <- "This study examines the endophytic fungi associated with Iridorchis clarkei in the Pacific Northwest."
candidates <- extract_candidate_names(text)
cat("Extracted candidates:", paste(candidates, collapse = ", "), "\n")

# Test batch_validate_names
valid_species <- batch_validate_names(candidates, lookup_tables)
cat("Valid species found:", nrow(valid_species), "\n")
if(nrow(valid_species) > 0) {
  print(valid_species)
}

# Test the tokens
tokens_vec <- unlist(strsplit(gsub("[[:punct:][:digit:]]", " ", tolower(text)), "\\s+"))
tokens_vec <- tokens_vec[tokens_vec != ""]

bigrams <- character(0)
if (length(tokens_vec) > 1) {
  for (i in 1:(length(tokens_vec) - 1)) {
    bigrams <- c(bigrams, paste(tokens_vec[i], tokens_vec[i + 1]))
  }
}

all_tokens <- c(tokens_vec, bigrams)
cat("All tokens:", paste(all_tokens, collapse = ", "), "\n")

# Check if the species matches would be found
if(nrow(valid_species) > 0) {
  cat("\nChecking matches:\n")
  for(i in 1:nrow(valid_species)) {
    resolved_match <- tolower(valid_species$resolved_name[i]) %in% all_tokens
    user_match <- tolower(valid_species$user_supplied_name[i]) %in% all_tokens
    cat("Row", i, ":\n")
    cat("  resolved_name:", valid_species$resolved_name[i], "-> in tokens:", resolved_match, "\n")
    cat("  user_supplied_name:", valid_species$user_supplied_name[i], "-> in tokens:", user_match, "\n")
    cat("  Overall match:", resolved_match | user_match, "\n")
  }
}
