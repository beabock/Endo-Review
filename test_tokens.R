# Test script for specific synonym case
library(tidyverse)

# Test the token creation for the specific case
text <- "This study examines the endophytic fungi associated with Iridorchis clarkei in the Pacific Northwest."

# Create tokens like in process_taxonomic_matches
tokens_vec <- unlist(strsplit(gsub("[[:punct:][:digit:]]", " ", tolower(text)), "\\s+"))
tokens_vec <- tokens_vec[tokens_vec != ""]

# Create bigrams
bigrams <- character(0)
if (length(tokens_vec) > 1) {
  for (i in 1:(length(tokens_vec) - 1)) {
    bigrams <- c(bigrams, paste(tokens_vec[i], tokens_vec[i + 1]))
  }
}

# Combine tokens and bigrams
all_tokens <- c(tokens_vec, bigrams)

cat("Text:", text, "\n")
cat("Tokens:", paste(tokens_vec, collapse = ", "), "\n")
cat("Bigrams:", paste(bigrams, collapse = ", "), "\n")

# Check if our target names are in the tokens
target_names <- c("iridorchis clarkei", "oberonia clarkei")
for (name in target_names) {
  cat("Is '", name, "' in all_tokens?", name %in% all_tokens, "\n")
}

# Also test the extract_candidate_names function
source("scripts/archive/optimized_taxa_detection.R")
candidates <- extract_candidate_names(text)
cat("Extracted candidates:", paste(candidates, collapse = ", "), "\n")
cat("Is 'iridorchis clarkei' in candidates?", "iridorchis clarkei" %in% candidates, "\n")
