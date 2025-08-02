# Debug script for synonym resolution duplicates
library(tidyverse)

# Load the species data
species_df <- readRDS("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review/models/species.rds")

# Check the raw data structure
cat("=== DEBUGGING SYNONYM RESOLUTION ===\n")
cat("Total species records:", nrow(species_df), "\n")

# Check how many records exist for veronicastrum lungtsuanense
veroni_matches <- species_df %>%
  filter(tolower(canonicalName) == "veronicastrum lungtsuanense")
cat("Records for 'veronicastrum lungtsuanense':", nrow(veroni_matches), "\n")
if(nrow(veroni_matches) > 0) {
  print(head(veroni_matches, 10))
}

# Check synonyms specifically
synonyms_raw <- species_df %>%
  filter(taxonomicStatus != "accepted" & !is.na(acceptedNameUsageID))
cat("\nTotal synonym records:", nrow(synonyms_raw), "\n")

veroni_synonyms <- synonyms_raw %>%
  filter(tolower(canonicalName) == "veronicastrum lungtsuanense")
cat("Synonym records for 'veronicastrum lungtsuanense':", nrow(veroni_synonyms), "\n")
if(nrow(veroni_synonyms) > 0) {
  print(head(veroni_synonyms, 10))
}

# Check accepted species
accepted_raw <- species_df %>%
  filter(taxonomicStatus == "accepted")
cat("\nTotal accepted species records:", nrow(accepted_raw), "\n")

# Create the synonym resolution step by step
cat("\n=== STEP BY STEP SYNONYM RESOLUTION ===\n")

# Step 1: Create synonyms table
synonyms <- species_df %>%
  filter(taxonomicStatus != "accepted" & !is.na(acceptedNameUsageID)) %>%
  select(taxonID, canonicalName, canonicalName_lower, acceptedNameUsageID)

cat("Step 1 - Synonyms table created, rows:", nrow(synonyms), "\n")

# Step 2: Create accepted species table  
accepted_species <- species_df %>%
  filter(taxonomicStatus == "accepted") %>%
  select(taxonID, canonicalName, canonicalName_lower, kingdom, phylum, family, genus)

cat("Step 2 - Accepted species table created, rows:", nrow(accepted_species), "\n")

# Step 3: Join synonyms with accepted species
joined <- synonyms %>%
  left_join(
    accepted_species %>% select(taxonID, canonicalName),
    by = c("acceptedNameUsageID" = "taxonID")
  )

cat("Step 3 - After join, rows:", nrow(joined), "\n")

# Check veronicastrum in joined data
veroni_joined <- joined %>%
  filter(tolower(canonicalName.x) == "veronicastrum lungtsuanense")
cat("Veronicastrum records after join:", nrow(veroni_joined), "\n")

# Step 4: Rename and filter
final_synonym_resolution <- joined %>%
  rename(acceptedName = canonicalName.y, synonymName = canonicalName.x) %>%
  filter(!is.na(acceptedName)) %>%
  select(synonymName, acceptedName, canonicalName_lower) %>%
  distinct()

cat("Step 4 - Final synonym resolution table, rows:", nrow(final_synonym_resolution), "\n")

# Check veronicastrum in final table
veroni_final <- final_synonym_resolution %>%
  filter(tolower(synonymName) == "veronicastrum lungtsuanense")
cat("Final veronicastrum records:", nrow(veroni_final), "\n")
if(nrow(veroni_final) > 0) {
  print(veroni_final)
}
