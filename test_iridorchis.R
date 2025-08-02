# Test just the synonym resolution to verify the fix
library(tidyverse)

# Load the species data
species_df <- readRDS("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review/models/species.rds")

# Source the fixed functions
source("scripts/archive/optimized_taxa_detection.R")

# Create lookup tables
lookup_tables <- create_lookup_tables(species_df)

# Test the specific case that was working in batch_validate_names
test_text <- "This study examines the endophytic fungi associated with Iridorchis clarkei in the Pacific Northwest."

cat("=== Testing Iridorchis clarkei case ===\n")
cat("Text:", test_text, "\n")

# Extract candidates
candidates <- extract_candidate_names(test_text)
cat("Candidates:", paste(candidates, collapse = ", "), "\n")

# Validate species
valid_species <- batch_validate_names(candidates, lookup_tables)
cat("Valid species found:", nrow(valid_species), "\n")
if(nrow(valid_species) > 0) {
  print(valid_species)
}

# Now test the full pipeline
plant_parts_keywords <- c("leaf", "leaves", "stem", "stems", "root", "roots")
result <- extract_plant_info(
  text = test_text,
  abstract_id = 1,
  predicted_label = "Presence", 
  lookup_tables = lookup_tables,
  plant_parts_keywords = plant_parts_keywords
)

cat("\nFull pipeline result:\n")
print(result %>% select(match_type, resolved_name, status))

# Let's also manually check the process_taxonomic_matches function
cat("\n=== Manual check of process_taxonomic_matches ===\n")
if(nrow(valid_species) > 0) {
  matches <- process_taxonomic_matches(valid_species, lookup_tables, test_text, 1, "Presence")
  cat("Number of match groups found:", length(matches), "\n")
  if(length(matches) > 0) {
    combined <- bind_rows(matches)
    print(combined %>% select(match_type, resolved_name, status))
  }
}
