# Script to examine species detection results structure (fixed)
library(readr)
library(dplyr)

cat("=== Examining Species Detection Results Structure ===\n\n")

# Check if file exists
species_file <- "results/species_detection_results.csv"
if (file.exists(species_file)) {
  cat("1. Loading species detection results...\n")
  species_data <- read_csv(species_file, show_col_types = FALSE)

  cat("   Dimensions: ", dim(species_data), "\n")
  cat("   Column names: ", paste(names(species_data), collapse = ", "), "\n")

  # Show first few rows
  cat("\n2. Sample data:\n")
  print(head(species_data, 5))

  # Look for fungal taxa
  fungal_data <- species_data %>%
    filter(kingdom == "Fungi") %>%
    select(id, resolved_name, kingdom, phylum, family, genus, match_type) %>%
    head(10)

  cat("\n3. Sample fungal taxa:\n")
  print(fungal_data)

  # Summary statistics
  cat("\n4. Summary statistics:\n")
  cat("   - Total records: ", nrow(species_data), "\n")
  cat("   - Unique abstracts: ", n_distinct(species_data$id), "\n")
  cat("   - Unique fungal taxa: ", n_distinct(species_data$resolved_name[species_data$kingdom == "Fungi"], na.rm = TRUE), "\n")
  cat("   - Kingdoms present: ", paste(unique(species_data$kingdom), collapse = ", "), "\n")

} else {
  cat("❌ Species detection results not found at ", species_file, "\n")
  cat("   Run 01_extract_species.R first to generate the data\n")
}

cat("\n=== Structure examination complete ===\n")