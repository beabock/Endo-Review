library(tidyverse)

# Load the files
enhanced <- read_csv('results/species_detection_results_mycorrhizal_enhanced.csv', show_col_types = FALSE)
consolidated <- read_csv('results/consolidated_dataset.csv', show_col_types = FALSE)

# Print column information
cat("Enhanced columns:\n")
print(colnames(enhanced))
cat("\nConsolidated columns:\n")
print(colnames(consolidated))
cat("\nOverlapping columns:\n")
print(intersect(colnames(enhanced), colnames(consolidated)))

cat("\nNumber of overlapping columns:", length(intersect(colnames(enhanced), colnames(consolidated))), "\n")