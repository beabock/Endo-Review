library(here)
# Test the load_gbif_backbone function specifically
setwd(here())

source("scripts/04_analysis/visualization/visualize_taxa_results.R")

message("Testing load_gbif_backbone function...")

tryCatch({
  data <- load_gbif_backbone("models/species.rds")
  message("Function completed successfully!")
  message("Data type: ", class(data))
  message("Dimensions: ", dim(data))
  message("Column names: ", paste(colnames(data), collapse = ", "))

  if (nrow(data) > 0) {
    message("First few rows:")
    print(head(data, 3))

    # Check for key columns
    key_cols <- c("kingdom", "phylum", "family", "genus", "canonicalName", "taxonRank")
    found_cols <- intersect(key_cols, colnames(data))
    message("Found key columns: ", paste(found_cols, collapse = ", "))

    # Check data quality
    if ("kingdom" %in% colnames(data)) {
      kingdoms <- unique(data$kingdom)
      message("Kingdoms present: ", paste(kingdoms, collapse = ", "))
    }
  }

}, error = function(e) {
  message("Error in load_gbif_backbone: ", e$message)
  message("Error call: ", deparse(e$call))
})
