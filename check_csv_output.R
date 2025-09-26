# Quick script to check the CSV output
library(readr)

cat("=== Checking CSV Output ===\n\n")

csv_file <- "results/test_enhanced_mycorrhizal_output.csv"
if (file.exists(csv_file)) {
  data <- read_csv(csv_file, show_col_types = FALSE)
  cat("CSV file columns: ", paste(names(data), collapse = ", "), "\n")
  cat("Number of columns: ", ncol(data), "\n")
  cat("Number of rows: ", nrow(data), "\n")

  # Show first row
  cat("\nFirst row sample:\n")
  print(data[1, ])

  # Check if mycorrhizal columns exist
  mycorrhizal_cols <- c("is_mycorrhizal", "funguild_guild", "confidence_ranking", "trophic_mode", "growth_form", "trait_confidence", "is_mycorrhizal_only")
  found_cols <- mycorrhizal_cols[mycorrhizal_cols %in% names(data)]

  cat("\nMycorrhizal columns found: ", paste(found_cols, collapse = ", "), "\n")
  if (length(found_cols) == length(mycorrhizal_cols)) {
    cat("✅ All mycorrhizal columns are present\n")
  } else {
    missing_cols <- setdiff(mycorrhizal_cols, names(data))
    cat("❌ Missing columns: ", paste(missing_cols, collapse = ", "), "\n")
  }
} else {
  cat("❌ CSV file not found: ", csv_file, "\n")
}