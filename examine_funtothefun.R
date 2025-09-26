# Script to examine the funtothefun.csv dataset
library(readr)
library(dplyr)

cat("=== Examining funtothefun.csv ===\n\n")

# Read the file
file_path <- "C:/Users/beabo/OneDrive/Documents/NAU/Sap_Sym/datasets/funtothefun.csv"

if (file.exists(file_path)) {
  cat("1. Loading dataset...\n")
  fun_data <- read_csv(file_path, show_col_types = FALSE)

  cat("   Dimensions: ", dim(fun_data), "\n")
  cat("   Column names: ", paste(names(fun_data), collapse = ", "), "\n")

  # Look for mycorrhizal-related columns
  mycorrhizal_cols <- grep("mycorrh", names(fun_data), ignore.case = TRUE)
  cat("\n2. Mycorrhizal-related columns:\n")
  if (length(mycorrhizal_cols) > 0) {
    for (col in names(fun_data)[mycorrhizal_cols]) {
      cat("   - ", col, "\n")
      unique_vals <- unique(fun_data[[col]])
      cat("     Unique values (first 10): ", paste(head(unique_vals, 10), collapse = ", "), "\n")
      if (length(unique_vals) > 10) {
        cat("     ... and ", length(unique_vals) - 10, " more\n")
      }
    }
  } else {
    cat("   No mycorrhizal columns found with 'mycorrh' pattern\n")
  }

  # Look for lifestyle/functional trait columns
  lifestyle_cols <- grep("lifestyle|guild|trait|function", names(fun_data), ignore.case = TRUE)
  cat("\n3. Lifestyle/trait-related columns:\n")
  if (length(lifestyle_cols) > 0) {
    for (col in names(fun_data)[lifestyle_cols]) {
      cat("   - ", col, "\n")
      unique_vals <- unique(fun_data[[col]])
      cat("     Unique values (first 5): ", paste(head(unique_vals, 5), collapse = ", "), "\n")
    }
  } else {
    cat("   No lifestyle columns found\n")
  }

  # Check taxonomy columns
  taxonomy_cols <- c("species", "genus", "family", "order", "class", "phylum", "kingdom")
  available_tax_cols <- taxonomy_cols[taxonomy_cols %in% names(fun_data)]

  cat("\n4. Available taxonomy columns:\n")
  for (col in available_tax_cols) {
    non_na_count <- sum(!is.na(fun_data[[col]]))
    cat("   - ", col, " (", non_na_count, " non-NA values)\n")
  }

  # Sample some mycorrhizal taxa
  cat("\n5. Sample taxa with mycorrhizal information:\n")

  # Try different column patterns for mycorrhizal identification
  mycorrhizal_sample <- fun_data

  # Look for rows that might contain mycorrhizal information
  if (length(mycorrhizal_cols) > 0) {
    # Use the first mycorrhizal column found
    myc_col <- names(fun_data)[mycorrhizal_cols[1]]
    mycorrhizal_sample <- fun_data %>%
      filter(!is.na(.data[[myc_col]])) %>%
      select(species, genus, family, .data[[myc_col]]) %>%
      head(10)
  } else if (length(lifestyle_cols) > 0) {
    # Try lifestyle columns
    lifestyle_col <- names(fun_data)[lifestyle_cols[1]]
    mycorrhizal_sample <- fun_data %>%
      filter(!is.na(.data[[lifestyle_col]])) %>%
      filter(grepl("mycorrh", .data[[lifestyle_col]], ignore.case = TRUE)) %>%
      select(species, genus, family, .data[[lifestyle_col]]) %>%
      head(10)
  }

  if (nrow(mycorrhizal_sample) > 0) {
    print(mycorrhizal_sample)
  } else {
    cat("   No clear mycorrhizal taxa found in sample\n")
    # Show first few rows instead
    print(head(fun_data, 5))
  }

  cat("\n6. Summary for mycorrhizal classification:\n")
  cat("   - Dataset has ", nrow(fun_data), " rows\n")
  cat("   - Can be used for taxonomic matching with genus/family/species\n")
  cat("   - Mycorrhizal information available in: ", paste(names(fun_data)[mycorrhizal_cols], collapse = ", "))

} else {
  cat("❌ File not found: ", file_path, "\n")
}

cat("\n=== Dataset examination complete ===\n")