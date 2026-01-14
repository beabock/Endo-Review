library(here)
# Test the load_gbif_backbone function specifically
setwd(here())

# =============================================================================
# PERFORMANCE SCORING FRAMEWORK
# =============================================================================

# Global counters for test results
total_tests <- 0
passed_tests <- 0
failed_tests <- 0

# Helper function for scoring
test_result <- function(passed, test_name, details = "") {
  total_tests <<- total_tests + 1
  if (passed) {
    passed_tests <<- passed_tests + 1
    cat("✅ PASS:", test_name, "\n")
  } else {
    failed_tests <<- failed_tests + 1
    cat("❌ FAIL:", test_name, "\n")
  }
  if (details != "") {
    cat("   ", details, "\n")
  }
}

source("scripts/04_analysis/visualization/visualize_taxa_results.R")

message("Testing load_gbif_backbone function...")

load_success <- FALSE
data_loaded <- NULL

tryCatch({
  data <- load_gbif_backbone("models/species.rds")
  message("Function completed successfully!")
  message("Data type: ", class(data))
  message("Dimensions: ", dim(data))
  message("Column names: ", paste(colnames(data), collapse = ", "))

  load_success <- TRUE
  data_loaded <- data

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

# Score data loading
test_result(load_success, "GBIF backbone loading", 
           ifelse(load_success, paste0("Loaded ", nrow(data_loaded), " taxa successfully"),
                  "Failed to load GBIF backbone data"))

# Score data structure if loaded successfully
if (load_success && !is.null(data_loaded)) {
  # Check for required columns
  required_cols <- c("kingdom", "phylum", "family", "genus", "canonicalName")
  found_required <- intersect(required_cols, colnames(data_loaded))
  columns_present <- length(found_required) == length(required_cols)
  test_result(columns_present, "Required columns present", 
             paste0(length(found_required), "/", length(required_cols), " required columns found"))
  
  # Check data has content
  has_data <- nrow(data_loaded) > 0 && ncol(data_loaded) > 0
  test_result(has_data, "Data content validation", 
             ifelse(has_data, paste0("Data has ", nrow(data_loaded), " rows and ", ncol(data_loaded), " columns"),
                    "Data is empty or malformed"))
}

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! GBIF backbone loading is working correctly.\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}
