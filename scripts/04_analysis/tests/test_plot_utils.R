# Test script for plot_utils.R

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

source("scripts/utils/plot_utils.R")

cat("Testing get_endo_colors function:\n")
colors_2 <- get_endo_colors(2)
colors_4 <- get_endo_colors(4)
colors_8 <- get_endo_colors(8)
colors_10 <- get_endo_colors(10)

cat("2 colors:", paste(colors_2, collapse = ", "), "\n")
cat("4 colors:", paste(colors_4, collapse = ", "), "\n")
cat("8 colors:", paste(colors_8, collapse = ", "), "\n")
cat("10 colors:", paste(colors_10, collapse = ", "), "\n")

# Score get_endo_colors function
get_endo_colors_works <- length(colors_2) == 2 && length(colors_4) == 4 && 
                         length(colors_8) == 8 && length(colors_10) == 10 &&
                         all(sapply(c(colors_2, colors_4, colors_8, colors_10), function(x) grepl("^#[0-9A-Fa-f]{6}$", x)))
test_result(get_endo_colors_works, "get_endo_colors function", 
           ifelse(get_endo_colors_works, "Returns correct number of valid hex colors",
                  "Color generation failed or returned invalid colors"))

# Test that endo_colors still work
cat("endo_colors presence_absence:", paste(endo_colors$presence_absence, collapse = ", "), "\n")
cat("endo_colors relevant_irrelevant:", paste(endo_colors$relevant_irrelevant, collapse = ", "), "\n")
cat("endo_colors found_not_found:", paste(endo_colors$found_not_found, collapse = ", "), "\n")

# Score endo_colors object
endo_colors_works <- exists("endo_colors") && 
                    is.list(endo_colors) && 
                    length(endo_colors) >= 3 &&
                    all(c("presence_absence", "relevant_irrelevant", "found_not_found") %in% names(endo_colors))
test_result(endo_colors_works, "endo_colors object", 
           ifelse(endo_colors_works, "endo_colors object exists with required color schemes",
                  "endo_colors object missing or incomplete"))

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! Plot utilities are working correctly.\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}