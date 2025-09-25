# Test script for plot_utils.R
source("scripts/utils/plot_utils.R")

cat("Testing get_endo_colors function:\n")
cat("2 colors:", paste(get_endo_colors(2), collapse = ", "), "\n")
cat("4 colors:", paste(get_endo_colors(4), collapse = ", "), "\n")
cat("8 colors:", paste(get_endo_colors(8), collapse = ", "), "\n")
cat("10 colors:", paste(get_endo_colors(10), collapse = ", "), "\n")

# Test that endo_colors still work
cat("endo_colors presence_absence:", paste(endo_colors$presence_absence, collapse = ", "), "\n")
cat("endo_colors relevant_irrelevant:", paste(endo_colors$relevant_irrelevant, collapse = ", "), "\n")
cat("endo_colors found_not_found:", paste(endo_colors$found_not_found, collapse = ", "), "\n")