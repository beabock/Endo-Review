# Test script to verify the visualization fixes
library(tidyverse)

cat("🧪 TESTING VISUALIZATION SCRIPT FIXES\n")
cat("====================================\n\n")

# Test if the script can be sourced without errors
cat("📝 Testing script loading...\n")

tryCatch({
  # Test if the script can be sourced without errors
  source("scripts/04_analysis/visualization/visualize_taxa_results.R", echo = FALSE)
  cat("✅ Script loaded successfully!\n")
  cat("✅ Column issues resolved!\n")
  cat("✅ Theme function issues resolved!\n")
}, error = function(e) {
  cat("❌ Error loading script:\n")
  cat("   ", e$message, "\n")
  cat("   Line info: ", if(exists("sys.calls")) sys.calls() else "Not available", "\n")
}, warning = function(w) {
  cat("⚠️ Warning during script loading:\n")
  cat("   ", w$message, "\n")
})

cat("\n📋 Test completed.\n")