# =============================================================================
# Test Memory-Optimized Species Mycorrhizal Detection
# =============================================================================
#
# Purpose: Test the memory-optimized version of species detection with smaller datasets
#
# Description: This script allows testing the memory optimizations on smaller subsets
# before running the full dataset. This helps identify and resolve memory issues
# incrementally.
#
# Author: B. Bock
# Date: 2024-09-26
#
# =============================================================================

library(tidyverse)
library(tictoc)

# Source the memory-optimized script
source("scripts/04_analysis/components/01_species_mycorrhizal.R")

cat("=== MEMORY-OPTIMIZED SPECIES DETECTION TEST ===\n")
cat("Testing memory optimizations with smaller datasets first\n\n")

# Load consolidated dataset
abstracts_file <- "results/consolidated_dataset.csv"
if (!file.exists(abstracts_file)) {
  stop("❌ Consolidated dataset not found. Run the consolidation script first.")
}

cat("📂 Loading consolidated dataset...\n")
full_data <- read_csv(abstracts_file, show_col_types = FALSE)
cat("   Loaded", nrow(full_data), "abstracts\n\n")

# Test with progressively larger datasets
test_sizes <- c(10, 50, 100)

for (test_size in test_sizes) {
  if (test_size > nrow(full_data)) {
    cat("⚠️ Test size", test_size, "exceeds dataset size, skipping\n")
    next
  }

  cat("🧪 Testing with", test_size, "abstracts...\n")

  # Sample random subset
  set.seed(42)  # For reproducible results
  test_data <- full_data %>%
    slice_sample(n = test_size)

  # Memory check before test
  cat("   📊 Memory before test: ")
  mem_before <- monitor_memory(threshold_gb = 8, context = "Before test")
  cat(round(mem_before$memory_used_gb, 2), "GB\n")

  # Time the test
  tic(paste("Test with", test_size, "abstracts"))

  tryCatch({
    # Run memory-optimized species detection
    test_results <- extract_species_mycorrhizal_data(
      test_data,
      output_file = paste0("results/test_species_mycorrhizal_", test_size, ".csv"),
      batch_size = test_size,  # Use full test size as batch for testing
      verbose = TRUE,
      force_rerun = TRUE  # Force fresh run for testing
    )

    toc()

    # Memory check after test
    cat("   📊 Memory after test: ")
    mem_after <- monitor_memory(threshold_gb = 8, context = "After test")
    cat(round(mem_after$memory_used_gb, 2), "GB\n")

    # Report results
    cat("   ✅ Test completed successfully!\n")
    cat("      Results:", nrow(test_results), "species detection records\n")
    cat("      Memory increase:", round(mem_after$memory_used_gb - mem_before$memory_used_gb, 2), "GB\n\n")

    # Clean up test file if desired
    # unlink(paste0("results/test_species_mycorrhizal_", test_size, ".csv"))

  }, error = function(e) {
    toc()
    cat("   ❌ Test failed with error:", conditionMessage(e), "\n")
    cat("   Error type:", class(e)[1], "\n")

    # Print stack trace if available
    if (inherits(e, "error")) {
      cat("   Stack trace:\n")
      print(sys.calls())
    }
    cat("\n")

    # Emergency memory cleanup
    tryCatch({
      aggressive_gc(verbose = TRUE)
    }, error = function(gc_error) {
      cat("   ⚠️ Memory cleanup also failed:", conditionMessage(gc_error), "\n")
    })
  })
}

# Final memory cleanup
cat("🧹 Final memory cleanup after all tests...\n")
aggressive_gc(verbose = TRUE)

cat("=== MEMORY OPTIMIZATION TESTING COMPLETE ===\n")
cat("If all tests passed, you can proceed with the full dataset.\n")
cat("Monitor memory usage and adjust batch sizes if needed.\n")