# Quick Start: Pipeline Testing Framework
# B. Bock
# One-click setup and testing of the extraction pipeline

cat("🚀 QUICK START: Pipeline Testing Framework\n")
cat("===========================================\n\n")

# Step 1: Create test subsets
cat("Step 1: Creating test subsets...\n")
source("scripts/04_analysis/create_test_subset.R")

# Create small subsets for quick testing
create_test_subset(
  sample_sizes = c(100, 500),
  sampling_method = "random",
  verbose = TRUE
)

cat("\n✅ Test subsets created!\n\n")

# Step 2: Run quick pipeline test
cat("Step 2: Running pipeline tests...\n")
source("scripts/04_analysis/test_pipeline_workflow.R")

# Run quick test on smallest subset (tests all components including analysis)
run_quick_test(subset_size = 100)

cat("\n✅ Quick test completed!\n\n")

# Step 3: Show next steps
cat("🎯 NEXT STEPS:\n")
cat("=============\n")
cat("1. 📊 Review results in 'test_results/' folder\n")
cat("2. 🧪 Run full test: run_full_test(c(100, 500))\n")
cat("3. 🔬 Test individual components as needed\n")
cat("4. 📈 Compare results across subset sizes\n")
cat("5. ✅ Run on full dataset when ready\n\n")

cat("📋 USEFUL COMMANDS:\n")
cat("==================\n")

cat("# Run full test on multiple subsets\n")
cat("run_full_test(subset_sizes = c(100, 500, 1000))\n\n")

cat("# Test specific components only\n")
cat("test_pipeline_workflow(\n")
cat("  subset_sizes = c(200),\n")
cat("  test_components = c('species', 'geography')\n")
cat(")\n\n")

cat("# Run main pipeline on subset\n")
cat("source('scripts/04_analysis/run_extraction_pipeline.R')\n")
cat("run_extraction_pipeline(\n")
cat("  input_file = 'test_data/test_subset_random_500.csv',\n")
cat("  force_rerun = TRUE\n")
cat(")\n\n")

cat("# Run on full dataset (when ready)\n")
cat("run_extraction_pipeline()\n\n")

cat("📁 OUTPUT LOCATIONS:\n")
cat("===================\n")
cat("• test_data/          - Test subsets\n")
cat("• test_results/       - Test results and reports\n")
cat("• results/           - Final pipeline results\n\n")

cat("🎉 Ready to test your pipeline safely!\n")
cat("   Start with the small subsets, then scale up.\n")
