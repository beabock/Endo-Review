# Quick Start: Pipeline Testing Framework
# B. Bock
# One-click setup and testing of the extraction pipeline

cat("🚀 QUICK START: Pipeline Testing Framework\n")
cat("DEBUG: Starting quick_start_testing.R at", Sys.time(), "\n")
cat("===========================================\n\n")
cat("🚀 QUICK START: Pipeline Testing Framework\n")
cat("===========================================\n\n")
# Step 1: Create test subsets
cat("Step 1: Creating test subsets...\n")
cat("DEBUG: About to source create_test_subset.R\n")
source("scripts/04_analysis/create_test_subset.R")
cat("DEBUG: Sourced create_test_subset.R, calling create_test_subset\n")

create_test_subset(
  sample_sizes = c(50, 100, 500),
  sampling_method = "random",
  verbose = TRUE
)

# Step 2: Run quick pipeline test (skipping species detection)
cat("Step 2: Running pipeline tests (skipping species detection)...\n")
cat("DEBUG: About to source test_pipeline_workflow.R\n")
source("scripts/04_analysis/tests/test_pipeline_workflow.R")
cat("DEBUG: Sourced test_pipeline_workflow.R, calling run_quick_test\n")

# Run quick test on smallest subset (tests all components except species)
run_quick_test(subset_size = 500, components = c("methods", "parts", "geography", "merge", "analysis"))

cat("DEBUG: run_quick_test completed\n")
cat("\n✅ Quick test completed!\n\n")
cat("DEBUG: create_test_subset completed\n")
cat("\n✅ Test subsets created!\n\n")

# Step 1: Create test subsets
cat("Step 1: Creating test subsets...\n")
source("scripts/04_analysis/create_test_subset.R")

# Create small subsets for quick testing
create_test_subset(
  sample_sizes = c(50, 100, 500),
  sampling_method = "random",
  verbose = TRUE
)

cat("\n✅ Test subsets created!\n\n")

# Step 2: Run quick pipeline test (skipping species detection)
cat("Step 2: Running pipeline tests (skipping species detection)...\n")
source("scripts/04_analysis/tests/test_pipeline_workflow.R")

# Run quick test on smallest subset (tests all components except species)
run_quick_test(subset_size = 500, components = c("methods", "parts", "geography", "merge", "analysis"))
#run_quick_test(subset_size = 50, components = c("analysis"))

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
cat("source('scripts/04_analysis/workflows/run_extraction_pipeline.R')\n")
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
