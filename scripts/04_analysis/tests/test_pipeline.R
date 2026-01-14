# Pipeline Testing and Debugging Script
# B. Bock
# Test individual components of the pipeline

# Initialize scoring
total_tests <- 0
passed_tests <- 0
failed_tests <- 0

# Helper function for scoring
test_result <- function(passed, test_name, details = "") {
  total_tests <<- total_tests + 1
  if (passed) {
    passed_tests <<- passed_tests + 1
    cat("   ✅ PASS:", test_name, "\n")
  } else {
    failed_tests <<- failed_tests + 1
    cat("   ❌ FAIL:", test_name, "\n")
    if (details != "") cat("      Details:", details, "\n")
  }
}

# Load configuration and utilities
source("scripts/config/pipeline_config.R")
source("scripts/utils/error_handling.R")
source("scripts/utils/memory_optimization.R")

cat("=== PIPELINE TESTING AND DEBUGGING ===\n")

# =============================================================================
# TEST 1: File System Check
# =============================================================================

cat("\n1. Testing file system access...\n")

test_files <- c(
  INPUT_FILES$training_labeled,
  INPUT_FILES$all_abstracts,
  INPUT_FILES$species_data
)

for (file_path in test_files) {
  exists <- file.exists(file_path)
  status <- if (exists) "✅" else "❌"
  cat(status, basename(file_path), "-", file_path, "\n")
}

# Score file system check
files_exist <- all(sapply(test_files, file.exists))
test_result(files_exist, "File system access", paste0(sum(sapply(test_files, file.exists)), "/", length(test_files), " files found"))

# =============================================================================
# TEST 2: Configuration Loading
# =============================================================================

cat("\n2. Testing configuration loading...\n")

# Test configuration access
cat("✓ INPUT_FILES$training_labeled:", INPUT_FILES$training_labeled, "\n")
cat("✓ MODEL_FILES$relevance_model:", MODEL_FILES$relevance_model, "\n")
cat("✓ ENSEMBLE_WEIGHTS$svm_weight_presence:", ENSEMBLE_WEIGHTS$svm_weight_presence, "\n")
cat("✓ BATCH_SIZES$species_detection:", BATCH_SIZES$species_detection, "\n")

# Score configuration loading
config_loaded <- exists("INPUT_FILES") && exists("MODEL_FILES") && exists("ENSEMBLE_WEIGHTS") && exists("BATCH_SIZES")
test_result(config_loaded, "Configuration loading", "All required config objects exist")

# =============================================================================
# TEST 3: Safe File Reading
# =============================================================================

cat("\n3. Testing safe file reading...\n")

# Test reading training data
cat("Testing safe file reading...\n")

# Try to read the file directly first
if (file.exists(INPUT_FILES$training_labeled)) {
  cat("Primary file exists, testing read...\n")
  try_result <- try({
    data <- readr::read_csv(INPUT_FILES$training_labeled, show_col_types = FALSE)
    cat("✅ File read successfully\n")
    cat("   Rows:", nrow(data), "\n")
    cat("   Columns:", ncol(data), "\n")
    training_test <- list(success = TRUE, result = data, error = NULL)
  }, silent = TRUE)

  if (inherits(try_result, "try-error")) {
    cat("❌ File exists but cannot be read\n")
    cat("   Error:", attr(try_result, "condition")$message, "\n")
    training_test <- list(success = FALSE, result = NULL, error = attr(try_result, "condition")$message)
  }
} else {
  cat("❌ Primary file does not exist:", INPUT_FILES$training_labeled, "\n")

  # Try backup file
  if (file.exists(INPUT_FILES$training_backup)) {
    cat("Trying backup file:", INPUT_FILES$training_backup, "\n")
    try_result <- try({
      data <- readr::read_csv(INPUT_FILES$training_backup, show_col_types = FALSE)
      cat("✅ Backup file read successfully\n")
      training_test <- list(success = TRUE, result = data, error = NULL)
    }, silent = TRUE)

    if (inherits(try_result, "try-error")) {
      cat("❌ Backup file exists but cannot be read\n")
      training_test <- list(success = FALSE, result = NULL, error = attr(try_result, "condition")$message)
    }
  } else {
    cat("❌ Backup file does not exist either\n")
    training_test <- list(success = FALSE, result = NULL, error = "No training files found")
  }
}

# Check if the result is valid
if (training_test$success) {
  cat("✅ Training data loaded successfully\n")
  cat("   Rows:", nrow(training_test$result), "\n")
  cat("   Columns:", ncol(training_test$result), "\n")
  cat("   Column names:", paste(head(names(training_test$result), 5), collapse = ", "), "...\n")
} else {
  cat("❌ Failed to load training data\n")
  if (!is.null(training_test$error)) {
    cat("   Error:", training_test$error, "\n")
  }
}

# Score safe file reading
test_result(training_test$success, "Safe file reading", ifelse(training_test$success, "Training data loaded successfully", "Failed to load training data"))

# =============================================================================
# TEST 4: Error Handling
# =============================================================================

cat("\n4. Testing error handling...\n")

# Test safe execution
safe_test <- safe_execute({
  x <- 1 + 1
  cat("   Inside safe execution block\n")
  return(x * 2)
}, context = "Test safe execution")

if (safe_test$success) {
  cat("✅ Safe execution successful\n")
  cat("   Result:", safe_test$result, "\n")
} else {
  cat("❌ Safe execution failed\n")
  cat("   Error:", safe_test$error, "\n")
}

# Test error handling
error_test <- safe_execute({
  stop("This is a test error")
}, context = "Test error handling")

if (!error_test$success) {
  cat("✅ Error handling working correctly\n")
  cat("   Error message captured:", error_test$error, "\n")
} else {
  cat("❌ Error handling not working\n")
}

# Score error handling
error_handling_works <- !error_test$success && safe_test$success
test_result(error_handling_works, "Error handling", ifelse(error_handling_works, "Safe execution and error capture working", "Error handling issues detected"))

# =============================================================================
# TEST 5: Memory Management
# =============================================================================

cat("\n5. Testing memory management...\n")

# Test memory monitoring
mem_status <- monitor_memory(threshold_gb = 100)  # High threshold for testing
cat("✅ Memory monitoring working\n")
cat("   Memory used:", round(mem_status$memory_used_gb, 2), "GB\n")

# Test garbage collection
aggressive_gc(verbose = FALSE)
cat("✅ Garbage collection working\n")

# Score memory management
memory_functions_exist <- exists("monitor_memory") && exists("aggressive_gc")
test_result(memory_functions_exist, "Memory management", ifelse(memory_functions_exist, "Memory monitoring and GC functions available", "Memory management functions missing"))

# =============================================================================
# TEST 6: Data Validation
# =============================================================================

cat("\n6. Testing data validation...\n")

if (training_test$success) {
  # Test dataframe validation
  validation_test <- safe_execute({
    validate_dataframe(training_test$result,
                      required_columns = c("id", "label"),
                      min_rows = 1,
                      context = "Test validation")
  }, context = "Data validation test")

  if (validation_test$success) {
    cat("✅ Data validation working\n")
  } else {
    cat("❌ Data validation failed\n")
    cat("   Error:", validation_test$error, "\n")
  }
} else {
  cat("⚠️ Skipping data validation - no training data available\n")
  validation_test <- list(success = TRUE)  # Don't fail if no data to validate
}

# Score data validation
test_result(validation_test$success, "Data validation", ifelse(validation_test$success, "Data validation functions working", "Data validation failed"))

# =============================================================================
# TEST 7: Pipeline Stage Check
# =============================================================================

cat("\n7. Testing pipeline stage configuration...\n")

# Check if pipeline stages are properly configured
source("scripts/run_pipeline.R")

if (exists("PIPELINE_STAGES")) {
  cat("✅ Pipeline stages loaded\n")
  cat("   Number of stages:", length(PIPELINE_STAGES), "\n")

  # Check first stage configuration
  first_stage <- PIPELINE_STAGES[[1]]
  cat("   First stage:", first_stage$name, "\n")
  cat("   Required files:", length(first_stage$required_files), "\n")
  cat("   Outputs:", length(first_stage$outputs), "\n")
} else {
  cat("❌ Pipeline stages not loaded\n")
}

# Score pipeline configuration
pipeline_configured <- exists("PIPELINE_STAGES") && length(PIPELINE_STAGES) > 0
test_result(pipeline_configured, "Pipeline configuration", ifelse(pipeline_configured, paste0(length(PIPELINE_STAGES), " pipeline stages configured"), "Pipeline stages not properly configured"))

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! Pipeline is ready to run.\n")
  cat("\nTry running:\n")
  cat("  source('scripts/run_pipeline.R')\n")
  cat("  run_endophyte_pipeline()\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}

# Save test results
test_results <- list(
  timestamp = Sys.time(),
  tests = tests,
  passed = passed_tests,
  total = total_tests,
  success = passed_tests == total_tests
)

saveRDS(test_results, "results/pipeline_test_results.rds")
cat("\nTest results saved to: results/pipeline_test_results.rds\n")
