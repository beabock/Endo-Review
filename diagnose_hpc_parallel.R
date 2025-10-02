#!/usr/bin/env Rscript
# =============================================================================
# diagnose_hpc_parallel.R - Diagnose HPC Parallel Issues
# =============================================================================

cat("🔍 HPC Parallel Processing Diagnostics\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Check system info
cat("💻 System Information:\n")
cat("  OS:", .Platform$OS.type, "\n")
cat("  R version:", R.version.string, "\n")
cat("  Available cores:", parallel::detectCores(), "\n")

# Check memory
if (.Platform$OS.type == "unix") {
  mem_info <- system("free -g", intern = TRUE)
  cat("  Memory info:\n")
  cat("   ", mem_info[2], "\n")
} else {
  cat("  Memory: Windows system\n")
}

# Test basic parallel functionality
cat("\n🧪 Testing Basic Parallel Functionality:\n")

# Test 1: Can we create a small cluster?
cat("  Test 1: Creating 2-core cluster...")
test1 <- tryCatch({
  cl <- parallel::makeCluster(2, type = "FORK")
  cat(" ✅ SUCCESS\n")
  
  # Test 2: Can we evaluate on cluster?
  cat("  Test 2: Testing cluster evaluation...")
  result <- parallel::clusterEvalQ(cl, {Sys.getpid()})
  cat(" ✅ SUCCESS (", length(result), "workers responded)\n")
  
  # Test 3: Can we export data?
  cat("  Test 3: Testing data export...")
  test_data <- data.frame(x = 1:10, y = letters[1:10])
  parallel::clusterExport(cl, "test_data")
  cat(" ✅ SUCCESS\n")
  
  # Clean up
  parallel::stopCluster(cl)
  TRUE
}, error = function(e) {
  cat(" ❌ FAILED:", e$message, "\n")
  FALSE
})

if (!test1) {
  cat("\n⚠️  Basic parallel processing failed. HPC system may not support FORK clusters.\n")
  cat("   Recommendation: Use sequential processing\n")
} else {
  cat("\n✅ Basic parallel processing works\n")
  
  # Test with larger cluster
  cat("  Test 4: Creating 8-core cluster...")
  test2 <- tryCatch({
    cl <- parallel::makeCluster(8, type = "FORK")
    
    # Test with libraries
    cat(" Testing library loading...")
    parallel::clusterEvalQ(cl, {
      library(tidyverse, quietly = TRUE)
      TRUE
    })
    cat(" ✅ SUCCESS\n")
    
    parallel::stopCluster(cl)
    TRUE
  }, error = function(e) {
    cat(" ❌ FAILED:", e$message, "\n")
    FALSE
  })
  
  if (!test2) {
    cat("   ⚠️ Large clusters or library loading failed\n")
    cat("   Recommendation: Use smaller clusters or sequential processing\n")
  }
}

# Check if lookup tables exist and their sizes
cat("\n📊 Data File Check:\n")
files_to_check <- c("lookup_tables.rds", "species.rds", "consolidated_dataset.csv")

for (file in files_to_check) {
  if (file.exists(file)) {
    size_mb <- file.size(file) / (1024*1024)
    cat("  ✅", file, ":", round(size_mb, 1), "MB\n")
    
    # Check if very large files might cause export issues
    if (size_mb > 500) {
      cat("    ⚠️ Large file - may cause clusterExport issues\n")
    }
  } else {
    cat("  ❌", file, "- NOT FOUND\n")
  }
}

cat("\n🎯 Recommendations:\n")
if (!test1) {
  cat("  • Use sequential processing: Rscript 01_species_mycorrhizal_hpc_sequential.R\n")
} else {
  cat("  • Parallel processing should work, but consider:\n")
  cat("    - Using fewer cores (4-8 instead of 16)\n")
  cat("    - Smaller batch sizes\n")
  cat("    - Sequential fallback if cluster export hangs\n")
}

cat("\n🔧 Quick Fixes:\n")
cat("  1. Kill current job: scancel <job_id>\n")
cat("  2. Use sequential version: sbatch -J sequential --wrap=\"Rscript 01_species_mycorrhizal_hpc_sequential.R\"\n")
cat("  3. Or modify current script to force sequential mode\n")