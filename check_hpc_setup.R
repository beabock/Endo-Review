# =============================================================================
# check_hpc_setup.R - Verify HPC Setup and File Dependencies
# =============================================================================
#
# Purpose: Verify all required files exist before HPC deployment
#
# Usage: Rscript check_hpc_setup.R
#
# =============================================================================

cat("🔍 HPC Setup Verification\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Ensure we're in the correct directory
if (!file.exists("01_species_mycorrhizal_hpc_optimized.R")) {
  stop("❌ Must be run from project root directory containing 01_species_mycorrhizal_hpc_optimized.R")
}

cat("✅ Running from correct root directory\n\n")

# Define all required files (all in root directory for HPC)
required_files <- list(
  "Main script" = "01_species_mycorrhizal_hpc_optimized.R",
  "HPC job script" = "submit_hpc_job.sh",
  "Consolidated dataset" = "consolidated_dataset.csv",
  "FUNGuild data" = "funtothefun.csv",
  "Species reference" = "species.rds",
  "Lookup tables" = "lookup_tables.rds",
  "Taxa detection script" = "optimized_taxa_detection.R",
  "Reference utils" = "reference_data_utils.R"
)

optional_files <- list(
  "Memory optimization" = "memory_optimization.R",
  "Species hash" = "species_hash.rds",
  "Genus hash" = "genus_hash.rds", 
  "Family hash" = "family_hash.rds"
)

# Check required files
cat("📋 Checking Required Files:\n")
missing_required <- character(0)

for (i in seq_along(required_files)) {
  name <- names(required_files)[i]
  file <- required_files[[i]]
  
  if (file.exists(file)) {
    size <- file.size(file)
    size_mb <- round(size / (1024*1024), 2)
    cat("  ✅", name, ":", file, "(", size_mb, "MB )\n")
  } else {
    cat("  ❌", name, ":", file, "- NOT FOUND\n")
    missing_required <- c(missing_required, file)
  }
}

# Check optional files  
cat("\n📋 Checking Optional Files:\n")
for (i in seq_along(optional_files)) {
  name <- names(optional_files)[i]
  file <- optional_files[[i]]
  
  if (file.exists(file)) {
    size <- file.size(file)
    size_mb <- round(size / (1024*1024), 2)
    cat("  ✅", name, ":", file, "(", size_mb, "MB )\n")
  } else {
    cat("  ⚠️", name, ":", file, "- not found (optional)\n")
  }
}

# Check data file row counts
cat("\n📊 Dataset Information:\n")
if (file.exists("consolidated_dataset.csv")) {
  tryCatch({
    library(readr, quietly = TRUE)
    data <- read_csv("consolidated_dataset.csv", show_col_types = FALSE)
    cat("  📄 Consolidated dataset:", nrow(data), "abstracts\n")
    cat("  📄 Columns:", paste(colnames(data), collapse = ", "), "\n")
  }, error = function(e) {
    cat("  ⚠️ Could not read dataset:", e$message, "\n")
  })
}

# Summary
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
if (length(missing_required) == 0) {
  cat("🎉 HPC SETUP READY!\n")
  cat("✅ All required files found\n")
  cat("✅ Ready for HPC deployment\n")
} else {
  cat("❌ HPC SETUP INCOMPLETE!\n") 
  cat("Missing required files:\n")
  for (file in missing_required) {
    cat("  -", file, "\n")
  }
  cat("\nPlease ensure all files are present before HPC deployment.\n")
}

cat("\n📋 Next steps:\n")
cat("  1. Upload all files to HPC system\n")
cat("  2. Adjust submit_hpc_job.sh for your HPC environment\n") 
cat("  3. Submit job: sbatch submit_hpc_job.sh\n")