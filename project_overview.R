# Endo-Review Project Overview and Navigation
# B. Bock
# September 22, 2025
#
# This script provides a comprehensive overview of the Endo-Review project structure
# and serves as a navigation guide for all project components.

library(tidyverse)
library(crayon)

cat("🌿 ENDO-REVIEW PROJECT OVERVIEW 🌿\n")
cat("=================================\n\n")

# Project structure overview
project_overview <- list(

  # Core directories
  core_dirs = c(
    "docs/" = "Documentation and guides",
    "scripts/" = "All R scripts organized by function",
    "models/" = "Trained ML models and reference data",
    "test_data/" = "Test datasets and subsets",
    "test_results/" = "Pipeline testing results",
    "results/" = "Main pipeline output files",
    "figures/" = "Generated plots and visualizations",
    "plots/" = "Analysis plots and summaries"
  ),

  # Script organization
  script_structure = list(
    "01_data_processing/" = c(
      "api_pull_abstracts.R" = "API data collection",
      "Combo_abstracts_pull2.R" = "Combined abstract processing",
      "Combo_abstracts.R" = "Legacy abstract processing"
    ),

    "02_model_training/" = c(
      "ML_compare_models_subset.R" = "ML model comparison and training"
    ),

    "03_prediction/" = c(
      "apply_models_to_full_dataset.R" = "Apply trained models to data"
    ),

    "04_analysis/" = list(
      "Core Pipeline:" = c(
        "run_extraction_pipeline.R" = "Main pipeline orchestrator",
        "test_pipeline_workflow.R" = "Comprehensive pipeline testing",
        "quick_start_testing.R" = "One-click pipeline testing",
        "create_test_subset.R" = "Generate test data subsets",
        "optimized_taxa_detection.R" = "Core taxa detection functions",
        "reference_data_utils.R" = "Data normalization utilities"
      ),

      "Pipeline Components:" = c(
        "01_extract_species.R" = "Species detection component",
        "02_extract_methods.R" = "Research methods detection",
        "03_extract_plant_parts.R" = "Plant parts identification",
        "04_extract_geography.R" = "Geographic location detection",
        "05_merge_results.R" = "Result consolidation"
      ),

      "Analysis Modules:" = c(
        "validation/absence_evidence_detection.R" = "Absence evidence analysis",
        "validation/find_all_plants_statement.R" = "Core statement detection",
        "validation/manual_validation_sample.R" = "Validation sampling",
        "temporal/temporal_trend_analysis.R" = "Temporal trend analysis",
        "visualization/run_taxa_visualizations.R" = "Taxa visualization",
        "visualization/visualize_extraction_results.R" = "Comprehensive plotting"
      ),

      "Workflow Scripts:" = c(
        "analysis_workflow.R" = "Analysis workflow orchestration",
        "geographic_bias_analysis.R" = "Geographic bias assessment",
        "extract_species_simple.R" = "Simplified species extraction"
      )
    ),

    "config/" = c(
      "pipeline_config.R" = "Pipeline configuration settings"
    ),

    "utils/" = c(
      "error_handling.R" = "Error handling utilities",
      "memory_optimization.R" = "Memory optimization tools"
    ),

    "archive/" = list(
      "Organized by function:" = c(
        "ml_models/" = "Machine learning model archives",
        "taxa_detection/" = "Taxa detection algorithm archives",
        "pipeline_versions/" = "Pipeline version archives",
        "deprecated/" = "Deprecated script archives"
      )
    )
  ),

  # Documentation structure
  documentation = c(
    "README.md" = "Main project README",
    "README_analysis_workflow.md" = "Analysis workflow guide",
    "README_testing_workflow.md" = "Testing framework guide",
    "README_modular_pipeline.md" = "Modular pipeline documentation",
    "README_taxa_detection.md" = "Taxa detection guide",
    "README_PIPELINE_IMPROVEMENTS.md" = "Pipeline improvement notes",
    "METHODS.md" = "Research methods documentation",
    "PROCESSING_WORKFLOW.md" = "Data processing workflow",
    "PROJECT_STRUCTURE.md" = "Project structure overview",
    "RESEARCH_OBJECTIVES.md" = "Research objectives",
    "SEARCH_STRATEGY.md" = "Literature search strategy",
    "PUBLICATION_STRATEGY.md" = "Publication planning",
    "CITATION_ANALYSIS_FRAMEWORK.md" = "Citation analysis framework",
    "DATA_COLLECTION_LOG.md" = "Data collection tracking",
    "MANUSCRIPT_PREPARATION_CHECKLIST.md" = "Manuscript preparation guide",
    "taxa_detection_improvements.md" = "Taxa detection improvements"
  ),

  # Quick start commands
  quick_start = c(
    "Test Pipeline:" = "source('scripts/04_analysis/quick_start_testing.R')",
    "Run Full Pipeline:" = "source('scripts/run_pipeline.R')",
    "Test Individual Components:" = "source('scripts/04_analysis/test_pipeline_workflow.R')",
    "Generate Documentation:" = "View files in docs/ folder"
  )
)

# Function to display project overview
display_overview <- function() {

  cat("📁 PROJECT STRUCTURE:\n")
  cat("===================\n\n")

  for (dir_name in names(project_overview$core_dirs)) {
    cat(sprintf("📂 %-15s - %s\n", dir_name, project_overview$core_dirs[dir_name]))
  }

  cat("\n🔧 SCRIPTS ORGANIZATION:\n")
  cat("=======================\n\n")

  for (section in names(project_overview$script_structure)) {
    cat(sprintf("📂 scripts/%s\n", section))

    subsection <- project_overview$script_structure[[section]]
    if (is.list(subsection)) {
      for (subsec_name in names(subsection)) {
        cat(sprintf("   📋 %s:\n", subsec_name))
        for (file in names(subsection[[subsec_name]])) {
          cat(sprintf("      📄 %-35s - %s\n", file, subsection[[subsec_name]][file]))
        }
        cat("\n")
      }
    } else {
      for (file in names(subsection)) {
        cat(sprintf("   📄 %-35s - %s\n", file, subsection[file]))
      }
      cat("\n")
    }
  }

  cat("📚 DOCUMENTATION:\n")
  cat("================\n\n")

  for (doc in names(project_overview$documentation)) {
    cat(sprintf("📖 %-35s - %s\n", doc, project_overview$documentation[doc]))
  }

  cat("\n🚀 QUICK START COMMANDS:\n")
  cat("========================\n\n")

  for (cmd_name in names(project_overview$quick_start)) {
    cat(sprintf("💻 %s:\n   %s\n\n", cmd_name, project_overview$quick_start[cmd_name]))
  }
}

# Function to check project health
check_project_health <- function() {

  cat("\n🏥 PROJECT HEALTH CHECK:\n")
  cat("=======================\n\n")

  # Check for required directories
  required_dirs <- c("docs", "scripts", "models", "test_data", "test_results", "results")
  missing_dirs <- c()

  for (dir in required_dirs) {
    if (!dir.exists(dir)) {
      missing_dirs <- c(missing_dirs, dir)
    }
  }

  if (length(missing_dirs) > 0) {
    cat("❌ Missing directories:", paste(missing_dirs, collapse = ", "), "\n")
  } else {
    cat("✅ All required directories present\n")
  }

  # Check for key files
  key_files <- c(
    "scripts/run_pipeline.R",
    "scripts/04_analysis/run_extraction_pipeline.R",
    "scripts/04_analysis/test_pipeline_workflow.R",
    "models/species.rds",
    "docs/README.md"
  )

  missing_files <- c()
  for (file in key_files) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }

  if (length(missing_files) > 0) {
    cat("❌ Missing key files:", paste(missing_files, collapse = ", "), "\n")
  } else {
    cat("✅ All key files present\n")
  }

  # Check archive organization
  archive_dirs <- c("ml_models", "taxa_detection", "pipeline_versions", "deprecated")
  archive_status <- sapply(archive_dirs, function(dir) {
    dir.exists(file.path("scripts/archive", dir))
  })

  if (all(archive_status)) {
    cat("✅ Archive folders properly organized\n")
  } else {
    cat("❌ Missing archive organization for:", paste(names(archive_status)[!archive_status], collapse = ", "), "\n")
  }

  # Check for duplicates (should be none after cleanup)
  potential_duplicates <- list(
    absence = c("scripts/04_analysis/absence_evidence_detection.R", "scripts/04_analysis/validation/absence_evidence_detection.R"),
    plants = c("scripts/04_analysis/find_all_plants_statement.R", "scripts/04_analysis/validation/find_all_plants_statement.R"),
    validation = c("scripts/04_analysis/manual_validation_sample.R", "scripts/04_analysis/validation/manual_validation_sample.R")
  )

  duplicates_found <- FALSE
  for (dup_set in potential_duplicates) {
    existing_files <- dup_set[file.exists(dup_set)]
    if (length(existing_files) > 1) {
      cat("❌ Duplicate files found:", paste(basename(existing_files), collapse = " & "), "\n")
      duplicates_found <- TRUE
    }
  }

  if (!duplicates_found) {
    cat("✅ No duplicate files detected\n")
  }

  # Summary
  cat("\n📊 SUMMARY:\n")
  cat("===========\n")
  issues <- length(missing_dirs) + length(missing_files) + duplicates_found
  if (issues == 0) {
    cat("🎉 Project structure is healthy and well-organized!\n")
  } else {
    cat(sprintf("⚠️  Found %d structural issues to address\n", issues))
  }
}

# Function to generate file statistics
generate_stats <- function() {

  cat("\n📈 PROJECT STATISTICS:\n")
  cat("=====================\n\n")

  # Count files by type
  r_files <- length(list.files(".", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
  rmd_files <- length(list.files(".", pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE))
  md_files <- length(list.files(".", pattern = "\\.md$", recursive = TRUE, full.names = TRUE))
  csv_files <- length(list.files(".", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE))
  rds_files <- length(list.files(".", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE))

  cat("📄 File Counts:\n")
  cat(sprintf("   R scripts:     %d\n", r_files))
  cat(sprintf("   R Markdown:    %d\n", rmd_files))
  cat(sprintf("   Markdown:      %d\n", md_files))
  cat(sprintf("   CSV files:     %d\n", csv_files))
  cat(sprintf("   RDS files:     %d\n", rds_files))
  cat("\n")

  # Directory sizes
  dirs_to_check <- c("scripts", "docs", "models", "test_data", "test_results", "results")
  cat("📂 Directory Contents:\n")

  for (dir in dirs_to_check) {
    if (dir.exists(dir)) {
      file_count <- length(list.files(dir, recursive = TRUE))
      cat(sprintf("   %-12s: %d files\n", paste0(dir, "/"), file_count))
    }
  }

  # Script organization stats
  cat("\n🔧 Script Organization:\n")

  script_dirs <- list.dirs("scripts", recursive = FALSE)
  for (dir in script_dirs) {
    if (basename(dir) != "archive") {
      file_count <- length(list.files(dir, pattern = "\\.R$", recursive = TRUE))
      cat(sprintf("   %-15s: %d R files\n", basename(dir), file_count))
    }
  }

  # Archive organization
  cat("\n📦 Archive Organization:\n")
  archive_subdirs <- list.dirs("scripts/archive", recursive = FALSE)
  for (subdir in archive_subdirs) {
    file_count <- length(list.files(subdir, pattern = "\\.R$"))
    cat(sprintf("   %-12s: %d archived scripts\n", basename(subdir), file_count))
  }
}

# Function to show navigation shortcuts
show_navigation <- function() {

  cat("\n🧭 NAVIGATION SHORTCUTS:\n")
  cat("=======================\n\n")

  shortcuts <- list(
    "Main Pipeline" = c(
      "Start full pipeline" = "source('scripts/run_pipeline.R')",
      "Test pipeline" = "source('scripts/04_analysis/quick_start_testing.R')",
      "Run extraction only" = "source('scripts/04_analysis/run_extraction_pipeline.R')"
    ),

    "Analysis Modules" = c(
      "Species detection" = "source('scripts/04_analysis/components/01_extract_species.R')",
      "Validation analysis" = "source('scripts/04_analysis/validation/absence_evidence_detection.R')",
      "Visualization" = "source('scripts/04_analysis/visualization/run_taxa_visualizations.R')",
      "Temporal analysis" = "source('scripts/04_analysis/temporal/temporal_trend_analysis.R')"
    ),

    "Testing & Validation" = c(
      "Quick pipeline test" = "run_quick_test(100)",
      "Full test suite" = "run_full_test()",
      "Component testing" = "test_pipeline_workflow()"
    ),

    "Documentation" = c(
      "Main README" = "View docs/README.md",
      "Analysis workflow" = "View docs/README_analysis_workflow.md",
      "Testing guide" = "View docs/README_testing_workflow.md",
      "Methods" = "View docs/METHODS.md"
    )
  )

  for (category in names(shortcuts)) {
    cat(sprintf("📂 %s:\n", category))
    for (shortcut_name in names(shortcuts[[category]])) {
      cat(sprintf("   %-20s → %s\n", shortcut_name, shortcuts[[category]][shortcut_name]))
    }
    cat("\n")
  }
}

# Main execution
cat("Welcome to the Endo-Review Project Overview!\n\n")
cat("This project investigates the universality of fungal endophytes in plants\n")
cat("through systematic analysis of scientific literature.\n\n")

# Display all sections
display_overview()
check_project_health()
generate_stats()
show_navigation()

cat("\n🎯 PROJECT OBJECTIVES:\n")
cat("=====================\n")
cat("• Test the hypothesis that 'all plants host fungi'\n")
cat("• Systematically review endophyte literature\n")
cat("• Develop automated text analysis pipeline\n")
cat("• Identify patterns in endophyte-plant associations\n")
cat("• Generate comprehensive visualizations and reports\n\n")

cat("📞 For help or questions, refer to the documentation in docs/ folder\n")
cat("🔗 GitHub: https://github.com/beabock/Endo-Review\n\n")

cat("🎉 Happy researching! 🌿🔬\n\n")
