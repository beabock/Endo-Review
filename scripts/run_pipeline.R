# Master Pipeline Runner for Endophyte Review Project
# B. Bock
# Comprehensive pipeline to run the complete endophyte analysis workflow

# =============================================================================
# SETUP AND CONFIGURATION
# =============================================================================

# Load configuration
source("scripts/config/pipeline_config.R")

# Load required libraries
required_packages <- c(
  "tidyverse", "tidytext", "caret", "Matrix", "text", "tm", "recipes", 
  "themis", "janitor", "tictoc", "maps", "ggplot2", "viridis", "scales",
  "lubridate", "stringr"
)

# Install missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# Load all packages
suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

cat("=== ENDOPHYTE REVIEW PIPELINE ===\n")
cat("Master pipeline runner for comprehensive analysis\n")
cat("Goal: Assess whether all plants host fungal endophytes\n\n")

# =============================================================================
# PIPELINE CONFIGURATION
# =============================================================================

# Pipeline stages
PIPELINE_STAGES <- list(
  model_training = list(
    name = "Model Training",
    script = "scripts/02_model_training/ML_compare_models_subset.R",
    description = "Train ML models for relevance and presence/absence classification",
    required_files = c(INPUT_FILES$training_labeled),
    outputs = c(MODEL_FILES$relevance_model, MODEL_FILES$presence_glmnet, MODEL_FILES$presence_svm),
    estimated_time = "15-30 minutes"
  ),
  
  full_prediction = list(
    name = "Full Dataset Prediction",
    script = "scripts/03_prediction/apply_models_to_full_dataset.R", 
    description = "Apply trained models to complete dataset",
    required_files = c(INPUT_FILES$all_abstracts, MODEL_FILES$relevance_model, 
                      MODEL_FILES$presence_glmnet, MODEL_FILES$presence_svm),
    outputs = c("results/full_dataset_predictions.csv", "results/relevant_abstracts_with_pa_predictions.csv"),
    estimated_time = "10-20 minutes"
  ),
  
  species_extraction = list(
    name = "Species and Information Extraction",
    script = "scripts/04_analysis/extract_species_simple.R",
    description = "Extract species, methods, geography, and plant parts",
    required_files = c("results/relevant_abstracts_with_pa_predictions.csv", INPUT_FILES$species_data),
    outputs = c("results/comprehensive_extraction_results.csv", "results/species_detection_weighted_ensemble.csv"),
    estimated_time = "30-60 minutes"
  ),
  
  geographic_analysis = list(
    name = "Geographic Bias Analysis", 
    script = "scripts/04_analysis/geographic_bias_analysis.R",
    description = "Analyze geographic patterns and research equity",
    required_files = c("results/comprehensive_extraction_results.csv"),
    outputs = c("results/geographic_bias_analysis_report.txt", "results/country_research_patterns.csv"),
    estimated_time = "5-10 minutes"
  ),
  
  temporal_analysis = list(
    name = "Temporal Trend Analysis",
    script = "scripts/04_analysis/temporal_trend_analysis.R", 
    description = "Analyze research trends over time",
    required_files = c("results/comprehensive_extraction_results.csv"),
    outputs = c("results/temporal_trends_summary.csv", "results/temporal_trends_report.txt"),
    estimated_time = "5-10 minutes"
  ),
  
  absence_detection = list(
    name = "Absence Evidence Detection",
    script = "scripts/04_analysis/absence_evidence_detection.R",
    description = "Identify studies reporting absence of endophytes", 
    required_files = c("results/comprehensive_extraction_results.csv"),
    outputs = c("results/absence_evidence_analysis.csv", "results/high_confidence_absence_evidence.csv"),
    estimated_time = "5-10 minutes"
  ),
  
  validation_sample = list(
    name = "Manual Validation Sample",
    script = "scripts/04_analysis/manual_validation_sample.R",
    description = "Generate stratified sample for manual validation",
    required_files = c("results/comprehensive_extraction_results.csv"),
    outputs = c("results/validation_sample_for_manual_review.csv", "results/VALIDATION_INSTRUCTIONS.md"),
    estimated_time = "2-5 minutes"
  )
)

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Run a single pipeline stage with error handling
#' @param stage_name Name of the stage
#' @param stage_config Configuration for the stage
#' @param force_run Whether to run even if outputs exist
run_pipeline_stage <- function(stage_name, stage_config, force_run = FALSE) {
  cat("\n", rep("=", 60), "\n")
  cat("STAGE:", stage_config$name, "\n")
  cat("Description:", stage_config$description, "\n")
  cat("Estimated time:", stage_config$estimated_time, "\n")
  cat(rep("=", 60), "\n")
  
  # Check if outputs already exist (unless forcing)
  if (!force_run && all(file.exists(stage_config$outputs))) {
    cat("✓ Outputs already exist. Skipping stage.\n")
    cat("  Use force_run = TRUE to rerun this stage.\n")
    return(list(success = TRUE, skipped = TRUE, message = "Outputs exist"))
  }
  
  # Check required files
  tryCatch({
    check_required_files(stage_config$required_files, stage_config$name)
  }, error = function(e) {
    return(list(success = FALSE, skipped = FALSE, message = paste("Missing files:", e$message)))
  })
  
  # Run the stage
  cat("Running:", stage_config$script, "\n")
  start_time <- Sys.time()
  
  result <- tryCatch({
    # Source the script
    source(stage_config$script, local = TRUE)
    
    # Check if outputs were created
    missing_outputs <- stage_config$outputs[!file.exists(stage_config$outputs)]
    if (length(missing_outputs) > 0) {
      warning("Some expected outputs were not created: ", paste(missing_outputs, collapse = ", "))
    }
    
    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    cat("✓ Stage completed successfully in", round(runtime, 1), "minutes\n")
    
    list(success = TRUE, skipped = FALSE, runtime = runtime, 
         message = "Completed successfully")
    
  }, error = function(e) {
    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    cat("✗ Stage failed after", round(runtime, 1), "minutes\n")
    cat("Error:", e$message, "\n")
    
    list(success = FALSE, skipped = FALSE, runtime = runtime,
         message = paste("Failed:", e$message))
  })
  
  return(result)
}

#' Generate pipeline summary report
#' @param results List of stage results
generate_pipeline_report <- function(results) {
  total_stages <- length(results)
  successful_stages <- sum(sapply(results, function(x) x$success))
  skipped_stages <- sum(sapply(results, function(x) x$skipped))
  failed_stages <- total_stages - successful_stages
  
  total_runtime <- sum(sapply(results, function(x) x$runtime %||% 0))
  
  report <- list(
    summary = list(
      total_stages = total_stages,
      successful = successful_stages,
      skipped = skipped_stages, 
      failed = failed_stages,
      total_runtime_minutes = round(total_runtime, 1)
    ),
    stage_details = results
  )
  
  return(report)
}

# =============================================================================
# MAIN PIPELINE EXECUTION
# =============================================================================

#' Run the complete endophyte analysis pipeline
#' @param stages Vector of stage names to run (default: all)
#' @param force_run Whether to rerun stages with existing outputs
#' @param stop_on_error Whether to stop pipeline if a stage fails
run_endophyte_pipeline <- function(stages = names(PIPELINE_STAGES), 
                                  force_run = FALSE, 
                                  stop_on_error = TRUE) {
  
  cat("Starting Endophyte Review Pipeline\n")
  cat("Stages to run:", paste(stages, collapse = ", "), "\n")
  cat("Force rerun:", force_run, "\n")
  cat("Stop on error:", stop_on_error, "\n\n")
  
  # Initialize results tracking
  pipeline_results <- list()
  pipeline_start_time <- Sys.time()
  
  # Run each requested stage
  for (stage_name in stages) {
    if (!stage_name %in% names(PIPELINE_STAGES)) {
      cat("Warning: Unknown stage '", stage_name, "'. Skipping.\n")
      next
    }
    
    stage_config <- PIPELINE_STAGES[[stage_name]]
    stage_result <- run_pipeline_stage(stage_name, stage_config, force_run)
    
    pipeline_results[[stage_name]] <- stage_result
    
    # Stop on error if requested
    if (!stage_result$success && stop_on_error) {
      cat("\n❌ Pipeline stopped due to error in stage:", stage_name, "\n")
      break
    }
    
    # Memory cleanup between stages
    gc(verbose = FALSE)
  }
  
  # Generate final report
  pipeline_end_time <- Sys.time()
  total_pipeline_time <- as.numeric(difftime(pipeline_end_time, pipeline_start_time, units = "mins"))
  
  report <- generate_pipeline_report(pipeline_results)
  report$summary$total_pipeline_time_minutes <- round(total_pipeline_time, 1)
  
  # Print summary
  cat("\n", rep("=", 80), "\n")
  cat("PIPELINE EXECUTION SUMMARY\n")
  cat(rep("=", 80), "\n")
  cat("Total stages:", report$summary$total_stages, "\n")
  cat("Successful:", report$summary$successful, "\n")
  cat("Skipped:", report$summary$skipped, "\n") 
  cat("Failed:", report$summary$failed, "\n")
  cat("Total runtime:", report$summary$total_pipeline_time_minutes, "minutes\n")
  
  if (report$summary$failed > 0) {
    cat("\n❌ Pipeline completed with errors\n")
    failed_stages <- names(pipeline_results)[sapply(pipeline_results, function(x) !x$success)]
    cat("Failed stages:", paste(failed_stages, collapse = ", "), "\n")
  } else {
    cat("\n✅ Pipeline completed successfully!\n")
  }
  
  # Save detailed report
  capture.output({
    cat("=== ENDOPHYTE PIPELINE EXECUTION REPORT ===\n")
    cat("Generated:", Sys.time(), "\n")
    cat("Total execution time:", report$summary$total_pipeline_time_minutes, "minutes\n\n")
    
    cat("STAGE RESULTS:\n")
    for (stage_name in names(pipeline_results)) {
      result <- pipeline_results[[stage_name]]
      status <- if (result$success) "✓ SUCCESS" else "✗ FAILED"
      if (result$skipped) status <- "⏭ SKIPPED"
      
      cat(stage_name, ":", status, "\n")
      cat("  Message:", result$message, "\n")
      if (!is.null(result$runtime)) {
        cat("  Runtime:", round(result$runtime, 1), "minutes\n")
      }
      cat("\n")
    }
    
    cat("NEXT STEPS:\n")
    if (report$summary$failed == 0) {
      cat("1. Review results in the results/ directory\n")
      cat("2. Examine comprehensive_extraction_results.csv for main findings\n")
      cat("3. Check validation_sample_for_manual_review.csv for quality control\n")
      cat("4. Use analysis reports for manuscript preparation\n")
    } else {
      cat("1. Address failed stages before proceeding\n")
      cat("2. Check error messages above for troubleshooting\n")
      cat("3. Ensure all required input files are available\n")
      cat("4. Consider running individual stages for debugging\n")
    }
  }, file = file.path(OUTPUT_DIRS$results, "pipeline_execution_report.txt"))
  
  return(report)
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Run only the core analysis pipeline (skip model training)
run_analysis_only <- function(force_run = FALSE) {
  analysis_stages <- c("full_prediction", "species_extraction", "geographic_analysis", 
                      "temporal_analysis", "absence_detection", "validation_sample")
  return(run_endophyte_pipeline(analysis_stages, force_run = force_run))
}

#' Run only model training and prediction stages
run_ml_pipeline <- function(force_run = FALSE) {
  ml_stages <- c("model_training", "full_prediction")
  return(run_endophyte_pipeline(ml_stages, force_run = force_run))
}

#' Run only the analysis stages (assumes extraction is complete)
run_downstream_analysis <- function(force_run = FALSE) {
  downstream_stages <- c("geographic_analysis", "temporal_analysis", 
                         "absence_detection", "validation_sample")
  return(run_endophyte_pipeline(downstream_stages, force_run = force_run))
}

# =============================================================================
# INTERACTIVE MODE
# =============================================================================

if (interactive()) {
  cat("\n=== INTERACTIVE PIPELINE RUNNER ===\n")
  cat("Available functions:\n")
  cat("• run_endophyte_pipeline() - Run complete pipeline\n")
  cat("• run_analysis_only() - Skip model training, run analysis\n") 
  cat("• run_ml_pipeline() - Run only ML training and prediction\n")
  cat("• run_downstream_analysis() - Run only final analysis stages\n")
  cat("\nExample usage:\n")
  cat("  result <- run_endophyte_pipeline()\n")
  cat("  result <- run_analysis_only(force_run = TRUE)\n")
  cat("\nTo run specific stages:\n")
  cat("  result <- run_endophyte_pipeline(stages = c('species_extraction', 'geographic_analysis'))\n")
}

# =============================================================================
# AUTO-RUN (if not in interactive mode)
# =============================================================================

if (!interactive()) {
  cat("Running complete pipeline automatically...\n")
  pipeline_result <- run_endophyte_pipeline()
  
  if (pipeline_result$summary$failed == 0) {
    cat("\n🎉 Pipeline completed successfully!\n")
    quit(status = 0)
  } else {
    cat("\n💥 Pipeline failed. Check logs for details.\n")
    quit(status = 1)
  }
}
