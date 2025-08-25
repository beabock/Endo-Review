# Pipeline Configuration File
# B. Bock
# Centralized configuration for Endophyte Review Pipeline
# This file defines all file paths, column mappings, and parameters used across scripts

# =============================================================================
# FILE PATHS CONFIGURATION
# =============================================================================

# Data directories
DATA_DIRS <- list(
  raw = "data/raw",
  processed = "data/processed", 
  gbif = "data/gbif_backbone"
)

# Input files
INPUT_FILES <- list(
  # Training data
  training_labeled = file.path(DATA_DIRS$raw, "Training_labeled_abs_6.csv"),
  training_backup = file.path(DATA_DIRS$raw, "Training_labeled_abs_5.csv"),
  
  # Full dataset
  all_abstracts = file.path(DATA_DIRS$processed, "All_abstracts_deduped.csv"),
  all_abstracts_backup = "data/All_Abstracts.csv",
  
  # Reference data
  species_data = "models/species.rds",
  families_data = "models/families.rds",
  genera_data = "models/genera.rds"
)

# Model files
MODEL_FILES <- list(
  relevance_model = "models/best_model_relevance_glmnet.rds",
  presence_glmnet = "models/best_model_presence_glmnet_ensemble.rds",
  presence_svm = "models/best_model_presence_svmLinear_ensemble.rds"
)

# Output directories
OUTPUT_DIRS <- list(
  results = "results",
  plots = "plots",
  models = "models",
  temp = "results/temp"
)

# =============================================================================
# COLUMN NAME MAPPINGS
# =============================================================================

# Standard column names expected by pipeline
STANDARD_COLUMNS <- list(
  # Core identification
  id = "id",
  doi = "doi",
  title = "article_title",
  abstract = "abstract",
  
  # Publication info
  authors = "authors",
  source_title = "source_title",
  publication_year = "publication_year",
  
  # Training labels
  label = "label",
  relevance = "relevance",
  
  # Prediction columns
  predicted_label = "predicted_label",
  final_classification = "final_classification",
  confidence = "confidence"
)

# Column name harmonization mapping (from various input formats to standard)
COLUMN_MAPPINGS <- list(
  # Title variations
  "title" = "article_title",
  "article_title" = "article_title",
  
  # Author variations  
  "authors" = "authors",
  "book_authors" = "book_authors",
  "author_full_names" = "author_full_names",
  
  # Publication variations
  "year" = "publication_year",
  "publication_year" = "publication_year",
  "source_title" = "source_title",
  "journal" = "source_title",
  
  # Geographic variations
  "language_of_original_document" = "language",
  "conference_name" = "conference_title",
  "abbreviated_source_title" = "journal_abbreviation",
  
  # Citation variations
  "times_cited" = "times_cited_wo_s_core",
  "total_times_cited" = "times_cited_all_databases",
  
  # Identifier variations
  "wos_id" = "ut_unique_wos_id",
  "pub_med_id" = "pubmed_id"
)

# =============================================================================
# MODEL PARAMETERS
# =============================================================================

# Relevance classification thresholds
RELEVANCE_THRESHOLDS <- list(
  loose = 0.5,
  medium = 0.6,
  strict = 0.8
)

# Presence/Absence classification thresholds  
PA_THRESHOLDS <- list(
  loose = 0.5,
  medium = 0.6,
  strict = 0.8,
  super_strict = 0.9
)

# Ensemble weights (optimized from training)
ENSEMBLE_WEIGHTS <- list(
  svm_weight_presence = 0.6,
  glm_weight_absence = 0.8,
  optimal_threshold = 0.55
)

# =============================================================================
# PROCESSING PARAMETERS
# =============================================================================

# Batch processing
BATCH_SIZES <- list(
  species_detection = 25,
  text_processing = 100,
  dtm_processing = 1000
)

# Parallel processing
PARALLEL_CONFIG <- list(
  default_workers = 2,
  max_workers = 4,
  use_parallel = TRUE
)

# Memory management
MEMORY_CONFIG <- list(
  use_sparse_matrices = TRUE,
  chunk_size = 5000,
  gc_frequency = 100  # Run garbage collection every N iterations
)

# =============================================================================
# VALIDATION PARAMETERS
# =============================================================================

# Manual validation sampling
VALIDATION_CONFIG <- list(
  target_sample_size = 200,
  high_confidence_n = 25,
  medium_confidence_n = 15,
  low_confidence_n = 8,
  min_per_stratum = 1
)

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Check if required files exist
#' @param files Vector of file paths to check
#' @param script_name Name of calling script for error messages
check_required_files <- function(files, script_name = "Unknown") {
  missing_files <- files[!file.exists(files)]
  
  if (length(missing_files) > 0) {
    cat("ERROR in", script_name, ":\n")
    cat("Missing required files:\n")
    for (file in missing_files) {
      cat("  -", file, "\n")
    }
    
    # Suggest alternatives if available
    for (file in missing_files) {
      if (file %in% INPUT_FILES) {
        backup_options <- INPUT_FILES[grepl("backup", names(INPUT_FILES))]
        if (length(backup_options) > 0) {
          cat("  Consider using backup files or running prerequisite scripts\n")
        }
      }
    }
    
    stop("Please ensure all required files exist before running the pipeline.")
  }
  
  cat("✓ All required files found for", script_name, "\n")
  return(TRUE)
}

#' Create output directories if they don't exist
#' @param dirs List of directory paths to create
ensure_output_dirs <- function(dirs = OUTPUT_DIRS) {
  for (dir_name in names(dirs)) {
    dir_path <- dirs[[dir_name]]
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cat("Created directory:", dir_path, "\n")
    }
  }
}

#' Harmonize column names using the mapping
#' @param data Data frame to harmonize
#' @param mappings Column mapping list (default: COLUMN_MAPPINGS)
harmonize_column_names <- function(data, mappings = COLUMN_MAPPINGS) {
  # Apply mappings where they exist
  current_names <- names(data)
  new_names <- current_names
  
  for (i in seq_along(current_names)) {
    old_name <- current_names[i]
    if (old_name %in% names(mappings)) {
      new_names[i] <- mappings[[old_name]]
    }
  }
  
  names(data) <- new_names
  return(data)
}

#' Safe file reading with fallback options
#' @param primary_file Primary file to try
#' @param backup_files Vector of backup file options
#' @param script_name Name of calling script
safe_read_csv <- function(primary_file, backup_files = NULL, script_name = "Unknown") {
  # Try primary file first
  if (file.exists(primary_file)) {
    cat("Reading primary file:", primary_file, "\n")
    return(readr::read_csv(primary_file, show_col_types = FALSE))
  }
  
  # Try backup files
  if (!is.null(backup_files)) {
    for (backup_file in backup_files) {
      if (file.exists(backup_file)) {
        cat("Primary file not found, using backup:", backup_file, "\n")
        return(readr::read_csv(backup_file, show_col_types = FALSE))
      }
    }
  }
  
  # If nothing works, provide helpful error
  stop("Could not find required data file for ", script_name, 
       ". Tried: ", paste(c(primary_file, backup_files), collapse = ", "))
}

#' Memory-efficient garbage collection
#' @param iteration Current iteration number
#' @param frequency How often to run gc (default from config)
manage_memory <- function(iteration, frequency = MEMORY_CONFIG$gc_frequency) {
  if (iteration %% frequency == 0) {
    gc(verbose = FALSE)
  }
}

# =============================================================================
# INITIALIZATION
# =============================================================================

# Ensure output directories exist when config is loaded
ensure_output_dirs()

cat("Pipeline configuration loaded successfully!\n")
cat("Key settings:\n")
cat("- Data directory:", DATA_DIRS$raw, "\n")
cat("- Results directory:", OUTPUT_DIRS$results, "\n") 
cat("- Parallel workers:", PARALLEL_CONFIG$default_workers, "\n")
cat("- Batch size (species):", BATCH_SIZES$species_detection, "\n")
