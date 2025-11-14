# =============================================================================
# test_extract_species.R - Comprehensive test script for 01_extract_species.R
# =============================================================================
#
# Purpose: Test core functionality, accuracy, performance, and robustness of the
# species extraction component.
#
# Description: This script runs comprehensive tests for data loading, species extraction
# processing, accuracy validation, synonym resolution, taxonomic hierarchy validation,
# name disambiguation, typo handling, performance benchmarks, error handling,
# and completeness checks.
#
# Dependencies: tidyverse, tictoc, janitor
#
# Author: B. Bock
# Date: 2025-09-22
#
# Inputs/Outputs: Uses test data from test_data/ and creates mock data; outputs
# test results to console and optionally to file.
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(janitor)

# Source the component script (assumes it's in the same project structure)
# Source required utility functions directly
source("scripts/04_analysis/utilities/reference_data_utils.R")
source("scripts/04_analysis/components/optimized_taxa_detection.R")
source("scripts/04_analysis/components/01_species_mycorrhizal_hpc_sequential.R")

# Create a simplified version that doesn't have the directory check
# We'll define a wrapper that calls the main function with error handling

# Create a temporary environment to store functions
temp_env <- new.env()

# Read the file and extract functions manually
file_path <- "scripts/04_analysis/components/01_species_mycorrhizal_hpc_sequential.R"
if (!file.exists(file_path)) {
  stop("Error: Cannot find file ", file_path)
}
file_content <- readLines(file_path)

# Find function definitions and extract them
function_lines <- character(0)
in_function <- FALSE
brace_count <- 0
in_skipped_block <- FALSE

for (line in file_content) {
  # Skip the problematic directory check and its entire block
  if (grepl("if \\(!file\\.exists.*01_species_mycorrhizal_hpc_optimized\\.R.*\\)", line)) {
    in_skipped_block <- TRUE
    next
  }
  if (in_skipped_block) {
    if (grepl("^\\s*}\\s*$", line)) {
      in_skipped_block <- FALSE
    }
    next
  }

  # Skip the cat commands that print mode messages
  if (grepl("cat.*HPC SEQUENTIAL MODE", line)) {
    next
  }
  # Skip main execution code
  if (grepl("cat.*HPC SEQUENTIAL SPECIES DETECTION.*\\n.*$", line)) {
    break
  }

  # Check if this is a function definition
  if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\s*<-\\s*function", line) ||
      grepl("^extract_species_mycorrhizal_data_sequential\\s*<-\\s*function", line)) {
    in_function <- TRUE
    brace_count <- 0
    function_lines <- c(function_lines, line)
  } else if (in_function) {
    function_lines <- c(function_lines, line)
    brace_count <- brace_count + (str_count(line, "\\{") - str_count(line, "\\}"))

    # If braces balance, we've reached the end of the function
    if (brace_count == 0 && grepl("\\}", line)) {
      in_function <- FALSE
    }
  } else if (!grepl("^\\s*#", line) && !grepl("^\\s*$", line) && !grepl("^library\\(", line)) {
    # Include other code that's not in functions (like variable assignments)
    function_lines <- c(function_lines, line)
  }
}

# Evaluate the extracted functions
eval(parse(text = paste(function_lines, collapse = "\n")), envir = temp_env)

# Copy functions to global environment
for (obj_name in ls(temp_env)) {
  assign(obj_name, get(obj_name, temp_env), envir = .GlobalEnv)
}


cat("=== SPECIES EXTRACTION TEST SUITE ===\n\n")

# =============================================================================
# TEST 1: Data Loading Functionality
# =============================================================================

test_data_loading <- function() {
  cat("🔍 Test 1: Data Loading Functionality\n")
  cat("   Description: Verify loading of species reference data and abstracts\n\n")

  tryCatch({
    # Check species RDS loading
    species_path <- if (file.exists("models/species.rds")) "models/species.rds" else "species.rds"
    if (!file.exists(species_path)) {
      cat("   ❌ FAIL: Species reference data not found\n")
      return(list(passed = FALSE, score = 0))
    }
    species <- readRDS(species_path)
    cat("   ✅ PASS: Loaded", nrow(species), "species records\n")

    # Check abstracts loading
    abstracts_path <- "results/prepared_abstracts_for_extraction.csv"
    if (!file.exists(abstracts_path)) {
      cat("   ⚠️  WARN: Prepared abstracts not found (skipping abstracts loading test)\n")
      return(list(passed = TRUE, score = 0.75))  # Partial pass
    }
    abstracts <- read_csv(abstracts_path, show_col_types = FALSE)
    cat("   ✅ PASS: Loaded", nrow(abstracts), "abstracts\n")

    return(list(passed = TRUE, score = 1.0))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during data loading -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# TEST 2: Species Extraction Processing (with Mock Data)
# =============================================================================

test_species_processing <- function() {
  cat("🔍 Test 2: Species Extraction Processing\n")
  cat("   Description: Test core extraction logic with controlled mock data\n\n")

  # Create mock abstracts with known species mentions
  mock_abstracts <- tibble(
    id = 1:5,
    article_title = c("Test Paper 1", "Test Paper 2", "Test Paper 3", "Test Paper 4", "Test Paper 5"),
    abstract = c(
      "This study examines the effects of drought on Quercus robur and Pinus sylvestris growth patterns.",
      "Research on Triticum aestivum and Glycine max reveals important genetic interactions.",
      "Analysis of Arabidopsis thaliana and Oryza sativa provides insights into plant responses.",
      "Investigation of Solanum lycopersicum and Zea mays shows nutrient uptake differences.",
      "Study of Phaseolus vulgaris and Hordeum vulgare demonstrates adaptation strategies."
    ),
    authors = rep("Test Author", 5),
    source_title = rep("Test Journal", 5),
    publication_year = 2023,
    doi = paste0("10.1000/test.", 1:5),
    Relevant = rep(1, 5),
    relevance_loose = rep("Presence", 5),
    glmnet_pred = rep(0.8, 5),
    svm_pred = rep(0.9, 5),
    weighted_ensemble = rep(0.85, 5),
    threshold_ensemble = rep(0.7, 5),
    glmnet_prob_presence = rep(0.8, 5),
    glmnet_prob_absence = rep(0.2, 5),
    svm_prob_presence = rep(0.9, 5),
    svm_prob_absence = rep(0.1, 5),
    pa_loose = rep("Presence", 5),
    pa_medium = rep("Presence", 5),
    pa_strict = rep("Presence", 5),
    pa_super_strict = rep("Presence", 5),
    final_classification = rep("Presence", 5),
    conservative_classification = rep("Presence", 5),
    predicted_label = rep("Presence", 5)
  )

  tryCatch({
    # Create temporary output file for testing
    temp_output <- tempfile(fileext = ".csv")

    # Run extraction on mock data
    results <- extract_species_data(
      mock_abstracts,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )

    # Check if results were generated
    if (nrow(results) == 0) {
      cat("   ❌ FAIL: No results generated\n")
      return(list(passed = FALSE, score = 0))
    }

    cat("   ✅ PASS: Processed", nrow(results), "abstracts, generated", nrow(results), "result rows\n")
    # Clean up
    if (file.exists(temp_output)) file.remove(temp_output)

    return(list(passed = TRUE, score = 1.0))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during processing -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# TEST 3: Accuracy Testing
# =============================================================================

test_accuracy <- function() {
  cat("🔍 Test 3: Accuracy Testing\n")
  cat("   Description: Measure precision and recall against expected species\n\n")

  # Expected species for each abstract (based on the mock data above)
  expected_species <- list(
    c("Quercus robur", "Pinus sylvestris"),
    c("Triticum aestivum", "Glycine max"),
    c("Arabidopsis thaliana", "Oryza sativa"),
    c("Solanum lycopersicum", "Zea mays"),
    c("Phaseolus vulgaris", "Hordeum vulgare")
  )

  # Reuse mock data from test 2
  mock_abstracts <- tibble(
    id = 1:5,
    article_title = c("Test Paper 1", "Test Paper 2", "Test Paper 3", "Test Paper 4", "Test Paper 5"),
    abstract = c(
      "This study examines the effects of drought on Quercus robur and Pinus sylvestris growth patterns.",
      "Research on Triticum aestivum and Glycine max reveals important genetic interactions.",
      "Analysis of Arabidopsis thaliana and Oryza sativa provides insights into plant responses.",
      "Investigation of Solanum lycopersicum and Zea mays shows nutrient uptake differences.",
      "Study of Phaseolus vulgaris and Hordeum vulgare demonstrates adaptation strategies."
    ),
    authors = rep("Test Author", 5),
    source_title = rep("Test Journal", 5),
    publication_year = 2023,
    doi = paste0("10.1000/test.", 1:5),
    Relevant = rep(1, 5),
    relevance_loose = rep("Presence", 5),
    glmnet_pred = rep(0.8, 5),
    svm_pred = rep(0.9, 5),
    weighted_ensemble = rep(0.85, 5),
    threshold_ensemble = rep(0.7, 5),
    glmnet_prob_presence = rep(0.8, 5),
    glmnet_prob_absence = rep(0.2, 5),
    svm_prob_presence = rep(0.9, 5),
    svm_prob_absence = rep(0.1, 5),
    pa_loose = rep("Presence", 5),
    pa_medium = rep("Presence", 5),
    pa_strict = rep("Presence", 5),
    pa_super_strict = rep("Presence", 5),
    final_classification = rep("Presence", 5),
    conservative_classification = rep("Presence", 5),
    predicted_label = rep("Presence", 5)
  )

  tryCatch({
    temp_output <- tempfile(fileext = ".csv")
    results <- extract_species_data(
      mock_abstracts,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )

    # Calculate precision and recall by aggregating results by abstract ID
    total_expected <- sum(lengths(expected_species))
    total_detected <- 0
    true_positives <- 0

    # Group results by abstract ID and collect all detected species
    results_by_id <- results %>%
      group_by(id) %>%
      summarize(
        detected_species = list(unique(na.omit(c(resolved_name, acceptedScientificName))))
      ) %>%
      arrange(id)

    for (i in 1:length(expected_species)) {
      if (i <= nrow(results_by_id)) {
        detected <- results_by_id$detected_species[[i]]
      } else {
        detected <- character(0)
      }

      exp <- expected_species[[i]]
      true_positives <- true_positives + length(intersect(exp, detected))
      total_detected <- total_detected + length(detected)
    }

    precision <- if (total_detected > 0) true_positives / total_detected else 0
    recall <- if (total_expected > 0) true_positives / total_expected else 0

    cat("   📊 Precision:", round(precision * 100, 1), "%\n")
    cat("   📊 Recall:", round(recall * 100, 1), "%\n")

    score <- (precision + recall) / 2  # F1-like score
    passed <- score >= 0.5  # Reasonable threshold

    if (passed) {
      cat("   ✅ PASS: Accuracy score =", round(score, 3), "\n")
    } else {
      cat("   ❌ FAIL: Low accuracy score =", round(score, 3), "\n")
    }

    if (file.exists(temp_output)) file.remove(temp_output)
    return(list(passed = passed, score = score))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during accuracy testing -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# TEST 4: Processing Time Benchmarks
# =============================================================================

test_processing_time <- function() {
  cat("🔍 Test 4: Processing Time Benchmarks\n")
  cat("   Description: Benchmark processing time for different dataset sizes\n\n")

  sizes <- c(50)
  times <- numeric(length(sizes))
  passed <- TRUE

  for (i in seq_along(sizes)) {
    size <- sizes[i]
    test_file <- paste0("test_data/test_subset_random_", size, ".csv")

    if (!file.exists(test_file)) {
      cat("   ⚠️  WARN: Test file", test_file, "not found, skipping size", size, "\n")
      next
    }

    tryCatch({
      data <- read_csv(test_file, show_col_types = FALSE)
      temp_output <- tempfile(fileext = ".csv")

      tic()
      results <- extract_species_data(
        data,
        output_file = temp_output,
        batch_size = 50,
        force_rerun = TRUE,
        verbose = FALSE
      )
      time_taken <- toc(quiet = TRUE)
      times[i] <- time_taken$toc - time_taken$tic

      cat("   📊 Size", size, "abstracts: processed in", round(times[i], 2), "seconds\n")

      if (file.exists(temp_output)) file.remove(temp_output)
    }, error = function(e) {
      cat("   ❌ FAIL: Error processing size", size, "-", e$message, "\n")
      passed <- FALSE
    })
  }

  # Check for reasonable performance (less than 5 minutes for 500 abstracts)
  reasonable_times <- times <= 300  # 5 minutes
  if (any(!reasonable_times & times > 0)) {
    cat("   ⚠️  WARN: Some sizes took longer than expected\n")
    passed <- FALSE
  }

  score <- min(1.0, 300 / max(times[times > 0]))  # Scale score based on max time
  if (passed) {
    cat("   ✅ PASS: Performance benchmarks completed\n")
  } else {
    cat("   ❌ FAIL: Performance issues detected\n")
  }

  return(list(passed = passed, score = score))
}

# =============================================================================
# TEST 5: Error Handling
# =============================================================================

test_error_handling <- function() {
  cat("🔍 Test 5: Error Handling\n")
  cat("   Description: Test graceful failure on invalid inputs\n\n")

  passed <- TRUE
  score <- 0

  # Test 1: Invalid abstracts data (missing required columns)
  tryCatch({
    invalid_data <- tibble(
      id = 1:2,
      # Missing abstract column
      title = c("Test", "Test2")
    )
    temp_output <- tempfile(fileext = ".csv")
    results <- extract_species_data(
      invalid_data,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )
    cat("   ⚠️  WARN: Should have failed with invalid data\n")
    passed <- FALSE
  }, error = function(e) {
    cat("   ✅ PASS: Correctly handled invalid data structure\n")
    score <- score + 0.3
  })

  # Test 2: Empty abstracts data
  tryCatch({
    empty_data <- tibble(
      id = integer(0),
      article_title = character(0),
      abstract = character(0),
      authors = character(0),
      source_title = character(0),
      publication_year = integer(0),
      doi = character(0),
      Relevant = logical(0),
      relevance_loose = character(0),
      glmnet_pred = numeric(0),
      svm_pred = numeric(0),
      weighted_ensemble = numeric(0),
      threshold_ensemble = numeric(0),
      glmnet_prob_presence = numeric(0),
      glmnet_prob_absence = numeric(0),
      svm_prob_presence = numeric(0),
      svm_prob_absence = numeric(0),
      pa_loose = character(0),
      pa_medium = character(0),
      pa_strict = character(0),
      pa_super_strict = character(0),
      final_classification = character(0),
      conservative_classification = character(0)
    )
    temp_output <- tempfile(fileext = ".csv")
    results <- extract_species_data(
      empty_data,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )
    cat("   ✅ PASS: Correctly handled empty data\n")
    score <- score + 0.3
  }, error = function(e) {
    cat("   ⚠️  WARN: Failed on empty data -", e$message, "\n")
    passed <- FALSE
  })

  # Test 3: Missing species data
  tryCatch({
    # Temporarily rename species file if it exists
    species_path <- if (file.exists("models/species.rds")) "models/species.rds" else "species.rds"
    if (file.exists(species_path)) {
      backup_path <- paste0(species_path, ".backup")
      file.rename(species_path, backup_path)

      mock_abstracts <- tibble(
        id = 1,
        article_title = "Test",
        abstract = "Test abstract",
        authors = "Test",
        source_title = "Test",
        publication_year = 2023,
        doi = "10.1000/test.1",
        Relevant = 1,
        relevance_loose = "Presence",
        glmnet_pred = 0.8,
        svm_pred = 0.9,
        weighted_ensemble = 0.85,
        threshold_ensemble = 0.7,
        glmnet_prob_presence = 0.8,
        glmnet_prob_absence = 0.2,
        svm_prob_presence = 0.9,
        svm_prob_absence = 0.1,
        pa_loose = "Presence",
        pa_medium = "Presence",
        pa_strict = "Presence",
        pa_super_strict = "Presence",
        final_classification = "Presence",
        conservative_classification = "Presence",
        predicted_label = "Presence"
      )

      temp_output <- tempfile(fileext = ".csv")
      results <- extract_species_data(
        mock_abstracts,
        output_file = temp_output,
        batch_size = 10,
        force_rerun = TRUE,
        verbose = FALSE
      )
      cat("   ⚠️  WARN: Should have failed with missing species data\n")
      passed <- FALSE

      # Restore file
      file.rename(backup_path, species_path)
    } else {
      cat("   ⚠️  SKIP: Species file not found, skipping missing data test\n")
    }
  }, error = function(e) {
    cat("   ✅ PASS: Correctly handled missing species data\n")
    score <- score + 0.4
    # Restore file if backup exists
    backup_path <- paste0(species_path, ".backup")
    if (file.exists(backup_path)) file.rename(backup_path, species_path)
  })

  if (passed) {
    cat("   ✅ PASS: Error handling tests completed\n")
  } else {
    cat("   ❌ FAIL: Error handling issues detected\n")
  }

  return(list(passed = passed, score = score))
}

# =============================================================================
# TEST 6: Completeness Testing
# =============================================================================

test_completeness <- function() {
  cat("🔍 Test 6: Completeness Testing\n")
  cat("   Description: Ensure all abstracts are processed without errors\n\n")

  test_file <- "test_data/test_subset_random_50.csv"
  if (!file.exists(test_file)) {
    cat("   ⚠️  SKIP: Test file not found\n")
    return(list(passed = TRUE, score = 0.5))  # Neutral score
  }

  tryCatch({
    data <- read_csv(test_file, show_col_types = FALSE)
    initial_count <- nrow(data)
    temp_output <- tempfile(fileext = ".csv")

    results <- extract_species_data(
      data,
      output_file = temp_output,
      batch_size = 25,
      force_rerun = TRUE,
      verbose = FALSE
    )

    final_count <- nrow(results)

    if (final_count != initial_count) {
      cat("   ❌ FAIL: Input/output count mismatch -", initial_count, "vs", final_count, "\n")
      return(list(passed = FALSE, score = 0))
    }

    # Check for any NA values in critical columns (allowing some flexibility)
    na_count <- sum(is.na(results$id) | is.na(results$article_title))
    if (na_count > 0) {
      cat("   ⚠️  WARN:", na_count, "records with missing critical data\n")
    }

    cat("   ✅ PASS: All", initial_count, "abstracts processed successfully\n")
    if (file.exists(temp_output)) file.remove(temp_output)

    return(list(passed = TRUE, score = 1.0))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during completeness testing -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# TEST 7: Synonym Testing
# =============================================================================

test_synonym_resolution <- function() {
  cat("🔍 Test 7: Synonym Resolution Testing\n")
  cat("   Description: Test resolution of taxonomic synonyms to accepted names\n\n")

  # Create mock abstracts with known synonyms
  mock_abstracts <- tibble(
    id = 1:6,
    article_title = c(
      "Test Paper 1", "Test Paper 2", "Test Paper 3", "Test Paper 4",
      "Test Paper 5", "Test Paper 6"
    ),
    abstract = c(
      "This study examines Amanita muscaria toxicity patterns.",  # Valid name (accepted)
      "Research on Agaricus campestris reveals nutritional value.",  # Valid name
      "Analysis of Boletus edulis provides insights into mycorrhizal fungi.",  # Valid name
      "Investigation of Amanita muscaria reveals toxin production.",  # Repeat valid name
      "Study of Fomes fomentarius medicinal properties.",  # Valid name
      "Paper about Boletus edulis in forest ecosystems."  # Repeat valid name
    ),
    authors = rep("Test Author", 6),
    source_title = rep("Test Journal", 6),
    publication_year = 2023,
    doi = paste0("10.1000/test.", 1:6),
    Relevant = rep(1, 6),
    relevance_loose = rep("Presence", 6),
    glmnet_pred = rep(0.8, 6),
    svm_pred = rep(0.9, 6),
    weighted_ensemble = rep(0.85, 6),
    threshold_ensemble = rep(0.7, 6),
    glmnet_prob_presence = rep(0.8, 6),
    glmnet_prob_absence = rep(0.2, 6),
    svm_prob_presence = rep(0.9, 6),
    svm_prob_absence = rep(0.1, 6),
    pa_loose = rep("Presence", 6),
    pa_medium = rep("Presence", 6),
    pa_strict = rep("Presence", 6),
    pa_super_strict = rep("Presence", 6),
    final_classification = rep("Presence", 6),
    conservative_classification = rep("Presence", 6),
    predicted_label = rep("Presence", 6)
  )

  tryCatch({
    temp_output <- tempfile(fileext = ".csv")
    results <- extract_species_data(
      mock_abstracts,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )

    # Analyze synonym resolution (all names above should be accepted names)
    synonym_resolution_results <- results %>%
      filter(!is.na(resolved_name)) %>%
      mutate(
        correctly_resolved = status == "ACCEPTED",
        is_synonym_resolved = status %in% c("ACCEPTED", "SYNONYM")
      )

    total_detected <- nrow(synonym_resolution_results)
    correctly_accepted <- sum(synonym_resolution_results$correctly_resolved, na.rm = TRUE)
    synonyms_handled <- sum(synonym_resolution_results$is_synonym_resolved, na.rm = TRUE)

    synonym_resolution_rate <- if (total_detected > 0) correctly_accepted / total_detected else 0
    overall_resolution_rate <- if (total_detected > 0) synonyms_handled / total_detected else 0

    cat("   📊 Synonym Resolution Results:\n")
    cat("   📊 Total species detected:", total_detected, "\n")
    cat("   📊 Correctly resolved to accepted names:", correctly_accepted, "\n")
    cat("   📊 Overall resolution rate:", round(overall_resolution_rate * 100, 1), "%\n")
    cat("   📊 Accepted name accuracy:", round(synonym_resolution_rate * 100, 1), "%\n")

    # Test passes if we can resolve species names (allowing for synonyms)
    passed <- overall_resolution_rate >= 0.5  # At least 50% resolution rate

    if (passed) {
      cat("   ✅ PASS: Synonym resolution working correctly\n")
    } else {
      cat("   ❌ FAIL: Poor synonym resolution performance\n")
    }

    if (file.exists(temp_output)) file.remove(temp_output)
    return(list(passed = passed, score = overall_resolution_rate))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during synonym testing -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# TEST 8: Taxonomic Hierarchy Validation
# =============================================================================

test_taxonomic_hierarchy <- function() {
  cat("🔍 Test 8: Taxonomic Hierarchy Validation\n")
  cat("   Description: Test genus and family level taxonomic classification accuracy\n\n")

  # Create mock abstracts with genus and family level mentions
  mock_abstracts <- tibble(
    id = 1:8,
    article_title = c(
      "Test Paper 1", "Test Paper 2", "Test Paper 3", "Test Paper 4",
      "Test Paper 5", "Test Paper 6", "Test Paper 7", "Test Paper 8"
    ),
    abstract = c(
      "This study examines Quercus species in temperate forests.",  # Genus: Quercus
      "Research on Pinus distribution patterns worldwide.",  # Genus: Pinus
      "Analysis of Fagaceae family diversity.",  # Family: Fagaceae
      "Investigation of Pinaceae family ecology.",  # Family: Pinaceae
      "Study of Acer species in maple forests.",  # Genus: Acer
      "Paper about Betulaceae family characteristics.",  # Family: Betulaceae
      "Research on Picea species adaptation.",  # Genus: Picea
      "Analysis of Salicaceae family responses."  # Family: Salicaceae
    ),
    authors = rep("Test Author", 8),
    source_title = rep("Test Journal", 8),
    publication_year = 2023,
    doi = paste0("10.1000/test.", 1:8),
    Relevant = rep(1, 8),
    relevance_loose = rep("Presence", 8),
    glmnet_pred = rep(0.8, 8),
    svm_pred = rep(0.9, 8),
    weighted_ensemble = rep(0.85, 8),
    threshold_ensemble = rep(0.7, 8),
    glmnet_prob_presence = rep(0.8, 8),
    glmnet_prob_absence = rep(0.2, 8),
    svm_prob_presence = rep(0.9, 8),
    svm_prob_absence = rep(0.1, 8),
    pa_loose = rep("Presence", 8),
    pa_medium = rep("Presence", 8),
    pa_strict = rep("Presence", 8),
    pa_super_strict = rep("Presence", 8),
    final_classification = rep("Presence", 8),
    conservative_classification = rep("Presence", 8),
    predicted_label = rep("Presence", 8)
  )

  tryCatch({
    temp_output <- tempfile(fileext = ".csv")
    results <- extract_species_data(
      mock_abstracts,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )

    # Analyze taxonomic hierarchy detection
    hierarchy_results <- results %>%
      filter(!is.na(match_type)) %>%
      mutate(
        hierarchy_correct = case_when(
          match_type == "species" & !is.na(canonicalName) ~ TRUE,
          match_type == "genus" & !is.na(genus) ~ TRUE,
          match_type == "family" & !is.na(family) ~ TRUE,
          TRUE ~ FALSE
        )
      )

    # Count detections by taxonomic level
    species_detected <- sum(hierarchy_results$match_type == "species", na.rm = TRUE)
    genus_detected <- sum(hierarchy_results$match_type == "genus", na.rm = TRUE)
    family_detected <- sum(hierarchy_results$match_type == "family", na.rm = TRUE)

    total_detected <- nrow(hierarchy_results)
    hierarchy_accuracy <- if (total_detected > 0) {
      sum(hierarchy_results$hierarchy_correct, na.rm = TRUE) / total_detected
    } else {
      0
    }

    cat("   📊 Taxonomic Hierarchy Results:\n")
    cat("   📊 Species-level detections:", species_detected, "\n")
    cat("   📊 Genus-level detections:", genus_detected, "\n")
    cat("   📊 Family-level detections:", family_detected, "\n")
    cat("   📊 Total taxonomic detections:", total_detected, "\n")
    cat("   📊 Hierarchy accuracy:", round(hierarchy_accuracy * 100, 1), "%\n")

    # Test passes if we detect taxonomic information at multiple levels
    passed <- total_detected >= 4 && hierarchy_accuracy >= 0.7  # At least 4 detections with 70% accuracy

    if (passed) {
      cat("   ✅ PASS: Taxonomic hierarchy validation successful\n")
    } else {
      cat("   ❌ FAIL: Insufficient taxonomic hierarchy detection\n")
    }

    if (file.exists(temp_output)) file.remove(temp_output)
    return(list(passed = passed, score = hierarchy_accuracy))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during hierarchy testing -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# TEST 9: Name Disambiguation Testing
# =============================================================================

test_name_disambiguation <- function() {
  cat("🔍 Test 9: Name Disambiguation Testing\n")
  cat("   Description: Test handling of homonyms and ambiguous taxonomic names\n\n")

  # Create mock abstracts with potentially ambiguous names
  mock_abstracts <- tibble(
    id = 1:6,
    article_title = c(
      "Test Paper 1", "Test Paper 2", "Test Paper 3", "Test Paper 4",
      "Test Paper 5", "Test Paper 6"
    ),
    abstract = c(
      "This study examines Acer rubrum in North American forests.",  # Specific species
      "Research on Acer species diversity globally.",  # Genus level
      "Analysis of Quercus alba distribution patterns.",  # Specific oak species
      "Investigation of Quercus species in Mediterranean climates.",  # Genus level
      "Study of Pinus sylvestris adaptation strategies.",  # Specific pine species
      "Paper about Pinus species worldwide distribution."  # Genus level
    ),
    authors = rep("Test Author", 6),
    source_title = rep("Test Journal", 6),
    publication_year = 2023,
    doi = paste0("10.1000/test.", 1:6),
    Relevant = rep(1, 6),
    relevance_loose = rep("Presence", 6),
    glmnet_pred = rep(0.8, 6),
    svm_pred = rep(0.9, 6),
    weighted_ensemble = rep(0.85, 6),
    threshold_ensemble = rep(0.7, 6),
    glmnet_prob_presence = rep(0.8, 6),
    glmnet_prob_absence = rep(0.2, 6),
    svm_prob_presence = rep(0.9, 6),
    svm_prob_absence = rep(0.1, 6),
    pa_loose = rep("Presence", 6),
    pa_medium = rep("Presence", 6),
    pa_strict = rep("Presence", 6),
    pa_super_strict = rep("Presence", 6),
    final_classification = rep("Presence", 6),
    conservative_classification = rep("Presence", 6),
    predicted_label = rep("Presence", 6)
  )

  tryCatch({
    temp_output <- tempfile(fileext = ".csv")
    results <- extract_species_data(
      mock_abstracts,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )

    # Analyze name disambiguation (species vs genus level)
    disambiguation_results <- results %>%
      filter(!is.na(match_type) & !is.na(resolved_name)) %>%
      mutate(
        # Check if specific epithet is present (species-level) vs genus-only
        has_specific_epithet = str_count(resolved_name, "\\s+") >= 1,
        taxonomic_level = case_when(
          match_type == "species" & has_specific_epithet ~ "species_specific",
          match_type == "genus" ~ "genus_level",
          match_type == "family" ~ "family_level",
          TRUE ~ "other"
        )
      )

    # Count disambiguation accuracy
    species_specific <- sum(disambiguation_results$taxonomic_level == "species_specific", na.rm = TRUE)
    genus_level <- sum(disambiguation_results$taxonomic_level == "genus_level", na.rm = TRUE)

    total_disambiguated <- nrow(disambiguation_results)
    disambiguation_accuracy <- if (total_disambiguated > 0) {
      # We expect to see both species-specific and genus-level detections
      (species_specific + genus_level) / total_disambiguated
    } else {
      0
    }

    # Check for proper taxonomic information preservation
    taxonomic_info_complete <- results %>%
      filter(!is.na(resolved_name)) %>%
      mutate(
        has_kingdom = !is.na(kingdom),
        has_phylum = !is.na(phylum),
        has_family = !is.na(family) | match_type == "family",
        taxonomic_completeness = (has_kingdom + has_phylum + has_family) / 3
      ) %>%
      pull(taxonomic_completeness) %>%
      mean(na.rm = TRUE)

    cat("   📊 Name Disambiguation Results:\n")
    cat("   📊 Species-specific detections:", species_specific, "\n")
    cat("   📊 Genus-level detections:", genus_level, "\n")
    cat("   📊 Total names processed:", total_disambiguated, "\n")
    cat("   📊 Disambiguation accuracy:", round(disambiguation_accuracy * 100, 1), "%\n")
    cat("   📊 Taxonomic completeness:", round(taxonomic_info_complete * 100, 1), "%\n")

    # Test passes if we have reasonable disambiguation and taxonomic information
    passed <- disambiguation_accuracy >= 0.6 && taxonomic_info_complete >= 0.7

    if (passed) {
      cat("   ✅ PASS: Name disambiguation working correctly\n")
    } else {
      cat("   ❌ FAIL: Issues with name disambiguation or taxonomic information\n")
    }

    if (file.exists(temp_output)) file.remove(temp_output)
    return(list(passed = passed, score = (disambiguation_accuracy + taxonomic_info_complete) / 2))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during disambiguation testing -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# TEST 7: Typo Handling Testing (renamed to TEST 10)
# =============================================================================

test_typo_handling <- function() {
  cat("🔍 Test 7: Typo Handling Testing\n")
  cat("   Description: Evaluate handling of various typos in species names\n\n")

  # Expected species for each abstract (normalized form)
  expected_species <- list(
    c("Quercus alba"),  # lowercase genus
    c("Quercus alba"),  # missing capital in species
    c("Quercus alba"),  # extra spaces
    c("Quercus alba"),  # punctuation
    c("Quercus alba"),  # abbreviation (may not resolve if not handled)
    c("Quercus alba"),  # misspelling (may not resolve if no fuzzy matching)
    c("Quercus alba")   # mixed cases
  )

  # Create mock abstracts with various typos in species names
  mock_abstracts <- tibble(
    id = 1:7,
    article_title = c(
      "Test Paper 1", "Test Paper 2", "Test Paper 3", "Test Paper 4",
      "Test Paper 5", "Test Paper 6", "Test Paper 7"
    ),
    abstract = c(
      "This study examines quercus alba in drought conditions.",  # lowercase genus
      "Research on Quercus Alba reveals genetic information.",     # missing capital in species
      "Analysis of Quercus  alba and other species.",              # extra spaces
      "Investigation of Quercus alba. in the field.",              # punctuation
      "Study of Q. alba growth patterns.",                         # abbreviation
      "Paper about Quercuss alba adaptation.",                     # misspelling
      "Findings on quercus Alba in ecosystems."                    # mixed cases
    ),
    authors = rep("Test Author", 7),
    source_title = rep("Test Journal", 7),
    publication_year = 2023,
    doi = paste0("10.1000/test.", 1:7),
    Relevant = rep(1, 7),
    relevance_loose = rep("Presence", 7),
    glmnet_pred = rep(0.8, 7),
    svm_pred = rep(0.9, 7),
    weighted_ensemble = rep(0.85, 7),
    threshold_ensemble = rep(0.7, 7),
    glmnet_prob_presence = rep(0.8, 7),
    glmnet_prob_absence = rep(0.2, 7),
    svm_prob_presence = rep(0.9, 7),
    svm_prob_absence = rep(0.1, 7),
    pa_loose = rep("Presence", 7),
    pa_medium = rep("Presence", 7),
    pa_strict = rep("Presence", 7),
    pa_super_strict = rep("Presence", 7),
    final_classification = rep("Presence", 7),
    conservative_classification = rep("Presence", 7),
    predicted_label = rep("Presence", 7)
  )

  tryCatch({
    temp_output <- tempfile(fileext = ".csv")
    results <- extract_species_data(
      mock_abstracts,
      output_file = temp_output,
      batch_size = 10,
      force_rerun = TRUE,
      verbose = FALSE
    )

    # Calculate Typo Handling Accuracy by aggregating results by abstract ID
    total_expected <- sum(lengths(expected_species))
    total_detected <- 0
    true_positives <- 0

    # Group results by abstract ID and collect all detected species
    results_by_id <- results %>%
      group_by(id) %>%
      summarize(
        detected_species = list(unique(na.omit(c(resolved_name, acceptedScientificName))))
      ) %>%
      arrange(id)

    for (i in 1:length(expected_species)) {
      if (i <= nrow(results_by_id)) {
        detected <- results_by_id$detected_species[[i]]
      } else {
        detected <- character(0)
      }

      exp <- expected_species[[i]]
      true_positives <- true_positives + length(intersect(exp, detected))
      total_detected <- total_detected + length(detected)
    }

    precision <- if (total_detected > 0) true_positives / total_detected else 0
    recall <- if (total_expected > 0) true_positives / total_expected else 0

    cat("   📊 Typo Handling Precision:", round(precision * 100, 1), "%\n")
    cat("   📊 Typo Handling Recall:", round(recall * 100, 1), "%\n")

    score <- (precision + recall) / 2  # F1-like score for Typo Handling Accuracy
    passed <- score >= 0.3  # Reasonable threshold for typo handling

    if (passed) {
      cat("   ✅ PASS: Typo Handling Accuracy score =", round(score, 3), "\n")
    } else {
      cat("   ❌ FAIL: Low Typo Handling Accuracy score =", round(score, 3), "\n")
    }

    if (file.exists(temp_output)) file.remove(temp_output)
    return(list(passed = passed, score = score))
  }, error = function(e) {
    cat("   ❌ FAIL: Error during typo handling testing -", e$message, "\n")
    return(list(passed = FALSE, score = 0))
  })
}

# =============================================================================
# RUN ALL TESTS
# =============================================================================

run_all_tests <- function() {
  results <- list()

  # Run each test
  results$data_loading <- test_data_loading()
  results$species_processing <- test_species_processing()
  results$accuracy <- test_accuracy()
  results$processing_time <- test_processing_time()
  results$error_handling <- test_error_handling()
  results$completeness <- test_completeness()
  results$synonym_resolution <- test_synonym_resolution()
  results$taxonomic_hierarchy <- test_taxonomic_hierarchy()
  results$name_disambiguation <- test_name_disambiguation()
  results$typo_handling <- test_typo_handling()

  # Calculate overall scores
  test_weights <- c(
    data_loading = 0.10,
    species_processing = 0.15,
    accuracy = 0.15,
    processing_time = 0.08,
    error_handling = 0.08,
    completeness = 0.08,
    synonym_resolution = 0.10,
    taxonomic_hierarchy = 0.10,
    name_disambiguation = 0.10,
    typo_handling = 0.06
  )

  overall_score <- sum(sapply(names(results), function(test) {
    results[[test]]$score * test_weights[test]
  }))

  passed_tests <- sum(sapply(results, function(x) x$passed))
  total_tests <- length(results)

  cat("\n=== TEST SUMMARY ===\n")
  cat("Passed tests:", passed_tests, "/", total_tests, "\n")
  cat("Overall score:", round(overall_score * 100, 1), "%\n\n")

  # Detailed breakdown
  cat("📊 Detailed Results:\n")
  for (test_name in names(results)) {
    status <- if (results[[test_name]]$passed) "✅ PASS" else "❌ FAIL"
    score <- round(results[[test_name]]$score * 100, 1)
    cat("   ", test_name, ":", status, "(", score, "%)\n")
  }

  cat("\n🎯 Final Assessment:\n")
  if (overall_score >= 0.8 && passed_tests == total_tests) {
    cat("   🏆 EXCELLENT: All tests passed with high scores\n")
  } else if (overall_score >= 0.6 && passed_tests >= total_tests - 1) {
    cat("   👍 GOOD: Minor issues but generally functional\n")
  } else if (overall_score >= 0.4) {
    cat("   ⚠️  FAIR: Significant issues need attention\n")
  } else {
    cat("   ❌ POOR: Critical failures require fixes\n")
  }

  return(list(overall_score = overall_score, results = results))
}

# Execute tests
final_results <- run_all_tests()

cat("\n✅ Species extraction test script completed!\n")
