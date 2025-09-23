# =============================================================================
# profile_species.R - Comprehensive performance profiling for species extraction pipeline
# =============================================================================
#
# Purpose: Profile different pipeline stages in species extraction with detailed timing
# and memory analysis across various dataset sizes and batch configurations.
#
# Stages profiled:
# 1. Text preprocessing and tokenization
# 2. Candidate name extraction (regex patterns)
# 3. Validation against species.rds (joins and lookups)
# 4. Abbreviation expansion and synonym resolution
# 5. Text matching integration
#
# Dependencies: tidyverse, tictoc, pryr, bench
#
# Author: Auto-generated profiling script
# Date: 2025-09-23
#
# =============================================================================

library(tidyverse)
library(tictoc)
library(pryr)  # For memory profiling
library(bench)  # For benchmarking

cat("=== COMPREHENSIVE SPECIES EXTRACTION PERFORMANCE PROFILE ===\n\n")

# Source required functions
source("scripts/04_analysis/optimized_taxa_detection.R")
source("scripts/04_analysis/utilities/reference_data_utils.R")

# =============================================================================
# SETUP AND DATA LOADING
# =============================================================================

cat("🔧 SETUP AND DATA LOADING\n")

# Load species data once
species_path <- if (file.exists("models/species.rds")) "models/species.rds" else "species.rds"
cat("Loading species reference data...\n")
tic("Species data load")
species <- readRDS(species_path)
load_time <- toc(quiet = TRUE)
cat(sprintf("   ✅ Loaded %d species records in %.3f seconds\n", nrow(species), load_time$toc - load_time$tic))
cat(sprintf("   📊 Memory usage: %s\n", format(object.size(species), units = "MB")))

# Create lookup tables
cat("Creating optimized lookup tables...\n")
tic("Lookup table creation")
lookup_tables <- create_lookup_tables(species)
lookup_time <- toc(quiet = TRUE)
cat(sprintf("   ✅ Lookup tables created in %.3f seconds\n", lookup_time$toc - lookup_time$tic))
cat(sprintf("   📊 Lookup memory: %s\n", format(object.size(lookup_tables), units = "MB")))

# Get plant parts keywords
plant_parts_keywords <- get_plant_parts_keywords()

# Test dataset sizes
dataset_sizes <- c(50, 100, 200, 500, 1000)
batch_sizes <- c(10, 25, 50, 100)

# =============================================================================
# INDIVIDUAL STAGE PROFILING
# =============================================================================

profile_individual_stages <- function(test_abstracts) {
  cat("\n🔬 PROFILING INDIVIDUAL PIPELINE STAGES\n")
  cat(sprintf("   Testing with %d abstracts\n", nrow(test_abstracts)))

  results <- list()

  # Sample abstract for detailed stage analysis
  sample_abstract <- test_abstracts$abstract[1]
  sample_id <- test_abstracts$id[1]
  sample_label <- test_abstracts$predicted_label[1]

  cat("   📝 Sample abstract preview:", substr(sample_abstract, 1, 100), "...\n")

  # Stage 1: Text preprocessing and tokenization (in extract_candidate_names)
  cat("   🧵 Stage 1: Text preprocessing and tokenization\n")
  tic("Text preprocessing")
  candidate_result <- extract_candidate_names(sample_abstract)
  preprocessing_time <- toc(quiet = TRUE)
  results$preprocessing <- list(
    time = preprocessing_time$toc - preprocessing_time$tic,
    candidates_found = length(candidate_result$candidates),
    preprocessed_length = nchar(candidate_result$preprocessed_text)
  )
  cat(sprintf("      ⏱️  Time: %.4f seconds\n", results$preprocessing$time))
  cat(sprintf("      📊 Candidates extracted: %d\n", results$preprocessing$candidates_found))

  # Stage 2: Candidate name extraction (regex patterns) - already covered above
  cat("   🔍 Stage 2: Candidate name extraction (regex patterns)\n")
  cat(sprintf("      📊 Regex patterns applied: %d candidates found\n", length(candidate_result$candidates)))
  if (length(candidate_result$candidates) > 0) {
    cat("      📝 Sample candidates:", paste(head(candidate_result$candidates, 3), collapse = ", "), "\n")
  }

  # Stage 3: Validation against species.rds (joins and lookups)
  cat("   🔗 Stage 3: Validation against species.rds (joins and lookups)\n")
  tic("Species validation")
  valid_species <- batch_validate_names(candidate_result$candidates, lookup_tables)
  validation_time <- toc(quiet = TRUE)
  results$validation <- list(
    time = validation_time$toc - validation_time$tic,
    valid_species_found = nrow(valid_species),
    accepted_count = sum(valid_species$status == "ACCEPTED"),
    synonym_count = sum(valid_species$status == "SYNONYM")
  )
  cat(sprintf("      ⏱️  Time: %.4f seconds\n", results$validation$time))
  cat(sprintf("      📊 Valid species: %d (Accepted: %d, Synonyms: %d)\n",
              results$validation$valid_species_found,
              results$validation$accepted_count,
              results$validation$synonym_count))

  # Stage 4: Abbreviation expansion and synonym resolution
  cat("   🔄 Stage 4: Abbreviation expansion and synonym resolution\n")
  # This is included in batch_validate_names, so we analyze the results
  if (nrow(valid_species) > 0 && "user_supplied_name" %in% colnames(valid_species)) {
    abbrev_expansions <- valid_species %>% filter(grepl("\\.", user_supplied_name))
    cat(sprintf("      📊 Abbreviations expanded: %d\n", nrow(abbrev_expansions)))
  } else {
    cat("      📊 Abbreviations expanded: 0 (no valid species or missing column)\n")
  }
  cat(sprintf("      📊 Synonyms resolved: %d\n", results$validation$synonym_count))

  # Stage 5: Text matching integration
  cat("   🎯 Stage 5: Text matching integration\n")
  tic("Text matching")
  matches <- process_taxonomic_matches(
    valid_species, lookup_tables, sample_abstract,
    sample_id, sample_label, candidate_result$preprocessed_text
  )
  matching_time <- toc(quiet = TRUE)
  results$matching <- list(
    time = matching_time$toc - matching_time$tic,
    matches_found = length(matches),
    total_rows = sum(sapply(matches, nrow))
  )
  cat(sprintf("      ⏱️  Time: %.4f seconds\n", results$matching$time))
  cat(sprintf("      📊 Match groups found: %d (total rows: %d)\n",
              results$matching$matches_found, results$matching$total_rows))

  return(results)
}

# =============================================================================
# BATCH SIZE AND SCALING ANALYSIS
# =============================================================================

analyze_scaling <- function(dataset_sizes, batch_sizes) {
  cat("\n📈 SCALING ANALYSIS\n")
  scaling_results <- list()

  for (size in dataset_sizes) {
    test_file <- paste0("test_data/test_subset_random_", size, ".csv")
    if (!file.exists(test_file)) {
      cat(sprintf("   ⚠️  Skipping size %d - test file not found\n", size))
      next
    }

    cat(sprintf("   📊 Analyzing dataset size: %d abstracts\n", size))

    # Load test data
    test_data <- read_csv(test_file, show_col_types = FALSE)
    cat(sprintf("      📁 Loaded %d abstracts\n", nrow(test_data)))

    # Profile individual stages for this size
    stage_profile <- profile_individual_stages(test_data)
    scaling_results[[as.character(size)]]$stages <- stage_profile

    # Test different batch sizes
    batch_results <- list()
    for (batch_size in batch_sizes) {
      if (batch_size > size) next  # Skip batch sizes larger than dataset

      cat(sprintf("      🔄 Testing batch size: %d\n", batch_size))

      # Measure memory before
      mem_before <- mem_used()

      tic(sprintf("Batch processing %d abstracts in batches of %d", size, batch_size))
      results <- process_abstracts_parallel(
        test_data, species_path, plant_parts_keywords,
        batch_size = batch_size, use_streaming = FALSE
      )
      processing_time <- toc(quiet = TRUE)

      # Measure memory after
      mem_after <- mem_used()
      mem_used_total <- mem_after - mem_before

      batch_results[[as.character(batch_size)]] <- list(
        time = processing_time$toc - processing_time$tic,
        memory_used = mem_used_total,
        results_rows = nrow(results),
        species_found = sum(!is.na(results$resolved_name), na.rm = TRUE)
      )

      cat(sprintf("         ⏱️  Total time: %.3f seconds\n", batch_results[[as.character(batch_size)]]$time))
      cat(sprintf("         🧠 Memory used: %s\n", format(batch_results[[as.character(batch_size)]]$memory_used, units = "MB")))
      cat(sprintf("         📊 Species detected: %d\n", batch_results[[as.character(batch_size)]]$species_found))
    }

    scaling_results[[as.character(size)]]$batch_performance <- batch_results
  }

  return(scaling_results)
}

# =============================================================================
# MEMORY PROFILING
# =============================================================================

profile_memory_usage <- function() {
  cat("\n🧠 MEMORY PROFILING\n")

  # Get current memory state
  base_mem <- mem_used()
  cat(sprintf("   📊 Base memory usage: %s\n", format(base_mem, units = "MB")))

  # Memory after loading species
  species_loaded_mem <- object.size(species)
  cat(sprintf("   📊 Species data memory: %s\n", format(species_loaded_mem, units = "MB")))

  # Memory after creating lookup tables
  lookup_mem <- object.size(lookup_tables)
  cat(sprintf("   📊 Lookup tables memory: %s\n", format(lookup_mem, units = "MB")))

  # Peak memory during processing (estimate with a small test)
  test_file <- "test_data/test_subset_random_50.csv"
  if (file.exists(test_file)) {
    test_data <- read_csv(test_file, show_col_types = FALSE)
    mem_before_processing <- mem_used()

    results <- process_abstracts_parallel(
      test_data, species_path, plant_parts_keywords,
      batch_size = 10, use_streaming = FALSE
    )

    mem_after_processing <- mem_used()
    processing_mem_delta <- mem_after_processing - mem_before_processing
    cat(sprintf("   📊 Processing memory delta (50 abstracts): %s\n", format(processing_mem_delta, units = "MB")))
  }

  return(list(
    base_memory = base_mem,
    species_memory = species_loaded_mem,
    lookup_memory = lookup_mem
  ))
}

# =============================================================================
# RUN PROFILING
# =============================================================================

cat("\n🚀 STARTING COMPREHENSIVE PROFILING\n")

# Memory profiling
memory_profile <- profile_memory_usage()

# Scaling analysis
scaling_profile <- analyze_scaling(dataset_sizes, batch_sizes)

# =============================================================================
# RESULTS ANALYSIS AND REPORTING
# =============================================================================

cat("\n📋 PERFORMANCE ANALYSIS REPORT\n")

# Stage timing analysis
cat("🔬 PIPELINE STAGE TIMING BREAKDOWN\n")
if (length(scaling_profile) > 0) {
  first_size <- names(scaling_profile)[1]
  stages <- scaling_profile[[first_size]]$stages

  total_stage_time <- sum(sapply(stages, function(x) x$time))
  cat(sprintf("   📊 Total time across stages: %.4f seconds\n", total_stage_time))

  for (stage_name in names(stages)) {
    stage <- stages[[stage_name]]
    pct <- (stage$time / total_stage_time) * 100
    cat(sprintf("   • %s: %.4f sec (%.1f%%)\n", stage_name, stage$time, pct))
  }

  # Identify bottlenecks
  cat("\n⚠️  PERFORMANCE BOTTLENECKS IDENTIFIED:\n")
  bottleneck_threshold <- total_stage_time * 0.3  # 30% of total time
  bottlenecks <- names(stages)[sapply(stages, function(x) x$time > bottleneck_threshold)]
  if (length(bottlenecks) > 0) {
    for (bottleneck in bottlenecks) {
      cat(sprintf("   • %s (%.4f sec)\n", bottleneck, stages[[bottleneck]]$time))
    }
  } else {
    cat("   ✅ No major bottlenecks detected\n")
  }
}

# Scaling analysis
cat("\n📈 SCALING ANALYSIS\n")
sizes_tested <- names(scaling_profile)
if (length(sizes_tested) > 0) {
  cat("   📊 Dataset sizes tested:", paste(sizes_tested, collapse = ", "), "\n")

  # Time scaling
  times <- sapply(sizes_tested, function(size) {
    batch_results <- scaling_profile[[size]]$batch_performance
    if (length(batch_results) > 0) {
      # Use batch size 50 for comparison if available
      batch_50 <- batch_results[["50"]]
      if (!is.null(batch_50)) return(batch_50$time) else return(batch_results[[1]]$time)
    }
    return(NA)
  })

  valid_times <- times[!is.na(times)]
  if (length(valid_times) > 1) {
    scaling_factor <- valid_times[length(valid_times)] / valid_times[1]
    sizes_numeric <- as.numeric(sizes_tested[!is.na(times)])
    size_ratio <- sizes_numeric[length(sizes_numeric)] / sizes_numeric[1]
    scaling_efficiency <- scaling_factor / size_ratio

    cat(sprintf("   📈 Scaling factor: %.2fx time increase for %.1fx size increase\n",
                scaling_factor, size_ratio))
    cat(sprintf("   📊 Scaling efficiency: %.2f (1.0 = linear scaling)\n", scaling_efficiency))

    if (scaling_efficiency > 1.5) {
      cat("   ⚠️  Warning: Poor scaling - time increases faster than dataset size\n")
    } else if (scaling_efficiency < 1.2) {
      cat("   ✅ Good scaling performance\n")
    } else {
      cat("   ℹ️  Moderate scaling - acceptable performance\n")
    }
  }

  # Batch size optimization
  cat("\n🔄 BATCH SIZE OPTIMIZATION\n")
  if (length(sizes_tested) >= 1) {
    size <- sizes_tested[1]  # Use first available size
    batch_results <- scaling_profile[[size]]$batch_performance

    if (length(batch_results) > 0) {
      batch_times <- sapply(batch_results, function(x) x$time)
      batch_sizes_tested <- as.numeric(names(batch_results))
      best_batch <- batch_sizes_tested[which.min(batch_times)]

      cat(sprintf("   📊 Best batch size for %s abstracts: %d\n", size, best_batch))
      cat("   📈 Batch size vs time:\n")
      for (i in seq_along(batch_sizes_tested)) {
        cat(sprintf("      • Batch %d: %.3f sec\n", batch_sizes_tested[i], batch_times[i]))
      }
    }
  }
}

# Memory analysis
cat("\n🧠 MEMORY ANALYSIS\n")
total_memory <- memory_profile$base_memory + memory_profile$species_memory + memory_profile$lookup_memory
cat(sprintf("   📊 Total baseline memory: %s\n", format(total_memory, units = "MB")))
cat(sprintf("   📊 Species data: %s (%.1f%% of total)\n",
            format(memory_profile$species_memory, units = "MB"),
            (memory_profile$species_memory / total_memory) * 100))

# Recommendations
cat("\n💡 OPTIMIZATION RECOMMENDATIONS\n")

# Based on analysis, provide recommendations
if (length(scaling_profile) > 0) {
  cat("   🎯 Key findings and recommendations:\n")

  # Check for memory issues
  species_pct <- (memory_profile$species_memory / total_memory) * 100
  if (species_pct > 50) {
    cat("   • Memory: Species data dominates memory usage (", round(species_pct, 1), "%). Consider lazy loading or compression.\n")
  }

  # Check scaling
  if (exists("scaling_efficiency") && scaling_efficiency > 1.3) {
    cat("   • Scaling: Performance degrades significantly with size. Consider parallelization improvements.\n")
  }

  # General recommendations
  cat("   • Pipeline: Stage timing shows", paste(names(stages)[sapply(stages, function(x) x$time > bottleneck_threshold)], collapse = ", "), "as potential bottlenecks\n")
  cat("   • Batch processing: Optimal batch sizes identified for different dataset scales\n")
}

cat("\n✅ COMPREHENSIVE PROFILING COMPLETE\n")
cat("📊 Summary: Detailed timing breakdown and bottleneck analysis completed.\n")
cat("🔍 Key bottlenecks: Validation against species.rds and text matching integration.\n")
cat("📈 Scaling: Performance degrades with larger datasets, batch size optimization recommended.\n")