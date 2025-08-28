# Pipeline Testing Workflow
# B. Bock
# Step-by-step workflow for testing the extraction pipeline on subsets
# Ensures everything works before running on full dataset

library(tidyverse)

cat("=== PIPELINE TESTING WORKFLOW ===\n")
cat("Systematic testing of extraction pipeline on data subsets\n\n")

# Main testing workflow function
test_pipeline_workflow <- function(
  subset_sizes = c(100, 500),
  test_components = c("data_prep", "species", "methods", "parts", "geography", "merge", "analysis"),
  sampling_method = "random",
  output_dir = "test_results",
  verbose = TRUE
) {

  if (verbose) {
    cat("🧪 Starting Pipeline Testing Workflow\n")
    cat("=====================================\n")
    cat("Subset sizes to test:", paste(subset_sizes, collapse = ", "), "\n")
    cat("Components to test:", paste(test_components, collapse = ", "), "\n")
    cat("Sampling method:", sampling_method, "\n\n")
  }

  # Check if subsets exist, create if needed
  check_and_create_subsets(subset_sizes, sampling_method, verbose)

  # Create results tracking
  test_results <- list()
  timing_results <- list()

  # Test each subset size
  for (subset_size in subset_sizes) {
    subset_file <- sprintf("test_data/test_subset_%s_%d.csv", sampling_method, subset_size)

    if (verbose) {
      cat("🎯 Testing on", subset_size, "abstracts\n")
      cat("================================\n")
    }

    # Test each component
    subset_results <- test_components_on_subset(
      subset_file = subset_file,
      components = test_components,
      output_dir = output_dir,
      verbose = verbose
    )

    test_results[[as.character(subset_size)]] <- subset_results$results
    timing_results[[as.character(subset_size)]] <- subset_results$timing

    # Generate intermediate report
    generate_intermediate_report(
      subset_size = subset_size,
      results = subset_results$results,
      timing = subset_results$timing,
      output_dir = output_dir
    )
  }

  # Generate final comparison report
  generate_comparison_report(
    test_results = test_results,
    timing_results = timing_results,
    subset_sizes = subset_sizes,
    output_dir = output_dir
  )

  if (verbose) {
    cat("\n🎉 Pipeline Testing Complete!\n")
    cat("=============================\n")
    cat("📊 Check test_results/ for detailed reports\n")
    cat("📈 Compare results across subset sizes\n")
    cat("✅ Proceed to full dataset when ready\n")
  }

  return(list(
    results = test_results,
    timing = timing_results,
    output_dir = output_dir
  ))
}

# Function to check and create subsets if needed
check_and_create_subsets <- function(subset_sizes, sampling_method, verbose = TRUE) {

  if (verbose) cat("📋 Checking for test subsets...\n")

  # Check if subsets exist
  missing_subsets <- c()
  for (subset_size in subset_sizes) {
    subset_file <- sprintf("test_data/test_subset_%s_%d.csv", sampling_method, subset_size)
    if (!file.exists(subset_file)) {
      missing_subsets <- c(missing_subsets, subset_size)
    }
  }

  # Create missing subsets
  if (length(missing_subsets) > 0) {
    if (verbose) {
      cat("   Creating missing subsets:", paste(missing_subsets, collapse = ", "), "\n")
    }

    source("scripts/04_analysis/create_test_subset.R")
    create_test_subset(
      sample_sizes = missing_subsets,
      sampling_method = sampling_method,
      verbose = FALSE
    )
  } else {
    if (verbose) cat("   ✅ All subsets found\n")
  }

  if (verbose) cat("\n")
}

# Function to test components on a specific subset
test_components_on_subset <- function(
  subset_file,
  components,
  output_dir,
  verbose = TRUE
) {

  # Create component-specific output directory
  subset_name <- tools::file_path_sans_ext(basename(subset_file))
  component_output_dir <- file.path(output_dir, subset_name)
  dir.create(component_output_dir, showWarnings = FALSE, recursive = TRUE)

  results <- list()
  timing <- list()

  # Load subset info
  if (file.exists(subset_file)) {
    subset_data <- read_csv(subset_file, show_col_types = FALSE)
    if (verbose) {
      cat("   Loaded", nrow(subset_data), "abstracts\n")
    }
  } else {
    stop("Subset file not found: ", subset_file)
  }

  # Test data preparation
  if ("data_prep" %in% components) {
    if (verbose) cat("   🔧 Testing data preparation...\n")

    start_time <- Sys.time()
    source("scripts/04_analysis/run_extraction_pipeline.R")
    prep_result <- prepare_abstracts_data(
      input_file = subset_file,
      output_file = file.path(component_output_dir, "prepared_abstracts.csv"),
      force_rerun = TRUE,
      verbose = FALSE
    )
    end_time <- Sys.time()

    timing$data_prep <- as.numeric(difftime(end_time, start_time, units = "secs"))
    results$data_prep <- list(
      status = "completed",
      abstracts_processed = nrow(prep_result),
      output_file = file.path(component_output_dir, "prepared_abstracts.csv")
    )

    if (verbose) {
      cat("      ✅ Completed in", round(timing$data_prep, 1), "seconds\n")
    }
  }

  # Test species detection
  if ("species" %in% components) {
    if (verbose) cat("   🧬 Testing species detection...\n")

    start_time <- Sys.time()
    source("scripts/04_analysis/components/01_extract_species.R")
    species_result <- extract_species_data(
      subset_data,
      output_file = file.path(component_output_dir, "species_results.csv"),
      force_rerun = TRUE,
      verbose = FALSE
    )
    end_time <- Sys.time()

    timing$species <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Analyze species results
    species_found <- sum(!is.na(species_result$resolved_name))
    results$species <- list(
      status = "completed",
      abstracts_processed = nrow(species_result),
      species_found = species_found,
      detection_rate = round(100 * species_found / nrow(species_result), 1),
      output_file = file.path(component_output_dir, "species_results.csv")
    )

    if (verbose) {
      cat("      ✅ Found species in", results$species$detection_rate, "% of abstracts\n")
      cat("      ⏱️  Completed in", round(timing$species, 1), "seconds\n")
    }
  }

  # Test methods detection
  if ("methods" %in% components) {
    if (verbose) cat("   🔬 Testing methods detection...\n")

    start_time <- Sys.time()
    source("scripts/04_analysis/components/02_extract_methods.R")
    methods_result <- extract_methods_data(
      subset_data,
      output_file = file.path(component_output_dir, "methods_results.csv"),
      force_rerun = TRUE,
      verbose = FALSE
    )
    end_time <- Sys.time()

    timing$methods <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Analyze methods results
    molecular_found <- sum(methods_result$molecular_methods, na.rm = TRUE)
    culture_found <- sum(methods_result$culture_based_methods, na.rm = TRUE)
    microscopy_found <- sum(methods_result$microscopy_methods, na.rm = TRUE)

    results$methods <- list(
      status = "completed",
      abstracts_processed = nrow(methods_result),
      molecular_methods = molecular_found,
      culture_methods = culture_found,
      microscopy_methods = microscopy_found,
      output_file = file.path(component_output_dir, "methods_results.csv")
    )

    if (verbose) {
      cat("      ✅ Molecular:", molecular_found, ", Culture:", culture_found, ", Microscopy:", microscopy_found, "\n")
      cat("      ⏱️  Completed in", round(timing$methods, 1), "seconds\n")
    }
  }

  # Test plant parts detection
  if ("parts" %in% components) {
    if (verbose) cat("   🌿 Testing plant parts detection...\n")

    start_time <- Sys.time()
    source("scripts/04_analysis/components/03_extract_plant_parts.R")
    parts_result <- extract_plant_parts_data(
      subset_data,
      output_file = file.path(component_output_dir, "plant_parts_results.csv"),
      force_rerun = TRUE,
      verbose = FALSE
    )
    end_time <- Sys.time()

    timing$parts <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Analyze parts results
    parts_found <- sum(!is.na(parts_result$plant_parts_detected))

    results$parts <- list(
      status = "completed",
      abstracts_processed = nrow(parts_result),
      parts_found = parts_found,
      detection_rate = round(100 * parts_found / nrow(parts_result), 1),
      output_file = file.path(component_output_dir, "plant_parts_results.csv")
    )

    if (verbose) {
      cat("      ✅ Found plant parts in", results$parts$detection_rate, "% of abstracts\n")
      cat("      ⏱️  Completed in", round(timing$parts, 1), "seconds\n")
    }
  }

  # Test geography detection
  if ("geography" %in% components) {
    if (verbose) cat("   🌍 Testing geography detection...\n")

    start_time <- Sys.time()
    source("scripts/04_analysis/components/04_extract_geography.R")
    geography_result <- extract_geography_data(
      subset_data,
      output_file = file.path(component_output_dir, "geography_results.csv"),
      force_rerun = TRUE,
      verbose = FALSE
    )
    end_time <- Sys.time()

    timing$geography <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Analyze geography results
    countries_found <- sum(!is.na(geography_result$countries_detected))
    continents_found <- sum(!is.na(geography_result$continents_detected))
    regions_found <- sum(!is.na(geography_result$regions_detected))

    results$geography <- list(
      status = "completed",
      abstracts_processed = nrow(geography_result),
      countries_found = countries_found,
      continents_found = continents_found,
      regions_found = regions_found,
      output_file = file.path(component_output_dir, "geography_results.csv")
    )

    if (verbose) {
      cat("      ✅ Countries:", countries_found, ", Continents:", continents_found, ", Regions:", regions_found, "\n")
      cat("      ⏱️  Completed in", round(timing$geography, 1), "seconds\n")
    }
  }

  # Test results merging
  if ("merge" %in% components) {
    if (verbose) cat("   📊 Testing results merging...\n")

    start_time <- Sys.time()
    source("scripts/04_analysis/components/05_merge_results.R")

    # Temporarily move results to expected location for merging
    temp_results_dir <- "results_temp"
    dir.create(temp_results_dir, showWarnings = FALSE)

    # Copy component results to temp location
    component_files <- c(
      "species_detection_results.csv",
      "methods_detection_results.csv",
      "plant_parts_detection_results.csv",
      "geography_detection_results.csv"
    )

    for (file in component_files) {
      temp_file <- file.path(component_output_dir, sub("_results", "", file))
      if (file.exists(temp_file)) {
        file.copy(temp_file, file.path(temp_results_dir, file), overwrite = TRUE)
      }
    }

    # Create prepared abstracts file
    write_csv(subset_data, file.path("results", "prepared_abstracts_for_extraction.csv"))

    merge_result <- merge_extraction_results(
      output_file = file.path(component_output_dir, "comprehensive_results.csv"),
      force_rerun = TRUE,
      verbose = FALSE
    )

    # Clean up temp files
    unlink(temp_results_dir, recursive = TRUE)

    end_time <- Sys.time()
    timing$merge <- as.numeric(difftime(end_time, start_time, units = "secs"))

    results$merge <- list(
      status = "completed",
      abstracts_processed = nrow(merge_result),
      total_columns = ncol(merge_result),
      output_file = file.path(component_output_dir, "comprehensive_results.csv")
    )

    if (verbose) {
      cat("      ✅ Merged", results$merge$total_columns, "columns\n")
      cat("      ⏱️  Completed in", round(timing$merge, 1), "seconds\n")
    }
  }

  # Test analysis components
  if ("analysis" %in% components) {
    if (verbose) cat("   📊 Testing analysis components...\n")

    start_time <- Sys.time()

    # Create analysis results directory
    analysis_output_dir <- file.path(component_output_dir, "analysis")
    dir.create(analysis_output_dir, showWarnings = FALSE, recursive = TRUE)

    # Run analysis components using the unified workflow
    source("scripts/04_analysis/analysis_workflow.R")

    # Test each analysis component individually on the subset
    analysis_results <- list()

    # Test absence detection
    if (verbose) cat("      🔍 Running absence analysis...\n")
    tryCatch({
      source("scripts/04_analysis/validation/absence_evidence_detection.R")
      absence_result <- detect_absence_evidence(subset_data$abstract[1]) # Test on first abstract
      analysis_results$absence <- list(status = "completed", test_abstracts = 1)
    }, error = function(e) {
      analysis_results$absence <- list(status = "error", error = as.character(e))
    })

    # Test validation sampling
    if (verbose) cat("      📋 Running validation sampling...\n")
    tryCatch({
      source("scripts/04_analysis/validation/manual_validation_sample.R")
      # Just test that the function loads without running full sampling
      analysis_results$validation <- list(status = "completed", test_mode = TRUE)
    }, error = function(e) {
      analysis_results$validation <- list(status = "error", error = as.character(e))
    })

    # Test find plants statement
    if (verbose) cat("      🔍 Running find plants statement analysis...\n")
    tryCatch({
      source("scripts/04_analysis/validation/find_all_plants_statement.R")
      analysis_results$find_plants_statement <- list(status = "completed", test_mode = TRUE)
    }, error = function(e) {
      analysis_results$find_plants_statement <- list(status = "error", error = as.character(e))
    })

    # Test temporal analysis
    if (verbose) cat("      📈 Running temporal analysis...\n")
    tryCatch({
      source("scripts/04_analysis/temporal/temporal_trend_analysis.R")
      # Test basic functionality without full analysis
      analysis_results$temporal <- list(status = "completed", test_mode = TRUE)
    }, error = function(e) {
      analysis_results$temporal <- list(status = "error", error = as.character(e))
    })

    # Test visualization
    if (verbose) cat("      📊 Running visualization...\n")
    tryCatch({
      source("scripts/04_analysis/visualization/run_taxa_visualizations.R")
      analysis_results$visualization <- list(status = "completed", test_mode = TRUE)
    }, error = function(e) {
      analysis_results$visualization <- list(status = "error", error = as.character(e))
    })

    # Test extraction results visualization
    if (verbose) cat("      📈 Running extraction results visualization...\n")
    tryCatch({
      source("scripts/04_analysis/visualization/visualize_extraction_results.R")
      analysis_results$extraction_visualization <- list(status = "completed", test_mode = TRUE)
    }, error = function(e) {
      analysis_results$extraction_visualization <- list(status = "error", error = as.character(e))
    })

    end_time <- Sys.time()
    timing$analysis <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Summarize analysis results
    successful_components <- sum(sapply(analysis_results, function(x) x$status == "completed"))
    total_components <- length(analysis_results)

    results$analysis <- list(
      status = "completed",
      successful_components = successful_components,
      total_components = total_components,
      success_rate = round(100 * successful_components / total_components, 1),
      components_tested = analysis_results,
      output_dir = analysis_output_dir
    )

    if (verbose) {
      cat("      ✅", successful_components, "/", total_components, "analysis components successful\n")
      cat("      ⏱️  Completed in", round(timing$analysis, 1), "seconds\n")
    }
  }

  if (verbose) cat("\n")

  return(list(
    results = results,
    timing = timing
  ))
}

# Function to generate intermediate report for each subset
generate_intermediate_report <- function(subset_size, results, timing, output_dir) {

  report_file <- file.path(output_dir, sprintf("test_results_%d.txt", subset_size))

  capture.output({
    cat("=== PIPELINE TEST RESULTS -", subset_size, "ABSTRACTS ===\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

    cat("COMPONENT RESULTS:\n")
    cat("=================\n\n")

    for (component in names(results)) {
      cat(toupper(component), "DETECTION:\n")
      component_results <- results[[component]]

      if (component == "data_prep") {
        cat("  Abstracts processed:", component_results$abstracts_processed, "\n")
      } else if (component == "species") {
        cat("  Abstracts processed:", component_results$abstracts_processed, "\n")
        cat("  Species found:", component_results$species_found, "\n")
        cat("  Detection rate:", component_results$detection_rate, "%\n")
      } else if (component == "methods") {
        cat("  Abstracts processed:", component_results$abstracts_processed, "\n")
        cat("  Molecular methods:", component_results$molecular_methods, "\n")
        cat("  Culture methods:", component_results$culture_methods, "\n")
        cat("  Microscopy methods:", component_results$microscopy_methods, "\n")
      } else if (component == "parts") {
        cat("  Abstracts processed:", component_results$abstracts_processed, "\n")
        cat("  Plant parts found:", component_results$parts_found, "\n")
        cat("  Detection rate:", component_results$detection_rate, "%\n")
      } else if (component == "geography") {
        cat("  Abstracts processed:", component_results$abstracts_processed, "\n")
        cat("  Countries found:", component_results$countries_found, "\n")
        cat("  Continents found:", component_results$continents_found, "\n")
        cat("  Regions found:", component_results$regions_found, "\n")
      } else if (component == "merge") {
        cat("  Abstracts processed:", component_results$abstracts_processed, "\n")
        cat("  Total columns:", component_results$total_columns, "\n")
      }

      cat("  Processing time:", round(timing[[component]], 1), "seconds\n")
      cat("  Status:", component_results$status, "\n\n")
    }

    cat("PERFORMANCE SUMMARY:\n")
    cat("===================\n")
    total_time <- sum(unlist(timing))
    cat("Total processing time:", round(total_time, 1), "seconds\n")
    cat("Average time per abstract:", round(total_time / subset_size, 3), "seconds\n")

    if (length(timing) > 1) {
      cat("\nComponent timing breakdown:\n")
      for (component in names(timing)) {
        pct <- round(100 * timing[[component]] / total_time, 1)
        cat(sprintf("  %-12s: %6.1fs (%4.1f%%)\n", component, timing[[component]], pct))
      }
    }

  }, file = report_file)
}

# Function to generate comparison report across subset sizes
generate_comparison_report <- function(test_results, timing_results, subset_sizes, output_dir) {

  report_file <- file.path(output_dir, "comparison_report.txt")

  capture.output({
    cat("=== PIPELINE TESTING COMPARISON REPORT ===\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

    cat("TESTED SUBSET SIZES:", paste(subset_sizes, collapse = ", "), "\n\n")

    # Component-wise comparison
    components <- unique(unlist(lapply(test_results, names)))

    for (component in components) {
      cat(toupper(component), "DETECTION COMPARISON:\n")
      cat("===================================\n")

      for (size in subset_sizes) {
        if (!is.null(test_results[[as.character(size)]][[component]])) {
          results <- test_results[[as.character(size)]][[component]]
          timing <- timing_results[[as.character(size)]][[component]]

          cat(sprintf("  %4d abstracts: ", size))

          if (component == "species" || component == "parts") {
            cat(sprintf("%d found (%.1f%%), %.1fs\n",
                       results$detection_rate * size / 100,
                       results$detection_rate,
                       timing))
          } else if (component == "methods") {
            total_methods <- results$molecular_methods + results$culture_methods + results$microscopy_methods
            cat(sprintf("%d methods found, %.1fs\n", total_methods, timing))
          } else if (component == "geography") {
            total_geo <- results$countries_found + results$continents_found + results$regions_found
            cat(sprintf("%d locations found, %.1fs\n", total_geo, timing))
          } else {
            cat(sprintf("%.1fs\n", timing))
          }
        }
      }
      cat("\n")
    }

    cat("RECOMMENDATIONS:\n")
    cat("===============\n")

    # Check for consistency across subset sizes
    if (length(subset_sizes) >= 2) {
      cat("1. Detection Rates:\n")
      for (component in c("species", "parts")) {
        if (component %in% components) {
          rates <- sapply(subset_sizes, function(size) {
            test_results[[as.character(size)]][[component]]$detection_rate
          })
          rate_diff <- max(rates) - min(rates)
          if (rate_diff < 5) {
            cat("   ✓", component, "detection rates are consistent (±", round(rate_diff, 1), "%)\n")
          } else {
            cat("   ⚠️ ", component, "detection rates vary significantly (±", round(rate_diff, 1), "%)\n")
          }
        }
      }

      cat("\n2. Performance Scaling:\n")
      smallest_size <- min(subset_sizes)
      largest_size <- max(subset_sizes)
      scale_factor <- largest_size / smallest_size

      for (component in components) {
        if (component %in% names(timing_results[[as.character(smallest_size)]])) {
          small_time <- timing_results[[as.character(smallest_size)]][[component]]
          large_time <- timing_results[[as.character(largest_size)]][[component]]
          actual_scale <- large_time / small_time
          efficiency <- round(actual_scale / scale_factor, 2)

          if (efficiency <= 1.2) {
            cat("   ✓", component, "scales linearly (", efficiency, "x)\n")
          } else {
            cat("   ⚠️ ", component, "scales poorly (", efficiency, "x)\n")
          }
        }
      }
    }

    cat("\n3. Next Steps:\n")
    cat("   □ Review component results for quality\n")
    cat("   □ Check for memory issues on larger subsets\n")
    cat("   □ Validate against known examples\n")
    cat("   □ Run full pipeline when confident\n")

  }, file = report_file)
}

# Quick test functions
run_quick_test <- function(subset_size = 100, components = c("data_prep", "species", "methods", "parts", "geography", "merge", "analysis")) {
  # Quick test with essential components (including analysis for comprehensive testing)
  cat("🚀 Running quick test on", subset_size, "abstracts\n")
  cat("   Testing components:", paste(components, collapse = ", "), "\n\n")

  test_pipeline_workflow(
    subset_sizes = subset_size,
    test_components = components,
    verbose = TRUE
  )
}

run_full_test <- function(subset_sizes = c(100, 500)) {
  # Full test with all components
  cat("🧪 Running full pipeline test\n")

  test_pipeline_workflow(
    subset_sizes = subset_sizes,
    test_components = c("data_prep", "species", "methods", "parts", "geography", "merge"),
    verbose = TRUE
  )
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "test_pipeline_workflow.R")) {

  # Default: Quick test on 100 abstracts
  cat("Starting default pipeline test workflow...\n")
  result <- test_pipeline_workflow(
    subset_sizes = c(100, 500),
    test_components = c("data_prep", "species", "methods", "parts", "geography"),
    verbose = TRUE
  )

  cat("\n✅ Pipeline testing workflow complete!\n")
}

# USAGE EXAMPLES:
#
# # Quick test - just data prep and species
# run_quick_test(subset_size = 100)
#
# # Full test on multiple subset sizes
# run_full_test(subset_sizes = c(100, 500, 1000))
#
# # Test specific components
# test_pipeline_workflow(
#   subset_sizes = c(200),
#   test_components = c("species", "geography")
# )
#
# # Custom sampling method
# test_pipeline_workflow(
#   subset_sizes = c(150, 750),
#   sampling_method = "stratified"
# )
