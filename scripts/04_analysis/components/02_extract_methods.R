# =============================================================================
# 02_extract_methods.R - Research methods detection component
# =============================================================================
#
# Purpose: Detect comprehensive research methods from abstracts using keyword matching
#
# Description: Script that detects research methods specific to fungal endophyte studies including:
# molecular, culture-based, microscopy, inoculation, plant-microbe interactions, bioactivity assays,
# physiological assays, ecological studies, and surface sterilization methods. Uses optimized
# vectorized functions for efficient keyword matching and batch processing. Part of the memory-efficient
# extraction pipeline that minimizes data duplication by only outputting id + methods columns.
#
# Dependencies: tidyverse, stringr, progress; scripts/04_analysis/utilities/reference_data_utils.R
#
# Author: B. Bock
# Date: 2024-09-26
#
# Inputs/Outputs: Reads consolidated dataset from results/consolidated_dataset.csv;
# outputs methods detection results to results/methods_detection_results.csv
#
# =============================================================================

library(tidyverse)
library(stringr)
library(progress)

# Source utilities
source("scripts/04_analysis/utilities/reference_data_utils.R")

cat("=== METHODS DETECTION COMPONENT ===\n")
cat("Extracting research methods information\n\n")

# Optimized vectorized function to detect comprehensive research methods
detect_research_methods_batch <- function(text_vector) {
  method_categories <- get_method_keywords()

  # Pre-compile patterns for better performance
  patterns <- purrr::map(method_categories, ~paste(., collapse = "|"))

  text_lower <- str_to_lower(text_vector)

  results <- purrr::map_dfc(names(patterns), function(category) {
    matches <- str_detect(text_lower, patterns[[category]])
    setNames(list(matches), category)
  })

  # Create methods summary with all detected methods
  # Note: unique() ensures each method category is listed only once per abstract
  methods_detected <- purrr::pmap_chr(results, function(...) {
    found <- unique(names(list(...))[unlist(list(...))])
    if(length(found) > 0) paste(found, collapse = "; ") else NA_character_
  })

  return(results %>%
    rename(
      molecular_methods = molecular,
      culture_based_methods = culture_based,
      microscopy_methods = microscopy,
      inoculation_methods = inoculation,
      plant_microbe_interaction_methods = plant_microbe_interaction,
      bioactivity_assays_methods = bioactivity_assays,
      physiological_assays_methods = physiological_assays,
      ecological_studies_methods = ecological_studies,
      surface_sterilization_methods = surface_sterilization
    ) %>%
    mutate(methods_summary = methods_detected))
}

# Main methods extraction function
extract_methods_data <- function(
  abstracts_data,
  output_file = "results/methods_detection_results.csv",
  batch_size = 1000,
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Recovery mechanism
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing methods detection results\n")
    existing_results <- read_csv(output_file, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
    return(existing_results)
  }

  if (verbose) cat("🔬 Starting methods detection for", nrow(abstracts_data), "abstracts\n")

  # Handle missing abstracts
  abstracts_text <- ifelse(is.na(abstracts_data$abstract), "", abstracts_data$abstract)

  # Keep only id and abstract columns for memory efficiency
  abstracts_data <- abstracts_data %>%
    select(id, abstract)

  # Process in batches for memory efficiency
  n_batches <- ceiling(length(abstracts_text) / batch_size)

  if (verbose) {
    cat("📊 Processing", length(abstracts_text), "abstracts in", n_batches, "batches\n")
    cat("⚙️  Batch size:", batch_size, "abstracts per batch\n")
    cat("🕐 Started at", format(Sys.time(), "%H:%M:%S"), "\n\n")
  }

  # Initialize progress bar
  if (verbose) {
    pb <- progress_bar$new(
      format = "Methods [:bar] :percent | ETA: :eta | :current/:total batches",
      total = n_batches,
      clear = FALSE
    )
  }

  methods_results <- map_dfr(1:n_batches, function(batch_num) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, length(abstracts_text))

    batch_text <- abstracts_text[start_idx:end_idx]
    batch_ids <- abstracts_data$id[start_idx:end_idx]

    if (verbose) {
      pb$tick()
    } else {
      cat("   🔬 Batch", batch_num, "of", n_batches, "(", length(batch_text), "abstracts)\n")
    }

    # Detect methods
    methods <- detect_research_methods_batch(batch_text)

    # Quick summary for all method categories
    molecular_found <- sum(methods$molecular_methods, na.rm = TRUE)
    culture_found <- sum(methods$culture_based_methods, na.rm = TRUE)
    microscopy_found <- sum(methods$microscopy_methods, na.rm = TRUE)
    inoculation_found <- sum(methods$inoculation_methods, na.rm = TRUE)
    interaction_found <- sum(methods$plant_microbe_interaction_methods, na.rm = TRUE)
    bioactivity_found <- sum(methods$bioactivity_assays_methods, na.rm = TRUE)
    physiological_found <- sum(methods$physiological_assays_methods, na.rm = TRUE)
    ecological_found <- sum(methods$ecological_studies_methods, na.rm = TRUE)
    sterilization_found <- sum(methods$surface_sterilization_methods, na.rm = TRUE)

    if (verbose && batch_num %% 10 == 0) {
      cat("      Methods found - Mol:", molecular_found, "Cult:", culture_found, "Micro:", microscopy_found,
          "Inoc:", inoculation_found, "Int:", interaction_found, "Bio:", bioactivity_found,
          "Phys:", physiological_found, "Eco:", ecological_found, "Ster:", sterilization_found, "\n")
    }

    # Combine with IDs
    bind_cols(
      tibble(id = batch_ids),
      methods
    )
  })

  # Join methods results with id and abstract only
  full_results <- bind_cols(
    select(abstracts_data, id, abstract),
    methods_results %>% select(-id)
  )

  # Save results with only id + methods columns for memory efficiency
  write_csv(full_results, output_file)

  # Comprehensive summary statistics
  total_abstracts <- nrow(full_results)
  molecular_total <- sum(full_results$molecular_methods, na.rm = TRUE)
  culture_total <- sum(full_results$culture_based_methods, na.rm = TRUE)
  microscopy_total <- sum(full_results$microscopy_methods, na.rm = TRUE)
  inoculation_total <- sum(full_results$inoculation_methods, na.rm = TRUE)
  interaction_total <- sum(full_results$plant_microbe_interaction_methods, na.rm = TRUE)
  bioactivity_total <- sum(full_results$bioactivity_assays_methods, na.rm = TRUE)
  physiological_total <- sum(full_results$physiological_assays_methods, na.rm = TRUE)
  ecological_total <- sum(full_results$ecological_studies_methods, na.rm = TRUE)
  sterilization_total <- sum(full_results$surface_sterilization_methods, na.rm = TRUE)
  any_methods <- sum(!is.na(full_results$methods_summary))

  if (verbose) {
    cat("\n🎉 Comprehensive methods detection completed!\n")
    cat("📈 Results Summary:\n")
    cat("   - Total abstracts processed:", total_abstracts, "\n")
    cat("   - Molecular methods:", molecular_total,
        "(", round(100 * molecular_total / total_abstracts, 1), "%)\n")
    cat("   - Culture-based methods:", culture_total,
        "(", round(100 * culture_total / total_abstracts, 1), "%)\n")
    cat("   - Microscopy methods:", microscopy_total,
        "(", round(100 * microscopy_total / total_abstracts, 1), "%)\n")
    cat("   - Inoculation methods:", inoculation_total,
        "(", round(100 * inoculation_total / total_abstracts, 1), "%)\n")
    cat("   - Plant-microbe interactions:", interaction_total,
        "(", round(100 * interaction_total / total_abstracts, 1), "%)\n")
    cat("   - Bioactivity assays:", bioactivity_total,
        "(", round(100 * bioactivity_total / total_abstracts, 1), "%)\n")
    cat("   - Physiological assays:", physiological_total,
        "(", round(100 * physiological_total / total_abstracts, 1), "%)\n")
    cat("   - Ecological studies:", ecological_total,
        "(", round(100 * ecological_total / total_abstracts, 1), "%)\n")
    cat("   - Surface sterilization:", sterilization_total,
        "(", round(100 * sterilization_total / total_abstracts, 1), "%)\n")
    cat("   - Any method information:", any_methods,
        "(", round(100 * any_methods / total_abstracts, 1), "%)\n")
    cat("💾 Results saved to:", output_file, "\n")
  }

  return(full_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "02_extract_methods.R")) {

  # Load consolidated dataset (contains abstracts and metadata)
  input_file <- "results/consolidated_dataset.csv"
  if (!file.exists(input_file)) {
    stop("❌ Consolidated dataset not found. Run the consolidation script first.")
  }

  cat("📖 Loading consolidated dataset from:", input_file, "\n")
  abstracts_data <- read_csv(input_file, show_col_types = FALSE)

  # Check if we have the required columns
  required_cols <- c("id", "abstract")
  missing_cols <- setdiff(required_cols, colnames(abstracts_data))
  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns in input data:", paste(missing_cols, collapse = ", "))
  }

  # Extract methods
  methods_results <- extract_methods_data(abstracts_data)

  cat("\n✅ Methods extraction component completed!\n")
}
