# =============================================================================
# 03_extract_plant_parts.R - Enhanced plant parts detection component
# =============================================================================
#
# Purpose: Detect plant parts studied from abstracts with comprehensive singular/plural grouping
# and context-aware detection
#
# Description: Enhanced script that detects plant parts (roots, leaves, stems, etc.) using:
# - Comprehensive singular/plural normalization for consistent grouping
# - Compound term recognition (e.g., "root tips", "leaf blades", "vascular bundles")
# - Batch processing for efficient text analysis
# Part of the memory-efficient extraction pipeline that minimizes data duplication by only outputting id + plant parts columns.
#
# Dependencies: tidyverse, stringr, progress; scripts/04_analysis/utilities/reference_data_utils.R
#
# Author: B. Bock
# Date: 2024-09-26
#
# Inputs/Outputs: Reads consolidated dataset from results/consolidated_dataset.csv;
# outputs enhanced plant parts detection results to results/plant_parts_detection_results.csv
#
# =============================================================================

library(tidyverse)
library(stringr)
library(progress)

# Source utilities
source("scripts/04_analysis/utilities/reference_data_utils.R")

cat("=== PLANT PARTS DETECTION COMPONENT ===\n")
cat("Extracting plant parts information\n\n")

# Function to detect compound plant part terms
detect_compound_plant_parts <- function(text_vector) {
  compound_patterns <- c(
    # Root-related compounds
    "root tip", "root tips", "root hair", "root hairs", "root cap", "root caps",
    "root apical meristem", "root nodule", "root nodules", "tap root", "tap roots",
    "fibrous root", "fibrous roots", "lateral root", "lateral roots", "adventitious root",
    "adventitious roots", "aerial root", "aerial roots", "prop root", "prop roots",
    "pneumatophore", "pneumatophores",

    # Leaf-related compounds
    "leaf blade", "leaf blades", "leaf margin", "leaf margins", "leaf tip", "leaf tips",
    "leaf base", "leaf bases", "leaf sheath", "leaf sheaths", "leaf surface", "leaf surfaces",
    "leaf vein", "leaf veins", "leaf trichome", "leaf trichomes", "guard cell", "guard cells",
    "stomatal", "stomata", "mesophyll", "palisade mesophyll", "spongy mesophyll",

    # Stem-related compounds
    "stem internode", "stem internodes", "stem node", "stem nodes", "stem bark", "stem barks",
    "vascular bundle", "vascular bundles", "bundle sheath", "bundle sheaths",
    "cambial zone", "cambial zones", "axillary bud", "axillary buds", "terminal bud",
    "terminal buds", "lateral bud", "lateral buds",

    # Flower-related compounds
    "flower bud", "flower buds", "floral organ", "floral organs", "petal", "petals",
    "sepal", "sepals", "stamen", "stamens", "pistil", "pistils", "anther", "anthers",
    "ovary", "ovaries", "ovule", "ovules", "style", "styles", "stigma", "stigmas",

    # Seed and fruit compounds
    "seed coat", "seed coats", "seed embryo", "seed embryos", "fruit pericarp",
    "fruit pericarps", "fruit tissue", "fruit tissues", "pericarp", "pericarps",
    "endosperm", "endosperms", "cotyledon", "cotyledons",

    # Tissue and cellular compounds
    "vascular tissue", "vascular tissues", "meristematic tissue", "meristematic tissues",
    "ground tissue", "ground tissues", "epidermal tissue", "epidermal tissues",
    "xylem vessel", "xylem vessels", "phloem sieve", "phloem sieves", "sieve tube",
    "sieve tubes", "companion cell", "companion cells", "cell wall", "cell walls",
    "middle lamella", "plasmodesma", "plasmodesmata", "intercellular space",
    "intercellular spaces",

    # Specialized structures
    "glandular trichome", "glandular trichomes", "secretory trichome", "secretory trichomes",
    "mycorrhizal root", "mycorrhizal roots", "mycorrhizal structure", "mycorrhizal structures",
    "haustorium", "haustoria", "haustorial", "transfer cell", "transfer cells"
  )

  compound_parts <- list()

  for (i in seq_along(text_vector)) {
    text <- text_vector[i]
    found_compounds <- c()

    for (pattern in compound_patterns) {
      if (str_detect(text, regex(pattern, ignore_case = TRUE))) {
        # Normalize the compound term
        normalized <- normalize_plant_part(pattern)
        if (!is.na(normalized)) {
          found_compounds <- c(found_compounds, normalized)
        }
      }
    }

    compound_parts[[i]] <- unique(found_compounds)
  }

  return(compound_parts)
}

# Enhanced vectorized function to detect plant parts with normalization
detect_plant_parts_batch <- function(text_vector) {
  plant_parts_keywords <- get_plant_parts_keywords()
  pattern <- paste0("\\b(", paste(plant_parts_keywords, collapse = "|"), ")\\b")

  text_lower <- str_to_lower(text_vector)
  parts_found <- str_extract_all(text_lower, pattern)

  # Apply normalization to group singular/plural forms
  parts_normalized <- map(parts_found, function(matches) {
    if (length(matches) == 0) return(character(0))
    normalize_plant_part(matches)
  })

  # Detect compound terms
  compound_parts_raw <- detect_compound_plant_parts(text_lower)

  # Ensure compound_parts has the same length as parts_normalized
  if (length(compound_parts_raw) != length(parts_normalized)) {
    # Pad or truncate compound_parts to match parts_normalized length
    compound_parts <- vector("list", length(parts_normalized))
    for (i in seq_along(parts_normalized)) {
      if (i <= length(compound_parts_raw)) {
        compound_parts[[i]] <- compound_parts_raw[[i]]
      } else {
        compound_parts[[i]] <- character(0)
      }
    }
  } else {
    compound_parts <- compound_parts_raw
  }

  # Combine simple and compound parts with explicit length matching
  combined_parts <- vector("list", length(parts_normalized))

  for (i in seq_along(parts_normalized)) {
    simple <- if (length(parts_normalized[[i]]) == 0) character(0) else parts_normalized[[i]]
    compound <- if (length(compound_parts[[i]]) == 0) character(0) else compound_parts[[i]]
    combined_parts[[i]] <- c(simple, compound)
  }

  # Remove duplicates after normalization and filter out NA values
  parts_clean <- map(combined_parts, ~{
    if (length(.) == 0) return(character(0))
    unique_parts <- unique(.x)
    unique_parts[!is.na(unique_parts)]
  })

  # Final normalization and deduplication
  parts_final <- map(parts_clean, ~{
    if (length(.) == 0) return(character(0))
    normalize_plant_part(.)
  })

  # Create result vectors with consistent structure
  plant_parts_detected <- character(length(parts_final))
  parts_count <- integer(length(parts_final))
  parts_normalized <- character(length(parts_final))

  for (i in seq_along(parts_final)) {
    final_parts <- parts_final[[i]]
    clean_parts <- parts_clean[[i]]

    if (length(final_parts) > 0 && any(!is.na(final_parts))) {
      valid_parts <- unique(final_parts[!is.na(final_parts)])
      plant_parts_detected[i] <- paste(valid_parts, collapse = "; ")
      parts_count[i] <- length(valid_parts)
    } else {
      plant_parts_detected[i] <- NA_character_
      parts_count[i] <- 0
    }

    if (length(clean_parts) > 0 && any(!is.na(clean_parts))) {
      valid_clean <- unique(clean_parts[!is.na(clean_parts)])
      parts_normalized[i] <- paste(valid_clean, collapse = "; ")
    } else {
      parts_normalized[i] <- NA_character_
    }
  }

  return(tibble(
    plant_parts_detected = plant_parts_detected,
    parts_count = parts_count,
    parts_normalized = parts_normalized
  ))
}

# Enhanced main plant parts extraction function with methods integration
extract_plant_parts_data <- function(
  abstracts_data,
  output_file = "results/plant_parts_detection_results.csv",
  batch_size = 1000,
  force_rerun = FALSE,
  verbose = TRUE
) {

  # Recovery mechanism
  if (file.exists(output_file) && !force_rerun) {
    if (verbose) cat("✅ Found existing plant parts detection results\n")
    existing_results <- read_csv(output_file, show_col_types = FALSE)
    if (verbose) cat("   Loaded", nrow(existing_results), "existing records\n")
    return(existing_results)
  }

  if (verbose) cat("🔬 Starting plant parts detection for", nrow(abstracts_data), "abstracts\n")

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
      format = "Plant Parts [:bar] :percent | ETA: :eta | :current/:total batches",
      total = n_batches,
      clear = FALSE
    )
  }

  plant_parts_results <- map_dfr(1:n_batches, function(batch_num) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, length(abstracts_text))

    batch_text <- abstracts_text[start_idx:end_idx]
    batch_ids <- abstracts_data$id[start_idx:end_idx]

    if (verbose) {
      pb$tick()
    } else {
      cat("   🌿 Batch", batch_num, "of", n_batches, "(", length(batch_text), "abstracts)\n")
    }

    # Detect plant parts
    plant_parts <- detect_plant_parts_batch(batch_text)

    # Enhanced summary with normalization information
    parts_found <- sum(!is.na(plant_parts$plant_parts_detected))
    avg_parts <- round(mean(plant_parts$parts_count[plant_parts$parts_count > 0], na.rm = TRUE), 1)

    if (verbose && batch_num %% 10 == 0) {
      cat("      Found parts in", parts_found, "abstracts (avg:", avg_parts, "normalized parts per abstract)\n")
    }

    # Combine with IDs and include normalization info
    # Ensure we have the same number of rows
    n_results <- nrow(plant_parts)
    if (n_results != length(batch_ids)) {
      # Adjust batch_ids to match plant_parts results
      batch_ids <- batch_ids[1:n_results]
    }

    bind_cols(
      tibble(id = batch_ids),
      plant_parts
    )
  })

  # Save results
  write_csv(plant_parts_results, output_file)

  # Summary
  total_abstracts <- nrow(plant_parts_results)
  abstracts_with_parts <- sum(!is.na(plant_parts_results$plant_parts_detected))
  total_parts_mentioned <- sum(plant_parts_results$parts_count, na.rm = TRUE)
  avg_parts_per_abstract <- round(total_parts_mentioned / abstracts_with_parts, 1)

  if (verbose) {
    cat("\n🎉 Plant parts detection completed!\n")
    cat("📈 Results:\n")
    cat("   - Total abstracts processed:", total_abstracts, "\n")
    cat("   - Abstracts with plant parts:", abstracts_with_parts,
        "(", round(100 * abstracts_with_parts / total_abstracts, 1), "%)\n")
    cat("   - Total plant parts mentioned:", total_parts_mentioned, "\n")
    cat("   - Average parts per abstract:", avg_parts_per_abstract, "\n")
    cat("💾 Results saved to:", output_file, "\n")
  }

  return(plant_parts_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "03_extract_plant_parts.R")) {

  verbose <- TRUE

  # Load consolidated dataset (contains abstracts and metadata)
  consolidated_file <- "results/consolidated_dataset.csv"
  if (!file.exists(consolidated_file)) {
    stop("❌ Consolidated dataset not found. Run the consolidation script first.")
  }

  cat("📖 Loading consolidated dataset from:", consolidated_file, "\n")
  if (verbose) {
    cat("   Loading only required columns for faster processing...\n")
    start_time <- Sys.time()
  }

  # Only load the columns we actually need to reduce memory usage and load time
  required_cols <- c("id", "abstract")
  abstracts_data <- read_csv(
    consolidated_file,
    show_col_types = FALSE,
    col_select = all_of(required_cols),
    progress = FALSE  # Disable progress bar for faster loading
  )

  if (verbose) {
    load_time <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    cat("   ✅ Loaded", nrow(abstracts_data), "records with", ncol(abstracts_data), "columns in", load_time, "seconds\n")
  }

  # Check if we have the required columns
  required_cols <- c("id", "abstract")
  missing_cols <- setdiff(required_cols, colnames(abstracts_data))
  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns in consolidated data:", paste(missing_cols, collapse = ", "))
  }

  # Extract plant parts
  plant_parts_results <- extract_plant_parts_data(abstracts_data)

  cat("\n✅ Plant parts extraction component completed!\n")
  cat("🌿 Applied singular/plural normalization for consistent grouping\n")
}
