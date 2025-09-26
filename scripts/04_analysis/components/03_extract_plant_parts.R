# =============================================================================
# 03_extract_plant_parts.R - Enhanced plant parts detection component
# =============================================================================
#
# Purpose: Detect plant parts studied from abstracts with comprehensive singular/plural grouping
# and context-aware detection using research method information
#
# Description: Enhanced script that detects plant parts (roots, leaves, stems, etc.) using:
# - Comprehensive singular/plural normalization for consistent grouping
# - Context-aware detection based on research methods (microscopy, molecular, inoculation, etc.)
# - Compound term recognition (e.g., "root tips", "leaf blades", "vascular bundles")
# - Integration with methods detection output for improved accuracy
# - Batch processing for efficient text analysis
#
# Dependencies: tidyverse, stringr, progress; scripts/04_analysis/utilities/reference_data_utils.R
#
# Author: B. Bock
# Date: 2024-09-22
# Enhanced: 2024-09-26 - Added normalization, context-awareness, and methods integration
#
# Inputs/Outputs: Reads methods detection results from results/methods_detection_results.csv;
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

# Enhanced vectorized function to detect plant parts with normalization and context awareness
detect_plant_parts_batch <- function(text_vector, method_context = NULL) {
  plant_parts_keywords <- get_plant_parts_keywords()
  pattern <- paste0("\\b(", paste(plant_parts_keywords, collapse = "|"), ")\\b")

  text_lower <- str_to_lower(text_vector)
  parts_found <- str_extract_all(text_lower, pattern)

  # Apply normalization to group singular/plural forms
  parts_normalized <- map(parts_found, ~normalize_plant_part(.))

  # Detect compound terms
  compound_parts <- detect_compound_plant_parts(text_lower)

  # Combine simple and compound parts
  combined_parts <- map2(parts_normalized, compound_parts, ~c(.x, .y))

  # Remove duplicates after normalization and filter out NA values
  parts_clean <- map(combined_parts, ~unique(.))
  parts_clean <- map(parts_clean, ~.[!is.na(.)])

  # If method context is provided, boost confidence for contextually relevant plant parts
  if (!is.null(method_context)) {
    parts_enhanced <- map2(parts_clean, method_context, function(parts, methods) {
      if (is.na(methods) || methods == "") return(parts)

      method_list <- unlist(str_split(methods, "; "))
      enhanced_parts <- parts

      # Boost scores for plant parts relevant to detected methods
      for (method in method_list) {
        if (str_detect(method, "microscopy|microscopic")) {
          # Microscopy methods often focus on cellular/tissue structures
          tissue_indicators <- c("cortex", "epidermis", "xylem", "phloem", "cambium", "pith",
                               "mesophyll", "trichome", "stoma", "vascular bundle", "cell wall")
          enhanced_parts <- c(enhanced_parts, intersect(tissue_indicators, plant_parts_keywords))
        }
        if (str_detect(method, "molecular|DNA|PCR|sequencing")) {
          # Molecular methods often reference cellular components
          cellular_indicators <- c("cell wall", "plasmodesma", "nucleus", "cytoplasm", "cell membrane")
          enhanced_parts <- c(enhanced_parts, intersect(cellular_indicators, plant_parts_keywords))
        }
        if (str_detect(method, "inoculation|colonization")) {
          # Inoculation studies typically focus on roots and sometimes leaves
          colonization_indicators <- c("root", "root tip", "root hair", "leaf", "mycorrhiza",
                                     "rhizome", "mycelium", "hypha")
          enhanced_parts <- c(enhanced_parts, intersect(colonization_indicators, plant_parts_keywords))
        }
        if (str_detect(method, "culture|isolation")) {
          # Culture-based methods often work with surface-sterilized tissues
          culture_indicators <- c("stem", "root", "leaf", "petiole", "internode", "shoot")
          enhanced_parts <- c(enhanced_parts, intersect(culture_indicators, plant_parts_keywords))
        }
        if (str_detect(method, "physiological|enzyme|metabolite")) {
          # Physiological studies often examine tissue responses
          physiological_indicators <- c("leaf", "root", "stem", "xylem", "phloem", "chloroplast")
          enhanced_parts <- c(enhanced_parts, intersect(physiological_indicators, plant_parts_keywords))
        }
        if (str_detect(method, "ecological|field|survey")) {
          # Ecological studies often look at whole plant or ecosystem level
          ecological_indicators <- c("root system", "canopy", "leaf", "flower", "seed")
          enhanced_parts <- c(enhanced_parts, intersect(ecological_indicators, plant_parts_keywords))
        }
      }

      return(unique(enhanced_parts))
    })
  } else {
    parts_enhanced <- parts_clean
  }

  # Final normalization and deduplication
  parts_final <- map(parts_enhanced, ~normalize_plant_part(.))

  return(tibble(
    plant_parts_detected = map_chr(parts_final, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    parts_count = map_int(parts_final, ~length(unique(.))),
    parts_normalized = map_chr(parts_enhanced, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_)
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

  if (verbose) cat("🔬 Starting enhanced plant parts detection for", nrow(abstracts_data), "abstracts\n")

  # Handle missing abstracts
  abstracts_text <- ifelse(is.na(abstracts_data$abstract), "", abstracts_data$abstract)

  # Extract method context information for enhanced detection
  methods_summary <- ifelse(is.na(abstracts_data$methods_summary), "", abstracts_data$methods_summary)

  # Process in batches for memory efficiency
  n_batches <- ceiling(length(abstracts_text) / batch_size)

  if (verbose) {
    cat("📊 Processing", length(abstracts_text), "abstracts in", n_batches, "batches\n")
    cat("⚙️  Batch size:", batch_size, "abstracts per batch\n")
    cat("🧬 Using method context for enhanced detection\n")
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
    batch_methods <- methods_summary[start_idx:end_idx]

    if (verbose) {
      pb$tick()
    } else {
      cat("   🌿 Batch", batch_num, "of", n_batches, "(", length(batch_text), "abstracts)\n")
    }

    # Detect plant parts with method context awareness
    plant_parts <- detect_plant_parts_batch(batch_text, batch_methods)

    # Enhanced summary with normalization information
    parts_found <- sum(!is.na(plant_parts$plant_parts_detected))
    avg_parts <- round(mean(plant_parts$parts_count[plant_parts$parts_count > 0], na.rm = TRUE), 1)

    if (verbose && batch_num %% 10 == 0) {
      cat("      Found parts in", parts_found, "abstracts (avg:", avg_parts, "normalized parts per abstract)\n")
    }

    # Combine with IDs and include normalization info
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

  # Load methods data (which contains abstracts and method information)
  methods_file <- "results/methods_detection_results.csv"
  if (!file.exists(methods_file)) {
    stop("❌ Methods detection results not found. Run 02_extract_methods.R first.")
  }

  cat("📖 Loading methods detection data from:", methods_file, "\n")
  if (verbose) {
    cat("   Loading large dataset, this may take a moment...\n")
    start_time <- Sys.time()
  }
  abstracts_data <- read_csv(methods_file, show_col_types = FALSE)
  if (verbose) {
    load_time <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    cat("   ✅ Loaded", nrow(abstracts_data), "records with", ncol(abstracts_data), "columns in", load_time, "seconds\n")
  }

  # Check if we have the required columns
  required_cols <- c("id", "abstract", "methods_summary")
  missing_cols <- setdiff(required_cols, colnames(abstracts_data))
  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns in methods data:", paste(missing_cols, collapse = ", "))
  }

  # Extract plant parts using enhanced detection with method context
  plant_parts_results <- extract_plant_parts_data(abstracts_data)

  cat("\n✅ Enhanced plant parts extraction component completed!\n")
  cat("🔗 Successfully integrated with methods detection data\n")
  cat("🌿 Applied singular/plural normalization for consistent grouping\n")
  cat("🧬 Used method context for improved detection accuracy\n")
}
