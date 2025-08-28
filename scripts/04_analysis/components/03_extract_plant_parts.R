# Plant Parts Detection Component
# B. Bock - Modular Version
# July 31, 2025 - Plant parts extraction (fast ~10-30 min)
#
# This script detects plant parts studied (roots, leaves, stems, etc.)
# Part of the modular extraction pipeline

library(tidyverse)
library(stringr)
library(progress)

# Source utilities
source("scripts/04_analysis/reference_data_utils.R")

cat("=== PLANT PARTS DETECTION COMPONENT ===\n")
cat("Extracting plant parts information\n\n")

# Optimized vectorized function to detect plant parts
detect_plant_parts_batch <- function(text_vector) {
  plant_parts_keywords <- get_plant_parts_keywords()
  pattern <- paste0("\\b(", paste(plant_parts_keywords, collapse = "|"), ")\\b")

  text_lower <- str_to_lower(text_vector)
  parts_found <- str_extract_all(text_lower, pattern)

  tibble(
    plant_parts_detected = map_chr(parts_found, ~if(length(.) > 0) paste(unique(.), collapse = "; ") else NA_character_),
    parts_count = map_int(parts_found, ~length(unique(.)))
  )
}

# Main plant parts extraction function
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

    # Quick summary
    parts_found <- sum(!is.na(plant_parts$plant_parts_detected))
    avg_parts <- round(mean(plant_parts$parts_count[plant_parts$parts_count > 0], na.rm = TRUE), 1)

    if (verbose && batch_num %% 10 == 0) {
      cat("      Found parts in", parts_found, "abstracts (avg:", avg_parts, "parts per abstract)\n")
    }

    # Combine with IDs
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

  # Load abstracts data
  abstracts_file <- "results/prepared_abstracts_for_extraction.csv"
  if (!file.exists(abstracts_file)) {
    stop("❌ Prepared abstracts not found. Run the pipeline script first or prepare data manually.")
  }

  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)

  # Extract plant parts
  plant_parts_results <- extract_plant_parts_data(abstracts_data)

  cat("\n✅ Plant parts extraction component completed!\n")
}
