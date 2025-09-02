# Research Methods Detection Component
# B. Bock - Modular Version
# July 31, 2025 - Methods extraction (fast ~10-30 min)
#
# This script detects research methods (molecular, culture, microscopy)
# Part of the modular extraction pipeline

library(tidyverse)
library(stringr)
library(progress)

# Source utilities
source("scripts/04_analysis/utilities/reference_data_utils.R")

cat("=== METHODS DETECTION COMPONENT ===\n")
cat("Extracting research methods information\n\n")

# Optimized vectorized function to detect research methods
detect_research_methods_batch <- function(text_vector) {
  method_categories <- get_method_keywords()

  # Pre-compile patterns for better performance
  patterns <- purrr::map(method_categories, ~paste(., collapse = "|"))

  text_lower <- str_to_lower(text_vector)

  results <- purrr::map_dfc(names(patterns), function(category) {
    matches <- str_detect(text_lower, patterns[[category]])
    setNames(list(matches), category)
  })

  # Create methods summary
  methods_detected <- purrr::pmap_chr(results, function(...) {
    found <- names(list(...))[unlist(list(...))]
    if(length(found) > 0) paste(found, collapse = "; ") else NA_character_
  })

  return(results %>%
    rename(molecular = molecular, culture_based = culture, microscopy = microscopy) %>%
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

    # Quick summary
    molecular_found <- sum(methods$molecular, na.rm = TRUE)
    culture_found <- sum(methods$culture_based, na.rm = TRUE)
    microscopy_found <- sum(methods$microscopy, na.rm = TRUE)

    if (verbose && batch_num %% 10 == 0) {
      cat("      Found: Molecular(", molecular_found, "), Culture(", culture_found, "), Microscopy(", microscopy_found, ")\n")
    }

    # Combine with IDs
    bind_cols(
      tibble(id = batch_ids),
      methods %>% rename(
        molecular_methods = molecular,
        culture_based_methods = culture_based,
        microscopy_methods = microscopy
      )
    )
  })

  # Save results
  write_csv(methods_results, output_file)

  # Summary
  total_abstracts <- nrow(methods_results)
  molecular_total <- sum(methods_results$molecular_methods, na.rm = TRUE)
  culture_total <- sum(methods_results$culture_based_methods, na.rm = TRUE)
  microscopy_total <- sum(methods_results$microscopy_methods, na.rm = TRUE)
  any_methods <- sum(!is.na(methods_results$methods_summary))

  if (verbose) {
    cat("\n🎉 Methods detection completed!\n")
    cat("📈 Results:\n")
    cat("   - Total abstracts processed:", total_abstracts, "\n")
    cat("   - Molecular methods:", molecular_total,
        "(", round(100 * molecular_total / total_abstracts, 1), "%)\n")
    cat("   - Culture-based methods:", culture_total,
        "(", round(100 * culture_total / total_abstracts, 1), "%)\n")
    cat("   - Microscopy methods:", microscopy_total,
        "(", round(100 * microscopy_total / total_abstracts, 1), "%)\n")
    cat("   - Any method information:", any_methods,
        "(", round(100 * any_methods / total_abstracts, 1), "%)\n")
    cat("💾 Results saved to:", output_file, "\n")
  }

  return(methods_results)
}

# Run if called directly
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "02_extract_methods.R")) {

  # Load abstracts data
  abstracts_file <- "results/prepared_abstracts_for_extraction.csv"
  if (!file.exists(abstracts_file)) {
    stop("❌ Prepared abstracts not found. Run the pipeline script first or prepare data manually.")
  }

  abstracts_data <- read_csv(abstracts_file, show_col_types = FALSE)

  # Extract methods
  methods_results <- extract_methods_data(abstracts_data)

  cat("\n✅ Methods extraction component completed!\n")
}
