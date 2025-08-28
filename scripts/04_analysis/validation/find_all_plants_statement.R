# Find "All Plants Host Fungi" Statement and Variations
# B. Bock
# Script to identify abstracts containing the core statement that "all plants host fungi"
# or variations thereof, which is the foundational premise of the research

library(tidyverse)
library(stringr)
library(stringi)
library(stringdist)
library(tictoc)
library(progress)

cat("=== FINDING 'ALL PLANTS HOST FUNGI' STATEMENT ===\n")
cat("Searching for the core premise statement and variations\n\n")

# Configuration - can be modified for different use cases
config <- list(
  # Search parameters
  fuzzy_threshold = 0.9,      # Similarity threshold for fuzzy matching (0-1)
  use_fuzzy_matching = TRUE,   # Enable/disable fuzzy matching for speed
  context_words = 10,          # Words of context to extract around patterns
  batch_size = 1000,           # Abstracts to process per batch (increased for speed)

  # Output settings
  output_dir = "results",      # Directory for output files
  save_all_results = TRUE,     # Save complete results file
  save_filtered_only = TRUE,   # Save filtered results (statements only)
  save_analysis_report = TRUE, # Save detailed analysis report

  # Performance settings
  verbose = TRUE,              # Show progress messages
  max_context_length = 500,    # Maximum characters for context display
  sample_size = 5              # Number of sample abstracts to show
)

cat("Configuration loaded - Fuzzy threshold:", config$fuzzy_threshold,
    "| Context words:", config$context_words,
    "| Batch size:", config$batch_size, "\n\n")

# Define comprehensive patterns for the core statement
core_statement_patterns <- list(

  # Primary patterns - exact matches for the core statement
primary = c(
    "all plants host fungi",
    "all plants host fungal",
    "every plant hosts fungi",
    "every plant hosts fungal",
    "all plant species host fungi",
    "all plant species host fungal",
    "plants universally host fungi",
    "plants universally host fungal",
    "fungi are ubiquitous in plants",
    "endophytes are ubiquitous",
    "endophytic fungi are ubiquitous",
    "all plants harbor fungi",
    "all plants harbor fungal",
    "fungi colonize all plants",
    "fungal colonization of all plants",
    "all plants have endophytes",
    "every plant has endophytes",
    "all plants are colonized by fungi",
    "universal fungal endophytes",
    "ubiquitous endophytic fungi"
  ),

  # Secondary patterns - variations and synonyms
  secondary = c(
    "most plants host fungi",
    "nearly all plants host fungi",
    "vast majority of plants host fungi",
    "plants generally host fungi",
    "plants typically host fungal",
    "fungi present in all plants",
    "fungal presence in all plants",
    "ubiquitous fungal colonization",
    "universal fungal colonization",
    "fungi found in all plants",
    "fungal endophytes in all plants",
    "endophytic fungi in every plant",
    "all plants are infected by fungi",
    "universal fungal infection",
    "plants are always colonized",
    "constant fungal presence",
    "invariable fungal colonization",
    "fungi are always present",
    "endophytes are always found"
  ),

  # Contextual patterns - statements that imply universality
  contextual = c(
    "no plant is without fungi",
    "no plant lacks fungal",
    "every plant has fungal",
    "all plants contain fungi",
    "all plants contain fungal",
    "fungi inhabit all plants",
    "fungal inhabitants of all plants",
    "plants are colonized by fungi",
    "universal fungal symbionts",
    "fungi are symbionts of all plants",
    "fungal endophytes are omnipresent",
    "omnipresent fungal colonization",
    "fungi are invariably present",
    "inevitable fungal colonization",
    "endophytes occur universally",
    "universal endophytic association",
    "consistent fungal presence",
    "fungal endophytes are pervasive"
  ),

  # Scientific variations - more formal/academic phrasing
  scientific = c(
    "fungal endophytes occur in all plants",
    "endophytic fungi occur in every plant",
    "all angiosperms host fungi",
    "all gymnosperms host fungi",
    "universal occurrence of fungal endophytes",
    "ubiquitous nature of fungal endophytes",
    "fungal endophytes are cosmopolitan",
    "cosmopolitan distribution of fungal endophytes",
    "endophytic fungi have global distribution",
    "worldwide occurrence of endophytes",
    "fungal endophytes are globally distributed",
    "pan-global fungal endophytes",
    "endophytes show universal distribution",
    "universal fungal symbiosis",
    "fungal endosymbionts are ubiquitous",
    "ubiquitous endosymbiotic fungi",
    "all terrestrial plants host fungi",
    "fungal endophytes in all ecosystems"
  ),

  # Review/generalization patterns - statements about fungal ubiquity
  review = c(
    "fungal endophytes are widespread",
    "endophytic fungi are widespread",
    "fungi are common in plants",
    "fungal colonization is common",
    "prevalent fungal colonization",
    "widespread fungal presence",
    "fungi are prevalent in plants",
    "fungal endophytes are prevalent",
    "endophytes are found everywhere",
    "fungal endophytes are common",
    "fungi commonly colonize plants",
    "frequent fungal colonization",
    "regular fungal presence",
    "routine fungal colonization",
    "typical fungal endophytes",
    "standard fungal colonization",
    "normal fungal presence",
    "expected fungal endophytes"
  ),

  # Quantitative patterns - numerical statements suggesting universality
  quantitative = c(
    "100% of plants host fungi",
    "fungi in 100% of plants",
    "endophytes in 100% of species",
    "all plant families host fungi",
    "fungi across all plant taxa",
    "endophytes in every family",
    "fungi in all plant groups",
    "universal among plant species",
    "across all plant lineages",
    "in every plant clade",
    "throughout the plant kingdom",
    "across the entire plant kingdom"
  )
)

# OPTIMIZED: Pre-compile patterns for much faster matching
compiled_patterns <- NULL

# Function to initialize compiled patterns
init_compiled_patterns <- function(patterns_list, fuzzy_threshold = 0.9) {
  compiled <- list()

  for (category in names(patterns_list)) {
    patterns <- patterns_list[[category]]
    category_patterns <- list()

    for (pattern in patterns) {
      pattern_lower <- str_to_lower(pattern)

      # Create regex pattern with word boundaries for exact matching
      regex_pattern <- paste0("\\b", str_replace_all(pattern_lower, "\\s+", "\\\\s+"), "\\b")

      category_patterns[[pattern]] <- list(
        original = pattern,
        lower = pattern_lower,
        regex = regex_pattern,
        words = str_split(pattern_lower, "\\s+")[[1]]
      )
    }

    compiled[[category]] <- category_patterns
  }

  return(compiled)
}

# HIGHLY OPTIMIZED: Vectorized pattern search with fast fuzzy matching
search_patterns_optimized <- function(text, compiled_patterns, fuzzy_threshold = 0.9) {
  if (is.na(text) || text == "" || nchar(text) < 10) return(list())

  text_lower <- str_to_lower(text)
  found_patterns <- list()

  # Pre-split text for fuzzy matching
  text_words <- str_split(text_lower, "\\s+")[[1]]

  for (category in names(compiled_patterns)) {
    category_matches <- c()
    category_patterns <- compiled_patterns[[category]]

    for (pattern_info in category_patterns) {
      pattern <- pattern_info$original
      pattern_lower <- pattern_info$lower
      regex_pattern <- pattern_info$regex
      pattern_words <- pattern_info$words

      # Fast exact match first (most common case)
      if (str_detect(text_lower, regex_pattern)) {
        category_matches <- c(category_matches, pattern)
      } else {
        # OPTIMIZED FUZZY MATCHING: Only for patterns that could fit
        if (length(pattern_words) <= length(text_words)) {
          # Use grep for fast substring search first
          if (str_detect(text_lower, pattern_words[1])) {
            # Quick similarity check using simple Levenshtein distance
            # Much faster than Jaccard-Winkler for most cases
            best_similarity <- 0

            # Slide window through text (optimized)
            for (i in seq(1, length(text_words) - length(pattern_words) + 1, by = 1)) {
              window <- text_words[i:(i + length(pattern_words) - 1)]
              window_text <- paste(window, collapse = " ")

              # Fast Levenshtein distance (much faster than Jaccard-Winkler)
              lev_dist <- adist(window_text, pattern_lower)[1, 1]
              max_len <- max(nchar(window_text), nchar(pattern_lower))
              similarity <- 1 - (lev_dist / max_len)

              if (similarity > best_similarity) {
                best_similarity <- similarity
              }

              # Early exit if we find a good enough match
              if (best_similarity >= fuzzy_threshold) break
            }

            if (best_similarity >= fuzzy_threshold) {
              category_matches <- c(category_matches, paste0(pattern, " (fuzzy: ", round(best_similarity, 2), ")"))
            }
          }
        }
      }
    }

    if (length(category_matches) > 0) {
      found_patterns[[category]] <- unique(category_matches)
    }
  }

  return(found_patterns)
}

# Function to extract context around found patterns
extract_context <- function(text, pattern, context_words = 10) {
  if (is.na(text) || text == "") return(NA_character_)

  # Clean and prepare text
  text_clean <- str_trim(text)
  if (text_clean == "") return(NA_character_)

  text_lower <- str_to_lower(text_clean)
  pattern_lower <- str_to_lower(str_trim(pattern))

  # Try exact match first
  pattern_pos <- stri_locate_first_fixed(text_lower, pattern_lower)

  # If no exact match, try with word boundaries for better accuracy
  if (is.na(pattern_pos[1])) {
    pattern_words <- str_split(pattern_lower, "\\s+")[[1]]
    if (length(pattern_words) > 1) {
      # For multi-word patterns, look for word sequence
      words <- str_split(text_lower, "\\s+")[[1]]
      pattern_start <- NA

      for (i in 1:(length(words) - length(pattern_words) + 1)) {
        if (all(words[i:(i+length(pattern_words)-1)] == pattern_words)) {
          pattern_start <- i
          break
        }
      }

      if (!is.na(pattern_start)) {
        # Convert word position back to character position
        words_split <- str_split(text_clean, "\\s+")[[1]]
        char_start <- sum(nchar(words_split[1:(pattern_start-1)])) +
                     (pattern_start - 1) + 1  # Add space characters
        char_end <- char_start + sum(nchar(words_split[pattern_start:(pattern_start+length(pattern_words)-1)])) +
                   (length(pattern_words) - 1)  # Add space characters between pattern words

        pattern_pos <- c(char_start, char_end)
      }
    }
  }

  if (is.na(pattern_pos[1])) return(NA_character_)

  # Extract context with character-based positioning for better accuracy
  start_pos <- max(1, pattern_pos[1] - (context_words * 6))  # Estimate ~6 chars per word
  end_pos <- min(nchar(text_clean), pattern_pos[2] + (context_words * 6))

  # Find word boundaries for clean extraction
  context_text <- substr(text_clean, start_pos, end_pos)

  # Clean up context (remove partial words at boundaries)
  context_words_list <- str_split(context_text, "\\s+")[[1]]
  if (length(context_words_list) > 0) {
    # Remove first word if it looks truncated (doesn't start with capital or common start)
    if (length(context_words_list) > 1 &&
        !str_detect(context_words_list[1], "^[A-Z]|^[0-9]|^[a-z]{4,}")) {
      context_words_list <- context_words_list[-1]
    }

    # Remove last word if it looks truncated
    if (length(context_words_list) > 1 &&
        !str_detect(context_words_list[length(context_words_list)], "[.!?]$|[a-z]{4,}$")) {
      context_words_list <- context_words_list[-length(context_words_list)]
    }

    context <- paste(context_words_list, collapse = " ")

    # Ensure reasonable length
    if (nchar(context) > 500) {
      context <- paste0(substr(context, 1, 500), "...")
    }

    return(context)
  }

  return(NA_character_)
}

# Function to analyze found statements
analyze_found_statements <- function(results_df) {
  analysis <- list()

  # Count by category
  analysis$by_category <- results_df %>%
    select(id, found_categories) %>%
    unnest(found_categories) %>%
    count(found_categories, name = "count") %>%
    arrange(desc(count))

  # Count by specific pattern
  analysis$by_pattern <- results_df %>%
    select(id, found_patterns) %>%
    unnest(found_patterns) %>%
    count(found_patterns, name = "count") %>%
    arrange(desc(count))

  # Summary statistics
  analysis$summary <- list(
    total_abstracts = nrow(results_df),
    abstracts_with_statement = sum(results_df$has_statement),
    percentage_with_statement = round(100 * sum(results_df$has_statement) / nrow(results_df), 2),
    total_patterns_found = sum(lengths(results_df$found_patterns))
  )

  return(analysis)
}

# Main execution function
find_all_plants_statement <- function(input_file = NULL,
                                     output_dir = "results",
                                     batch_size = 1000,
                                     fuzzy_threshold = 0.9,
                                     use_fuzzy_matching = TRUE,
                                     context_words = 10,
                                     verbose = TRUE) {

  cat("Starting search for 'all plants host fungi' statement...\n")
  tic("Statement search")

  # Validate inputs
  if (!is.null(input_file) && !file.exists(input_file)) {
    stop("Input file does not exist: ", input_file)
  }

  if (!is.numeric(fuzzy_threshold) || fuzzy_threshold < 0 || fuzzy_threshold > 1) {
    stop("fuzzy_threshold must be a number between 0 and 1")
  }

  if (!is.numeric(context_words) || context_words < 1) {
    stop("context_words must be a positive integer")
  }

  # Determine input data source
  if (is.null(input_file)) {
    # Try to load from results
    possible_files <- c(
      "results/relevant_abstracts_with_pa_predictions.csv",
      "results/predictions_with_abstracts.csv",
      "data/processed_abstracts.csv"
    )

    data <- NULL
    text_column <- "abstract"

    for (file_path in possible_files) {
      if (file.exists(file_path)) {
        if (verbose) cat("Loading data from:", file_path, "\n")
        data <- read_csv(file_path, show_col_types = FALSE)
        break
      }
    }

    if (is.null(data)) {
      stop("No suitable data file found. Please specify input_file or ensure data files exist in expected locations.")
    }
  } else {
    if (verbose) cat("Loading specified file:", input_file, "\n")
    data <- read_csv(input_file, show_col_types = FALSE)
    text_column <- "abstract"
  }

  # Validate required columns
  required_cols <- c("id", "article_title", text_column)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove rows with missing abstracts
  original_n <- nrow(data)
  data <- data %>% filter(!is.na(.data[[text_column]]), .data[[text_column]] != "")
  if (verbose && nrow(data) < original_n) {
    cat("Removed", original_n - nrow(data), "rows with missing/empty abstracts\n")
  }

  if (nrow(data) == 0) {
    stop("No valid abstracts found to process")
  }

  if (verbose) cat("Processing", nrow(data), "abstracts...\n")

  # Initialize compiled patterns for optimized matching
  if (verbose) cat("Pre-compiling patterns for optimized matching...\n")
  compiled_patterns <- init_compiled_patterns(core_statement_patterns, fuzzy_threshold)

  # Process abstracts in batches for efficiency
  n_batches <- ceiling(nrow(data) / batch_size)
  results_list <- list()

  # Initialize progress bar
  if (verbose) {
    pb <- progress_bar$new(
      format = "Processing [:bar] :percent | ETA: :eta | Batch :current/:total",
      total = n_batches,
      clear = FALSE
    )
  }

  for (batch_num in 1:n_batches) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, nrow(data))

    batch_data <- data[start_idx:end_idx, ]

    if (verbose) {
      pb$tick()
    } else {
      cat("Processing batch", batch_num, "of", n_batches,
          "(abstracts", start_idx, "to", end_idx, ")\n")
    }

    # Process each abstract in the batch with error handling
    batch_results <- map_dfr(1:nrow(batch_data), function(i) {
      tryCatch({
        abstract_text <- batch_data[[text_column]][i]

        # Search for patterns using OPTIMIZED function
        found <- search_patterns_optimized(abstract_text, compiled_patterns, fuzzy_threshold = fuzzy_threshold)

        # Extract contexts for found patterns
        contexts <- list()
        if (length(found) > 0) {
          for (category in names(found)) {
            for (pattern in found[[category]]) {
              # Remove fuzzy indicator for context extraction
              clean_pattern <- str_remove(pattern, " \\(fuzzy: [0-9.]+\\)$")
              context <- extract_context(abstract_text, clean_pattern, context_words)
              if (!is.na(context)) {
                contexts <- c(contexts, context)
              }
            }
          }
          # Remove duplicate contexts (from overlapping patterns)
          contexts <- unique(contexts)
        }

        # Return results
        result <- tibble(
          id = batch_data$id[i],
          title = batch_data$article_title[i],
          abstract = abstract_text,
          doi = if ("doi" %in% names(batch_data)) batch_data$doi[i] else NA_character_,
          has_statement = length(found) > 0,
          found_categories = list(names(found)),
          found_patterns = list(unlist(found)),
          contexts = list(contexts),
          prediction = if ("final_classification" %in% names(batch_data))
            batch_data$final_classification[i] else NA_character_,
          confidence = if ("confidence" %in% names(batch_data))
            batch_data$confidence[i] else NA_real_
        )

        return(result)
      }, error = function(e) {
        # Return error information for failed processing
        if (verbose) warning("Error processing abstract ", batch_data$id[i], ": ", e$message)
        return(tibble(
          id = batch_data$id[i],
          title = batch_data$article_title[i],
          abstract = batch_data[[text_column]][i],
          doi = if ("doi" %in% names(batch_data)) batch_data$doi[i] else NA_character_,
          has_statement = FALSE,
          found_categories = list(character(0)),
          found_patterns = list(character(0)),
          contexts = list(character(0)),
          prediction = NA_character_,
          confidence = NA_real_,
          processing_error = e$message
        ))
      })
    })

    results_list[[batch_num]] <- batch_results
  }

  # Combine all results
  all_results <- bind_rows(results_list)

  # Filter to only abstracts containing the statement
  statement_abstracts <- all_results %>%
    filter(has_statement)

  # Perform analysis on all results (not just filtered ones)
  analysis <- analyze_found_statements(all_results)

  # Save results
  dir.create(output_dir, showWarnings = FALSE)

  # Save all results
  write_csv(all_results, file.path(output_dir, "all_plants_statement_search_all.csv"))

  # Save only abstracts with statement
  write_csv(statement_abstracts, file.path(output_dir, "all_plants_statement_found.csv"))

  # Save analysis summary
  capture.output({
    cat("=== 'ALL PLANTS HOST FUNGI' STATEMENT ANALYSIS ===\n")
    cat("Generated:", Sys.time(), "\n\n")

    cat("SEARCH PARAMETERS:\n")
    cat("Total abstracts searched:", analysis$summary$total_abstracts, "\n")
    cat("Abstracts containing statement:", analysis$summary$abstracts_with_statement, "\n")
    cat("Percentage with statement:", analysis$summary$percentage_with_statement, "%\n")
    cat("Total patterns found:", analysis$summary$total_patterns_found, "\n\n")

    cat("PATTERNS FOUND BY CATEGORY:\n")
    print(analysis$by_category)
    cat("\n")

    cat("MOST COMMON PATTERNS:\n")
    print(analysis$by_pattern %>% head(20))
    cat("\n")

    cat("SAMPLE ABSTRACTS:\n")
    if (nrow(statement_abstracts) > 0) {
      sample_abstracts <- statement_abstracts %>% head(5)
      for (i in 1:nrow(sample_abstracts)) {
        cat("--- Abstract", i, "---\n")
        cat("Title:", sample_abstracts$title[i], "\n")
        cat("DOI:", sample_abstracts$doi[i], "\n")
        cat("Found patterns:", paste(unlist(sample_abstracts$found_patterns[i]), collapse = "; "), "\n")
        # Display only first context or combine unique contexts intelligently
        contexts <- unlist(sample_abstracts$contexts[i])
        if (length(contexts) > 0) {
          # Take first context and limit length
          context_text <- contexts[1]
          if (nchar(context_text) > 200) {
            context_text <- paste0(substr(context_text, 1, 200), "...")
          }
          cat("Context:", context_text, "\n\n")
        } else {
          cat("Context: No context available\n\n")
        }
      }
    }

  }, file = file.path(output_dir, "all_plants_statement_analysis.txt"))

  toc()

  # Print summary
  cat("\n=== SEARCH COMPLETE ===\n")
  cat("Results saved to:", output_dir, "\n")
  cat("- all_plants_statement_search_all.csv: All abstracts with search results\n")
  cat("- all_plants_statement_found.csv: Only abstracts containing the statement\n")
  cat("- all_plants_statement_analysis.txt: Detailed analysis report\n\n")

  cat("Summary:\n")
  cat("Total abstracts searched:", analysis$summary$total_abstracts, "\n")
  cat("Abstracts with statement:", analysis$summary$abstracts_with_statement,
      sprintf("(%.1f%%)\n", analysis$summary$percentage_with_statement))
  cat("Total patterns found:", analysis$summary$total_patterns_found, "\n\n")

  return(list(
    all_results = all_results,
    statement_abstracts = statement_abstracts,
    analysis = analysis
  ))
}

# Run the analysis with default configuration
if (interactive() || TRUE) {  # Allow running when sourced
  results <- find_all_plants_statement(
    output_dir = config$output_dir,
    batch_size = config$batch_size,
    fuzzy_threshold = config$fuzzy_threshold,
    context_words = config$context_words,
    verbose = config$verbose
  )
}

cat("\nStatement search complete! 🔍📄\n")

# USAGE EXAMPLES:
#
# Basic usage (uses default config):
# results <- find_all_plants_statement()
#
# Custom parameters:
# results <- find_all_plants_statement(
#   fuzzy_threshold = 0.8,     # More lenient fuzzy matching
#   context_words = 15,        # More context words
#   batch_size = 1000          # Larger batches for faster processing
# )
#
# Specify input file:
# results <- find_all_plants_statement(
#   input_file = "path/to/your/data.csv"
# )
#
# Quiet mode (no progress messages):
# results <- find_all_plants_statement(verbose = FALSE)
#
# RESULTS ACCESS:
# All results: results$all_results
# Only statements: results$statement_abstracts
# Analysis summary: results$analysis
