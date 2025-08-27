# Find "All Plants Host Fungi" Statement and Variations
# B. Bock
# Script to identify abstracts containing the core statement that "all plants host fungi"
# or variations thereof, which is the foundational premise of the research

library(tidyverse)
library(stringr)
library(stringi)
library(tictoc)

cat("=== FINDING 'ALL PLANTS HOST FUNGI' STATEMENT ===\n")
cat("Searching for the core premise statement and variations\n\n")

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
    "fungal endophytes are ubiquitous",
    "endophytic fungi are ubiquitous",
    "all plants harbor fungi",
    "all plants harbor fungal",
    "fungi colonize all plants",
    "fungal colonization of all plants"
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
    "endophytic fungi in every plant"
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
    "fungi are symbionts of all plants"
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
    "cosmopolitan distribution of fungal endophytes"
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
    "fungal endophytes are prevalent"
  )
)

# Function to search for patterns in text
search_patterns <- function(text, patterns_list, case_sensitive = FALSE) {
  if (is.na(text) || text == "") return(list())

  text_lower <- str_to_lower(text)

  found_patterns <- list()

  for (category in names(patterns_list)) {
    patterns <- patterns_list[[category]]
    category_matches <- c()

    for (pattern in patterns) {
      pattern_lower <- str_to_lower(pattern)

      # Use stringi for faster pattern matching
      if (stri_detect_fixed(text_lower, pattern_lower)) {
        category_matches <- c(category_matches, pattern)

        # Also check for fuzzy matches (allowing for small variations)
        # Look for patterns with small edits (insertions/deletions)
        fuzzy_pattern <- paste0("(?i)", stri_replace_all_regex(pattern_lower, "\\s+", "\\\\s+"))
        if (stri_detect_regex(text_lower, fuzzy_pattern)) {
          category_matches <- c(category_matches, paste0(pattern, " (fuzzy)"))
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

  text_lower <- str_to_lower(text)
  pattern_lower <- str_to_lower(pattern)

  # Find pattern position
  pattern_pos <- stri_locate_first_fixed(text_lower, pattern_lower)

  if (is.na(pattern_pos[1])) return(NA_character_)

  # Split text into words
  words <- str_split(text, "\\s+")[[1]]
  words_lower <- str_to_lower(words)

  # Find word position of pattern
  pattern_words <- str_split(pattern_lower, "\\s+")[[1]]

  # Find starting position of pattern in words
  start_pos <- NA
  for (i in 1:(length(words_lower) - length(pattern_words) + 1)) {
    if (all(words_lower[i:(i+length(pattern_words)-1)] == pattern_words)) {
      start_pos <- i
      break
    }
  }

  if (is.na(start_pos)) return(NA_character_)

  # Extract context
  context_start <- max(1, start_pos - context_words)
  context_end <- min(length(words), start_pos + length(pattern_words) - 1 + context_words)

  context_words <- words[context_start:context_end]
  context <- paste(context_words, collapse = " ")

  return(context)
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
find_all_plants_statement <- function(input_file = NULL, output_dir = "results") {

  cat("Starting search for 'all plants host fungi' statement...\n")
  tic("Statement search")

  # Determine input data source
  if (is.null(input_file)) {
    # Try to load from results
     if (file.exists("results/relevant_abstracts_with_pa_predictions.csv")) {
      cat("Loading classification results...\n")
      data <- read_csv("results/relevant_abstracts_with_pa_predictions.csv")
      text_column <- "abstract"
    } else {
      stop("No suitable data file found. Please specify input_file or ensure results exist.")
    }
  } else {
    cat("Loading specified file:", input_file, "\n")
    data <- read_csv(input_file)
    text_column <- "abstract"
  }

  cat("Processing", nrow(data), "abstracts...\n")

  # Process abstracts in batches for efficiency
  batch_size <- 500
  n_batches <- ceiling(nrow(data) / batch_size)

  results_list <- list()

  for (batch_num in 1:n_batches) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, nrow(data))

    batch_data <- data[start_idx:end_idx, ]

    cat("Processing batch", batch_num, "of", n_batches,
        "(abstracts", start_idx, "to", end_idx, ")\n")

    # Process each abstract in the batch
    batch_results <- map_dfr(1:nrow(batch_data), function(i) {
      abstract_text <- batch_data[[text_column]][i]

      # Search for patterns
      found <- search_patterns(abstract_text, core_statement_patterns)

      # Extract contexts for found patterns
      contexts <- list()
      if (length(found) > 0) {
        for (category in names(found)) {
          for (pattern in found[[category]]) {
            # Remove fuzzy indicator for context extraction
            clean_pattern <- str_remove(pattern, " \\(fuzzy\\)$")
            context <- extract_context(abstract_text, clean_pattern)
            if (!is.na(context)) {
              contexts <- c(contexts, context)
            }
          }
        }
      }

      # Return results
      tibble(
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
    })

    results_list[[batch_num]] <- batch_results
  }

  # Combine all results
  all_results <- bind_rows(results_list)

  # Filter to only abstracts containing the statement
  statement_abstracts <- all_results %>%
    filter(has_statement)

  # Perform analysis
  analysis <- analyze_found_statements(statement_abstracts)

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
        cat("Context:", paste(unlist(sample_abstracts$contexts[i]), collapse = " [...] ")[1], "\n\n")
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

# Run the analysis
if (interactive() || TRUE) {  # Allow running when sourced
  results <- find_all_plants_statement()
}

cat("\nStatement search complete! 🔍📄\n")