## Minimal optimized_taxa_detection.R
# Provides small, self-contained implementations of helper functions
# referenced by extract_species_simple.R: setup_parallel(),
# create_lookup_tables(), and process_abstracts_parallel().

library(stringr)
library(dplyr)
library(purrr)

#' Setup a parallel backend using doParallel
#' This is a minimal helper that registers a cluster for foreach/doParallel
setup_parallel <- function(workers = parallel::detectCores(logical = FALSE)) {
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package 'doParallel' is required for setup_parallel(). Please install it with install.packages('doParallel')")
  }
  cl <- parallel::makeCluster(workers)
  doParallel::registerDoParallel(cl)
  # store cluster so it can be stopped later if needed
  options("endophyte_parallel_cluster" = cl)
  invisible(cl)
}

#' Create simple lookup tables from a species reference dataframe
#' Accepts common column names and builds a small set of lookup vectors
create_lookup_tables <- function(species_df) {
  if (is.null(species_df) || nrow(species_df) == 0) {
    stop("create_lookup_tables: species dataframe is empty or NULL")
  }

  # Normalize to data.frame
  species_df <- as.data.frame(species_df, stringsAsFactors = FALSE)

  # Find a plausible name column
  name_cols <- c("scientificName", "resolved_name", "canonicalName", "acceptedScientificName", "name", "scientific_name")
  name_col <- intersect(name_cols, names(species_df))
  if (length(name_col) == 0) {
    # Fallback: use first column
    name_col <- names(species_df)[1]
  } else {
    name_col <- name_col[1]
  }

  # Extract canonical names (lowercase) and remove NAs
  canonical <- tolower(trimws(species_df[[name_col]]))
  canonical <- canonical[!is.na(canonical) & canonical != ""]
  canonical <- unique(canonical)

  # Build genus list (first token of binomial)
  genus <- unique(word(canonical, 1))
  genus <- genus[!is.na(genus) & genus != ""]

  # Also build tokenized forms to allow quick matching
  tokens <- unique(unlist(str_split(canonical, "\\s+")))

  lookup <- list(
    canonical = canonical,
    genus = genus,
    tokens = tokens,
    df = species_df,
    name_col = name_col
  )

  class(lookup) <- c("endophyte_lookup", class(lookup))
  return(lookup)
}

#' Very small species detection routine used by the main pipeline
#' This is intentionally simple: exact canonical name matching plus genus matching.
detect_species_in_text <- function(text, lookup) {
  text_lc <- tolower(as.character(text))
    # Move hyphen to end of character class to avoid escaping issues
    text_lc <- str_replace_all(text_lc, "[^a-z0-9_\\s-]", " ")

  found <- character(0)

  # Exact canonical matches (word boundaries)
  for (nm in lookup$canonical) {
    pattern <- paste0("\\b", nm, "\\b")
    if (str_detect(text_lc, pattern)) {
      found <- c(found, nm)
    }
  }

  # If none, try genus-level matches (may be noisy)
  if (length(found) == 0) {
    for (g in lookup$genus) {
      pattern <- paste0("\\b", g, "\\b")
      if (str_detect(text_lc, pattern)) {
        # record genus with a marker
        found <- c(found, paste0(g, " (genus)") )
      }
    }
  }

  found <- unique(found)
  if (length(found) == 0) found <- NA_character_

  return(found)
}

#' Process a set of abstracts and return a data.frame with species matches
#' This is a simple, robust implementation and does not attempt sophisticated NLP.
process_abstracts_parallel <- function(abstracts, lookup_tables, plant_parts_keywords = NULL, batch_size = 10, workers = 1) {
  # abstracts: dataframe with at least columns id, title, abstract
  if (!"id" %in% names(abstracts)) stop("process_abstracts_parallel: 'abstracts' must contain column 'id'")

  # Vectorized apply over rows
  results <- purrr::map_dfr(1:nrow(abstracts), function(i) {
    row <- abstracts[i, ]
    txt <- ifelse(is.na(row$abstract), "", row$abstract)
    species_found <- detect_species_in_text(txt, lookup_tables)

    tib <- tibble::tibble(
      id = row$id,
      detected_species = ifelse(all(is.na(species_found)), NA_character_, paste(na.omit(species_found), collapse = "; "))
    )
    return(tib)
  })

  return(results)
}
