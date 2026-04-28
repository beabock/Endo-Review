library(tidyverse)
library(stringr)

# 1. PRE-CLEANING: Fix structural breaks in the raw text before parsing
# This handles things like '(none,' or '(not provided,' which cause CSV column shifts
raw_lines <- readLines("data/Ollama_extraction_all.csv", warn = FALSE)

# Fix the specific "unclosed parenthesis" issues found in rows like 3364
clean_lines <- raw_lines %>%
  str_replace_all("\\(none,", "(none),") %>%
  str_replace_all("\\(not provided,", "(not provided),") %>%
  str_replace_all("\\(n/a,", "(n/a),") %>%
  # Remove any stray backslashes that might escape quotes incorrectly
  str_replace_all("\\\\", "")

# Write to a temporary file for reading
temp_raw <- tempfile(fileext = ".csv")
writeLines(clean_lines, temp_raw)

# 2. LOAD DATA
# Use read_csv (readr) instead of read.csv; it is much better at identifying 'problems'
ds <- readr::read_csv(temp_raw, show_col_types = FALSE)

# Log any rows that readr flagged as structurally broken
if (nrow(readr::problems(ds)) > 0) {
  message("Warning: Structurally malformed rows detected:")
  print(readr::problems(ds))
}

missing_tokens <- c(
    "", "na", "n/a", "none", "not provided", 
    "not specified", "unknown", "not applicable",
    "(none)", "(not provided)", "(n/a)"
)

# 3. NORMALIZATION FUNCTIONS
normalize_text <- function(x) {
    if(!is.character(x)) return(x)
    x_norm <- x %>%
        str_squish() %>%
        str_to_lower() %>%
        # Remove any lingering internal newlines that break CSV structures
        str_replace_all("[\r\n]", " ") 
    
    x_norm[x_norm %in% missing_tokens] <- NA_character_
    x_norm
}

clean_all_columns <- function(df) {
    # Identify character columns
    char_cols <- names(df)[vapply(df, is.character, logical(1))]

    df %>%
        mutate(
            relevance_raw = relevance,
            doc_type_ai_raw = doc_type_ai
        ) %>%
        # Apply normalization to all character columns
        mutate(across(all_of(char_cols), normalize_text)) %>%
        mutate(
            relevance = case_when(
                relevance %in% c("relevant", "releant", "relelevant", "relevance criteria met") ~ "Relevant",
                relevance %in% c("potentially relevant", "relevance uncertain", "uncertain") ~ "Uncertain",
                relevance %in% c("not relevant", "irrelevant") ~ "Irrelevant",
                str_detect(relevance, "relev") & !str_detect(relevance, "not") & !str_detect(relevance, "uncertain") ~ "Relevant",
                str_detect(relevance, "uncertain|potential") ~ "Uncertain",
                str_detect(relevance, "not relevant|irrelevant") ~ "Irrelevant",
                TRUE ~ "Uncertain"
            ),
            doc_type_ai_clean = case_when(
                is.na(doc_type_ai) ~ "Unknown",
                str_detect(doc_type_ai, "title and abstract") ~ "Title + Abstract",
                str_detect(doc_type_ai, "^abstract$") ~ "Abstract",
                str_detect(doc_type_ai, "full-text|full text") ~ "Full-Text",
                str_detect(doc_type_ai, "^title$") ~ "Title",
                str_detect(doc_type_ai, "review|microreview") ~ "Review",
                str_detect(doc_type_ai, "article|journal|report|text|book chapter") ~ "Other Document",
                TRUE ~ "Unknown"
            ),
            presence_absence_clean = case_when(
                is.na(presence_absence) ~ "Uncertain",
                str_detect(presence_absence, "present") ~ "Presence",
                str_detect(presence_absence, "absent") ~ "Absence",
                TRUE ~ "Uncertain"
            ),
            doi_clean = doi %>%
                str_replace("^doi:\\s*", "") %>%
                str_squish(),
            # Ensure page count is numeric or NA
            page_count_clean = as.integer(gsub("[^0-9]", "", as.character(page_count)))
        )
}

# 4. EXECUTE & SAVE
ds_clean <- clean_all_columns(ds)

# Final structural check: if host or microbe name is suspiciously long, it's a merged row
ds_clean <- ds_clean %>%
  filter(str_length(host_clean) < 500) # Prevents merged-row 'poison pills'

write.csv(ds_clean, "data/Ollama_cleaned.csv", row.names = FALSE)
message("Cleanup complete. Cleaned file written to data/Ollama_cleaned.csv")