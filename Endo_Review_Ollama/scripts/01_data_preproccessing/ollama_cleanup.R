library(tidyverse)
library(stringr)

# 1. PRE-CLEANING: Structural fixes for AI-generated artifacts
raw_lines <- readLines("data/Ollama_extraction_all.csv", warn = FALSE)

clean_lines <- raw_lines %>%
  str_replace_all("\\(none,", "(none),") %>%
  str_replace_all("\\(not provided,", "(not provided),") %>%
  str_replace_all("\\(n/a,", "(n/a),") %>%
  str_replace_all("\\\\", "")

temp_raw <- tempfile(fileext = ".csv")
writeLines(clean_lines, temp_raw)

# 2. LOAD DATA
ds <- readr::read_csv(temp_raw, show_col_types = FALSE)

missing_tokens <- c(
    "", "na", "n/a", "none", "not provided", 
    "not specified", "unknown", "not applicable",
    "(none)", "(not provided)", "(n/a)"
)

# 3. NORMALIZATION
normalize_text <- function(x) {
    if(!is.character(x)) return(x)
    x_norm <- x %>%
        str_squish() %>%
        str_to_lower() %>%
        # NEW: Strip out JSON-style brackets and keys
        str_remove_all("\\{\\'scientific_name\\'\\:\\s*\\'") %>%
        str_remove_all("\\'\\}") %>%
        str_remove_all("\\[") %>%
        str_remove_all("\\]") %>%
        str_replace_all("\\'tissue\\'\\:.*", "") %>% # Remove the 'tissue' parts
        str_replace_all("\\'", "") %>%               # Remove remaining single quotes
        str_squish()
    
    x_norm[x_norm %in% missing_tokens] <- NA_character_
    x_norm
}

clean_all_columns <- function(df) {
    char_cols <- names(df)[vapply(df, is.character, logical(1))]

    df %>%
        mutate(
            relevance_raw = relevance,
            doc_type_ai_raw = doc_type_ai
        ) %>%
        mutate(across(all_of(char_cols), normalize_text)) %>%
        mutate(
            relevance = case_when(
                relevance %in% c("relevant", "releant", "relelevant") ~ "Relevant",
                relevance %in% c("potentially relevant", "uncertain") ~ "Uncertain",
                str_detect(relevance, "not relevant|irrelevant") ~ "Irrelevant",
                TRUE ~ "Uncertain"
            ),
            doc_type_ai_clean = case_when(
                str_detect(doc_type_ai, "full-text|full text") ~ "Full-Text",
                str_detect(doc_type_ai, "abstract") ~ "Abstract",
                TRUE ~ "Other/Unknown"
            ),
            presence_absence_clean = case_when(
                str_detect(presence_absence, "present") ~ "Presence",
                str_detect(presence_absence, "absent") ~ "Absence",
                TRUE ~ "Uncertain"
            )
        )
}

# 4. EXECUTE & SANITY FILTER
ds_clean <- clean_all_columns(ds)

# Remove "Poison Pill" rows where columns merged (indicated by extreme length)
ds_clean <- ds_clean %>%
  filter(str_length(plant_host) < 300) %>%
  filter(str_length(fungal_taxon) < 300)

write.csv(ds_clean, "data/Ollama_cleaned.csv", row.names = FALSE)
message("Cleanup complete. Cleaned file written to data/Ollama_cleaned.csv")