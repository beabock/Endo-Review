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
        # 1. Remove JSON/Dictionary artifacts
        str_remove_all("\\{'scientific_name'\\: ?") %>%
        str_remove_all("'tissue'\\: ?'.*?'") %>%
        str_remove_all("[\\{\\}\\[\\]\\']|\\:\\s?") %>%
        
        # 2. Remove AI "Commentary" inside parentheses (e.g., '(bacteria')
        # But keep it if it looks like a year/authority like (L.) or (1890)
        str_remove_all("\\((none|unknown|no fungus|not a fungus|bacteria|see text).*?\\)") %>%
        
        # 3. Clean up ghost characters and standard symbols
        str_replace_all("[\r\n\t]", " ") %>%
        str_squish() %>%
        str_to_lower()
    
    # 4. Final filter: If the "name" is still just a bunch of noise, NA it
    x_norm[x_norm %in% missing_tokens] <- NA_character_
    
    # If the string is still ridiculously long, it's a hallucination
    x_norm[str_length(x_norm) > 150] <- NA_character_
    
    return(x_norm)
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