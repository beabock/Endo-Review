library(tidyverse)
library(stringr)

# --- PHASE 1: STRUCTURAL HEALING (Raw Text Level) ---
message("Starting structural healing of raw CSV...")

raw_lines <- readLines("data/Ollama_extraction_all.csv", warn = FALSE)

clean_lines <- raw_lines %>%
  # 1. Remove all double quotes to prevent unclosed-quote "line swallowing"
  str_replace_all('"', '') %>%
  # 2. Fix the specific unclosed parenthesis artifacts from Ollama
  str_replace_all("\\(none,", "none,") %>%
  str_replace_all("\\(not provided,", "not provided,") %>%
  str_replace_all("\\(n/a,", "n/a,") %>%
  # 3. Strip backslashes that might try to escape characters
  str_replace_all("\\\\", "")

# Write to a temporary file so read_csv can handle it cleanly
temp_raw <- tempfile(fileext = ".csv")
writeLines(clean_lines, temp_raw)


# --- PHASE 2: DATA LOADING ---
# Now we use read_csv on the "healed" text
ds <- readr::read_csv(temp_raw, show_col_types = FALSE)


# --- PHASE 3: TAXONOMIC & CONTENT CLEANING ---
missing_tokens <- c(
    "", "na", "n/a", "none", "not provided", 
    "not specified", "unknown", "not applicable",
    "(none)", "(not provided)", "(n/a)"
)

normalize_text <- function(x) {
    if(!is.character(x)) return(x)
    
    x_norm <- x %>%
        # Remove JSON/Dictionary artifacts (the {scientific_name: ...} rows)
        str_remove_all("\\{'scientific_name'\\: ?") %>%
        str_remove_all("'tissue'\\: ?'.*?'") %>%
        str_remove_all("[\\{\\}\\[\\]\\']|\\:\\s?") %>%
        
        # Remove AI "Commentary" inside parentheses (e.g., '(bacteria')
        str_remove_all("\\((none|unknown|no fungus|not a fungus|bacteria|see text).*?\\)") %>%
        
        # Clean up ghost characters
        str_replace_all("[\r\n\t]", " ") %>%
        str_squish() %>%
        str_to_lower()
    
    # Final cleanup of missing values and hallucinations
    x_norm[x_norm %in% missing_tokens] <- NA_character_
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

# --- PHASE 4: EXECUTION & SAVING ---
ds_clean <- clean_all_columns(ds)

# Final safety check: drop rows where the merger might have survived
ds_clean <- ds_clean %>%
  filter(str_length(plant_host) < 300) %>%
  filter(str_length(fungal_taxon) < 300)

write.csv(ds_clean, "data/Ollama_cleaned.csv", row.names = FALSE)
message(paste("Cleanup complete.", nrow(ds_clean), "rows written to data/Ollama_cleaned.csv"))