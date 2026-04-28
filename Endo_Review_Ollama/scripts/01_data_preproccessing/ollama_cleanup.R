library(tidyverse)
library(stringr)

# --- STAGE 1: PERMANENT STRUCTURAL HEALING ---
message("Stitching split rows and healing CSV structure...")

# Read the file as one giant string to handle newlines
raw_text <- readChar("data/Ollama_extraction_all.csv", file.info("data/Ollama_extraction_all.csv")$size)

# 1. Stitch: Find newlines followed by a lowercase letter and remove them
healed_text <- str_replace_all(raw_text, "\\n([a-z])", "\\1")

# 2. Heal structural artifacts
healed_text <- healed_text %>%
  str_replace_all('"', '') %>%
  str_replace_all("\\(none,", "none,") %>%
  str_replace_all("\\(not provided,", "not provided,") %>%
  str_replace_all("\\(n/a,", "n/a,") %>%
  str_replace_all("\\\\", "")

# Write to the temp file
temp_raw <- tempfile(fileext = ".csv")
writeLines(healed_text, temp_raw)

# --- STAGE 2: DATA LOADING ---
ds <- readr::read_csv(temp_raw, show_col_types = FALSE)

# --- STAGE 3: THE BULLETPROOF CLEANER ---
missing_tokens <- c(
    "", "na", "n/a", "none", "not provided", 
    "not specified", "unknown", "not applicable",
    "(none)", "(not provided)", "(n/a)"
)

normalize_text <- function(x) {
    if(!is.character(x)) return(x)
    
    x_norm <- x %>%
        # Remove JSON artifacts: {scientific_name: 'xyz', 'tissue': '...'}
        str_remove_all("\\{'scientific_name'\\: ?") %>%
        str_remove_all("'tissue'\\: ?'.*?'") %>%
        str_remove_all("[\\{\\}\\[\\]\\']|\\:\\s?") %>%
        
        # Remove AI "Commentary" (e.g., '(bacteria', 'not a fungus)')
        str_remove_all("\\((none|unknown|no fungus|not a fungus|bacteria|see text).*?\\)") %>%
        
        # Clean up technical whitespace
        str_replace_all("[\r\n\t]", " ") %>%
        str_squish() %>%
        str_to_lower()
    
    # KILLER LOGIC: If it's a fragment or a sentence, turn it to NA
    # If it contains an unclosed parenthesis, it's garbage.
    x_norm[str_detect(x_norm, "\\(|\\)|\\{|\\}") & !str_detect(x_norm, "\\(.*?\\)")] <- NA_character_
    
    # If it contains "not mentioned" or other low-info AI phrases, NA it.
    x_norm[str_detect(x_norm, "not mentioned|no fungus|no specific|not specified|unknown")] <- NA_character_

    # Final length and token check
    x_norm[x_norm %in% missing_tokens] <- NA_character_
    x_norm[str_length(x_norm) > 150] <- NA_character_
    
    return(x_norm)
}

# --- STAGE 4: CLEANING & SAVING ---
clean_all_columns <- function(df) {
    char_cols <- names(df)[vapply(df, is.character, logical(1))]
    df %>%
        mutate(across(all_of(char_cols), normalize_text)) %>%
        mutate(
            relevance = case_when(
                str_detect(relevance, "relev") & !str_detect(relevance, "not") ~ "Relevant",
                str_detect(relevance, "not relevant|irrelevant") ~ "Irrelevant",
                TRUE ~ "Uncertain"
            ),
            doc_type_ai_clean = case_when(
                str_detect(doc_type_ai, "full-text|full text") ~ "Full-Text",
                str_detect(doc_type_ai, "abstract") ~ "Abstract",
                TRUE ~ "Other/Unknown"
            )
        )
}

ds_clean <- clean_all_columns(ds)

# Final safety filter to prevent merged-row 'poison pills'
ds_clean <- ds_clean %>%
  filter(str_length(plant_host) < 250) %>%
  filter(str_length(fungal_taxon) < 250)

write.csv(ds_clean, "data/Ollama_cleaned.csv", row.names = FALSE)
message(paste("Healed and cleaned.", nrow(ds_clean), "rows written to data/Ollama_cleaned.csv"))