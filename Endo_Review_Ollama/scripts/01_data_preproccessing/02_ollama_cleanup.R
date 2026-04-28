library(tidyverse)
library(stringr)

# ==============================================================================
# PHASE 1: PRE-PARSER SANITIZATION
# Treat the file as a raw text string to fix catastrophic formatting errors.
# ==============================================================================
message("Phase 1: Sanitizing raw text stream...")

raw_text <- readChar("data/Ollama_python_healed.csv", file.info("data/Ollama_extraction_all.csv")$size)

# Stitch split rows (newline followed by lowercase letter)
healed_text <- str_replace_all(raw_text, "\\n([a-z])", "\\1")
# Annihilate quotes and backslashes that break CSV parsers
healed_text <- str_replace_all(healed_text, '["\\\\]', '') 

temp_healed <- tempfile()
writeLines(healed_text, temp_healed)


# ==============================================================================
# PHASE 2: STRUCTURAL LOCKING
# Force the drifting columns into a strict 15-column grid.
# ==============================================================================
message("Phase 2: Enforcing strict column grid...")

lines <- readLines(temp_healed)
data_lines <- lines[-1] # Drop header

force_grid <- function(line) {
  parts <- str_split(line, ",")[[1]]
  # If row is disastrously short, drop it
  if (length(parts) < 15) return(NULL) 
  
  # Columns 1-10 are strictly defined.
  core_cols <- parts[1:10]
  # Columns 11+ (interaction_notes, biome, country, etc.) often contain commas. 
  # We glue the overflow into the final "source_file" column to preserve data, 
  # but keep columns 1-10 perfectly aligned for the Python script.
  overflow_cols <- c(
      parts[11], parts[12], parts[13], parts[14], 
      paste(parts[15:length(parts)], collapse = "; ")
  )
  return(c(core_cols, overflow_cols))
}

ds_grid <- as.data.frame(do.call(rbind, compact(map(data_lines, force_grid))))
colnames(ds_grid) <- c("relevance","doc_type_ai","doc_type_pages","page_count","doi",
                       "plant_host","fungal_taxon","tissue","presence_absence",
                       "primary_guild","interaction_notes","biome","country",
                       "data_source","source_file")


# ==============================================================================
# PHASE 3 & 4: CONTENT SANITIZATION & SEMANTIC VALIDATION
# Clean the text and enforce biological rules on the taxon column.
# ==============================================================================
message("Phase 3 & 4: Sanitizing content and validating semantics...")

# Stop-words that mean "we don't have a taxon"
null_words <- c("endophytic", "endophyte", "unspecified", "unknown", "none", "n/a", "fungus", "fungi")

clean_taxon <- function(x) {
    if(!is.character(x)) return(x)
    
    x <- x %>%
        # 1. Strip JSON, HTML, and technical wrappers
        str_replace_all("<.*?>", " ") %>%
        str_remove_all("\\{'scientific_name'\\: ?|'tissue'\\: ?'.*?'") %>%
        str_remove_all("[\\{\\}\\[\\]\\']") %>%
        
        # 2. Strip ALL parenthetical content (usually AI chatter or common names)
        str_remove_all("\\(.*?\\)") %>%
        
        # 3. Strip common AI prefixes
        str_remove_all("^(taxon|phylum|class|order|family|genus|species|name|role)\\s*:?\\s*") %>%
        
        # 4. Standardize whitespace
        str_squish() %>%
        str_to_lower()
    
    # SEMANTIC VALIDATION 1: Exact matches for useless words -> NA
    x[x %in% null_words] <- NA_character_
    
    # SEMANTIC VALIDATION 2: Word Count Guardrail
    # A valid taxon is rarely more than 4 words (e.g., "Fusarium oxysporum f. sp. cubense").
    # If the AI wrote a sentence (> 6 words), kill it.
    word_counts <- str_count(x, "\\w+")
    x[word_counts > 6] <- NA_character_
    
    # SEMANTIC VALIDATION 3: Length Guardrail
    x[str_length(x) > 60] <- NA_character_
    
    return(x)
}

# Apply standard cleaning to all columns, but strict biological rules to the taxon
ds_final <- ds_grid %>%
    mutate(across(everything(), ~ str_squish(str_to_lower(.x)))) %>%
    mutate(fungal_taxon = clean_taxon(fungal_taxon)) %>%
    
    # Filter out Clinical/Human hosts
    filter(!str_detect(plant_host, "homo sapiens|human|carcinoma|patient|infant|clinical")) %>%
    
    # Standardize categoricals
    mutate(
        relevance = ifelse(str_detect(relevance, "relev") & !str_detect(relevance, "not"), "Relevant", "Uncertain"),
        doc_type_ai_clean = ifelse(str_detect(doc_type_ai, "full-text|full text"), "Full-Text", "Abstract")
    )


# ==============================================================================
# FINAL SAVE
# ==============================================================================
write.csv(ds_final, "data/Ollama_cleaned.csv", row.names = FALSE)
message(sprintf("Architecture complete. %d clean rows ready for GBIF.", nrow(ds_final)))