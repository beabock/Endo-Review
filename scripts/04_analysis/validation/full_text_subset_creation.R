# Creating this script to pull a subset of full text data to be manually read by BB
# Updated 2/17/26 to handle multi-row abstracts

library(dplyr)
library(stringr)
library(readr)

# Load existing utilities
source("scripts/utils/plot_utils.R")
utils_path <- "scripts/04_analysis/utilities/reference_data_utils.R"
if (file.exists(utils_path)) {
  source(utils_path)
}

results <- read_csv("results/datasets/comprehensive_extraction_results.csv", show_col_types = FALSE)

# 1. Collapse Dataset to One Row Per DOI
# This ensures we are sampling 500 unique papers, not 500 extraction rows.
results_collapsed <- results %>%
  filter(!is.na(doi)) %>%
  group_by(doi) %>%
  summarize(
    # Keep the first instance of basic metadata
    article_title = first(article_title),
    authors = first(authors),
    publication_year = first(publication_year),
    abstract = first(abstract),
    # Take the max probability so we prioritize papers where the model found SOMETHING relevant
    glmnet_prob_presence = max(glmnet_prob_presence, na.rm = TRUE),
    relevance_strict = first(relevance_strict),
    # Collapse extracted lists into single strings for easy viewing in the spreadsheet
    plant_parts_predicted = paste(unique(plant_parts_detected), collapse = "; "),
    methods_predicted = paste(unique(methods_summary), collapse = "; "),
    .groups = "drop"
  )

# 2. Prioritize and Sample
set.seed(1998) 

manual_review_candidates <- results_collapsed %>%
  # Prioritize by the highest probability found in that paper
  arrange(desc(glmnet_prob_presence)) %>%
  slice_head(n = 500) %>%
  mutate(
    first_author = str_extract(authors, "^[^, ]+"),
    # Clean up file names (remove special characters that break file systems)
    first_author_clean = str_replace_all(first_author, "[^[:alnum:]]", ""),
    file_name_suggested = paste0(first_author_clean, "_", publication_year, ".pdf")
  )

# 3. Create the Manual Review Spreadsheet
spreadsheet_for_review <- manual_review_candidates %>%
  select(
    doi, 
    file_name_suggested,
    article_title, 
    publication_year,
    abstract,
    # These show you what the model found across all its rows for this paper
    model_plant_parts = plant_parts_predicted,
    model_methods = methods_predicted,
    # Placeholder columns for your manual entry
    full_text_found = publication_year, # Just a placeholder to overwrite
    manual_is_endophyte = publication_year,
    manual_plant_parts = publication_year,
    manual_methods = publication_year,
    notes = publication_year
  ) %>%
  mutate(
    across(c(full_text_found, manual_is_endophyte, manual_plant_parts, manual_methods, notes), ~ NA_character_),
    full_text_found = "To Search"
  )

# 4. Export
write_csv(spreadsheet_for_review, "scripts/04_analysis/validation/Endophyte_FullText_Review_Log.csv")

cat("Total unique papers for full-text search:", nrow(spreadsheet_for_review), "\n")

# Create a simple BibTeX file from your DOIs
# This creates a 'stub' for each paper that Zotero will then flesh out
bib_entries <- paste0("@article{paper", 1:nrow(manual_review_candidates), 
                      ",\n  doi = {", manual_review_candidates$doi, "}\n}")

writeLines(bib_entries, "scripts/04_analysis/validation/To_Import.bib")
