library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(forcats)

source("scripts/05_plotting/theme_utils.R")

INPUT_FILE <- "data/Ollama_cleaned_synresolved_standardized_final.csv"
OUTPUT_DIR <- "results/tissue_analysis"
RAW_COUNTS_FILE <- file.path(OUTPUT_DIR, "tissue_counts_by_study_raw.csv")
PLANT_COUNTS_FILE <- file.path(OUTPUT_DIR, "tissue_counts_by_study_plant_parts.csv")
RAW_PLOT_FILE <- file.path(OUTPUT_DIR, "top_tissue_terms_by_study_raw.png")
PLANT_PLOT_FILE <- file.path(OUTPUT_DIR, "top_tissue_parts_by_study.png")

if (!file.exists(INPUT_FILE)) {
	stop("Input file not found: ", INPUT_FILE)
}

if (!dir.exists(OUTPUT_DIR)) {
	dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
}

df <- read_csv(INPUT_FILE, show_col_types = FALSE)

required_cols <- c("paper_id", "tissue")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
	stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

clean_tissue_value <- function(x) {
	x %>%
		str_to_lower() %>%
		str_replace_all("[\"'`]+", "") %>%
		str_replace_all("\\s+", " ") %>%
		str_trim()
}

is_missing_tissue <- function(x) {
	x %in% c("", "na", "n/a", "none", "unknown", "not specified", "not-specified", "not_mentioned")
}

split_tissues <- function(x) {
	# Support common multi-value separators used in extracted metadata.
	str_split(x, "\\s*(?:/|\\band\\b|&)\\s*")
}

# Build one row per paper_id x tissue token, then deduplicate.
paper_tissue <- df %>%
	transmute(
		paper_id = as.character(paper_id),
		tissue_raw = clean_tissue_value(as.character(tissue))
	) %>%
	filter(!is.na(paper_id), paper_id != "", !is.na(tissue_raw), !is_missing_tissue(tissue_raw)) %>%
	mutate(tissue_tokens = split_tissues(tissue_raw)) %>%
	unnest_longer(tissue_tokens) %>%
	mutate(
		tissue_token = tissue_tokens %>%
			str_squish() %>%
			str_replace_all("[^a-z0-9\\s-]", "")
	) %>%
	filter(!is.na(tissue_token), tissue_token != "", !is_missing_tissue(tissue_token)) %>%
	distinct(paper_id, tissue_token)

raw_counts <- paper_tissue %>%
	count(tissue_token, name = "study_count", sort = TRUE)

write_csv(raw_counts, RAW_COUNTS_FILE)

# Canonical plant tissue-part categories.
paper_tissue_plant <- paper_tissue %>%
	mutate(
		tissue_part = case_when(
			str_detect(tissue_token, "leaf|foliar|foliage|needle|phylloplane") ~ "Leaf",
			str_detect(tissue_token, "root|rhizosphere|rhizoplane") ~ "Root",
			str_detect(tissue_token, "stem|wood|bark|caulosphere|phloem|cambial") ~ "Stem/Wood/Bark",
			str_detect(tissue_token, "seed") ~ "Seed",
			str_detect(tissue_token, "fruit") ~ "Fruit",
			str_detect(tissue_token, "flower|inflorescence|reproductive") ~ "Flower/Reproductive",
			str_detect(tissue_token, "tuber") ~ "Tuber",
			str_detect(tissue_token, "rhizome") ~ "Rhizome",
			str_detect(tissue_token, "nodule") ~ "Nodule",
			TRUE ~ NA_character_
		)
	) %>%
	filter(!is.na(tissue_part)) %>%
	distinct(paper_id, tissue_part)

plant_counts <- paper_tissue_plant %>%
	count(tissue_part, name = "study_count", sort = TRUE)

write_csv(plant_counts, PLANT_COUNTS_FILE)

top_n_raw <- 20
raw_plot_data <- raw_counts %>%
	slice_head(n = top_n_raw) %>%
	mutate(tissue_token = fct_reorder(tissue_token, study_count))

raw_plot <- ggplot(raw_plot_data, aes(x = tissue_token, y = study_count)) +
	geom_col(fill = endo_palette_discrete[1], width = 0.8) +
	geom_text(aes(label = study_count), hjust = -0.1, size = 3.2) +
	coord_flip(clip = "off") +
	theme_endo_bw(base_size = 12) +
	theme(
		plot.title = element_text(face = "bold")
	) +
	labs(
		title = "Top tissue terms by number of studies",
		subtitle = "Each study contributes at most one count per tissue term",
		x = "Tissue term",
		y = "Number of studies"
	)

ggsave(RAW_PLOT_FILE, raw_plot, width = 10, height = 8, dpi = 300)

plant_plot_data <- plant_counts %>%
	mutate(tissue_part = fct_reorder(tissue_part, study_count))

plant_plot <- ggplot(plant_plot_data, aes(x = tissue_part, y = study_count)) +
	geom_col(fill = endo_palette_discrete[2], width = 0.8) +
	geom_text(aes(label = study_count), hjust = -0.1, size = 3.5) +
	coord_flip(clip = "off") +
	theme_endo_bw(base_size = 12) +
	theme(
		plot.title = element_text(face = "bold")
	) +
	labs(
		title = "Most frequent plant tissue parts studied for endophytes",
		subtitle = "Each study contributes at most one count per tissue part",
		x = "Plant tissue part",
		y = "Number of studies"
	)

ggsave(PLANT_PLOT_FILE, plant_plot, width = 9, height = 6, dpi = 300)

cat("Tissue plotting complete:\n")
cat("  Input rows: ", nrow(df), "\n", sep = "")
cat("  Unique paper x tissue tokens: ", nrow(paper_tissue), "\n", sep = "")
cat("  Raw tissue terms saved to: ", RAW_COUNTS_FILE, "\n", sep = "")
cat("  Plant tissue parts saved to: ", PLANT_COUNTS_FILE, "\n", sep = "")
cat("  Raw plot saved to: ", RAW_PLOT_FILE, "\n", sep = "")
cat("  Plant tissue-part plot saved to: ", PLANT_PLOT_FILE, "\n", sep = "")
