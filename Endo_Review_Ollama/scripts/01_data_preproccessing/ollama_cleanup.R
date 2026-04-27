# Gotta do some cleanup

library(tidyverse)

ds <- read.csv("data/Ollama_extraction_all.csv")

missing_tokens <- c(
	"",
	"na",
	"n/a",
	"none",
	"not provided",
	"not specified",
	"unknown",
	"not applicable"
)

normalize_text <- function(x) {
	x_norm <- x %>%
		str_squish() %>%
		str_to_lower()

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
				relevance %in% c("relevant", "releant", "relelevant", "relevance criteria met") ~ "Relevant",
				relevance %in% c("potentially relevant", "relevance uncertain", "uncertain") ~ "Uncertain",
				relevance %in% c("not relevant", "irrelevant") ~ "Irrelevant",
				str_detect(relevance, "relev") &
					!str_detect(relevance, "not") &
					!str_detect(relevance, "uncertain") ~ "Relevant",
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
				str_detect(doc_type_ai, "not applicable|not specified|not provided|unknown|vulnerability disclosure") ~ "Unknown",
				TRUE ~ "Other Document"
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
			page_count_clean = readr::parse_integer(as.character(page_count), na = c("", "na", "n/a"))
		)
}

make_qa_report <- function(df) {
	tibble(
		column = c("relevance", "doc_type_ai", "presence_absence", "doi", "page_count"),
		raw_distinct = c(
			n_distinct(df$relevance_raw, na.rm = TRUE),
			n_distinct(df$doc_type_ai_raw, na.rm = TRUE),
			n_distinct(df$presence_absence, na.rm = TRUE),
			n_distinct(df$doi, na.rm = TRUE),
			n_distinct(df$page_count, na.rm = TRUE)
		),
		clean_distinct = c(
			n_distinct(df$relevance, na.rm = TRUE),
			n_distinct(df$doc_type_ai_clean, na.rm = TRUE),
			n_distinct(df$presence_absence_clean, na.rm = TRUE),
			n_distinct(df$doi_clean, na.rm = TRUE),
			n_distinct(df$page_count_clean, na.rm = TRUE)
		),
		clean_na = c(
			sum(is.na(df$relevance)),
			sum(is.na(df$doc_type_ai_clean)),
			sum(is.na(df$presence_absence_clean)),
			sum(is.na(df$doi_clean)),
			sum(is.na(df$page_count_clean))
		)
	)
}

ds_clean <- clean_all_columns(ds)
qa_report <- make_qa_report(ds_clean)

# Key post-clean checks.
ds_clean %>% count(relevance, sort = TRUE)
ds_clean %>% count(doc_type_ai_clean, sort = TRUE)
ds_clean %>% count(presence_absence_clean, sort = TRUE)
qa_report

write.csv(ds_clean, "data/Ollama_cleaned.csv", row.names = FALSE)
