#!/usr/bin/env Rscript
# =================================================================================
# 04_interaction_bias.R
# =================================================================================
# Purpose: Analyze relationships between different metadata categories to reveal 
#          complex interaction biases (e.g., Fungal Phylum vs Continent).
#
# Analyses:
#   - Fungal Phylum x Continent Heatmap
#   - Plant Phylum x Continent Heatmap
#   - Fungal Phylum x Tissue Heatmap
#   - Plant Family representation bias
#
# Outputs:
#   - results/interaction_analysis/ (various CSVs and PNGs)
#
# Usage: Rscript scripts/04_analyses/04_interaction_bias.R
# =================================================================================

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(rnaturalearth)
library(sf)

source("scripts/utils/pipeline_helpers.R")
source("scripts/05_plotting/theme_utils.R")

INPUT_FILE <- "data/Ollama_cleaned_synresolved_standardized_final.csv"
COUNTRY_FILE <- "data/country_enriched_data.csv"
OUTPUT_DIR <- "results/interaction_analysis"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# The cache_read_object function is now defined in pipeline_helpers.R

if (!file.exists(INPUT_FILE)) stop("Input file not found: ", INPUT_FILE)

message("Loading interaction data...")
df <- read_csv(INPUT_FILE, show_col_types = FALSE)

# We also need the country-to-continent mapping from the enriched data
country_df <- read_csv(COUNTRY_FILE, show_col_types = FALSE) %>%
  select(country_name, iso_a3) %>% distinct()

# To get continents, we can use rnaturalearth
world <- ne_countries(scale = 50, returnclass = "sf") %>%
  sf::st_drop_geometry() %>%
  select(iso_a3, continent)

country_map <- country_df %>%
  left_join(world, by = "iso_a3") %>%
  mutate(continent = replace_na(continent, "Unknown")) %>%
  select(country = country_name, continent) %>% distinct()

message("Loading minimal GBIF taxonomy for Phyla from cache...")
gbif_qs_path <- file.path(CACHE_DIR, "gbif_taxa_min.qs")
gbif_rds_path <- file.path(CACHE_DIR, "gbif_taxa_min.rds")
gbif_min <- cache_read_object(gbif_qs_path, gbif_rds_path) %>%
  select(taxonID, kingdom, phylum) %>%
  mutate(
    taxonID = as.character(taxonID),
    phylum = ifelse(is.na(phylum) | phylum == "", "Unassigned", phylum)
  )

# Extract first accepted ID for simple joining
df_clean <- df %>%
  mutate(
    fungal_id = str_extract(fungal_taxon_accepted_ids, "^[^;]+"),
    plant_id = str_extract(plant_host_accepted_ids, "^[^;]+")
  ) %>%
  left_join(gbif_min %>% rename(fungal_phylum = phylum, f_king = kingdom), by = c("fungal_id" = "taxonID")) %>%
  left_join(gbif_min %>% rename(plant_phylum = phylum, p_king = kingdom), by = c("plant_id" = "taxonID")) %>%
  left_join(country_map, by = "country") %>%
  mutate(
    fungal_phylum = replace_na(fungal_phylum, "Unknown Fungi"),
    plant_phylum = replace_na(plant_phylum, "Unknown Plant"),
    continent = replace_na(continent, "Unknown"),
    tissue = replace_na(tissue, "Unknown")
  )

# --- 1. Fungal Phylum vs Continent ---
message("Analyzing Fungal Phylum x Continent...")

fungal_continent <- df_clean %>%
  filter(!is.na(paper_id), fungal_phylum != "Unknown Fungi", continent != "Unknown") %>%
  distinct(paper_id, fungal_phylum, continent) %>%
  count(fungal_phylum, continent, name = "study_count")

# Keep top fungal phyla to avoid clutter
top_fungal <- fungal_continent %>% group_by(fungal_phylum) %>% summarize(total = sum(study_count)) %>% top_n(5, total) %>% pull(fungal_phylum)

fc_plot_data <- fungal_continent %>%
  filter(fungal_phylum %in% top_fungal) %>%
  group_by(continent) %>%
  mutate(prop = study_count / sum(study_count))

fc_plot <- ggplot(fc_plot_data, aes(x = continent, y = fungal_phylum, fill = study_count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = study_count), color = ifelse(fc_plot_data$study_count > max(fc_plot_data$study_count)/2, "white", "black")) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  theme_endo_bw() +
  labs(title = "Fungal Phyla Studied by Continent", x = "Continent", y = "Fungal Phylum", fill = "Studies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "fungal_phylum_vs_continent.png"), fc_plot, width = 8, height = 6)
write_csv(fungal_continent, file.path(OUTPUT_DIR, "fungal_phylum_vs_continent.csv"))

# --- 2. Fungal Phylum vs Tissue ---
message("Analyzing Fungal Phylum x Tissue...")

# Standardize tissues a bit for the plot
df_clean <- df_clean %>%
  mutate(tissue_group = case_when(
    str_detect(tolower(tissue), "leaf|foliar") ~ "Leaf",
    str_detect(tolower(tissue), "root|rhizo") ~ "Root",
    str_detect(tolower(tissue), "stem|wood|bark|shoot") ~ "Stem/Wood",
    str_detect(tolower(tissue), "seed") ~ "Seed",
    TRUE ~ "Other/Unknown"
  ))

fungal_tissue <- df_clean %>%
  filter(!is.na(paper_id), fungal_phylum != "Unknown Fungi", tissue_group != "Other/Unknown") %>%
  distinct(paper_id, fungal_phylum, tissue_group) %>%
  count(fungal_phylum, tissue_group, name = "study_count")

ft_plot_data <- fungal_tissue %>%
  filter(fungal_phylum %in% top_fungal)

ft_plot <- ggplot(ft_plot_data, aes(x = tissue_group, y = fungal_phylum, fill = study_count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = study_count), color = ifelse(ft_plot_data$study_count > max(ft_plot_data$study_count)/2, "white", "black")) +
  scale_fill_gradient(low = "#f7fcf5", high = "#00441b") +
  theme_endo_bw() +
  labs(title = "Fungal Phyla Studied by Plant Tissue", x = "Tissue Category", y = "Fungal Phylum", fill = "Studies")

ggsave(file.path(OUTPUT_DIR, "fungal_phylum_vs_tissue.png"), ft_plot, width = 8, height = 6)
write_csv(fungal_tissue, file.path(OUTPUT_DIR, "fungal_phylum_vs_tissue.csv"))

# --- 3. Statistical Tests for Interaction Bias ---
message("Running statistical tests...")

# Test 1: Fungal Phylum vs Continent (Chi-Square)
# Are certain fungal phyla disproportionately studied in certain continents?
fc_table <- table(
  factor(df_clean$fungal_phylum[df_clean$fungal_phylum %in% top_fungal]), 
  factor(df_clean$continent[df_clean$continent != "Unknown" & df_clean$fungal_phylum %in% top_fungal])
)

fc_chi <- chisq.test(fc_table, simulate.p.value = TRUE, B = 2000)
fc_stats <- tibble(
  test = "Chi-Square: Fungal Phylum x Continent",
  statistic = fc_chi$statistic,
  p_value = fc_chi$p.value,
  method = fc_chi$method,
  interpretation = ifelse(fc_chi$p.value < 0.05, 
                          "Significant bias: Fungal phyla are not studied equally across continents.", 
                          "No significant bias detected.")
)

# Test 2: Fungal Phylum vs Tissue Category (Chi-Square)
# Are certain fungal phyla disproportionately studied in specific tissues?
ft_table <- table(
  factor(df_clean$fungal_phylum[df_clean$fungal_phylum %in% top_fungal]), 
  factor(df_clean$tissue_group[df_clean$tissue_group != "Other/Unknown" & df_clean$fungal_phylum %in% top_fungal])
)

ft_chi <- chisq.test(ft_table, simulate.p.value = TRUE, B = 2000)
ft_stats <- tibble(
  test = "Chi-Square: Fungal Phylum x Tissue Category",
  statistic = ft_chi$statistic,
  p_value = ft_chi$p.value,
  method = ft_chi$method,
  interpretation = ifelse(ft_chi$p.value < 0.05, 
                          "Significant bias: Fungal phyla are not studied equally across plant tissues.", 
                          "No significant bias detected.")
)

# Combine and save statistical results
all_stats <- bind_rows(fc_stats, ft_stats)
write_csv(all_stats, file.path(OUTPUT_DIR, "interaction_statistical_tests.csv"))

cat("\n=== Statistical Test Summary ===\n")
print(all_stats)

message("Interaction analysis complete. Outputs saved to ", OUTPUT_DIR)
