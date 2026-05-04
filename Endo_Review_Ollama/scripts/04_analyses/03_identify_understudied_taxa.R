#!/usr/bin/env Rscript
# =================================================================================
# 03_identify_understudied_taxa.R
# =================================================================================
# Purpose: To identify plant taxa (families, genera, species) and countries that
#          are not represented in the endophyte literature dataset.
#
# Output:
#   - CSV files listing unstudied taxa and countries.
# =================================================================================

library(dplyr)
library(readr)
library(stringr)
library(rnaturalearth)
library(sf)

source("scripts/utils/pipeline_helpers.R")
source("scripts/utils/disputed_territory_parent_iso.R")

# --- Configuration ---
STUDIED_SPECIES_FILE <- "results/taxonomy_analysis/top_studied_plant_species.csv"
COUNTRY_DATA_FILE <- "data/country_enriched_data.csv"
COUNTRIES_ZERO_FILE <- "results/countries_study_count_zero.csv"
GBIF_TAXON_FILE <- "data/Reference_datasets/gbif_backbone/Taxon.tsv"
OUTPUT_DIR <- "results/understudied_analysis"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

normalize_name <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_squish()
}

load_known_species_reference <- function() {
  # Prefer the species-reference cache from 02_taxonomy; support legacy and new names.
  cache_candidates <- list(
    c(file.path(CACHE_DIR, "gbif_reference_species.qs"), file.path(CACHE_DIR, "gbif_reference_species.rds")),
    c(file.path(CACHE_DIR, "Taxon_reference_species.qs"), file.path(CACHE_DIR, "Taxon_reference_species.rds"))
  )

  for (candidate in cache_candidates) {
    qs_path <- candidate[[1]]
    rds_path <- candidate[[2]]
    if (file.exists(qs_path) || file.exists(rds_path)) {
      cat("Loading known species reference from cache...\n")
      obj <- cache_read_object(qs_path, rds_path)
      required_cols <- c("canonicalName", "genus", "family")
      if (all(required_cols %in% names(obj))) {
        return(obj %>% select(all_of(required_cols)) %>% distinct())
      }
      cat("Cache found but missing required columns (canonicalName/genus/family); falling back to raw GBIF Taxon.tsv\n")
      break
    }
  }

  # Fallback path: read minimal columns directly from GBIF Taxon.tsv.
  if (!file.exists(GBIF_TAXON_FILE)) {
    stop("Could not load known species reference. Neither usable cache nor GBIF Taxon.tsv was found.")
  }

  cat("Building known species reference from GBIF Taxon.tsv (fallback)...\n")
  read_tsv(
    GBIF_TAXON_FILE,
    show_col_types = FALSE,
    progress = FALSE,
    col_select = all_of(c("kingdom", "taxonRank", "taxonomicStatus", "canonicalName", "genus", "family"))
  ) %>%
    mutate(
      kingdom = str_squish(kingdom),
      taxonRank = str_to_upper(str_squish(taxonRank)),
      taxonomicStatus = str_to_lower(str_squish(taxonomicStatus))
    ) %>%
    filter(
      kingdom == "Plantae",
      taxonRank == "SPECIES",
      taxonomicStatus == "accepted",
      !is.na(canonicalName),
      canonicalName != ""
    ) %>%
    select(canonicalName, genus, family) %>%
    distinct()
}

# --- 1. Identify Unstudied Plant Taxa ---

cat("--- Identifying understudied plant taxa ---\n")

all_known_taxa <- load_known_species_reference()

# Load the list of species studied in the dataset
cat("Loading studied species data...\n")
studied_taxa <- read_csv(STUDIED_SPECIES_FILE, show_col_types = FALSE)

required_studied_cols <- c("canonicalName", "genus", "family")
missing_studied_cols <- setdiff(required_studied_cols, names(studied_taxa))
if (length(missing_studied_cols) > 0) {
  stop("Studied species file is missing required columns: ", paste(missing_studied_cols, collapse = ", "))
}

# Normalize names before setdiff so matching is robust to case/whitespace differences.
known_species_df <- all_known_taxa %>%
  filter(!is.na(canonicalName), canonicalName != "") %>%
  transmute(label = canonicalName, key = normalize_name(canonicalName)) %>%
  filter(!is.na(key), key != "") %>%
  distinct(key, .keep_all = TRUE)

known_genera_df <- all_known_taxa %>%
  filter(!is.na(genus), genus != "") %>%
  transmute(label = genus, key = normalize_name(genus)) %>%
  filter(!is.na(key), key != "") %>%
  distinct(key, .keep_all = TRUE)

known_families_df <- all_known_taxa %>%
  filter(!is.na(family), family != "") %>%
  transmute(label = family, key = normalize_name(family)) %>%
  filter(!is.na(key), key != "") %>%
  distinct(key, .keep_all = TRUE)

studied_species_keys <- studied_taxa %>%
  filter(!is.na(canonicalName), canonicalName != "") %>%
  transmute(key = normalize_name(canonicalName)) %>%
  distinct() %>%
  pull(key)

studied_genera_keys <- studied_taxa %>%
  filter(!is.na(genus), genus != "") %>%
  transmute(key = normalize_name(genus)) %>%
  distinct() %>%
  pull(key)

studied_families_keys <- studied_taxa %>%
  filter(!is.na(family), family != "") %>%
  transmute(key = normalize_name(family)) %>%
  distinct() %>%
  pull(key)

# Get unique known/studied counts
known_families <- known_families_df$key
known_genera <- known_genera_df$key
known_species <- known_species_df$key

cat("Total known plant families in GBIF:", length(known_families), "\n")
cat("Total known plant genera in GBIF:", length(known_genera), "\n")
cat("Total known plant species in GBIF:", length(known_species), "\n")

studied_families <- studied_families_keys
studied_genera <- studied_genera_keys
studied_species <- studied_species_keys

cat("Studied plant families:", length(studied_families), "\n")
cat("Studied plant genera:", length(studied_genera), "\n")
cat("Studied plant species:", length(studied_species), "\n")

# Find the difference and map back to readable labels
unstudied_families <- known_families_df %>%
  filter(!key %in% studied_families) %>%
  transmute(family = label) %>%
  arrange(family)

unstudied_genera <- known_genera_df %>%
  filter(!key %in% studied_genera) %>%
  transmute(genus = label) %>%
  arrange(genus)

unstudied_species <- known_species_df %>%
  filter(!key %in% studied_species) %>%
  transmute(species = label) %>%
  arrange(species)

# Save to CSV
write_csv(unstudied_families, file.path(OUTPUT_DIR, "unstudied_plant_families.csv"))
write_csv(unstudied_genera, file.path(OUTPUT_DIR, "unstudied_plant_genera.csv"))
write_csv(unstudied_species, file.path(OUTPUT_DIR, "unstudied_plant_species.csv"))

cat("Saved unstudied families to:", file.path(OUTPUT_DIR, "unstudied_plant_families.csv"), "\n")
cat("Saved unstudied genera to:", file.path(OUTPUT_DIR, "unstudied_plant_genera.csv"), "\n")
cat("Saved unstudied species to:", file.path(OUTPUT_DIR, "unstudied_plant_species.csv"), "\n")

# --- 2. Identify Unstudied Countries ---
# This logic is also in geographic_plotting.R, but consolidating here for a dedicated output.

cat("\n--- Identifying unstudied countries ---\n")

# Prefer reusing existing zero-study output from geographic analysis if present.
if (file.exists(COUNTRIES_ZERO_FILE)) {
  cat("Reusing existing zero-study countries output from geographic analysis...\n")
  zero_countries <- read_csv(COUNTRIES_ZERO_FILE, show_col_types = FALSE)
  unstudied_countries <- zero_countries %>%
    select(any_of(c("iso_a3", "name", "data_status"))) %>%
    distinct()
} else {
  # Fallback: recompute from country_enriched_data and world country universe.
  world <- ne_countries(scale = 110, returnclass = "sf") %>%
    select(iso_a3, name) %>%
    apply_disputed_parent_iso_world() %>%
    st_drop_geometry() %>%
    filter(iso_a3 != "-99") %>%
    distinct(iso_a3, name)

  studied_countries_data <- read_csv(COUNTRY_DATA_FILE, show_col_types = FALSE)
  studied_countries_iso <- studied_countries_data %>%
    filter(study_count > 0) %>%
    distinct(iso_a3) %>%
    pull(iso_a3)

  unstudied_countries <- world %>%
    filter(!iso_a3 %in% studied_countries_iso)
}

write_csv(unstudied_countries, file.path(OUTPUT_DIR, "unstudied_countries.csv"))
cat("Saved unstudied countries to:", file.path(OUTPUT_DIR, "unstudied_countries.csv"), "\n")

cat("\nAnalysis of understudied entities complete.\n")
