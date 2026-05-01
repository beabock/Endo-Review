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
library(rnaturalearth)
library(sf)

# --- Configuration ---
CACHE_DIR <- "results/taxonomy_analysis/cache"
STUDIED_SPECIES_FILE <- "results/taxonomy_analysis/top_studied_plant_species.csv"
COUNTRY_DATA_FILE <- "data/country_enriched_data.csv"
OUTPUT_DIR <- "results/understudied_analysis"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Helper function to read cached objects (from 02_taxonomy.R) ---
cache_read_object <- function(qs_path, rds_path) {
  if (file.exists(qs_path)) {
    if (requireNamespace("qs", quietly = TRUE)) {
      cat("Reading from qs cache:", qs_path, "\n")
      return(qs::qread(qs_path))
    } else {
      cat("qs package not available, falling back to RDS for reading.\n")
    }
  }
  if (file.exists(rds_path)) {
    cat("Reading from RDS cache:", rds_path, "\n")
    return(readRDS(rds_path))
  }
  stop("Neither qs nor RDS cache file found.")
}

# --- 1. Identify Unstudied Plant Taxa ---

cat("--- Identifying understudied plant taxa ---\n")

# Load the minimized GBIF plantae dataset from cache
gbif_qs_path <- file.path(CACHE_DIR, "gbif_taxa_min.qs")
gbif_rds_path <- file.path(CACHE_DIR, "gbif_taxa_min.rds")

if (!file.exists(gbif_qs_path) && !file.exists(gbif_rds_path)) {
  stop("GBIF cache not found. Please run 'scripts/04_analyses/02_taxonomy.R' first.")
}
cat("Loading cached GBIF plant data...\n")
all_known_taxa <- cache_read_object(gbif_qs_path, gbif_rds_path)

# Load the list of species studied in the dataset
cat("Loading studied species data...\n")
studied_taxa <- read_csv(STUDIED_SPECIES_FILE, show_col_types = FALSE)

# Get unique known taxa at each level
known_families <- all_known_taxa %>% filter(!is.na(family)) %>% distinct(family) %>% pull(family)
known_genera <- all_known_taxa %>% filter(!is.na(genus)) %>% distinct(genus) %>% pull(genus)
known_species <- all_known_taxa %>% filter(!is.na(canonicalName)) %>% distinct(canonicalName) %>% pull(canonicalName)

cat("Total known plant families in GBIF:", length(known_families), "\n")
cat("Total known plant genera in GBIF:", length(known_genera), "\n")
cat("Total known plant species in GBIF:", length(known_species), "\n")

# Get unique studied taxa at each level
studied_families <- studied_taxa %>% filter(!is.na(family)) %>% distinct(family) %>% pull(family)
studied_genera <- studied_taxa %>% filter(!is.na(genus)) %>% distinct(genus) %>% pull(genus)
studied_species <- studied_taxa %>% filter(!is.na(canonicalName)) %>% distinct(canonicalName) %>% pull(canonicalName)

cat("Studied plant families:", length(studied_families), "\n")
cat("Studied plant genera:", length(studied_genera), "\n")
cat("Studied plant species:", length(studied_species), "\n")

# Find the difference
unstudied_families <- setdiff(known_families, studied_families)
unstudied_genera <- setdiff(known_genera, studied_genera)
unstudied_species <- setdiff(known_species, studied_species)

# Save to CSV
write.csv(data.frame(family = unstudied_families), file.path(OUTPUT_DIR, "unstudied_plant_families.csv"), row.names = FALSE)
write.csv(data.frame(genus = unstudied_genera), file.path(OUTPUT_DIR, "unstudied_plant_genera.csv"), row.names = FALSE)
write.csv(data.frame(species = unstudied_species), file.path(OUTPUT_DIR, "unstudied_plant_species.csv"), row.names = FALSE)

cat("Saved unstudied families to:", file.path(OUTPUT_DIR, "unstudied_plant_families.csv"), "\n")
cat("Saved unstudied genera to:", file.path(OUTPUT_DIR, "unstudied_plant_genera.csv"), "\n")
cat("Saved unstudied species to:", file.path(OUTPUT_DIR, "unstudied_plant_species.csv"), "\n")

# --- 2. Identify Unstudied Countries ---
# This logic is also in geographic_plotting.R, but consolidating here for a dedicated output.

cat("\n--- Identifying unstudied countries ---\n")

# Load world map to get the "universe" of countries
world <- ne_countries(scale = 110, returnclass = "sf") %>%
  select(iso_a3, name) %>%
  st_drop_geometry() %>%
  filter(iso_a3 != "-99")

# Load the list of countries present in our dataset
studied_countries_data <- read_csv(COUNTRY_DATA_FILE, show_col_types = FALSE)
studied_countries_iso <- studied_countries_data %>% 
  filter(study_count > 0) %>%
  distinct(iso_a3) %>%
  pull(iso_a3)

# Find the difference
unstudied_countries <- world %>%
  filter(!iso_a3 %in% studied_countries_iso)

# Save to CSV
write.csv(unstudied_countries, file.path(OUTPUT_DIR, "unstudied_countries.csv"), row.names = FALSE)
cat("Saved unstudied countries to:", file.path(OUTPUT_DIR, "unstudied_countries.csv"), "\n")

cat("\nAnalysis of understudied entities complete.\n")
