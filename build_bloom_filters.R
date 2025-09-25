# Build Bloom Filter Databases for Plants and Fungi
#
# This script creates DuckDB bloom filter databases for fast taxonomic name
# pre-filtering. Run this once during setup or when species data is updated.

library(tidyverse)
source("scripts/04_analysis/bloom_filter_duckdb.R")

message("Loading species data...")

# Load plant species data
if (file.exists("models/species_plants.rds")) {
  plants_df <- readRDS("models/species_plants.rds")
} else {
  stop("Plant species data not found at models/species_plants.rds")
}

# Load fungi species data
if (file.exists("models/species_fungi.rds")) {
  fungi_df <- readRDS("models/species_fungi.rds")
} else {
  stop("Fungi species data not found at models/species_fungi.rds")
}

message(sprintf("Loaded %d plant species and %d fungal species",
                nrow(plants_df), nrow(fungi_df)))

# Build bloom filter databases
build_bloom_filter_databases(
  plants_df = plants_df,
  fungi_df = fungi_df,
  force_rebuild = TRUE  # Force rebuild to ensure latest data
)

message("Bloom filter databases built successfully!")
message("Bloom filters are now available for sub-millisecond taxonomic name filtering.")