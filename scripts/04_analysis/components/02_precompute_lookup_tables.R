# =============================================================================
# 02_precompute_lookup_tables.R - Pre-compute expensive lookup tables for HPC
# =============================================================================
#
# Purpose: Create and save lookup tables once to avoid recreating them every run
#
# Description: This script pre-computes the expensive lookup tables including hash tables
# for large species datasets and saves them to disk for reuse in the main HPC script.
#
# Usage: Run this once before running the main HPC script
#
# Outputs: lookup_tables.rds, species_hash.rds, genus_hash.rds, family_hash.rds
#
# =============================================================================

library(tidyverse)
library(janitor)

# Source required functions
source("optimized_taxa_detection.R")
source("reference_data_utils.R")

# Enhanced logging for pre-computation
precompute_log <- function(message) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  cat(paste0("[", timestamp, "] ", message, "\n"))

  # Also write to log file
  write(paste0("[", timestamp, "] ", message), file = "lookup_tables_creation.log", append = TRUE)
}

precompute_log("🚀 === PRE-COMPUTING LOOKUP TABLES ===")

# =============================================================================
# LOAD SPECIES DATA
# =============================================================================

precompute_log("📂 Loading species reference data...")
if (!file.exists("species.rds")) {
  stop("❌ species.rds not found in root directory!")
}

species <- readRDS("species.rds")
precompute_log(paste0("✅ Loaded ", nrow(species), " species records"))

# =============================================================================
# CREATE BASE LOOKUP TABLES
# =============================================================================

precompute_log("🔍 Creating base lookup tables...")
memory_before <- sum(gc()[, 2]) / 1024

lookup_tables <- create_lookup_tables(species)

memory_after <- sum(gc()[, 2]) / 1024
memory_used <- memory_after - memory_before

precompute_log(paste0("✅ Base lookup tables created (", round(memory_used, 2), "GB)"))

# =============================================================================
# CREATE HPC-OPTIMIZED HASH TABLES
# =============================================================================

precompute_log("🚀 Creating HPC-optimized hash tables (this may take several minutes)...")

# Use a moderate threshold to create hash tables
hash_threshold <- 25000  # Lower than default to ensure hash tables are created

if (nrow(species) > hash_threshold) {
  precompute_log("   Creating species hash table...")
  start_time <- Sys.time()

  if (!is.null(lookup_tables$species_names_vector)) {
    species_env <- new.env(hash = TRUE, size = length(lookup_tables$species_names_vector) * 1.5)
    for (name in lookup_tables$species_names_vector) {
      species_env[[name]] <- TRUE
    }
    lookup_tables$species_hash <- species_env
    precompute_log(paste0("   ✅ Species hash: ", length(lookup_tables$species_names_vector), " entries"))
  }

  species_hash_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  precompute_log(paste0("   ⏱️ Species hash completed in ", round(species_hash_time, 1), " seconds"))

  # Genus hash
  if (!is.null(lookup_tables$genus_names_vector)) {
    precompute_log("   Creating genus hash table...")
    genus_start <- Sys.time()

    genus_env <- new.env(hash = TRUE, size = length(lookup_tables$genus_names_vector) * 1.5)
    for (name in lookup_tables$genus_names_vector) {
      genus_env[[name]] <- TRUE
    }
    lookup_tables$genus_hash <- genus_env

    genus_hash_time <- as.numeric(difftime(Sys.time(), genus_start, units = "secs"))
    precompute_log(paste0("   ✅ Genus hash: ", length(lookup_tables$genus_names_vector), " entries (", round(genus_hash_time, 1), "s)"))
  }

  # Family hash
  if (!is.null(lookup_tables$family_names_vector)) {
    precompute_log("   Creating family hash table...")
    family_start <- Sys.time()

    family_env <- new.env(hash = TRUE, size = length(lookup_tables$family_names_vector) * 1.5)
    for (name in lookup_tables$family_names_vector) {
      family_env[[name]] <- TRUE
    }
    lookup_tables$family_hash <- family_env

    family_hash_time <- as.numeric(difftime(Sys.time(), family_start, units = "secs"))
    precompute_log(paste0("   ✅ Family hash: ", length(lookup_tables$family_names_vector), " entries (", round(family_hash_time, 1), "s)"))
  }
} else {
  precompute_log("   ℹ️ Dataset too small for hash tables, skipping...")
}

# =============================================================================
# SAVE LOOKUP TABLES FOR REUSE
# =============================================================================

precompute_log("💾 Saving lookup tables to disk...")

# Save complete lookup tables
saveRDS(lookup_tables, "lookup_tables.rds")
file_size_mb <- file.size("lookup_tables.rds") / (1024*1024)
precompute_log(paste0("✅ Saved lookup_tables.rds (", round(file_size_mb, 1), "MB)"))

# Save individual components for memory efficiency
if (!is.null(lookup_tables$species_hash)) {
  saveRDS(lookup_tables$species_hash, "species_hash.rds")
  precompute_log("✅ Saved species_hash.rds")
}

if (!is.null(lookup_tables$genus_hash)) {
  saveRDS(lookup_tables$genus_hash, "genus_hash.rds")
  precompute_log("✅ Saved genus_hash.rds")
}

if (!is.null(lookup_tables$family_hash)) {
  saveRDS(lookup_tables$family_hash, "family_hash.rds")
  precompute_log("✅ Saved family_hash.rds")
}

# Save vector versions for quick access
if (!is.null(lookup_tables$species_names_vector)) {
  saveRDS(lookup_tables$species_names_vector, "species_names_vector.rds")
  precompute_log("✅ Saved species_names_vector.rds")
}

if (!is.null(lookup_tables$genus_names_vector)) {
  saveRDS(lookup_tables$genus_names_vector, "genus_names_vector.rds")
  precompute_log("✅ Saved genus_names_vector.rds")
}

if (!is.null(lookup_tables$family_names_vector)) {
  saveRDS(lookup_tables$family_names_vector, "family_names_vector.rds")
  precompute_log("✅ Saved family_names_vector.rds")
}

# =============================================================================
# SUMMARY
# =============================================================================

precompute_log("🎉 === LOOKUP TABLE PRE-COMPUTATION COMPLETED ===")
precompute_log("")
precompute_log("📋 Files created for reuse:")
precompute_log("   • lookup_tables.rds (complete)")
precompute_log("   • species_hash.rds")
precompute_log("   • genus_hash.rds")
precompute_log("   • family_hash.rds")
precompute_log("   • species_names_vector.rds")
precompute_log("   • genus_names_vector.rds")
precompute_log("   • family_names_vector.rds")
precompute_log("")
precompute_log("🚀 Ready to run HPC script with pre-computed lookup tables!")
precompute_log("   This should eliminate the 60+ second bottleneck in your main script.")

# =============================================================================
# RUN PRE-COMPUTATION
# =============================================================================

cat("\n🚀 Starting lookup table pre-computation...\n")
cat("This may take several minutes but only needs to be done once.\n\n")

# Execute the pre-computation
precompute_log("🚀 Starting lookup table pre-computation...")

# Load species data
species <- readRDS("species.rds")

# Create and save lookup tables
lookup_tables <- create_lookup_tables(species)

# Create HPC-optimized hash tables for large datasets
if (nrow(species) > hash_threshold) {
  precompute_log("   Creating hash tables for large dataset...")

  if (!is.null(lookup_tables$species_names_vector)) {
    species_env <- new.env(hash = TRUE, size = length(lookup_tables$species_names_vector) * 1.5)
    for (name in lookup_tables$species_names_vector) {
      species_env[[name]] <- TRUE
    }
    lookup_tables$species_hash <- species_env
  }

  if (!is.null(lookup_tables$genus_names_vector)) {
    genus_env <- new.env(hash = TRUE, size = length(lookup_tables$genus_names_vector) * 1.5)
    for (name in lookup_tables$genus_names_vector) {
      genus_env[[name]] <- TRUE
    }
    lookup_tables$genus_hash <- genus_env
  }

  if (!is.null(lookup_tables$family_names_vector)) {
    family_env <- new.env(hash = TRUE, size = length(lookup_tables$family_names_vector) * 1.5)
    for (name in lookup_tables$family_names_vector) {
      family_env[[name]] <- TRUE
    }
    lookup_tables$family_hash <- family_env
  }
}

# Save all the lookup tables
saveRDS(lookup_tables, "lookup_tables.rds")

if (!is.null(lookup_tables$species_hash)) {
  saveRDS(lookup_tables$species_hash, "species_hash.rds")
}

if (!is.null(lookup_tables$genus_hash)) {
  saveRDS(lookup_tables$genus_hash, "genus_hash.rds")
}

if (!is.null(lookup_tables$family_hash)) {
  saveRDS(lookup_tables$family_hash, "family_hash.rds")
}

if (!is.null(lookup_tables$species_names_vector)) {
  saveRDS(lookup_tables$species_names_vector, "species_names_vector.rds")
}

if (!is.null(lookup_tables$genus_names_vector)) {
  saveRDS(lookup_tables$genus_names_vector, "genus_names_vector.rds")
}

if (!is.null(lookup_tables$family_names_vector)) {
  saveRDS(lookup_tables$family_names_vector, "family_names_vector.rds")
}

precompute_log("✅ Lookup table pre-computation completed!")