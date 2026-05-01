#!/usr/bin/env Rscript
# =================================================================================
# taxonomy_representation.R
# =================================================================================
# Purpose: Create publication-ready plots showing absolute and relative 
#          representation of plant taxa (species, genera, families) by phylum
#
# Output plots:
#   - Absolute counts: known vs. studied taxa by phylum
#   - Relative coverage: percentage of known taxa that are studied by phylum
#   - Separate visualizations for species, genera, and families
#
# Usage: Rscript scripts/05_plotting/taxonomy_representation.R
# =================================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

theme_utils_paths <- c(
  "scripts/05_plotting/theme_utils.R",
  "scripts/plotting/theme_utils.R"
)
for (theme_utils_path in theme_utils_paths) {
  if (file.exists(theme_utils_path)) {
    source(theme_utils_path)
    break
  }
}

# Configuration
TAXONOMY_RESULTS_DIR <- "results/taxonomy_analysis"
PLOTS_OUTPUT_DIR <- "plots/taxonomy"
TAXONOMY_LEVELS <- list(
  species = "plant_species_coverage_by_phylum.csv",
  genus = "plant_genus_coverage_by_phylum.csv",
  family = "plant_family_coverage_by_phylum.csv"
)

# Create output directory
dir.create(PLOTS_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =================================================================================
# DATA LOADING AND PREPARATION
# =================================================================================

#' Load and prepare taxonomy coverage data
#' @param filename CSV filename in TAXONOMY_RESULTS_DIR
#' @param level_name Name of the taxonomic level for labeling
#' @param phylum_order Optional factor levels for consistent phylum ordering
#' @return Prepared data frame ready for plotting
load_taxonomy_data <- function(filename, level_name, phylum_order = NULL) {
  file_path <- file.path(TAXONOMY_RESULTS_DIR, filename)
  
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  data <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(level = level_name) %>%
    # Exclude unassigned phyla
    filter(phylum != "Unassigned")
  
  # Identify the known/studied column pair for this level
  known_col <- colnames(data)[grepl("^known_", colnames(data))]
  
  # Only filter if we have a valid known column
  if (length(known_col) > 0) {
    data <- data %>%
      filter(.data[[known_col]] > 0)
  }
  
  # Apply consistent phylum ordering if provided
  if (!is.null(phylum_order)) {
    data <- data %>%
      mutate(phylum = factor(phylum, levels = phylum_order))
  }
  
  return(data)
}

# Load all three levels first to establish consistent ordering
cat("Loading taxonomy coverage data...\n")
species_data_raw <- read_csv(
  file.path(TAXONOMY_RESULTS_DIR, TAXONOMY_LEVELS$species), 
  show_col_types = FALSE
) %>%
  filter(phylum != "Unassigned")

# Create consistent phylum ordering based on total known species (most to least)
phylum_order <- species_data_raw %>%
  arrange(desc(known_species)) %>%
  pull(phylum)

cat("Phylum ordering (by total known species, most to least):\n")
cat(paste0("  ", seq_along(phylum_order), ". ", phylum_order, "\n"), sep="")

# Load all three levels with consistent ordering
species_data <- load_taxonomy_data(
  TAXONOMY_LEVELS$species, 
  "Species",
  phylum_order = phylum_order
)
genus_data <- load_taxonomy_data(
  TAXONOMY_LEVELS$genus, 
  "Genus",
  phylum_order = phylum_order
)
family_data <- load_taxonomy_data(
  TAXONOMY_LEVELS$family, 
  "Family",
  phylum_order = phylum_order
)

# =================================================================================
# ABSOLUTE REPRESENTATION PLOTS
# =================================================================================

#' Create absolute representation plot (known vs studied)
#' @param data Data frame with coverage data
#' @param title Plot title
#' @param taxon_label Label for the taxon count (e.g., "Species", "Genera")
#' @param known_col Name of known taxa column
#' @param studied_col Name of studied taxa column
#' @return ggplot object
plot_absolute_representation <- function(data, title, taxon_label, 
                                        known_col, studied_col) {
  # Prepare data for stacked bar plot
  plot_data <- data %>%
    select(phylum, all_of(known_col), all_of(studied_col)) %>%
    rename(Known = all_of(known_col), Studied = all_of(studied_col)) %>%
    # Calculate unstudied as the difference
    mutate(`Not Studied` = Known - Studied) %>%
    select(phylum, Studied, `Not Studied`) %>%
    pivot_longer(
      cols = c(Studied, `Not Studied`),
      names_to = "Type",
      values_to = "Count"
    ) %>%
    mutate(Type = factor(Type, levels = c("Studied", "Not Studied")))
  
  plot <- ggplot(plot_data, aes(x = phylum, y = Count, fill = Type)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(
      values = c("Studied" = "#0072B2", "Not Studied" = "#CCCCCC"),
      name = "Category"
    ) +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = title,
      x = "Phylum",
      y = paste("Number of", tolower(taxon_label)),
      subtitle = paste("Composition of known plant", tolower(taxon_label), "by study coverage")
    ) +
    theme_endo_bw(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )
  
  return(plot)
}

#' Create relative representation plot (coverage percentage)
#' @param data Data frame with coverage data
#' @param title Plot title
#' @param taxon_label Label for the taxon (e.g., "Species", "Genera")
#' @return ggplot object
plot_relative_representation <- function(data, title, taxon_label) {
  # Rename coverage column for consistency
  coverage_cols <- colnames(data)[grepl("coverage", colnames(data))]
  
  plot_data <- data %>%
    select(phylum, all_of(coverage_cols)) %>%
    rename(coverage_raw = all_of(coverage_cols[1]))
  
  # Detect if coverage is in 0-1 range (decimal) or 0-100 range (percentage)
  max_coverage <- max(plot_data$coverage_raw, na.rm = TRUE)
  
  plot_data <- plot_data %>%
    mutate(
      coverage_percent = if (max_coverage < 2) {
        pmin(coverage_raw * 100, 100)  # 0-1 range: multiply by 100
      } else {
        pmin(coverage_raw, 100)         # 0-100 range: use as-is, cap at 100
      }
    )

  coverage_upper <- ceiling(max(plot_data$coverage_percent, na.rm = TRUE) / 5) * 5
  if (coverage_upper <= 0 || is.na(coverage_upper)) {
    coverage_upper <- 1
  }
  
  plot <- ggplot(plot_data, aes(x = phylum, y = coverage_percent, fill = coverage_percent)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_gradient(
      low = "#E69F00",
      high = "#009E73",
      name = "Coverage (%)",
      limits = c(0, coverage_upper)
    ) +
    scale_y_continuous(
      limits = c(0, coverage_upper),
      expand = expansion(mult = c(0, 0.05)),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      x = "Phylum",
      y = "Coverage (%)",
      subtitle = paste("Percentage of known plant", tolower(taxon_label), "that are studied")
    ) +
    theme_endo_bw(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.major.x = element_blank(),
      legend.position = "right"
    )
  
  return(plot)
}

# =================================================================================
# GENERATE PLOTS
# =================================================================================

cat("Generating absolute representation plots...\n")

# Species plots
p_species_abs <- plot_absolute_representation(
  species_data,
  "Plant Species: Known vs. Studied by Phylum",
  "Species",
  "known_species",
  "studied_species"
)

p_species_rel <- plot_relative_representation(
  species_data,
  "Plant Species: Study Coverage by Phylum",
  "Species"
)

# Genus plots
p_genus_abs <- plot_absolute_representation(
  genus_data,
  "Plant Genera: Known vs. Studied by Phylum",
  "Genera",
  "known_genera",
  "studied_genera"
)

p_genus_rel <- plot_relative_representation(
  genus_data,
  "Plant Genera: Study Coverage by Phylum",
  "Genera"
)

# Family plots
p_family_abs <- plot_absolute_representation(
  family_data,
  "Plant Families: Known vs. Studied by Phylum",
  "Families",
  "known_families",
  "studied_families"
)

p_family_rel <- plot_relative_representation(
  family_data,
  "Plant Families: Study Coverage by Phylum",
  "Families"
)

# =================================================================================
# SAVE PLOTS
# =================================================================================

save_plot <- function(plot, filename, width = 10, height = 6) {
  filepath <- file.path(PLOTS_OUTPUT_DIR, filename)
  ggsave(
    filepath,
    plot,
    width = width,
    height = height,
    dpi = 300,
    units = "in"
  )
  cat("Saved:", filepath, "\n")
}

cat("Saving plots to", PLOTS_OUTPUT_DIR, "\n")

save_plot(p_species_abs, "01_species_absolute_representation.png", width = 11, height = 7)
save_plot(p_species_rel, "02_species_relative_representation.png", width = 11, height = 7)

save_plot(p_genus_abs, "03_genera_absolute_representation.png", width = 11, height = 7)
save_plot(p_genus_rel, "04_genera_relative_representation.png", width = 11, height = 7)

save_plot(p_family_abs, "05_families_absolute_representation.png", width = 11, height = 7)
save_plot(p_family_rel, "06_families_relative_representation.png", width = 11, height = 7)

# =================================================================================
# PRINT SUMMARY STATISTICS
# =================================================================================

cat("\n=================================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("=================================================================================\n\n")

print_summary <- function(data, level_name, known_col, studied_col) {
  cat("--- ", level_name, " ---\n")
  cat("Total known:", sum(data[[known_col]]), "\n")
  cat("Total studied:", sum(data[[studied_col]]), "\n")
  cat("Overall coverage:", 
      round(sum(data[[studied_col]]) / sum(data[[known_col]]) * 100, 2), "%\n")
  cat("Number of phyla:", nrow(data), "\n\n")
}

print_summary(species_data, "SPECIES", "known_species", "studied_species")
print_summary(genus_data, "GENERA", "known_genera", "studied_genera")
print_summary(family_data, "FAMILIES", "known_families", "studied_families")

cat("=================================================================================\n")
cat("Plots saved to:", PLOTS_OUTPUT_DIR, "\n")
cat("=================================================================================\n")
