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
PLOTS_OUTPUT_DIR <- "results/taxonomy_analysis/plots"
TAXONOMY_LEVELS <- list(
  species = "plant_species_coverage_by_phylum.csv",
  genus = "plant_genus_coverage_by_phylum.csv",
  family = "plant_family_coverage_by_phylum.csv"
)

# Phylum common name mapping
phylum_common_names <- tibble(
  phylum = c("Tracheophyta", "Bryophyta", "Marchantiophyta", "Anthocerotophyta",
             "Rhodophyta", "Chlorophyta", "Charophyta", "Glaucophyta", "Langiophytophyta"),
  common_name = c("Vascular Plants", "Mosses", "Liverworts", "Hornworts",
                  "Red Algae", "Green Algae", "Stoneworts", "Glaucophytes", "Langiophytes")
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
    filter(phylum != "Unassigned") %>%
    left_join(phylum_common_names, by = "phylum")
  
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
# TAXONOMY SUMMARY PLOTS
# =================================================================================

prepare_taxonomy_summary <- function(data, known_col, studied_col) {
  data %>%
    select(phylum, common_name, all_of(known_col), all_of(studied_col)) %>%
    distinct() %>%
    rename(
      known_count = all_of(known_col),
      studied_count = all_of(studied_col)
    ) %>%
    mutate(
      common_name = if_else(is.na(common_name) | common_name == "", phylum, common_name),
      not_studied_count = pmax(known_count - studied_count, 0),
      coverage_percent = if_else(known_count > 0, studied_count / known_count * 100, NA_real_),
      phylum_label = paste0(common_name, " (", phylum, ")\nn=", comma(known_count))
    ) %>%
    distinct(phylum, .keep_all = TRUE)
}

plot_coverage_bar <- function(summary_data, title, taxon_label) {
  plot_data <- summary_data %>%
    arrange(coverage_percent) %>%
    mutate(phylum_label = factor(phylum_label, levels = phylum_label))

  max_coverage <- max(plot_data$coverage_percent, na.rm = TRUE)
  if (is.na(max_coverage) || max_coverage <= 0) {
    max_coverage <- 1
  }

  ggplot(plot_data, aes(x = coverage_percent, y = phylum_label)) +
    geom_col(fill = "#0072B2", width = 0.72) +
    geom_text(
      aes(label = paste0(round(coverage_percent, 1), "%")),
      hjust = -0.12,
      size = 3
    ) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(0, max_coverage * 1.15),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      x = "Coverage (%)",
      y = "Phylum",
      subtitle = paste("Simple coverage view for known plant", tolower(taxon_label), "represented in the literature")
    ) +
    theme_endo_bw(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 9),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(5.5, 24, 5.5, 5.5)
    )
}

plot_studied_count_bar <- function(summary_data, title, taxon_label) {
  plot_data <- summary_data %>%
    arrange(studied_count) %>%
    mutate(phylum_label = factor(phylum_label, levels = phylum_label))

  max_studied <- max(plot_data$studied_count, na.rm = TRUE)
  if (is.na(max_studied) || max_studied <= 0) {
    max_studied <- 1
  }

  ggplot(plot_data, aes(x = studied_count, y = phylum_label)) +
    geom_col(fill = "#009E73", width = 0.72) +
    geom_text(
      aes(label = comma(studied_count)),
      hjust = -0.12,
      size = 3
    ) +
    scale_x_continuous(
      labels = comma,
      limits = c(0, max_studied * 1.15),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      x = paste("Studied", tolower(taxon_label)),
      y = "Phylum",
      subtitle = paste("Absolute number of studied plant", tolower(taxon_label), "by phylum")
    ) +
    theme_endo_bw(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 9),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(5.5, 24, 5.5, 5.5)
    )
}

plot_coverage_lollipop <- function(summary_data, title, taxon_label) {
  plot_data <- summary_data %>%
    arrange(coverage_percent) %>%
    mutate(phylum_label = factor(phylum_label, levels = phylum_label))

  max_coverage <- max(plot_data$coverage_percent, na.rm = TRUE)
  if (is.na(max_coverage) || max_coverage <= 0) {
    max_coverage <- 1
  }

  ggplot(plot_data, aes(x = coverage_percent, y = phylum_label)) +
    geom_segment(
      aes(x = 0, xend = coverage_percent, yend = phylum_label),
      linewidth = 1.1,
      color = "#D0D0D0"
    ) +
    geom_point(size = 3.2, color = "#D55E00") +
    geom_text(
      aes(label = paste0(round(coverage_percent, 1), "%")),
      hjust = -0.12,
      size = 3
    ) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(0, max_coverage * 1.15),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      x = "Coverage (%)",
      y = "Phylum",
      subtitle = paste("Lollipop view of coverage for plant", tolower(taxon_label), "by phylum")
    ) +
    theme_endo_bw(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 9),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(5.5, 24, 5.5, 5.5)
    )
}

plot_taxonomy_heatmap <- function(summary_data, title, taxon_label) {
  long_data <- summary_data %>%
    select(phylum_label, known_count, studied_count, coverage_percent) %>%
    pivot_longer(
      cols = c(known_count, studied_count, coverage_percent),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metric = factor(
        metric,
        levels = c("known_count", "studied_count", "coverage_percent"),
        labels = c("Known", "Studied", "Coverage %")
      )
    ) %>%
    group_by(metric) %>%
    mutate(
      value_scaled = if (all(is.na(value))) {
        NA_real_
      } else if (dplyr::n_distinct(value, na.rm = TRUE) <= 1) {
        0.5
      } else {
        scales::rescale(value, to = c(0, 1), na.rm = TRUE)
      },
      label = case_when(
        metric == "Coverage %" ~ paste0(round(value, 1), "%"),
        TRUE ~ comma(value)
      ),
      text_color = if_else(is.na(value_scaled) | value_scaled < 0.65, "#222222", "white")
    ) %>%
    ungroup()

  ggplot(long_data, aes(x = metric, y = phylum_label, fill = value_scaled)) +
    geom_tile(color = "white", width = 0.92, height = 0.9) +
    geom_text(aes(label = label, color = text_color), size = 3, show.legend = FALSE) +
    scale_fill_gradient(low = "#F7F7F7", high = "#2C7FB8", limits = c(0, 1), na.value = "#F0F0F0", guide = "none") +
    scale_color_identity() +
    labs(
      title = title,
      x = NULL,
      y = "Phylum",
      subtitle = paste("Compact summary table for plant", tolower(taxon_label), "known, studied, and coverage")
    ) +
    theme_endo_bw(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 10, face = "bold"),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

save_plot <- function(plot, filename, width = 11, height = 7) {
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

build_taxonomy_views <- function(data, taxon_label, known_col, studied_col) {
  summary_data <- prepare_taxonomy_summary(data, known_col, studied_col)

  list(
    coverage_bar = plot_coverage_bar(
      summary_data,
      paste0("Plant ", taxon_label, ": Coverage by Phylum"),
      taxon_label
    ),
    studied_bar = plot_studied_count_bar(
      summary_data,
      paste0("Plant ", taxon_label, ": Studied Counts by Phylum"),
      taxon_label
    ),
    lollipop = plot_coverage_lollipop(
      summary_data,
      paste0("Plant ", taxon_label, ": Coverage Lollipop by Phylum"),
      taxon_label
    ),
    heatmap = plot_taxonomy_heatmap(
      summary_data,
      paste0("Plant ", taxon_label, ": Summary Heatmap by Phylum"),
      taxon_label
    )
  )
}

# =================================================================================
# GENERATE PLOTS
# =================================================================================

cat("Generating taxonomy bias plots...\n")

species_views <- build_taxonomy_views(species_data, "Species", "known_species", "studied_species")
genus_views <- build_taxonomy_views(genus_data, "Genera", "known_genera", "studied_genera")
family_views <- build_taxonomy_views(family_data, "Families", "known_families", "studied_families")

# =================================================================================
# SAVE PLOTS
# =================================================================================

cat("Saving plots to", PLOTS_OUTPUT_DIR, "\n")

save_plot(species_views$coverage_bar, "01_species_coverage_bar.png", width = 10, height = 7)
save_plot(species_views$studied_bar, "02_species_studied_bar.png", width = 10, height = 7)
save_plot(species_views$lollipop, "03_species_coverage_lollipop.png", width = 10, height = 7)
save_plot(species_views$heatmap, "04_species_summary_heatmap.png", width = 10, height = 7)

save_plot(genus_views$coverage_bar, "05_genera_coverage_bar.png", width = 10, height = 7)
save_plot(genus_views$studied_bar, "06_genera_studied_bar.png", width = 10, height = 7)
save_plot(genus_views$lollipop, "07_genera_coverage_lollipop.png", width = 10, height = 7)
save_plot(genus_views$heatmap, "08_genera_summary_heatmap.png", width = 10, height = 7)

save_plot(family_views$coverage_bar, "09_families_coverage_bar.png", width = 10, height = 7)
save_plot(family_views$studied_bar, "10_families_studied_bar.png", width = 10, height = 7)
save_plot(family_views$lollipop, "11_families_coverage_lollipop.png", width = 10, height = 7)
save_plot(family_views$heatmap, "12_families_summary_heatmap.png", width = 10, height = 7)

cat("\nTaxonomy representation plots complete!\n")
