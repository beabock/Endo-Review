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
  # Get phylum labels with common names and known counts
  phylum_labels <- data %>%
    select(phylum, common_name, all_of(known_col)) %>%
    distinct() %>%
    mutate(
      phylum_label = paste0(common_name, " (", phylum, ")\nn=", .data[[known_col]])
    ) %>%
    select(phylum, phylum_label)

  # Prepare data for stacked bar plot and compute percent labels
  phylum_known <- data %>%
    select(phylum, all_of(known_col)) %>%
    distinct() %>%
    rename(KnownTotal = all_of(known_col))

  plot_data <- data %>%
    select(phylum, all_of(known_col), all_of(studied_col)) %>%
    rename(Known = all_of(known_col), Studied = all_of(studied_col)) %>%
    mutate(`Not Studied` = Known - Studied) %>%
    select(phylum, Known, Studied, `Not Studied`) %>%
    pivot_longer(
      cols = c(Studied, `Not Studied`),
      names_to = "Type",
      values_to = "Count"
    ) %>%
    mutate(Type = factor(Type, levels = c("Studied", "Not Studied"))) %>%
    left_join(phylum_known, by = "phylum") %>%
    mutate(
      phylum_label = factor(phylum_label, levels = phylum_labels$phylum_label),
      percent = ifelse(Known > 0, Count / Known * 100, NA_real_),
      label = ifelse(
        !is.na(percent) & (Count >= 5 || percent >= 12),
        paste0(scales::comma(Count), "\n(", round(percent, 1), "%)"),
        ""
      )
    )
    mutate(
      percent = ifelse(Known > 0, Count / Known * 100, NA_real_),
      phylum_label = factor(phylum_label, levels = phylum_labels$phylum_label))+
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      lineheight = 0.9,
      check_overlap = TRUE
    ) +
    scale_fill_manual(
    )

  plot <- ggplot(plot_data, aes(x = phylum_label, y = Count, fill = Type)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3)
    
  plot <- plot +
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
  
  # Get phylum labels with common names and known counts
  known_col <- colnames(data)[grepl("^known_", colnames(data))][1]
  phylum_labels <- data %>%
    select(phylum, common_name, all_of(known_col)) %>%
    distinct() %>%
    mutate(
      phylum_label = paste0(common_name, " (", phylum, ")\nn=", .data[[known_col]])
    ) %>%
    select(phylum, phylum_label)
  
  plot_data <- data %>%
    select(phylum, all_of(coverage_cols)) %>%
    rename(coverage_raw = all_of(coverage_cols[1]))
  
  # Detect if coverage is in 0-1 range (decimal) or 0-100 range (percentage)
  max_coverage <- max(plot_data$coverage_raw, na.rm = TRUE)
  
  # compute coverage percent
  plot_data <- plot_data %>%
    mutate(
      coverage_percent = if (max_coverage < 2) {
        pmin(coverage_raw * 100, 100)
      } else {
        pmin(coverage_raw, 100)
      }
    ) %>%
    left_join(phylum_labels, by = "phylum") %>%
    mutate(phylum_label = factor(phylum_label, levels = phylum_labels$phylum_label))

  # attach absolute counts where available and build label
  studied_col <- colnames(data)[grepl("^studied_", colnames(data))][1]
  if (!is.null(studied_col) && studied_col %in% colnames(data)) {
    plot_data <- plot_data %>%
      left_join(data %>% select(phylum, all_of(studied_col)), by = "phylum") %>%
      rename(studied_count = all_of(studied_col)) %>%
      mutate(label = ifelse(!is.na(coverage_percent), paste0(scales::comma(studied_count), " (", round(coverage_percent,1), "% )"), ""))
  } else {
    plot_data <- plot_data %>%
      mutate(label = ifelse(!is.na(coverage_percent), paste0(round(coverage_percent,1), "%"), ""))
  }
  
  coverage_upper <- ceiling(max(plot_data$coverage_percent, na.rm = TRUE) / 5) * 5
  if (coverage_upper <= 0 || is.na(coverage_upper)) {
    coverage_upper <- 1
  }
  
  plot <- ggplot(plot_data, aes(x = phylum_label, y = coverage_percent, fill = coverage_percent)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(x = phylum_label, y = coverage_percent, label = label), inherit.aes = FALSE, vjust = -0.3, size = 3) +
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


#' Create relative stacked-percent representation plot (Studied vs Not Studied = 100%)
#' @param data Data frame with coverage data
#' @param title Plot title
#' @param taxon_label Label for the taxon (e.g., "Species", "Genera")
#' @return ggplot object
plot_relative_representation_percent <- function(data, title, taxon_label) {
  # Find known/studied column names
  known_col <- colnames(data)[grepl("^known_", colnames(data))][1]
  studied_col <- colnames(data)[grepl("^studied_", colnames(data))][1]

  # Build phylum labels (common name + scientific + known count)
  phylum_labels <- data %>%
    select(phylum, common_name, all_of(known_col)) %>%
    distinct() %>%
    mutate(phylum_label = paste0(common_name, " (", phylum, ")\nn=", .data[[known_col]])) %>%
    select(phylum, phylum_label)

  # Prepare percent representation data
  plot_data <- data %>%
    select(phylum, all_of(known_col), all_of(studied_col)) %>%
    rename(known = all_of(known_col), studied = all_of(studied_col)) %>%
    distinct() %>%
    mutate(
      studied_pct = ifelse(known > 0, studied / known, NA_real_),
      notstudied_pct = ifelse(known > 0, 1 - studied_pct, NA_real_)
    ) %>%
    left_join(phylum_labels, by = "phylum") %>%
    mutate(phylum_label = factor(phylum_label, levels = phylum_labels$phylum_label))

  plot_long <- plot_data %>%
    select(phylum, phylum_label, known, studied) %>%
    distinct() %>%
    mutate(notstudied = known - studied) %>%
    select(phylum, phylum_label, known, studied, notstudied) %>%
    pivot_longer(cols = c(studied, notstudied), names_to = "raw_type", values_to = "count") %>%
    mutate(
      Type = factor(raw_type, levels = c("studied", "notstudied"), labels = c("Studied", "Not Studied")),
      pct = ifelse(known > 0, count / known, NA_real_),
      label = ifelse(!is.na(pct) & pct >= 0.02, paste0(scales::comma(count), "\n", round(pct * 100, 1), "%"), "")
    )

  plot <- ggplot(plot_long, aes(x = phylum_label, y = pct, fill = Type)) +
    geom_bar(stat = "identity", position = "fill", width = 0.7) +
    geom_text(aes(label = label), position = position_fill(vjust = 0.5), size = 3, color = "#222222") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = c("Studied" = "#0072B2", "Not Studied" = "#CCCCCC"), name = "Category") +
    labs(
      title = title,
      x = "Phylum",
      y = "Percent of known",
      subtitle = paste("Percentage of known", tolower(taxon_label), "represented (stacked to 100%)")
    ) +
    theme_endo_bw(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.major.x = element_blank(),
      legend.position = "top"
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

# Additional stacked-percent relative plots (each bar fills to 100%)
p_species_rel_pct <- plot_relative_representation_percent(
  species_data,
  "Plant Species: Percent Represented by Phylum (stacked to 100%)",
  "Species"
)

p_genus_rel_pct <- plot_relative_representation_percent(
  genus_data,
  "Plant Genera: Percent Represented by Phylum (stacked to 100%)",
  "Genera"
)

p_family_rel_pct <- plot_relative_representation_percent(
  family_data,
  "Plant Families: Percent Represented by Phylum (stacked to 100%)",
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

# Save stacked-percent relative plots
save_plot(p_species_rel_pct, "02b_species_relative_representation_percent.png", width = 11, height = 7)
save_plot(p_genus_rel_pct, "04b_genera_relative_representation_percent.png", width = 11, height = 7)
save_plot(p_family_rel_pct, "06b_families_relative_representation_percent.png", width = 11, height = 7)

cat("\nTaxonomy representation plots complete!\n")
