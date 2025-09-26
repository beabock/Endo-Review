# =================================================================================
# shared_utils.R - Shared Utilities for Visualization Scripts
# =================================================================================
#
# Purpose: Common utility functions used across visualization scripts to reduce
#          code duplication and ensure consistency
#
# Usage: Source this file in visualization scripts to access shared utilities
#
# =================================================================================

library(here)
library(tidyverse)
library(scales)
source("scripts/utils/plot_utils.R")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Define standard plot directories
PLOT_DIRECTORIES <- list(
  main = "plots/main/",
  supplementary = "plots/supplementary/",
  geographic = "plots/geographic/",
  extraction = "plots/extraction/"
)

# ==============================================================================
# SHARED DATA LOADING FUNCTIONS
# ==============================================================================

#' Load comprehensive extraction results with error handling
#' @param file_path Path to the comprehensive extraction results CSV
#' @return Data frame with loaded data
load_comprehensive_data <- function(file_path = "results/comprehensive_extraction_results.csv") {
  if (!file.exists(file_path)) {
    stop(paste0("Required file not found: ", file_path, "\nPlease run the extraction pipeline first."))
  }

  cat("Loading comprehensive extraction results from:", file_path, "\n")
  data <- read_csv(file_path, show_col_types = FALSE)

  cat("Loaded", nrow(data), "records with", length(names(data)), "columns\n")

  return(data)
}

# ==============================================================================
# SHARED SAVE FUNCTIONS
# ==============================================================================

#' Unified plot saving function with consistent parameters and directory creation
#' @param plot ggplot2 plot object
#' @param filename Base filename (without path)
#' @param category Directory category ("main", "supplementary", "geographic", "extraction")
#' @param width Plot width in inches (default: 12)
#' @param height Plot height in inches (default: 8)
#' @param dpi Plot DPI (default: 300)
save_plot_organized <- function(plot, filename, category = "main", width = 12, height = 8, dpi = 300) {
  # Create directory if it doesn't exist
  dir_path <- PLOT_DIRECTORIES[[category]]
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Construct full path
  full_path <- paste0(dir_path, filename)

  # Save plot
  ggsave(full_path, plot, width = width, height = height, dpi = dpi, units = "in")

  cat("Saved plot:", full_path, "\n")
  return(full_path)
}

# ==============================================================================
# SHARED FILTERING FUNCTIONS
# ==============================================================================

#' Apply mycorrhizal filtering for main vs supplementary analysis modes
#' @param data Data frame containing mycorrhizal classification data
#' @param include_mycorrhizal_only Logical: if TRUE, include mycorrhizal-only papers
#' @return Filtered data frame
filter_mycorrhizal_papers <- function(data, include_mycorrhizal_only = FALSE) {
  if (!"is_mycorrhizal_only" %in% colnames(data)) {
    message("Mycorrhizal classification not available in dataset")
    return(data)
  }

  if (include_mycorrhizal_only) {
    message("Including mycorrhizal-only papers in analysis (supplementary mode)")
    return(data)
  } else {
    message("Excluding mycorrhizal-only papers from analysis (main mode)")
    filtered_data <- data %>%
      filter(!is_mycorrhizal_only)
    message("Filtered out ", nrow(data) - nrow(filtered_data), " mycorrhizal-only abstracts")
    return(filtered_data)
  }
}

# ==============================================================================
# SHARED THEME AND STYLING
# ==============================================================================

#' Get enhanced theme with consistent styling for all visualizations
#' @param base_size Base font size (default: 12)
#' @return ggplot2 theme object
get_visualization_theme <- function(base_size = 12) {
  endo_theme(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size * 1.2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size * 0.9, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# ==============================================================================
# SHARED DATA PROCESSING
# ==============================================================================

#' Create standardized data summary for consistent reporting
#' @param data Comprehensive extraction results data
#' @param group_by_column Column to group by for summary (default: "id")
#' @return Summary data frame
create_data_summary <- function(data, group_by_column = "id") {
  summary <- data %>%
    group_by(!!sym(group_by_column)) %>%
    summarise(
      has_species = any(!is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName)),
      has_plant_parts = any(!is.na(plant_parts_detected)),
      has_geography = any(!is.na(geographic_summary)),
      has_methods = any(!is.na(molecular_methods) | !is.na(culture_based_methods) | !is.na(microscopy_methods)),
      has_mycorrhizal_only = any(is_mycorrhizal_only),
      predicted_label = first(final_classification),
      .groups = "drop"
    )

  cat("\nData Summary:\n")
  cat("- Total entries:", nrow(summary), "\n")
  cat("- With species detected:", sum(summary$has_species),
      "(", round(100 * mean(summary$has_species), 1), "%)\n")
  cat("- With plant parts:", sum(summary$has_plant_parts),
      "(", round(100 * mean(summary$has_plant_parts), 1), "%)\n")
  cat("- With geography:", sum(summary$has_geography),
      "(", round(100 * mean(summary$has_geography), 1), "%)\n")
  cat("- With methods:", sum(summary$has_methods),
      "(", round(100 * mean(summary$has_methods), 1), "%)\n\n")

  return(summary)
}

# ==============================================================================
# SHARED PLOTTING HELPERS
# ==============================================================================

#' Create consistent color mapping for prediction types
#' @return Named vector of colors
get_prediction_colors <- function() {
  return(endo_colors$presence_absence)
}

#' Create consistent fill mapping for found/not found categories
#' @return Named vector of colors
get_found_not_found_colors <- function() {
  return(endo_colors$found_not_found)
}

#' Standardize kingdom names for consistent plotting
#' @param kingdom_vector Vector of kingdom names
#' @return Standardized kingdom names
standardize_kingdom_names <- function(kingdom_vector) {
  case_when(
    kingdom_vector == "Plantae" ~ "Plantae",
    kingdom_vector == "Fungi" ~ "Fungi",
    tolower(kingdom_vector) == "plantae" ~ "Plantae",
    tolower(kingdom_vector) == "fungi" ~ "Fungi",
    TRUE ~ kingdom_vector
  )
}

# ==============================================================================
# SHARED VALIDATION FUNCTIONS
# ==============================================================================

#' Validate required columns exist in data
#' @param data Data frame to validate
#' @param required_cols Vector of required column names
#' @return Logical: TRUE if all columns exist
validate_data_structure <- function(data, required_cols = c("id", "final_classification")) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }

  return(TRUE)
}

# ==============================================================================
# USAGE EXAMPLES
# ==============================================================================
#
# In visualization scripts, use:
#
# source("scripts/04_analysis/visualization/shared_utils.R")
#
# # Load data consistently
# data <- load_comprehensive_data()
#
# # Apply filtering
# main_data <- filter_mycorrhizal_papers(data, include_mycorrhizal_only = FALSE)
# supp_data <- filter_mycorrhizal_papers(data, include_mycorrhizal_only = TRUE)
#
# # Save plots consistently
# save_plot_organized(my_plot, "my_plot.png", category = "main")
#
# # Apply consistent theme
# my_plot + get_visualization_theme()
#
# ==============================================================================