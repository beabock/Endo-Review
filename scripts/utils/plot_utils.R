# =================================================================================
# plot_utils.R - Utility Functions for Plotting in Endo-Review Project
# =================================================================================
#
# Purpose: This file contains utility functions for generating consistent plots
#          and visualizations across the Endo-Review project.
#
# Description: Includes custom colorblind-friendly color palettes, themes, and helper functions
#              based on Okabe-Ito colorblind-friendly palette and ggplot2 themes.
#
# Dependencies: Requires ggplot2 package.
#
# =================================================================================
# Plot utilities for Endo-Review project

# Define colorblind-friendly color palette (muted tones with good contrast)
# Colors chosen to be distinguishable for people with color vision deficiency while maintaining aesthetic appeal

endo_palette <- c(
  "#5B8FA8",  # Muted steel blue
  "#7FB77E",  # Dusty green
  "#D18C63",  # Warm clay orange
  "#9C6843",  # Earthy brown
  "#B78FA8",  # Muted lavender
  "#6E6E6E",  # Medium gray
  "#C9B28E",  # Soft tan
  "#E3D4A3"   # Warm cream
)



# Define custom color mappings for common plot categories in Endo-Review
endo_colors <- list(
  presence_absence = c(Presence = "#46ACC8", Absence = "#B40F20"),
  relevant_irrelevant = c(Relevant = "#46ACC8", Irrelevant = "#B40F20"),
  found_not_found = c(Found = "#46ACC8", `Not Found` = "#B40F20")
)

# endo_theme function
# Purpose: Creates a custom ggplot2 theme for consistent plotting in Endo-Review
# Parameters:
#   base_size: numeric, base font size for text elements (default 11)
#   base_family: character, base font family (default "sans")
# Return: A ggplot2 theme object
# Usage: Add to ggplot with + endo_theme()
endo_theme <- function(base_size = 11, base_family = "sans") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = base_size * 1.2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5),
      axis.title = element_text(size = base_size, margin = margin(10, 10, 10, 10)),
      axis.text = element_text(size = base_size * 0.9, margin = margin(5, 5, 5, 5)),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size * 0.9),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.text = element_text(size = base_size, face = "bold"),
      strip.background = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
}

# get_endo_colors function
# Purpose: Retrieves a colorblind-friendly color palette for consistent coloring
# Parameters:
#   n: integer, number of colors to return (default 5)
#   type: character, "discrete" or "continuous" (default "discrete")
# Return: A vector of color hex codes
# Usage notes: Useful for scale_color_manual or scale_fill_manual in ggplot2
get_endo_colors <- function(n = 5, type = "discrete") {
  if (type == "continuous") {
    # For continuous scales, return the full palette
    return(endo_palette)
  } else {
    # For discrete scales, return first n colors, cycling if needed
    colors_needed <- endo_palette
    if (n > length(endo_palette)) {
      # If more colors needed than available, cycle through the palette
      colors_needed <- rep(endo_palette, ceiling(n / length(endo_palette)))[1:n]
    } else {
      colors_needed <- endo_palette[1:n]
    }
    return(colors_needed)
  }
}