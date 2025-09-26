# Visualization Script for Comprehensive Extraction Results
# B. Bock
# July 30, 2025
#
# This script creates comprehensive visualizations from the output of extract_species_simple.R
# Generates plots for:
# 1. Species detection patterns
# 2. Research methods distribution
# 3. Plant parts frequency
# 4. Geographic distribution
# 5. Prediction quality analysis 

library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(RColorBrewer)
library(forcats)
library(treemapify)
library(maps)
library(countrycode)

source("scripts/utils/plot_utils.R")
# Load centralized reference utilities (normalization helpers)
utils_path <- "scripts/04_analysis/utilities/reference_data_utils.R"
if (file.exists(utils_path)) {
  source(utils_path)
} else {
  warning("reference_data_utils.R not found; country/plant-part normalization helpers unavailable")
}

# Need to adjust abstract counts throughout to group by abstract.id


# Custom theme for consistent visualization
custom_theme <- endo_theme(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    plot.margin = margin(10, 10, 10, 10)
  )


# Function to save plots with consistent format
save_plot <- function(plot, filename, width = 12, height = 8, dpi = 300) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename, plot, width = width, height = height, dpi = dpi, units = "in")
  cat("Saved plot:", filename, "\n")
}

# Load and prepare data ---------------------------------------------------

cat("=== COMPREHENSIVE EXTRACTION VISUALIZATION ===\n")
cat("Loading results from extract_species_simple.R...\n\n")

# Check for required input files
if (!file.exists("results/comprehensive_extraction_results.csv")) {
  stop("Error: comprehensive_extraction_results.csv not found. Please run extract_species_simple.R first.")
}

# Load main results
if (!file.exists("results/comprehensive_extraction_results.csv")) {
  stop("File results/comprehensive_extraction_results.csv not found. Please ensure the extraction pipeline has been run first.")
}

# Check if the file has the expected structure for visualization
results_check <- read_csv("results/comprehensive_extraction_results.csv", n_max = 5, show_col_types = FALSE)
expected_cols <- c("id", "predicted_label")  # Minimum required columns
missing_cols <- setdiff(expected_cols, names(results_check))

# If predicted_label is missing, try to use final_classification as a fallback
if ("predicted_label" %in% missing_cols && "final_classification" %in% names(results_check)) {
  cat("⚠️  predicted_label column not found, using final_classification as fallback\n")
  expected_cols <- c("id", "final_classification")
  missing_cols <- setdiff(expected_cols, names(results_check))
}

if (length(missing_cols) > 0) {
  cat("❌ File structure issue: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  cat("This visualization script is designed for comprehensive extraction results.\n")
  cat("For absence detection results, use the absence-specific visualization or run the full extraction pipeline first.\n")
  stop("Incompatible data structure for visualization")
}

# Load the full dataset
results <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)

cat("Loaded", nrow(results), "abstracts with comprehensive extraction data\n")

# Data preparation and summary statistics
results_clean <- results %>%
  mutate(
    # Handle prediction labels - use final_classification as the main prediction column
    predicted_label = if("final_classification" %in% names(results)) {
      final_classification  # Use final_classification as the main prediction
    } else {
      "Unknown"  # Default fallback
    },
    # Clean prediction labels
    predicted_label = case_when(
      predicted_label == "Presence" ~ "Presence",
      predicted_label == "Absence" ~ "Absence",
      TRUE ~ as.character(predicted_label)
    ),
    
    # Create method categories for original 3 methods
    has_molecular = coalesce(molecular_methods, FALSE),
    has_culture = coalesce(culture_based_methods, FALSE),
    has_microscopy = coalesce(microscopy_methods, FALSE),

    # Create expanded method categories for all 9 method types
    has_inoculation = coalesce(inoculation_methods, FALSE),
    has_plant_microbe_interaction = coalesce(plant_microbe_interaction_methods, FALSE),
    has_bioactivity_assays = coalesce(bioactivity_assays_methods, FALSE),
    has_physiological_assays = coalesce(physiological_assays_methods, FALSE),
    has_ecological_studies = coalesce(ecological_studies_methods, FALSE),
    has_surface_sterilization = coalesce(surface_sterilization_methods, FALSE),

    # Create combined method category for original 3 methods
    methods_combined = case_when(
      has_molecular & has_culture & has_microscopy ~ "All three methods",
      has_molecular & has_culture ~ "Molecular + Culture",
      has_molecular & has_microscopy ~ "Molecular + Microscopy",
      has_culture & has_microscopy ~ "Culture + Microscopy",
      has_molecular ~ "Molecular only",
      has_culture ~ "Culture only",
      has_microscopy ~ "Microscopy only",
      TRUE ~ "No methods detected"
    ),

    # Create expanded combined method category for all 9 methods
    methods_expanded = case_when(
      has_molecular & has_culture & has_microscopy & has_inoculation & has_plant_microbe_interaction ~
        "Multiple methods (5+ types)",
      has_molecular & has_culture & has_microscopy ~ "Molecular + Culture + Microscopy",
      has_molecular & has_culture ~ "Molecular + Culture",
      has_molecular & has_microscopy ~ "Molecular + Microscopy",
      has_culture & has_microscopy ~ "Culture + Microscopy",
      has_molecular ~ "Molecular only",
      has_culture ~ "Culture only",
      has_microscopy ~ "Microscopy only",
      has_inoculation ~ "Inoculation only",
      has_plant_microbe_interaction ~ "Plant-microbe interaction only",
      has_bioactivity_assays ~ "Bioactivity assays only",
      has_physiological_assays ~ "Physiological assays only",
      has_ecological_studies ~ "Ecological studies only",
      has_surface_sterilization ~ "Surface sterilization only",
      TRUE ~ "No methods detected"
    ),
    
    # Species detection indicators
    has_species = !is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName),
    has_plant_parts = !is.na(plant_parts_detected),
    has_geography = !is.na(geographic_summary),
    
    # Confidence categories - handle model-specific probability columns
    confidence_cat = {
      # Try ensemble probabilities first (most comprehensive)
      if("ensemble_presence_prob" %in% names(results) && any(!is.na(results$ensemble_presence_prob))) {
        ensemble_conf <- pmax(results$ensemble_presence_prob, results$ensemble_absence_prob, na.rm = TRUE)
        case_when(
          !is.na(ensemble_conf) & ensemble_conf >= 0.9 ~ "Very High (≥90%)",
          !is.na(ensemble_conf) & ensemble_conf >= 0.8 ~ "High (80-90%)",
          !is.na(ensemble_conf) & ensemble_conf >= 0.7 ~ "Medium (70-80%)",
          !is.na(ensemble_conf) & ensemble_conf >= 0.6 ~ "Low (60-70%)",
          !is.na(ensemble_conf) ~ "Very Low (<60%)",
          TRUE ~ "Unknown"
        )
      } else if("glmnet_prob_presence" %in% names(results) && any(!is.na(results$glmnet_prob_presence))) {
        # Use glmnet probabilities as fallback
        glmnet_conf <- pmax(results$glmnet_prob_presence, results$glmnet_prob_absence, na.rm = TRUE)
        case_when(
          !is.na(glmnet_conf) & glmnet_conf >= 0.9 ~ "Very High (≥90%)",
          !is.na(glmnet_conf) & glmnet_conf >= 0.8 ~ "High (80-90%)",
          !is.na(glmnet_conf) & glmnet_conf >= 0.7 ~ "Medium (70-80%)",
          !is.na(glmnet_conf) & glmnet_conf >= 0.6 ~ "Low (60-70%)",
          !is.na(glmnet_conf) ~ "Very Low (<60%)",
          TRUE ~ "Unknown"
        )
      } else {
        # No probability columns available
        "Unknown"
      }
    },
    
    # Geographic regions
    has_global_north = !is.na(global_north_countries),
    has_global_south = !is.na(global_south_countries),
    geographic_region = case_when(
      has_global_north & has_global_south ~ "Both North & South",
      has_global_north ~ "Global North",
      has_global_south ~ "Global South", 
      has_geography ~ "Other regions",
      TRUE ~ "No geography"
    )
  )

# Print summary statistics (grouping by abstract ID to avoid double counting)
abstract_summary <- results_clean %>%
  group_by(id) %>%
  summarise(
    has_species = any(has_species),
    has_plant_parts = any(has_plant_parts),
    has_geography = any(has_geography),
    has_methods = any(methods_combined != "No methods detected"),
    has_mycorrhizal_only = any(is_mycorrhizal_only),
    predicted_label = first(predicted_label),
    .groups = "drop"
  )

cat("\nData Summary:\n")
cat("- Total abstracts:", nrow(abstract_summary), "\n")
cat("- With species detected:", sum(abstract_summary$has_species), 
    "(", round(100 * mean(abstract_summary$has_species), 1), "%)\n")
cat("- With plant parts:", sum(abstract_summary$has_plant_parts),
    "(", round(100 * mean(abstract_summary$has_plant_parts), 1), "%)\n")
cat("- With geography:", sum(abstract_summary$has_geography),
    "(", round(100 * mean(abstract_summary$has_geography), 1), "%)\n")
cat("- With methods:", sum(abstract_summary$has_methods),
    "(", round(100 * mean(abstract_summary$has_methods), 1), "%)\n\n")

# 1. SPECIES DETECTION OVERVIEW ------------------------------------------

cat("Creating species detection overview plots...\n")

# Overall species detection rate
total_abstracts_species <- nrow(abstract_summary)
abstracts_with_species <- sum(abstract_summary$has_species)
species_rate <- abstracts_with_species / total_abstracts_species * 100

p1_species_detection <- data.frame(
  Category = c("With Species", "Without Species"),
  Count = c(abstracts_with_species, total_abstracts_species - abstracts_with_species),
  Rate = c(species_rate, 100 - species_rate)
) %>%
  ggplot(aes(x = "All Abstracts", y = Count, fill = Category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Count, "\n(", round(Rate, 1), "%)")),
            position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
  scale_fill_manual(values = c("With Species" = "#46ACC8", "Without Species" = "#B40F20")) +
  labs(
    title = "Overall Species Detection Rate",
    subtitle = paste("Out of", format(total_abstracts_species, big.mark = ","), "total abstracts"),
    x = "",
    y = "Number of Abstracts",
    caption = paste0("Species detection rate: ", round(species_rate, 1), "%")
  ) +
  custom_theme +
  theme(legend.position = "bottom")

# Save species detection plot
save_plot(p1_species_detection, "plots/extraction/species_detection_rate.png", height = 6)

# 2. RESEARCH METHODS ANALYSIS -------------------------------------------

cat("Creating research methods analysis plots...\n")

# Original 3 methods distribution
methods_abstract_summary <- results_clean %>%
  group_by(id) %>%
  summarise(
    has_molecular = any(has_molecular),
    has_culture = any(has_culture),
    has_microscopy = any(has_microscopy),
    .groups = "drop"
  )

methods_summary <- methods_abstract_summary %>%
  summarise(
    Molecular = sum(has_molecular),
    Culture = sum(has_culture),
    Microscopy = sum(has_microscopy)
  ) %>%
  pivot_longer(everything(), names_to = "Method", values_to = "Count") %>%
  mutate(
    Percentage = Count / nrow(methods_abstract_summary) * 100,
    Method = fct_reorder(Method, Count)
  )

p3_methods_individual <- methods_summary %>%
  ggplot(aes(x = Method, y = Count, fill = Method)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = get_endo_colors(3)) +
  labs(
    title = "Core Research Methods Detection",
    subtitle = "Traditional method frequencies (Molecular, Culture, Microscopy)",
    x = "Research Method",
    y = "Number of Abstracts",
    caption = paste("Total abstracts analyzed:", nrow(methods_abstract_summary))
  ) +
  custom_theme +
  theme(legend.position = "none")

# All 9 methods distribution
methods_expanded_summary <- results_clean %>%
  group_by(id) %>%
  summarise(
    has_molecular = any(has_molecular),
    has_culture = any(has_culture),
    has_microscopy = any(has_microscopy),
    has_inoculation = any(has_inoculation),
    has_plant_microbe_interaction = any(has_plant_microbe_interaction),
    has_bioactivity_assays = any(has_bioactivity_assays),
    has_physiological_assays = any(has_physiological_assays),
    has_ecological_studies = any(has_ecological_studies),
    has_surface_sterilization = any(has_surface_sterilization),
    .groups = "drop"
  )

methods_expanded_counts <- methods_expanded_summary %>%
  summarise(
    `Molecular` = sum(has_molecular),
    `Culture-based` = sum(has_culture),
    `Microscopy` = sum(has_microscopy),
    `Inoculation` = sum(has_inoculation),
    `Plant-microbe interaction` = sum(has_plant_microbe_interaction),
    `Bioactivity assays` = sum(has_bioactivity_assays),
    `Physiological assays` = sum(has_physiological_assays),
    `Ecological studies` = sum(has_ecological_studies),
    `Surface sterilization` = sum(has_surface_sterilization)
  ) %>%
  pivot_longer(everything(), names_to = "Method", values_to = "Count") %>%
  mutate(
    Percentage = Count / nrow(methods_expanded_summary) * 100,
    Method = fct_reorder(Method, Count)
  ) %>%
  filter(Count > 0)  # Only show methods that were detected

p3b_methods_expanded <- methods_expanded_counts %>%
  ggplot(aes(x = Method, y = Count, fill = Method)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
            hjust = -0.1, size = 3) +
  scale_fill_viridis_d() +
  labs(
    title = "Expanded Research Methods Detection",
    subtitle = "All 9 method types detected across abstracts",
    x = "Research Method",
    y = "Number of Abstracts"
  ) +
  coord_flip() +
  custom_theme +
  theme(legend.position = "none")

# Combined methods analysis (original 3)
methods_combined_summary <- results_clean %>%
  group_by(id) %>%
  summarise(methods_combined = first(methods_combined), .groups = "drop") %>%
  count(methods_combined, name = "count") %>%
  mutate(
    percentage = count / sum(count) * 100,
    methods_combined = fct_reorder(methods_combined, count)
  ) %>%
  filter(count > 0)

p4_methods_combined <- methods_combined_summary %>%
  ggplot(aes(x = methods_combined, y = count, fill = methods_combined)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
            hjust = -0.1, size = 3.5) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Combined Core Research Methods",
    subtitle = "Traditional method combinations used in endophyte studies",
    x = "Method Combination",
    y = "Number of Abstracts"
  ) +
  coord_flip() +
  custom_theme +
  theme(legend.position = "none")

# Expanded combined methods analysis
methods_expanded_combined_summary <- results_clean %>%
  group_by(id) %>%
  summarise(methods_expanded = first(methods_expanded), .groups = "drop") %>%
  count(methods_expanded, name = "count") %>%
  mutate(
    percentage = count / sum(count) * 100,
    methods_expanded = fct_reorder(methods_expanded, count)
  ) %>%
  filter(count > 0) %>%
  slice_max(count, n = 15)  # Show top 15 combinations

p4b_methods_expanded_combined <- methods_expanded_combined_summary %>%
  ggplot(aes(x = methods_expanded, y = count, fill = methods_expanded)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
            hjust = -0.1, size = 2.5) +
  scale_fill_viridis_d(option = "cividis") +
  labs(
    title = "Expanded Combined Research Methods",
    subtitle = "Top 15 method combinations (all 9 method types)",
    x = "Method Combination",
    y = "Number of Abstracts"
  ) +
  coord_flip() +
  custom_theme +
  theme(legend.position = "none")

# Save methods plots individually
save_plot(p3_methods_individual, "plots/extraction/methods_individual_core.png", height = 6)
save_plot(p3b_methods_expanded, "plots/extraction/methods_individual_expanded.png", height = 8)
save_plot(p4_methods_combined, "plots/extraction/methods_combined_core.png", height = 6)
save_plot(p4b_methods_expanded_combined, "plots/extraction/methods_combined_expanded.png", height = 8)

# 3. PLANT PARTS ANALYSIS ------------------------------------------------

cat("Creating plant parts analysis plots...\n")

if (sum(results_clean$has_plant_parts) > 0) {

  # Enhanced plant parts analysis using new parts_count and parts_normalized columns
  plant_parts_data <- results_clean %>%
    filter(has_plant_parts, !is.na(parts_normalized)) %>%
    select(id, plant_parts_detected, parts_normalized, parts_count) %>%
    # Use the normalized parts data for consistent grouping
    separate_rows(parts_normalized, sep = "; ") %>%
    filter(!is.na(parts_normalized), parts_normalized != "") %>%
    # Count occurrences per normalized plant part
    count(parts_normalized, name = "total_mentions") %>%
    arrange(desc(total_mentions)) %>%
    slice_max(total_mentions, n = 20) %>%  # Top 20 most frequent
    mutate(
      parts_normalized = fct_reorder(parts_normalized, total_mentions),
      display_name = stringr::str_to_title(parts_normalized)
    )

  p6_plant_parts_freq <- plant_parts_data %>%
    ggplot(aes(x = display_name, y = total_mentions)) +
    geom_col(fill = endo_palette[1], width = 0.8) +
    geom_text(aes(label = total_mentions), hjust = -0.1, size = 3) +
    labs(
      title = "Most Frequently Studied Plant Parts",
      subtitle = paste("Top 20 normalized plant parts from", sum(abstract_summary$has_plant_parts), "unique abstracts"),
      x = "Plant Part",
      y = "Total Mentions Across Abstracts"
    ) +
    coord_flip() +
    custom_theme

  # Plant parts count distribution analysis
  parts_count_summary <- results_clean %>%
    filter(has_plant_parts) %>%
    count(parts_count, name = "abstracts_per_count") %>%
    mutate(
      percentage = abstracts_per_count / sum(abstracts_per_count) * 100,
      parts_count = fct_reorder(as.factor(parts_count), parts_count)
    )

  p6b_plant_parts_distribution <- parts_count_summary %>%
    ggplot(aes(x = parts_count, y = abstracts_per_count, fill = parts_count)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(abstracts_per_count, "\n(", round(percentage, 1), "%)")),
              vjust = -0.3, size = 3) +
    scale_fill_viridis_d() +
    labs(
      title = "Plant Parts Count Distribution",
      subtitle = "Number of abstracts mentioning specific numbers of plant parts",
      x = "Number of Plant Parts per Abstract",
      y = "Number of Abstracts"
    ) +
    custom_theme +
    theme(legend.position = "none")
  
  # Plant parts by prediction type
  plant_parts_by_pred <- results_clean %>%
    group_by(predicted_label) %>%
    summarise(
      total = n(),
      with_parts = sum(has_plant_parts),
      parts_rate = with_parts / total,
      .groups = "drop"
    ) %>%
    ggplot(aes(x = fct_reorder(predicted_label, parts_rate))) +
    geom_col(aes(y = total), fill = "lightgray", alpha = 0.7, width = 0.6) +
    geom_col(aes(y = with_parts), fill = endo_palette[1], width = 0.6) +
    geom_text(aes(y = with_parts + total * 0.05,
                  label = paste0(round(parts_rate * 100, 1), "%")),
              size = 4, fontface = "bold") +
    labs(
      title = "Plant Parts Detection by Prediction Type",
      subtitle = "Percentage of abstracts mentioning specific plant parts",
      x = "Prediction Type", 
      y = "Number of Abstracts"
    ) +
    custom_theme
  
  # Save plant parts plots individually
  save_plot(p6_plant_parts_freq, "plots/extraction/plant_parts_frequency.png", height = 8)
  save_plot(p6b_plant_parts_distribution, "plots/extraction/plant_parts_distribution.png", height = 6)
  save_plot(plant_parts_by_pred, "plots/extraction/plant_parts_by_prediction.png", height = 6)
  
} else {
  cat("No plant parts data available for visualization.\n")
}

# 4. GEOGRAPHIC ANALYSIS -------------------------------------------------

cat("Creating geographic distribution plots...\n")

if (sum(results_clean$has_geography) > 0) {

  # Enhanced geographic analysis using new specific columns
  # Global North vs South distribution
  p7_geo_regions <- results_clean %>%
    count(geographic_region, name = "count") %>%
    mutate(
      percentage = count / sum(count) * 100,
      geographic_region = fct_reorder(geographic_region, count)
    ) %>%
    ggplot(aes(x = geographic_region, y = count, fill = geographic_region)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
              hjust = -0.1, size = 3.5) +
    scale_fill_viridis_d(option = "cividis") +
    labs(
      title = "Geographic Distribution of Studies",
      subtitle = "Global North vs South classification",
      x = "Geographic Region",
      y = "Number of Abstracts"
    ) +
    coord_flip() +
    custom_theme +
    theme(legend.position = "none")

  # Countries distribution using new countries_detected column
  countries_data <- results_clean %>%
    filter(!is.na(countries_detected)) %>%
    select(countries_detected) %>%
    separate_rows(countries_detected, sep = "; ") %>%
    filter(!is.na(countries_detected), countries_detected != "") %>%
    # Standardize country names
    mutate(country_std = if (exists("normalize_country_vector")) {
      normalize_country_vector(countries_detected)
    } else {
      stringr::str_to_title(countries_detected)
    }) %>%
    count(country_std, name = "frequency") %>%
    arrange(desc(frequency)) %>%
    slice_max(frequency, n = 15) %>%
    mutate(country_display = fct_reorder(stringr::str_to_title(country_std), frequency))

  p8_countries <- countries_data %>%
    ggplot(aes(x = country_display, y = frequency)) +
    geom_col(fill = endo_palette[4], width = 0.8) +
    geom_text(aes(label = frequency), hjust = -0.1, size = 3) +
    labs(
      title = "Most Studied Countries",
      subtitle = paste("Top 15 countries from", sum(!is.na(results_clean$countries_detected)), "abstracts with country information"),
      x = "Country",
      y = "Number of Studies"
    ) +
    coord_flip() +
    custom_theme

  # Continents distribution using new continents_detected column
  continents_data <- results_clean %>%
    filter(!is.na(continents_detected)) %>%
    select(continents_detected) %>%
    separate_rows(continents_detected, sep = "; ") %>%
    filter(!is.na(continents_detected), continents_detected != "") %>%
    count(continents_detected, name = "frequency") %>%
    arrange(desc(frequency)) %>%
    mutate(continent_display = fct_reorder(continents_detected, frequency))

  p8b_continents <- continents_data %>%
    ggplot(aes(x = continent_display, y = frequency)) +
    geom_col(fill = endo_palette[3], width = 0.7) +
    geom_text(aes(label = frequency), hjust = -0.1, size = 3) +
    labs(
      title = "Studies by Continent",
      subtitle = "Distribution of research across continents",
      x = "Continent",
      y = "Number of Studies"
    ) +
    coord_flip() +
    custom_theme

  # Geographic information completeness
  geo_completeness <- results_clean %>%
    filter(has_geography) %>%
    summarise(
      has_countries = sum(!is.na(countries_detected)),
      has_continents = sum(!is.na(continents_detected)),
      has_regions = sum(!is.na(regions_detected)),
      has_coordinates = sum(has_coordinates)
    ) %>%
    pivot_longer(everything(), names_to = "geo_type", values_to = "count") %>%
    mutate(
      percentage = count / sum(count) * 100,
      geo_type = case_when(
        geo_type == "has_countries" ~ "Countries",
        geo_type == "has_continents" ~ "Continents",
        geo_type == "has_regions" ~ "Regions",
        geo_type == "has_coordinates" ~ "Coordinates"
      ),
      geo_type = fct_reorder(geo_type, count)
    )

  p8c_geo_completeness <- geo_completeness %>%
    ggplot(aes(x = geo_type, y = count, fill = geo_type)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
              vjust = -0.3, size = 3) +
    scale_fill_viridis_d() +
    labs(
      title = "Geographic Information Completeness",
      subtitle = "Types of geographic information detected",
      x = "Geographic Information Type",
      y = "Number of Abstracts"
    ) +
    custom_theme +
    theme(legend.position = "none")

  # Save geography plots individually
  save_plot(p7_geo_regions, "plots/extraction/geographic_regions.png", height = 6)
  save_plot(p8_countries, "plots/extraction/top_countries.png", height = 8)
  save_plot(p8b_continents, "plots/extraction/continents_distribution.png", height = 6)
  save_plot(p8c_geo_completeness, "plots/extraction/geographic_completeness.png", height = 6)
  
} else {
  cat("No geographic data available for visualization.\n")
}

# 4. MYCORRHIZAL CLASSIFICATION ANALYSIS ---------------------------------

cat("Creating mycorrhizal classification analysis plots...\n")

if (sum(!is.na(results_clean$is_mycorrhizal)) > 0) {

  # Mycorrhizal-only papers distribution
  mycorrhizal_summary <- results_clean %>%
    group_by(id) %>%
    summarise(
      has_mycorrhizal_only = any(is_mycorrhizal_only),
      predicted_label = first(predicted_label),
      .groups = "drop"
    ) %>%
    count(has_mycorrhizal_only, predicted_label, name = "count") %>%
    mutate(
      percentage = count / sum(count) * 100,
      mycorrhizal_status = if_else(has_mycorrhizal_only, "Mycorrhizal-only", "Mixed/Non-mycorrhizal")
    )

  p5_mycorrhizal_distribution <- mycorrhizal_summary %>%
    ggplot(aes(x = mycorrhizal_status, y = count, fill = predicted_label)) +
    geom_col(width = 0.7, position = "dodge") +
    geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
              position = position_dodge(width = 0.7), vjust = -0.3, size = 3) +
    scale_fill_manual(values = endo_colors$presence_absence) +
    labs(
      title = "Mycorrhizal-only Papers Distribution",
      subtitle = "Papers mentioning only mycorrhizal fungi vs mixed papers",
      x = "Mycorrhizal Status",
      y = "Number of Abstracts",
      fill = "Prediction Type"
    ) +
    custom_theme

  # Fungal guild analysis (if funguild_guild column exists)
  if ("funguild_guild" %in% names(results_clean)) {
    guild_data <- results_clean %>%
      filter(!is.na(funguild_guild), funguild_guild != "Non-fungal") %>%
      count(funguild_guild, name = "count") %>%
      mutate(
        percentage = count / sum(count) * 100,
        funguild_guild = fct_reorder(funguild_guild, count)
      ) %>%
      slice_max(count, n = 10)  # Top 10 guilds

    p5b_fungal_guilds <- guild_data %>%
      ggplot(aes(x = funguild_guild, y = count)) +
      geom_col(fill = endo_palette[5], width = 0.8) +
      geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
                hjust = -0.1, size = 2.5) +
      labs(
        title = "Fungal Guild Distribution",
        subtitle = "Top 10 fungal guilds identified in abstracts",
        x = "Fungal Guild",
        y = "Number of Mentions"
      ) +
      coord_flip() +
      custom_theme
  }

  # Save mycorrhizal plots individually
  save_plot(p5_mycorrhizal_distribution, "plots/extraction/mycorrhizal_distribution.png", height = 6)
  if (exists("p5b_fungal_guilds")) {
    save_plot(p5b_fungal_guilds, "plots/extraction/fungal_guilds.png", height = 8)
  }

} else {
  cat("No mycorrhizal classification data available for visualization.\n")
}

# 5. PREDICTION QUALITY ANALYSIS -----------------------------------------

cat("Creating prediction quality analysis plots...\n")

# Confidence distribution - handle both confidence types with conditional plotting
if (sum(!is.na(results_clean$confidence) & is.numeric(results_clean$confidence)) > 0) {
  # Numeric confidence scores available
  p9_confidence <- results_clean %>%
    filter(!is.na(confidence)) %>%
    ggplot(aes(x = confidence, fill = predicted_label)) +
    geom_histogram(bins = 20, alpha = 0.7) +
    facet_wrap(~predicted_label, scales = "free_y") +
    scale_fill_manual(values = endo_colors$presence_absence) +
    labs(
      title = "Prediction Confidence Distribution",
      subtitle = "Model confidence scores by prediction type",
      x = "Confidence Score",
      y = "Number of Abstracts",
      fill = "Prediction"
    ) +
    custom_theme +
    theme(legend.position = "none")
} else if (sum(!is.na(results_clean$confidence_level)) > 0) {
  # Categorical confidence levels available (absence detection)
  conf_dist_data <- results_clean %>%
    filter(!is.na(confidence_level), !is.na(predicted_label)) %>%
    count(confidence_level, predicted_label)

  p9_confidence <- conf_dist_data %>%
    ggplot(aes(x = confidence_level, y = n, fill = predicted_label)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = c(endo_colors$presence_absence, "Unknown" = "lightgray")) +
    labs(
      title = "Confidence Level Distribution",
      subtitle = "Confidence levels by prediction type",
      x = "Confidence Level",
      y = "Number of Abstracts",
      fill = "Prediction"
    ) +
    custom_theme
} else {
  # No confidence data available - create placeholder
  p9_confidence <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "No confidence data available",
             size = 6, alpha = 0.7) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()
}

# Information completeness by confidence - handle both confidence types
completeness_base <- results_clean %>%
  mutate(
    completeness_score = (as.numeric(has_species) + as.numeric(has_plant_parts) +
                         as.numeric(has_geography) + as.numeric(methods_combined != "No methods detected")) / 4
  )

# Use different confidence filters based on available data
if (sum(!is.na(completeness_base$confidence) & is.numeric(completeness_base$confidence)) > 0) {
  completeness_data <- completeness_base %>%
    filter(!is.na(confidence))
} else if (sum(!is.na(completeness_base$confidence_level)) > 0) {
  completeness_data <- completeness_base %>%
    filter(!is.na(confidence_level))
} else {
  # No confidence data - use all data but warn
  completeness_data <- completeness_base
  warning("No confidence data available for completeness analysis")
}

completeness_data <- completeness_data %>%
  group_by(confidence_cat, predicted_label) %>%
  summarise(
    avg_completeness = mean(completeness_score),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count >= 5)  # Only show categories with enough data

p10_completeness <- completeness_data %>%
  ggplot(aes(x = fct_reorder(confidence_cat, avg_completeness), 
             y = avg_completeness, fill = predicted_label)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_text(aes(label = count), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = endo_colors$presence_absence) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Information Completeness by Confidence Level",
    subtitle = "Higher confidence predictions tend to have more complete information",
    x = "Confidence Category",
    y = "Average Information Completeness",
    fill = "Prediction",
    caption = "Numbers show abstract count per category"
  ) +
  custom_theme

# Data quality heatmap
quality_matrix <- results_clean %>%
  group_by(predicted_label) %>%
  summarise(
    `Species Detection` = mean(has_species) * 100,
    `Plant Parts` = mean(has_plant_parts) * 100,
    `Geography` = mean(has_geography) * 100,
    `Research Methods` = mean(methods_combined != "No methods detected") * 100,
    Count = n(),
    .groups = "drop"
  ) %>%
  filter(Count >= 10) %>%  # Only show categories with sufficient data
  select(-Count) %>%
  pivot_longer(-predicted_label, names_to = "Information_Type", values_to = "Percentage")

p11_quality_heatmap <- quality_matrix %>%
  ggplot(aes(x = Information_Type, y = predicted_label, fill = Percentage)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = 50, name = "Detection\nRate (%)") +
  labs(
    title = "Information Completeness Heatmap",
    subtitle = "Detection rates for different types of information by prediction",
    x = "Information Type",
    y = "Prediction Type"
  ) +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save quality plots individually
save_plot(p9_confidence, "plots/extraction/confidence_distribution.png", height = 6)
save_plot(p10_completeness, "plots/extraction/completeness_by_confidence.png", height = 6)
save_plot(p11_quality_heatmap, "plots/extraction/quality_heatmap.png", height = 6)

# 6. COMPREHENSIVE SUMMARY DASHBOARD ------------------------------------

cat("Creating comprehensive summary dashboard...\n")

# Key metrics summary
total_abstracts <- nrow(results_clean)
species_rate <- mean(results_clean$has_species) * 100
methods_rate <- mean(results_clean$methods_combined != "No methods detected") * 100
geography_rate <- mean(results_clean$has_geography) * 100
plant_parts_rate <- mean(results_clean$has_plant_parts) * 100

# Create summary text plot
summary_text <- paste0(
  "COMPREHENSIVE EXTRACTION SUMMARY\n\n",
  "Total Abstracts Analyzed: ", format(total_abstracts, big.mark = ","), "\n",
  "Species Detection Rate: ", round(species_rate, 1), "%\n",
  "Research Methods Detected: ", round(methods_rate, 1), "%\n", 
  "Geographic Information: ", round(geography_rate, 1), "%\n",
  "Plant Parts Information: ", round(plant_parts_rate, 1), "%\n\n",
  "Top Species Kingdoms:\n",
  if(sum(results_clean$has_species) > 0) {
    paste(results_clean %>% 
          filter(has_species) %>% 
          count(kingdom, sort = TRUE) %>% 
          mutate(perc = round(n/sum(n)*100, 1)) %>%
          unite(summary, kingdom, n, perc, sep = ": ") %>%
          pull(summary), collapse = "\n")
  } else {"No species data available"},
  "\n\nMost Common Methods:\n",
  paste0("Molecular: ", sum(results_clean$has_molecular), " abstracts\n",
         "Culture: ", sum(results_clean$has_culture), " abstracts\n",
         "Microscopy: ", sum(results_clean$has_microscopy), " abstracts")
)

p_summary_text <- ggplot() +
  annotate("text", x = 0.1, y = 0.5, label = summary_text, 
           hjust = 0, vjust = 0.5, size = 4, family = "mono") +
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "black", size = 1))

# Information type distribution
info_summary <- data.frame(
  Type = c("Species", "Methods", "Geography", "Plant Parts"),
  Count = c(sum(results_clean$has_species),
            sum(results_clean$methods_combined != "No methods detected"),
            sum(results_clean$has_geography),
            sum(results_clean$has_plant_parts)),
  Percentage = c(species_rate, methods_rate, geography_rate, plant_parts_rate)
) %>%
  mutate(Type = fct_reorder(Type, Count))

p_info_summary <- info_summary %>%
  ggplot(aes(x = Type, y = Count, fill = Type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = get_endo_colors(4)) +
  labs(
    title = "Information Detection Overview",
    subtitle = "Extraction success rates across all abstracts",
    x = "Information Type",
    y = "Number of Abstracts",
    caption = paste("Based on", total_abstracts, "total abstracts")
  ) +
  custom_theme +
  theme(legend.position = "none")

# Save summary dashboard plots individually
save_plot(p_summary_text, "plots/extraction/summary_text.png", width = 8, height = 8)
save_plot(p_info_summary, "plots/extraction/info_summary.png", width = 8, height = 8)

# 7. SAVE VISUALIZATION SUMMARY ------------------------------------------

cat("\nCreating visualization summary report...\n")

capture.output({
  cat("=== EXTRACTION RESULTS VISUALIZATION SUMMARY ===\n")
  cat("Generated:", Sys.time(), "\n")
  cat("Source: comprehensive_extraction_results.csv\n\n")
  
  cat("FILES GENERATED:\n")
  cat("1. plots/extraction/species_detection_rate.png - Overall species detection rate\n")
  cat("2. plots/extraction/methods_individual_core.png - Core research methods (3 traditional types)\n")
  cat("3. plots/extraction/methods_individual_expanded.png - Expanded research methods (all 9 types)\n")
  cat("4. plots/extraction/methods_combined_core.png - Combined core method frequencies\n")
  cat("5. plots/extraction/methods_combined_expanded.png - Combined expanded method frequencies\n")
  if (sum(results_clean$has_plant_parts) > 0) {
    cat("6. plots/extraction/plant_parts_frequency.png - Most studied plant parts (enhanced)\n")
    cat("7. plots/extraction/plant_parts_distribution.png - Plant parts count distribution\n")
    cat("8. plots/extraction/plant_parts_by_prediction.png - Plant parts detection by prediction type\n")
  }
  if (sum(results_clean$has_geography) > 0) {
    cat("9. plots/extraction/geographic_regions.png - Geographic distribution by region\n")
    cat("10. plots/extraction/top_countries.png - Most studied countries\n")
    cat("11. plots/extraction/continents_distribution.png - Studies by continent\n")
    cat("12. plots/extraction/geographic_completeness.png - Geographic information completeness\n")
  }
  if (sum(!is.na(results_clean$is_mycorrhizal)) > 0) {
    cat("13. plots/extraction/mycorrhizal_distribution.png - Mycorrhizal-only papers distribution\n")
    if ("funguild_guild" %in% names(results_clean)) {
      cat("14. plots/extraction/fungal_guilds.png - Fungal guild distribution\n")
    }
  }
  cat("15. plots/extraction/confidence_distribution.png - Confidence distributions by prediction\n")
  cat("16. plots/extraction/completeness_by_confidence.png - Information completeness by confidence\n")
  cat("17. plots/extraction/quality_heatmap.png - Information detection rates heatmap\n")
  cat("18. plots/extraction/summary_text.png - Overall summary statistics\n")
  cat("19. plots/extraction/info_summary.png - Information detection overview\n\n")
  
  cat("KEY FINDINGS:\n")
  cat("- Species detection rate:", round(species_rate, 1), "%\n")
  cat("- Research methods coverage:", round(methods_rate, 1), "%\n")
  cat("- Geographic information:", round(geography_rate, 1), "%\n")
  cat("- Plant parts information:", round(plant_parts_rate, 1), "%\n\n")
  
  if (sum(results_clean$has_species) > 0) {
    kingdom_breakdown <- results_clean %>%
      filter(has_species) %>%
      count(kingdom, sort = TRUE) %>%
      mutate(percentage = round(n/sum(n)*100, 1))
    
    cat("SPECIES BY KINGDOM:\n")
    for (i in 1:nrow(kingdom_breakdown)) {
      cat("-", kingdom_breakdown$kingdom[i], ":", kingdom_breakdown$n[i], 
          "abstracts (", kingdom_breakdown$percentage[i], "%)\n")
    }
    cat("\n")
  }
  
  cat("RESEARCH METHODS BREAKDOWN:\n")
  cat("- Molecular methods:", sum(results_clean$has_molecular), "abstracts\n")
  cat("- Culture-based methods:", sum(results_clean$has_culture), "abstracts\n")
  cat("- Microscopy methods:", sum(results_clean$has_microscopy), "abstracts\n\n")
  
  cat("PREDICTION TYPE ANALYSIS:\n")
  pred_breakdown <- results_clean %>%
    count(predicted_label, sort = TRUE) %>%
    mutate(percentage = round(n/sum(n)*100, 1))
  
  for (i in 1:nrow(pred_breakdown)) {
    cat("-", pred_breakdown$predicted_label[i], ":", pred_breakdown$n[i],
        "abstracts (", pred_breakdown$percentage[i], "%)\n")
  }
  
  cat("\nRECOMMENDations FOR ANALYSIS:\n")
  cat("1. Focus manual review on 'Presence' predictions with species detected\n")
  cat("2. Investigate 'Absence' predictions that have species (potential misclassification)\n")
  cat("3. Prioritize abstracts with complete information (species + methods + geography)\n")
  cat("4. Use plant parts data for ecological niche analysis\n")
  cat("5. Consider geographic patterns for global endophyte distribution studies\n")
  
}, file = "results/visualization_summary_report.txt")

cat("Visualization summary saved to: results/visualization_summary_report.txt\n")

# Clean up
cat("\n=== VISUALIZATION COMPLETE ===\n")
cat("Generated", length(list.files("plots/extraction", pattern = "*.png")), "visualization files in plots/extraction/\n")
cat("\nKey outputs:\n")
cat("📊 Species detection patterns and kingdom distribution (individual plots)\n")
cat("🔬 Research methods usage and combinations - both core (3 types) and expanded (9 types)\n")
if (sum(results_clean$has_plant_parts) > 0) cat("🌱 Enhanced plant parts frequency with normalization and count distribution\n")
if (sum(results_clean$has_geography) > 0) cat("🌍 Comprehensive geographic distribution with countries, continents, and regions\n")
if (sum(!is.na(results_clean$is_mycorrhizal)) > 0) cat("🍄 Mycorrhizal classification analysis including fungal guilds\n")
cat("📈 Prediction quality and information completeness (individual plots)\n")
cat("📋 Summary statistics and information overview (individual plots)\n\n")

cat("Next steps:\n")
cat("1. Review generated individual plots for patterns and insights\n")
cat("2. Use findings to guide systematic review priorities\n")
cat("3. Consider combining plots manually in external software if needed\n")
cat("4. Integrate visualizations into research presentations and publications\n\n")

cat("All individual visualization files saved to plots/extraction/ directory! 📊✨\n")
