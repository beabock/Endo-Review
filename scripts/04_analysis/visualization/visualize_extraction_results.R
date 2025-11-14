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
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)  # For coordinate reference system transformations

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
save_plot <- function(plot, filename, version, width = 12, height = 8, dpi = 300) {
  dir_path <- if(version == "main") "plots/main/" else "plots/supplementary/"
  full_path <- paste0(dir_path, filename)
  dir.create(dirname(full_path), showWarnings = FALSE, recursive = TRUE)
  ggsave(full_path, plot, width = width, height = height, dpi = dpi, units = "in")
  cat("Saved plot:", full_path, "\n")
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

# Add mycorrhizal filtering capabilities for dual analysis modes
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
      filter(is.na(is_mycorrhizal_only) | !is_mycorrhizal_only)
    message("Filtered out ", nrow(data) - nrow(filtered_data), " mycorrhizal-only abstracts")
    return(filtered_data)
  }
}

# Create main and supplementary datasets for dual analysis
main_data <- filter_mycorrhizal_papers(results_clean, include_mycorrhizal_only = FALSE)
supp_data <- filter_mycorrhizal_papers(results_clean, include_mycorrhizal_only = TRUE)

# Create versions for dual analysis
versions <- list("main" = main_data, "supp" = supp_data)

for (version_name in names(versions)) {
  data <- versions[[version_name]]
  version_prefix <- version_name

  cat("\n=== GENERATING", toupper(version_prefix), "VERSION VISUALIZATIONS ===\n\n")

  # Recalculate summary statistics for this dataset
  abstract_summary <- data %>%
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

  cat("\nData Summary for ", version_prefix, ":\n")
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

  cat("Creating species detection overview plots (", version_prefix, ")...\n")

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
  save_plot(p1_species_detection, "species_detection_rate.png", version_prefix, height = 6)

  # 2. RESEARCH METHODS ANALYSIS -------------------------------------------

  cat("Creating research methods analysis plots (", version_prefix, ")...\n")

  # Original 3 methods distribution
  methods_abstract_summary <- data %>%
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
  methods_expanded_summary <- data %>%
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
    scale_fill_manual(values = get_endo_colors(n = nrow(methods_expanded_counts))) +
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
  methods_combined_summary <- data %>%
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
    scale_fill_manual(values = get_endo_colors(n = nrow(methods_combined_summary))) +
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
  methods_expanded_combined_summary <- data %>%
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
    scale_fill_manual(values = get_endo_colors(n = nrow(methods_expanded_combined_summary))) +
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
  save_plot(p3_methods_individual, "methods_individual_core.png", version_prefix, height = 6)
  save_plot(p3b_methods_expanded, "methods_individual_expanded.png", version_prefix, height = 8)
  save_plot(p4_methods_combined, "methods_combined_core.png", version_prefix, height = 6)
  save_plot(p4b_methods_expanded_combined, "methods_combined_expanded.png", version_prefix, height = 8)

  # 3. PLANT PARTS ANALYSIS ------------------------------------------------

  cat("Creating plant parts analysis plots (", version_prefix, ")...\n")

  if (sum(abstract_summary$has_plant_parts) > 0) {

    # Enhanced plant parts analysis using new parts_count and parts_normalized columns
    plant_parts_data <- data %>%
      filter(has_plant_parts, !is.na(parts_normalized)) %>%
      select(id, plant_parts_detected, parts_normalized, parts_count) %>%
      # Use the normalized parts data for consistent grouping
      separate_rows(parts_normalized, sep = "; ") %>%
      filter(!is.na(parts_normalized), parts_normalized != "",
             !grepl("mycorrhiza", parts_normalized, ignore.case = TRUE),
             !grepl("proliferation", parts_normalized, ignore.case = TRUE)) %>%
      # Count unique abstracts per normalized plant part (not total mentions)
      distinct(id, parts_normalized) %>%
      count(parts_normalized, name = "unique_abstracts") %>%
      arrange(desc(unique_abstracts)) %>%
      slice_max(unique_abstracts, n = 15) %>%  # Top 15 most frequent
      mutate(
        display_name = stringr::str_to_title(parts_normalized),
        display_name = fct_reorder(display_name, unique_abstracts)
      )

    p6_plant_parts_freq <- plant_parts_data %>%
      ggplot(aes(x = display_name, y = unique_abstracts)) +
      geom_col(fill = endo_palette[1], width = 0.8) +
      geom_text(aes(label = unique_abstracts), hjust = -0.1, size = 3) +
      labs(
        title = "Most Frequently Studied Plant Parts",
        subtitle = paste("Top 15 normalized plant parts from", sum(abstract_summary$has_plant_parts), "unique abstracts"),
        x = "Plant Part",
        y = "Number of Unique Abstracts"
      ) +
      coord_flip() +
      custom_theme

    # Plant parts count distribution analysis (unique abstracts only)
    parts_count_summary <- data %>%
      filter(has_plant_parts) %>%
      distinct(id, parts_count) %>%
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
      scale_fill_manual(values = get_endo_colors(n = nrow(parts_count_summary))) +
      labs(
        title = "Plant Parts Count Distribution",
        subtitle = "Number of abstracts mentioning specific numbers of plant parts",
        x = "Number of Plant Parts per Abstract",
        y = "Number of Abstracts"
      ) +
      custom_theme +
      theme(legend.position = "none")

    # Save plant parts plots individually
    save_plot(p6_plant_parts_freq, "plant_parts_frequency.png", version_prefix, height = 8)
    save_plot(p6b_plant_parts_distribution, "plant_parts_distribution.png", version_prefix, height = 6)

  } else {
    cat("No plant parts data available for visualization.\n")
  }

  # 4. GEOGRAPHIC ANALYSIS -------------------------------------------------

  cat("Creating geographic distribution plots (", version_prefix, ")...\n")

  if (sum(abstract_summary$has_geography) > 0) {

    # Enhanced geographic analysis using new specific columns
    # Global North vs South distribution (unique abstracts only)
    geo_data <- abstract_summary %>%
      left_join(data %>% select(id, geographic_region) %>% distinct(), by = "id") %>%
      count(geographic_region, name = "count")

    # Add logging for diagnosis
    cat("DEBUG: Unique geographic regions found:\n")
    print(unique(geo_data$geographic_region))
    cat("DEBUG: Number of unique geographic regions:", nrow(geo_data), "\n")
    cat("DEBUG: Geographic region counts:\n")
    print(geo_data)

    p7_geo_regions <- geo_data %>%
      mutate(
        percentage = count / sum(count) * 100,
        geographic_region = fct_reorder(geographic_region, count)
      ) %>%
      ggplot(aes(x = geographic_region, y = count, fill = geographic_region)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
                hjust = -0.1, size = 3.5) +
      scale_fill_manual(values = get_endo_colors(nrow(geo_data))) +
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
    countries_data <- data %>%
      filter(!is.na(countries_detected)) %>%
      select(id, countries_detected) %>%
      distinct(id, countries_detected) %>%  # One record per abstract per country combination
      separate_rows(countries_detected, sep = "; ") %>%
      filter(!is.na(countries_detected), countries_detected != "") %>%
      # Standardize country names
      mutate(country_std = if (exists("normalize_country_vector")) {
        normalize_country_vector(countries_detected)
      } else {
        stringr::str_to_title(countries_detected)
      }) %>%
      distinct(id, country_std) %>%  # Ensure unique abstract-country combinations
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
        subtitle = paste("Top 15 countries from", length(unique(data$id[!is.na(data$countries_detected)])), "abstracts with country information"),
        x = "Country",
        y = "Number of Abstracts"
      ) +
      coord_flip() +
      custom_theme

    # Bottom countries distribution
    all_countries <- unique(countrycode::codelist$country.name.en)
    all_countries_std <- if (exists("normalize_country_vector")) {
      normalize_country_vector(all_countries)
    } else {
      stringr::str_to_title(all_countries)
    }
    all_countries_df <- data.frame(country_std = all_countries_std, frequency = 0)
    countries_data_full <- all_countries_df %>%
      left_join(countries_data %>% select(country_std, frequency), by = "country_std", suffix = c("", ".y")) %>%
      mutate(frequency = coalesce(frequency.y, 0)) %>%
      select(country_std, frequency)
    bottom_countries_data <- countries_data_full %>%
      arrange(frequency) %>%
      head(15) %>%
      mutate(country_display = fct_reorder(stringr::str_to_title(country_std), frequency))

    p8_bottom_countries <- bottom_countries_data %>%
      ggplot(aes(x = country_display, y = frequency)) +
      geom_col(fill = endo_palette[4], width = 0.8) +
      geom_text(aes(label = frequency), hjust = 1.1, size = 3) +
      labs(
        title = "Least Represented Countries",
        subtitle = "Bottom 15 countries from global list, including those with 0 abstracts",
        x = "Country",
        y = "Number of Abstracts"
      ) +
      coord_flip() +
      custom_theme

    # Continents distribution using new continents_detected column
    continents_data <- data %>%
      filter(!is.na(continents_detected)) %>%
      select(id, continents_detected) %>%
      distinct(id, continents_detected) %>%  # One record per abstract per continent combination
      separate_rows(continents_detected, sep = "; ") %>%
      filter(!is.na(continents_detected), continents_detected != "") %>%
      distinct(id, continents_detected) %>%  # Ensure unique abstract-continent combinations
      count(continents_detected, name = "frequency") %>%
      arrange(desc(frequency)) %>%
      mutate(continent_display = fct_reorder(continents_detected, frequency))

    # Geographic information completeness
    geo_completeness <- data %>%
      filter(has_geography) %>%
      distinct(id, countries_detected, continents_detected, regions_detected, has_coordinates) %>%
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
      scale_fill_manual(values = get_endo_colors(n = nrow(geo_completeness))) +
      labs(
        title = "Geographic Information Completeness",
        subtitle = "Types of geographic information detected",
        x = "Geographic Information Type",
        y = "Number of Abstracts"
      ) +
      custom_theme +
      theme(legend.position = "none")

    # World choropleth map with Robinson projection
    world <- ne_countries(scale = "medium", returnclass = "sf")
    countries_map_data <- data %>%
      filter(!is.na(countries_detected)) %>%
      select(id, countries_detected) %>%
      distinct(id, countries_detected) %>%
      separate_rows(countries_detected, sep = "; ") %>%
      filter(!is.na(countries_detected), countries_detected != "") %>%
      mutate(country_std = if (exists("normalize_country_vector")) {
        normalize_country_vector(countries_detected)
      } else {
        stringr::str_to_title(countries_detected)
      }) %>%
      distinct(id, country_std) %>%
      count(country_std, name = "frequency") %>%
      mutate(country_std = str_trim(country_std))
    countries_map_data <- countries_map_data %>%
      mutate(iso3 = countrycode(country_std, "country.name", "iso3c", warn = FALSE)) %>%
      filter(!is.na(iso3))
    world_data <- world %>%
      left_join(countries_map_data, by = c("iso_a3" = "iso3"))

    # Transform to Robinson projection
    world_robinson <- st_transform(world_data, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

    p_world_map <- ggplot(data = world_robinson) +
      geom_sf(aes(fill = frequency), color = "white", size = 0.1) +
      scale_fill_gradient(low = endo_colors$gradient_low, high = endo_colors$gradient_high,
                     na.value = "lightgray",
                     name = "Abstract Count",
                     breaks = scales::breaks_pretty(n = 4),
                     labels = scales::comma_format(accuracy = 1)) +
      labs(
        title = if(version_prefix == "main") "Global Distribution of Endophyte Research" else paste("Global Distribution of Endophyte Research -", toupper(version_prefix), "Version"),
        subtitle = "Number of abstracts mentioning each country (Robinson projection)",
        caption = paste("Total abstracts with geography:", sum(abstract_summary$has_geography), "• Gray = 0 studies")
      ) +
      custom_theme +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.5, "cm")
      )

    # Log-scale version of world map for main version only with Robinson projection
    if (version_prefix == "main") {
      p_world_map_log <- ggplot(data = world_robinson) +
        geom_sf(aes(fill = frequency), color = "white", size = 0.1) +
        scale_fill_gradient(low = endo_colors$gradient_low, high = endo_colors$gradient_high,
                           na.value = "lightgray",
                           name = "Abstract Count",
                           trans = "log10",
                           breaks = c(1, 10, 100, 1000),
                           labels = scales::comma_format(accuracy = 1)) +
        labs(
          title = "Global Distribution of Endophyte Research (Log Scale)",
          subtitle = "Number of abstracts mentioning each country (Robinson projection, logarithmic scale)",
          caption = paste("Total abstracts with geography:", sum(abstract_summary$has_geography), "• Gray = 0 studies")
        ) +
        custom_theme +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.5, "cm")
        )
    }

    # Create CSV of lowest represented countries in the tropics
    cat("Creating CSV of lowest represented countries in tropics...\n")

    # Get tropical countries using predefined list from reference data utils
    source("scripts/04_analysis/utilities/reference_data_utils.R")

    # Define tropical zone countries (between Tropic of Cancer and Capricorn, plus other commonly considered tropical countries)
    tropical_zone_countries <- c(
      # Africa
      "Nigeria", "Ghana", "Ivory Coast", "Senegal", "Mali", "Burkina Faso", "Chad", "Sudan",
      "Ethiopia", "Kenya", "Tanzania", "Uganda", "Rwanda", "Cameroon", "Central African Republic",
      "Gabon", "Equatorial Guinea", "Congo", "Democratic Republic of the Congo", "Angola",
      "Mozambique", "Madagascar", "Mauritius", "Seychelles",
      # Asia
      "India", "Sri Lanka", "Thailand", "Vietnam", "Cambodia", "Laos", "Malaysia", "Indonesia",
      "Philippines", "Papua New Guinea", "Bangladesh", "Myanmar", "Brunei",
      # Americas
      "Brazil", "Colombia", "Peru", "Ecuador", "Venezuela", "Guyana", "Suriname", "French Guiana",
      "Panama", "Costa Rica", "Nicaragua", "Honduras", "Belize", "Mexico",
      # Oceania
      "Australia", "Timor-Leste", "Solomon Islands", "Vanuatu", "Samoa", "Fiji", "Tonga"
    )

    # Filter bottom countries data for tropical countries only
    tropical_bottom_countries <- countries_data_full %>%
      filter(country_std %in% tropical_zone_countries) %>%
      arrange(frequency) %>%
      filter(frequency < 15) %>%  # Include all countries with fewer than 15 observations
      select(country_std, frequency) %>%
      mutate(rank = row_number())

    # Calculate percentage of abstracts from North America and Europe only
    north_america_europe_only_abstracts <- data %>%
      filter(!is.na(countries_detected)) %>%
      mutate(
        # Check if countries are only from North America and Europe
        countries_list = str_split(countries_detected, "; "),
        is_north_america_europe_only = sapply(countries_list, function(countries) {
          if (length(countries) == 0) return(NA)
          # Get standardized country names
          std_countries <- if (exists("normalize_country_vector")) {
            normalize_country_vector(countries)
          } else {
            stringr::str_to_title(countries)
          }
          # Get Global North countries
          global_north <- get_global_north_countries()
          # Check if all countries are in Global North (which includes North America and Europe)
          all(std_countries %in% global_north)
        })
      ) %>%
      filter(is_north_america_europe_only == TRUE) %>%
      distinct(id) %>%
      nrow()

    total_abstracts_with_geography <- data %>%
      filter(!is.na(countries_detected)) %>%
      distinct(id) %>%
      nrow()

    north_america_europe_only_percentage <- if (total_abstracts_with_geography > 0) {
      round((north_america_europe_only_abstracts / total_abstracts_with_geography) * 100, 2)
    } else {
      0
    }

    # Create summary text for North America/Europe dominance
    na_eu_summary <- paste0(
      "North America and Europe Only Analysis (", toupper(version_prefix), " VERSION)\n",
      "Total abstracts with geography: ", total_abstracts_with_geography, "\n",
      "Abstracts from North America/Europe only: ", north_america_europe_only_abstracts, "\n",
      "Percentage: ", north_america_europe_only_percentage, "%\n"
    )

    # Save to a separate file
    writeLines(na_eu_summary, file.path("results", paste0("north_america_europe_only_analysis_", version_prefix, ".txt")))
    cat("✓ Saved North America/Europe dominance analysis to results/north_america_europe_only_analysis_", version_prefix, ".txt\n")

    # Save to CSV
    write.csv(tropical_bottom_countries, file.path("results", paste0("lowest_represented_tropical_countries_", version_prefix, ".csv")), row.names = FALSE)
    cat("✓ Saved lowest represented tropical countries to results/lowest_represented_tropical_countries_", version_prefix, ".csv\n")

    # Save geography plots individually
    save_plot(p7_geo_regions, "geographic_regions.png", version_prefix, height = 6)
    save_plot(p8_countries, "top_countries.png", version_prefix, height = 8)
    save_plot(p8_bottom_countries, "bottom_countries.png", version_prefix, height = 8)
    save_plot(p8c_geo_completeness, "geographic_completeness.png", version_prefix, height = 6)
    save_plot(p_world_map, "world_map_countries.png", version_prefix, width = 12, height = 8)
    if (version_prefix == "main") {
      save_plot(p_world_map_log, "world_map_countries_log.png", version_prefix, width = 12, height = 8)
    }

  } else {
    cat("No geographic data available for visualization.\n")
  }

  if (version_prefix == "supp") {
    # 4. MYCORRHIZAL CLASSIFICATION ANALYSIS ---------------------------------

    cat("Creating mycorrhizal classification analysis plots (supp only)...\n")

    if (sum(!is.na(data$is_mycorrhizal)) > 0) {


    } else {
      cat("No mycorrhizal classification data available for visualization.\n")
    }
  }


  # 6. COMPREHENSIVE SUMMARY DASHBOARD ------------------------------------

  cat("Creating comprehensive summary dashboard (", version_prefix, ")...\n")

  # Key metrics summary
  total_abstracts <- nrow(abstract_summary)
  species_rate <- mean(abstract_summary$has_species) * 100
  methods_rate <- mean(abstract_summary$has_methods) * 100
  geography_rate <- mean(abstract_summary$has_geography) * 100
  plant_parts_rate <- mean(abstract_summary$has_plant_parts) * 100

  # Create summary text plot
  summary_text <- paste0(
    "COMPREHENSIVE EXTRACTION SUMMARY (", toupper(version_prefix), " VERSION)\n\n",
    "Total Abstracts Analyzed: ", format(total_abstracts, big.mark = ","), "\n",
    "Species Detection Rate: ", round(species_rate, 1), "%\n",
    "Research Methods Detected: ", round(methods_rate, 1), "%\n",
    "Geographic Information: ", round(geography_rate, 1), "%\n",
    "Plant Parts Information: ", round(plant_parts_rate, 1), "%\n\n",
    "Top Species Kingdoms:\n",
    if(sum(abstract_summary$has_species) > 0) {
      paste(data %>%
            filter(has_species) %>%
            select(id, kingdom) %>%
            distinct(id, kingdom) %>%
            count(kingdom, sort = TRUE) %>%
            mutate(perc = round(n/sum(n)*100, 1)) %>%
            unite(summary, kingdom, n, perc, sep = ": ") %>%
            pull(summary), collapse = "\n")
    } else {"No species data available"},
    "\n\nMost Common Methods:\n",
    paste0("Molecular: ", sum(methods_abstract_summary$has_molecular), " abstracts\n",
           "Culture: ", sum(methods_abstract_summary$has_culture), " abstracts\n",
           "Microscopy: ", sum(methods_abstract_summary$has_microscopy), " abstracts")
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
    Count = c(sum(abstract_summary$has_species),
              sum(abstract_summary$has_methods),
              sum(abstract_summary$has_geography),
              sum(abstract_summary$has_plant_parts)),
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
  save_plot(p_summary_text, "summary_text.png", version_prefix, width = 8, height = 8)
  save_plot(p_info_summary, "info_summary.png", version_prefix, width = 8, height = 8)

  # 7. SAVE VISUALIZATION SUMMARY ------------------------------------------

  cat("\nCreating visualization summary report (", version_prefix, ")...\n")

  dir_prefix <- if(version_prefix == "main") "plots/main/" else "plots/supplementary/"

  capture.output({
    cat("=== EXTRACTION RESULTS VISUALIZATION SUMMARY (", toupper(version_prefix), " VERSION) ===\n")
    cat("Generated:", Sys.time(), "\n")
    cat("Source: comprehensive_extraction_results.csv\n\n")

    cat("FILES GENERATED:\n")
    cat("1. ", dir_prefix, "species_detection_rate.png - Overall species detection rate\n")
    cat("2. ", dir_prefix, "methods_individual_core.png - Core research methods (3 traditional types)\n")
    cat("3. ", dir_prefix, "methods_individual_expanded.png - Expanded research methods (all 9 types)\n")
    cat("4. ", dir_prefix, "methods_combined_core.png - Combined core method frequencies\n")
    cat("5. ", dir_prefix, "methods_combined_expanded.png - Combined expanded method frequencies\n")
    if (sum(abstract_summary$has_plant_parts) > 0) {
      cat("6. ", dir_prefix, "plant_parts_frequency.png - Most studied plant parts (enhanced)\n")
      cat("7. ", dir_prefix, "plant_parts_distribution.png - Plant parts count distribution\n")
      
    }
    if (sum(abstract_summary$has_geography) > 0) {
      cat("9. ", dir_prefix, "geographic_regions.png - Geographic distribution by region\n")
      cat("10. ", dir_prefix, "top_countries.png - Most studied countries\n")
      cat("11. ", dir_prefix, "bottom_countries.png - Least represented countries\n")
      cat("12. ", dir_prefix, "geographic_completeness.png - Geographic information completeness\n")
      cat("13. ", dir_prefix, "world_map_countries.png - World choropleth map of countries\n")
      if (version_prefix == "main") {
        cat("13b. ", dir_prefix, "world_map_countries_log.png - World choropleth map of countries (log scale)\n")
      }
    }
    cat("13. ", dir_prefix, "summary_text.png - Overall summary statistics\n")
    cat("14. ", dir_prefix, "info_summary.png - Information detection overview\n\n")

    cat("KEY FINDINGS:\n")
    cat("- Species detection rate:", round(species_rate, 1), "%\n")
    cat("- Research methods coverage:", round(methods_rate, 1), "%\n")
    cat("- Geographic information:", round(geography_rate, 1), "%\n")
    cat("- Plant parts information:", round(plant_parts_rate, 1), "%\n\n")

    if (sum(abstract_summary$has_species) > 0) {
      kingdom_breakdown <- data %>%
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
    cat("- Molecular methods:", sum(methods_abstract_summary$has_molecular), "abstracts\n")
    cat("- Culture-based methods:", sum(methods_abstract_summary$has_culture), "abstracts\n")
    cat("- Microscopy methods:", sum(methods_abstract_summary$has_microscopy), "abstracts\n\n")

    cat("\nRECOMMENDations FOR ANALYSIS:\n")
    cat("1. Focus manual review on 'Presence' predictions with species detected\n")
    cat("2. Investigate 'Absence' predictions that have species (potential misclassification)\n")
    cat("3. Prioritize abstracts with complete information (species + methods + geography)\n")
    cat("4. Use plant parts data for ecological niche analysis\n")
    cat("5. Consider geographic patterns for global endophyte distribution studies\n")

  }, file = paste0("results/visualization_summary_report_", version_prefix, ".txt"))

  cat("Visualization summary saved to: results/visualization_summary_report_", version_prefix, ".txt\n")

}

   # 8. TEMPORAL METHOD TRENDS -----------------------------------------------
   # Create the bar plot of abstracts per year filled by detection method

   cat("Creating temporal method trends visualization (", version_prefix, ")...\n")

   # Prepare temporal method data
   temporal_method_data <- data %>%
     filter(!is.na(publication_year), publication_year >= 1926, publication_year <= 2025) %>%
     select(id, publication_year, has_molecular, has_culture, has_microscopy) %>%
     group_by(id, publication_year) %>%
     summarise(
       has_molecular = any(has_molecular),
       has_culture = any(has_culture),
       has_microscopy = any(has_microscopy),
       .groups = "drop"
     ) %>%
     # Create method category for each abstract
     mutate(
       method_category = case_when(
         has_molecular & has_culture & has_microscopy ~ "All three methods",
         has_molecular & has_culture ~ "Molecular + Culture",
         has_molecular & has_microscopy ~ "Molecular + Microscopy",
         has_culture & has_microscopy ~ "Culture + Microscopy",
         has_molecular ~ "Molecular only",
         has_culture ~ "Culture only",
         has_microscopy ~ "Microscopy only",
         TRUE ~ "No methods detected"
       )
     ) %>%
     # Aggregate by year and method
     count(publication_year, method_category, name = "abstract_count") %>%
     # Create a complete sequence of years to ensure gaps are filled
     complete(publication_year = full_seq(c(1926:2025), 1), method_category, fill = list(abstract_count = 0))

   # Create the bar plot
   method_colors <- c(
     "All three methods" = "#5B8FA8",
     "Molecular + Culture" = "#7FB77E",
     "Molecular + Microscopy" = "#D18C63",
     "Culture + Microscopy" = "#9C6843",
     "Molecular only" = "#B78FA8",
     "Culture only" = "#6E6E6E",
     "Microscopy only" = "#C9B28E",
     "No methods detected" = "lightgrey"
   )

   p_temporal_methods <- temporal_method_data %>%
     ggplot(aes(x = publication_year, y = abstract_count, fill = method_category)) +
     geom_bar(stat = "identity", position = "stack", width = 0.8) +
     scale_fill_manual(
       values = method_colors,
       name = "Detection Method"
     ) +
     scale_x_continuous(
       breaks = seq(1930, 2025, by = 10),
       expand = c(0, 0)
     ) +
     scale_y_continuous(
       expand = c(0, 0),
       labels = scales::comma_format()
     ) +
     labs(
       title = "Temporal Trends in Endophyte Detection Methods",
       subtitle = paste("Number of abstracts per year by detection method (", version_prefix, " dataset)"),
       x = "Publication Year",
       y = "Number of Abstracts",
       caption = paste("Total abstracts:", sum(temporal_method_data$abstract_count))
     ) +
     custom_theme +
     theme(
       legend.position = "bottom",
       legend.title = element_text(size = 10),
       legend.text = element_text(size = 9),
       axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
     )

   # Save the temporal methods plot
   save_plot(p_temporal_methods, "temporal_method_trends.png", version_prefix, width = 14, height = 8)

   # Create a line version for better trend visibility
   p_temporal_methods_line <- temporal_method_data %>%
     group_by(publication_year) %>%
     mutate(total_abstracts = sum(abstract_count)) %>%
     ungroup() %>%
     mutate(method_proportion = abstract_count / total_abstracts) %>%
     ggplot(aes(x = publication_year, y = method_proportion, color = method_category)) +
     geom_line(linewidth = 1.2, alpha = 0.8) +
     geom_point(size = 2, alpha = 0.8) +
     scale_color_manual(
       values = method_colors,
       name = "Detection Method"
     ) +
     scale_x_continuous(
       breaks = seq(1930, 2025, by = 10),
       expand = c(0, 0)
     ) +
     scale_y_continuous(
       labels = scales::percent_format(),
       expand = c(0, 0)
     ) +
     labs(
       title = "Proportion of Detection Methods Over Time",
       subtitle = paste("Relative frequency of each method as percentage of annual abstracts (", version_prefix, " dataset)"),
       x = "Publication Year",
       y = "Proportion of Abstracts (%)",
       caption = paste("Based on", length(unique(temporal_method_data$publication_year)), "years of data")
     ) +
     custom_theme +
     theme(
       legend.position = "bottom",
       legend.title = element_text(size = 10),
       legend.text = element_text(size = 9),
       axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
     )

   # Save the line version
   save_plot(p_temporal_methods_line, "temporal_method_proportions.png", version_prefix, width = 14, height = 8)

 

# Close the loop

# Clean up
cat("\n=== VISUALIZATION COMPLETE ===\n")
cat("Generated dual-version visualization files in plots/main/ and plots/supplementary/ subdirectories\n")
cat("Generated separate summary reports: results/visualization_summary_report_main.txt and results/visualization_summary_report_supp.txt\n")
cat("\nKey outputs:\n")
cat("📊 Species detection patterns and kingdom distribution (main and supp versions)\n")
cat("🔬 Research methods usage and combinations - both core (3 types) and expanded (9 types)\n")
cat("📈 Temporal method trends - stacked bar plot and proportion line plot (NEW)\n")
if (sum(main_data$has_plant_parts) > 0) cat("🌱 Enhanced plant parts frequency with normalization and count distribution\n")
if (sum(main_data$has_geography) > 0) cat("🌍 Comprehensive geographic distribution with countries, continents, regions, and world choropleth map (linear and log scale)\n")
cat("📋 Summary statistics and information overview (individual plots)\n\n")

cat("Next steps:\n")
cat("1. Review generated individual plots for patterns and insights\n")
cat("2. Compare main vs supp versions for differences in mycorrhizal filtering\n")
cat("3. Use findings to guide systematic review priorities\n")
cat("4. Consider combining plots manually in external software if needed\n")
cat("5. Integrate visualizations into research presentations and publications\n\n")

# Create manuscript-ready log file with key statistics for extraction results
create_manuscript_log_extraction <- function() {
  log_file <- "results/visualize_extraction_results_manuscript_log.txt"

  # Calculate key statistics for manuscript
  total_abstracts <- nrow(abstract_summary)
  abstracts_with_species <- sum(abstract_summary$has_species)
  abstracts_with_geography <- sum(abstract_summary$has_geography)
  abstracts_with_methods <- sum(abstract_summary$has_methods)

  # Geographic analysis summary
  if (sum(abstract_summary$has_geography) > 0) {
    geo_summary <- data %>%
      filter(!is.na(countries_detected)) %>%
      select(id, countries_detected) %>%
      distinct(id, countries_detected) %>%
      separate_rows(countries_detected, sep = "; ") %>%
      filter(!is.na(countries_detected), countries_detected != "") %>%
      mutate(country_std = if (exists("normalize_country_vector")) {
        normalize_country_vector(countries_detected)
      } else {
        stringr::str_to_title(countries_detected)
      }) %>%
      distinct(id, country_std) %>%
      count(country_std, name = "frequency") %>%
      arrange(desc(frequency))

    top_countries <- head(geo_summary, 5)
  }

  # Methods summary
  if (sum(abstract_summary$has_methods) > 0) {
    methods_summary <- data %>%
      group_by(id) %>%
      summarise(methods_combined = first(methods_combined), .groups = "drop") %>%
      count(methods_combined, name = "count") %>%
      mutate(percentage = count / sum(count) * 100) %>%
      arrange(desc(count))
  }

  capture.output({
    cat("=== VISUALIZE_EXTRACTION_RESULTS.R MANUSCRIPT STATISTICS ===\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

    cat("DATASET OVERVIEW:\n")
    cat("================\n")
    cat(sprintf("- Total abstracts analyzed: %s\n", format(total_abstracts, big.mark = ",")))
    cat(sprintf("- Abstracts with species detected: %s (%.1f%%)\n",
                format(abstracts_with_species, big.mark = ","),
                round(abstracts_with_species/total_abstracts * 100, 1)))
    cat(sprintf("- Abstracts with geographic information: %s (%.1f%%)\n",
                format(abstracts_with_geography, big.mark = ","),
                round(abstracts_with_geography/total_abstracts * 100, 1)))
    cat(sprintf("- Abstracts with research methods: %s (%.1f%%)\n\n",
                format(abstracts_with_methods, big.mark = ","),
                round(abstracts_with_methods/total_abstracts * 100, 1)))

    if (exists("geo_summary")) {
      cat("GEOGRAPHIC DISTRIBUTION:\n")
      cat("=======================\n")
      cat("Top 5 countries by number of abstracts:\n")
      for(i in 1:min(5, nrow(geo_summary))) {
        country <- geo_summary[i,]
        cat(sprintf("%d. %s: %s abstracts\n", i, country$country_std, format(country$frequency, big.mark = ",")))
      }
      cat("\n")
    }

    if (exists("methods_summary")) {
      cat("RESEARCH METHODS:\n")
      cat("================\n")
      cat("Most common method combinations:\n")
      for(i in 1:min(5, nrow(methods_summary))) {
        method <- methods_summary[i,]
        cat(sprintf("%d. %s: %s abstracts (%.1f%%)\n",
                    i, method$methods_combined,
                    format(method$count, big.mark = ","),
                    round(method$percentage, 1)))
      }
      cat("\n")
    }

    cat("VISUALIZATION FEATURES:\n")
    cat("======================\n")
    cat("- World maps with Robinson projection (more accurate than Mercator)\n")
    cat("- Standard log scales (1, 10, 100, 1000) for data clarity\n")
    cat("- Geographic regions (North vs South classification)\n")
    cat("- Research method distributions (3 core + 9 expanded methods)\n")
    cat("- Plant part frequency analysis\n")
    cat("- Confidence level distributions\n")
    cat("- Dual analysis modes (main/supplementary for mycorrhizal filtering)\n\n")

    cat("TECHNICAL IMPROVEMENTS:\n")
    cat("=======================\n")
    cat("- Map projection: Changed from Mercator to Robinson for better area representation\n")
    cat("- Legend clarity: Explicitly states 'Gray = 0 studies' for data interpretation\n")
    cat("- Scale standardization: Log scale ticks use standard breaks (1, 10, 100, 1000)\n")
    cat("- Enhanced readability: Larger fonts, better legend positioning\n")
    cat("- Geographic completeness: Shows countries, continents, regions, coordinates\n\n")

    cat("KEY FINDINGS FOR MANUSCRIPT:\n")
    cat("===========================\n")
    cat(sprintf("- Geographic coverage shows %s abstracts with location data\n",
                format(abstracts_with_geography, big.mark = ",")))
    cat(sprintf("- Research methods detected in %s abstracts\n",
                format(abstracts_with_methods, big.mark = ",")))
    if (exists("geo_summary") && nrow(geo_summary) > 0) {
      cat(sprintf("- Most studied country: %s (%s abstracts)\n",
                  geo_summary$country_std[1], format(geo_summary$frequency[1], big.mark = ",")))
    }
    if (exists("methods_summary") && nrow(methods_summary) > 0) {
      cat(sprintf("- Most common method approach: %s\n", methods_summary$methods_combined[1]))
    }
    cat("- Analysis reveals global research patterns in endophyte studies\n")

  }, file = log_file)

  message("Manuscript-ready statistics saved to: ", log_file)
}

# Generate the manuscript log
create_manuscript_log_extraction()

cat("All visualization files saved to plots/main/ and plots/supplementary/ directories! 📊✨\n")
