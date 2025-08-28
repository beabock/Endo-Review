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

# Load centralized reference utilities (normalization helpers)
utils_path <- "scripts/04_analysis/reference_data_utils.R"
if (file.exists(utils_path)) {
  source(utils_path)
} else {
  warning("reference_data_utils.R not found; country/plant-part normalization helpers unavailable")
}

# Need to adjust abstract counts throughout to group by abstract.id


# Custom theme for consistent visualization
custom_theme <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Custom color palette for endophyte research
endo_palette <- c(
  "#2E8B57",  # Sea green - for plants
  "#8B4513",  # Saddle brown - for fungi
  "#4682B4",  # Steel blue - for molecular methods
  "#CD853F",  # Peru - for culture methods
  "#9370DB",  # Medium purple - for microscopy
  "#DC143C",  # Crimson - for presence
  "#228B22",  # Forest green - for absence
  "#FF8C00",  # Dark orange - for uncertain
  "#708090"   # Slate gray - for other
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
results <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)

cat("Loaded", nrow(results), "abstracts with comprehensive extraction data\n")

# Data preparation and summary statistics
results_clean <- results %>%
  mutate(
    # Clean prediction labels
    predicted_label = case_when(
      is.na(predicted_label) ~ "Unknown",
      predicted_label == "Presence" ~ "Presence",
      predicted_label == "Absence" ~ "Absence",
      TRUE ~ as.character(predicted_label)
    ),
    
    # Create method categories
    has_molecular = coalesce(molecular_methods, FALSE),
    has_culture = coalesce(culture_based_methods, FALSE),
    has_microscopy = coalesce(microscopy_methods, FALSE),
    
    # Create combined method category
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
    
    # Species detection indicators
    has_species = !is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName),
    has_plant_parts = !is.na(plant_parts_detected),
    has_geography = !is.na(geographic_summary),
    
    # Confidence categories
    confidence_cat = case_when(
      is.na(confidence) ~ "Unknown",
      confidence >= 0.9 ~ "Very High (≥90%)",
      confidence >= 0.8 ~ "High (80-90%)",
      confidence >= 0.7 ~ "Medium (70-80%)",
      confidence >= 0.6 ~ "Low (60-70%)",
      TRUE ~ "Very Low (<60%)"
    ),
    
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
  scale_fill_manual(values = c("With Species" = endo_palette[1], "Without Species" = "lightgray")) +
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
save_plot(p1_species_detection, "figures/species_detection_rate.png", height = 6)

# 2. RESEARCH METHODS ANALYSIS -------------------------------------------

cat("Creating research methods analysis plots...\n")

# Methods distribution  
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
  scale_fill_manual(values = endo_palette[3:5]) +
  labs(
    title = "Research Methods Detection",
    subtitle = "Individual method frequencies across all abstracts",
    x = "Research Method",
    y = "Number of Abstracts",
    caption = paste("Total abstracts analyzed:", nrow(methods_abstract_summary))
  ) +
  custom_theme +
  theme(legend.position = "none")

# Combined methods analysis
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
    title = "Combined Research Methods",
    subtitle = "Method combinations used in endophyte studies",
    x = "Method Combination",
    y = "Number of Abstracts"
  ) +
  coord_flip() +
  custom_theme +
  theme(legend.position = "none")

# Save methods plots individually
save_plot(p3_methods_individual, "figures/methods_individual.png", height = 6)
save_plot(p4_methods_combined, "figures/methods_combined.png", height = 6)

# 3. PLANT PARTS ANALYSIS ------------------------------------------------

cat("Creating plant parts analysis plots...\n")

if (sum(results_clean$has_plant_parts) > 0) {
  
  # Extract and count plant parts - preserve all unique information per abstract
  plant_parts_data <- results_clean %>%
    filter(has_plant_parts) %>%
    select(id, plant_parts_detected) %>%
    # First get all unique plant parts per abstract (avoiding double counting within abstracts)
    separate_rows(plant_parts_detected, sep = "; ") %>%
    filter(!is.na(plant_parts_detected), plant_parts_detected != "") %>%
    # Normalize plant part terms (singular/plural grouping)
    mutate(plant_part_norm = if (exists("normalize_plant_part")) normalize_plant_part(plant_parts_detected) else tolower(plant_parts_detected)) %>%
    # Get unique combinations per abstract to avoid losing information
    distinct(id, plant_part_norm) %>%
    # Count how many unique abstracts mention each plant part
    count(plant_part_norm, name = "unique_abstracts") %>%
    arrange(desc(unique_abstracts)) %>%
    slice_max(unique_abstracts, n = 20) %>%  # Top 20 most frequent
    mutate(plant_part_norm = fct_reorder(plant_part_norm, unique_abstracts),
           plant_parts_detected = stringr::str_to_title(plant_part_norm))
  
  p6_plant_parts_freq <- plant_parts_data %>%
    ggplot(aes(x = plant_parts_detected, y = unique_abstracts)) +
    geom_col(fill = endo_palette[1], width = 0.8) +
    geom_text(aes(label = unique_abstracts), hjust = -0.1, size = 3) +
    labs(
      title = "Most Frequently Studied Plant Parts",
      subtitle = paste("Top 20 plant parts from", sum(abstract_summary$has_plant_parts), "unique abstracts"),
      x = "Plant Part",
      y = "Number of Unique Abstracts"
    ) +
    coord_flip() +
    custom_theme
  
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
  save_plot(p6_plant_parts_freq, "figures/plant_parts_frequency.png", height = 8)
  save_plot(plant_parts_by_pred, "figures/plant_parts_by_prediction.png", height = 6)
  
} else {
  cat("No plant parts data available for visualization.\n")
}

# 4. GEOGRAPHIC ANALYSIS -------------------------------------------------

cat("Creating geographic distribution plots...\n")

if (sum(results_clean$has_geography) > 0) {
  
  # Geographic regions distribution
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
  
  # Extract country data if available
  countries_data <- results_clean %>%
    filter(!is.na(countries_detected)) %>%
    select(countries_detected) %>%
    separate_rows(countries_detected, sep = "; ") %>%
  filter(!is.na(countries_detected), countries_detected != "") %>%
  # Standardize country names and group synonyms
  mutate(country_std = if (exists("normalize_country_vector")) normalize_country_vector(countries_detected) else stringr::str_to_title(countries_detected)) %>%
  count(country_std, name = "frequency") %>%
  arrange(desc(frequency)) %>%
  slice_max(frequency, n = 15) %>%
  mutate(countries_detected = fct_reorder(stringr::str_to_title(country_std), frequency))
  
  if (nrow(countries_data) > 0) {
    p8_countries <- countries_data %>%
      ggplot(aes(x = countries_detected, y = frequency)) +
      geom_col(fill = endo_palette[4], width = 0.8) +
      geom_text(aes(label = frequency), hjust = -0.1, size = 3) +
      labs(
        title = "Most Studied Countries",
        subtitle = paste("Top 15 countries from", sum(!is.na(results_clean$countries_detected)), "abstracts"),
        x = "Country",
        y = "Number of Studies"
      ) +
      coord_flip() +
      custom_theme
    
    # Save geography plots individually
    save_plot(p7_geo_regions, "figures/geographic_regions.png", height = 6)
    save_plot(p8_countries, "figures/top_countries.png", height = 8)
  } else {
    # Save only regions plot if no country data
    save_plot(p7_geo_regions, "figures/geographic_regions.png", height = 6)
  }
  
} else {
  cat("No geographic data available for visualization.\n")
}

# 5. PREDICTION QUALITY ANALYSIS -----------------------------------------

cat("Creating prediction quality analysis plots...\n")

# Confidence distribution
p9_confidence <- results_clean %>%
  filter(!is.na(confidence)) %>%
  ggplot(aes(x = confidence, fill = predicted_label)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  facet_wrap(~predicted_label, scales = "free_y") +
  scale_fill_manual(values = c("Presence" = endo_palette[6], "Absence" = endo_palette[7])) +
  labs(
    title = "Prediction Confidence Distribution",
    subtitle = "Model confidence scores by prediction type",
    x = "Confidence Score",
    y = "Number of Abstracts",
    fill = "Prediction"
  ) +
  custom_theme +
  theme(legend.position = "none")

# Information completeness by confidence
completeness_data <- results_clean %>%
  filter(!is.na(confidence)) %>%
  mutate(
    completeness_score = (as.numeric(has_species) + as.numeric(has_plant_parts) + 
                         as.numeric(has_geography) + as.numeric(methods_combined != "No methods detected")) / 4
  ) %>%
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
  scale_fill_manual(values = c("Presence" = endo_palette[6], "Absence" = endo_palette[7])) +
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
save_plot(p9_confidence, "figures/confidence_distribution.png", height = 6)
save_plot(p10_completeness, "figures/completeness_by_confidence.png", height = 6)
save_plot(p11_quality_heatmap, "figures/quality_heatmap.png", height = 6)

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
  scale_fill_manual(values = endo_palette[1:4]) +
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
save_plot(p_summary_text, "figures/summary_text.png", width = 8, height = 8)
save_plot(p_info_summary, "figures/info_summary.png", width = 8, height = 8)

# 7. SAVE VISUALIZATION SUMMARY ------------------------------------------

cat("\nCreating visualization summary report...\n")

capture.output({
  cat("=== EXTRACTION RESULTS VISUALIZATION SUMMARY ===\n")
  cat("Generated:", Sys.time(), "\n")
  cat("Source: comprehensive_extraction_results.csv\n\n")
  
  cat("FILES GENERATED:\n")
  cat("1. species_detection_rate.png - Overall species detection rate\n")
  cat("2. methods_individual.png - Individual method frequencies\n")
  cat("3. methods_combined.png - Combined method frequencies\n")
  if (sum(results_clean$has_plant_parts) > 0) {
    cat("4. plant_parts_frequency.png - Most studied plant parts\n")
    cat("5. plant_parts_by_prediction.png - Plant parts detection by prediction type\n")
  }
  if (sum(results_clean$has_geography) > 0) {
    cat("6. geographic_regions.png - Geographic distribution by region\n")
    cat("7. top_countries.png - Most studied countries (if available)\n")
  }
  cat("8. confidence_distribution.png - Confidence distributions by prediction\n")
  cat("9. completeness_by_confidence.png - Information completeness by confidence\n")
  cat("10. quality_heatmap.png - Information detection rates heatmap\n")
  cat("11. summary_text.png - Overall summary statistics\n")
  cat("12. info_summary.png - Information detection overview\n\n")
  
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
cat("Generated", length(list.files("figures", pattern = "*.png")), "visualization files in figures/\n")
cat("\nKey outputs:\n")
cat("📊 Species detection patterns and kingdom distribution (individual plots)\n")
cat("🔬 Research methods usage and combinations (individual plots)\n")
if (sum(results_clean$has_plant_parts) > 0) cat("🌱 Plant parts frequency and preferences (individual plots)\n")
if (sum(results_clean$has_geography) > 0) cat("🌍 Geographic distribution of studies (individual plots)\n")
cat("📈 Prediction quality and information completeness (individual plots)\n")
cat("📋 Summary statistics and information overview (individual plots)\n\n")

cat("Next steps:\n")
cat("1. Review generated individual plots for patterns and insights\n")
cat("2. Use findings to guide systematic review priorities\n")
cat("3. Consider combining plots manually in external software if needed\n")
cat("4. Integrate visualizations into research presentations and publications\n\n")

cat("All individual visualization files saved to figures/ directory! 📊✨\n")
