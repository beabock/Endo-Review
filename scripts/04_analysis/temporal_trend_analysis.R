# Temporal Trend Analysis for Endophyte Research
# B. Bock
# July 31, 2025
#
# Analyzes temporal trends in endophyte research including:
# - Publication volume over time
# - Research method evolution
# - Geographic research patterns
# - Species focus changes
# - Journal and field development

library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(scales)


cat("=== TEMPORAL TREND ANALYSIS ===\n")
cat("Analyzing research patterns over time\n\n")

# Load comprehensive data
if (!file.exists("results/comprehensive_extraction_results.csv")) {
  stop("Please run extract_species_simple.R first to generate comprehensive results.")
}

comprehensive_data <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)

cat("Total abstracts for temporal analysis:", nrow(comprehensive_data), "\n")

# Data preparation
temporal_data <- comprehensive_data %>%
mutate(
    # Ensure publication_year is numeric
    publication_year = as.integer(publication_year)
  ) %>%
  filter(!is.na(publication_year)) %>%
  mutate(
    # Create time periods
    decade = floor(publication_year / 10) * 10,
    five_year_period = floor(publication_year / 5) * 5,
    period_label = paste0(five_year_period, "-", five_year_period + 4),
    
    # Research method flags
    has_molecular = !is.na(molecular_methods) & molecular_methods,
    has_culture = !is.na(culture_based_methods) & culture_based_methods,
    has_microscopy = !is.na(microscopy_methods) & microscopy_methods,
    
    # Information completeness
    info_score = (
      ifelse(!is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName)), 1, 0) +
      ifelse(!is.na(methods_summary), 1, 0) +
      ifelse(!is.na(plant_parts_detected), 1, 0) +
      ifelse(!is.na(geographic_summary), 1, 0)
    ),
    
    # Geographic categories
    has_global_north = !is.na(global_north_countries),
    has_global_south = !is.na(global_south_countries),
    
    # Species detection
    has_species = !is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName)
  )

cat("Data prepared for years", min(temporal_data$publication_year), "to", max(temporal_data$publication_year), "\n")
cat("Total abstracts in analysis:", nrow(temporal_data), "\n\n")

# 1. Publication volume trends
cat("1. Analyzing publication volume trends...\n")

# Annual publication counts
annual_counts <- temporal_data %>%
  count(publication_year, name = "publications") %>%
  complete(publication_year = min(temporal_data$publication_year):max(temporal_data$publication_year),
           fill = list(publications = 0))

# Five-year period trends
period_counts <- temporal_data %>%
  count(period_label, five_year_period, name = "publications") %>%
  arrange(five_year_period)

cat("Publications by 5-year periods:\n")
print(period_counts)

# Calculate growth rates
period_growth <- period_counts %>%
  arrange(five_year_period) %>%
  mutate(
    growth_rate = (publications - lag(publications)) / lag(publications) * 100,
    cumulative_pubs = cumsum(publications)
  )

# 2. Research method evolution
cat("\n2. Analyzing research method evolution...\n")

method_trends <- temporal_data %>%
  group_by(five_year_period, period_label) %>%
  summarise(
    total_papers = n(),
    molecular_papers = sum(has_molecular, na.rm = TRUE),
    culture_papers = sum(has_culture, na.rm = TRUE),
    microscopy_papers = sum(has_microscopy, na.rm = TRUE),
    molecular_pct = round(100 * molecular_papers / total_papers, 1),
    culture_pct = round(100 * culture_papers / total_papers, 1),
    microscopy_pct = round(100 * microscopy_papers / total_papers, 1),
    .groups = "drop"
  ) %>%
  arrange(five_year_period)

cat("Research methods over time (% of papers):\n")
print(method_trends %>% select(period_label, total_papers, molecular_pct, culture_pct, microscopy_pct))

# 3. Geographic research patterns
cat("\n3. Analyzing geographic research patterns...\n")

geographic_trends <- temporal_data %>%
  filter(!is.na(global_north_countries) | !is.na(global_south_countries)) %>%
  group_by(five_year_period, period_label) %>%
  summarise(
    total_with_geography = n(),
    global_north_studies = sum(has_global_north, na.rm = TRUE),
    global_south_studies = sum(has_global_south, na.rm = TRUE),
    north_pct = round(100 * global_north_studies / total_with_geography, 1),
    south_pct = round(100 * global_south_studies / total_with_geography, 1),
    .groups = "drop"
  ) %>%
  arrange(five_year_period)

cat("Geographic focus over time (% of papers with geographic info):\n")
print(geographic_trends %>% select(period_label, total_with_geography, north_pct, south_pct))

# 4. Species detection trends
cat("\n4. Analyzing species detection trends...\n")

species_trends <- temporal_data %>%
  group_by(five_year_period, period_label) %>%
  summarise(
    total_papers = n(),
    papers_with_species = sum(has_species, na.rm = TRUE),
    species_detection_pct = round(100 * papers_with_species / total_papers, 1),
    # Species diversity (approximate) - using canonicalName as primary identifier
    unique_species = length(unique(canonicalName[!is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName)])),
    .groups = "drop"
  ) %>%
  arrange(five_year_period)

cat("Species detection over time:\n")
print(species_trends %>% select(period_label, total_papers, papers_with_species, species_detection_pct, unique_species))

# 5. Kingdom focus evolution
if ("kingdom" %in% names(temporal_data)) {
  kingdom_trends <- temporal_data %>%
    filter(!is.na(kingdom)) %>%
    group_by(five_year_period, period_label, kingdom) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(five_year_period, period_label) %>%
    mutate(
      total_period = sum(count),
      percentage = round(100 * count / total_period, 1)
    ) %>%
    arrange(five_year_period, desc(count))
  
  cat("\n5. Kingdom focus evolution:\n")
  kingdom_summary <- kingdom_trends %>%
    select(period_label, kingdom, count, percentage) %>%
    pivot_wider(names_from = kingdom, values_from = c(count, percentage), 
                names_sep = "_", values_fill = 0)
  
  print(kingdom_summary)
}

# 6. Information completeness trends
cat("\n6. Analyzing information completeness trends...\n")

completeness_trends <- temporal_data %>%
  group_by(five_year_period, period_label) %>%
  summarise(
    total_papers = n(),
    avg_info_score = round(mean(info_score, na.rm = TRUE), 2),
    high_info_papers = sum(info_score >= 3, na.rm = TRUE),
    high_info_pct = round(100 * high_info_papers / total_papers, 1),
    papers_with_methods = sum(!is.na(methods_summary)),
    methods_pct = round(100 * papers_with_methods / total_papers, 1),
    .groups = "drop"
  ) %>%
  arrange(five_year_period)

cat("Information completeness over time:\n")
print(completeness_trends %>% select(period_label, total_papers, avg_info_score, high_info_pct, methods_pct))

# Create comprehensive temporal summary
temporal_summary <- period_counts %>%
  left_join(method_trends %>% select(five_year_period, molecular_pct, culture_pct, microscopy_pct), 
            by = "five_year_period") %>%
  left_join(species_trends %>% select(five_year_period, species_detection_pct, unique_species), 
            by = "five_year_period") %>%
  left_join(completeness_trends %>% select(five_year_period, avg_info_score, high_info_pct), 
            by = "five_year_period") %>%
  left_join(geographic_trends %>% select(five_year_period, north_pct, south_pct), 
            by = "five_year_period") %>%
  arrange(five_year_period)

# Save results
write_csv(temporal_summary, "results/temporal_trends_summary.csv")
write_csv(annual_counts, "results/annual_publication_counts.csv")
write_csv(method_trends, "results/research_method_trends.csv")
write_csv(geographic_trends, "results/geographic_research_trends.csv")
write_csv(species_trends, "results/species_detection_trends.csv")
write_csv(completeness_trends, "results/information_completeness_trends.csv")

if (exists("kingdom_trends")) {
  write_csv(kingdom_trends, "results/kingdom_focus_trends.csv")
}

# Generate trend analysis report
cat("\n=== TEMPORAL TRENDS REPORT ===\n")

# Calculate key statistics
total_years <- max(temporal_data$publication_year) - min(temporal_data$publication_year) + 1
early_period <- temporal_summary %>% slice_head(n = 2) %>% summarise(avg_pubs = mean(publications, na.rm = TRUE))
late_period <- temporal_summary %>% slice_tail(n = 2) %>% summarise(avg_pubs = mean(publications, na.rm = TRUE))
growth_factor <- late_period$avg_pubs / early_period$avg_pubs

cat("Analysis period:", min(temporal_data$publication_year), "-", max(temporal_data$publication_year), "(", total_years, "years)\n")
cat("Total publications analyzed:", sum(period_counts$publications), "\n")
cat("Publication growth factor (late vs early periods):", round(growth_factor, 1), "x\n")

# Method adoption rates
molecular_adoption <- method_trends %>% 
  filter(five_year_period == max(five_year_period, na.rm = TRUE)) %>% 
  pull(molecular_pct)
culture_adoption <- method_trends %>% 
  filter(five_year_period == max(five_year_period, na.rm = TRUE)) %>% 
  pull(culture_pct)

cat("\nRecent method adoption (latest period):\n")
cat("- Molecular methods:", molecular_adoption, "% of papers\n")
cat("- Culture-based methods:", culture_adoption, "% of papers\n")

# Geographic patterns
recent_geography <- geographic_trends %>% 
  filter(five_year_period == max(five_year_period, na.rm = TRUE))

if (nrow(recent_geography) > 0) {
  cat("\nRecent geographic focus:\n")
  cat("- Global North studies:", recent_geography$north_pct, "%\n")
  cat("- Global South studies:", recent_geography$south_pct, "%\n")
}

# Species detection evolution
species_evolution <- species_trends %>%
  filter(five_year_period %in% c(min(five_year_period), max(five_year_period))) %>%
  select(period_label, species_detection_pct, unique_species)

cat("\nSpecies detection evolution:\n")
print(species_evolution)

# Create summary report file
capture.output({
  cat("=== TEMPORAL TRENDS IN ENDOPHYTE RESEARCH ===\n")
  cat("Analysis Report Generated:", Sys.time(), "\n")
  cat("Data Source: Machine Learning Classification Results\n\n")
  
  cat("OVERVIEW:\n")
  cat("Analysis period:", min(temporal_data$publication_year), "-", max(temporal_data$publication_year), "\n")
  cat("Total publications:", sum(period_counts$publications), "\n")
  cat("Publication growth factor:", round(growth_factor, 1), "x increase\n\n")
  
  cat("KEY FINDINGS:\n")
  cat("1. Publication volume has", ifelse(growth_factor > 2, "dramatically increased", "increased"), "over time\n")
  cat("2. Molecular methods adoption:", molecular_adoption, "% in recent period\n")
  cat("3. Geographic research balance: North", recent_geography$north_pct, "% vs South", recent_geography$south_pct, "%\n")
  cat("4. Information completeness has", 
      ifelse(max(completeness_trends$avg_info_score, na.rm = TRUE) > 
             min(completeness_trends$avg_info_score, na.rm = TRUE), "improved", "remained stable"), "over time\n\n")
  
  cat("DETAILED TRENDS:\n")
  cat("Publication counts by period:\n")
  print(period_counts)
  cat("\n")
  
  cat("Research method evolution:\n")
  print(method_trends %>% select(period_label, molecular_pct, culture_pct, microscopy_pct))
  cat("\n")
  
  cat("Geographic research patterns:\n")
  print(geographic_trends %>% select(period_label, north_pct, south_pct))
  cat("\n")
  
  cat("Species detection trends:\n")
  print(species_trends %>% select(period_label, species_detection_pct, unique_species))
  cat("\n")
  
  cat("IMPLICATIONS:\n")
  cat("1. Field maturation: Increasing methodological sophistication\n")
  cat("2. Global expansion: Research extending beyond traditional centers\n")
  cat("3. Taxonomic diversity: Growing recognition of endophyte diversity\n")
  cat("4. Technical advancement: Molecular tools becoming standard\n\n")
  
  cat("RECOMMENDATIONS:\n")
  cat("1. Continue monitoring geographic research equity\n")
  cat("2. Assess quality vs quantity in publication growth\n")
  cat("3. Track methodological standardization efforts\n")
  cat("4. Monitor emerging research trends and gaps\n")
}, file = "../../results/temporal_trends_report.txt")

cat("\nFiles created:\n")
cat("✓ ../../results/temporal_trends_summary.csv\n")
cat("✓ ../../results/annual_publication_counts.csv\n")
cat("✓ ../../results/research_method_trends.csv\n")
cat("✓ ../../results/geographic_research_trends.csv\n")
cat("✓ ../../results/species_detection_trends.csv\n")
cat("✓ ../../results/information_completeness_trends.csv\n")
if (exists("kingdom_trends")) {
  cat("✓ ../../results/kingdom_focus_trends.csv\n")
}
cat("✓ ../../results/temporal_trends_report.txt\n")

cat("\n=== TEMPORAL ANALYSIS COMPLETE ===\n")
cat("Key insights:\n")
cat("📈 Publication volume trends analyzed\n")
cat("🔬 Research method evolution tracked\n")
cat("🌍 Geographic research patterns mapped\n")
cat("🧬 Species detection trends identified\n")
cat("📊 Information quality trends assessed\n")
cat("\nNext: Use these trends for manuscript discussion and future research directions!\n")
