# Geographic Bias and Research Gap Analysis
# B. Bock
# July 31, 2025
#
# Analyzes geographic patterns in endophyte research to identify:
# - Research concentration vs biodiversity hotspots
# - Global North/South research equity
# - Understudied regions and ecosystems
# - Research accessibility and capacity gaps

library(tidyverse)
library(maps)
library(ggplot2)
library(viridis)
library(scales)

# Load centralized reference data utilities
source("scripts/04_analysis/reference_data_utils.R")

cat("=== GEOGRAPHIC BIAS ANALYSIS ===\n")
cat("Analyzing research patterns and equity gaps\n\n")

# Load comprehensive data
if (!file.exists("results/comprehensive_extraction_results.csv")) {
  stop("Please run extract_species_simple.R first to generate comprehensive results.")
}

comprehensive_data <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)

# Filter for abstracts with geographic information
geographic_data <- comprehensive_data %>%
  filter(!is.na(countries_detected) | !is.na(continents_detected) | !is.na(regions_detected)) %>%
  mutate(
    # Create research quality indicators
    has_species = !is.na(canonicalName) | !is.na(resolved_name) | !is.na(acceptedScientificName),
    has_molecular = !is.na(molecular_methods) & molecular_methods,
    has_culture = !is.na(culture_based_methods) & culture_based_methods,
    has_microscopy = !is.na(microscopy_methods) & microscopy_methods,
    
    # Research sophistication score
    method_score = (
      ifelse(has_molecular, 2, 0) +  # Molecular methods weighted higher
      ifelse(has_culture, 1, 0) +
      ifelse(has_microscopy, 1, 0)
    ),
    
    # Information completeness
    info_score = (
      ifelse(has_species, 1, 0) +
      ifelse(!is.na(methods_summary), 1, 0) +
      ifelse(!is.na(plant_parts_detected), 1, 0) +
      ifelse(!is.na(geographic_summary), 1, 0)
    ),
    
    # Development status
    has_global_north = !is.na(global_north_countries),
    has_global_south = !is.na(global_south_countries),
    development_focus = case_when(
      has_global_north & has_global_south ~ "Mixed",
      has_global_north ~ "Global North",
      has_global_south ~ "Global South",
      TRUE ~ "Unspecified"
    )
  )

# Count unique abstracts with geographic information (consolidating info across rows)
unique_geographic <- geographic_data %>%
  group_by(id) %>%
  summarise(
    # Consolidate all the important information across rows for each abstract
    development_focus = first(development_focus[!is.na(development_focus)]),
    has_species = any(has_species, na.rm = TRUE),
    has_molecular = any(has_molecular, na.rm = TRUE),
    has_culture = any(has_culture, na.rm = TRUE),
    has_microscopy = any(has_microscopy, na.rm = TRUE),
    method_score = max(method_score, na.rm = TRUE),
    info_score = max(info_score, na.rm = TRUE),
    has_global_north = any(has_global_north, na.rm = TRUE),
    has_global_south = any(has_global_south, na.rm = TRUE),
    countries_detected = paste(unique(countries_detected[!is.na(countries_detected)]), collapse = "; "),
    continents_detected = paste(unique(continents_detected[!is.na(continents_detected)]), collapse = "; "),
    regions_detected = paste(unique(regions_detected[!is.na(regions_detected)]), collapse = "; "),
    publication_year = first(publication_year[!is.na(publication_year)]),
    .groups = "drop"
  ) %>%
  # Clean up empty consolidations
  mutate(
    countries_detected = ifelse(countries_detected == "", NA_character_, countries_detected),
    continents_detected = ifelse(continents_detected == "", NA_character_, continents_detected),
    regions_detected = ifelse(regions_detected == "", NA_character_, regions_detected),
    method_score = ifelse(is.infinite(method_score), 0, method_score),
    info_score = ifelse(is.infinite(info_score), 0, info_score)
  )

cat("Total abstracts with geographic information:", nrow(unique_geographic), "\n")

# 1. Global North vs South research equity (accounting for zero-study countries)
cat("\n1. Analyzing Global North/South research equity...\n")

# Analysis of abstracts that could be classified (have geographic info)
development_analysis <- unique_geographic %>%
  filter(development_focus %in% c("Global North", "Global South")) %>%
  group_by(development_focus) %>%
  summarise(
    total_studies = n(),
    studies_with_species = sum(has_species),
    avg_method_score = round(mean(method_score, na.rm = TRUE), 2),
    avg_info_score = round(mean(info_score, na.rm = TRUE), 2),
    molecular_studies = sum(has_molecular, na.rm = TRUE),
    molecular_pct = round(100 * molecular_studies / total_studies, 1),
    high_quality_studies = sum(info_score >= 3, na.rm = TRUE),
    high_quality_pct = round(100 * high_quality_studies / total_studies, 1),
    .groups = "drop"
  )

cat("Research equity analysis (studies with geographic classification):\n")
print(development_analysis)

# Calculate equity ratios for classified studies
north_data <- development_analysis %>% filter(development_focus == "Global North")
south_data <- development_analysis %>% filter(development_focus == "Global South")

if (nrow(north_data) > 0 && nrow(south_data) > 0) {
  equity_ratios <- tibble(
    metric = c("Total Studies", "Molecular Methods", "High Quality", "Method Score", "Info Score"),
    north_value = c(north_data$total_studies, north_data$molecular_pct, 
                   north_data$high_quality_pct, north_data$avg_method_score, north_data$avg_info_score),
    south_value = c(south_data$total_studies, south_data$molecular_pct, 
                   south_data$high_quality_pct, south_data$avg_method_score, south_data$avg_info_score),
    north_south_ratio = round(north_value / south_value, 2),
    equity_assessment = case_when(
      north_south_ratio > 2 ~ "Major North bias",
      north_south_ratio > 1.5 ~ "Moderate North bias", 
      north_south_ratio > 1.1 ~ "Slight North bias",
      north_south_ratio < 0.9 ~ "South advantage",
      TRUE ~ "Roughly equitable"
    )
  )
  
  cat("\nEquity ratios (North/South) for classified studies:\n")
  print(equity_ratios)
}

cat("\nNOTE: Comprehensive country-level analysis (including zero-study countries) follows below.\n")

# 2. Country-specific analysis (including countries with 0 studies)
cat("\n2. Analyzing country-specific research patterns (including zero-study countries)...\n")

# Get complete country classification from centralized utility
all_countries <- get_country_classifications()

# Extract individual countries and count studies (consolidating info across rows)
countries_with_studies <- geographic_data %>%
  filter(!is.na(countries_detected)) %>%
  # First consolidate information per abstract
  group_by(id) %>%
  summarise(
    countries_detected = paste(unique(countries_detected[!is.na(countries_detected)]), collapse = "; "),
    development_focus = first(development_focus[!is.na(development_focus)]),
    has_species = any(has_species, na.rm = TRUE),
    has_molecular = any(has_molecular, na.rm = TRUE),
    method_score = max(method_score, na.rm = TRUE),
    info_score = max(info_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Clean up and separate countries
  mutate(
    method_score = ifelse(is.infinite(method_score), 0, method_score),
    info_score = ifelse(is.infinite(info_score), 0, info_score)
  ) %>%
  separate_rows(countries_detected, sep = "; ") %>%
  mutate(country_clean = str_trim(countries_detected)) %>%
  filter(country_clean != "") %>%
  # Standardize country names using centralized utility
  mutate(country_clean = sapply(country_clean, standardize_country_name)) %>%
  group_by(country_clean) %>%
  summarise(
    total_studies = n(),
    studies_with_species = sum(has_species),
    molecular_studies = sum(has_molecular, na.rm = TRUE),
    molecular_pct = round(100 * molecular_studies / total_studies, 1),
    avg_method_score = round(mean(method_score, na.rm = TRUE), 2),
    avg_info_score = round(mean(info_score, na.rm = TRUE), 2),
    high_quality_studies = sum(info_score >= 3, na.rm = TRUE),
    .groups = "drop"
  )

# Comprehensive country analysis including zero-study countries
country_studies_complete <- all_countries %>%
  left_join(countries_with_studies, by = c("country" = "country_clean")) %>%
  mutate(
    total_studies = replace_na(total_studies, 0),
    studies_with_species = replace_na(studies_with_species, 0),
    molecular_studies = replace_na(molecular_studies, 0),
    high_quality_studies = replace_na(high_quality_studies, 0),
    molecular_pct = ifelse(total_studies == 0, 0, molecular_pct),
    avg_method_score = replace_na(avg_method_score, 0),
    avg_info_score = replace_na(avg_info_score, 0)
  ) %>%
  arrange(desc(total_studies))

cat("Top 15 most studied countries:\n")
print(country_studies_complete %>% filter(total_studies > 0) %>% head(15))

# Zero-study countries analysis
zero_study_countries <- country_studies_complete %>%
  filter(total_studies == 0) %>%
  count(development_focus, name = "countries_with_zero_studies")

cat("\nCountries with ZERO endophyte research studies:\n")
print(zero_study_countries)

# Comprehensive Global North vs South analysis
comprehensive_equity <- country_studies_complete %>%
  group_by(development_focus) %>%
  summarise(
    total_countries = n(),
    countries_with_studies = sum(total_studies > 0),
    countries_with_zero_studies = sum(total_studies == 0),
    pct_countries_with_studies = round(100 * countries_with_studies / total_countries, 1),
    total_research_volume = sum(total_studies),
    avg_studies_per_country = round(mean(total_studies), 2),
    median_studies_per_country = median(total_studies),
    .groups = "drop"
  )

cat("\nComprehensive Global North vs South equity analysis:\n")
print(comprehensive_equity)

# Calculate research coverage ratios
north_coverage <- comprehensive_equity %>% filter(development_focus == "Global North")
south_coverage <- comprehensive_equity %>% filter(development_focus == "Global South")

if (nrow(north_coverage) > 0 && nrow(south_coverage) > 0) {
  coverage_ratios <- tibble(
    metric = c("% Countries with Studies", "Avg Studies per Country", "Total Research Volume"),
    north_value = c(north_coverage$pct_countries_with_studies, 
                   north_coverage$avg_studies_per_country, 
                   north_coverage$total_research_volume),
    south_value = c(south_coverage$pct_countries_with_studies, 
                   south_coverage$avg_studies_per_country, 
                   south_coverage$total_research_volume),
    north_south_ratio = round(north_value / south_value, 2),
    equity_assessment = case_when(
      north_south_ratio > 3 ~ "Severe North bias",
      north_south_ratio > 2 ~ "Major North bias",
      north_south_ratio > 1.5 ~ "Moderate North bias", 
      north_south_ratio > 1.1 ~ "Slight North bias",
      north_south_ratio < 0.9 ~ "South advantage",
      TRUE ~ "Roughly equitable"
    )
  )
  
  cat("\nResearch coverage ratios (North/South) - accounting for ALL countries:\n")
  print(coverage_ratios)
}

# Identify understudied countries with biodiversity potential
understudied_high_biodiversity <- country_studies_complete %>%
  filter(
    development_focus == "Global South",
    total_studies <= 2,  # Very few or no studies
    country %in% get_biodiversity_hotspots()  # High biodiversity countries
  ) %>%
  arrange(total_studies, country)

cat("\nUnderstudied high-biodiversity countries (≤2 studies):\n")
print(understudied_high_biodiversity %>% select(country, development_focus, total_studies))

# Identify understudied countries more broadly (for general reporting)
understudied_countries <- country_studies_complete %>%
  filter(
    development_focus == "Global South",
    total_studies < 5  # Countries with very few studies
  ) %>%
  arrange(total_studies, country)

cat("\nUnderstudied Global South countries (<5 studies):\n")
cat("Found", nrow(understudied_countries), "countries with fewer than 5 studies\n")

# 3. Continental analysis
cat("\n3. Analyzing continental research patterns...\n")

continental_studies <- geographic_data %>%
  filter(!is.na(continents_detected)) %>%
  # First consolidate information per abstract
  group_by(id) %>%
  summarise(
    continents_detected = paste(unique(continents_detected[!is.na(continents_detected)]), collapse = "; "),
    has_species = any(has_species, na.rm = TRUE),
    has_molecular = any(has_molecular, na.rm = TRUE),
    method_score = max(method_score, na.rm = TRUE),
    info_score = max(info_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Clean up and separate continents
  mutate(
    method_score = ifelse(is.infinite(method_score), 0, method_score),
    info_score = ifelse(is.infinite(info_score), 0, info_score)
  ) %>%
  separate_rows(continents_detected, sep = "; ") %>%
  mutate(continent_clean = str_trim(continents_detected)) %>%
  filter(continent_clean != "") %>%
  group_by(continent_clean) %>%
  summarise(
    total_studies = n(),
    studies_with_species = sum(has_species),
    species_pct = round(100 * studies_with_species / total_studies, 1),
    molecular_studies = sum(has_molecular, na.rm = TRUE),
    molecular_pct = round(100 * molecular_studies / total_studies, 1),
    avg_method_score = round(mean(method_score, na.rm = TRUE), 2),
    high_quality_studies = sum(info_score >= 3, na.rm = TRUE),
    high_quality_pct = round(100 * high_quality_studies / total_studies, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(total_studies))

cat("Research by continent:\n")
print(continental_studies)

# 4. Ecosystem and habitat analysis
cat("\n4. Analyzing ecosystem research coverage...\n")

ecosystem_studies <- geographic_data %>%
  filter(!is.na(regions_detected)) %>%
  # First consolidate information per abstract
  group_by(id) %>%
  summarise(
    regions_detected = paste(unique(regions_detected[!is.na(regions_detected)]), collapse = "; "),
    has_species = any(has_species, na.rm = TRUE),
    has_molecular = any(has_molecular, na.rm = TRUE),
    info_score = max(info_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Clean up and separate ecosystems
  mutate(
    info_score = ifelse(is.infinite(info_score), 0, info_score)
  ) %>%
  separate_rows(regions_detected, sep = "; ") %>%
  mutate(ecosystem_clean = str_trim(regions_detected)) %>%
  filter(ecosystem_clean != "") %>%
  group_by(ecosystem_clean) %>%
  summarise(
    total_studies = n(),
    studies_with_species = sum(has_species),
    molecular_studies = sum(has_molecular, na.rm = TRUE),
    molecular_pct = round(100 * molecular_studies / total_studies, 1),
    avg_info_score = round(mean(info_score, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(total_studies))

cat("Research by ecosystem/habitat (top 15):\n")
print(ecosystem_studies %>% head(15))

# Identify understudied ecosystems
understudied_ecosystems <- ecosystem_studies %>%
  filter(total_studies < 3) %>%
  arrange(desc(total_studies))

cat("\nUnderstudied ecosystems (< 3 studies):\n")
print(understudied_ecosystems)

# 5. Research capacity and accessibility analysis
cat("\n5. Analyzing research capacity indicators...\n")

# Compare method sophistication by region
capacity_analysis <- geographic_data %>%
  filter(development_focus %in% c("Global North", "Global South")) %>%
  group_by(development_focus) %>%
  summarise(
    # Method access
    molecular_access = round(100 * sum(has_molecular, na.rm = TRUE) / n(), 1),
    culture_access = round(100 * sum(has_culture, na.rm = TRUE) / n(), 1),
    microscopy_access = round(100 * sum(has_microscopy, na.rm = TRUE) / n(), 1),
    
    # Publication characteristics. commenting out bc currently not in data
    avg_publication_year = round(mean(publication_year, na.rm = TRUE), 1),
    recent_studies = sum(publication_year >= 2015, na.rm = TRUE),
    recent_pct = round(100 * recent_studies / n(), 1),
    
    # Study quality
    comprehensive_studies = sum(info_score >= 3, na.rm = TRUE),
    comprehensive_pct = round(100 * comprehensive_studies / n(), 1),
    
    .groups = "drop"
  )

cat("Research capacity by development status:\n")
print(capacity_analysis)

# 6. Biodiversity hotspot coverage
cat("\n6. Analyzing biodiversity hotspot research coverage...\n")

# Get biodiversity hotspots from centralized utility
biodiversity_hotspots <- get_biodiversity_hotspots()

hotspot_coverage <- geographic_data %>%
  filter(!is.na(countries_detected)) %>%
  separate_rows(countries_detected, sep = "; ") %>%
  mutate(
    country_clean = str_trim(tolower(countries_detected)),
    is_hotspot = country_clean %in% biodiversity_hotspots
  ) %>%
  group_by(is_hotspot) %>%
  summarise(
    total_studies = n(),
    molecular_studies = sum(has_molecular, na.rm = TRUE),
    molecular_pct = round(100 * molecular_studies / total_studies, 1),
    high_quality_studies = sum(info_score >= 3, na.rm = TRUE),
    high_quality_pct = round(100 * high_quality_studies / total_studies, 1),
    .groups = "drop"
  ) %>%
  mutate(
    hotspot_status = ifelse(is_hotspot, "Biodiversity Hotspot", "Other Region")
  )

cat("Research in biodiversity hotspots vs other regions:\n")
print(hotspot_coverage %>% select(-is_hotspot))

# Calculate research priorities
research_gaps <- tibble(
  gap_category = c(
    "Understudied Global South Countries",
    "Low-Capacity Regions", 
    "Biodiversity Hotspots",
    "Underrepresented Ecosystems",
    "Methodological Equity"
  ),
  priority_level = c("High", "High", "Medium", "Medium", "High"),
  description = c(
    paste("Countries with <5 studies:", nrow(understudied_countries)),
    "Global South regions with limited method access",
    "High biodiversity regions with low study density", 
    paste("Ecosystems with <3 studies:", nrow(understudied_ecosystems)),
    "Molecular method access disparity between North/South"
  )
)

# Save all results
write_csv(development_analysis, "results/geographic_equity_analysis.csv")
write_csv(country_studies_complete, "results/country_research_patterns.csv")
write_csv(continental_studies, "results/continental_research_analysis.csv")
write_csv(ecosystem_studies, "results/ecosystem_research_coverage.csv")
write_csv(capacity_analysis, "results/research_capacity_analysis.csv")
write_csv(hotspot_coverage, "results/biodiversity_hotspot_coverage.csv")
write_csv(research_gaps, "results/research_gap_priorities.csv")

if (exists("equity_ratios")) {
  write_csv(equity_ratios, "results/north_south_equity_ratios.csv")
}

# === Plot: Global Research Intensity by Country (grouped by abstract id) ===
cat("\nGenerating global research intensity heat map by country (grouped by abstract id)...\n")

# Prepare country-level data for mapping, using the complete country dataset
country_map_data <- country_studies_complete %>%
  filter(total_studies > 0) %>%
  select(country, total_studies, development_focus) %>%
  mutate(country_clean = tolower(country))

# Get world map data
world_map <- map_data("world") %>%
  mutate(region = tolower(region))

# Merge map data with research intensity
map_plot_data <- world_map %>%
  left_join(country_map_data, by = c("region" = "country_clean"))

# Plot heat map
heatmap_plot <- ggplot(map_plot_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = total_studies), color = "gray70", size = 0.1) +
  scale_fill_viridis(option = "C", na.value = "white", direction = -1, name = "Research Intensity (Unique Abstracts)") +
  theme_minimal() +
  labs(
    title = "Global Intensity of Endophyte Research by Country",
    subtitle = "Number of unique abstracts per country",
    caption = "Data: Comprehensive Extraction Results"
  ) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# Save plot
heatmap_file <- "plots/global_research_intensity_heatmap.png"
ggsave(heatmap_file, heatmap_plot, width = 12, height = 6, dpi = 300)
cat("Heat map saved to:", heatmap_file, "\n")

# Generate comprehensive geographic analysis report
capture.output({
  cat("=== GEOGRAPHIC BIAS AND RESEARCH GAP ANALYSIS REPORT ===\n")
  cat("Generated:", Sys.time(), "\n")
  cat("Data Source: Comprehensive Extraction Results\n\n")
  
  cat("EXECUTIVE SUMMARY:\n")
  cat("Total studies with geographic information:", nrow(geographic_data), "\n")
  if (exists("equity_ratios")) {
    major_biases <- equity_ratios %>% filter(str_detect(equity_assessment, "Major|Moderate"))
    cat("Major geographic biases identified:", nrow(major_biases), "\n")
  }
  cat("Understudied Global South countries:", nrow(understudied_countries), "\n")
  cat("Understudied ecosystems:", nrow(understudied_ecosystems), "\n\n")
  
  cat("KEY FINDINGS:\n\n")
  
  cat("1. GLOBAL NORTH/SOUTH EQUITY:\n")
  print(development_analysis)
  cat("\n")
  
  if (exists("equity_ratios")) {
    cat("Equity Assessment:\n")
    print(equity_ratios %>% select(metric, north_south_ratio, equity_assessment))
    cat("\n")
  }
  
  cat("2. COUNTRY-LEVEL PATTERNS:\n")
  cat("Most studied countries:\n")
  print(country_studies_complete %>% filter(total_studies > 0) %>% head(10) %>% select(country, total_studies, molecular_pct, development_focus))
  cat("\n")
  
  cat("Understudied high-biodiversity countries:\n")
  print(understudied_countries %>% head(10) %>% select(country, total_studies, molecular_pct))
  cat("\n")
  
  cat("3. CONTINENTAL DISTRIBUTION:\n")
  print(continental_studies %>% select(continent_clean, total_studies, molecular_pct, high_quality_pct))
  cat("\n")
  
  cat("4. ECOSYSTEM COVERAGE:\n")
  cat("Well-studied ecosystems:\n")
  print(ecosystem_studies %>% head(10) %>% select(ecosystem_clean, total_studies, molecular_pct))
  cat("\n")
  
  cat("Understudied ecosystems:\n")
  print(understudied_ecosystems %>% select(ecosystem_clean, total_studies))
  cat("\n")
  
  cat("5. RESEARCH CAPACITY:\n")
  print(capacity_analysis)
  cat("\n")
  
  cat("6. BIODIVERSITY HOTSPOT COVERAGE:\n")
  print(hotspot_coverage %>% select(hotspot_status, total_studies, molecular_pct, high_quality_pct))
  cat("\n")
  
  cat("RESEARCH GAPS AND PRIORITIES:\n")
  print(research_gaps)
  cat("\n")
  
  cat("RECOMMENDATIONS:\n")
  cat("1. CAPACITY BUILDING:\n")
  cat("   - Support molecular method access in Global South\n")
  cat("   - Develop regional research networks\n")
  cat("   - Provide technical training and equipment\n\n")
  
  cat("2. GEOGRAPHIC PRIORITIES:\n")
  cat("   - Focus on understudied biodiversity hotspots\n")
  cat("   - Expand research in Africa, Asia, South America\n")
  cat("   - Include indigenous and local knowledge systems\n\n")
  
  cat("3. ECOSYSTEM PRIORITIES:\n")
  cat("   - Study understudied habitat types\n")
  cat("   - Focus on threatened ecosystems\n")
  cat("   - Include climate change vulnerable regions\n\n")
  
  cat("4. METHODOLOGICAL EQUITY:\n")
  cat("   - Ensure molecular method accessibility\n")
  cat("   - Support publication in international journals\n")
  cat("   - Promote collaborative research partnerships\n\n")
  
  cat("5. FUTURE RESEARCH DIRECTIONS:\n")
  cat("   - Systematic sampling of understudied regions\n")
  cat("   - Long-term monitoring in biodiversity hotspots\n")
  cat("   - Integration with conservation priorities\n")
  cat("   - Climate change impact assessments\n")
  
}, file = "results/geographic_bias_analysis_report.txt")

cat("\nFiles created:\n")
cat("✓ results/geographic_equity_analysis.csv\n")
cat("✓ results/country_research_patterns.csv\n")
cat("✓ results/continental_research_analysis.csv\n")
cat("✓ results/ecosystem_research_coverage.csv\n")
cat("✓ results/research_capacity_analysis.csv\n")
cat("✓ results/biodiversity_hotspot_coverage.csv\n")
cat("✓ results/research_gap_priorities.csv\n")
if (exists("equity_ratios")) {
  cat("✓ results/north_south_equity_ratios.csv\n")
}
cat("✓ results/geographic_bias_analysis_report.txt\n")

cat("\n=== RESEARCH EQUITY SUMMARY ===\n")
cat("Key findings including zero-study countries:\n\n")

# Print summary of zero-study situation
if (exists("zero_study_countries")) {
  total_zero_studies <- sum(zero_study_countries$countries_with_zero_studies)
  cat("CRITICAL FINDING: ", total_zero_studies, " countries have ZERO endophyte research studies\n")
  print(zero_study_countries)
}

# Print comprehensive equity analysis
if (exists("comprehensive_equity")) {
  cat("\nCountry coverage by development status:\n")
  print(comprehensive_equity)
}

# Print coverage ratios
if (exists("coverage_ratios")) {
  cat("\nResearch coverage disparities (accounting for ALL countries):\n")
  print(coverage_ratios)
}

cat("\nThis analysis reveals the true scope of geographic bias by including countries\n")
cat("with zero studies, providing a more accurate assessment of global research equity.\n")

# Save comprehensive results
if (exists("country_studies_complete")) {
  write_csv(country_studies_complete, "results/country_analysis_with_zero_studies.csv")
  cat("\n✓ results/country_analysis_with_zero_studies.csv\n")
}
if (exists("comprehensive_equity")) {
  write_csv(comprehensive_equity, "results/global_north_south_comprehensive_equity.csv")
  cat("✓ results/global_north_south_comprehensive_equity.csv\n")
}

cat("\n=== GEOGRAPHIC ANALYSIS COMPLETE ===\n")
cat("Key insights identified:\n")
cat("🌍 Global research equity assessed\n")
cat("📍 Country-level patterns mapped\n")
cat("🏞️ Ecosystem coverage analyzed\n")
cat("🔬 Research capacity gaps identified\n")
cat("🎯 Priority regions highlighted\n")
cat("\nUse these findings to guide future research priorities and funding decisions!\n")
