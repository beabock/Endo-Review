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

cat("=== GEOGRAPHIC BIAS AND RESEARCH GAP ANALYSIS ===\n")
cat("Identifying research inequities and understudied regions\n\n")

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

cat("Total abstracts with geographic information:", nrow(geographic_data), "\n")

# 1. Global North vs South research equity
cat("\n1. Analyzing Global North/South research equity...\n")

development_analysis <- geographic_data %>%
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

cat("Research equity analysis:\n")
print(development_analysis)

# Calculate equity ratios
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
  
  cat("\nEquity ratios (North/South):\n")
  print(equity_ratios)
}

# 2. Country-specific analysis
cat("\n2. Analyzing country-specific research patterns...\n")

# Extract individual countries and count studies
country_studies <- geographic_data %>%
  filter(!is.na(countries_detected)) %>%
  separate_rows(countries_detected, sep = "; ") %>%
  mutate(country_clean = str_trim(countries_detected)) %>%
  filter(country_clean != "") %>%
  group_by(country_clean, development_focus) %>%
  summarise(
    total_studies = n(),
    studies_with_species = sum(has_species),
    molecular_studies = sum(has_molecular, na.rm = TRUE),
    molecular_pct = round(100 * molecular_studies / total_studies, 1),
    avg_method_score = round(mean(method_score, na.rm = TRUE), 2),
    avg_info_score = round(mean(info_score, na.rm = TRUE), 2),
    high_quality_studies = sum(info_score >= 3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_studies))

cat("Top 15 most studied countries:\n")
print(country_studies %>% head(15))

# Identify understudied countries with high biodiversity potential
understudied_countries <- country_studies %>%
  filter(total_studies < 5, development_focus == "Global South") %>%
  arrange(desc(total_studies))

cat("\nUnderstudied Global South countries (< 5 studies):\n")
print(understudied_countries %>% head(10))

# 3. Continental analysis
cat("\n3. Analyzing continental research patterns...\n")

continental_studies <- geographic_data %>%
  filter(!is.na(continents_detected)) %>%
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

# Define major biodiversity hotspots (simplified list)
biodiversity_hotspots <- c(
  "madagascar", "brazil", "indonesia", "malaysia", "philippines", "colombia", 
  "ecuador", "peru", "costa rica", "mexico", "south africa", "australia", 
  "new zealand", "chile", "california", "mediterranean", "india", "china",
  "myanmar", "thailand", "vietnam", "cameroon", "tanzania", "kenya"
)

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
write_csv(country_studies, "results/country_research_patterns.csv")
write_csv(continental_studies, "results/continental_research_analysis.csv")
write_csv(ecosystem_studies, "results/ecosystem_research_coverage.csv")
write_csv(capacity_analysis, "results/research_capacity_analysis.csv")
write_csv(hotspot_coverage, "results/biodiversity_hotspot_coverage.csv")
write_csv(research_gaps, "results/research_gap_priorities.csv")

if (exists("equity_ratios")) {
  write_csv(equity_ratios, "results/north_south_equity_ratios.csv")
}

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
  print(country_studies %>% head(10) %>% select(country_clean, total_studies, molecular_pct, development_focus))
  cat("\n")
  
  cat("Understudied high-biodiversity countries:\n")
  print(understudied_countries %>% head(10) %>% select(country_clean, total_studies, molecular_pct))
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

cat("\n=== GEOGRAPHIC ANALYSIS COMPLETE ===\n")
cat("Key insights identified:\n")
cat("🌍 Global research equity assessed\n")
cat("📍 Country-level patterns mapped\n")
cat("🏞️ Ecosystem coverage analyzed\n")
cat("🔬 Research capacity gaps identified\n")
cat("🎯 Priority regions highlighted\n")
cat("\nUse these findings to guide future research priorities and funding decisions!\n")
