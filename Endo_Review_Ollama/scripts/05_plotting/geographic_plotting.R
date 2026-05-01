library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)
library(grid)

source("scripts/utils/disputed_territory_parent_iso.R")
source("scripts/05_plotting/theme_utils.R")

# Load the enriched country-level data
country_papers <- read.csv("data/country_enriched_data.csv") %>%
  mutate(
    study_count = as.numeric(study_count),
    study_count_plot = log10(study_count + 1)
  ) %>%
  distinct(iso_a3, .keep_all = TRUE)

# Load world map
world <- ne_countries(scale = 50, returnclass = "sf") %>%
  apply_disputed_parent_iso_world() %>%
  filter(!is.na(iso_a3), iso_a3 != "-99")

world_lookup <- world %>%
  st_drop_geometry() %>%
  distinct(iso_a3, .keep_all = TRUE) %>%
  select(iso_a3, name)

# Join study counts with world map data
world_data <- world %>%
  left_join(country_papers, by = c("iso_a3" = "iso_a3"), relationship = "many-to-one")

# Build explicit status labels for countries in the world map.
country_status <- world_lookup %>%
  left_join(country_papers %>% select(iso_a3, study_count), by = "iso_a3", relationship = "many-to-one") %>%
  mutate(
    data_status = case_when(
      is.na(study_count) ~ "NA",
      study_count == 0 ~ "0",
      study_count > 0 ~ ">0"
    )
  )

# Transform to Robinson projection
robinson_proj <- "+proj=robin"
world_robinson <- st_transform(world_data, robinson_proj)

# Create the map with a smooth gradient while compressing extreme outliers
legend_breaks <- c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000)
legend_breaks <- legend_breaks[legend_breaks <= max(world_robinson$study_count, na.rm = TRUE)]

map <- ggplot(world_robinson) +
  geom_sf(aes(fill = study_count_plot), color = "white", linewidth = 0.2) +
  scale_fill_endo_map(
    name = "Studies per country",
    breaks = log10(legend_breaks + 1),
    labels = scales::label_number(accuracy = 1)(legend_breaks),
    na.value = "#666666"
  ) +
  guides(fill = guide_endo_colorbar(width_cm = 14, height_cm = 0.45)) +
  theme_endo_bw(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "Number of Endophyte Studies by Country",
    caption = "Dark gray = No data | Light colors = fewer studies | Dark colors = more studies"
  )

# Save the map
ggsave("results/study_count_by_country_robinson.png", map, width = 14, height = 8, dpi = 300)


# Print summary stats
cat("Summary of studies by country:\n")
# Joining with country names for a more readable summary
summary_data <- country_papers %>%
  left_join(world_lookup, by = "iso_a3", relationship = "many-to-one") %>%
  select(country_name = name, iso_a3, study_count) %>%
  arrange(desc(study_count))
print(summary_data)


cat("\nTotal countries with studies:", nrow(country_papers), "\n")
cat("Total studies across all countries:", sum(country_papers$study_count), "\n")

# Show countries NOT represented in the dataset
all_countries <- world_lookup %>% pull(iso_a3) %>% unique() %>% sort()
countries_with_data <- country_papers %>% filter(study_count > 0) %>% pull(iso_a3) %>% unique() %>% sort()
countries_without_data <- setdiff(all_countries, countries_with_data)

cat("\n\nCountries/territories NOT represented in dataset (", length(countries_without_data), " total):\n", sep="")
# Get country names from world map for display
world_names <- world_lookup %>%
  filter(iso_a3 %in% countries_without_data) %>%
  arrange(name)
cat(paste(world_names$name, collapse = ", "), "\n")


# Explicit NA vs 0 reporting
na_countries <- country_status %>%
  filter(data_status == "NA") %>%
  arrange(name)

zero_countries <- country_status %>%
  filter(data_status == "0") %>%
  arrange(name)

cat("\nCountries with NA study_count (no match or missing value): ", nrow(na_countries), "\n", sep = "")
cat(paste(na_countries$name, collapse = ", "), "\n")

cat("\nCountries with study_count == 0: ", nrow(zero_countries), "\n", sep = "")
cat(paste(zero_countries$name, collapse = ", "), "\n")

write.csv(na_countries, "results/countries_study_count_NA.csv", row.names = FALSE)
write.csv(zero_countries, "results/countries_study_count_zero.csv", row.names = FALSE)


# === SCATTER PLOTS: GDP AND LATITUDE ===

# Load analysis data for scatter plots
analysis_data <- country_papers %>%
  filter(!is.na(study_count), !is.na(centroid_lat)) %>%
  mutate(
    centroid_lat = as.numeric(centroid_lat),
    gdp_current_usd = as.numeric(gdp_current_usd),
    gdp_log10 = ifelse(!is.na(gdp_current_usd) & gdp_current_usd > 0, log10(gdp_current_usd), NA_real_)
  )

# Load correlation statistics from analysis output
corr_stats <- read.csv("results/country_analysis/country_gdp_latitude_correlations.csv")

# Prepare data for GDP plot
gdp_data <- analysis_data %>% 
  filter(!is.na(gdp_log10), !is.na(study_count)) %>%
  mutate(
    x_value = gdp_log10,
    y_value = log10(study_count + 1)
  ) %>%
  select(x_value, y_value, study_count, country_name, iso_a3)

gdp_stats <- corr_stats %>%
  filter(analysis == "study_count_vs_log10_gdp") %>%
  mutate(
    label = paste0(
      "Pearson r = ", round(pearson_r, 3), " (p ", 
      if_else(pearson_p < 0.001, "< 0.001", paste0("= ", round(pearson_p, 3))), ")\n",
      "Spearman ρ = ", round(spearman_rho, 3), " (p ",
      if_else(spearman_p < 0.001, "< 0.001", paste0("= ", round(spearman_p, 3))), ")"
    )
  )

# Prepare data for latitude plot
lat_data <- analysis_data %>% 
  filter(!is.na(centroid_lat), !is.na(study_count)) %>%
  mutate(
    x_value = centroid_lat,
    y_value = log10(study_count + 1)
  ) %>%
  select(x_value, y_value, study_count, country_name, iso_a3)

lat_stats <- corr_stats %>%
  filter(analysis == "study_count_vs_latitude") %>%
  mutate(
    label = paste0(
      "Pearson r = ", round(pearson_r, 3), " (p ", 
      if_else(pearson_p < 0.001, "< 0.001", paste0("= ", round(pearson_p, 3))), ")\n",
      "Spearman ρ = ", round(spearman_rho, 3), " (p ",
      if_else(spearman_p < 0.001, "< 0.001", paste0("= ", round(spearman_p, 3))), ")"
    )
  )

# GDP scatter plot
scatter_plot_gdp <- ggplot(gdp_data, aes(x = x_value, y = y_value)) +
  geom_point(alpha = 0.65, size = 2.5, color = "#2b8cbe") +
  geom_smooth(method = "lm", se = TRUE, color = "#d7301f", linewidth = 0.8) +
  geom_text(
    data = data.frame(
      x = min(gdp_data$x_value, na.rm = TRUE) + 0.05 * (max(gdp_data$x_value, na.rm = TRUE) - min(gdp_data$x_value, na.rm = TRUE)),
      y = max(gdp_data$y_value, na.rm = TRUE) - 0.05 * (max(gdp_data$y_value, na.rm = TRUE) - min(gdp_data$y_value, na.rm = TRUE)),
      label = gdp_stats$label
    ),
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 1,
    size = 3.5, color = "#333333",
    inherit.aes = FALSE
  ) +
  theme_endo_bw(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Study Count vs log10(GDP)",
    x = "log10(Current GDP, USD)",
    y = "log10(Study count + 1)"
  )

# Latitude scatter plot
scatter_plot_lat <- ggplot(lat_data, aes(x = x_value, y = y_value)) +
  geom_point(alpha = 0.65, size = 2.5, color = "#2b8cbe") +
  geom_smooth(method = "lm", se = TRUE, color = "#d7301f", linewidth = 0.8) +
  geom_text(
    data = data.frame(
      x = min(lat_data$x_value, na.rm = TRUE) + 0.05 * (max(lat_data$x_value, na.rm = TRUE) - min(lat_data$x_value, na.rm = TRUE)),
      y = max(lat_data$y_value, na.rm = TRUE) - 0.05 * (max(lat_data$y_value, na.rm = TRUE) - min(lat_data$y_value, na.rm = TRUE)),
      label = lat_stats$label
    ),
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 1,
    size = 3.5, color = "#333333",
    inherit.aes = FALSE
  ) +
  theme_endo_bw(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Study Count vs Latitude",
    x = "Country centroid latitude",
    y = "log10(Study count + 1)"
  )

ggsave("results/country_analysis/country_study_count_vs_gdp.png", scatter_plot_gdp, width = 7, height = 6, dpi = 300)
ggsave("results/country_analysis/country_study_count_vs_latitude.png", scatter_plot_lat, width = 7, height = 6, dpi = 300)

cat("\nScatter plots saved to:\n")
cat("  - results/country_analysis/country_study_count_vs_gdp.png\n")
cat("  - results/country_analysis/country_study_count_vs_latitude.png\n")



