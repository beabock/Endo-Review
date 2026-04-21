library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)
library(grid)

source("scripts/utils/disputed_territory_parent_iso.R")
source("scripts/plotting/theme_utils.R")

# Load the enriched country-level data
country_papers <- read.csv("data/country_enriched_data.csv")

# Preserve the precomputed study counts from the enrichment step
country_papers <- country_papers %>%
  mutate(
    study_count = as.numeric(study_count),
    study_count_plot = log10(study_count + 1)
  )

# Load world map
world <- ne_countries(scale = 50, returnclass = "sf") %>%
  apply_disputed_parent_iso_world() %>%
  filter(!is.na(iso_a3), iso_a3 != "-99")

# Join study counts with world map data
world_data <- world %>%
  left_join(country_papers, by = c("iso_a3" = "iso_a3"))

# Build explicit status labels for countries in the world map.
country_status <- world %>%
  st_drop_geometry() %>%
  distinct(iso_a3, name) %>%
  left_join(country_papers %>% select(iso_a3, study_count), by = "iso_a3") %>%
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
  left_join(world %>% st_drop_geometry() %>% distinct(iso_a3, name), by = "iso_a3") %>%
  select(country_name = name, iso_a3, study_count) %>%
  arrange(desc(study_count))
print(summary_data)


cat("\nTotal countries with studies:", nrow(country_papers), "\n")
cat("Total studies across all countries:", sum(country_papers$study_count), "\n")

# Show countries NOT represented in the dataset
all_countries <- world %>% pull(iso_a3) %>% unique() %>% sort()
countries_with_data <- country_papers %>% filter(study_count > 0) %>% pull(iso_a3) %>% unique() %>% sort()
countries_without_data <- setdiff(all_countries, countries_with_data)

cat("\n\nCountries/territories NOT represented in dataset (", length(countries_without_data), " total):\n", sep="")
# Get country names from world map for display
world_names <- world %>%
  filter(iso_a3 %in% countries_without_data) %>%
  select(name, iso_a3) %>%
  st_drop_geometry() %>%
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


