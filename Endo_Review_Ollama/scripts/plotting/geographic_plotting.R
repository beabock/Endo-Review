library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)

# Load the standardized geographic data
standardized_data <- read.csv("data/standardized_country_data.csv")

# Count studies per country
country_papers <- standardized_data %>%
  group_by(iso_a3) %>%
  summarise(study_count = n(), .groups = 'drop')

# Load world map
world <- ne_countries(scale = 50, returnclass = "sf")

# Join study counts with world map data
world_data <- world %>%
  left_join(country_papers, by = c("iso_a3" = "iso_a3"))

# Transform to Robinson projection
robinson_proj <- "+proj=robin"
world_robinson <- st_transform(world_data, robinson_proj)

# Create the map with better distinction between no data and low counts
map <- ggplot(world_robinson) +
  geom_sf(aes(fill = study_count), color = "white", linewidth = 0.2) +
  scale_fill_gradientn(
    name = "Number of Studies",
    colors = c("#d4d4d4", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#e31a1c", "#bd0026", "#800026"),
    values = scales::rescale(c(0, 1, 2, 3, 5, 10, 15, 20, 50)),
    na.value = "#666666",
    breaks = c(1, 2, 3, 5, 10, 15, 20),
    limits = c(0.5, NA)
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 15,
    barheight = 0.8
  )) +
  theme_minimal() +
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
    caption = "Dark gray = No data | Light colors = Low study counts | Dark colors = High study counts"
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
countries_with_data <- country_papers %>% pull(iso_a3) %>% unique() %>% sort()
countries_without_data <- setdiff(all_countries, countries_with_data)

cat("\n\nCountries/territories NOT represented in dataset (", length(countries_without_data), " total):\n", sep="")
# Get country names from world map for display
world_names <- world %>%
  filter(iso_a3 %in% countries_without_data) %>%
  select(name, iso_a3) %>%
  st_drop_geometry() %>%
  arrange(name)
cat(paste(world_names$name, collapse = ", "), "\n")
