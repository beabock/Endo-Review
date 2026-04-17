library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

INPUT_FILE <- "data/country_enriched_data.csv"
RESULTS_DIR <- "results/country_analysis"
SUMMARY_FILE <- file.path(RESULTS_DIR, "country_gdp_latitude_summary.csv")
CORR_FILE <- file.path(RESULTS_DIR, "country_gdp_latitude_correlations.csv")
SCATTER_FILE <- file.path(RESULTS_DIR, "country_gdp_latitude_scatter.png")

if (!file.exists(INPUT_FILE)) {
  stop("Input file not found: ", INPUT_FILE)
}

if (!dir.exists(RESULTS_DIR)) {
  dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
}

country_data <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  mutate(
    study_count = as.numeric(study_count),
    centroid_lat = as.numeric(centroid_lat),
    gdp_current_usd = as.numeric(gdp_current_usd),
    gdp_log10 = ifelse(!is.na(gdp_current_usd) & gdp_current_usd > 0, log10(gdp_current_usd), NA_real_)
  )

analysis_data <- country_data %>%
  filter(!is.na(study_count), !is.na(centroid_lat))

corr_pairs <- list(
  list(
    x = "gdp_log10",
    y = "study_count",
    label = "study_count_vs_log10_gdp",
    x_label = "log10(Current GDP, USD)",
    y_label = "Study count"
  ),
  list(
    x = "centroid_lat",
    y = "study_count",
    label = "study_count_vs_latitude",
    x_label = "Country centroid latitude",
    y_label = "Study count"
  )
)

correlation_results <- lapply(corr_pairs, function(spec) {
  subset_data <- analysis_data %>% filter(!is.na(.data[[spec$x]]), !is.na(.data[[spec$y]]))

  pearson <- cor.test(subset_data[[spec$x]], subset_data[[spec$y]], method = "pearson")
  spearman <- cor.test(subset_data[[spec$x]], subset_data[[spec$y]], method = "spearman", exact = FALSE)

  tibble(
    analysis = spec$label,
    n = nrow(subset_data),
    pearson_r = unname(pearson$estimate),
    pearson_p = pearson$p.value,
    spearman_rho = unname(spearman$estimate),
    spearman_p = spearman$p.value
  )
}) %>%
  bind_rows()

write_csv(correlation_results, CORR_FILE)

summary_table <- analysis_data %>%
  select(iso_a3, country_name, study_count, centroid_lat, centroid_lon, gdp_year, gdp_current_usd, gdp_log10) %>%
  arrange(desc(study_count), country_name)

write_csv(summary_table, SUMMARY_FILE)

scatter_data <- bind_rows(
  analysis_data %>% mutate(analysis = "study_count_vs_log10_gdp", x_value = gdp_log10, x_label = "log10(Current GDP, USD)") %>%
    select(analysis, x_value, x_label, study_count, country_name, iso_a3),
  analysis_data %>% mutate(analysis = "study_count_vs_latitude", x_value = centroid_lat, x_label = "Country centroid latitude") %>%
    select(analysis, x_value, x_label, study_count, country_name, iso_a3)
) %>%
  filter(!is.na(x_value), !is.na(study_count))

scatter_plot <- ggplot(scatter_data, aes(x = x_value, y = study_count)) +
  geom_point(alpha = 0.65, size = 2, color = "#2b8cbe") +
  geom_smooth(method = "lm", se = TRUE, color = "#d7301f", linewidth = 0.8) +
  facet_wrap(~analysis, scales = "free_x", ncol = 1, labeller = labeller(
    analysis = c(
      study_count_vs_log10_gdp = "Study Count vs log10(GDP)",
      study_count_vs_latitude = "Study Count vs Latitude"
    )
  )) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Country-level research intensity vs GDP and latitude",
    x = NULL,
    y = "Study count"
  )

ggsave(SCATTER_FILE, scatter_plot, width = 9, height = 10, dpi = 300)

cat("Country GDP/latitude analysis complete:\n")
cat("  Input countries: ", nrow(country_data), "\n", sep = "")
cat("  Countries analyzed: ", nrow(analysis_data), "\n", sep = "")
cat("  Correlation rows written: ", nrow(correlation_results), "\n", sep = "")
cat("  Summary table saved to: ", SUMMARY_FILE, "\n", sep = "")
cat("  Correlations saved to: ", CORR_FILE, "\n", sep = "")
cat("  Scatter plot saved to: ", SCATTER_FILE, "\n", sep = "")
