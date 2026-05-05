#!/usr/bin/env Rscript
# Visualize understudied country enrichment in biodiversity priority regions
# Shows what % of understudied endophyte countries rank at different priority thresholds

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)

library(scales)
source("scripts/05_plotting/theme_utils.R")

# Setup
INPUT_SENSITIVITY <- "results/biodiversity_priority_overlap/sensitivity_analysis.csv"
INPUT_COUNTRY_SUMMARY <- "results/country_analysis/country_gdp_latitude_summary.csv"
INPUT_PRIORITY_COUNTRIES <- "data/biodiversity/biodiversity_priority_countries.csv"
if (!file.exists(INPUT_PRIORITY_COUNTRIES)) {
  INPUT_PRIORITY_COUNTRIES <- "data/biodiversity_priority_countries.csv"
}
OUTPUT_PLOT <- file.path(OUTPUT_DIR, "priority_overlap_sensitivity.png")
OUTPUT_PLOT_DETAILED <- file.path(OUTPUT_DIR, "priority_overlap_sensitivity_detailed.png")

OUTPUT_SCATTER_COMBINED <- file.path(OUTPUT_DIR, "priority_overlap_scatter.png")
OUTPUT_SCATTER_TOTAL <- file.path(OUTPUT_DIR, "priority_overlap_scatter_total.png")
OUTPUT_SCATTER_ENDEMIC <- file.path(OUTPUT_DIR, "priority_overlap_scatter_endemic.png")
OUTPUT_SCATTER_THREATENED <- file.path(OUTPUT_DIR, "priority_overlap_scatter_threatened.png")
OUTPUT_Unevenness_COMBINED <- file.path(OUTPUT_DIR, "priority_overlap_unevenness.png")
OUTPUT_Unevenness_DETAILED <- file.path(OUTPUT_DIR, "priority_overlap_unevenness_detailed.png")
# Load data
sensitivity <- read_csv(INPUT_SENSITIVITY, show_col_types = FALSE)
country_summary <- read_csv(INPUT_COUNTRY_SUMMARY, show_col_types = FALSE)

priority_countries <- read_csv(INPUT_PRIORITY_COUNTRIES, show_col_types = FALSE)
priority_countries <- priority_countries %>%
  mutate(
    iso_a3 = if ("iso3" %in% names(.)) as.character(iso3) else if ("iso_a3" %in% names(.)) as.character(iso_a3) else NA_character_
  )
total_countries <- nrow(country_summary)
understudied_countries <- sum(country_summary$study_count == 0, na.rm = TRUE)

# Convert quantiles to percentile labels (top X%) and add random-expectation baselines
sensitivity <- sensitivity %>%
  mutate(
    priority_pct = 100 * (1 - quantile),
    priority_label = paste0("Top ", round(priority_pct, 0), "%"),
    priority_label = factor(priority_label,
      levels = c("Top 10%", "Top 25%", "Top 50%", "Top 75%")
    ),
    expected_overlap_pct = 100 * n_priority_countries / total_countries,
    expected_overlap_count = understudied_countries * n_priority_countries / total_countries
  ) %>%
  arrange(priority_label)

plot_metric_map <- c(
  WB_TOTAL = "Total species",
  WB_SMALL50XENDEMIC100 = "Endemic species",
  WB_TPROB80 = "Threatened species probability"
)

plot_data <- country_summary %>%
  select(iso_a3, country_name, study_count) %>%
  mutate(
    study_count = as.numeric(study_count),
    study_count_log = log10(study_count + 1),
    understudied = study_count == 0
  ) %>%
  left_join(
    priority_countries %>%
      filter(source %in% names(plot_metric_map)) %>%
      mutate(metric_label = recode(source, !!!plot_metric_map)) %>%
      select(iso_a3, source, metric_label, priority_score),
    by = "iso_a3"
  ) %>%
  filter(!is.na(priority_score)) %>%
  mutate(
    metric_value = as.numeric(priority_score),
    metric_label = factor(metric_label, levels = unname(plot_metric_map))
  )

make_scatter <- function(df, metric_name, output_path, plot_title, plot_subtitle, x_label) {
  metric_df <- df %>% filter(metric_label == metric_name)

  if (nrow(metric_df) == 0) {
    return(NULL)
  }

  fit <- lm(study_count_log ~ metric_value, data = metric_df)
  fit_summary <- summary(fit)
  correlation_test <- suppressWarnings(cor.test(metric_df$metric_value, metric_df$study_count_log, method = "spearman", exact = FALSE))

  stats_label <- paste0(
    "n = ", nrow(metric_df),
    "\nR2 = ", round(fit_summary$r.squared, 2),
    "\nSpearman r = ", round(unname(correlation_test$estimate), 2),
    "\np = ", format.pval(fit_summary$coefficients[2, 4], digits = 2, eps = 0.001)
  )

  label_data <- tibble(
    x_pos = quantile(metric_df$metric_value, 0.05, na.rm = TRUE),
    y_pos = max(metric_df$study_count_log, na.rm = TRUE),
    stats_label = stats_label
  )

  p <- ggplot(metric_df, aes(x = metric_value, y = study_count_log)) +
    geom_point(aes(color = understudied), alpha = 0.8, size = 2.2) +
    geom_smooth(method = "lm", se = TRUE, color = "#b22222", linewidth = 0.9) +
    geom_label(
      data = label_data,
      aes(x = x_pos, y = y_pos, label = stats_label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1.1,
      size = 3.2,
      label.size = 0.25,
      fill = "white",
      alpha = 0.88
    ) +
    theme_endo_bw(base_size = 12) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_label,
      y = "log10(endophyte study count + 1)"
    ) +
    scale_x_continuous(labels = comma) +
    scale_color_manual(
      values = c(
        `TRUE` = "#E24A33",
        `FALSE` = "#1f78b4"
      ),
      labels = c("Studied countries", "Understudied countries"),
      name = NULL
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )

  ggsave(output_path, p, width = 8, height = 5.5, dpi = 300, bg = "white")
  return(p)
}

# ===== PLOT 1: Observed vs expected overlap (percent) =====
p1 <- ggplot(sensitivity, aes(x = priority_label, group = 1)) +
  geom_line(aes(y = expected_overlap_pct, color = "Expected by chance"), linewidth = 1.0, linetype = "dashed") +
  geom_point(aes(y = expected_overlap_pct, color = "Expected by chance"), size = 2.8) +
  geom_line(aes(y = pct_understudied_overlapping, color = "Observed understudied overlap"), linewidth = 1.1) +
  geom_point(aes(y = pct_understudied_overlapping, color = "Observed understudied overlap"), size = 3.2) +
  geom_text(
    aes(y = pct_understudied_overlapping, label = paste0(round(pct_understudied_overlapping, 1), "%")),
    vjust = -0.9,
    fontface = "bold",
    size = 3.8,
    color = "#222222"
  ) +
  scale_color_manual(
    values = c(
      "Observed understudied overlap" = "#E24A33",
      "Expected by chance" = "#7A7A7A"
    ),
    name = NULL
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    x = "Priority Level (World Bank Biodiversity Metrics)",
    y = "Understudied Endophyte Countries (%)",
    title = "Understudied Endophyte Regions are Concentrated in High-Priority Biodiversity Areas",
    subtitle = paste0(understudied_countries, " understudied countries compared against ", total_countries, " total countries")
  ) +
  theme_endo_bw() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40", margin = margin(b = 10)),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  )

ggsave(OUTPUT_PLOT, p1, width = 10, height = 6, dpi = 300, bg = "white")
cat("Main plot saved to:", OUTPUT_PLOT, "\n")

# ===== PLOT 2: Observed vs expected counts =====
p2 <- ggplot(sensitivity, aes(x = priority_label, group = 1)) +
  geom_linerange(aes(ymin = expected_overlap_count, ymax = n_overlap_countries, color = "Observed minus expected"), linewidth = 1.2) +
  geom_point(aes(y = n_overlap_countries, color = "Observed overlap"), size = 3.2) +
  geom_point(aes(y = expected_overlap_count, color = "Expected overlap"), size = 2.8, shape = 17) +
  geom_text(
    aes(y = n_overlap_countries, label = n_overlap_countries),
    vjust = -0.9,
    fontface = "bold",
    size = 3.6,
    color = "#222222"
  ) +
  scale_color_manual(
    values = c(
      "Observed overlap" = "#E24A33",
      "Expected overlap" = "#7A7A7A",
      "Observed minus expected" = "#C0C0C0"
    ),
    name = NULL
  ) +
  labs(
    x = "Priority Level",
    y = "Number of Understudied Countries",
    title = "Observed Overlap Versus Random Expectation"
  ) +
  theme_endo_bw() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  )

p2_combined <- grid.arrange(
  p1,
  p2,
  ncol = 1,
  heights = c(1.15, 0.95),
  top = grid::textGrob(
    "Priority Overlap Analysis",
    gp = grid::gpar(fontsize = 13, fontface = "bold")
  )
)

ggsave(OUTPUT_PLOT_DETAILED, p2_combined, width = 12, height = 9, dpi = 300, bg = "white")
cat("Detailed plot saved to:", OUTPUT_PLOT_DETAILED, "\n")

# ===== PLOT 3: Metric scatter plots (combined + standalone) =====
scatter_total <- make_scatter(
  plot_data,
  "Total species",
  OUTPUT_SCATTER_TOTAL,
  "Endophyte Study Effort vs World Bank Total Species Richness",
  paste0("WB_TOTAL; understudied countries highlighted (n = ", understudied_countries, ")"),
  "World Bank total species count"
)

scatter_endemic <- make_scatter(
  plot_data,
  "Endemic species",
  OUTPUT_SCATTER_ENDEMIC,
  "Endophyte Study Effort vs World Bank Endemic Species Richness",
  paste0("WB_SMALL50XENDEMIC100; understudied countries highlighted (n = ", understudied_countries, ")"),
  "World Bank endemic species count"
)

scatter_threatened <- make_scatter(
  plot_data,
  "Threatened species probability",
  OUTPUT_SCATTER_THREATENED,
  "Endophyte Study Effort vs World Bank Threatened Species Probability",
  paste0("WB_TPROB80; understudied countries highlighted (n = ", understudied_countries, ")"),
  "World Bank threatened species probability"
)

facet_plot <- plot_data %>%
  mutate(metric_label = factor(metric_label, levels = unname(plot_metric_map))) %>%
  ggplot(aes(x = metric_value, y = study_count_log)) +
  geom_point(aes(color = understudied), alpha = 0.8, size = 1.9) +
  geom_smooth(method = "lm", se = TRUE, color = "#b22222", linewidth = 0.8) +
  facet_wrap(~ metric_label, scales = "free_x", nrow = 1) +
  theme_endo_bw(base_size = 11) +
  labs(
    title = "Endophyte Study Effort vs World Bank Biodiversity Metrics",
    subtitle = "Each panel shows a different World Bank richness metric; understudied countries are highlighted",
    x = NULL,
    y = "log10(endophyte study count + 1)"
  ) +
  scale_x_continuous(labels = comma) +
  scale_color_manual(
    values = c(
      `TRUE` = "#E24A33",
      `FALSE` = "#1f78b4"
    ),
    labels = c("Studied countries", "Understudied countries"),
    name = NULL
  ) +
  geom_label(
    data = plot_data %>%
      group_by(metric_label) %>%
      summarise(
        x_pos = quantile(metric_value, 0.05, na.rm = TRUE),
        y_pos = max(study_count_log, na.rm = TRUE),
        stats_label = paste0(
          "n = ", n(),
          "\nR2 = ", round(summary(lm(study_count_log ~ metric_value))$r.squared, 2),
          "\nSpearman r = ", round(unname(cor.test(metric_value, study_count_log, method = "spearman", exact = FALSE)$estimate), 2),
          "\np = ", format.pval(summary(lm(study_count_log ~ metric_value))$coefficients[2, 4], digits = 2, eps = 0.001)
        ),
        .groups = "drop"
      ),
    aes(x = x_pos, y = y_pos, label = stats_label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1.1,
    size = 3,
    label.size = 0.25,
    fill = "white",
    alpha = 0.88
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

ggsave(OUTPUT_SCATTER_COMBINED, facet_plot, width = 14, height = 5.5, dpi = 300, bg = "white")
cat("Combined scatter plot saved to:", OUTPUT_SCATTER_COMBINED, "\n")

if (!is.null(scatter_total)) cat("Standalone total-species scatter saved to:", OUTPUT_SCATTER_TOTAL, "\n")
if (!is.null(scatter_endemic)) cat("Standalone endemic-species scatter saved to:", OUTPUT_SCATTER_ENDEMIC, "\n")
if (!is.null(scatter_threatened)) cat("Standalone threatened-probability scatter saved to:", OUTPUT_SCATTER_THREATENED, "\n")

# ===== PLOT 4: Study-count unevenness across biodiversity-metric quartiles =====
unevenness_data <- plot_data %>%
  group_by(metric_label) %>%
  mutate(metric_quartile = ntile(metric_value, 4)) %>%
  ungroup() %>%
  mutate(
    metric_quartile = factor(
      metric_quartile,
      levels = c(1, 2, 3, 4),
      labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")
    )
  )

unevenness_stats <- unevenness_data %>%
  group_by(metric_label) %>%
  summarise(
    x_pos = 1.5,
    y_pos = max(study_count, na.rm = TRUE),
    p_value = suppressWarnings(kruskal.test(study_count ~ metric_quartile)$p.value),
    median_low = median(study_count[metric_quartile == "Q1 (lowest)"], na.rm = TRUE),
    median_high = median(study_count[metric_quartile == "Q4 (highest)"], na.rm = TRUE),
    stats_label = paste0(
      "Kruskal-Wallis p = ", format.pval(p_value, digits = 2, eps = 0.001),
      "\nmedian Q1 = ", round(median_low, 1),
      "\nmedian Q4 = ", round(median_high, 1)
    ),
    .groups = "drop"
  )

unevenness_plot <- unevenness_data %>%
  ggplot(aes(x = metric_quartile, y = study_count)) +
  geom_boxplot(aes(fill = metric_quartile), outlier.shape = NA, alpha = 0.75, width = 0.7) +
  geom_jitter(aes(color = understudied), width = 0.12, alpha = 0.45, size = 1.2) +
  facet_wrap(~ metric_label, scales = "free_y") +
  geom_label(
    data = unevenness_stats,
    aes(x = x_pos, y = y_pos, label = stats_label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1.1,
    size = 3,
    label.size = 0.25,
    fill = "white",
    alpha = 0.88
  ) +
  scale_fill_manual(
    values = c(
      "Q1 (lowest)" = "#d9d9d9",
      "Q2" = "#a6bddb",
      "Q3" = "#74a9cf",
      "Q4 (highest)" = "#0570b0"
    ),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(
      `TRUE` = "#E24A33",
      `FALSE` = "#1f78b4"
    ),
    labels = c("Studied countries", "Understudied countries"),
    name = NULL
  ) +
  scale_y_continuous(labels = comma) +
  theme_endo_bw(base_size = 11) +
  labs(
    title = "Are Study Counts Uneven Across Biodiversity-Priority Quartiles?",
    subtitle = "Each panel bins countries by one World Bank biodiversity metric; higher quartiles should reveal lower study effort if bias is uneven",
    x = "Biodiversity priority quartile",
    y = "Endophyte study count"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

ggsave(OUTPUT_Unevenness_COMBINED, unevenness_plot, width = 13.5, height = 5.5, dpi = 300, bg = "white")
cat("Combined unevenness plot saved to:", OUTPUT_Unevenness_COMBINED, "\n")

unevenness_detailed <- unevenness_plot +
  theme(strip.text = element_text(face = "bold"))

ggsave(OUTPUT_Unevenness_DETAILED, unevenness_detailed, width = 13.5, height = 5.5, dpi = 300, bg = "white")
cat("Detailed unevenness plot saved to:", OUTPUT_Unevenness_DETAILED, "\n")

cat("\nPlots complete\n")
