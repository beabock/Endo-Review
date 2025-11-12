# Create Figure 1A: PRISMA-style flow diagram for literature screening pipeline
# B. Bock
# October 2025

library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)

# Extract abstract counts for proportional sizing
extract_count <- function(label) {
  # Extract the first number from the label (the main count)
  count_str <- str_extract(label, "\\d{1,3}(?:,\\d{3})*(?=\\s)")
  if (is.na(count_str)) return(1000)  # Default small size for non-count steps
  as.numeric(gsub(",", "", count_str))
}

library(stringr)  # Add stringr for str_extract

# Define the flow diagram data
flow_steps <- data.frame(
  step = 1:8,
  label = c(
    "Initial database searches:\nWeb of Science (14,855)\nPubMed (10,873)\nScopus (15,427)",
    "Combined datasets:\n40,776 abstracts",
    "Removed duplicates by DOI:\n23,321 abstracts",
    "Removed duplicates by abstract text:\n23,007 abstracts",
    "Filtered to articles only:\n22,587 abstracts",
    "Removed duplicates by title:\n21,891 abstracts",
    "ML relevance classification:\n19,071 relevant abstracts\n(excluding 1,436 mycorrhizal-only studies)",
    "ML presence/absence classification:\n18,982 presence\n89 absence"
  ),
  x = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
  y = c(0.95, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25)
) %>%
  mutate(
    # Extract abstract counts for proportional sizing
    abstract_count = sapply(label, extract_count),
    # Calculate circle sizes proportional to sqrt of counts (for area proportionality)
    circle_size = sqrt(abstract_count) / sqrt(max(abstract_count)) * 25  # Scale to reasonable size range
  )

# Calculate box heights and positions to prevent overlap
box_heights <- flow_steps$circle_size / 35  # Scale down for better spacing
box_bottoms <- flow_steps$y - box_heights/2
box_tops <- flow_steps$y + box_heights/2

# Create separate data for arrows with proper positioning
arrow_data <- data.frame(
  x = flow_steps$x[1:7],
  y_start = box_bottoms[1:7] - 0.02,
  y_end = box_tops[2:8] + 0.02
)

# Create boxes for each step with proportional heights based on abstract counts
p_flow <- ggplot(flow_steps, aes(x = x, y = y, label = label)) +
  # Draw boxes with height proportional to abstract counts
  geom_tile(aes(height = box_heights, width = 0.35),
            fill = "#46ACC8", alpha = 0.8, color = "black", size = 1) +
  # Add text labels with better positioning
  geom_text(size = 3.2, fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5,
            lineheight = 0.8) +
  # Draw arrows between boxes with proper positioning
  geom_segment(data = arrow_data,
               aes(x = x, y = y_start, xend = x, yend = y_end),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "#B40F20", size = 1.2,
               inherit.aes = FALSE) +
  xlim(0.1, 0.9) +
  ylim(0.0, 1.2) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Figure 1A: Literature Screening Pipeline",
    subtitle = "PRISMA-style flow diagram showing systematic literature review process\nBox heights proportional to abstract counts"
  )

# Save the plot
dir.create("plots/main", showWarnings = FALSE, recursive = TRUE)
ggsave("plots/main/figure1a_pipeline_flow.png", p_flow, width = 10, height = 12, dpi = 300, bg = "white")

# Also save as supplementary
dir.create("plots/supplementary", showWarnings = FALSE, recursive = TRUE)
ggsave("plots/supplementary/figure1a_pipeline_flow.png", p_flow, width = 10, height = 12, dpi = 300, bg = "white")

cat("Figure 1A created and saved to plots/main/ and plots/supplementary/\n")