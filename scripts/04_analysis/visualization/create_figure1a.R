# Create Figure 1A: PRISMA-style flow diagram for literature screening pipeline
# B. Bock
# October 2025

library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)

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
    "ML relevance classification:\n19,071 relevant abstracts\n(excluding mycorrhizal-only studies)",
    "ML presence/absence classification:\n18,982 presence\n89 absence"
  ),
  x = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
  y = c(0.95, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25)
)

# Create the flow diagram plot
p_flow <- ggplot(flow_steps, aes(x = x, y = y, label = label)) +
  geom_point(size = 20, color = "#46ACC8", alpha = 0.8) +
  geom_text(size = 3.5, fontface = "bold", color = "black", hjust = 0.5, vjust = 0.5) +
  geom_segment(data = flow_steps[-nrow(flow_steps), ],
               aes(x = x, y = y - 0.05, xend = x, yend = y - 0.1),
               arrow = arrow(length = unit(0.3, "cm")), color = "#B40F20", size = 1) +
  xlim(0.3, 0.7) +
  ylim(0.2, 1.0) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Figure 1A: Literature Screening Pipeline",
    subtitle = "PRISMA-style flow diagram showing systematic literature review process"
  )

# Save the plot
dir.create("plots/main", showWarnings = FALSE, recursive = TRUE)
ggsave("plots/main/figure1a_pipeline_flow.png", p_flow, width = 10, height = 12, dpi = 300, bg = "white")

# Also save as supplementary
dir.create("plots/supplementary", showWarnings = FALSE, recursive = TRUE)
ggsave("plots/supplementary/figure1a_pipeline_flow.png", p_flow, width = 10, height = 12, dpi = 300, bg = "white")

cat("Figure 1A created and saved to plots/main/ and plots/supplementary/\n")