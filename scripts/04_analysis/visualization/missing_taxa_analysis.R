library(tidyverse)
library(here)
library(scales)

# Setup output directories
dir.create("results/manuscript_tables", showWarnings = FALSE, recursive = TRUE)

message("--- Starting Unrepresented Taxa Prioritization ---")

# 1. Load Data
tryCatch({
  unrep_df <- read_csv("results/unrepresented_taxa.csv", show_col_types = FALSE)
  
  # Try loading accepted species
  if (file.exists("models/accepted_species.rds")) {
    ref_df <- readRDS("models/accepted_species.rds")
  } else if (file.exists("models/species.rds")) {
    ref_df <- readRDS("models/species.rds")
  } else {
    stop("Could not find reference species model (accepted_species.rds or species.rds)")
  }
  
  message("Data loaded successfully.")
}, error = function(e) {
  stop("Error loading data: ", e$message)
})

# 2. STRICT FILTER: Enforce Plantae and Fungi Only
message("Filtering strictly for Plantae and Fungi...")

unrep_df <- unrep_df %>%
  filter(kingdom %in% c("Plantae", "Fungi"))

ref_df <- ref_df %>%
  filter(kingdom %in% c("Plantae", "Fungi"))

message("Records retained: ", nrow(unrep_df), " unrepresented taxa.")

# 3. Calculate Reference Sizes
# We need to know how many species exist in reality for every Family and Genus
message("Calculating taxonomic sizes from reference database...")

ref_family_counts <- ref_df %>%
  filter(!is.na(family)) %>%
  group_by(kingdom, phylum, family) %>%
  summarise(total_known_species = n(), .groups = "drop")

ref_genus_counts <- ref_df %>%
  filter(!is.na(genus)) %>%
  group_by(kingdom, phylum, family, genus) %>%
  summarise(total_known_species = n(), .groups = "drop")

# 4. Identify "Black Holes" (Entire Families Missing)
missing_families_list <- unrep_df %>%
  filter(taxa_level == "family") %>%
  select(kingdom, phylum, family) %>%
  distinct() %>%
  left_join(ref_family_counts, by = c("kingdom", "phylum", "family")) %>%
  filter(!is.na(total_known_species)) %>% 
  arrange(desc(total_known_species))

# 5. Identify "Missing Giants" (Entire Genera Missing)
missing_genera_list <- unrep_df %>%
  filter(taxa_level == "genus") %>%
  select(kingdom, phylum, family, genus) %>%
  distinct() %>%
  left_join(ref_genus_counts, by = c("kingdom", "phylum", "family", "genus")) %>%
  filter(!is.na(total_known_species)) %>% 
  arrange(desc(total_known_species))

# 6. Function to format tables for Manuscript
create_top_table <- function(data, group_col, n_top = 5) {
  data %>%
    group_by(kingdom, phylum) %>%
    slice_max(order_by = total_known_species, n = n_top) %>%
    mutate(
      formatted_entry = paste0(!!sym(group_col), " (", comma(total_known_species), ")")
    ) %>%
    summarise(
      top_missing_examples = paste(formatted_entry, collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(kingdom, phylum)
}

# 7. Generate Manuscript Outputs (Tables & Plots)
message("Generating summary tables and plots...")

# CSV Outputs
top_missing_families_summary <- create_top_table(missing_families_list, "family", n_top = 5)
write_csv(top_missing_families_summary, "results/manuscript_tables/top_missing_families_by_phylum.csv")

top_missing_genera_summary <- create_top_table(missing_genera_list, "genus", n_top = 5)
write_csv(top_missing_genera_summary, "results/manuscript_tables/top_missing_genera_by_phylum.csv")

major_gaps_plantae <- missing_families_list %>% filter(kingdom == "Plantae") %>% head(20)
major_gaps_fungi <- missing_families_list %>% filter(kingdom == "Fungi") %>% head(20)

write_csv(major_gaps_plantae, "results/manuscript_tables/major_gaps_plantae.csv")
write_csv(major_gaps_fungi, "results/manuscript_tables/major_gaps_fungi.csv")

# Plotting Function
plot_missing_giants <- function(data, kingdom_name, color_hex) {
  p_data <- data %>%
    filter(kingdom == kingdom_name) %>%
    slice_head(n = 15) %>%
    mutate(family = fct_reorder(family, total_known_species))
  
  if(nrow(p_data) == 0) return(NULL)

  ggplot(p_data, aes(x = total_known_species, y = family)) +
    geom_col(fill = color_hex, width = 0.7) +
    geom_text(aes(label = comma(total_known_species)), hjust = -0.1, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste("Top 15 Unrepresented", kingdom_name, "Families"),
      subtitle = "Ranked by number of known species (GBIF Backbone)",
      x = "Total Species in Family (None Found in Literature)",
      y = "Family"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 10))
}

p_plants <- plot_missing_giants(missing_families_list, "Plantae", "#7CAE00")
p_fungi <- plot_missing_giants(missing_families_list, "Fungi", "#C77CFF")

if(!is.null(p_plants)) ggsave("results/manuscript_tables/plot_unrepresented_families_plantae.png", p_plants, width = 8, height = 6)
if(!is.null(p_fungi)) ggsave("results/manuscript_tables/plot_unrepresented_families_fungi.png", p_fungi, width = 8, height = 6)


# 8. MANUSCRIPT TEXT GENERATION (Focus: Plants)
message("Generating manuscript text file for Plants...")

# Prepare data for text generation
plant_stats <- list(
  total_missing_fam = nrow(missing_families_list %>% filter(kingdom == "Plantae")),
  total_missing_gen = nrow(missing_genera_list %>% filter(kingdom == "Plantae")),
  top_fam = major_gaps_plantae %>% slice(1)
)

# Generate Phylum-specific sentences
phylum_text <- missing_families_list %>%
  filter(kingdom == "Plantae") %>%
  group_by(phylum) %>%
  slice_max(total_known_species, n = 3) %>%
  summarise(
    examples = paste0(family, " (", comma(total_known_species), " spp.)", collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    sentence = paste0("- **", phylum, "**: Notable absences include ", examples, ".")
  ) %>%
  pull(sentence) %>%
  paste(collapse = "\n")

# Create the text content
manuscript_text <- paste0(
"# Unrepresented Plant Taxa: Gap Analysis\n\n",

"## Executive Summary\n",
"Our analysis identified **", comma(plant_stats$total_missing_fam), "** plant families and **", 
comma(plant_stats$total_missing_gen), "** plant genera that were completely absent from the literature analyzed. ",
"These 'taxonomic blind spots' represent significant opportunities for future research.\n\n",

"## Major Family Gaps\n",
"The single largest unrepresented plant family was **", plant_stats$top_fam$family, "**, ",
"which contains approximately ", comma(plant_stats$top_fam$total_known_species), " known species but had zero representation in our dataset. ",
"Other prominent families with zero detected species include:\n\n",
paste(paste0("- ", major_gaps_plantae$family[2:6], " (", comma(major_gaps_plantae$total_known_species[2:6]), " spp.)"), collapse = "\n"), 
"\n\n",

"## Breakdown by Phylum\n",
"The following list highlights the largest missing families within each major plant phylum:\n\n",
phylum_text, "\n\n",

"## Major Genus Gaps\n",
"Even within represented families, specific large genera were notably absent. The largest unrepresented genera include:\n",
paste(paste0("- *", missing_genera_list$genus[1:5], "* (", comma(missing_genera_list$total_known_species[1:5]), " spp.; Family: ", missing_genera_list$family[1:5], ")"), collapse = "\n"),
"\n"
)

# Write to file
write_lines(manuscript_text, "results/manuscript_tables/plant_gap_analysis.md")

message("\n--- SUCCESS ---")
message("Manuscript text generated: 'results/manuscript_tables/plant_gap_analysis.md'")
message("You can open this file in any text editor or RStudio to copy/paste into your manuscript.")