# Visualization Script for Taxa Detection Results
# This script creates visualizations from the output of extract_species_simple.R
# Updated to work with comprehensive_extraction_results.csv and filter extinct species

library(tidyverse)
library(rgbif)
library(scales)

# Custom theme for consistent visualization
custom_theme <- theme_bw(base_size = 14)
theme_set(custom_theme)

# Custom color palette
cus_pal <- c(
  "#A1C181",  # soft sage green — for plants
  "#619B8A",  # muted teal — evokes moss or lichens
  "#C97E7E",  # dusty rose — for fungi like Russula or Hygrophoropsis
  "#D9AE94"   # pale mushroom beige — for caps and forest floor tones
)

# Function to save plots with consistent dimensions and format
save_plot <- function(filename, plot, width = 12, height = 8, units = "in", ...) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename, plot, width = width, height = height, units = units, dpi = 300, ...)
  message("Saved plot to: ", filename)
}

# Function to load and process PBDB data for extinct species filtering
load_pbdb_extinct_species <- function(pbdb_file = "data/raw/pbdb_all.csv") {
  if (!file.exists(pbdb_file)) {
    warning("PBDB file not found at ", pbdb_file, ". Extinct species filtering will be skipped.")
    return(NULL)
  }
  
  message("Loading PBDB data to identify extinct species...")
  
  # Read PBDB data, skipping metadata rows
  pbdb_data <- read_csv(pbdb_file, skip = 15, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  # Extract extinct species names
  extinct_species <- pbdb_data %>%
    filter(is_extant == "extinct") %>%
    pull(accepted_name) %>%
    unique() %>%
    na.omit()
  
  message("Found ", length(extinct_species), " extinct species in PBDB dataset")
  
  return(extinct_species)
}

# Function to filter out extinct species from taxa results
filter_extinct_species <- function(taxa_results, extinct_species = NULL) {
  if (is.null(extinct_species)) {
    message("No extinct species list provided - returning original dataset")
    return(taxa_results)
  }
  
  initial_count <- nrow(taxa_results)
  
  # Filter out extinct species
  filtered_results <- taxa_results %>%
    filter(!canonicalName %in% extinct_species | is.na(canonicalName))
  
  final_count <- nrow(filtered_results)
  message("Filtered out ", initial_count - final_count, " records of extinct species")
  
  return(filtered_results)
}

# MAIN VISUALIZATION FUNCTIONS -----------------------------------------------

# Function to plot plant taxa diversity per phylum
plot_plant_diversity_per_phylum <- function(taxa_results, reference_species_df, 
                                           extinct_species = NULL, output_dir = "plots") {
  
  # Filter out extinct species from both datasets
  taxa_results_clean <- filter_extinct_species(taxa_results, extinct_species)
  reference_clean <- filter_extinct_species(reference_species_df, extinct_species)
  
  # Focus on plant data only
  plant_taxa <- taxa_results_clean %>%
    filter(kingdom == "Plantae", !is.na(phylum)) %>%
    # Remove duplicates and get unique taxa per level
    distinct(id, phylum, family, genus, canonicalName, predicted_label)
  
  plant_reference <- reference_clean %>%
    filter(kingdom == "Plantae", !is.na(phylum)) %>%
    distinct(phylum, family, genus, canonicalName)
  
  # Count species per phylum in our dataset
  species_per_phylum_dataset <- plant_taxa %>%
    filter(!is.na(canonicalName)) %>%
    group_by(phylum, predicted_label) %>%
    summarise(n_species_found = n_distinct(canonicalName), .groups = "drop") %>%
    complete(phylum, predicted_label = c("Presence", "Absence"), 
             fill = list(n_species_found = 0))
  
  # Count total species per phylum in reference
  species_per_phylum_reference <- plant_reference %>%
    filter(!is.na(canonicalName)) %>%
    group_by(phylum) %>%
    summarise(total_species = n_distinct(canonicalName), .groups = "drop")
  
  # Combine for comparison
  species_comparison <- species_per_phylum_dataset %>%
    group_by(phylum) %>%
    summarise(n_species_found = sum(n_species_found), .groups = "drop") %>%
    left_join(species_per_phylum_reference, by = "phylum") %>%
    mutate(
      coverage_pct = (n_species_found / total_species) * 100,
      n_species_missing = total_species - n_species_found
    ) %>%
    arrange(desc(n_species_found))
  
  # Create species diversity plot
  species_plot <- species_per_phylum_dataset %>%
    ggplot(aes(x = reorder(phylum, n_species_found), y = n_species_found, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Plant Species Diversity per Phylum in Dataset",
      subtitle = "Number of unique species detected in endophyte research abstracts",
      x = "Phylum",
      y = "Number of Species",
      fill = "Classification"
    ) +
    scale_fill_manual(values = cus_pal[1:2]) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text = element_text(size = 11)
    )
  
  save_plot(file.path(output_dir, "plant_species_per_phylum.png"), species_plot)
  
  # Coverage comparison plot
  coverage_long <- species_comparison %>%
    select(phylum, n_species_found, n_species_missing) %>%
    pivot_longer(cols = c(n_species_found, n_species_missing),
                 names_to = "status", values_to = "count") %>%
    mutate(status = recode(status,
                          n_species_found = "Found in Dataset",
                          n_species_missing = "Missing from Dataset"))
  
  coverage_plot <- coverage_long %>%
    ggplot(aes(x = reorder(phylum, count), y = count, fill = status)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Plant Species Coverage per Phylum",
      subtitle = "Comparison of species found vs. total known species",
      x = "Phylum",
      y = "Number of Species",
      fill = "Status"
    ) +
    scale_fill_manual(values = c("Found in Dataset" = cus_pal[1], 
                                "Missing from Dataset" = cus_pal[3])) +
    theme_minimal()
  
  save_plot(file.path(output_dir, "plant_species_coverage_per_phylum.png"), coverage_plot)
  
  # Family-level analysis
  families_per_phylum_dataset <- plant_taxa %>%
    filter(!is.na(family)) %>%
    group_by(phylum, predicted_label) %>%
    summarise(n_families_found = n_distinct(family), .groups = "drop") %>%
    complete(phylum, predicted_label = c("Presence", "Absence"), 
             fill = list(n_families_found = 0))
  
  families_per_phylum_reference <- plant_reference %>%
    filter(!is.na(family)) %>%
    group_by(phylum) %>%
    summarise(total_families = n_distinct(family), .groups = "drop")
  
  families_plot <- families_per_phylum_dataset %>%
    ggplot(aes(x = reorder(phylum, n_families_found), y = n_families_found, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Plant Families per Phylum in Dataset",
      subtitle = "Number of unique families detected in endophyte research abstracts",
      x = "Phylum",
      y = "Number of Families",
      fill = "Classification"
    ) +
    scale_fill_manual(values = cus_pal[1:2]) +
    theme_minimal()
  
  save_plot(file.path(output_dir, "plant_families_per_phylum.png"), families_plot)
  
  # Genus-level analysis
  genera_per_phylum_dataset <- plant_taxa %>%
    filter(!is.na(genus)) %>%
    group_by(phylum, predicted_label) %>%
    summarise(n_genera_found = n_distinct(genus), .groups = "drop") %>%
    complete(phylum, predicted_label = c("Presence", "Absence"), 
             fill = list(n_genera_found = 0))
  
  genera_per_phylum_reference <- plant_reference %>%
    filter(!is.na(genus)) %>%
    group_by(phylum) %>%
    summarise(total_genera = n_distinct(genus), .groups = "drop")
  
  genera_plot <- genera_per_phylum_dataset %>%
    ggplot(aes(x = reorder(phylum, n_genera_found), y = n_genera_found, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Plant Genera per Phylum in Dataset",
      subtitle = "Number of unique genera detected in endophyte research abstracts",
      x = "Phylum",
      y = "Number of Genera",
      fill = "Classification"
    ) +
    scale_fill_manual(values = cus_pal[1:2]) +
    theme_minimal()
  
  save_plot(file.path(output_dir, "plant_genera_per_phylum.png"), genera_plot)
  
  # Save summary statistics
  summary_stats <- list(
    species_summary = species_comparison,
    families_dataset = families_per_phylum_dataset %>% 
      group_by(phylum) %>% 
      summarise(total_families_found = sum(n_families_found), .groups = "drop") %>%
      left_join(families_per_phylum_reference, by = "phylum"),
    genera_dataset = genera_per_phylum_dataset %>% 
      group_by(phylum) %>% 
      summarise(total_genera_found = sum(n_genera_found), .groups = "drop") %>%
      left_join(genera_per_phylum_reference, by = "phylum")
  )
  
  write_csv(summary_stats$species_summary, file.path(output_dir, "species_per_phylum_summary.csv"))
  write_csv(summary_stats$families_dataset, file.path(output_dir, "families_per_phylum_summary.csv"))
  write_csv(summary_stats$genera_dataset, file.path(output_dir, "genera_per_phylum_summary.csv"))
  
  return(list(
    species_plot = species_plot,
    coverage_plot = coverage_plot,
    families_plot = families_plot,
    genera_plot = genera_plot,
    summary_stats = summary_stats
  ))
}

# Function to plot research methods summary
plot_research_methods_summary <- function(taxa_results, output_dir = "plots") {
  if (!"methods_summary" %in% colnames(taxa_results)) {
    message("No methods_summary column found - skipping methods visualization")
    return(NULL)
  }
  
  # Extract methods data
  methods_data <- taxa_results %>%
    filter(!is.na(methods_summary)) %>%
    distinct(id, predicted_label, molecular_methods, culture_based_methods, microscopy_methods) %>%
    pivot_longer(cols = ends_with("_methods"), 
                 names_to = "method_type", 
                 values_to = "detected") %>%
    mutate(
      method_type = str_remove(method_type, "_methods"),
      method_type = str_to_title(str_replace_all(method_type, "_", " "))
    ) %>%
    filter(detected == TRUE) %>%
    count(method_type, predicted_label, name = "n_abstracts")
  
  methods_plot <- methods_data %>%
    ggplot(aes(x = method_type, y = n_abstracts, fill = predicted_label)) +
    geom_col(position = "dodge") +
    labs(
      title = "Research Methods Used in Endophyte Studies",
      x = "Method Type",
      y = "Number of Abstracts",
      fill = "Classification"
    ) +
    scale_fill_manual(values = cus_pal[1:2]) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  save_plot(file.path(output_dir, "research_methods_summary.png"), methods_plot)
  
  return(methods_plot)
}

# Function to plot geographic distribution
plot_geographic_distribution <- function(taxa_results, output_dir = "plots") {
  if (!"countries_detected" %in% colnames(taxa_results)) {
    message("No countries_detected column found - skipping geographic visualization")
    return(NULL)
  }
  
  # Extract geographic data
  geo_data <- taxa_results %>%
    filter(!is.na(countries_detected)) %>%
    distinct(id, predicted_label, global_north_countries, global_south_countries) %>%
    mutate(
      has_global_north = !is.na(global_north_countries) & global_north_countries > 0,
      has_global_south = !is.na(global_south_countries) & global_south_countries > 0,
      region = case_when(
        has_global_north & has_global_south ~ "Both",
        has_global_north ~ "Global North",
        has_global_south ~ "Global South",
        TRUE ~ "Unknown"
      )
    ) %>%
    count(region, predicted_label, name = "n_abstracts")
  
  geo_plot <- geo_data %>%
    ggplot(aes(x = region, y = n_abstracts, fill = predicted_label)) +
    geom_col(position = "dodge") +
    labs(
      title = "Geographic Distribution of Endophyte Research",
      x = "Region",
      y = "Number of Abstracts",
      fill = "Classification"
    ) +
    scale_fill_manual(values = cus_pal[1:2]) +
    theme_minimal()
  
  save_plot(file.path(output_dir, "geographic_distribution.png"), geo_plot)
  
  return(geo_plot)
}

# Main function to run all visualizations
visualize_comprehensive_results <- function(results_file = "results/comprehensive_extraction_results.csv",
                                          species_file = "models/species.rds", 
                                          pbdb_file = "data/raw/pbdb_all.csv",
                                          output_dir = "plots") {
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Load data
  message("Loading comprehensive extraction results from ", results_file)
  taxa_results <- read_csv(results_file, show_col_types = FALSE)
  
  message("Loading reference species data from ", species_file)
  if (file.exists(species_file)) {
    reference_species <- readRDS(species_file)
  } else {
    stop("Species reference file not found at ", species_file)
  }
  
  # Load extinct species list
  extinct_species <- load_pbdb_extinct_species(pbdb_file)
  
  # Run main visualizations
  message("Creating plant diversity visualizations...")
  diversity_plots <- plot_plant_diversity_per_phylum(taxa_results, reference_species, 
                                                    extinct_species, output_dir)
  
  message("Creating research methods visualizations...")
  methods_plot <- plot_research_methods_summary(taxa_results, output_dir)
  
  message("Creating geographic distribution visualizations...")
  geo_plot <- plot_geographic_distribution(taxa_results, output_dir)
  
  # Create summary report
  summary_report <- tibble(
    visualization_type = c("Plant Species per Phylum", "Plant Families per Phylum", 
                          "Plant Genera per Phylum", "Species Coverage", 
                          "Research Methods", "Geographic Distribution"),
    file_created = c("plant_species_per_phylum.png", "plant_families_per_phylum.png",
                    "plant_genera_per_phylum.png", "plant_species_coverage_per_phylum.png",
                    "research_methods_summary.png", "geographic_distribution.png"),
    status = "completed"
  )
  
  write_csv(summary_report, file.path(output_dir, "visualization_summary.csv"))
  
  message("All visualizations complete! Results saved to ", output_dir)
  message("Key outputs:")
  message("  - Plant species diversity per phylum")
  message("  - Plant families diversity per phylum") 
  message("  - Plant genera diversity per phylum")
  message("  - Species coverage comparison with reference dataset")
  message("  - Research methods summary")
  message("  - Geographic distribution analysis")
  
  return(list(
    diversity_plots = diversity_plots,
    methods_plot = methods_plot,
    geo_plot = geo_plot,
    summary_report = summary_report
  ))
}
# Example usage
if (interactive()) {
  message("To create visualizations from comprehensive extraction results:")
  message('visualize_comprehensive_results()')
  message('# or with custom paths:')
  message('visualize_comprehensive_results(')
  message('  results_file = "results/comprehensive_extraction_results.csv",')
  message('  species_file = "models/species.rds",')
  message('  pbdb_file = "data/raw/pbdb_all.csv",')
  message('  output_dir = "plots"')
  message(')')
}
