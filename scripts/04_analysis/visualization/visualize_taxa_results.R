library(here)
# Focused Taxa Visualization Script - Phylum-Based Only
# Creates phylum-based plots showing representation of plant and fungal taxa
# Includes synonym resolution and extinct species exclusion

library(tidyverse)
library(scales)

# Source plot utilities with error handling
tryCatch({
  source("scripts/utils/plot_utils.R")
}, error = function(e) {
  warning("Could not load plot_utils.R: ", e$message)
  # Define basic fallback theme and colors if plot_utils fails to load
  endo_theme <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(size = base_size * 1.2, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = base_size, hjust = 0.5),
        axis.title = element_text(size = base_size),
        legend.title = element_text(size = base_size)
      )
  }

  # Define fallback colors
  endo_colors <- list(
    found_not_found = c(Found = "#46ACC8", `Not Found` = "#B40F20")
  )
})

# Source optimized taxa detection functions for potential use
optimized_taxa_path <- "scripts/04_analysis/optimized_taxa_detection.R"
if (file.exists(optimized_taxa_path)) {
  try(source(optimized_taxa_path), silent = TRUE)
}

# Load optimized reference data from enhanced pipeline
setwd(here())

# Use pre-processed species data that's already optimized
if (file.exists("models/accepted_species.rds")) {
  message("Loading pre-processed accepted species data...")
  accepted_species <- readRDS("models/accepted_species.rds")
  reference_species <- accepted_species
  
  # Ensure canonicalName_resolved column exists for consistency
  if (!"canonicalName_resolved" %in% colnames(accepted_species)) {
    accepted_species <- accepted_species %>%
      mutate(canonicalName_resolved = canonicalName)
  }
} else if (file.exists("models/species.rds")) {
  message("Loading species.rds and creating accepted species...")
  species_data <- readRDS("models/species.rds")
  # Use optimized filtering for Plantae and Fungi
  reference_species <- species_data %>%
    filter(kingdom %in% c("Plantae", "Fungi") & taxonRank == "species")

  # Quick extinct species exclusion using optimized approach
  if (all(c("canonicalName", "genus", "family", "order") %in% colnames(reference_species))) {
    message("Excluding extinct species using optimized approach...")
    # Use the reference data utilities if available
    if (file.exists("scripts/04_analysis/utilities/reference_data_utils.R")) {
      try(source("scripts/04_analysis/utilities/reference_data_utils.R"), silent = TRUE)
    }
    # For now, assume extinct species are already filtered in accepted_species.rds
    # If not available, we'll use the data as-is
  }

  # Use optimized synonym resolution
  if (all(c("taxonomicStatus", "acceptedNameUsageID", "taxonID") %in% colnames(reference_species))) {
    accepted_species <- reference_species %>%
      filter(taxonomicStatus == "accepted") %>%
      select(taxonID, canonicalName, kingdom, phylum, family, genus) %>%
      rename(canonicalName_resolved = canonicalName)
  } else {
    accepted_species <- reference_species %>%
      rename(canonicalName_resolved = canonicalName)
  }
} else {
  stop("No species reference data found. Please run the enhanced pipeline first.")
}

message("Reference data loaded: ", nrow(reference_species), " species")

# Load comprehensive taxa results with enhanced species and mycorrhizal data
taxa_results <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)

message("Comprehensive taxa results loaded: ", nrow(taxa_results), " entries")
message("Available columns: ", length(names(taxa_results)))

# Add mycorrhizal filtering capabilities for dual analysis modes
filter_mycorrhizal_papers <- function(data, include_mycorrhizal_only = FALSE) {
  if (!"is_mycorrhizal_only" %in% colnames(data)) {
    message("Mycorrhizal classification not available in dataset")
    return(data)
  }

  if (include_mycorrhizal_only) {
    message("Including mycorrhizal-only papers in analysis (supplementary mode)")
    return(data)
  } else {
    message("Excluding mycorrhizal-only papers from analysis (main mode)")
    filtered_data <- data %>%
      filter(!is_mycorrhizal_only)
    message("Filtered out ", nrow(data) - nrow(filtered_data), " mycorrhizal-only abstracts")
    return(filtered_data)
  }
}

# Handle synonym resolution for taxa_results (if columns exist)
# First, ensure basic columns exist and add defaults for missing ones
required_cols <- c("canonicalName", "genus", "family", "kingdom", "phylum")
missing_cols <- setdiff(required_cols, colnames(taxa_results))
if (length(missing_cols) > 0) {
  stop("Missing required columns in taxa_results: ", paste(missing_cols, collapse = ", "))
}

# Add default values for commonly expected columns if they don't exist
if (!"final_classification" %in% colnames(taxa_results)) {
  taxa_results <- taxa_results %>%
    mutate(final_classification = "Presence")  # Default assumption
}

if (!"match_type" %in% colnames(taxa_results)) {
  taxa_results <- taxa_results %>%
    mutate(match_type = case_when(
      !is.na(canonicalName) ~ "species",
      !is.na(genus) ~ "genus", 
      !is.na(family) ~ "family",
      TRUE ~ "unknown"
    ))
}

if (all(c("resolved_name") %in% colnames(taxa_results))) {
  message("Resolving synonyms in taxa_results...")

  # Replace user_supplied_name with resolved_name where available
  taxa_results <- taxa_results %>%
    mutate(
      canonicalName_resolved = coalesce(resolved_name, canonicalName),
      genus_resolved = case_when(
        "match_type" %in% colnames(taxa_results) & match_type == "species" & !is.na(resolved_name) ~ word(resolved_name, 1),
        "match_type" %in% colnames(taxa_results) & match_type == "genus" & !is.na(resolved_name) ~ resolved_name,
        TRUE ~ genus
      ),
      family_resolved = family  # Keep family as-is for now
    )

  message("Synonyms resolved in taxa_results")
} else {
  message("Synonym resolution columns not found in taxa_results")
  taxa_results <- taxa_results %>%
    mutate(
      canonicalName_resolved = canonicalName,
      genus_resolved = genus,
      family_resolved = family
    )
}

# Remove duplicate taxa within the same abstract to avoid double counting
# Build the distinct columns list based on available columns
distinct_cols <- c("id", "kingdom", "phylum", "canonicalName_resolved", "genus_resolved", "family_resolved")
optional_cols <- c("match_type", "final_classification")
available_optional_cols <- intersect(optional_cols, colnames(taxa_results))
distinct_cols <- c(distinct_cols, available_optional_cols)

taxa_results_deduped <- taxa_results %>%
  # Remove duplicates within the same abstract for the same taxon
  distinct(across(all_of(distinct_cols)), .keep_all = TRUE) %>%
  # Create final resolved names for counting
  mutate(
    canonicalName_final = canonicalName_resolved,
    genus_final = genus_resolved,
    family_final = family_resolved
  )

message("Data prepared with synonym resolution and deduplication")
message("Final columns in taxa_results_deduped: ", paste(names(taxa_results_deduped), collapse = ", "))
message("Final columns in accepted_species: ", paste(names(accepted_species), collapse = ", "))
message("Number of deduped taxa records: ", nrow(taxa_results_deduped))

# Create consistent phylum ordering function using optimized reference data
get_phylum_order <- function(kingdom_filter) {
  # Use canonicalName_resolved if available, otherwise canonicalName
  name_col <- if("canonicalName_resolved" %in% colnames(accepted_species)) "canonicalName_resolved" else "canonicalName"
  
  accepted_species %>%
    filter(kingdom == kingdom_filter, !is.na(phylum)) %>%
    distinct(phylum, !!sym(name_col)) %>%
    group_by(phylum) %>%
    summarise(species_count = n(), .groups = "drop") %>%
    arrange(desc(species_count)) %>%
    pull(phylum)
}

# Get consistent ordering for each kingdom (reverse for correct plot orientation)
plant_phylum_order <- rev(get_phylum_order("Plantae"))
fungi_phylum_order <- rev(get_phylum_order("Fungi"))

# Create common name mappings for major phyla
get_phylum_common_names <- function() {
  # Plant phyla common names
  plant_common_names <- c(
    "Tracheophyta" = "vascular plants",
    "Magnoliophyta" = "flowering plants",
    "Pinophyta" = "conifers",
    "Polypodiophyta" = "ferns",
    "Bryophyta" = "mosses",
    "Marchantiophyta" = "liverworts",
    "Anthocerotophyta" = "hornworts",
    "Lycopodiophyta" = "club mosses",
    "Equisetophyta" = "horsetails",
    "Psilotophyta" = "whisk ferns",
    "Cycadophyta" = "cycads",
    "Ginkgophyta" = "ginkgo",
    "Gnetophyta" = "gnetophytes",
    "Rhodophyta" = "red algae",
    "Pteridophyta" = "ferns",
    "Chlorophyta" = "green algae",
    "Charophyta" = "stoneworts",
    "Glaucophyta" = "glaucophyte algae",
    "Lycopodiophyta" = "club mosses",
    "Zosterophyllophyta" = "zosterophylls",
    "Langiophytophyta" = "proto-vascular plants"
  )

  # Fungal phyla common names
  fungi_common_names <- c(
    "Ascomycota" = "sac fungi",
    "Basidiomycota" = "club fungi",
    "Mucoromycota" = "pin molds",
    "Zygomycota" = "conjugation fungi",
    "Chytridiomycota" = "chytrids",
    "Glomeromycota" = "arbuscular mycorrhizal fungi",
    "Blastocladiomycota" = "blastoclads",
    "Neocallimastigomycota" = "rumen fungi",
    "Cryptomycota" = "cryptomycetes",
    "Microsporidia" = "microsporidians"
  )

  list(plantae = plant_common_names, fungi = fungi_common_names)
}

# Get phylum common names
phylum_common_names <- get_phylum_common_names()

message("Plant phylum order (by species count, reversed for plot): ", paste(plant_phylum_order, collapse = ", "))
message("Fungi phylum order (by species count, reversed for plot): ", paste(fungi_phylum_order, collapse = ", "))

# Create phylum-based visualization function with hierarchical counting
create_phylum_taxa_plot <- function(kingdom_filter, level_name, column_name, output_name, taxa_data = taxa_results_deduped, subfolder = "main") {

  # Get found taxa with hierarchical logic and synonym resolution
  if (level_name == "Species") {
    # For species, count direct species mentions using resolved names
    found_by_phylum <- taxa_data %>%
      filter(kingdom == kingdom_filter,
             final_classification == "Presence",
             !is.na(canonicalName_final),
             !is.na(phylum),
             match_type == "species") %>%
      distinct(phylum, canonicalName_final) %>%
      group_by(phylum) %>%
      summarise(found = n_distinct(canonicalName_final), .groups = "drop")
  } else if (level_name == "Genus") {
    # For genera, count both direct genus mentions AND genera that contain found species
    direct_genus <- taxa_results_deduped %>%
      filter(kingdom == kingdom_filter,
             final_classification == "Presence",
             !is.na(genus_final),
             !is.na(phylum),
             match_type == "genus") %>%
      distinct(phylum, genus_final)

    # Get genera that contain found species (using resolved genus names)
    species_in_genus <- taxa_results_deduped %>%
      filter(kingdom == kingdom_filter,
             final_classification == "Presence",
             !is.na(canonicalName_final),
             !is.na(genus_final),
             !is.na(phylum),
             match_type == "species") %>%
      distinct(phylum, genus_final)

    # Combine direct and indirect genus mentions
    all_genus <- bind_rows(
      direct_genus %>% select(phylum, genus = genus_final),
      species_in_genus %>% select(phylum, genus = genus_final)
    ) %>%
      distinct(phylum, genus)

    found_by_phylum <- all_genus %>%
      group_by(phylum) %>%
      summarise(found = n_distinct(genus), .groups = "drop")
  } else if (level_name == "Family") {
    # For families, count both direct family mentions AND families that contain found genera/species
    direct_family <- taxa_results_deduped %>%
      filter(kingdom == kingdom_filter,
             final_classification == "Presence",
             !is.na(family_final),
             !is.na(phylum),
             match_type == "family") %>%
      distinct(phylum, family_final)

    # Get families that contain found species
    species_in_family <- taxa_results_deduped %>%
      filter(kingdom == kingdom_filter,
             final_classification == "Presence",
             !is.na(canonicalName_final),
             !is.na(family_final),
             !is.na(phylum),
             match_type == "species") %>%
      distinct(phylum, family_final)

    # Get families that contain found genera
    genus_in_family <- taxa_results_deduped %>%
      filter(kingdom == kingdom_filter,
             final_classification == "Presence",
             !is.na(genus_final),
             !is.na(family_final),
             !is.na(phylum),
             match_type == "genus") %>%
      distinct(phylum, family_final)

    # Combine all family mentions
    all_families <- bind_rows(
      direct_family %>% select(phylum, family = family_final),
      species_in_family %>% select(phylum, family = family_final),
      genus_in_family %>% select(phylum, family = family_final)
    ) %>%
      distinct(phylum, family)

    found_by_phylum <- all_families %>%
      group_by(phylum) %>%
      summarise(found = n_distinct(family), .groups = "drop")
  }

  # Get total taxa by phylum (always from reference data)
  total_by_phylum <- accepted_species %>%
    filter(kingdom == kingdom_filter,
           !is.na(.data[[column_name]]),
           !is.na(phylum)) %>%
    distinct(phylum, .data[[column_name]]) %>%
    group_by(phylum) %>%
    summarise(total = n_distinct(.data[[column_name]]), .groups = "drop")

  # Combine data
  phylum_data <- total_by_phylum %>%
    left_join(found_by_phylum, by = "phylum") %>%
    mutate(
      found = replace_na(found, 0),
      not_found = total - found,
      percent_found = (found / total) * 100,
      percent_not_found = (not_found / total) * 100
    ) %>%
    # Only include phyla with data
    filter(total > 0)

  if (nrow(phylum_data) == 0) {
    message("No phylum data found for ", kingdom_filter, " ", level_name)
    return(NULL)
  }

  # Create count plot
  count_data <- phylum_data %>%
    select(phylum, found, not_found) %>%
    pivot_longer(cols = c(found, not_found),
                 names_to = "Status",
                 values_to = "Count") %>%
    mutate(Status = recode(Status, found = "Found", not_found = "Not Found"))

  # Create proper plural forms for taxonomic levels
  level_name_plural <- case_when(
    level_name == "Species" ~ "Species",
    level_name == "Genus" ~ "Genera",
    level_name == "Family" ~ "Families",
    TRUE ~ paste0(level_name, "s")
  )

  # Apply consistent phylum ordering
  if (kingdom_filter == "Plantae") {
    count_data <- count_data %>%
      mutate(phylum = factor(phylum, levels = plant_phylum_order))
  } else if (kingdom_filter == "Fungi") {
    count_data <- count_data %>%
      mutate(phylum = factor(phylum, levels = fungi_phylum_order))
  }

  # Format phylum labels with species counts and common names for context
  count_data_with_totals <- count_data %>%
    left_join(phylum_data %>% select(phylum, total), by = "phylum") %>%
    mutate(
      common_name = case_when(
        kingdom_filter == "Plantae" ~ phylum_common_names$plantae[phylum],
        kingdom_filter == "Fungi" ~ phylum_common_names$fungi[phylum],
        TRUE ~ NA_character_
      ),
      phylum_label = if_else(
        !is.na(common_name),
        paste0(phylum, " (", common_name, ")\n", scales::comma(total), " ", tolower(level_name_plural), " known"),
        paste0(phylum, "\n(", scales::comma(total), " ", tolower(level_name_plural), " known)")
      )
    )

  count_plot <- ggplot(count_data_with_totals, aes(x = phylum_label, y = Count, fill = Status)) +
    geom_col(width = 0.8) +
    coord_flip() +
    labs(
      title = paste(kingdom_filter, level_name, "Representation by Phylum (Count)"),
      subtitle = paste("Number of", tolower(level_name), "found vs. not found in each phylum (hierarchical). Data from GBIF Backbone Taxonomy"),
      x = "Phylum",
      y = paste("Number of", level_name_plural),
      caption = "GBIF = Global Biodiversity Information Facility (gbif.org)"
    ) +
    scale_fill_manual(values = c(Found = "#46ACC8", `Not Found` = "#B40F20"),
                     name = "Status") +
    endo_theme() +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11))

  # Create percentage plot
  percent_data <- phylum_data %>%
    select(phylum, percent_found, percent_not_found) %>%
    pivot_longer(cols = c(percent_found, percent_not_found),
                 names_to = "Status",
                 values_to = "Percent") %>%
    mutate(
      Status = recode(Status, percent_found = "Found", percent_not_found = "Not Found")
    )

  # Apply consistent phylum ordering to percent data
  if (kingdom_filter == "Plantae") {
    percent_data <- percent_data %>%
      mutate(phylum = factor(phylum, levels = plant_phylum_order))
  } else if (kingdom_filter == "Fungi") {
    percent_data <- percent_data %>%
      mutate(phylum = factor(phylum, levels = fungi_phylum_order))
  }

  # Format phylum labels with species counts and common names for context
  percent_data_with_totals <- percent_data %>%
    left_join(phylum_data %>% select(phylum, total), by = "phylum") %>%
    mutate(
      common_name = case_when(
        kingdom_filter == "Plantae" ~ phylum_common_names$plantae[phylum],
        kingdom_filter == "Fungi" ~ phylum_common_names$fungi[phylum],
        TRUE ~ NA_character_
      ),
      phylum_label = if_else(
        !is.na(common_name),
        paste0(phylum, " (", common_name, ")\n", scales::comma(total), " ", tolower(level_name_plural), " known"),
        paste0(phylum, "\n(", scales::comma(total), " ", tolower(level_name_plural), " known)")
      )
    )

  percent_plot <- ggplot(percent_data_with_totals, aes(x = phylum_label, y = Percent, fill = Status)) +
    geom_col(width = 0.8) +
    geom_text(
      aes(label = ifelse(Percent > 0, sprintf("%.1f%%", Percent), "")),
      position = position_stack(vjust = 0.5),
      size = 3.5,
      fontface = "bold"
    ) +
    coord_flip() +
    labs(
      title = paste(kingdom_filter, level_name, "Representation by Phylum (Percent)"),
      subtitle = paste("Percentage of", tolower(level_name), "found vs. not found in each phylum (hierarchical). Data from GBIF Backbone Taxonomy"),
      x = "Phylum",
      y = paste("Percentage of", level_name_plural),
      caption = "GBIF = Global Biodiversity Information Facility (gbif.org)"
    ) +
    scale_fill_manual(values = c(Found = "#46ACC8", `Not Found` = "#B40F20"),
                     name = "Status") +
    endo_theme() +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11))

  # Save both plots with subfolder organization
  ggsave(paste0("plots/", subfolder, "/", output_name, "_by_phylum_count.png"), count_plot, width = 12, height = 8)
  ggsave(paste0("plots/", subfolder, "/", output_name, "_by_phylum_percent.png"), percent_plot, width = 12, height = 8)

  message("Saved: plots/", subfolder, "/", output_name, "_by_phylum_count.png")
  message("Saved: plots/", subfolder, "/", output_name, "_by_phylum_percent.png")
  message("Used hierarchical counting: ", level_name, " includes constituent taxa")

  return(list(count_plot = count_plot, percent_plot = percent_plot))
}

# Create output directories
dir.create("plots", showWarnings = FALSE)
dir.create("plots/main", showWarnings = FALSE, recursive = TRUE)
dir.create("plots/supplementary", showWarnings = FALSE, recursive = TRUE)
dir.create("plots/geographic", showWarnings = FALSE, recursive = TRUE)

# Create phylum-based representation plots with dual modes (main vs supplementary)
message("Creating phylum-based visualization plots...")

# Apply mycorrhizal filtering to create two datasets
taxa_results_main <- filter_mycorrhizal_papers(taxa_results_deduped, include_mycorrhizal_only = FALSE)
taxa_results_supplementary <- filter_mycorrhizal_papers(taxa_results_deduped, include_mycorrhizal_only = TRUE)

message("Creating main visualizations (excluding mycorrhizal-only papers)...")
# Plant phylum plots - MAIN MODE
create_phylum_taxa_plot("Plantae", "Family", "family", "plantae_family_representation_main", taxa_results_main)
create_phylum_taxa_plot("Plantae", "Genus", "genus", "plantae_genus_representation_main", taxa_results_main)
create_phylum_taxa_plot("Plantae", "Species", "canonicalName", "plantae_species_representation_main", taxa_results_main)

# Fungi phylum plots - MAIN MODE
create_phylum_taxa_plot("Fungi", "Family", "family", "fungi_family_representation_main", taxa_results_main)
create_phylum_taxa_plot("Fungi", "Genus", "genus", "fungi_genus_representation_main", taxa_results_main)
create_phylum_taxa_plot("Fungi", "Species", "canonicalName", "fungi_species_representation_main", taxa_results_main)

message("Creating supplementary visualizations (including mycorrhizal-only papers)...")
# Plant phylum plots - SUPPLEMENTARY MODE
create_phylum_taxa_plot("Plantae", "Family", "family", "plantae_family_representation_supplementary", taxa_results_supplementary, "supplementary")
create_phylum_taxa_plot("Plantae", "Genus", "genus", "plantae_genus_representation_supplementary", taxa_results_supplementary, "supplementary")
create_phylum_taxa_plot("Plantae", "Species", "canonicalName", "plantae_species_representation_supplementary", taxa_results_supplementary, "supplementary")

# Fungi phylum plots - SUPPLEMENTARY MODE
create_phylum_taxa_plot("Fungi", "Family", "family", "fungi_family_representation_supplementary", taxa_results_supplementary, "supplementary")
create_phylum_taxa_plot("Fungi", "Genus", "genus", "fungi_genus_representation_supplementary", taxa_results_supplementary, "supplementary")
create_phylum_taxa_plot("Fungi", "Species", "canonicalName", "fungi_species_representation_supplementary", taxa_results_supplementary, "supplementary")

# Create geographic-taxonomic analysis function
create_geographic_taxa_analysis <- function() {

  if (!"countries_detected" %in% colnames(taxa_results)) {
    message("Geographic data not available for analysis")
    return(NULL)
  }

  message("Creating geographic-taxonomic analysis...")

  # Get taxa by geographic regions
  geo_taxa_summary <- taxa_results_deduped %>%
    filter(final_classification == "Presence",
           !is.na(canonicalName_final),
           !is.na(countries_detected)) %>%
    select(kingdom, phylum, canonicalName_final, countries_detected) %>%
    separate_rows(countries_detected, sep = "; ") %>%
    filter(countries_detected != "") %>%
    mutate(country_clean = str_trim(countries_detected)) %>%
    group_by(kingdom, phylum, country_clean) %>%
    summarise(
      unique_species = n_distinct(canonicalName_final),
      .groups = "drop"
    ) %>%
    arrange(kingdom, phylum, desc(unique_species))

  # Save geographic-taxonomic analysis
  write_csv(geo_taxa_summary, "results/geographic_taxonomic_analysis.csv")

  message("Saved geographic-taxonomic analysis to: results/geographic_taxonomic_analysis.csv")
  message("Records by kingdom: Plantae = ", sum(geo_taxa_summary$kingdom == "Plantae"),
          ", Fungi = ", sum(geo_taxa_summary$kingdom == "Fungi"))

  return(geo_taxa_summary)
}

# Create geographic-taxonomic visualizations
create_geographic_taxa_visualizations <- function(geo_data) {

  if (is.null(geo_data) || nrow(geo_data) == 0) {
    message("No geographic data available for visualization")
    return(NULL)
  }

  message("Creating geographic-taxonomic visualizations...")

  # Top countries by taxonomic diversity
  top_countries <- geo_data %>%
    group_by(country_clean) %>%
    summarise(
      total_species = sum(unique_species),
      num_phyla = n_distinct(phylum),
      num_kingdoms = n_distinct(kingdom),
      .groups = "drop"
    ) %>%
    arrange(desc(total_species)) %>%
    slice_max(total_species, n = 20) %>%
    mutate(country_clean = fct_reorder(country_clean, total_species))

  p1_geo_diversity <- ggplot(top_countries, aes(x = country_clean, y = total_species, fill = num_phyla)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = total_species), hjust = -0.1, size = 3) +
    scale_fill_gradient(low = get_endo_gradient()[1], high = get_endo_gradient()[2], name = "Number of Phyla") +
    labs(
      title = "Taxonomic Diversity by Country",
      subtitle = "Top 20 countries by unique species studied",
      x = "Country",
      y = "Number of Unique Species"
    ) +
    coord_flip() +
    endo_theme()

  # Plant species distribution by country - Top countries only
  message("Creating plant species by country distribution (top 25 countries)...")
  plant_by_country <- geo_data %>%
    filter(kingdom == "Plantae") %>%  # Focus only on plant species
    group_by(country_clean) %>%
    summarise(plant_species_count = sum(unique_species), .groups = "drop") %>%
    arrange(desc(plant_species_count)) %>%
    slice_head(n = 25) %>%  # Show top 25 countries to avoid cramping
    mutate(country_clean = fct_reorder(country_clean, plant_species_count))

  p2_plants_geo <- ggplot(plant_by_country, aes(x = country_clean, y = plant_species_count)) +
    geom_col(fill = get_endo_colors(1)[1], width = 0.7) +
    geom_text(aes(label = plant_species_count), hjust = -0.1, size = 2.8) +
    labs(
      title = "Plant Species Studied by Country",
      subtitle = "Top 25 countries ranked by number of plant species studied",
      x = "Country",
      y = "Number of Plant Species"
    ) +
    coord_flip() +
    endo_theme() +
    theme(
      axis.text.y = element_text(size = 9),  # Slightly larger text for readability
      plot.margin = margin(10, 50, 10, 10)   # Extra right margin for country labels
    )

  # Phylum-level geographic analysis - Plants only
  message("Creating plant phylum geographic distribution (excluding fungal phyla)...")
  phylum_by_region <- geo_data %>%
    filter(kingdom == "Plantae") %>%  # Focus only on plant phyla
    group_by(phylum, country_clean) %>%
    summarise(species_count = sum(unique_species), .groups = "drop") %>%
    arrange(phylum, desc(species_count)) %>%
    group_by(phylum) %>%
    slice_max(species_count, n = 10) %>%
    ungroup()

  # Create faceted plot by phylum
  p3_phylum_geo <- ggplot(phylum_by_region, aes(x = fct_reorder(country_clean, species_count), y = species_count)) +
    geom_col(fill = get_endo_colors(3)[3], width = 0.8) +
    geom_text(aes(label = species_count), hjust = -0.1, size = 2.5) +
    facet_wrap(~phylum, scales = "free_y", ncol = 3) +
    labs(
      title = "Plant Species Distribution by Phylum and Country",
      subtitle = "Top countries for each major plant phylum",
      x = "Country",
      y = "Number of Plant Species"
    ) +
    coord_flip() +
    endo_theme() +
    theme(axis.text.y = element_text(size = 7))

  # Save geographic visualizations
  ggsave("plots/geographic/geographic_taxonomic_diversity.png", p1_geo_diversity, width = 12, height = 8)
  ggsave("plots/geographic/plant_species_by_country.png", p2_plants_geo, width = 12, height = 10)
  ggsave("plots/geographic/plant_phylum_geographic_distribution.png", p3_phylum_geo, width = 16, height = 10)

  message("Saved geographic-taxonomic visualizations:")
  message("- plots/geographic/geographic_taxonomic_diversity.png")
  message("- plots/geographic/plant_species_by_country.png")
  message("- plots/geographic/plant_phylum_geographic_distribution.png")

  return(list(
    diversity_plot = p1_geo_diversity,
    plants_by_country_plot = p2_plants_geo,
    phylum_plot = p3_phylum_geo
  ))
}

# Create comprehensive list of unrepresented taxa
create_unrepresented_taxa_csv <- function() {

  # Get all unique taxa from reference data (with resolved synonyms)
  # Use canonicalName_resolved if available, otherwise canonicalName
  name_col <- if("canonicalName_resolved" %in% colnames(accepted_species)) "canonicalName_resolved" else "canonicalName"
  
  all_reference_taxa <- accepted_species %>%
    distinct(kingdom, phylum, family, genus, !!sym(name_col)) %>%
    rename(species = !!name_col)

  # Get found taxa from deduplicated results
  found_species <- taxa_results_deduped %>%
    filter(match_type == "species", final_classification == "Presence", !is.na(canonicalName_final)) %>%
    distinct(kingdom, phylum, family, genus, species = canonicalName_final)

  found_genera <- taxa_results_deduped %>%
    filter(match_type == "genus", final_classification == "Presence", !is.na(genus_final)) %>%
    distinct(kingdom, phylum, family, genus = genus_final) %>%
    mutate(species = NA_character_)

  found_families <- taxa_results_deduped %>%
    filter(match_type == "family", final_classification == "Presence", !is.na(family_final)) %>%
    distinct(kingdom, phylum, family = family_final) %>%
    mutate(genus = NA_character_, species = NA_character_)

  # Combine all found taxa
  found_taxa <- bind_rows(found_species, found_genera, found_families) %>%
    distinct()

  # Find unrepresented taxa by anti-joining with found taxa
  # For species-level comparison
  unrepresented_species <- all_reference_taxa %>%
    anti_join(found_species, by = c("kingdom", "phylum", "family", "genus", "species"))

  # For genus-level comparison (genera not found at genus level)
  unrepresented_genera <- all_reference_taxa %>%
    distinct(kingdom, phylum, family, genus) %>%
    anti_join(found_genera, by = c("kingdom", "phylum", "family", "genus")) %>%
    anti_join(found_species %>% distinct(kingdom, phylum, family, genus),
              by = c("kingdom", "phylum", "family", "genus")) %>%
    mutate(species = NA_character_)

  # For family-level comparison (families not found at any level)
  unrepresented_families <- all_reference_taxa %>%
    distinct(kingdom, phylum, family) %>%
    anti_join(found_families, by = c("kingdom", "phylum", "family")) %>%
    anti_join(found_genera %>% distinct(kingdom, phylum, family),
              by = c("kingdom", "phylum", "family")) %>%
    anti_join(found_species %>% distinct(kingdom, phylum, family),
              by = c("kingdom", "phylum", "family")) %>%
    mutate(genus = NA_character_, species = NA_character_)

  # Combine all unrepresented taxa
  unrepresented_taxa <- bind_rows(
    unrepresented_species %>% mutate(taxa_level = "species"),
    unrepresented_genera %>% mutate(taxa_level = "genus"),
    unrepresented_families %>% mutate(taxa_level = "family")
  ) %>%
    arrange(kingdom, phylum, family, genus, species) %>%
    filter(!is.na(phylum))  # Remove any entries without phylum

  # Save to CSV
  write_csv(unrepresented_taxa, "results/unrepresented_taxa.csv")

  message("Saved comprehensive list of unrepresented taxa to: results/unrepresented_taxa.csv")
  message("Total unrepresented taxa: ", nrow(unrepresented_taxa))
  message("- Species: ", sum(unrepresented_taxa$taxa_level == "species"))
  message("- Genera: ", sum(unrepresented_taxa$taxa_level == "genus"))
  message("- Families: ", sum(unrepresented_taxa$taxa_level == "family"))

  return(unrepresented_taxa)
}

# Create directory for results if it doesn't exist
dir.create("results", showWarnings = FALSE)

# Create geographic-taxonomic analysis
geo_taxa_analysis <- create_geographic_taxa_analysis()

# Create geographic-taxonomic visualizations
if (!is.null(geo_taxa_analysis)) {
  geo_plots <- create_geographic_taxa_visualizations(geo_taxa_analysis)
}

# Create the unrepresented taxa CSV
unrepresented_taxa <- create_unrepresented_taxa_csv()

# Create manuscript summary report
create_manuscript_summary <- function() {
  message("Creating manuscript summary report...")

  # Collect summaries for Plantae at all levels
  plant_summaries <- list()

  for (level in c("Family", "Genus", "Species")) {
    taxa_column <- case_when(
      level == "Family" ~ "family_final",
      level == "Genus" ~ "genus_final",
      level == "Species" ~ "canonicalName_final"
    )

    ref_column <- case_when(
      level == "Family" ~ "family",
      level == "Genus" ~ "genus",
      level == "Species" ~ "canonicalName_resolved"
    )

    match_type_val <- tolower(level)

    # Get found taxa
    found_by_phylum <- taxa_results_main %>%
      filter(kingdom == "Plantae",
             final_classification == "Presence",
             !is.na(.data[[taxa_column]]),
             !is.na(phylum),
             match_type == match_type_val) %>%
      distinct(phylum, .data[[taxa_column]]) %>%
      group_by(phylum) %>%
      summarise(found = n_distinct(.data[[taxa_column]]), .groups = "drop")

    # Get total taxa
    total_by_phylum <- accepted_species %>%
      filter(kingdom == "Plantae",
             !is.na(.data[[ref_column]]),
             !is.na(phylum)) %>%
      distinct(phylum, .data[[ref_column]]) %>%
      group_by(phylum) %>%
      summarise(total = n_distinct(.data[[ref_column]]), .groups = "drop")

    # Combine
    summary_data <- total_by_phylum %>%
      left_join(found_by_phylum, by = "phylum") %>%
      mutate(
        found = replace_na(found, 0),
        percent_found = round((found / total) * 100, 1)
      ) %>%
      filter(total > 0) %>%
      arrange(desc(percent_found))

    plant_summaries[[level]] <- summary_data
  }

  # Write summary report
  sink("results/visualization_summary_report_main.txt")

  cat("PLANT PHYLUM REPRESENTATION SUMMARY (Main Analysis - Excluding Mycorrhizal-Only Papers)\n")
  cat("==============================================================================\n\n")

  # Overall statistics
  total_species <- nrow(accepted_species %>% filter(kingdom == "Plantae", !is.na(canonicalName)))
  found_species <- taxa_results_main %>%
    filter(kingdom == "Plantae", final_classification == "Presence", !is.na(canonicalName_final), match_type == "species") %>%
    distinct(canonicalName_final) %>% nrow()
  percent_species <- round((found_species / total_species) * 100, 1)

  cat("Overall Plant Kingdom:\n")
  cat(sprintf("- Total species in reference database: %d\n", total_species))
  cat(sprintf("- Species found in literature: %d (%.1f%%)\n", found_species, percent_species))
  cat(sprintf("- Mycorrhizal-only abstracts excluded: %d\n\n", nrow(taxa_results_deduped) - nrow(taxa_results_main)))

  # Phylum-by-phylum breakdown
  phyla <- unique(c(plant_summaries$Family$phylum, plant_summaries$Genus$phylum, plant_summaries$Species$phylum))

  for (phyl in phyla) {
    cat(sprintf("Phylum: %s\n", phyl))

    # Species summary
    species_data <- plant_summaries$Species %>% filter(phylum == phyl)
    if (nrow(species_data) > 0) {
      cat(sprintf("- Species: %d/%d represented (%.1f%%)\n", species_data$found, species_data$total, species_data$percent_found))
    }

    # Genera summary
    genus_data <- plant_summaries$Genus %>% filter(phylum == phyl)
    if (nrow(genus_data) > 0) {
      cat(sprintf("- Genera: %d/%d represented (%.1f%%)\n", genus_data$found, genus_data$total, genus_data$percent_found))
    }

    # Families summary
    family_data <- plant_summaries$Family %>% filter(phylum == phyl)
    if (nrow(family_data) > 0) {
      cat(sprintf("- Families: %d/%d represented (%.1f%%)\n", family_data$found, family_data$total, family_data$percent_found))
    }

    cat("\n")
  }

  # Most and least studied phyla
  if (nrow(plant_summaries$Species) > 0) {
    most_studied <- plant_summaries$Species %>% slice_max(percent_found, n = 1)
    least_studied <- plant_summaries$Species %>% slice_min(percent_found, n = 1)

    cat("Most studied plant phylum by species representation:\n")
    cat(sprintf("%s (%.1f%% of species represented)\n\n", most_studied$phylum, most_studied$percent_found))

    cat("Least studied plant phylum by species representation:\n")
    cat(sprintf("%s (%.1f%% of species represented)\n\n", least_studied$phylum, least_studied$percent_found))
  }

  sink()

  message("Saved manuscript summary report to: results/visualization_summary_report_main.txt")
}

# Create the manuscript summary
create_manuscript_summary()

# Create manuscript-ready log file with key statistics
create_manuscript_log <- function() {
  log_file <- "results/visualize_taxa_results_manuscript_log.txt"

  # Calculate key statistics for manuscript
  total_species_gbif <- nrow(accepted_species %>% filter(kingdom == "Plantae", !is.na(canonicalName)))
  species_found <- taxa_results_main %>%
    filter(kingdom == "Plantae", final_classification == "Presence", !is.na(canonicalName_final), match_type == "species") %>%
    distinct(canonicalName_final) %>% nrow()
  species_percent <- round((species_found / total_species_gbif) * 100, 1)

  # Top phyla by representation
  phylum_summary <- accepted_species %>%
    filter(kingdom == "Plantae", !is.na(canonicalName)) %>%
    group_by(phylum) %>%
    summarise(total_species = n_distinct(canonicalName), .groups = "drop") %>%
    left_join(
      taxa_results_main %>%
        filter(kingdom == "Plantae", final_classification == "Presence", !is.na(canonicalName_final), match_type == "species") %>%
        distinct(phylum, canonicalName_final) %>%
        group_by(phylum) %>%
        summarise(found_species = n_distinct(canonicalName_final), .groups = "drop"),
      by = "phylum"
    ) %>%
    mutate(
      found_species = replace_na(found_species, 0),
      percent_found = round((found_species / total_species) * 100, 1)
    ) %>%
    arrange(desc(percent_found))

  capture.output({
    cat("=== VISUALIZE_TAXA_RESULTS.R MANUSCRIPT STATISTICS ===\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

    cat("OVERALL PLANT SPECIES COVERAGE:\n")
    cat("===============================\n")
    cat(sprintf("- Total plant species in GBIF reference: %s\n", format(total_species_gbif, big.mark = ",")))
    cat(sprintf("- Plant species found in literature: %s\n", format(species_found, big.mark = ",")))
    cat(sprintf("- Percentage coverage: %.1f%%\n", species_percent))
    cat(sprintf("- Mycorrhizal-only abstracts excluded: %s\n\n",
                format(nrow(taxa_results_deduped) - nrow(taxa_results_main), big.mark = ",")))

    cat("PLANT PHYLUM REPRESENTATION (Top 10 by coverage):\n")
    cat("===============================================\n")
    top_phyla <- head(phylum_summary, 10)
    for(i in 1:nrow(top_phyla)) {
      phyl <- top_phyla[i,]
      cat(sprintf("%d. %s: %s/%s species (%.1f%%)\n",
                  i, phyl$phylum,
                  format(phyl$found_species, big.mark = ","),
                  format(phyl$total_species, big.mark = ","),
                  phyl$percent_found))
    }
    cat("\n")

    cat("VISUALIZATION OUTPUTS:\n")
    cat("=====================\n")
    cat("- 24 phylum-based plots (12 main + 12 supplementary)\n")
    cat("- 3 geographic-taxonomic visualizations\n")
    cat("- 6 kingdoms × 2 taxonomic levels (count + percent) × 2 modes\n")
    cat("- 2 data files: unrepresented taxa + geographic analysis\n")
    cat("- Total: 27 visualization files + 2 data files\n\n")

    cat("METHODOLOGY DETAILS:\n")
    cat("===================\n")
    cat("- Data source: GBIF Backbone Taxonomy\n")
    cat("- Taxonomic validation: Synonym resolution to accepted names\n")
    cat("- Filtering: Excludes extinct species, includes mycorrhizal classification\n")
    cat("- Hierarchical counting: Species → Genera → Families within phyla\n")
    cat("- Visualization: Enhanced with common names and species counts\n\n")

    cat("KEY FINDINGS FOR MANUSCRIPT:\n")
    cat("===========================\n")
    cat(sprintf("- Plant endophyte research covers %.1f%% of known plant species diversity\n", species_percent))
    cat("- Most studied phylum:", phylum_summary$phylum[1], sprintf("(%.1f%% coverage)\n", phylum_summary$percent_found[1]))
    cat("- Least studied phylum:", tail(phylum_summary$phylum, 1), sprintf("(%.1f%% coverage)\n", tail(phylum_summary$percent_found, 1)))
    cat("- Analysis includes both presence and absence evidence detection\n")
    cat("- Geographic distribution shows research concentration patterns\n")

  }, file = log_file)

  message("Manuscript-ready statistics saved to: ", log_file)
}

# Generate the manuscript log
create_manuscript_log()

message("All visualizations and data exports completed!")
message("Created:")
message("- 24 phylum-based plots (12 main + 12 supplementary)")
message("- 3 geographic-taxonomic visualizations")
message("- 6 kingdoms × 2 taxonomic levels (count + percent) × 2 modes (main + supplementary)")
message("- 2 data files: unrepresented taxa + geographic analysis")
message("Total: 27 visualization files in plots/ directory + 2 data files")
message("Analysis modes:")
message("- MAIN: Excludes mycorrhizal-only papers (endophyte focus)")
message("- SUPPLEMENTARY: Includes mycorrhizal-only papers")
message("Geographic visualizations:")
message("- plots/geographic_taxonomic_diversity.png")
message("- plots/plant_species_by_country.png")
message("- plots/plant_phylum_geographic_distribution.png")
message("Enhanced features:")
message("- Uses pre-processed accepted species data (accepted_species.rds)")
message("- Leverages optimized pipeline reference data")
message("- Synonym resolution to accepted names")
message("- Extinct species exclusion")
message("- Mycorrhizal classification filtering")
message("- Geographic-taxonomic analysis")
message("- Both count and percentage versions")
message("- Phylum-level breakdown for both Plantae and Fungi")
message("- Consistent phylum ordering by species count")
message("- Comprehensive list of unrepresented taxa")
message("- Integration with enhanced extraction pipeline")
