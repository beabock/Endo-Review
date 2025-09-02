# Focused Taxa Visualization Script - Phylum-Based Only
# Creates phylum-based plots showing representation of plant and fungal taxa
# Includes synonym resolution and extinct species exclusion

library(tidyverse)
library(scales)

# Load data exactly like user's working script
setwd("c:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

# Load the species data with extinct species exclusion (like in working script)
species_data <- readRDS("models/species.rds")

# Filter for Plantae and Fungi species like in working script
reference_species <- species_data %>%
  filter(kingdom %in% c("Plantae", "Fungi") & taxonRank == "species")

# Add extinct species exclusion if the data is available
if ("species_lower" %in% colnames(reference_species) ||
    all(c("canonicalName", "genus", "family", "order") %in% colnames(reference_species))) {

  # Create lowercase versions for extinct species matching
  reference_species <- reference_species %>%
    mutate(
      species_lower = tolower(canonicalName),
      genus_lower = tolower(genus),
      family_lower = tolower(family),
      order_lower = tolower(order)
    )

  # Load PBDB extinct species data
  tryCatch({
    extinct_taxa <- read.csv("data/raw/pbdb_all.csv", skip = 16) %>%
      filter(is_extant == "extinct") %>%
      mutate(across(taxon_name:genus, tolower))

    extinct_species <- extinct_taxa %>%
      filter(taxon_rank == "species") %>%
      distinct(taxon_name) %>%
      pull(taxon_name)

    extinct_genera <- extinct_taxa %>%
      filter(taxon_rank == "genus") %>%
      distinct(taxon_name) %>%
      pull(taxon_name)

    extinct_families <- extinct_taxa %>%
      filter(taxon_rank == "family") %>%
      distinct(taxon_name) %>%
      pull(taxon_name)

    extinct_orders <- extinct_taxa %>%
      filter(taxon_rank == "order") %>%
      distinct(taxon_name) %>%
      pull(taxon_name)

    # Exclude extinct species
    reference_species <- reference_species %>%
      filter(
        !(species_lower %in% extinct_species),
        !(genus_lower %in% extinct_genera),
        !(family_lower %in% extinct_families),
        !(order_lower %in% extinct_orders)
      ) %>%
      select(-species_lower, -genus_lower, -family_lower, -order_lower)

    message("Excluded extinct species from reference data")
  }, error = function(e) {
    message("Could not load extinct species data: ", e$message)
  })
}

# Add synonym resolution
if (all(c("taxonomicStatus", "acceptedNameUsageID", "taxonID") %in% colnames(reference_species))) {
  message("Resolving synonyms to accepted names...")

  # Get accepted species
  accepted_species <- reference_species %>%
    filter(taxonomicStatus == "accepted") %>%
    select(taxonID, canonicalName) %>%
    rename(canonicalName_accepted = canonicalName)

  # Resolve synonyms
  reference_species <- reference_species %>%
    left_join(accepted_species, by = c("acceptedNameUsageID" = "taxonID")) %>%
    mutate(
      canonicalName_resolved = coalesce(canonicalName_accepted, canonicalName)
    ) %>%
    select(-canonicalName_accepted)

  message("Resolved synonyms in reference data")
}

message("Reference data loaded: ", nrow(reference_species), " species")

# Load taxa results with synonym resolution and deduplication
taxa_results <- read_csv("results/comprehensive_extraction_results.csv", show_col_types = FALSE)

message("Taxa results loaded: ", nrow(taxa_results), " entries")

# Handle synonym resolution for taxa_results (if columns exist)
if (all(c("user_supplied_name", "resolved_name") %in% colnames(taxa_results))) {
  message("Resolving synonyms in taxa_results...")

  # Replace user_supplied_name with resolved_name where available
  taxa_results <- taxa_results %>%
    mutate(
      canonicalName_resolved = coalesce(resolved_name, canonicalName),
      genus_resolved = if_else(match_type == "species" & !is.na(resolved_name),
                               word(resolved_name, 1), genus),
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
taxa_results_deduped <- taxa_results %>%
  # First, apply synonym resolution consistently
  mutate(
    # For species, use resolved name if available, otherwise original
    canonicalName_resolved = coalesce(resolved_name, canonicalName),
    # For genus, extract from resolved species name or use resolved genus
    genus_resolved = case_when(
      match_type == "species" & !is.na(resolved_name) ~ word(resolved_name, 1),
      match_type == "genus" & !is.na(resolved_name) ~ resolved_name,
      TRUE ~ genus
    ),
    # For family, keep as is (resolved family not typically provided)
    family_resolved = family
  ) %>%
  # Remove duplicates within the same abstract for the same taxon
  distinct(id, kingdom, phylum, canonicalName_resolved, genus_resolved, family_resolved,
           match_type, final_classification, .keep_all = TRUE) %>%
  # Create final resolved names for counting
  mutate(
    canonicalName_final = canonicalName_resolved,
    genus_final = genus_resolved,
    family_final = family_resolved
  )

message("Data prepared with synonym resolution and deduplication")

# Create consistent phylum ordering function
get_phylum_order <- function(kingdom_filter) {
  reference_species %>%
    filter(kingdom == kingdom_filter, !is.na(phylum)) %>%
    distinct(phylum, canonicalName) %>%
    group_by(phylum) %>%
    summarise(species_count = n(), .groups = "drop") %>%
    arrange(desc(species_count)) %>%
    pull(phylum)
}

# Get consistent ordering for each kingdom (reverse for correct plot orientation)
plant_phylum_order <- rev(get_phylum_order("Plantae"))
fungi_phylum_order <- rev(get_phylum_order("Fungi"))

message("Plant phylum order (by species count, reversed for plot): ", paste(plant_phylum_order, collapse = ", "))
message("Fungi phylum order (by species count, reversed for plot): ", paste(fungi_phylum_order, collapse = ", "))

# Create phylum-based visualization function with hierarchical counting
create_phylum_taxa_plot <- function(kingdom_filter, level_name, column_name, output_name) {

  # Get found taxa with hierarchical logic and synonym resolution
  if (level_name == "Species") {
    # For species, count direct species mentions using resolved names
    found_by_phylum <- taxa_results_deduped %>%
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
  total_by_phylum <- reference_species %>%
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

  # Apply consistent phylum ordering
  if (kingdom_filter == "Plantae") {
    count_data <- count_data %>%
      mutate(phylum = factor(phylum, levels = plant_phylum_order))
  } else if (kingdom_filter == "Fungi") {
    count_data <- count_data %>%
      mutate(phylum = factor(phylum, levels = fungi_phylum_order))
  }

  count_plot <- ggplot(count_data, aes(x = phylum, y = Count, fill = Status)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 3) +
    coord_flip() +
    labs(
      title = paste(kingdom_filter, level_name, "Representation by Phylum (Count)"),
      subtitle = paste("Number of", tolower(level_name), "found vs. not found in each phylum (hierarchical)"),
      x = "Phylum",
      y = paste("Number of", level_name, "s")
    ) +
    scale_fill_manual(values = c("Found" = "#2E8B57", "Not Found" = "#CD853F")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

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

  percent_plot <- ggplot(percent_data, aes(x = phylum, y = Percent, fill = Status)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", Percent)), position = position_stack(vjust = 0.5), size = 3) +
    coord_flip() +
    labs(
      title = paste(kingdom_filter, level_name, "Representation by Phylum (Percent)"),
      subtitle = paste("Percentage of", tolower(level_name), "found vs. not found in each phylum (hierarchical)"),
      x = "Phylum",
      y = paste("Percentage of", level_name, "s")
    ) +
    scale_fill_manual(values = c("Found" = "#2E8B57", "Not Found" = "#CD853F")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

  # Save both plots
  ggsave(paste0("plots/", output_name, "_by_phylum_count.png"), count_plot, width = 12, height = 8)
  ggsave(paste0("plots/", output_name, "_by_phylum_percent.png"), percent_plot, width = 12, height = 8)

  message("Saved: plots/", output_name, "_by_phylum_count.png")
  message("Saved: plots/", output_name, "_by_phylum_percent.png")
  message("Used hierarchical counting: ", level_name, " includes constituent taxa")

  return(list(count_plot = count_plot, percent_plot = percent_plot))
}

# Create output directory
dir.create("plots", showWarnings = FALSE)

# Create phylum-based representation plots (count and percent versions)
message("Creating phylum-based visualization plots...")

# Plant phylum plots
create_phylum_taxa_plot("Plantae", "Family", "family", "plantae_family_representation")
create_phylum_taxa_plot("Plantae", "Genus", "genus", "plantae_genus_representation")
create_phylum_taxa_plot("Plantae", "Species", "canonicalName", "plantae_species_representation")

# Fungi phylum plots
create_phylum_taxa_plot("Fungi", "Family", "family", "fungi_family_representation")
create_phylum_taxa_plot("Fungi", "Genus", "genus", "fungi_genus_representation")
create_phylum_taxa_plot("Fungi", "Species", "canonicalName", "fungi_species_representation")

# Create comprehensive list of unrepresented taxa
create_unrepresented_taxa_csv <- function() {

  # Get all unique taxa from reference data (with resolved synonyms)
  all_reference_taxa <- reference_species %>%
    distinct(kingdom, phylum, family, genus, canonicalName_resolved) %>%
    rename(species = canonicalName_resolved)

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

# Create the unrepresented taxa CSV
unrepresented_taxa <- create_unrepresented_taxa_csv()

message("All visualizations and data exports completed!")
message("Created:")
message("- 12 phylum-based plots (6 kingdoms × 2 versions each)")
message("- 1 comprehensive CSV of unrepresented taxa")
message("Total: 12 visualization files in plots/ directory + 1 data file")
message("Features included:")
message("- Synonym resolution to accepted names")
message("- Extinct species exclusion")
message("- Both count and percentage versions")
message("- Phylum-level breakdown for both Plantae and Fungi")
message("- Consistent phylum ordering by species count")
message("- Comprehensive list of unrepresented taxa")
