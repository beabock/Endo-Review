# Visualization Script for Taxa Detection Results
# This script creates visualizations from the output of taxa_detection.R

library(tidyverse)
library(rgbif)
library(scales)

# Custom theme for consistent visualization
custom_theme <- theme_bw(base_size = 18)
theme_set(custom_theme)

# Custom color palette
cus_pal <- c(
  "#A1C181",  # soft sage green — for plants
  "#619B8A",  # muted teal — evokes moss or lichens
  "#C97E7E",  # dusty rose — for fungi like Russula or Hygrophoropsis
  "#D9AE94"   # pale mushroom beige — for caps and forest floor tones
)

# Function to save plots with consistent dimensions and format
save_plot <- function(filename, plot, width = 12, height = 7, units = "in", ...) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename, plot, width = width, height = height, units = units, ...)
  message("Saved plot to: ", filename)
}

# PLANT PARTS CONFIGURATION -----------------------------------------------

# Consolidated plant parts keywords with better organization
plant_parts_keywords <- c(
  # Basic structures
  "fruit", "fruits", "root", "roots", "leaf", "leaves", "stem", "stems", 
  "flower", "flowers", "seed", "seeds", "bark", "branch", "branches",
  "twig", "twigs", "shoot", "shoots", "bud", "buds", "trunk", "trunks",
  
  # Reproductive structures
  "pistil", "pistils", "anther", "anthers", "carpel", "carpels", 
  "sepal", "sepals", "petal", "petals", "stigma", "stigmas", 
  "style", "styles", "ovary", "ovaries", "ovule", "ovules",
  "calyx", "calyces", "corolla", "corollas", "pollen",
  "inflorescence", "inflorescences", "floret", "florets",
  
  # Specialized structures
  "rhizome", "rhizomes", "tuber", "tubers", "bulb", "bulbs", 
  "corm", "corms", "tendril", "tendrils", "thorn", "thorns",
  "cone", "cones", "needle", "needles"
)

# Create mapping for plural/singular normalization
create_plant_part_groups <- function(keywords) {
  groups <- character()
  for (word in keywords) {
    if (str_ends(word, "s") && !word %in% c("cortex", "xylem", "phloem")) {
      singular <- str_remove(word, "s$")
      if (singular %in% keywords) {
        groups[word] <- singular
      }
    }
  }
  # Add special cases
  groups["calyces"] <- "calyx"
  groups["cortices"] <- "cortex"
  groups["stomata"] <- "stoma"
  groups["laminae"] <- "lamina"
  groups["paleae"] <- "palea"
  
  return(groups)
}

plant_part_groups <- create_plant_part_groups(plant_parts_keywords)

# VISUALIZATION FUNCTIONS -------------------------------------------------

# Function to plot abstracts per plant phyla
plot_abstracts_per_phyla <- function(taxa_results, output_dir = "plots") {
  # Get expected phyla
  plantae_key <- 6
  fungi_key <- 5
  
  # Get plant phyla
  plant_phyla <- name_lookup(
    higherTaxonKey = plantae_key,
    rank = "PHYLUM",
    status = "ACCEPTED",
    isExtinct = FALSE,
    limit = 5000
  )$data
  
  # Get fungi phyla
  fungi_phyla <- name_lookup(
    higherTaxonKey = fungi_key,
    rank = "PHYLUM",
    status = "ACCEPTED",
    isExtinct = FALSE,
    limit = 5000
  )$data
  
  expected_plant_phyla <- unique(plant_phyla$canonicalName)
  expected_fungi_phyla <- unique(fungi_phyla$canonicalName)
  
  # Create plant phyla plot
  plant_phylum_summary <- taxa_results %>%
    filter(kingdom == "Plantae", !is.na(phylum)) %>%
    distinct(id, phylum, predicted_label) %>%
    count(phylum, predicted_label, name = "abstracts_with_label") %>%
    complete(phylum = expected_plant_phyla, 
             predicted_label = c("Presence", "Absence"),
             fill = list(abstracts_with_label = 0)) %>%
    mutate(
      phylum = factor(phylum, levels = sort(expected_plant_phyla)),
      predicted_label = factor(predicted_label, levels = c("Absence", "Presence"))
    )
  
  plant_phylum_plot <- ggplot(plant_phylum_summary, 
                             aes(x = phylum, y = abstracts_with_label, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Abstract Mentions in Plant Phyla",
      x = "Phylum",
      y = "Abstracts Mentioned"
    ) +
    scale_fill_manual(values = cus_pal[1:2]) +
    theme_minimal()
  
  save_plot(file.path(output_dir, "plant_phyla.png"), plant_phylum_plot)
  
  # Create fungi phyla plot
  fungi_phylum_summary <- taxa_results %>%
    filter(kingdom == "Fungi", !is.na(phylum)) %>%
    distinct(id, phylum, predicted_label) %>%
    count(phylum, predicted_label, name = "abstracts_with_label") %>%
    complete(phylum = expected_fungi_phyla, 
             predicted_label = c("Presence", "Absence"),
             fill = list(abstracts_with_label = 0)) %>%
    mutate(
      phylum = factor(phylum, levels = sort(expected_fungi_phyla)),
      predicted_label = factor(predicted_label, levels = c("Absence", "Presence"))
    )
  
  fungi_phylum_plot <- ggplot(fungi_phylum_summary, 
                             aes(x = phylum, y = abstracts_with_label, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Abstract Mentions in Fungi Phyla",
      x = "Phylum",
      y = "Abstracts Mentioned"
    ) +
    scale_fill_manual(values = cus_pal[3:4]) +
    theme_minimal()
  
  save_plot(file.path(output_dir, "fungi_phyla.png"), fungi_phylum_plot)
  
  return(list(plant_phylum_plot = plant_phylum_plot, fungi_phylum_plot = fungi_phylum_plot))
}

# Function to analyze plant parts
plot_plant_parts <- function(taxa_results, output_dir = "plots") {
  # Create mapping for plural/singular normalization
  plant_part_groups <- create_plant_part_groups(plant_parts_keywords)
  
  # Get plant part columns
  plant_part_cols <- intersect(names(taxa_results), plant_parts_keywords)
  
  if (length(plant_part_cols) == 0) {
    message("No plant part columns found in the data")
    return(NULL)
  }
  
  # Summarize plant parts by label
  plant_parts_summary <- taxa_results %>%
    select(id, predicted_label, all_of(plant_part_cols)) %>%
    distinct() %>%
    group_by(predicted_label) %>%
    summarise(across(all_of(plant_part_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(
      cols = all_of(plant_part_cols),
      names_to = "plant_part",
      values_to = "n_abstracts"
    )
  
  # Group plant parts (combine singular/plural forms)
  plant_parts_grouped <- plant_parts_summary %>%
    mutate(
      grouped_part = ifelse(plant_part %in% names(plant_part_groups), 
                           plant_part_groups[plant_part], 
                           plant_part)
    ) %>%
    group_by(predicted_label, grouped_part) %>%
    summarise(n_abstracts = sum(n_abstracts), .groups = "drop")
  
  # Find most and least mentioned plant parts
  top_parts <- plant_parts_grouped %>%
    group_by(grouped_part) %>%
    summarise(total = sum(n_abstracts)) %>%
    arrange(desc(total)) %>%
    slice_head(n = 20)
  
  bottom_parts <- plant_parts_grouped %>%
    group_by(grouped_part) %>%
    summarise(total = sum(n_abstracts)) %>%
    arrange(total) %>%
    filter(total > 0) %>%  # Only include parts that are mentioned at least once
    slice_head(n = 20)
  
  # Create plots
  top_plot <- plant_parts_grouped %>%
    filter(grouped_part %in% top_parts$grouped_part) %>%
    ggplot(aes(x = reorder(grouped_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Most Frequently Mentioned Plant Parts",
      x = "Plant Part",
      y = "Number of Abstracts"
    ) +
    scale_fill_manual(values = cus_pal[1:2])
  
  save_plot(file.path(output_dir, "plant_parts_top.png"), top_plot)
  
  bottom_plot <- plant_parts_grouped %>%
    filter(grouped_part %in% bottom_parts$grouped_part) %>%
    ggplot(aes(x = reorder(grouped_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Least Frequently Mentioned Plant Parts",
      x = "Plant Part",
      y = "Number of Abstracts"
    ) +
    scale_fill_manual(values = cus_pal[1:2])
  
  save_plot(file.path(output_dir, "plant_parts_bottom.png"), bottom_plot)
  
  return(list(top_plot = top_plot, bottom_plot = bottom_plot))
}

# Function to analyze taxonomic coverage
plot_taxonomic_coverage <- function(taxa_results, output_dir = "plots") {
  # Load species data
  if (!file.exists("species.rds")) {
    stop("species.rds file not found. Please run the taxa_detection.R script first.")
  }
  species <- readRDS("species.rds")
  
  # Create or load lookup tables
  if (file.exists("accepted_species.rds")) {
    accepted_species <- readRDS("accepted_species.rds")
  } else {
    accepted_species <- species %>%
      filter(taxonomicStatus == "accepted", !is.na(phylum), !is.na(family), !is.na(genus))
    saveRDS(accepted_species, "accepted_species.rds")
  }
  
  if (file.exists("families.rds") && file.exists("genera.rds")) {
    families_from_species <- readRDS("families.rds")
    genera_from_species <- readRDS("genera.rds")
  } else {
    families_from_species <- accepted_species %>%
      distinct(family, phylum, kingdom) %>%
      rename(canonicalName = family)
    
    genera_from_species <- accepted_species %>%
      distinct(genus, phylum, family, kingdom) %>%
      rename(canonicalName = genus)
    
    saveRDS(families_from_species, "families.rds")
    saveRDS(genera_from_species, "genera.rds")
  }
  
  # Get expected phyla
  plantae_key <- 6
  phyla <- name_lookup(
    higherTaxonKey = plantae_key,
    rank = "PHYLUM",
    status = "ACCEPTED",
    isExtinct = FALSE,
    limit = 5000
  )$data
  
  expected_plant_phyla <- unique(phyla$canonicalName)
  phylum_order <- sort(expected_plant_phyla)
  
  # Clean dataset
  plant_df_clean <- taxa_results %>%
    filter(kingdom == "Plantae", !is.na(phylum))
  
  # Analyze family coverage
  plant_families_backbone <- accepted_species %>%
    filter(kingdom == "Plantae") %>%
    filter(!is.na(family), !is.na(phylum)) %>%
    distinct(phylum, family)
  
  total_families_per_phylum <- plant_families_backbone %>%
    group_by(phylum) %>%
    summarise(total_families = n_distinct(family), .groups = "drop")
  
  dataset_families <- plant_df_clean %>%
    filter(!is.na(family)) %>%
    distinct(phylum, family)
  
  families_in_dataset <- dataset_families %>%
    group_by(phylum) %>%
    summarise(n_families_found = n_distinct(family), .groups = "drop")
  
  families_coverage <- total_families_per_phylum %>%
    left_join(families_in_dataset, by = "phylum") %>%
    mutate(
      n_families_found = replace_na(n_families_found, 0),
      n_families_missing = total_families - n_families_found
    ) %>%
    arrange(phylum)
  
  families_coverage$phylum <- factor(families_coverage$phylum, levels = phylum_order)
  
  # Prepare data for family coverage plot
  families_long <- families_coverage %>%
    filter(!is.na(phylum)) %>%
    select(phylum, n_families_found, n_families_missing) %>%
    pivot_longer(
      cols = c(n_families_found, n_families_missing),
      names_to = "status",
      values_to = "count"
    ) %>%
    mutate(
      status = recode(status,
                     n_families_found = "Found",
                     n_families_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  # Color palette
  coverage_pal <- c(
    "Found" = "#A1C181",   # calm sage green — presence
    "Missing" = "#C97E7E"  # dusty rose — absence
  )
  
  # Create family coverage plot
  family_plot <- ggplot(families_long, aes(x = phylum, y = count, fill = status)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Plant Family Coverage",
      x = "Phylum",
      y = "Number of Families",
      fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal)
  
  save_plot(file.path(output_dir, "family_coverage.png"), family_plot)
  
  # Create proportional family coverage plot
  families_coverage_prop <- families_coverage %>%
    filter(!is.na(phylum)) %>%
    mutate(
      prop_found = n_families_found / total_families,
      prop_missing = n_families_missing / total_families
    )
  
  families_long_prop <- families_coverage_prop %>%
    select(phylum, prop_found, prop_missing) %>%
    pivot_longer(
      cols = c(prop_found, prop_missing),
      names_to = "status",
      values_to = "proportion"
    ) %>%
    mutate(
      status = recode(status,
                     prop_found = "Found",
                     prop_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  family_prop_plot <- ggplot(families_long_prop, aes(x = phylum, y = proportion, fill = status)) +
    geom_col() +
    geom_text(
      aes(label = scales::percent(proportion, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      size = 4,
      color = "black"
    ) +
    coord_flip() +
    labs(
      title = "Proportional Plant Family Coverage",
      x = "Phylum",
      y = "Proportion of Families",
      fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  save_plot(file.path(output_dir, "family_coverage_prop.png"), family_prop_plot)
  
  # Similar analysis for genera
  plant_genera_backbone <- accepted_species %>%
    filter(kingdom == "Plantae") %>%
    filter(!is.na(genus), !is.na(phylum)) %>%
    distinct(phylum, genus)
  
  total_genera_per_phylum <- plant_genera_backbone %>%
    group_by(phylum) %>%
    summarise(total_genera = n_distinct(genus), .groups = "drop")
  
  dataset_genera <- plant_df_clean %>%
    filter(!is.na(genus)) %>%
    distinct(phylum, genus)
  
  genera_in_dataset <- dataset_genera %>%
    group_by(phylum) %>%
    summarise(n_genera_found = n_distinct(genus), .groups = "drop")
  
  genera_coverage <- total_genera_per_phylum %>%
    left_join(genera_in_dataset, by = "phylum") %>%
    mutate(
      n_genera_found = replace_na(n_genera_found, 0),
      n_genera_missing = total_genera - n_genera_found
    ) %>%
    arrange(phylum)
  
  genera_coverage$phylum <- factor(genera_coverage$phylum, levels = phylum_order)
  
  # Create genus coverage plots
  genera_long <- genera_coverage %>%
    filter(!is.na(phylum)) %>%
    select(phylum, n_genera_found, n_genera_missing) %>%
    pivot_longer(cols = starts_with("n_"), names_to = "status", values_to = "count") %>%
    mutate(
      status = recode(status, n_genera_found = "Found", n_genera_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  genus_plot <- ggplot(genera_long, aes(x = phylum, y = count, fill = status)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Plant Genus Coverage",
      x = "Phylum", y = "Number of Genera", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal)
  
  save_plot(file.path(output_dir, "genus_coverage.png"), genus_plot)
  
  # Proportional genus coverage
  genera_coverage_prop <- genera_coverage %>%
    mutate(
      prop_found = n_genera_found / total_genera,
      prop_missing = n_genera_missing / total_genera
    )
  
  genera_long_prop <- genera_coverage_prop %>%
    pivot_longer(cols = starts_with("prop_"), names_to = "status", values_to = "proportion") %>%
    mutate(
      status = recode(status, prop_found = "Found", prop_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  genus_prop_plot <- ggplot(genera_long_prop, aes(x = phylum, y = proportion, fill = status)) +
    geom_col() +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              size = 4, color = "black") +
    coord_flip() +
    labs(
      title = "Proportional Plant Genus Coverage",
      x = "Phylum", y = "Proportion of Genera", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  save_plot(file.path(output_dir, "genus_coverage_prop.png"), genus_prop_plot)
  
  # Similar analysis for species
  plant_species_backbone <- accepted_species %>%
    filter(kingdom == "Plantae") %>%
    filter(!is.na(canonicalName), !is.na(phylum)) %>%
    distinct(phylum, canonicalName)
  
  total_species_per_phylum <- plant_species_backbone %>%
    group_by(phylum) %>%
    summarise(total_species = n_distinct(canonicalName), .groups = "drop")
  
  dataset_species <- plant_df_clean %>%
    filter(!is.na(canonicalName)) %>%
    distinct(phylum, canonicalName)
  
  species_in_dataset <- dataset_species %>%
    group_by(phylum) %>%
    summarise(n_species_found = n_distinct(canonicalName), .groups = "drop")
  
  species_coverage <- total_species_per_phylum %>%
    left_join(species_in_dataset, by = "phylum") %>%
    mutate(
      n_species_found = replace_na(n_species_found, 0),
      n_species_missing = total_species - n_species_found
    ) %>%
    arrange(phylum)
  
  species_coverage$phylum <- factor(species_coverage$phylum, levels = phylum_order)
  
  # Create species coverage plots
  species_long <- species_coverage %>%
    select(phylum, n_species_found, n_species_missing) %>%
    pivot_longer(cols = starts_with("n_"), names_to = "status", values_to = "count") %>%
    mutate(
      status = recode(status, n_species_found = "Found", n_species_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  species_plot <- ggplot(species_long, aes(x = phylum, y = count, fill = status)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Plant Species Coverage",
      x = "Phylum", y = "Number of Species", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal)
  
  save_plot(file.path(output_dir, "species_coverage.png"), species_plot)
  
  # Proportional species coverage
  species_coverage_prop <- species_coverage %>%
    mutate(
      prop_found = n_species_found / total_species,
      prop_missing = n_species_missing / total_species
    )
  
  species_long_prop <- species_coverage_prop %>%
    pivot_longer(cols = starts_with("prop_"), names_to = "status", values_to = "proportion") %>%
    mutate(
      status = recode(status, prop_found = "Found", prop_missing = "Missing"),
      phylum = factor(phylum, levels = phylum_order)
    )
  
  species_prop_plot <- ggplot(species_long_prop, aes(x = phylum, y = proportion, fill = status)) +
    geom_col() +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              size = 4, color = "black") +
    coord_flip() +
    labs(
      title = "Proportional Plant Species Coverage",
      x = "Phylum", y = "Proportion of Species", fill = "Coverage Status"
    ) +
    scale_fill_manual(values = coverage_pal) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  save_plot(file.path(output_dir, "species_coverage_prop.png"), species_prop_plot)
  
  # Save missing taxa lists
  missing_families <- plant_families_backbone %>%
    anti_join(dataset_families, by = c("phylum", "family")) %>%
    distinct(family)
  
  missing_genera <- plant_genera_backbone %>%
    anti_join(dataset_genera, by = c("phylum", "genus")) %>%
    distinct(genus)
  
  missing_species <- plant_species_backbone %>%
    anti_join(dataset_species, by = c("phylum", "canonicalName")) %>%
    distinct(canonicalName)
  
  write_csv(missing_families, file.path(output_dir, "missing_families.csv"))
  write_csv(missing_genera, file.path(output_dir, "missing_genera.csv"))
  write_csv(missing_species, file.path(output_dir, "missing_species.csv"))
  
  return(list(
    family_plot = family_plot,
    family_prop_plot = family_prop_plot,
    genus_plot = genus_plot,
    genus_prop_plot = genus_prop_plot,
    species_plot = species_plot,
    species_prop_plot = species_prop_plot
  ))
}

# Function to detect research methods in abstracts
detect_research_methods <- function(abstracts_df) {
  # Define method categories and their keywords
  method_categories <- list(
    molecular = c("pcr", "dna", "rna", "sequenc", "primer", "amplif", "gene", "genom", 
                 "transcript", "clone", "phylogen", "molecular", "extraction", "isolat", 
                 "genetic", "marker", "polymorphism", "nucleotide", "hybridiz"),
    
    culture_based = c("culture", "isolat", "plate", "medium", "agar", "petri", "colony", 
                     "incubat", "inocul", "sterile", "aseptic", "axenic", "pure culture", 
                     "ferment", "broth", "in vitro"),
    
    microscopy = c("microscop", "stain", "section", "histolog", "morpholog", "ultrastructur", 
                  "sem", "tem", "scanning electron", "transmission electron", "light microscop", 
                  "confocal", "fluorescen"),
    
    field_observation = c("survey", "observ", "field", "collect", "sampl", "transect", 
                         "quadrat", "plot", "ecolog", "habitat", "in situ", "wild", "native"),
    
    computational = c("bioinformatic", "algorithm", "comput", "model", "simulat", "predict", 
                     "database", "software", "pipeline", "script", "analysis", "statistic")
  )
  
  # Function to detect methods in a text
  detect_methods <- function(text) {
    text_lower <- tolower(text)
    results <- sapply(names(method_categories), function(category) {
      keywords <- method_categories[[category]]
      matches <- sapply(keywords, function(keyword) {
        grepl(keyword, text_lower)
      })
      any(matches)
    })
    return(results)
  }
  
  # Apply detection to all abstracts
  methods_detected <- abstracts_df %>%
    mutate(
      abstract_text = abstract,
      molecular_methods = NA,
      culture_based_methods = NA,
      microscopy_methods = NA,
      field_observation_methods = NA,
      computational_methods = NA
    )
  
  # Process each abstract
  for (i in 1:nrow(methods_detected)) {
    if (!is.na(methods_detected$abstract_text[i])) {
      detection_results <- detect_methods(methods_detected$abstract_text[i])
      methods_detected$molecular_methods[i] <- detection_results["molecular"]
      methods_detected$culture_based_methods[i] <- detection_results["culture_based"]
      methods_detected$microscopy_methods[i] <- detection_results["microscopy"]
      methods_detected$field_observation_methods[i] <- detection_results["field_observation"]
      methods_detected$computational_methods[i] <- detection_results["computational"]
    }
  }
  
  return(methods_detected)
}

# Function to analyze methods and correlate with taxa
plot_research_methods <- function(taxa_results, abstracts_df, output_dir = "plots") {
  # Check if abstracts_df has the required columns
  if (!all(c("id", "abstract") %in% colnames(abstracts_df))) {
    stop("abstracts_df must contain 'id' and 'abstract' columns")
  }
  
  # Detect methods in abstracts
  methods_df <- detect_research_methods(abstracts_df)
  
  # Join with plant species data
  taxa_methods_df <- taxa_results %>%
    left_join(methods_df %>% 
                select(id, molecular_methods, culture_based_methods, 
                       microscopy_methods, field_observation_methods, 
                       computational_methods),
              by = "id")
  
  # Summarize methods by kingdom
  methods_by_kingdom <- taxa_methods_df %>%
    filter(!is.na(kingdom)) %>%
    group_by(kingdom) %>%
    summarise(
      total = n(),
      molecular = sum(molecular_methods, na.rm = TRUE),
      culture_based = sum(culture_based_methods, na.rm = TRUE),
      microscopy = sum(microscopy_methods, na.rm = TRUE),
      field_observation = sum(field_observation_methods, na.rm = TRUE),
      computational = sum(computational_methods, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      molecular_pct = molecular / total * 100,
      culture_based_pct = culture_based / total * 100,
      microscopy_pct = microscopy / total * 100,
      field_observation_pct = field_observation / total * 100,
      computational_pct = computational / total * 100
    )
  
  # Summarize methods by phylum
  methods_by_phylum <- taxa_methods_df %>%
    filter(!is.na(phylum)) %>%
    group_by(kingdom, phylum) %>%
    summarise(
      total = n(),
      molecular = sum(molecular_methods, na.rm = TRUE),
      culture_based = sum(culture_based_methods, na.rm = TRUE),
      microscopy = sum(microscopy_methods, na.rm = TRUE),
      field_observation = sum(field_observation_methods, na.rm = TRUE),
      computational = sum(computational_methods, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      molecular_pct = molecular / total * 100,
      culture_based_pct = culture_based / total * 100,
      microscopy_pct = microscopy / total * 100,
      field_observation_pct = field_observation / total * 100,
      computational_pct = computational / total * 100
    )
  
  # Create plots
  
  # Methods by kingdom plot
  kingdom_methods_long <- methods_by_kingdom %>%
    pivot_longer(
      cols = c(molecular_pct, culture_based_pct, microscopy_pct, 
               field_observation_pct, computational_pct),
      names_to = "method",
      values_to = "percentage"
    ) %>%
    mutate(
      method = recode(method,
                     molecular_pct = "Molecular",
                     culture_based_pct = "Culture-based",
                     microscopy_pct = "Microscopy",
                     field_observation_pct = "Field Observation",
                     computational_pct = "Computational")
    )
  
  kingdom_plot <- ggplot(kingdom_methods_long, 
                        aes(x = kingdom, y = percentage, fill = method)) +
    geom_col(position = "dodge") +
    labs(
      title = "Research Methods by Kingdom",
      x = "Kingdom",
      y = "Percentage of Abstracts",
      fill = "Method"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  save_plot(file.path(output_dir, "methods_by_kingdom.png"), kingdom_plot)
  
  # Methods by phylum plot
  phylum_methods_long <- methods_by_phylum %>%
    filter(total >= 5) %>%  # Filter to phyla with at least 5 mentions
    pivot_longer(
      cols = c(molecular_pct, culture_based_pct, microscopy_pct, 
               field_observation_pct, computational_pct),
      names_to = "method",
      values_to = "percentage"
    ) %>%
    mutate(
      method = recode(method,
                     molecular_pct = "Molecular",
                     culture_based_pct = "Culture-based",
                     microscopy_pct = "Microscopy",
                     field_observation_pct = "Field Observation",
                     computational_pct = "Computational")
    )
  
  phylum_plot <- ggplot(phylum_methods_long, 
                       aes(x = phylum, y = percentage, fill = method)) +
    geom_col(position = "dodge") +
    facet_wrap(~ kingdom, scales = "free_x") +
    labs(
      title = "Research Methods by Phylum",
      x = "Phylum",
      y = "Percentage of Abstracts",
      fill = "Method"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  save_plot(file.path(output_dir, "methods_by_phylum.png"), phylum_plot)
  
  # Analyze publication year trends if available
  year_trend_plot <- NULL
  
  # Check if publication_year exists in both dataframes
  has_pub_year <- "publication_year" %in% colnames(abstracts_df)
  
  if (has_pub_year) {
    # Make sure the column is properly joined
    methods_by_year <- taxa_methods_df %>%
      filter(!is.na(publication_year)) %>%
      group_by(publication_year) %>%
      summarise(
        total = n(),
        molecular = sum(molecular_methods, na.rm = TRUE) / total * 100,
        culture_based = sum(culture_based_methods, na.rm = TRUE) / total * 100,
        microscopy = sum(microscopy_methods, na.rm = TRUE) / total * 100,
        field_observation = sum(field_observation_methods, na.rm = TRUE) / total * 100,
        computational = sum(computational_methods, na.rm = TRUE) / total * 100,
        .groups = "drop"
      )
    
    year_methods_long <- methods_by_year %>%
      pivot_longer(
        cols = c(molecular, culture_based, microscopy, field_observation, computational),
        names_to = "method",
        values_to = "percentage"
      ) %>%
      mutate(
        method = recode(method,
                       molecular = "Molecular",
                       culture_based = "Culture-based",
                       microscopy = "Microscopy",
                       field_observation = "Field Observation",
                       computational = "Computational")
      )
    
    year_trend_plot <- ggplot(year_methods_long, 
                             aes(x = publication_year, y = percentage, color = method)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Research Methods Trends Over Time",
        x = "Publication Year",
        y = "Percentage of Abstracts",
        color = "Method"
      ) +
      theme_minimal()
    
    save_plot(file.path(output_dir, "methods_by_year.png"), year_trend_plot)
  }
  
  # Save method data for further analysis
  write_csv(methods_by_kingdom, file.path(output_dir, "methods_by_kingdom.csv"))
  write_csv(methods_by_phylum, file.path(output_dir, "methods_by_phylum.csv"))
  
  return(list(
    kingdom_plot = kingdom_plot,
    phylum_plot = phylum_plot,
    year_trend_plot = year_trend_plot,
    methods_by_kingdom = methods_by_kingdom,
    methods_by_phylum = methods_by_phylum
  ))
}

# Main function to run all visualizations
visualize_taxa_results <- function(taxa_results_file, abstracts_file = NULL, output_dir = "plots") {
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Load taxa results
  message("Loading taxa results from ", taxa_results_file)
  taxa_results <- read_csv(taxa_results_file, show_col_types = FALSE)
  
  # Run visualizations
  message("Creating phyla visualizations...")
  phyla_plots <- plot_abstracts_per_phyla(taxa_results, output_dir)
  
  message("Creating plant parts visualizations...")
  plant_parts_plots <- plot_plant_parts(taxa_results, output_dir)
  
  message("Creating taxonomic coverage visualizations...")
  taxonomic_coverage_plots <- plot_taxonomic_coverage(taxa_results, output_dir)
  
  # Run research methods visualization if abstracts file is provided
  methods_plots <- NULL
  if (!is.null(abstracts_file)) {
    message("Loading abstracts from ", abstracts_file)
    abstracts <- read_csv(abstracts_file, show_col_types = FALSE)
    
    message("Creating research methods visualizations...")
    methods_plots <- plot_research_methods(taxa_results, abstracts, output_dir)
  }
  
  message("All visualizations complete! Results saved to ", output_dir)
  
  return(list(
    phyla_plots = phyla_plots,
    plant_parts_plots = plant_parts_plots,
    taxonomic_coverage_plots = taxonomic_coverage_plots,
    methods_plots = methods_plots
  ))
}

# Example usage
if (interactive()) {
  message("To create visualizations from taxa detection results:")
  message('visualize_taxa_results("taxa_info_results.csv", "full_predictions_with_metadata.csv", "plots")')
}
