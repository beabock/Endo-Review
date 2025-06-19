
# Trying to extract plant names -------------------------------------------
#BB
#June 2025
#Extracting information from the labeled dataset of abstracts, including plant taxonomy, fungal taxonomy, and part of plant


# Load necessary libraries

library(tidyverse)  
library(quanteda)
library(rgbif)
library(furrr)
library(janitor)
library(vroom)

custom_theme <- theme_bw(base_size = 18)

theme_set(custom_theme)

#Need to go back and remove any abstracts that are just empty.
set.seed(123)

getwd()

cus_pal <- c(
  "#A1C181",  # soft sage green — for plants
  "#619B8A",  # muted teal — evokes moss or lichens
  "#C97E7E",  # dusty rose — for fungi like Russula or Hygrophoropsis
  "#D9AE94"   # pale mushroom beige — for caps and forest floor tones
)

save_plot <- function(filename, plot, width = 12, height = 7, units = "in", ...) {
  ggsave(filename, plot, width = width, height = height, units = units, ...)
}

# Code --------------------------------------------------------------------



#download all accepted species from backbone because their api is overwhelmbed by this code
backbone <- vroom("gbif_backbone/Taxon.tsv", 
                  delim = "\t",
                  col_select = c("taxonRank", "taxonomicStatus", "canonicalName", 
                                 "kingdom", "phylum", "class", "order", 
                                 "family", "genus"))

if (!file.exists("accepted_species.rds")) {
  accepted_species <- backbone %>%
    filter(taxonRank == "species", taxonomicStatus == "accepted") %>%
    select(canonicalName, kingdom, phylum, class, order, family, genus)
  saveRDS(accepted_species, "accepted_species.rds")
} else {
  accepted_species <- readRDS("accepted_species.rds")
}

# Load and sample 100 abstracts randomly
labeled_abstracts <- read.csv("full_predictions_with_metadata.csv") %>%
  clean_names() #%>%
  #sample_n(size = 5)

plan(multisession)

# Function to correct capitalization (Genus uppercase, species lowercase)
correct_capitalization <- function(name) {
  words <- unlist(strsplit(name, " "))
  if (length(words) == 2) {
    words[1] <- paste0(toupper(substring(words[1], 1, 1)), tolower(substring(words[1], 2)))
    words[2] <- tolower(words[2])
    return(paste(words, collapse = " "))
  }
  return(name)
}

#Might need to go back above to include all of these cols. 
# gbif_fields <- c("canonicalName", "rank", "confidence", "matchType", "kingdom", "phylum", 
#                  "class", "order", "family", "genus", "species", 
#                  "kingdomKey", "phylumKey", "classKey", "orderKey", 
#                  "familyKey", "genusKey", "speciesKey")

gbif_fields <- c("canonicalName", "rank", "confidence", "matchType", "kingdom", "phylum", 
                 "class", "order", "family", "genus")



# Function to query GBIF and get additional fields (batch processing)
batch_validate_species <- function(names) {
  res <- name_backbone_checklist(names)  # Batch query
  
  # Ensure required columns exist before selecting
  required_columns <- gbif_fields
  
  valid_data <- res %>%
    filter(status == "ACCEPTED" & (kingdom == "Plantae" | kingdom == "Fungi") & rank != "KINGDOM") %>%
    select(any_of(required_columns)) %>%
    mutate(across(.cols = matches("Key$"), .fns = as.character))
  
  return(valid_data)
}

#Look into other options for premade dictionaries, in case I'm missing anything.
plant_parts_keywords <- c(
  "fruit", "fruits", "root", "roots", "rhizoid", "rhizoids", "leaf", "leaves", 
  "twig", "twigs", "branch", "branches", "bark", "stems", "stem", "flowers", 
  "flower", "shoot", "shoots", "seed", "seeds", "node", "nodes", "leaflet", 
  "leaflets", "pistil", "pistils", "anther", "anthers", "carpel", "carpels", 
  "sepal", "sepals", "petal", "petals", "stigma", "stigmas", "style", "styles", 
  "ovary", "ovaries", "calyx", "calyces", "corolla", "corollas", "peduncle", 
  "peduncles", "rachis", "rachises", "inflorescence", "inflorescences", "trunk", 
  "trunks", "cork", "buds", "bud", "pollen", "cones", "cone", "tuber", "tubers", 
  "bulb", "bulbs", "corm", "corms", "cladode", "cladodes", "vascular bundle", 
  "vascular bundles", "xylem", "phloem", "cortex", "cortices", "endosperm", 
  "cotyledon", "cotyledons", "hypocotyl", "hypocotyls", "epicotyl", "epicotyls", 
  "flowering stem", "flowering stems", "internode", "internodes", "leaf vein", 
  "leaf veins", "leaf blade", "leaf blades", "palmate", "palmatations", "needle", 
  "needles", "fascicle", "fascicles", "cuticle", "cuticles", "stomata", "stoma", 
  "vascular cambium", "vascular cambiums", "petiole", "petioles", "axil", "axils", 
  "phyllode", "phyllodes", "perianth", "perianths", "rachilla", "rachillas", 
  "pedicel", "pedicels", "lateral root", "lateral roots", "taproot", "taproots", 
  "root cap", "root caps", "root hair", "root hairs", "lignin", "pith", "pericycle", 
  "pericycles", "parenchyma", "colleter", "colleters", "scutellum", "scutella", 
  "coleoptile", "coleoptiles", "sporophyte", "sporophytes", "gametophyte", "gametophytes"
)

# Function to extract plant info
extract_plant_info <- function(text, abstract_id, predicted_label, ngrams, valid_species_lookup){
  # Tokenize text
  tokens <- tokens(text, remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower()
  
  # Extract plant parts
  plant_parts_found <- unlist(tokens) %>%
    .[. %in% plant_parts_keywords] %>%
    unique()
  
  # Create binary indicator for plant parts
  plant_parts_indicator <- setNames(as.integer(plant_parts_keywords %in% plant_parts_found), plant_parts_keywords)
  
  # Generate ngrams and correct capitalization
  plant_candidates <- sapply(ngrams, correct_capitalization)
  
  # Validate genus-species combinations using precomputed lookup
  valid_species <- valid_species_lookup %>%
    filter(canonicalName %in% unique(plant_candidates))
  
  # Create the final output
  if (nrow(valid_species) > 0) {
    valid_species <- valid_species %>%
      mutate(id = abstract_id, predicted_label = predicted_label)
    final_df <- cbind(valid_species, as.data.frame(t(plant_parts_indicator)))
  } else {
    plant_parts_df <- cbind(
      data.frame(
        setNames(
          replicate(length(gbif_fields), NA, simplify = FALSE),
          gbif_fields
        ),
        id = abstract_id,
        predicted_label = predicted_label,
        stringsAsFactors = FALSE
      ),
      as.data.frame(t(plant_parts_indicator))
    )
    final_df <- plant_parts_df
  }
  
  return(final_df)
}

#This takes a while to run, have it go overnight.
# Precompute ngrams and species validation
abs <- labeled_abstracts %>%
 # filter(predicted_label == "Presence") %>%
  mutate(ngrams = map(abstract, ~ {
    tokens(.x, remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_tolower() %>%
      tokens_ngrams(n = 2) %>%
      unlist() %>%
      sapply(function(name) gsub("_", " ", name))
  }))

# Collect all unique candidate names for validation
all_candidates <- abs %>%
  pull(ngrams) %>%
  unlist() %>%
  unique() %>%
#  str_subset("^[A-Z][a-z]+ [a-z]+$") %>% 
  sapply(correct_capitalization) #Might be causing problems.

force_refresh <- FALSE #Change to TRUE if i want to clear the cache of the valid_species_lookup

# Batch validate candidates. this part takes a while.
if (file.exists("valid_species_lookup.rds") & !force_refresh) {
  valid_species_lookup <- readRDS("valid_species_lookup.rds")
} else {
  valid_species_lookup <- accepted_species %>%
    filter(canonicalName %in% all_candidates & kingdom %in% c("Plantae", "Fungi"))
  saveRDS(valid_species_lookup, "valid_species_lookup.rds")
}

# Apply the extraction function in parallel
plant_species_df <- future_pmap_dfr(
  list(
    abs$abstract,
    abs$id,
    abs$predicted_label,
    abs$ngrams
  ),
  ~extract_plant_info(..1, ..2, ..3, ..4, valid_species_lookup), .options = furrr_options(seed = TRUE)
)

# View the result
print(plant_species_df)

# Save the result as a CSV
#write.csv(plant_species_df, "plant_info_results_all.csv", row.names = FALSE)


#Can just read the above csv instead of running the above code


# Start here --------------------------------------------------------------

plant_species_df <- read.csv("plant_info_results_all.csv")

plant_species_df%>%
  group_by(predicted_label)%>%
  tally()


plant_species_df %>%
  filter(kingdom == "Plantae")%>%
  group_by(phylum)%>%
  tally()


plantae_key <- name_backbone(name = "Plantae")$usageKey

phyla <- name_lookup(
  higherTaxonKey = plantae_key, #Might switch this to kingdom_key at some point
  rank = "PHYLUM",
  limit = 5000
)$data

expected_plant_phyla <- unique(phyla$canonicalName)

fungi_key <- name_backbone(name = "Fungi")$usageKey

phyla_f <- name_lookup(
  higherTaxonKey = fungi_key,
  rank = "PHYLUM",
  limit = 5000
)
expected_fung_phyla <- unique(phyla_f$data$canonicalName)

#Curious about total number of species per phylum.

name_lookup(higherTaxonKey = 9, rank = "SPECIES", status = "ACCEPTED", limit = 0)

get_species_count <- function(phylum_key) {
  tryCatch({
    res <- name_lookup(
      higherTaxonKey = phylum_key,
      rank = "SPECIES",
      status = "ACCEPTED",
      limit = 0
    )
    return(res$meta$count)
  }, error = function(e) {
    return(NA_integer_)
  })
}


phylum_species_counts <- phyla %>%
  mutate(total_species = map_int(key, get_species_count))

phylum_species_counts_fungi <- phyla_f$data %>%
  mutate(total_species = map_int(key, get_species_count))


#Plot raw first
summarize_phyla <- function(df, expected_phyla, kingdom_filter = "Plantae", phylum_species_counts) {
  # Step 1: Filter the data
  df_filtered <- df %>%
    filter(kingdom == kingdom_filter, !is.na(phylum)) %>%
    distinct(id, phylum, predicted_label)
  
  # Step 2: Count abstracts per phylum and predicted_label
  phylum_counts <- df_filtered %>%
    count(phylum, predicted_label, name = "abstracts_with_label")
  
  # Step 3: Merge with species totals
  counts_joined <- phylum_counts %>%
    left_join(phylum_species_counts %>% select(canonicalName, total_species),
              by = c("phylum" = "canonicalName")) %>%
    mutate(ratio = abstracts_with_label / total_species)
  
  # Plot 1: Raw counts
  plot_raw <- ggplot(phylum_counts, aes(x = reorder(phylum, abstracts_with_label), y = abstracts_with_label)) +
    geom_col(aes(fill = predicted_label)) +
    coord_flip() +
    labs(
      title = paste("Abstract Mentions in", kingdom_filter, "Phyla"),
      x = "Phylum",
      y = "Abstracts Mentioned"
    ) +
    scale_fill_manual(values = cus_pal)
  
  # Plot 2: Normalized by species richness
  plot_ratio <- ggplot(counts_joined, aes(x = reorder(phylum, ratio), y = ratio)) +
    geom_col(aes(fill = predicted_label)) +
    coord_flip() +
    labs(
      title = paste("Abstract Mentions per Total Species in", kingdom_filter, "Phyla"),
      x = "Phylum",
      y = "Ratio: Abstracts / Known Species"
    ) +
    scale_fill_manual(values = cus_pal)
  
  # Option 1: return as a named list
  return(list(raw_plot = plot_raw, ratio_plot = plot_ratio))
  
}

plant_phyla_counts <- summarize_phyla(plant_species_df, expected_plant_phyla, kingdom_filter = "Plantae", phylum_species_counts)

plant_phyla_counts$raw_plot
plant_phyla_counts$ratio_plot


save_plot("plant_raw_species_counts.png", plant_phyla_counts$raw_plot)

save_plot("plant_norm_species_counts.png", plant_phyla_counts$ratio_plot)


saveRDS(plant_phyla_counts, file = "phylum_species_counts.rds")

#Think about this more

#now fungi
fung_phyla_counts <- summarize_phyla(
  df = plant_species_df,              
  expected_phyla = expected_fung_phyla,
  kingdom_filter = "Fungi",
  phylum_species_counts = phylum_species_counts_fungi
)


fung_phyla_counts$raw_plot
fung_phyla_counts$ratio_plot


save_plot("fung_raw_species_counts.png", fung_phyla_counts$raw_plot)
save_plot("fung_norm_species_counts.png", fung_phyla_counts$ratio_plot)



#Now for plant parts
plant_parts_summary_by_label <- plant_species_df %>%
  select(id, predicted_label, any_of(plant_parts_keywords)) %>% #fix this later back to all_of, need to rerun all code so that the plant_parts_keywords is updated when making plant_species_df
  distinct() %>%
  group_by(predicted_label) %>%
  summarise(across(any_of(plant_parts_keywords), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols = any_of(plant_parts_keywords),
    names_to = "plant_part",
    values_to = "n_abstracts"
  )

#Need to use whatever keywords I used for the training unfort

# View result

# Create a named vector where each plural maps to a singular form
plant_part_groups <- c(
  "roots" = "root",
  "fruits" = "fruit",
  "leaves" = "leaf",
  "twigs" = "twig",
  "branches" = "branch",
  "stems" = "stem",
  "flowers" = "flower",
  "shoots" = "shoot",
  "seeds" = "seed",
  "nodes" = "node",
  "leaflets" = "leaflet",
  "pistils" = "pistil",
  "anthers" = "anther",
  "carpels" = "carpel",
  "sepals" = "sepal",
  "petals" = "petal",
  "stigmas" = "stigma",
  "styles" = "style",
  "ovaries" = "ovary",
  "calyces" = "calyx",
  "corollas" = "corolla",
  "peduncles" = "peduncle",
  "rachises" = "rachis",
  "inflorescences" = "inflorescence",
  "trunks" = "trunk",
  "buds" = "bud",
  "cones" = "cone",
  "tubers" = "tuber",
  "bulbs" = "bulb",
  "corms" = "corm",
  "cladodes" = "cladode",
  "vascular bundles" = "vascular bundle",
  "cotyledons" = "cotyledon",
  "hypocotyls" = "hypocotyl",
  "epicotyls" = "epicotyl",
  "flowering stems" = "flowering stem",
  "internodes" = "internode",
  "leaf veins" = "leaf vein",
  "leaf blades" = "leaf blade",
  "palmatations" = "palmate",
  "needles" = "needle",
  "fascicles" = "fascicle",
  "cuticles" = "cuticle",
  "stomata" = "stoma",
  "vascular cambiums" = "vascular cambium",
  "petioles" = "petiole",
  "axils" = "axil",
  "phyllodes" = "phyllode",
  "perianths" = "perianth",
  "rachillas" = "rachilla",
  "pedicels" = "pedicel",
  "lateral roots" = "lateral root",
  "taproots" = "taproot",
  "root caps" = "root cap",
  "root hairs" = "root hair",
  "pericycles" = "pericycle",
  "colleters" = "colleter",
  "scutella" = "scutellum",
  "coleoptiles" = "coleoptile",
  "sporophytes" = "sporophyte",
  "gametophytes" = "gametophyte"
)

plant_parts_grouped <- plant_parts_summary_by_label %>%
  mutate(
    grouped_part = recode(plant_part, !!!plant_part_groups, .default = plant_part)
  ) %>%
  group_by(predicted_label, grouped_part) %>%
  summarise(n_abstracts = sum(n_abstracts), .groups = "drop")


saveRDS(plant_parts_grouped, file = "plant_parts_grouped.rds")


top_parts <- plant_parts_grouped %>%
  group_by(grouped_part) %>%
  summarise(total = sum(n_abstracts)) %>%
  filter(total > 75)

plant_parts_plot <- plant_parts_grouped %>%
  filter(grouped_part %in% top_parts$grouped_part) %>%
  ggplot(aes(x = reorder(grouped_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Mentions of Plant Parts (Grouped Singular/Plural)",
    x = "Plant Part",
    y = "Number of Abstracts"
  )+
  scale_fill_manual(values = cus_pal)

save_plot("plant_parts.png", plant_parts_plot)

# Outputs -----------------------------------------------------------------

phylum_species_counts <- readRDS("phylum_species_counts.rds")

phylum_species_counts

plant_parts_grouped <- readRDS("plant_parts_grouped.rds")

#think about if glomeromycota come up with the root plant part

#Create a list of all plant species and see what's missing. 