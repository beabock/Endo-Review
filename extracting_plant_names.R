
# Trying to extract plant names -------------------------------------------

# Load necessary libraries

library(tidyverse)  
library(quanteda)
library(rgbif)
library(furrr)
library(janitor)

theme_set(theme_bw())

#Need to go back and remove any abstracts that are just empty.
set.seed(123)

getwd()

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

gbif_fields <- c("canonicalName", "rank", "confidence", "matchType", "kingdom", "phylum", 
                 "class", "order", "family", "genus", "species", 
                 "kingdomKey", "phylumKey", "classKey", "orderKey", 
                 "familyKey", "genusKey", "speciesKey")


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
# Precompute ngrams and species validation, currently only pulling from Presence
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
  sapply(correct_capitalization)

force_refresh <- FALSE #Change to TRUE if i want to clear the cache of the valid_species_lookup

# Batch validate candidates. this part takes a while.
if (file.exists("valid_species_lookup.rds")) {
  valid_species_lookup <- readRDS("valid_species_lookup.rds")
} else {
  valid_species_lookup <- batch_validate_species(all_candidates)
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
write.csv(plant_species_df, "plant_info_results_all.csv", row.names = FALSE)

#Only did 147 abstracts

#plant_species_df <- read.csv("plant_info_results.csv")



plantae_key <- name_backbone(name = "Plantae")$usageKey

phyla <- name_lookup(
  higherTaxonKey = plantae_key,
  rank = "PHYLUM",
  limit = 5000
)
expected_plant_phyla <- unique(phyla$data$canonicalName)

fungi_key <- name_backbone(name = "Fungi")$usageKey

phyla_f <- name_lookup(
  higherTaxonKey = fungi_key,
  rank = "PHYLUM",
  limit = 5000
)
expected_fung_phyla <- unique(phyla_f$data$canonicalName)

summarize_phyla <- function(df, expected_phyla, kingdom_filter = "Plantae") {
  # Filter by kingdom and keep only phyla with abstract IDs
  df_filtered <- df %>%
    filter(kingdom == kingdom_filter, !is.na(phylum)) %>%
    distinct(id, phylum, predicted_label)  # unique per abstract, phylum, and label
  
  # Observed and missing phyla
  obs <- unique(df_filtered$phylum)
  missing <- setdiff(expected_phyla, obs)
  
  cat("\nMissing phyla:\n")
  print(missing)
  cat("\nObserved phyla:\n")
  print(obs)
  
  total_abstracts <- df %>% distinct(id) %>% nrow()
  cat("Total abstracts:", total_abstracts, "\n")
  
  # Count abstracts per phylum and predicted_label
  phylum_counts <- df_filtered %>%
    count(phylum, predicted_label, sort = TRUE)
  
  # Plot
  ggplot(phylum_counts, aes(x = reorder(phylum, n), y = n)) +
    geom_col(fill = ifelse(kingdom_filter == "Plantae", "forestgreen", "darkorchid")) +
    coord_flip() +
    facet_wrap(~predicted_label) +
    labs(
      title = paste("Phylum Representation in", kingdom_filter, "Abstracts"),
      x = "Phylum",
      y = "Number of Abstracts"
    ) 
}


summarize_phyla(plant_species_df, expected_plant_phyla)

summarize_phyla(plant_species_df, expected_fung_phyla, kingdom_filter = "Fungi")

#Now for plant parts
plant_parts_summary_by_label <- plant_species_df %>%
  select(id, predicted_label, all_of(plant_parts_keywords)) %>%
  distinct() %>%
  group_by(predicted_label) %>%
  summarise(across(all_of(plant_parts_keywords), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols = all_of(plant_parts_keywords),
    names_to = "plant_part",
    values_to = "n_abstracts"
  )

# View result
print(plant_parts_summary_by_label %>% arrange(predicted_label, desc(n_abstracts)))

ggplot(plant_parts_summary_by_label, aes(x = reorder(plant_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Mentions of Plant Parts by Predicted Label",
    x = "Plant Part",
    y = "Number of Abstracts"
  ) +
  theme_minimal()
