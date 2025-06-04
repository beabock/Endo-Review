
# Trying to extract plant names -------------------------------------------

# Load necessary libraries

library(tidyverse)  
library(quanteda)
library(rgbif)
library(furrr)
library(janitor)

#Need to go back and remove any abstracts that are just empty.
set.seed(123)

getwd()

# Load and sample 100 abstracts randomly
labeled_abstracts <- read.csv("full_predictions_with_metadata.csv") %>%
  clean_names() #%>%
  #sample_n(size = 100)

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

# Function to query GBIF and get additional fields (batch processing)
batch_validate_species <- function(names) {
  res <- name_backbone_checklist(names)  # Batch query
  
  # Ensure required columns exist before selecting
  required_columns <- c("canonicalName", "rank", "confidence", "matchType", "kingdom", "phylum", 
                        "class", "order", "family", "genus", "species", 
                        "kingdomKey", "phylumKey", "classKey", "orderKey", 
                        "familyKey", "genusKey", "speciesKey")
  
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
extract_plant_info <- function(text, abstract_id, predicted_label, valid_species_lookup) {
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
  plant_candidates <- tokens %>%
    tokens_ngrams(n = 2) %>%
    unlist() %>%
    gsub("_", " ", .) %>%
    sapply(correct_capitalization)
  
  # Validate genus-species combinations using precomputed lookup
  valid_species <- valid_species_lookup %>%
    filter(canonicalName %in% unique(plant_candidates))
  
  # Create the final output
  if (nrow(valid_species) > 0) {
    valid_species <- valid_species %>%
      mutate(id = abstract_id, predicted_label = predicted_label)
    final_df <- cbind(valid_species, as.data.frame(t(plant_parts_indicator)))
  } else {
    plant_parts_df <- as.data.frame(t(plant_parts_indicator))
    plant_parts_df <- cbind(
      data.frame(
        canonicalName = NA, rank = NA, confidence = NA, matchType = NA, kingdom = NA,
        phylum = NA, class = NA, order = NA, family = NA, genus = NA, species = NA,
        kingdomKey = NA, phylumKey = NA, classKey = NA, orderKey = NA, familyKey = NA,
        genusKey = NA, speciesKey = NA, id = abstract_id, predicted_label = predicted_label,
        stringsAsFactors = FALSE
      ),
      plant_parts_df
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

# Batch validate candidates. this part takes a while.
valid_species_lookup <- batch_validate_species(all_candidates)

# Apply the extraction function in parallel
plant_species_df <- future_pmap_dfr(
  list(
    presence_abstracts$abstract,
    presence_abstracts$id,
    presence_abstracts$predicted_label
  ),
  ~extract_plant_info(..1, ..2, ..3, valid_species_lookup)
)

# View the result
print(plant_species_df)

# Save the result as a CSV
write.csv(plant_species_df, "plant_info_results_all.csv", row.names = FALSE)

#plant_species_df <- read.csv("Results/plant_info_results.csv")

#Look at representation across plant phyla
expected_plant_phyla <- c(
  "Anthophyta",        # flowering plants (aka Magnoliophyta)
  "Pinophyta",         # conifers
  "Pteridophyta",      # ferns
  "Bryophyta",         # mosses
  "Marchantiophyta",   # liverworts
  "Lycopodiophyta",    # club mosses
  "Gnetophyta",        # gnetophytes
  "Cycadophyta",       # cycads
  "Ginkgophyta",       # ginkgo
  "Charophyta",        # green algae group most related to land plants
  "Chlorophyta"        # other green algae
)

observed_phyla <- unique(plant_species_df$phylum[!is.na(plant_species_df$phylum)])

missing_phyla <- setdiff(expected_plant_phyla, observed_phyla)

print(missing_phyla)

phylum_summary <- plant_species_df %>%
  filter(kingdom == "Plantae")%>%
  filter(!is.na(phylum)) %>%
  count(phylum, sort = TRUE)

phylum_summary %>%
  ggplot(aes(x = reorder(phylum, n), y = n)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(
    title = "Phylum Representation",
    x = "Phylum",
    y = "Number of Abstracts"
  ) +
  theme_minimal()


#Now for plant parts
plant_parts_summary <- plant_species_df %>%
  select(id, all_of(plant_parts_keywords)) %>%
  distinct() %>%  # remove duplicates in case same abstract has multiple species hits
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "plant_part", values_to = "n_abstracts")

# View result
print(plant_parts_summary %>% arrange(desc(n_abstracts)))