#Doing the same thing as in extrracting_plant_names but I'm trying a string approach instead of a token approach.


library(tidyverse)  
library(quanteda)
library(rgbif)
library(janitor)
library(vroom)
library(irlba)
library(furrr)
library(progressr)
library(purrr)
library(dplyr)
library(stringi)

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

# Plant parts -------------------------------------------------------------

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
  "coleoptile", "coleoptiles", "sporophyte", "sporophytes", "gametophyte", "gametophytes", # Reproductive Structures
  "ovule", "ovules", "filament", "filaments", "receptacle", "receptacles",
  "floret", "florets", "spikelet", "spikelets", "glume", "glumes", 
  "lemma", "lemmas", "palea", "paleae",
  
  # Photosynthetic/Leaf-related
  "mesophyll", "lamina", "laminae", "stipule", "stipules",
  
  # Anatomical/Structural Features
  "meristem", "meristems", "epidermis", "endodermis", 
  "trichome", "trichomes", "resin duct", "resin ducts",
  
  # Specialized Roots/Stems
  "aerial root", "aerial roots", "adventitious root", "adventitious roots",
  "haustorium", "haustoria", "tendril", "tendrils", "rhizome", "rhizomes",
  
  # Tissue Types
  "sclerenchyma", "collenchyma",
  
  # Seed-Related
  "hilum", "micropyle", "testa", "seed coat", "seed coats", 
  "aleurone", "embryo", "embryos",
  
  # Developmental
  "callus", "calluses", "primordium", "primordia", "apex", "apices",
  "procambium", "protoplast", "protoplasts"
)

# Create a named vector where each plural maps to a singular form
plant_part_groups <- c(
  # Common structures
  "fruits" = "fruit", "roots" = "root", "rhizoids" = "rhizoid", "leaves" = "leaf",
  "twigs" = "twig", "branches" = "branch", "stems" = "stem", "flowers" = "flower",
  "shoots" = "shoot", "seeds" = "seed", "nodes" = "node", "leaflets" = "leaflet",
  "pistils" = "pistil", "anthers" = "anther", "carpels" = "carpel", "sepals" = "sepal",
  "petals" = "petal", "stigmas" = "stigma", "styles" = "style", "ovaries" = "ovary",
  "calyces" = "calyx", "corollas" = "corolla", "peduncles" = "peduncle", "rachises" = "rachis",
  "inflorescences" = "inflorescence", "trunks" = "trunk", "buds" = "bud", "cones" = "cone",
  "tubers" = "tuber", "bulbs" = "bulb", "corms" = "corm", "cladodes" = "cladode",
  "vascular bundles" = "vascular bundle", "cotyledons" = "cotyledon", "hypocotyls" = "hypocotyl",
  "epicotyls" = "epicotyl", "flowering stems" = "flowering stem", "internodes" = "internode",
  "leaf veins" = "leaf vein", "leaf blades" = "leaf blade", "palmatations" = "palmate",
  "needles" = "needle", "fascicles" = "fascicle", "cuticles" = "cuticle", "stomata" = "stoma",
  "vascular cambiums" = "vascular cambium", "petioles" = "petiole", "axils" = "axil",
  "phyllodes" = "phyllode", "perianths" = "perianth", "rachillas" = "rachilla",
  "pedicels" = "pedicel", "lateral roots" = "lateral root", "taproots" = "taproot",
  "root caps" = "root cap", "root hairs" = "root hair", "pericycles" = "pericycle",
  "colleters" = "colleter", "scutella" = "scutellum", "coleoptiles" = "coleoptile",
  "sporophytes" = "sporophyte", "gametophytes" = "gametophyte",
  
  # Reproductive
  "ovules" = "ovule", "filaments" = "filament", "receptacles" = "receptacle",
  "florets" = "floret", "spikelets" = "spikelet", "glumes" = "glume", "lemmas" = "lemma",
  "paleae" = "palea",
  
  # Photosynthetic
  "laminae" = "lamina", "stipules" = "stipule",
  
  # Anatomical
  "meristems" = "meristem", "trichomes" = "trichome", "resin ducts" = "resin duct",
  
  # Specialized roots/stems
  "aerial roots" = "aerial root", "adventitious roots" = "adventitious root",
  "haustoria" = "haustorium", "tendrils" = "tendril", "rhizomes" = "rhizome",
  
  # Seed-related
  "seed coats" = "seed coat", "embryos" = "embryo",
  
  # Developmental
  "calluses" = "callus", "primordia" = "primordium", "apices" = "apex",
  
  # Protoplasmic
  "protoplasts" = "protoplast"
)


# Code --------------------------------------------------------------------



#download all accepted species from backbone because their api is overwhelmbed by this code
backbone <- vroom("gbif_backbone/Taxon.tsv", 
                  delim = "\t")

if (!file.exists("species.rds")) {
  species <- backbone %>%
    filter(kingdom %in% c("Plantae") & taxonRank == "species") #Include Fungi later. for now this is just plants.
  saveRDS(species, "species.rds") 
} else {
  species <- readRDS("species.rds")
}


labeled_abstracts <- read.csv("labeled_and_predicted_abstracts.csv")

#Real species. Includes extinct ones. Has synonyms for renamed ones.
accepted_names <- backbone %>%
  filter(taxonomicStatus == "accepted" & kingdom == "Plantae") %>%
  select(-acceptedNameUsageID)

plan(multisession)

handlers(global = TRUE)
handlers("txtprogressbar")


species_vec <- accepted_names$canonicalName %>% unique() %>% discard(~ is.na(.) | . == "")
genus_vec   <- accepted_names$genus %>% unique() %>% discard(~ is.na(.) | . == "")
family_vec  <- accepted_names$family %>% unique() %>% discard(~ is.na(.) | . == "")
phylum_vec  <- accepted_names$phylum %>% unique() %>% discard(~ is.na(.) | . == "")

# Pre-lowercase taxon values only
species_vec_lc <- tolower(species_vec)
genus_vec_lc <- tolower(genus_vec)
family_vec_lc <- tolower(family_vec)
phylum_vec_lc <- tolower(phylum_vec)

all_taxa <- c(
  setNames(species_vec_lc, rep("species", length(species_vec_lc))),
  setNames(genus_vec_lc, rep("genus", length(genus_vec_lc))),
  setNames(family_vec_lc, rep("family", length(family_vec_lc))),
  setNames(phylum_vec_lc, rep("phylum", length(phylum_vec_lc)))
)

find_all_taxon_matches <- function(text) {
  lower_text <- tolower(text)
  
  matched <- names(all_taxa)[
    stringi::stri_detect_fixed(lower_text, names(all_taxa), case_insensitive = TRUE)
  ]
  levels <- all_taxa[matched]
  
  if (length(matched) > 0) tibble(name = matched, level = levels) else NULL
}

# Wrapper per abstract
match_all_taxa <- function(abstract, id, label, threshold, presence, absence) {
  matches <- find_all_taxon_matches(abstract)
  if (is.null(matches)) return(NULL)
  matches %>%
    mutate(
      id = id, label = label, abstract = abstract,
      threshold = threshold, presence = presence, absence = absence
    )
}

test <- labeled_abstracts %>% slice_sample(n = 30)

# Run with progress bar
matches_df <- with_progress({
  p <- progressor(along = test$abstract)
  
  future_pmap_dfr(
    list(
      test$abstract,
      test$id,
      test$label,
      test$threshold,
      test$presence,
      test$absence
    ),
    ~{
      p()
      match_all_taxa(..1, ..2, ..3, ..4, ..5, ..6)
    },
    .options = furrr_options(seed = TRUE)
  )
})
#End of experiment

#Might save matches_df here

plant_species_df <- matches_df %>%
  filter(!is.na(level)) %>%
  mutate(phylum = str_to_sentence(str_to_lower(phylum)))

#Add in taxonomic data
plant_species_df <- plant_species_df %>%
  left_join(accepted_names, by = c("name" = "canonicalName"))