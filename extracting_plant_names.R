
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
library(irlba)

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
                  col_select = c(
                    "taxonRank",       # rank, to filter species
                    "taxonomicStatus", # accepted/synonym status
                    "canonicalName",   # scientific name
                    "genericName", #new
                 #   "scientificName", #scientific has more info than canonical name. don't use
                 "acceptedNameUsageID",
                 "originalNameUsageID",
                 "nomenclaturalStatus",
                 "taxonID",
                 "parentNameUsageID",
                 
                    "kingdom",
                    "phylum",
                    "class",
                    "order",
                    "family",
                    "genus"          # logical extinct flag (TRUE/FALSE)
                 
                  )) #Could use acceptedNameUsageID

if (!file.exists("species.rds")) {
  species <- backbone %>%
    filter(kingdom %in% c("Plantae", "Fungi") & taxonRank == "species") 
  saveRDS(species, "species.rds")
} else {
  species <- readRDS("species.rds")
}
#This file, species, is now all species, regardless of if they are valid. Can connect invalid species to valid ones. 

#Some full names are not listed as species but rather as subspecies or varieties. Think on this.

#Important!
#On a synonym, the accepted ID matches to original name usage ID

#e.g. see below.
test <- species %>%
  filter(originalNameUsageID == 2512800)


if (!file.exists("families.rds")) {
  families <- backbone %>%
    filter(kingdom %in% c("Plantae", "Fungi") & taxonRank == "family") 
  saveRDS(families, "families.rds")
} else {
  families <- readRDS("families.rds")
}
#TaxonID seems to work for families.


#Does genera need to be specific to the families they are in?
if (!file.exists("genera.rds")) {
  genera <- backbone %>%
    filter(kingdom %in% c("Plantae", "Fungi") & taxonRank == "genus") 
  saveRDS(genera, "genera.rds")
} else {
  genera <- readRDS("genera.rds")
}


# Load and sample 100 abstracts randomly
labeled_abstracts <- read.csv("full_predictions_with_metadata.csv") %>%
  clean_names() #%>%
  #sample_n(size = 5)

abstracts_long <- labeled_abstracts %>%
  pivot_longer(
    cols = c(label_loose, label_medium, label_strict),
    names_to = "threshold",
    values_to = "predicted_label"
  ) %>%
  filter(predicted_label %in% c("Presence", "Absence"))

#threshold_levels <- c("label_loose", "label_medium", "label_strict")
threshold_levels <- c("label_loose") #Just doing loose for now

training_abstracts <- read.csv("Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(!is.na(doi), authors != "", label %in% c("Presence", "Absence")) %>%
  crossing(threshold = threshold_levels) %>%  # duplicate across thresholds
  mutate(predicted_label = label,
         volume = as.character(volume))

labeled_abstracts <- bind_rows(training_abstracts, abstracts_long) %>%
  mutate(id = row_number()) %>%
  relocate(id)


absences <- labeled_abstracts %>%
  filter(threshold == "label_loose")%>%
  filter(predicted_label == "Absence")

#write.csv(absences, "absences.csv", row.names = F)

# Making a pca plot rq ----------------------------------------------------
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

gbif_fields <- c("canonicalName", "resolved_name", "rank", "confidence", "matchType", "kingdom", "phylum", 
                 "class", "order", "family", "genus")

#valid_species_lookup <- batch_validate_species(all_candidates)

test <- all_candidates[1:20] #Whats up with the quotes around things?

#Is parentName ID the thing to look at?

species %>%
  group_by(parentNameUsageID)%>%
  tally()%>%
  arrange()%>%
  slice_sample(n=2)

test <- species %>%
  filter(parentNameUsageID== 7293439)

#Okay got it. If something is a synonym, the value in acceptedNameID will match the taxonID in the accepted column. They will share parentNameUsageIDs.

accepted_names <- species %>%
  filter(taxonomicStatus == "accepted")%>%
  select(-acceptedNameUsageID)

batch_validate_species <- function(names, backbone_df = species) {
  names <- unique(names)
  
  # Step 1: Direct matches
  validated <- tibble(user_supplied_name = names) %>%
    left_join(backbone_df, by = c("user_supplied_name" = "canonicalName")) %>%
    rename(canonicalName = user_supplied_name)
  
  # Step 2: Pull synonyms and connect to accepted names
  syns <- validated %>%
    filter(taxonomicStatus != "accepted" & !is.na(acceptedNameUsageID)) %>%
    left_join(
      accepted_names,
      by = c("acceptedNameUsageID" = "taxonID", "parentNameUsageID"),
      suffix = c("", "_accepted")
    ) %>%
    mutate(canonicalName = canonicalName_accepted) %>%
    select(-ends_with("_accepted"))
  
  # Step 3: Keep accepted names as-is
  reals <- validated %>%
    filter(taxonomicStatus == "accepted")
  
  # Step 4: Combine and finalize
  final <- bind_rows(syns, reals) %>%
    filter(!is.na(canonicalName)) %>%
    mutate(
      resolved_name = canonicalName,
      status = ifelse(!is.na(kingdom), "ACCEPTED", "NO_MATCH"),
      acceptedScientificName = ifelse(status == "ACCEPTED", canonicalName, NA)
    )
  
  return(final)
}




#Refine this more. I think it's still not doing a great job.
# abs$abstract,
# abs$id,
# abs$predicted_label,
# abs$ngrams
# ),
# ~extract_plant_info(..1, ..2, ..3, ..4, valid_species_lookup),

# Function to extract plant info
extract_plant_info <- function(text, abstract_id, predicted_label, ngrams, valid_species_lookup) {
  # Tokenize abstract
  tokens_vec <- tokens(text, remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    unlist()
  
  # Identify plant part mentions
  plant_parts_found <- unique(tokens_vec[tokens_vec %in% plant_parts_keywords])
  plant_parts_indicator <- setNames(as.integer(plant_parts_keywords %in% plant_parts_found), plant_parts_keywords)
  
  # Candidate species names
  plant_candidates <- sapply(ngrams, correct_capitalization)
  
  # Species match
  valid_species <- valid_species_lookup %>%
    filter(canonicalName %in% plant_candidates)
  
  # Genus/family mentions (always tracked)
  genus_mentions <- unique(tokens_vec[tokens_vec %in% tolower(genera$canonicalName)])
  family_mentions <- unique(tokens_vec[tokens_vec %in% tolower(families$canonicalName)])
  
  # These flags are now independent (not mutually exclusive)
  mentions_species <- nrow(valid_species) > 0
  mentions_genus   <- length(genus_mentions) > 0
  mentions_family  <- length(family_mentions) > 0
  
  # Main result block
  if (mentions_species) {
    final_df <- valid_species %>%
      mutate(id = abstract_id, predicted_label = predicted_label)
    final_df <- cbind(final_df, as.data.frame(t(plant_parts_indicator)))
  } else {
    # Fallback placeholder
    placeholder <- data.frame(
      setNames(replicate(length(gbif_fields), NA, simplify = FALSE), gbif_fields),
      id = abstract_id,
      predicted_label = predicted_label,
      stringsAsFactors = FALSE
    )
    
    # Backfill from genus
    if (mentions_genus) {
      genus_info <- genera %>%
        filter(tolower(canonicalName) %in% genus_mentions) %>%
        left_join(species, by = "genus", relationship = "many-to-many") %>%
        filter(!is.na(family)) %>%
        slice(1)
      
      if (nrow(genus_info) > 0) {
        for (field in gbif_fields) {
          if (field %in% names(genus_info)) {
            placeholder[[field]] <- genus_info[[field]]
          }
        }
      }
    }
    
    # Backfill from family if no phylum yet
    if (mentions_family && is.na(placeholder$phylum)) {
      family_info <- families %>%
        filter(tolower(canonicalName) %in% family_mentions) %>%
        left_join(species, by = "family", relationship = "many-to-many") %>%
        slice(1)
      
      if (nrow(family_info) > 0) {
        for (field in gbif_fields) {
          if (field %in% names(family_info)) {
            placeholder[[field]] <- family_info[[field]]
          }
        }
      }
    }
    
    final_df <- cbind(placeholder, as.data.frame(t(plant_parts_indicator)))
  }
  
  # Add metadata
  final_df$mentions_species <- mentions_species
  final_df$mentions_genus   <- mentions_genus
  final_df$mentions_family  <- mentions_family
  final_df$mentioned_genera <- paste(genus_mentions, collapse = "; ")
  final_df$mentioned_families <- paste(family_mentions, collapse = "; ")
  
  return(final_df)
}


force_refresh = F

for (i in seq_along(threshold_levels)) {
  
  threshold_name <- threshold_levels[i]
  message("Processing threshold: ", threshold_name)
  
  # Filter abstracts for the current threshold
  abs <- labeled_abstracts %>%
    filter(threshold == threshold_name) %>%
    mutate(ngrams = map(abstract, ~ {
      tokens(.x, remove_punct = TRUE, remove_numbers = TRUE) %>%
        tokens_tolower() %>%
        tokens_ngrams(n = 2) %>%
        unlist() %>%
        sapply(function(name) gsub("_", " ", name))
    }))
  
  # Collect all unique candidate names
  all_candidates <- abs %>%
    pull(ngrams) %>%
    unlist() %>%
    unique() %>%
    sapply(correct_capitalization)
  
  # Load or generate GBIF validation lookup
  if (file.exists("valid_species_lookup.rds") && !force_refresh) {
    valid_species_lookup <- readRDS("valid_species_lookup.rds")
  } else {
    valid_species_lookup <- batch_validate_species(all_candidates)
    saveRDS(valid_species_lookup, "valid_species_lookup.rds")
  }
  
  # double check that valid species loopup isn't getting rid of any info I need
  if (!"resolved_name" %in% names(valid_species_lookup)) {
    stop("The 'resolved_name' column is missing from valid_species_lookup. Check batch_validate_species() output.")
  }
  
  # Extract plant and fungal info in parallel
  plant_species_df <- future_pmap_dfr(
    list(
      abs$abstract,
      abs$id,
      abs$predicted_label,
      abs$ngrams
    ),
    ~extract_plant_info(..1, ..2, ..3, ..4, valid_species_lookup),
    .options = furrr_options(seed = TRUE)
  )
  
  # Save results with threshold in name
  out_name <- paste0("taxa_info_results_", threshold_name, ".csv")
  write.csv(plant_species_df, out_name, row.names = FALSE)
  }

#Can just read the above csv instead of running the above code
#check that id is actually distinct.

# Start here --------------------------------------------------------------
#threshold_levels <- c("label_loose", "label_medium", "label_strict")
threshold_levels <- c("label_loose")
#plantae_key <- name_backbone(name = "Plantae")$usageKey
plantae_key <- 6


phyla <- name_lookup(
  higherTaxonKey = plantae_key, #Might switch this to kingdom_key at some point
  rank = "PHYLUM",
  status = "ACCEPTED",
  isExtinct = FALSE,
  limit = 5000
)$data

expected_plant_phyla <- unique(phyla$canonicalName)

fungi_key <- name_backbone(name = "Fungi")$usageKey

phyla_f <- name_lookup(
  higherTaxonKey = fungi_key,
  rank = "PHYLUM",
  status = "ACCEPTED",
  limit = 5000
)
expected_fung_phyla <- unique(phyla_f$data$canonicalName)

phylum_order_fungi <- sort(expected_fung_phyla)

# Define phylum_order once using alphabetical or desired order
phylum_order <- sort(expected_plant_phyla)


for (i in seq_along(threshold_levels)) {

  threshold_name <- threshold_levels[i]
  
plant_species_df <- read.csv(paste0("taxa_info_results_", threshold_name, ".csv"))

fungi_phyla_per_abstract <- plant_species_df %>%
  filter(kingdom == "Fungi") %>%
  group_by(id) %>%
  summarise(phyla = list(unique(phylum)), .groups = "drop")

# Step 2: Find abstracts where fungi mentioned are ONLY Glomeromycota
only_glom_abstracts <- fungi_phyla_per_abstract %>%
  filter(
    map_lgl(phyla, ~ {
      clean <- na.omit(.x)
      length(clean) > 0 &&
        all(clean == "Glomeromycota")
    })
  ) %>%
  pull(id)

plant_species_df <- plant_species_df %>%
  filter(!id %in% only_glom_abstracts) %>%
  filter(predicted_label %in% c("Absence", "Presence")) %>%
  group_by(id) %>%
  filter(!(any(predicted_label == "Presence") & all(is.na(phylum)))) %>%
  ungroup()

plant_species_df <- plant_species_df %>%
  filter(!is.na(phylum)) 


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
summarize_phyla <- function(df, expected_phyla, kingdom_filter = "Plantae", phylum_species_counts, threshold_name = "", phylum_order = NULL) {
  # Step 1: Filter the data
  df_filtered <- df %>%
    filter(kingdom == kingdom_filter, !is.na(phylum)) %>%
    distinct(id, phylum, predicted_label)
  
  # Step 2: Count abstracts per phylum and predicted_label
  observed_counts <- df_filtered %>%
    count(phylum, predicted_label, name = "abstracts_with_label")
  
  full_phyla_grid <- expand_grid(
    phylum = expected_phyla,
    predicted_label = c("Presence", "Absence")
  )
  
  phylum_counts <- full_phyla_grid %>%
    left_join(observed_counts, by = c("phylum", "predicted_label")) %>%
    mutate(abstracts_with_label = replace_na(abstracts_with_label, 0))
  
  
  phylum_counts$phylum <- factor(phylum_counts$phylum, levels = phylum_order)
  
  phylum_counts$predicted_label <- factor(phylum_counts$predicted_label, levels = c("Absence", "Presence"))
  
  # Step 3: Merge with species totals
  counts_joined <- phylum_counts %>%
    left_join(phylum_species_counts %>% select(canonicalName, total_species),
              by = c("phylum" = "canonicalName")) %>%
    mutate(ratio = abstracts_with_label / total_species)%>%
    filter(!is.na(phylum))
  
  # Ensure the phylum order is consistent in the second plot
  counts_joined$phylum <- factor(counts_joined$phylum, levels = phylum_order)
  
  counts_joined$predicted_label <- factor(counts_joined$predicted_label, levels = c("Absence", "Presence"))
  
  phylum_counts <- phylum_counts %>%
    arrange(phylum, predicted_label) %>%
    filter(!is.na(phylum))
  
  # Plot 1: Raw counts
  plot_raw <- ggplot(phylum_counts, aes(x = phylum, y = abstracts_with_label)) +
    geom_col(aes(fill = predicted_label)) +
    # geom_text(
    #   aes(label = abstracts_with_label),
    #   position = position_stack(vjust = 0.5),  # centers text in stacked segment
    #   size = 5,
    #   show.legend = FALSE
    # ) +
    scale_color_manual(values = cus_pal)+
    coord_flip() +
    labs(
      title = paste("Abstract Mentions in", kingdom_filter, "Phyla", "(", threshold_name, ")"),
      x = "Phylum",
      y = "Abstracts Mentioned"
    ) +
    scale_fill_manual(values = cus_pal) +
    expand_limits(y = max(phylum_counts$abstracts_with_label) * 1.1)
  
  # Plot 2: Normalized by species richness
  plot_ratio <- ggplot(counts_joined, aes(x = phylum, y = ratio)) +
    geom_col(aes(fill = predicted_label)) +
    coord_flip() +
    labs(
      title = paste("Abstract Mentions per Total Species in", kingdom_filter, "Phyla", "(", threshold_name, ")"),
      x = "Phylum",
      y = "Ratio: Abstracts / Known Species"
    ) +
    scale_fill_manual(values = cus_pal)+
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  
  return(list(raw_plot = plot_raw, ratio_plot = plot_ratio))
}

plant_phyla_counts <- summarize_phyla(plant_species_df, expected_plant_phyla, kingdom_filter = "Plantae", phylum_species_counts, threshold_name = threshold_name, phylum_order = phylum_order)

plant_phyla_counts$raw_plot
plant_phyla_counts$ratio_plot


save_plot(paste0("Results/", threshold_name, "/plant_raw_species_counts_", threshold_name, ".png"), plant_phyla_counts$raw_plot)

save_plot(paste0("Results/", threshold_name, "/plant_norm_species_counts_", threshold_name, ".png"), plant_phyla_counts$ratio_plot)


#now fungi
fung_phyla_counts <- summarize_phyla(
  df = plant_species_df,              
  expected_phyla = expected_fung_phyla,
  kingdom_filter = "Fungi",
  phylum_species_counts = phylum_species_counts_fungi,
  threshold_name = threshold_name,
  phylum_order = phylum_order_fungi
)


fung_phyla_counts$raw_plot
fung_phyla_counts$ratio_plot


save_plot(paste0("Results/", threshold_name, "/fung_raw_species_counts_", threshold_name, ".png"), fung_phyla_counts$raw_plot)
save_plot(paste0("Results/", threshold_name, "/fung_norm_species_counts_", threshold_name, ".png"), fung_phyla_counts$ratio_plot)



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

plant_parts_grouped <- plant_parts_summary_by_label %>%
  mutate(
    grouped_part = recode(plant_part, !!!plant_part_groups, .default = plant_part)
  ) %>%
  group_by(predicted_label, grouped_part) %>%
  summarise(n_abstracts = sum(n_abstracts), .groups = "drop")


top_parts <- plant_parts_grouped %>%
  group_by(grouped_part) %>%
  summarise(total = sum(n_abstracts)) %>%
  filter(total > 75)

bottom_parts <- plant_parts_grouped %>%
  group_by(grouped_part) %>%
  summarise(total = sum(n_abstracts)) %>%
  filter(total < 5)

plant_parts_plot_bottom <- plant_parts_grouped %>%
  filter(grouped_part %in% bottom_parts$grouped_part) %>%
  ggplot(aes(x = reorder(grouped_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Least Represented Plant Parts (Grouped Singular/Plural) (", threshold_name, ")"),
    x = "Plant Part",
    y = "Number of Abstracts"
  )+
  scale_fill_manual(values = cus_pal)



save_plot(paste0("Results/", threshold_name, "/plant_parts_bottom_", threshold_name, ".png"), plant_parts_plot_bottom)

plant_parts_plot <- plant_parts_grouped %>%
  filter(grouped_part %in% top_parts$grouped_part) %>%
  ggplot(aes(x = reorder(grouped_part, n_abstracts), y = n_abstracts, fill = predicted_label)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Mentions of Plant Parts (Grouped Singular/Plural) (", threshold_name, ")"),
    x = "Plant Part",
    y = "Number of Abstracts"
  )+
  scale_fill_manual(values = cus_pal)

save_plot(paste0("Results/", threshold_name, "/plant_parts_", threshold_name, ".png"), plant_parts_plot)

# Plant families now ------------------------------------------------------

plant_families_backbone <- backbone %>%
  filter(
    kingdom == "Plantae",
    taxonRank == "family",
    taxonomicStatus == "accepted",
    !is.na(phylum),
    !is.na(family) #Could add a filter here for extinct if I want to
  ) %>%
  distinct(phylum, family)

total_families_per_phylum <- plant_families_backbone %>%
  group_by(phylum) %>%
  summarise(total_families = n_distinct(family), .groups = "drop")

# Extract families present in your dataset
dataset_families <- plant_species_df %>%
  filter(kingdom == "Plantae", !is.na(phylum), !is.na(family)) %>%
  distinct(phylum, family)

# Count dataset families per phylum (observed)
families_in_dataset <- dataset_families %>%
  group_by(phylum) %>%
  summarise(n_families_found = n_distinct(family), .groups = "drop")

# Join and calculate missing families
families_coverage <- total_families_per_phylum %>%
  left_join(families_in_dataset, by = "phylum") %>%
  mutate(
    n_families_found = replace_na(n_families_found, 0),
    n_families_missing = total_families - n_families_found
  ) %>%
  arrange(phylum)

families_coverage$phylum <- factor(families_coverage$phylum, levels = phylum_order)

# Output coverage summary
print(families_coverage)
write.csv(families_coverage, paste0("Results/", threshold_name, "/plant_families_coverage_", threshold_name, ".csv"), row.names = FALSE)

# Optional: list of families present in backbone but missing from dataset
missing_families <- plant_families_backbone %>%
  anti_join(dataset_families, by = c("phylum", "family")) %>%
  arrange(phylum, family)

missing_fams_tracheo <- plant_families_backbone %>%
  anti_join(dataset_families, by = c("phylum", "family")) %>%
  filter(phylum == "Tracheophyta")%>%
  arrange(phylum, family)

print(missing_fams_tracheo)

write.csv(missing_families, paste0("Results/", threshold_name, "/missing_plant_families_", threshold_name, ".csv"), row.names = FALSE)

write.csv(missing_fams_tracheo, paste0("Results/", threshold_name, "/tracheo_missing_plant_families_", threshold_name, ".csv"), row.names = FALSE)

# Prepare for plotting
families_long <- families_coverage %>%
  filter(!is.na(phylum))%>%
  select(phylum, n_families_found, n_families_missing) %>%
  pivot_longer(cols = c(n_families_found, n_families_missing),
               names_to = "status",
               values_to = "count") %>%
  mutate(
    status = recode(status,
                    n_families_found = "Found",
                    n_families_missing = "Missing"),
    phylum = factor(phylum, levels = phylum_order)  # preserve order
  )


family_pal <- c(
  "Found" = "#A1C181",   # calm sage green — presence, success
  "Missing" = "#C97E7E"  # dusty rose — subtle red tone for absence
)


# Plot: Found vs Missing families by phylum
fams <- ggplot(families_long, aes(x = phylum, y = count, fill = status)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Plant Family Coverage (", threshold_name, ")"),
    x = "Phylum",
    y = "Number of Families",
    fill = "Coverage Status"
  ) +
  scale_fill_manual(values = family_pal)

fams

save_plot(paste0("Results/", threshold_name, "/families_missing_present_", threshold_name, ".png"), fams)

families_coverage_prop <- families_coverage %>%
  filter(!is.na(phylum))%>%
  mutate(
    prop_found = n_families_found / total_families,
    prop_missing = n_families_missing / total_families
  )

families_long_prop <- families_coverage_prop %>%
  select(phylum, prop_found, prop_missing) %>%
  pivot_longer(cols = c(prop_found, prop_missing),
               names_to = "status",
               values_to = "proportion") %>%
  mutate(
    status = recode(status,
                    prop_found = "Found",
                    prop_missing = "Missing"),
    phylum = factor(phylum, levels = phylum_order)
  )

fams_prop <- ggplot(families_long_prop, aes(x = phylum, y = proportion, fill = status)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  coord_flip() +
  labs(
    title = paste0("Proportional Plant Family Coverage (", threshold_name, ")"),
    x = "Phylum",
    y = "Proportion of Families",
    fill = "Coverage Status"
  ) +
  scale_fill_manual(values = family_pal) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

fams_prop

save_plot(paste0("Results/", threshold_name, "/families_missing_present_prop_", threshold_name, ".png"), fams_prop)

} #End loop
