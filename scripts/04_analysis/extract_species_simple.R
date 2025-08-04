# Simple Species Extraction for Classification Results
# B. Bock  
# July 30, 2025
# Updated: July 31, 2025 - Using new comprehensive search results
#
# Comprehensive script to extract:
# 1. Species information (plants and fungi)
# 2. Plant parts studied
# 3. Research methods (culture, microscopy, molecular)
# 4. Geographic locations: one thing I need to fix is niger is both a species name and a country.... the country niger is showing up as represented more than it is
#
# Data Source: Web of Science search conducted July 31, 2025
# Search Strategy: ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungus" OR "endophytic fungi")
#                 AND (plant* OR moss* OR bryophyte* OR liverwort* OR hornwort* OR fern* OR 
#                      lycophyte* OR pteridophyte* OR algae OR green alga* OR macroalga* OR 
#                      cyanobacteria OR cyanobiont* OR photobiont* OR lichen*)
#
# Focuses on the weighted ensemble (89.8% accuracy) predictions

library(tidyverse)
library(tictoc)
library(janitor)

# Source the detection functions
source("scripts/archive/optimized_taxa_detection.R")

cat("=== COMPREHENSIVE EXTRACTION PIPELINE ===\n")
cat("Processing weighted ensemble results (best ML performance)\n")
cat("Extracting: Species | Plant Parts | Methods | Geography\n\n")

# Define extraction functions ------------------------------------------------

# Function to detect research methods in abstracts (from visualize_taxa_results.R)
detect_research_methods <- function(text) {
  # Define method categories and their keywords
  method_categories <- list(
    molecular = c("pcr", "dna", "rna", "sequenc", "primer", "amplif", "gene", "genom", 
                 "transcript", "clone", "phylogen", "molecular", "extraction", "isolat", 
                 "genetic", "marker", "polymorphism", "nucleotide", "hybridiz", 
                 "rrna", "18s", "28s", "rdna", "barcode", "phylogeny"),
    
    culture_based = c("culture*", "isolat", "plate", "medium", "agar", "petri", "colony", 
                     "incubat", "sterile", "aseptic", "axenic", 
                     "ferment", "broth", "in vitro", "cultivation"),
    
    microscopy = c("microscop", "stain", "section", "histolog", "morpholog", "ultrastructur", 
                  "sem", "tem", "scanning electron", "transmission electron", "light microscop", 
                  "confocal", "fluorescen", "magnification", "micrograph", "optical")
  )
  
  text_lower <- tolower(text)
  results <- sapply(names(method_categories), function(category) {
    keywords <- method_categories[[category]]
    matches <- sapply(keywords, function(keyword) {
      grepl(keyword, text_lower)
    })
    any(matches)
  })
  
  # Create summary
  methods_found <- names(results)[results]
  return(list(
    molecular = results["molecular"],
    culture_based = results["culture_based"],
    microscopy = results["microscopy"],
    methods_detected = if(length(methods_found) > 0) paste(methods_found, collapse = "; ") else NA
  ))
}

# Function to detect geographic locations
detect_geographic_locations <- function(text) {
  # Comprehensive country lists categorized by Global North/South
  # Based on UN classifications and economic development indicators
  
  global_north_countries <- c(
  # North America
  "united states", "usa", "america", "canada", "greenland", "bermuda",
  
  # Europe (EU and high-income non-EU)
  "united kingdom", "uk", "england", "scotland", "wales", "northern ireland",
  "ireland", "germany", "france", "italy", "spain", "portugal", "netherlands",
  "belgium", "luxembourg", "austria", "switzerland", "denmark", "sweden",
  "norway", "finland", "iceland", "liechtenstein", "andorra", "monaco",
  "san marino", "vatican", "vatican city", "czech republic", "slovakia",
  "slovenia", "estonia", "latvia", "lithuania", "poland", "malta", "cyprus",
  "greece", "hungary", "croatia",
  
  # Southeastern and Eastern Europe (borderline/high-income)
  "romania", "bulgaria",

  # Asia-Pacific High Income
  "japan", "south korea", "republic of korea", "australia", "new zealand",
  "singapore", "taiwan", "hong kong", "macao", "israel", "brunei",

  # Others
  "russia"
)
  
  global_south_countries <- c(
  # Latin America & Caribbean
  "mexico", "brazil", "argentina", "chile", "colombia", "peru", "venezuela",
  "ecuador", "bolivia", "paraguay", "uruguay", "guyana", "suriname",
  "french guiana", "guatemala", "honduras", "el salvador", "nicaragua",
  "costa rica", "panama", "cuba", "haiti", "dominican republic", "jamaica",
  "trinidad and tobago", "barbados", "belize", "bahamas", "saint lucia",
  "saint vincent and the grenadines", "grenada", "dominica", "antigua and barbuda",
  "saint kitts and nevis", "saint kitts", "saint vincent", "antigua",
  "curaçao", "aruba", "sint maarten", "bonaire",

  # Africa
  "niger", "nigeria", "ethiopia", "egypt", "south africa", "kenya", "uganda",
  "tanzania", "ghana", "mozambique", "madagascar", "cameroon",
  "côte d’ivoire", "ivory coast", "niger", "mali", "zambia", "senegal",
  "malawi", "burkina faso", "chad", "rwanda", "guinea", "benin", "tunisia",
  "burundi", "liberia", "sierra leone", "lesotho", "namibia", "botswana",
  "gabon", "gambia", "mauritania", "eswatini", "swaziland", "djibouti",
  "central african republic", "republic of the congo", "congo", "democratic republic of the congo",
  "angola", "zimbabwe", "sudan", "south sudan", "libya", "algeria",
  "somalia", "eritrea", "guinea-bissau", "equatorial guinea",
  "sao tome and principe", "comoros", "cape verde", "seychelles",
  "reunion", "western sahara",

  # Asia
  "china", "india", "indonesia", "pakistan", "bangladesh", "vietnam",
  "philippines", "thailand", "myanmar", "burma", "cambodia", "laos",
  "nepal", "sri lanka", "malaysia", "bhutan", "maldives", "mongolia",
  "afghanistan", "north korea", "timor-leste", "east timor", "kazakhstan",
  "uzbekistan", "kyrgyzstan", "tajikistan", "turkmenistan",
  "armenia", "azerbaijan", "georgia",

  # Middle East
  "turkey", "iran", "iraq", "syria", "jordan", "lebanon", "palestine",
  "palestinian territories", "saudi arabia", "yemen", "united arab emirates",
  "oman", "qatar", "bahrain", "kuwait",

  # Pacific Islands
  "papua new guinea", "fiji", "solomon islands", "vanuatu", "samoa",
  "tonga", "kiribati", "palau", "nauru", "tuvalu", "marshall islands",
  "micronesia", "federated states of micronesia", "cook islands", "niue",
  "tokelau", "american samoa", "guam", "northern mariana islands",
  "french polynesia", "new caledonia", "wallis and futuna"
)
  
  # All countries combined
  all_countries <- c(global_north_countries, global_south_countries)
  
  continents <- c("africa", "asia", "europe", "north america", "south america", 
                  "australia", "oceania", "antarctica")
  
  regions <- c("mediterranean", "tropical", "temperate", "boreal", "arctic", "alpine", 
               "coastal", "mountain", "desert", "rainforest", "savanna", "grassland",
               "wetland", "forest", "woodland", "prairie", "steppe", "tundra", "taiga",
               "subtropical", "equatorial", "subantarctic", "subarctic", "montane",
               "lowland", "highland", "riparian", "littoral", "estuarine", "mangrove",
               "deciduous", "coniferous", "mixed forest", "cloud forest", "dry forest")
  
  text_lower <- tolower(text)
  
  # Find countries and categorize
  cleaned_countries <- sapply(all_countries, function(country) {
  if (country == "niger") {
    # Match only if "Niger" appears capitalized in original text or as "Republic of Niger"
    grepl("\\bRepublic of Niger\\b", text, ignore.case = TRUE) ||
    grepl("\\bNiger\\b", text)  # Capitalized only
  } else {
    grepl(paste0("\\b", country, "\\b"), text_lower)
  }
})

  countries_found <- all_countries[cleaned_countries]
  
  country_presence <- setNames(as.integer(all_countries %in% countries_found), all_countries)

  # Categorize found countries
  north_countries <- intersect(countries_found, global_north_countries)
  south_countries <- intersect(countries_found, global_south_countries)
  
  # Find continents
  continents_found <- continents[sapply(continents, function(continent) {
    grepl(paste0("\\b", continent, "\\b"), text_lower)
  })]
  
  # Find regions/ecosystems
  regions_found <- regions[sapply(regions, function(region) {
    grepl(paste0("\\b", region, "\\b"), text_lower)
  })]
  
  # Look for coordinate patterns (latitude/longitude)
  coord_pattern <- "\\b\\d{1,2}[°]?\\s*[NS]?\\s*,?\\s*\\d{1,3}[°]?\\s*[EW]?\\b"
  has_coordinates <- grepl(coord_pattern, text)
  
  return(list(
  countries = if(length(countries_found) > 0) paste(countries_found, collapse = "; ") else NA,
  global_north_countries = if(length(north_countries) > 0) paste(north_countries, collapse = "; ") else NA,
  global_south_countries = if(length(south_countries) > 0) paste(south_countries, collapse = "; ") else NA,
  continents = if(length(continents_found) > 0) paste(continents_found, collapse = "; ") else NA,
  regions = if(length(regions_found) > 0) paste(regions_found, collapse = "; ") else NA,
  has_coordinates = has_coordinates,
  geographic_info = paste(c(countries_found, continents_found, regions_found), collapse = "; "),
  country_presence = country_presence
))
}

# Function to detect plant parts (enhanced from existing)
detect_plant_parts <- function(text) {
  # Comprehensive plant parts keywords (deduplicated)
  plant_parts_keywords <- unique(c(
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
    "stamen", "stamens", "filament", "filaments", "receptacle",
    "berry", "berries", "drupe", "drupes", "achene", "achenes",
    "samara", "samaras", "capsule", "capsules", "silique", "siliques",
    
    # Specialized structures
    "rhizome", "rhizomes", "tuber", "tubers", "bulb", "bulbs",
    "corm", "corms", "tendril", "tendrils", "thorn", "thorns",
    "needle", "needles", "spine", "spines", "scale", "scales",
    "keel", "keels", "ligule", "ligules", "pulvinus", "pulvini",
    "lenticel", "lenticels", "haustorium", "haustoria", "tiller", "tillers",
    "cone", "cones", "pod", "pods", "runner", "runners", "stolon", "stolons",
    "pseudobulb", "pseudobulbs", "phylloclade", "phylloclades",
    
    # Galls and abnormal structures
    "gall", "galls", "witch broom", "witches broom", "canker", "cankers",
    "tumor", "tumors", "neoplasm", "neoplasms", "hyperplasia", "hypertrophy",
    "callus", "calli", "proliferation", "proliferations",
    
    # Anatomical features
    "xylem", "phloem", "cortex", "cortices", "epidermis", "endodermis",
    "mesophyll", "parenchyma", "sclerenchyma", "collenchyma",
    "stoma", "stomata", "cuticle", "cuticles", "trichome", "trichomes",
    "meristem", "meristems", "pericycle", "cambium", "cambia",
    "resin duct", "resin ducts", "vascular bundle", "vascular bundles",
    "pith", "guard cell", "guard cells", "lumen", "lumens",
    "chloroplast", "chloroplasts", "amyloplast", "amyloplasts",
    "vessel element", "vessel elements", "tracheid", "tracheids",
    "sieve tube", "sieve tubes", "companion cell", "companion cells",
    
    # Leaf parts
    "petiole", "petioles", "lamina", "laminae", "stipule", "stipules",
    "leaflet", "leaflets", "node", "nodes", "internode", "internodes",
    "midrib", "midribs", "vein", "veins", "margin", "margins",
    "blade", "blades", "sheath", "sheaths", "ochrea", "ochreae",
    
    # Root parts
    "taproot", "taproots", "fibrous root", "fibrous roots", "root hair", "root hairs",
    "root cap", "root caps", "lateral root", "lateral roots",
    "adventitious root", "adventitious roots", "aerial root", "aerial roots",
    "prop root", "prop roots", "buttress root", "buttress roots",
    
    # Wood and bark features
    "heartwood", "sapwood", "annual ring", "annual rings", "growth ring", "growth rings",
    "ray", "rays", "tylosis", "tyloses", "extractive", "extractives",
    "cellulose", "lignin", "hemicellulose", "pectin", "suberin",
    
    # Secretory structures
    "resin canal", "resin canals", "oil duct", "oil ducts", "latex vessel", "latex vessels",
    "mucilage canal", "mucilage canals", "secretory cell", "secretory cells",
    "glandular hair", "glandular hairs", "nectary", "nectaries",
    
    # Surface features
    "waxy bloom", "waxy blooms", "pubescence", "glaucous surface",
    "papilla", "papillae", "emergences"
  ))
  
  text_lower <- tolower(text)
  parts_found <- plant_parts_keywords[sapply(plant_parts_keywords, function(part) {
    grepl(paste0("\\b", part, "\\b"), text_lower)
  })]
  
  return(list(
    plant_parts_detected = if(length(parts_found) > 0) paste(parts_found, collapse = "; ") else NA,
    parts_count = length(parts_found)
  ))
}

# Step 1: Prepare the data ------------------------------------------------

cat("Step 1: Preparing classification results for species detection...\n")

# Load the relevant abstracts with predictions
classification_results <- read_csv("results/predictions/relevant_abstracts_with_pa_predictions.csv")

# Load and bind training dataset (only those with real DOIs)
cat("  Loading training dataset for inclusion...\n")
training_data_raw <- read_csv("data/raw/Training_labeled_abs_5.csv") %>%
  clean_names() %>%
  filter(!is.na(doi) & doi != "" & nchar(doi) > 5) %>%  # Only real DOIs
  filter(label %in% c("Presence", "Absence"))  # Only relevant labels

# Create training data that matches classification_results structure
training_data <- training_data_raw %>%
  select(article_title, abstract, authors, source_title, publication_year, doi, label) %>%
  mutate(
    id = max(classification_results$id, na.rm = TRUE) + row_number(),  # Unique IDs
    # Map to classification_results column names
    Relevant = ifelse(label == "Presence", TRUE, FALSE),
    relevance_loose = ifelse(label == "Presence", TRUE, FALSE),
    glmnet_pred = label,
    svm_pred = label,
    weighted_ensemble = label,
    threshold_ensemble = label,
    glmnet_prob_presence = ifelse(label == "Presence", 0.95, 0.05),
    glmnet_prob_absence = ifelse(label == "Absence", 0.95, 0.05),
    svm_prob_presence = ifelse(label == "Presence", 0.95, 0.05),
    svm_prob_absence = ifelse(label == "Absence", 0.95, 0.05),
    pa_loose = ifelse(label == "Presence", TRUE, FALSE),
    pa_medium = ifelse(label == "Presence", TRUE, FALSE),
    pa_strict = ifelse(label == "Presence", TRUE, FALSE),
    pa_super_strict = ifelse(label == "Presence", TRUE, FALSE),
    final_classification = label,
    conservative_classification = label,
    source = "training"
  ) %>%
  # Select only the columns that exist in classification_results, plus source
  select(all_of(names(classification_results)), source)

cat("  Added", nrow(training_data), "training abstracts with valid DOIs\n")

# Combine classification results with training data
combined_results <- classification_results %>%
  mutate(
    source = "classification",
    # Ensure consistent data types
    publication_year = as.character(publication_year),
    Relevant = as.character(Relevant),
    relevance_loose = as.character(relevance_loose),
    pa_loose = as.character(pa_loose),
    pa_medium = as.character(pa_medium),
    pa_strict = as.character(pa_strict),
    pa_super_strict = as.character(pa_super_strict)
  ) %>%
  bind_rows(
    training_data %>%
      mutate(
        # Ensure training data has consistent types
        publication_year = as.character(publication_year),
        Relevant = as.character(Relevant),
        relevance_loose = as.character(relevance_loose),
        pa_loose = as.character(pa_loose),
        pa_medium = as.character(pa_medium),
        pa_strict = as.character(pa_strict),
        pa_super_strict = as.character(pa_super_strict)
      )
  )

cat("  Combined dataset:", nrow(combined_results), "total abstracts\n")

# Focus on the weighted ensemble predictions (best performance) + training data
abstracts_for_species <- combined_results %>%
  filter(!is.na(weighted_ensemble)) %>%  # Has either prediction or training label
  # Keep ALL columns from combined_results to preserve metadata
  rename(
    title = article_title,
    predicted_label = weighted_ensemble
  ) %>%
  # Add confidence score
  mutate(
    confidence = pmax(glmnet_prob_presence, glmnet_prob_absence, na.rm = TRUE)
  )

cat("  Prepared", nrow(abstracts_for_species), "abstracts with weighted ensemble predictions\n")

# Create breakdown by prediction type
prediction_summary <- abstracts_for_species %>%
  count(predicted_label, name = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 1))

cat("  Prediction breakdown:\n")
print(prediction_summary)

# Step 2: Run species detection -------------------------------------------

cat("\nStep 2: Running species detection...\n")

# Check if species detection results already exist (recovery mechanism)
species_file <- "results/species_detection_weighted_ensemble.csv"
if (file.exists(species_file)) {
  cat("⚠️  Found existing species detection results. Do you want to:\n")
  cat("   - Skip species detection and use existing results\n")
  cat("   - Or delete this file and rerun from scratch\n")
  cat("   File:", species_file, "\n")
  
  # For now, we'll load existing results and continue
  cat("   Loading existing species detection results...\n")
  all_species_results <- read_csv(species_file, show_col_types = FALSE)
  cat("   ✅ Loaded", nrow(all_species_results), "existing species detection records\n")
  
  # Skip to step 2.5
  skip_species_detection <- FALSE #Changing this right now to force it to run
} else {
  skip_species_detection <- FALSE
}

if (!skip_species_detection) {

# Load species reference data and set up processing
if (file.exists("species.rds")) {
  species <- readRDS("species.rds")
} else if (file.exists("models/species.rds")) {
  species <- readRDS("models/species.rds")
} else {
  stop("Species reference data not found. Please ensure species.rds exists.")
}

cat("  Loaded species reference data:", nrow(species), "species records\n")

# Set up parallel processing and lookup tables
setup_parallel(workers = 2)
lookup_tables <- create_lookup_tables(species)

# Define plant parts keywords for species detection - comprehensive list (deduplicated)
plant_parts_keywords_species <- unique(c(
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
  "stamen", "stamens", "filament", "filaments", "receptacle",
  "berry", "berries", "drupe", "drupes", "achene", "achenes",
  "samara", "samaras", "capsule", "capsules", "silique", "siliques",
  
  # Specialized structures
  "rhizome", "rhizomes", "tuber", "tubers", "bulb", "bulbs",
  "corm", "corms", "tendril", "tendrils", "thorn", "thorns",
  "needle", "needles", "spine", "spines", "scale", "scales",
  "keel", "keels", "ligule", "ligules", "pulvinus", "pulvini",
  "lenticel", "lenticels", "haustorium", "haustoria", "tiller", "tillers",
  "cone", "cones", "pod", "pods", "runner", "runners", "stolon", "stolons",
  "pseudobulb", "pseudobulbs", "phylloclade", "phylloclades",
  
  # Galls and abnormal structures
  "gall", "galls", "witch broom", "witches broom", "canker", "cankers",
  "tumor", "tumors", "neoplasm", "neoplasms", "hyperplasia", "hypertrophy",
  "callus", "calli", "proliferation", "proliferations",
  
  # Anatomical features
  "xylem", "phloem", "cortex", "cortices", "epidermis", "endodermis",
  "mesophyll", "parenchyma", "sclerenchyma", "collenchyma",
  "stoma", "stomata", "cuticle", "cuticles", "trichome", "trichomes",
  "meristem", "meristems", "pericycle", "cambium", "cambia",
  "resin duct", "resin ducts", "vascular bundle", "vascular bundles",
  "pith", "guard cell", "guard cells", "lumen", "lumens",
  "chloroplast", "chloroplasts", "amyloplast", "amyloplasts",
  "vessel element", "vessel elements", "tracheid", "tracheids",
  "sieve tube", "sieve tubes", "companion cell", "companion cells",
  
  # Leaf parts
  "petiole", "petioles", "lamina", "laminae", "stipule", "stipules",
  "leaflet", "leaflets", "node", "nodes", "internode", "internodes",
  "midrib", "midribs", "vein", "veins", "margin", "margins",
  "blade", "blades", "sheath", "sheaths", "ochrea", "ochreae",
  
  # Root parts
  "taproot", "taproots", "fibrous root", "fibrous roots", "root hair", "root hairs",
  "root cap", "root caps", "lateral root", "lateral roots",
  "adventitious root", "adventitious roots", "aerial root", "aerial roots",
  "prop root", "prop roots", "buttress root", "buttress roots",
  
  # Wood and bark features
  "heartwood", "sapwood", "annual ring", "annual rings", "growth ring", "growth rings",
  "ray", "rays", "tylosis", "tyloses", "extractive", "extractives",
  "cellulose", "lignin", "hemicellulose", "pectin", "suberin",
  
  # Secretory structures
  "resin canal", "resin canals", "oil duct", "oil ducts", "latex vessel", "latex vessels",
  "mucilage canal", "mucilage canals", "secretory cell", "secretory cells",
  "glandular hair", "glandular hairs", "nectary", "nectaries",
  
  # Surface features
  "waxy bloom", "waxy blooms", "pubescence", "glaucous surface",
  "papilla", "papillae", "emergences"
))

# Process species detection in batches
tic("Species detection")

batch_size <- 25
n_batches <- ceiling(nrow(abstracts_for_species) / batch_size)

cat("Processing", nrow(abstracts_for_species), "abstracts in", n_batches, "batches...\n")

all_species_results <- map_dfr(1:n_batches, function(i) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(abstracts_for_species))
  
  batch_data <- abstracts_for_species[start_idx:end_idx, ]
  
  cat("  Processing batch", i, "of", n_batches, "(rows", start_idx, "to", end_idx, ")\n")
  
  # Process this batch
  batch_results <- process_abstracts_parallel(
    abstracts = batch_data,
    lookup_tables = lookup_tables,
    plant_parts_keywords = plant_parts_keywords_species,
    batch_size = 10,
    workers = 1
  )
  
  # Save intermediate results every 10 batches to prevent data loss
  if (i %% 10 == 0) {
    temp_file <- paste0("results/temp_species_batch_", i, ".csv")
    write_csv(batch_results, temp_file)
    cat("    ✅ Saved intermediate results to", temp_file, "\n")
  }
  
  return(batch_results)
})

# Save species detection results
write_csv(all_species_results, "results/species_detection_weighted_ensemble.csv")
cat("Species detection completed. Results saved to: results/species_detection_weighted_ensemble.csv\n")

toc()

} # End of species detection if-block

# Step 2.5: Extract additional information -----------------------------------

cat("\nStep 2.5: Extracting plant parts, methods, and geographic information...\n")
tic("Additional information extraction")

# Apply extraction functions to all abstracts
cat("  Extracting research methods...\n")
methods_results <- map_dfr(1:nrow(abstracts_for_species), function(i) {
  if (i %% 100 == 0) cat("    Processed", i, "of", nrow(abstracts_for_species), "abstracts\n")
  
  abstract_text <- abstracts_for_species$abstract[i]
  if (is.na(abstract_text)) abstract_text <- ""
  
  # Extract methods
  methods <- detect_research_methods(abstract_text)
  
  # Extract plant parts  
  plant_parts <- detect_plant_parts(abstract_text)
  
  # Extract geographic info
  geography <- detect_geographic_locations(abstract_text)
  
  return(data.frame(
    id = abstracts_for_species$id[i],
    molecular_methods = methods$molecular,
    culture_based_methods = methods$culture_based,
    microscopy_methods = methods$microscopy,
    methods_summary = methods$methods_detected,
    plant_parts_detected = plant_parts$plant_parts_detected,
    parts_count = plant_parts$parts_count,
    countries_detected = geography$countries,
    global_north_countries = geography$global_north_countries,
    global_south_countries = geography$global_south_countries,
    continents_detected = geography$continents,
    regions_detected = geography$regions,
    has_coordinates = geography$has_coordinates,
    geographic_summary = geography$geographic_info
  ))
})

cat("  Additional information extraction completed\n")
toc()

# Step 3: Combine and analyze results -----------------------------------------

cat("\nStep 3: Combining species and additional information...\n")

if (file.exists("results/species_detection_weighted_ensemble.csv")) {
  
  # Load species detection results
  species_results <- read_csv("results/species_detection_weighted_ensemble.csv", show_col_types = FALSE)
  
  # Combine with additional extraction results
  comprehensive_results <- species_results %>%
    left_join(methods_results, by = "id") %>%
    # Add ALL original classification data and metadata
    left_join(
      abstracts_for_species %>% 
        # Select metadata columns that aren't already in species_results
        select(id, abstract, authors, source_title, publication_year, doi, 
               source, confidence, glmnet_prob_presence, glmnet_prob_absence,
               # Include any other prediction columns
               starts_with("glmnet_"), starts_with("svm_"), 
               starts_with("pa_"), contains("ensemble"), 
               contains("Relevant"), contains("classification")),
      by = "id"
    )
  
  # Save comprehensive results
  write_csv(comprehensive_results, "results/comprehensive_extraction_results.csv")
  
  # Calculate total unique abstracts for accurate reporting
  total_unique_abstracts <- length(unique(comprehensive_results$id))
  
  # Report on metadata preservation
  cat("Metadata check in comprehensive results:\n")
  metadata_cols <- c("abstract", "authors", "source_title", "publication_year", "doi", "source")
  for (col in metadata_cols) {
    if (col %in% names(comprehensive_results)) {
      # Count unique abstracts with non-NA values for this column
      non_na_count <- comprehensive_results %>%
        group_by(id) %>%
        summarise(has_data = any(!is.na(.data[[col]])), .groups = "drop") %>%
        pull(has_data) %>%
        sum()
      cat("  ✓", col, ":", non_na_count, "abstracts with data out of", total_unique_abstracts, "total\n")
    } else {
      cat("  ✗", col, ": MISSING from comprehensive results\n")
    }
  }
  
  # Also report total columns
  cat("  Total columns in comprehensive results:", ncol(comprehensive_results), "\n")
  cat("  Key sections: Species info, Plant parts, Methods, Geography, Metadata\n")
  
  cat("Analysis of comprehensive extraction results:\n")
  cat("  Total unique abstracts processed:", total_unique_abstracts, "\n")

  # Species analysis (check for actual species column names)
  species_columns <- c("resolved_name", "canonicalName", "acceptedScientificName")
  species_col <- species_columns[species_columns %in% names(comprehensive_results)][1]
  
  if (!is.na(species_col)) {
    # Count unique abstracts with species detected
    abstracts_with_species <- comprehensive_results %>%
      group_by(id) %>%
      summarise(has_species = any(!is.na(.data[[species_col]])), .groups = "drop") %>%
      pull(has_species) %>%
      sum()
    
    cat("  Abstracts with species detected:", abstracts_with_species, 
        "(", round(100 * abstracts_with_species / total_unique_abstracts, 1), "%)\n")

    # Unique species
    unique_species <- comprehensive_results %>%
      filter(!is.na(.data[[species_col]])) %>%
      distinct(.data[[species_col]]) %>%
      nrow()
  } else {
    cat("  No species column found in results\n")
    abstracts_with_species <- 0
    unique_species <- 0
  }
  
  cat("  Unique species detected:", unique_species, "\n")
  
  # Methods analysis - count unique abstracts
  methods_summary <- comprehensive_results %>%
    group_by(id) %>%
    summarise(
      molecular = any(molecular_methods, na.rm = TRUE),
      culture = any(culture_based_methods, na.rm = TRUE),
      microscopy = any(microscopy_methods, na.rm = TRUE),
      has_methods = any(!is.na(methods_summary)),
      .groups = "drop"
    ) %>%
    summarise(
      molecular = sum(molecular),
      culture = sum(culture),
      microscopy = sum(microscopy),
      total_with_methods = sum(has_methods)
    )
  
  cat("  Research methods detected:\n")
  cat("    Molecular methods:", methods_summary$molecular, "abstracts\n")
  cat("    Culture-based methods:", methods_summary$culture, "abstracts\n") 
  cat("    Microscopy methods:", methods_summary$microscopy, "abstracts\n")
  cat("    Abstracts with any method info:", methods_summary$total_with_methods, "\n")
  
  # Plant parts analysis - count unique abstracts
  abstracts_with_parts <- comprehensive_results %>%
    group_by(id) %>%
    summarise(has_parts = any(!is.na(plant_parts_detected)), .groups = "drop") %>%
    pull(has_parts) %>%
    sum()
  
  cat("  Plant parts information:\n")
  cat("    Abstracts with plant parts:", abstracts_with_parts, 
      "(", round(100 * abstracts_with_parts / total_unique_abstracts, 1), "%)\n")
  
  # Geographic analysis - count unique abstracts
  geographic_summary <- comprehensive_results %>%
    group_by(id) %>%
    summarise(
      with_countries = any(!is.na(countries_detected)),
      with_global_north = any(!is.na(global_north_countries)),
      with_global_south = any(!is.na(global_south_countries)),
      with_continents = any(!is.na(continents_detected)),
      with_regions = any(!is.na(regions_detected)),
      with_coordinates = any(has_coordinates, na.rm = TRUE),
      with_any_geography = any(!is.na(countries_detected) | !is.na(continents_detected) | !is.na(regions_detected)),
      .groups = "drop"
    ) %>%
    summarise(
      with_countries = sum(with_countries),
      with_global_north = sum(with_global_north),
      with_global_south = sum(with_global_south),
      with_continents = sum(with_continents),
      with_regions = sum(with_regions),
      with_coordinates = sum(with_coordinates),
      with_any_geography = sum(with_any_geography)
    )
  
  cat("  Geographic information:\n")
  cat("    Abstracts with countries:", geographic_summary$with_countries, "\n")
  cat("    Abstracts with Global North countries:", geographic_summary$with_global_north, "\n")
  cat("    Abstracts with Global South countries:", geographic_summary$with_global_south, "\n")
  cat("    Abstracts with continents:", geographic_summary$with_continents, "\n")
  cat("    Abstracts with regions:", geographic_summary$with_regions, "\n")
  cat("    Abstracts with coordinates:", geographic_summary$with_coordinates, "\n")
  cat("    Abstracts with any geographic info:", geographic_summary$with_any_geography, "\n")
  
  # Kingdom analysis (check for kingdom column)
  kingdom_columns <- c("kingdom", "kingdom.x", "kingdom.y")
  kingdom_col <- kingdom_columns[kingdom_columns %in% names(comprehensive_results)][1]
  
  if (!is.na(kingdom_col) && !is.na(species_col)) {
    kingdom_summary <- comprehensive_results %>%
      filter(!is.na(.data[[species_col]])) %>%
      count(.data[[kingdom_col]], name = "abstracts") %>%
      arrange(desc(abstracts))
  } else {
    kingdom_summary <- data.frame()
  }
  
  if (nrow(kingdom_summary) > 0) {
    cat("  Species by kingdom:\n")
    for (i in 1:nrow(kingdom_summary)) {
      kingdom_name <- kingdom_summary[[kingdom_col]][i]
      if (!is.na(kingdom_name)) {
        cat("    ", kingdom_name, ":", kingdom_summary$abstracts[i], "abstracts\n")
      }
    }
  }
  
  # Prediction type analysis - count unique abstracts per prediction type
  prediction_analysis <- comprehensive_results %>%
    group_by(id, predicted_label) %>%
    summarise(
      has_species = if(!is.na(species_col)) any(!is.na(.data[[species_col]])) else FALSE,
      molecular = any(molecular_methods, na.rm = TRUE),
      culture = any(culture_based_methods, na.rm = TRUE),
      microscopy = any(microscopy_methods, na.rm = TRUE),
      with_geography = any(!is.na(countries_detected) | !is.na(continents_detected) | !is.na(regions_detected)),
      .groups = "drop"
    ) %>%
    group_by(predicted_label) %>%
    summarise(
      total = n(),
      with_species = sum(has_species),
      molecular = sum(molecular),
      culture = sum(culture),
      microscopy = sum(microscopy),
      with_geography = sum(with_geography),
      .groups = "drop"
    )
  
  cat("  Analysis by prediction type:\n")
  print(prediction_analysis)
  
  # Create detailed summary report
  capture.output({
    cat("=== COMPREHENSIVE EXTRACTION SUMMARY REPORT ===\n")
    cat("Generated:", Sys.time(), "\n")
    cat("Input: Weighted ensemble predictions (89.8% ML accuracy)\n")
    cat("Extractions: Species, Plant Parts, Methods, Geography\n\n")
    
    cat("OVERVIEW:\n")
    cat("Total unique abstracts processed:", total_unique_abstracts, "\n")
    cat("Abstracts with species:", abstracts_with_species, 
        "(", round(100 * abstracts_with_species / total_unique_abstracts, 1), "%)\n")
    cat("Unique species detected:", unique_species, "\n\n")
    
    cat("RESEARCH METHODS:\n")
    cat("Molecular methods:", methods_summary$molecular, "abstracts\n")
    cat("Culture-based methods:", methods_summary$culture, "abstracts\n")
    cat("Microscopy methods:", methods_summary$microscopy, "abstracts\n")
    cat("Any method information:", methods_summary$total_with_methods, "abstracts\n\n")
    
    cat("PLANT PARTS:\n")
    cat("Abstracts with plant parts:", abstracts_with_parts, 
        "(", round(100 * abstracts_with_parts / total_unique_abstracts, 1), "%)\n\n")
    
    cat("GEOGRAPHIC INFORMATION:\n")
    cat("Countries mentioned:", geographic_summary$with_countries, "abstracts\n")
    cat("Global North countries:", geographic_summary$with_global_north, "abstracts\n")
    cat("Global South countries:", geographic_summary$with_global_south, "abstracts\n")
    cat("Continents mentioned:", geographic_summary$with_continents, "abstracts\n")
    cat("Regions/ecosystems mentioned:", geographic_summary$with_regions, "abstracts\n")
    cat("Coordinates provided:", geographic_summary$with_coordinates, "abstracts\n")
    cat("Any geographic info:", geographic_summary$with_any_geography, "abstracts\n\n")
    
    cat("SPECIES BY KINGDOM:\n")
    if (nrow(kingdom_summary) > 0) {
      for (i in 1:nrow(kingdom_summary)) {
        kingdom_name <- kingdom_summary[[kingdom_col]][i]
        if (!is.na(kingdom_name)) {
          cat(kingdom_name, ":", kingdom_summary$abstracts[i], "abstracts\n")
        }
      }
    }
    cat("\n")
    
    cat("ANALYSIS BY PREDICTION TYPE:\n")
    print(prediction_analysis)
    cat("\n")
    
    cat("DATA QUALITY INDICATORS:\n")
    cat("1. Species detection rate: ", round(100 * abstracts_with_species / total_unique_abstracts, 1), "%\n")
    cat("2. Method information coverage: ", round(100 * methods_summary$total_with_methods / total_unique_abstracts, 1), "%\n")
    cat("3. Plant parts coverage: ", round(100 * abstracts_with_parts / total_unique_abstracts, 1), "%\n")
    cat("4. Geographic coverage: ", round(100 * geographic_summary$with_any_geography / total_unique_abstracts, 1), "%\n\n")
    
    cat("RECOMMENDATIONS:\n")
    cat("1. Focus manual review on abstracts with species + methods + geography\n")
    cat("2. Prioritize 'Presence' predictions with comprehensive information\n")
    cat("3. Review 'Absence' predictions with species detected (potential misclassification)\n")
    cat("4. Use geographic and method information for study characterization\n")
    cat("5. Consider plant parts information for endophyte ecology analysis\n")
  }, file = "results/comprehensive_extraction_report.txt")
  
  cat("Comprehensive report saved to: results/comprehensive_extraction_report.txt\n")
  
} else {
  cat("No species detection results found - running methods and geography extraction only.\n")
  
  # Save just the additional extraction results
  write_csv(methods_results, "results/methods_geography_extraction.csv")
  cat("Methods and geography results saved to: results/methods_geography_extraction.csv\n")
}

cat("\n=== COMPREHENSIVE EXTRACTION COMPLETE ===\n")
cat("Key output files:\n")
cat("- results/comprehensive_extraction_results.csv: Complete results with all extractions\n")
cat("- results/species_detection_weighted_ensemble.csv: Species detection details\n") 
cat("- results/comprehensive_extraction_report.txt: Detailed analysis report\n\n")
cat("Data extracted:\n")
cat("✓ Species identification (plants and fungi)\n")
cat("✓ Plant parts studied (roots, leaves, stems, etc.)\n")
cat("✓ Research methods (molecular, culture, microscopy)\n")
cat("✓ Geographic locations (countries, regions, coordinates)\n\n")
cat("Next steps:\n")
cat("1. Review comprehensive_extraction_results.csv for systematic review data\n")
cat("2. Use geographic and method information for study characterization\n")
cat("3. Focus on abstracts with complete information for priority review\n")
cat("4. Consider running visualization scripts for deeper analysis\n\n")

cat("Comprehensive extraction pipeline complete! 🧬🔬🌍\n")
