# Reference Data Utilities for Endophyte Analysis
# B. Bock
# August 21, 2025
#
# Centralized definitions for:
# - Country classifications (Global North/South)
# - Plant parts keywords
# - Geographic regions and ecosystems
# - Method keywords
# 
# This script consolidates reference data used across multiple analysis scripts
# to ensure consistency and avoid duplication.

# Function to get comprehensive country classification
get_country_classifications <- function() {
  # Comprehensive list of countries with Global North/South classification
  # Based on UN classification and economic development indicators
  country_classifications <- tibble::tribble(
    ~country, ~development_focus,
    
    # Global North countries (developed economies)
    "United States", "Global North",
    "USA", "Global North",
    "US", "Global North", 
    "Canada", "Global North", 
    "United Kingdom", "Global North",
    "UK", "Global North",
    "Britain", "Global North",
    "England", "Global North",
    "Scotland", "Global North",
    "Wales", "Global North",
    "Northern Ireland", "Global North",
    "Germany", "Global North",
    "France", "Global North",
    "Italy", "Global North",
    "Spain", "Global North",
    "Netherlands", "Global North",
    "Holland", "Global North",
    "Belgium", "Global North",
    "Switzerland", "Global North",
    "Austria", "Global North",
    "Sweden", "Global North",
    "Norway", "Global North",
    "Denmark", "Global North",
    "Finland", "Global North",
    "Ireland", "Global North",
    "Portugal", "Global North",
    "Greece", "Global North",
    "Luxembourg", "Global North",
    "Iceland", "Global North",
    "Australia", "Global North",
    "New Zealand", "Global North",
    "Japan", "Global North",
    "South Korea", "Global North",
    "Singapore", "Global North",
    "Taiwan", "Global North",
    "Hong Kong", "Global North",
    "Israel", "Global North",
    "Czech Republic", "Global North",
    "Czechia", "Global North",
    "Slovakia", "Global North",
    "Slovenia", "Global North",
    "Estonia", "Global North",
    "Latvia", "Global North",
    "Lithuania", "Global North",
    "Poland", "Global North",
    "Hungary", "Global North",
    "Croatia", "Global North",
    "Cyprus", "Global North",
    "Malta", "Global North",
    
    # Global South countries
    "China", "Global South",
    "People's Republic of China", "Global South",
    "PRC", "Global South",
    "India", "Global South",
    "Republic of India", "Global South",
    "Brazil", "Global South",
    "Brasil", "Global South",
    "Federative Republic of Brazil", "Global South",
    "Mexico", "Global South",
    "México", "Global South",
    "United Mexican States", "Global South",
    "Argentina", "Global South",
    "Chile", "Global South",
    "Colombia", "Global South",
    "Peru", "Global South",
    "Venezuela", "Global South",
    "Ecuador", "Global South",
    "Bolivia", "Global South",
    "Uruguay", "Global South",
    "Paraguay", "Global South",
    "Guyana", "Global South",
    "Suriname", "Global South",
    "French Guiana", "Global South",
    "Nigeria", "Global South",
    "South Africa", "Global South",
    "Republic of South Africa", "Global South",
    "RSA", "Global South",
    "Iran", "Global South",
    "Islamic Republic of Iran", "Global South",
    "Persia", "Global South",
    "Russia", "Global South",
    "Russian Federation", "Global South",
    "Soviet Union", "Global South",
    "USSR", "Global South",
    "Kenya", "Global South",
    "Ethiopia", "Global South",
    "Ghana", "Global South",
    "Morocco", "Global South",
    "Egypt", "Global South",
    "Tunisia", "Global South",
    "Algeria", "Global South",
    "Libya", "Global South",
    "Sudan", "Global South",
    "Tanzania", "Global South",
    "Uganda", "Global South",
    "Rwanda", "Global South",
    "Cameroon", "Global South",
    "Ivory Coast", "Global South",
    "Senegal", "Global South",
    "Mali", "Global South",
    "Burkina Faso", "Global South",
    "Niger", "Global South",  # Note: Also a species name - needs special handling
    "Chad", "Global South",
    "Central African Republic", "Global South",
    "Democratic Republic of the Congo", "Global South",
    "DRC", "Global South",
    "Republic of the Congo", "Global South",
    "Congo", "Global South",
    "Gabon", "Global South",
    "Equatorial Guinea", "Global South",
    "Madagascar", "Global South",
    "Mauritius", "Global South",
    "Seychelles", "Global South",
    "Mozambique", "Global South",
    "Zambia", "Global South",
    "Zimbabwe", "Global South",
    "Botswana", "Global South",
    "Namibia", "Global South",
    "Lesotho", "Global South",
    "Swaziland", "Global South",
    "Eswatini", "Global South",
    "Malawi", "Global South",
    "Turkey", "Global South",
    "Russia", "Global South",
    "Russian Federation", "Global South",
    "Ukraine", "Global South",
    "Belarus", "Global South",
    "Moldova", "Global South",
    "Romania", "Global South",
    "Bulgaria", "Global South",
    "Serbia", "Global South",
    "Montenegro", "Global South",
    "Bosnia and Herzegovina", "Global South",
    "North Macedonia", "Global South",
    "Macedonia", "Global South",
    "Albania", "Global South",
    "Kosovo", "Global South",
    "Georgia", "Global South",
    "Armenia", "Global South",
    "Azerbaijan", "Global South",
    "Kazakhstan", "Global South",
    "Uzbekistan", "Global South",
    "Turkmenistan", "Global South",
    "Kyrgyzstan", "Global South",
    "Tajikistan", "Global South",
    "Afghanistan", "Global South",
    "Pakistan", "Global South",
    "Bangladesh", "Global South",
    "Sri Lanka", "Global South",
    "Maldives", "Global South",
    "Nepal", "Global South",
    "Bhutan", "Global South",
    "Myanmar", "Global South",
    "Burma", "Global South",
    "Thailand", "Global South",
    "Vietnam", "Global South",
    "Laos", "Global South",
    "Cambodia", "Global South",
    "Malaysia", "Global South",
    "Indonesia", "Global South",
    "Philippines", "Global South",
    "Brunei", "Global South",
    "East Timor", "Global South",
    "Timor-Leste", "Global South",
    "Papua New Guinea", "Global South",
    "PNG", "Global South",
    "Fiji", "Global South",
    "Vanuatu", "Global South",
    "Solomon Islands", "Global South",
    "Samoa", "Global South",
    "Tonga", "Global South",
    "Iran", "Global South",
    "Iraq", "Global South",
    "Saudi Arabia", "Global South",
    "Kuwait", "Global South",
    "Bahrain", "Global South",
    "Qatar", "Global South",
    "United Arab Emirates", "Global South",
    "UAE", "Global South",
    "Oman", "Global South",
    "Yemen", "Global South",
    "Jordan", "Global South",
    "Lebanon", "Global South",
    "Syria", "Global South",
    "Mongolia", "Global South",
    "North Korea", "Global South"
  )
  
  return(country_classifications)
}

# Function to get Global North countries as vector
get_global_north_countries <- function() {
  get_country_classifications() %>%
    dplyr::filter(development_focus == "Global North") %>%
    dplyr::pull(country)
}

# Function to get Global South countries as vector  
get_global_south_countries <- function() {
  get_country_classifications() %>%
    dplyr::filter(development_focus == "Global South") %>%
    dplyr::pull(country)
}

# Function to get all countries as vector
get_all_countries <- function() {
  get_country_classifications() %>%
    dplyr::pull(country)
}

# Function to get plant parts keywords
get_plant_parts_keywords <- function() {
  # Comprehensive plant parts keywords (deduplicated) - especially relevant for endophyte studies
  plant_parts_keywords <- unique(c(
    # Basic plant parts
    "fruit", "fruits", "root", "roots", "leaf", "leaves", "stem", "stems", 
    "flower", "flowers", "seed", "seeds", "bark", "branch", "branches",
    "twig", "twigs", "shoot", "shoots", "bud", "buds", "wood", "timber",
    "trunk", "trunks", "crown", "canopy", "foliage",
    
    # Specific tissues and structures (important for endophyte localization)
    "phloem", "xylem", "cortex", "cortices", "epidermis", "endodermis", "hypodermis",
    "pericarp", "mesocarp", "endocarp", "exocarp", "hull", "husk", "pod", "pods",
    "capsule", "capsules", "berry", "berries", "drupe", "drupes",
    "achene", "achenes", "caryopsis", "samara", "samaras", "silique", "siliques",
    
    # Tissue types (endophyte habitats)
    "parenchyma", "sclerenchyma", "collenchyma", "aerenchyma", "chlorenchyma",
    "vascular bundle", "vascular bundles", "bundle sheath", "bundle sheaths",
    "pith", "medulla", "cambium", "cambia", "periderm", "cuticle", "cuticles",
    "endosperm", "embryo", "embryos", "cotyledon", "cotyledons",
    "hypocotyl", "epicotyl", "radicle", "plumule", "coleoptile", "coleorhiza",
    
    # Specialized structures (common endophyte hosts)
    "meristem", "meristems", "pericycle", "stele", "steles",
    "apical meristem", "lateral meristem", "intercalary meristem", "cambial zone",
    "root apical meristem", "shoot apical meristem", "RAM", "SAM",
    
    # Leaf parts (major endophyte habitat)
    "blade", "blades", "petiole", "petioles", "lamina", "laminae",
    "leaflet", "leaflets", "node", "nodes", "internode", "internodes",
    "stipule", "stipules", "sheath", "sheaths", "leaf sheath", "leaf sheaths",
    "midrib", "midribs", "vein", "veins", "venation", "vascular traces",
    "leaf margin", "leaf margins", "leaf tip", "leaf tips", "leaf base",
    
    # Root parts (important endophyte niches)
    "taproot", "taproots", "fibrous root", "fibrous roots", "root hair", "root hairs",
    "root cap", "root caps", "root tip", "root tips", "lateral root", "lateral roots",
    "adventitious root", "adventitious roots", "prop root", "prop roots",
    "aerial root", "aerial roots", "pneumatophore", "pneumatophores",
    "root nodule", "root nodules", "mycorrhiza", "mycorrhizae", "mycorrhizal",
    
    # Stem parts (endophyte colonization sites)
    "internode", "internodes", "node", "nodes", "axil", "axils",
    "terminal bud", "terminal buds", "axillary bud", "axillary buds",
    "lenticel", "lenticels", "stolon", "stolons", "rhizome", "rhizomes",
    "corm", "corms", "tuber", "tubers", "bulb", "bulbs", "pseudobulb", "pseudobulbs",
    "runner", "runners", "offset", "offsets",
    
    # Flower parts (less common but documented endophyte sites)
    "petal", "petals", "sepal", "sepals", "stamen", "stamens",
    "pistil", "pistils", "anther", "anthers", "filament", "filaments",
    "ovary", "ovaries", "ovule", "ovules", "style", "styles",
    "stigma", "stigmas", "receptacle", "receptacles", "calyx", "calyces",
    "corolla", "corollas", "perianth", "tepals", "tepal", "nectary", "nectaries",
    "inflorescence", "inflorescences", "spike", "spikes", "raceme", "racemes",
    "panicle", "panicles", "umbel", "umbels", "cyme", "cymes",
    
    # Fruit parts (endophyte transmission sites)
    "pericarp", "pericarps", "placenta", "placentas", "funiculus", "funiculi",
    "raphe", "hilum", "micropyle", "chalaza", "aril", "arils",
    
    # Secretory and storage structures (endophyte-rich environments)
    "resin duct", "resin ducts", "resin canal", "resin canals",
    "oil duct", "oil ducts", "latex vessel", "latex vessels", "laticifer", "laticifers",
    "mucilage canal", "mucilage canals", "secretory cell", "secretory cells",
    "glandular hair", "glandular hairs", "glandular trichome", "glandular trichomes",
    "salt gland", "salt glands", "nectar spur", "nectar spurs",
    
    # Surface structures and specialized cells
    "trichome", "trichomes", "hair", "hairs", "scale", "scales",
    "papilla", "papillae", "emergences", "prickle", "prickles",
    "spine", "spines", "thorn", "thorns", "stipular spine", "stipular spines",
    "tendril", "tendrils", "bract", "bracts", "bracteole", "bracteoles",
    
    # Anatomical features important for endophyte studies
    "guard cell", "guard cells", "subsidiary cell", "subsidiary cells",
    "stoma", "stomata", "stomatal", "substomatal chamber", "substomatal chambers",
    "intercellular space", "intercellular spaces", "air space", "air spaces",
    "cell wall", "cell walls", "middle lamella", "plasmodesmata",
    
    # Wood anatomy (for woody plant endophytes)
    "heartwood", "sapwood", "earlywood", "latewood", "annual ring", "annual rings",
    "growth ring", "growth rings", "ray", "rays", "vessel", "vessels",
    "tracheid", "tracheids", "fiber", "fibers", "tylosis", "tyloses",
    
    # Reproductive structures (spore/seed transmission)
    "cone", "cones", "strobilus", "strobili", "microsporangium", "microsporangia",
    "megasporangium", "megasporangia", "sporangium", "sporangia",
    "pollen sac", "pollen sacs", "pollen grain", "pollen grains",
    
    # Galls and abnormal structures (endophyte-induced)
    "gall", "galls", "tumor", "tumors", "neoplasm", "neoplasms",
    "witch broom", "witches broom", "fasciation", "fasciations",
    "canker", "cankers", "lesion", "lesions", "hyperplasia", "hypertrophy",
    "callus", "calli", "proliferation", "proliferations"
  ))
  
  return(plant_parts_keywords)
}

# Function to get geographic region keywords
get_geographic_keywords <- function() {
  regions <- c(
    # Continents (both formal and colloquial)
    "Africa", "Asia", "Europe", "North America", "South America", "Oceania", "Antarctica", "Australia",
    "African", "Asian", "European", "American", "North American", "South American", "Oceanic", "Antarctic", "Australian",
    
    # Major regions
    "Middle East", "Central Asia", "Southeast Asia", "East Asia", "South Asia", "West Asia",
    "Eastern Europe", "Western Europe", "Northern Europe", "Southern Europe", "Central Europe",
    "Central America", "Caribbean", "Mesoamerica", "Amazonia", "Amazon Basin", "Amazon",
    "Patagonia", "Andes", "Andean", "Mediterranean", "Scandinavia", "Scandinavian",
    "Siberia", "Siberian", "Far East", "Near East", "Levant", "Balkans", "Caucasus",
    "Sub-Saharan Africa", "Maghreb", "Horn of Africa", "East Africa", "West Africa", "Southern Africa",
    "Indian Subcontinent", "Indo-Pacific", "Polynesia", "Melanesia", "Micronesia",
    
    # Ecosystems and biomes (comprehensive)
    "tropical", "temperate", "boreal", "arctic", "subarctic", "subtropical", "subantarctic",
    "rainforest", "rain forest", "cloud forest", "dry forest", "deciduous forest", "coniferous forest", 
    "mixed forest", "old growth", "primary forest", "secondary forest", "gallery forest",
    "woodland", "savanna", "grassland", "prairie", "steppe", "pampas", "veld",
    "desert", "semi-desert", "arid", "semi-arid", "xeric", "mesic", "hydric",
    "tundra", "taiga", "chaparral", "maquis", "scrubland", "shrubland",
    "wetland", "marsh", "swamp", "bog", "fen", "peatland", "mire", "mangrove",
    "coastal", "littoral", "marine", "estuarine", "intertidal", "riparian",
    "freshwater", "aquatic", "lacustrine", "fluvial", "stream", "river", "lake",
    "alpine", "subalpine", "montane", "submontane", "lowland", "upland", "highland",
    "karst", "limestone", "volcanic", "geothermal", "serpentine",
    
    # Climate zones
    "equatorial", "tropical monsoon", "humid continental", "oceanic", "mediterranean climate",
    "humid subtropical", "semi-arid", "desert climate", "polar", "ice cap"
  )
  
  return(regions)
}

# Function to get research method keywords
get_method_keywords <- function() {
  list(
    molecular = c(
      # Basic molecular biology
      "DNA", "RNA", "PCR", "qPCR", "real-time PCR", "RT-PCR", "nested PCR", "multiplex PCR",
      "ITS", "18S", "28S", "16S", "rDNA", "ribosomal", "sequencing", "sequence", "sequences",
      "phylogenetic", "phylogeny", "phylogenetics", "BLAST", "GenBank", "NCBI", "ENA",
      "primer", "primers", "amplification", "amplified", "gel electrophoresis", "agarose gel",
      "restriction", "RFLP", "fingerprint", "fingerprinting", "genetic fingerprinting",
      
      # Next-generation and advanced sequencing
      "next-generation sequencing", "NGS", "high-throughput sequencing",
      "Illumina", "454", "PacBio", "Oxford Nanopore", "Ion Torrent",
      "whole genome sequencing", "WGS", "RNA-seq", "transcriptome", "transcriptomic",
      "metagenomics", "metagenomic", "metabarcoding", "metabarcodes", "eDNA", "environmental DNA",
      "amplicon sequencing", "paired-end sequencing", "single-cell sequencing",
      
      # Molecular markers and barcoding
      "RAPD", "AFLP", "SSR", "microsatellite", "SNP", "single nucleotide polymorphism",
      "barcode", "barcoding", "DNA barcoding", "COI", "rbcL", "matK", "trnH-psbA",
      "internal transcribed spacer", "large subunit", "LSU", "small subunit", "SSU",
      
      # Molecular techniques and analysis
      "cloning", "cloned", "clone library", "plasmid", "vector", "transformation",
      "in situ hybridization", "FISH", "Southern blot", "Northern blot", "Western blot",
      "proteomics", "proteomic", "mass spectrometry", "LC-MS", "GC-MS",
      "bioinformatics", "bioinformatic", "computational biology", "genome assembly"
    ),
    
    culture = c(
      # Basic culturing
      "culture", "cultured", "culturing", "cultivation", "cultivated", "cultivating",
      "isolation", "isolated", "isolate", "isolates", "isolating",
      "medium", "media", "agar", "broth", "plate", "plates", "plating", "plated",
      "petri dish", "petri dishes", "culture dish", "culture dishes",
      
      # Culture conditions and techniques
      "colony", "colonies", "streak", "streaking", "serial dilution", "dilution plating",
      "selective medium", "selective media", "differential medium", "enrichment medium",
      "minimal medium", "defined medium", "complex medium", "synthetic medium",
      "antibiotic", "antibiotics", "antimicrobial", "fungicide", "bactericide",
      "incubation", "incubated", "incubating", "sterile", "sterilized", "sterilization",
      "autoclave", "autoclaved", "aseptic", "aseptic technique", "laminar flow",
      
      # Culture types and outcomes
      "pure culture", "mixed culture", "co-culture", "axenic", "gnotobiotic",
      "contamination", "contaminated", "sterility", "viable", "viability",
      "growth rate", "colony forming unit", "CFU", "morphology", "morphological",
      "sporulation", "spore formation", "conidiation", "conidiophore"
    ),
    
    microscopy = c(
      # Light microscopy
      "microscopy", "microscope", "microscopic", "microscopical", "observation",
      "light microscopy", "LM", "bright field", "dark field", "phase contrast",
      "differential interference contrast", "DIC", "Nomarski", "polarized light",
      
      # Electron microscopy
      "electron microscopy", "EM", "SEM", "TEM", "scanning electron", "transmission electron",
      "scanning electron microscopy", "transmission electron microscopy",
      "cryo-electron microscopy", "cryo-EM", "freeze fracture", "critical point drying",
      
      # Advanced microscopy
      "confocal", "confocal microscopy", "CLSM", "fluorescence microscopy", "epifluorescence",
      "two-photon microscopy", "multiphoton", "super-resolution", "STED", "PALM", "STORM",
      "atomic force microscopy", "AFM", "scanning probe microscopy",
      
      # Sample preparation and staining
      "staining", "stained", "histology", "histological", "histochemistry", "cytochemistry",
      "sectioning", "microtome", "ultramicrotome", "embedding", "embedded",
      "fixation", "fixed", "fixative", "glutaraldehyde", "formaldehyde", "osmium",
      "paraffin", "resin", "epoxy", "spurr", "LR white", "acrylic",
      "dehydration", "clearing", "mounting", "coverslip",
      
      # Specific stains and dyes
      "toluidine blue", "methylene blue", "safranin", "crystal violet", "malachite green",
      "congo red", "aniline blue", "calcofluor", "calcofluor white", "fluorescein",
      "DAPI", "propidium iodide", "GFP", "YFP", "RFP", "immunofluorescence",
      "antibody", "antibodies", "immunohistochemistry", "immunocytochemistry"
    )
  )
}

# Function to standardize country names (handles common variations and homonyms)
standardize_country_name <- function(country_text) {
  # Handle common variations and homonyms
  country_text <- stringr::str_to_title(country_text)
  
  # Specific mappings for common variations
  country_mappings <- c(
    "Usa" = "United States",
    "Us" = "United States", 
    "United States Of America" = "United States",
    "Uk" = "United Kingdom",
    "Britain" = "United Kingdom",
    "Great Britain" = "United Kingdom",
    "Russian Federation" = "Russia",
    "Drc" = "Democratic Republic of the Congo",
    "Burma" = "Myanmar",
    "Czechia" = "Czech Republic",
    "Holland" = "Netherlands",
    "Uae" = "United Arab Emirates"
  )
  
  # Apply mappings
  if (country_text %in% names(country_mappings)) {
    return(country_mappings[country_text])
  }
  
  return(country_text)
}

# Function to handle problematic homonyms (e.g., Niger as country vs species)
filter_country_homonyms <- function(text, country) {
  # Special handling for countries that are also species names
  if (country == "Niger") {
    # Only count as country if it appears with geographic context
    geographic_context <- stringr::str_detect(text, 
      stringr::regex("\\bniger\\b.{0,50}\\b(africa|country|nation|west|sahel|niamey)", ignore_case = TRUE))
    return(geographic_context)
  }
  
  # Add other problematic cases as needed
  return(TRUE)  # Default: count as country
}

# Function to get biodiversity hotspot countries
get_biodiversity_hotspots <- function() {
  hotspots <- c(
    "Madagascar", "Brazil", "Indonesia", "Malaysia", "Philippines", "Colombia", 
    "Ecuador", "Peru", "Costa Rica", "Mexico", "South Africa", "Australia", 
    "New Zealand", "Chile", "India", "China", "Myanmar", "Thailand", 
    "Vietnam", "Cameroon", "Tanzania", "Kenya", "Papua New Guinea"
  )
  
  return(hotspots)
}

# Example usage and testing function
test_reference_data <- function() {
  cat("=== Testing Reference Data Utils ===\n")
  
  # Test country classifications
  countries <- get_country_classifications()
  cat("Total countries defined:", nrow(countries), "\n")
  
  north_count <- sum(countries$development_focus == "Global North")
  south_count <- sum(countries$development_focus == "Global South")
  cat("Global North countries:", north_count, "\n")
  cat("Global South countries:", south_count, "\n")
  
  # Test plant parts
  plant_parts <- get_plant_parts_keywords()
  cat("Plant parts keywords:", length(plant_parts), "\n")
  
  # Test methods
  methods <- get_method_keywords()
  cat("Molecular method keywords:", length(methods$molecular), "\n")
  cat("Culture method keywords:", length(methods$culture), "\n")
  cat("Microscopy method keywords:", length(methods$microscopy), "\n")
  
  cat("=== Reference Data Utils Test Complete ===\n")
}

# Load required libraries (only if not already loaded)
if (!require("dplyr", quietly = TRUE)) {
  stop("dplyr package required but not available")
}
if (!require("stringr", quietly = TRUE)) {
  stop("stringr package required but not available")  
}
if (!require("tibble", quietly = TRUE)) {
  stop("tibble package required but not available")
}

cat("Reference Data Utils loaded successfully.\n")
cat("Available functions:\n")
cat("- get_country_classifications()\n")
cat("- get_global_north_countries()\n")
cat("- get_global_south_countries()\n") 
cat("- get_all_countries()\n")
cat("- get_plant_parts_keywords()\n")
cat("- get_geographic_keywords()\n")
cat("- get_continent_keywords() # subset of geographic keywords\n")
cat("- get_region_keywords() # subset of geographic keywords\n")
cat("- get_method_keywords()\n")
cat("- standardize_country_name()\n")
cat("- filter_country_homonyms()\n")
cat("- get_biodiversity_hotspots()\n")
cat("- test_reference_data() # for testing\n")

# Convenience function to get just continent keywords (subset of geographic keywords)
get_continent_keywords <- function() {
  c("africa", "asia", "europe", "north america", "south america", 
    "australia", "oceania", "antarctica")
}

# Convenience function to get region/ecosystem keywords (subset of geographic keywords)  
get_region_keywords <- function() {
  geographic_keywords <- get_geographic_keywords()
  # Return ecosystems and biomes (exclude continent names)
  continents <- c("Africa", "Asia", "Europe", "North America", "South America", 
                  "Oceania", "Antarctica", "Australia", "African", "Asian", 
                  "European", "American", "North American", "South American", 
                  "Oceanic", "Antarctic", "Australian")
  
  regions <- setdiff(geographic_keywords, continents)
  return(regions)
}
