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
    "Guatemala", "Global South",
    "Belize", "Global South",
    "Honduras", "Global South",
    "El Salvador", "Global South",
    "Nicaragua", "Global South",
    "Costa Rica", "Global South",
    "Panama", "Global South",
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
    "Guinea", "Global South",
    "Guinea-Bissau", "Global South",
    "Morocco", "Global South",
    "Egypt", "Global South",
    "Tunisia", "Global South",
    "Algeria", "Global South",
    "Libya", "Global South",
    "Sudan", "Global South",
    "Tanzania", "Global South",
    "Uganda", "Global South",
    "Rwanda", "Global South",
    "Burundi", "Global South",
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
    "North Korea", "Global South",
    "Antarctica", "Global South"  # Include for plotting coverage
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

    # Major geographic regions
    "Middle East", "Central Asia", "Southeast Asia", "East Asia", "South Asia", "West Asia", "Southwest Asia",
    "Eastern Europe", "Western Europe", "Northern Europe", "Southern Europe", "Central Europe", "Southeastern Europe",
    "Central America", "Caribbean", "Mesoamerica", "Mesoamerican", "Amazonia", "Amazon Basin", "Amazon", "Amazonian",
    "Patagonia", "Patagonian", "Andes", "Andean", "Mediterranean", "Scandinavia", "Scandinavian",
    "Siberia", "Siberian", "Far East", "Near East", "Levant", "Balkans", "Balkan", "Baltic", "Caucasus", "Caucasian",
    "Sub-Saharan Africa", "Sub-Saharan", "Saharan", "Sahelian", "Maghreb", "Horn of Africa", "East Africa", "East African",
    "West Africa", "West African", "Southern Africa", "Southern African", "Central Africa",
    "Indian Subcontinent", "Indo-Pacific", "Indomalayan", "Polynesia", "Polynesian", "Melanesia", "Melanesian", "Micronesia",
    "Sahel", "Sahara", "Kalahari", "Namib", "Gobi", "Taklamakan", "Thar", "Arabian Desert",
    "Great Lakes", "Rift Valley", "African Great Lakes", "Lake Victoria", "Lake Tanganyika", "Lake Malawi",
    "Himalayas", "Himalayan", "Tibetan Plateau", "Pamir Mountains", "Altai Mountains", "Ural Mountains",
    "Rocky Mountains", "Appalachian Mountains", "Sierra Madre", "Cordillera", "Andes Mountains",

    # Subregions and specific areas
    "Bengal", "Deccan", "Malabar Coast", "Coromandel Coast", "Western Ghats", "Eastern Ghats",
    "Indochina", "Indochinese", "Indochinese Peninsula", "Malay Peninsula", "Malayan", "Korean Peninsula", "Arabian Peninsula",
    "Iberian Peninsula", "Iberian", "Balkan Peninsula", "Scandinavian Peninsula", "Italian Peninsula",
    "Anatolia", "Anatolian", "Mesopotamia", "Fertile Crescent", "Levant", "Transcaucasia",
    "Pacific Northwest", "Great Plains", "Midwest", "Southwest", "Northeast", "Southeast",
    "Pampas", "Gran Chaco", "Pantanal", "Cerrado", "Mata Atlântica", "Atlantic Forest",
    "Taiga", "Steppe", "Prairie", "Puszta", "Veld", "Bushveld", "Karoo", "Fynbos",
    "Outback", "Great Barrier Reef", "Coral Triangle", "Ring of Fire",
    "Neotropical", "Neotropics", "Paleotropical", "Paleotropics", "Afrotropical", "Holarctic",
    "Borneo", "Bornean", "Sumatra", "Sumatran", "Indonesian", "Californian", "Appalachian",
    "Tibetan", "Himalayan", "Guinean", "Congolese", "Congo Basin", "Southeast Asian",
    "Alaskan", "Canadian", "Australasian", "West Indian", "Arctic", "Antarctic", "Subarctic",
    "Panamanian",

    # Biodiversity hotspots and conservation areas
    "Biodiversity hotspot", "endemic area", "center of diversity", "refugia", "refugium",
    "Cape Floristic Region", "Madagascar", "Madagascan", "Western Ghats", "Sri Lanka", "Himalayas",
    "Indo-Burma", "Sundaland", "Wallacea", "Philippines", "Japan", "Southwest Australia",
    "Chilean Winter Rainfall", "Brazilian Cerrado", "Brazilian Atlantic Forest",
    "Caribbean Islands", "Mesoamerica", "Tumbes-Choco-Magdalena", "Tropical Andes",
    "Mediterranean Basin", "Caucasus", "Mountains of Central Asia", "Eastern Afromontane",
    "Coastal Forests of Eastern Africa", "Horn of Africa", "Maputaland-Pondoland-Albany",
    "Succulent Karoo", "Guinean Forests of West Africa", "Eastern Arc Mountains",

    # Ecosystems and biomes (comprehensive)
    "tropical", "temperate", "boreal", "arctic", "subarctic", "subtropical", "subantarctic", "palaearctic",
    "rainforest", "rain forest", "cloud forest", "dry forest", "deciduous forest", "coniferous forest",
    "mixed forest", "old growth", "primary forest", "secondary forest", "gallery forest", "riparian forest",
    "woodland", "savanna", "savannah", "grassland", "prairie", "steppe", "pampas", "veld", "meadow", "pasture",
    "desert", "semi-desert", "arid", "semi-arid", "xeric", "mesic", "hydric", "hyper-arid",
    "tundra", "taiga", "chaparral", "maquis", "garigue", "scrubland", "shrubland", "heathland", "moorland",
    "wetland", "marsh", "swamp", "bog", "fen", "peatland", "mire", "mangrove", "mangrove forest",
    "coastal", "littoral", "marine", "estuarine", "intertidal", "supratidal", "sublittoral",
    "freshwater", "aquatic", "lacustrine", "fluvial", "stream", "river", "lake", "pond", "reservoir",
    "alpine", "subalpine", "montane", "submontane", "lowland", "upland", "highland", "foothill",
    "karst", "limestone", "volcanic", "geothermal", "serpentine", "ultramafic", "calcareous", "acidic",

    # Vegetation formations and habitats
    "evergreen forest", "deciduous forest", "broadleaf forest", "needleleaf forest", "mixed broadleaf-needleleaf",
    "thorn forest", "thorn scrub", "spiny forest", "succulent forest", "palm forest", "bamboo forest",
    "monsoon forest", "dipterocarp forest", "teak forest", "pine forest", "oak forest", "beech forest",
    "eucalyptus forest", "acacia woodland", "miombo woodland", "caatinga", "campo rupestre",
    "paramo", "puna", "alpine meadow", "arctic tundra", "alpine tundra", "forest tundra",
    "salt marsh", "freshwater marsh", "reed bed", "papyrus swamp", "cypress swamp", "bottomland forest",
    "floodplain forest", "alluvial forest", "varzea forest", "igapo forest", "swamp forest", "peat swamp",
    "cloud forest", "elfin forest", "dwarf forest", "krummholz", "tree line", "timberline",
    "dune system", "sand dune", "beach", "strand", "cliff", "escarpment", "gorge", "canyon", "valley",
    "plateau", "plain", "basin", "delta", "estuary", "lagoon", "bay", "gulf", "strait", "channel",

    # Climate zones and environmental conditions
    "equatorial", "tropical wet", "tropical moist", "tropical dry", "tropical monsoon", "tropical savanna",
    "humid subtropical", "subtropical wet", "subtropical moist", "subtropical dry", "mediterranean climate",
    "humid continental", "subarctic climate", "oceanic climate", "semi-arid climate", "arid climate",
    "desert climate", "polar climate", "ice cap climate", "highland climate", "monsoon climate",
    "trade wind climate", "maritime climate", "continental climate", "seasonal climate",
    "temperate oceanic", "temperate continental", "cold desert", "hot desert", "coastal desert",

    # Ecological and environmental classifications
    "ecotone", "ecological transition", "habitat mosaic", "landscape mosaic", "heterogeneous landscape",
    "disturbed habitat", "undisturbed habitat", "pristine habitat", "degraded habitat", "fragmented habitat",
    "island habitat", "mainland habitat", "continental island", "oceanic island", "land bridge island",
    "endemic habitat", "introduced habitat", "anthropogenic habitat", "urban habitat", "agricultural landscape",
    "agroecosystem", "agricultural ecosystem", "pastoral system", "cropland", "orchard", "plantation",
    "secondary forest", "regrowth forest", "degraded forest", "logged forest", "selectively logged",
    "protected area", "national park", "nature reserve", "wildlife sanctuary", "biosphere reserve",
    "world heritage site", "ramsar site", "important bird area", "key biodiversity area",
    "corridor", "wildlife corridor", "riparian corridor", "stepping stone", "buffer zone"
  )

  return(regions)
}

# Function to get research method keywords - comprehensive for fungal endophyte studies
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
      "bioinformatics", "bioinformatic", "computational biology", "genome assembly",

      # Endophyte-specific molecular methods
      "endophyte identification", "fungal identification", "molecular identification",
      "taxonomic identification", "species identification", "strain typing"
    ),

    culture_based = c(
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
      "sporulation", "spore formation", "conidiation", "conidiophore",

      # Endophyte-specific isolation and culturing
      "endophyte isolation", "fungal isolation", "tissue isolation", "plant tissue culture",
      "surface sterilization", "disinfection", "bleach", "sodium hypochlorite", "ethanol",
      "mercury chloride", "plant material sterilization", "tissue fragmentation"
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
      "antibody", "antibodies", "immunohistochemistry", "immunocytochemistry",

      # Endophyte-specific microscopy
      "colonization", "colonization pattern", "hyphal penetration", "tissue colonization",
      "intercellular", "intracellular", "vascular colonization", "root colonization"
    ),

    inoculation = c(
      # Plant inoculation methods
      "inoculation", "inoculated", "inoculating", "inoculum", "inocula",
      "seed inoculation", "seed treatment", "root inoculation", "soil inoculation",
      "foliar spray", "foliar application", "stem injection", "hypocotyl injection",
      "colonization", "colonized", "colonizing", "re-isolation", "re-isolated",

      # Inoculation techniques
      "spore suspension", "mycelial suspension", "conidial suspension", "culture filtrate",
      "bacterial suspension", "fungal suspension", "endophyte suspension",
      "plant growth promoting", "PGP", "plant growth promotion",

      # Colonization assessment
      "colonization rate", "colonization percentage", "infection rate", "recovery rate",
      "endophytic colonization", "systemic colonization", "localized colonization",
      "tissue specificity", "host specificity", "plant-microbe interaction"
    ),

    plant_microbe_interaction = c(
      # Co-cultivation and dual culture systems
      "dual culture", "co-cultivation", "co-culture", "interaction", "interactions",
      "antagonism", "antagonistic", "synergism", "synergistic", "competition",
      "plant-fungal interaction", "plant-microbe interaction", "host-pathogen interaction",

      # Interaction assays
      "pathogenicity test", "virulence assay", "disease assay", "symptom development",
      "lesion development", "biotrophy", "necrotrophy", "hemibiotrophy",
      "compatible interaction", "incompatible interaction", "hypersensitive response",

      # Endophyte-host interaction studies
      "endophyte-plant interaction", "mutualism", "mutualistic", "symbiosis", "symbiotic",
      "commensalism", "commensal", "parasitism", "parasitic", "endosymbiosis",
      "beneficial interaction", "stress tolerance", "abiotic stress", "biotic stress"
    ),

    bioactivity_assays = c(
      # Antimicrobial activity
      "antimicrobial", "antibacterial", "antifungal", "antiviral", "antibiosis",
      "zone of inhibition", "disk diffusion", "well diffusion", "broth dilution",
      "MIC", "minimum inhibitory concentration", "antagonistic activity",

      # Plant growth promotion
      "plant growth promotion", "PGP", "growth promotion", "biomass increase",
      "seed germination", "root development", "shoot development", "plant vigor",
      "nitrogen fixation", "phosphate solubilization", "siderophore production",

      # Biocontrol activity
      "biocontrol", "biological control", "disease suppression", "pathogen suppression",
      "induced resistance", "systemic resistance", "ISR", "SAR", "systemic acquired resistance",
      "plant protection", "crop protection", "disease resistance", "pest resistance"
    ),

    physiological_assays = c(
      # Enzyme assays
      "enzyme", "enzymes", "enzyme activity", "cellulase", "cellulolytic", "pectinase",
      "pectinolytic", "amylase", "amylolytic", "protease", "proteolytic", "lipase",
      "lipolytic", "chitinase", "chitinolytic", "glucanase", "glucanolytc",
      "xylanase", "xylanolytc", "laccase", "peroxidase", "catalase", "superoxide dismutase",

      # Metabolite analysis
      "metabolite", "metabolites", "secondary metabolite", "bioactive compound",
      "phytochemical", "alkaloid", "alkaloids", "flavonoid", "flavonoids", "terpenoid",
      "terpenoids", "phenolic", "phenolics", "antioxidant", "antioxidants",
      "metabolomics", "metabolomic", "GC-MS", "LC-MS", "NMR", "HPLC",

      # Physiological studies
      "stress tolerance", "salt tolerance", "drought tolerance", "heat tolerance",
      "cold tolerance", "heavy metal tolerance", "pH tolerance", "osmotic stress",
      "oxidative stress", "antioxidant activity", "ROS", "reactive oxygen species"
    ),

    ecological_studies = c(
      # Diversity analysis
      "diversity", "species diversity", "fungal diversity", "endophyte diversity",
      "Shannon index", "Simpson index", "species richness", "evenness", "abundance",
      "community structure", "community composition", "species composition",
      "relative abundance", "frequency", "occurrence", "prevalence", "incidence",

      # Ecological methods
      "field survey", "field collection", "sampling", "sample collection",
      "ecological survey", "biodiversity assessment", "species inventory",
      "succession", "successional", "seasonal variation", "temporal variation",
      "spatial distribution", "geographic distribution", "host preference",

      # Statistical analysis
      "ordination", "PCA", "principal component analysis", "cluster analysis",
      "multivariate analysis", "statistical analysis", "ANOVA", "correlation",
      "regression", "canonical correspondence analysis", "CCA", "RDA", "redundancy analysis"
    ),

    surface_sterilization = c(
      # Surface sterilization techniques
      "surface sterilization", "surface disinfection", "sterilization", "disinfection",
      "bleach treatment", "sodium hypochlorite", "NaOCl", "ethanol", "EtOH",
      "mercuric chloride", "HgCl2", "hydrogen peroxide", "H2O2", "flaming",
      "autoclaving", "dry heat", "moist heat", "UV sterilization", "gamma irradiation",

      # Endophyte isolation specific
      "endophyte isolation", "tissue isolation", "plant tissue isolation",
      "fragmentation", "tissue fragmentation", "homogenization", "maceration",
      "serial washing", "rinsing", "sterile water", "sterile distilled water",
      "Tween 20", "surfactant", "detergent", "wetting agent",

      # Verification of sterilization
      "sterility test", "sterility check", "contamination check", "imprint method",
      "roll method", "wash water test", "control plate", "negative control",
      "epiphyte removal", "surface contaminant", "surface microbe"
    )
  )
}

# Enhanced function to standardize country names with comprehensive synonym handling
standardize_country_name <- function(country_text) {
  # Handle NULL, empty, or missing inputs
  if (is.null(country_text) || length(country_text) == 0) return(NA_character_)
  country_text <- as.character(country_text)

  # Remove extra whitespace and convert to title case for consistent matching
  country_clean <- stringr::str_trim(country_text)
  country_title <- stringr::str_to_title(country_clean)

  # Comprehensive synonym mappings organized by category
  country_mappings <- c(
    # United States and variants
    "Usa" = "United States",
    "Us" = "United States",
    "U.S.A." = "United States",
    "U.S." = "United States",
    "United States Of America" = "United States",
    "The United States" = "United States",

    # United Kingdom and variants
    "Uk" = "United Kingdom",
    "Britain" = "United Kingdom",
    "Great Britain" = "United Kingdom",
    "England" = "United Kingdom",  # Note: England is part of UK
    "Scotland" = "United Kingdom",
    "Wales" = "United Kingdom",
    "Northern Ireland" = "United Kingdom",

    # Major Asian countries
    "Russian Federation" = "Russia",
    "Soviet Union" = "Russia",  # Historical
    "Ussr" = "Russia",  # Historical
    "People'S Republic Of China" = "China",
    "Prc" = "China",
    "Republic Of China" = "Taiwan",  # Note: Taiwan claims this name
    "Republic Of Korea" = "South Korea",
    "Korea" = "South Korea",  # Ambiguous - defaults to South Korea
    "Dprk" = "North Korea",  # Democratic People's Republic of Korea
    "Burma" = "Myanmar",
    "Viet Nam" = "Vietnam",
    "Sri Lanka" = "Sri Lanka",  # No change needed but included for completeness

    # Middle East and Gulf countries
    "Uae" = "United Arab Emirates",
    "Islamic Republic Of Iran" = "Iran",
    "Persia" = "Iran",  # Historical/traditional name
    "Saudi Arabia" = "Saudi Arabia",  # No change but included
    "Kuwait" = "Kuwait",

    # African countries
    "Drc" = "Democratic Republic of the Congo",
    "Democratic Republic Of Congo" = "Democratic Republic of the Congo",
    "Republic Of The Congo" = "Republic of the Congo",
    "Congo" = "Republic of the Congo",  # Ambiguous - defaults to Republic of the Congo
    "Za" = "South Africa",  # Internet country code
    "Rsa" = "South Africa",  # Republic of South Africa
    "Ivory Coast" = "Côte d'Ivoire",
    "Cote D'Ivoire" = "Côte d'Ivoire",
    "Cote D’Ivoire" = "Côte d'Ivoire",
    "Swaziland" = "Eswatini",  # Name change in 2018
    "The Gambia" = "Gambia",
    "Cape Verde" = "Cabo Verde",

    # European countries
    "Czechia" = "Czech Republic",
    "Holland" = "Netherlands",
    "The Netherlands" = "Netherlands",
    "Macedonia" = "North Macedonia",  # Name change in 2019
    "Fyrom" = "North Macedonia",  # Former Yugoslav Republic of Macedonia

    # Latin American countries
    "Brasil" = "Brazil",
    "México" = "Mexico",
    "Méjico" = "Mexico",  # Alternative spelling
    "Argentine Republic" = "Argentina",
    "Republic Of Argentina" = "Argentina",

    # Oceania and Pacific
    "East Timor" = "Timor-Leste",
    "Timor Leste" = "Timor-Leste",
    "Png" = "Papua New Guinea",

    # Additional historical and alternative names
    "Zaire" = "Democratic Republic of the Congo",  # Historical name
    "Rhodesia" = "Zimbabwe",  # Historical name
    "Bechuanaland" = "Botswana",  # Historical name
    "Basutoland" = "Lesotho",  # Historical name
    "Gold Coast" = "Ghana",  # Historical name
    "Tanganyika" = "Tanzania",  # Historical name
    "Formosa" = "Taiwan",  # Historical name
    "Siam" = "Thailand",  # Historical name
    "Ceylon" = "Sri Lanka",  # Historical name

    # Sub-national entities that might be confused with countries
    "Scotland" = "United Kingdom",
    "Wales" = "United Kingdom",
    "Northern Ireland" = "United Kingdom",
    "Catalonia" = "Spain",  # Autonomous region
    "Basque Country" = "Spain",
    "Quebec" = "Canada",  # Province
    "Texas" = "United States",  # State
    "California" = "United States",
    "Florida" = "United States",

    # Alternative spellings and transliterations
    "Moldova" = "Moldova",
    "Moldavia" = "Moldova",  # Alternative name
    "Belorussia" = "Belarus",  # Alternative name
    "Byelorussia" = "Belarus",
    "Kirghizia" = "Kyrgyzstan",  # Alternative spelling
    "Kirgizstan" = "Kyrgyzstan",
    "Tadjikistan" = "Tajikistan",  # Alternative spelling
    "Tadzhikistan" = "Tajikistan",
    "Turkmenia" = "Turkmenistan",  # Alternative name
    "Azerbaijan" = "Azerbaijan",  # Note: Also a region in Iran
    "Azerbaidjan" = "Azerbaijan",

    # Special cases and abbreviations
    "Uk" = "United Kingdom",
    "Us" = "United States",
    "Eu" = "European Union",  # Not a country but sometimes confused
    "Un" = "United Nations",  # Not a country
    "Who" = "World Health Organization"  # Not a country
  )

  # Apply mappings using regex patterns for better matching
  out <- vapply(country_title, function(ct) {
    # Check if this is already a valid country name - if so, don't apply mappings
    all_countries <- get_all_countries()
    if (ct %in% all_countries) {
      return(ct)
    }

    # First try exact matches
    if (ct %in% names(country_mappings)) {
      return(unname(country_mappings[ct]))
    }

    # Try regex patterns for cases with special characters or variations
    for (pattern in names(country_mappings)) {
      if (grepl(paste0("\\b", pattern, "\\b"), ct, ignore.case = TRUE)) {
        return(unname(country_mappings[pattern]))
      }
    }

    # If no mapping found, return the title-cased input
    return(ct)
  }, FUN.VALUE = "")

  return(as.character(out))
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
cat("- get_research_institution_mappings() # map institutions to countries\n")
cat("- get_adjectival_region_mappings() # map regional adjectives to countries/regions\n")
cat("- get_method_keywords()\n")
cat("- standardize_country_name()\n")
cat("- filter_country_homonyms()\n")
cat("- get_biodiversity_hotspots()\n")
cat("- test_reference_data() # for testing\n")
cat("- standardize_country_name() # normalize country synonyms\n")
cat("- normalize_country_vector() # vectorized wrapper that ensures canonical names\n")
cat("- normalize_plant_part() # singular/plural normalization for plant parts\n")

# Convenience function to get just continent keywords (subset of geographic keywords)
get_continent_keywords <- function() {
  c("africa", "asia", "europe", "north america", "south america", 
    "australia", "oceania", "antarctica", "antarctic")
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

# Function to map research institutions to their countries
# Maps institutional acronyms and full names to geographic locations
get_research_institution_mappings <- function() {
  list(
    # Major tropical research institutions
    "STRI" = "Panama",
    "Smithsonian Tropical Research Institute" = "Panama",
    "Smithsonian Tropical Research" = "Panama",
    
    # UK institutions
    "Kew" = "United Kingdom",
    "Royal Botanic Gardens Kew" = "United Kingdom",
    "Royal Botanic Gardens, Kew" = "United Kingdom",
    "RBG Kew" = "United Kingdom",
    "Royal Botanic Garden Edinburgh" = "United Kingdom",
    "RBGE" = "United Kingdom",
    
    # Australian institutions
    "CSIRO" = "Australia",
    "Commonwealth Scientific and Industrial Research Organisation" = "Australia",
    "Royal Botanic Gardens Victoria" = "Australia",
    "Royal Botanic Gardens Sydney" = "Australia",
    "Australian National Herbarium" = "Australia",
    
    # US institutions
    "USDA" = "United States",
    "United States Department of Agriculture" = "United States",
    "Smithsonian Institution" = "United States",
    "New York Botanical Garden" = "United States",
    "NYBG" = "United States",
    "Missouri Botanical Garden" = "United States",
    "MBG" = "United States",
    "Harvard University Herbaria" = "United States",
    
    # Asian institutions
    "Chinese Academy of Sciences" = "China",
    "CAS" = "China",
    "RIKEN" = "Japan",
    "National Institute for Basic Biology" = "Japan",
    "NIBB" = "Japan",
    
    # European institutions
    "Max Planck Institute" = "Germany",
    "MPI" = "Germany",
    "INRA" = "France",
    "Institut National de la Recherche Agronomique" = "France",
    "Wageningen University" = "Netherlands",
    "Wageningen UR" = "Netherlands",
    
    # Latin American institutions
    "INPA" = "Brazil",
    "Instituto Nacional de Pesquisas da Amazônia" = "Brazil",
    "UNAM" = "Mexico",
    "Universidad Nacional Autónoma de México" = "Mexico",
    
    # African institutions
    "ICRAF" = "Kenya",
    "World Agroforestry Centre" = "Kenya",
    "World Agroforestry Center" = "Kenya",
    
    # International institutions with specific locations
    "CIAT" = "Colombia",
    "Centro Internacional de Agricultura Tropical" = "Colombia",
    "CIMMYT" = "Mexico",
    "International Maize and Wheat Improvement Center" = "Mexico",
    "IRRI" = "Philippines",
    "International Rice Research Institute" = "Philippines"
  )
}

# Function to map adjectival and regional terms to countries or regions
# Captures geographic information from regional adjectives and demonyms
get_adjectival_region_mappings <- function() {
  list(
    # South American regional forms
    "Amazonian" = c("region" = "Amazon Basin", "countries" = "Brazil; Peru; Colombia; Ecuador; Bolivia; Venezuela"),
    "Andean" = c("region" = "Andes Mountains", "countries" = "Peru; Ecuador; Colombia; Bolivia; Chile; Argentina"),
    "Patagonian" = c("region" = "Patagonia", "countries" = "Argentina; Chile"),
    
    # Central American regional forms
    "Mesoamerican" = c("region" = "Mesoamerica", "countries" = "Mexico; Guatemala; Belize; Honduras; El Salvador; Nicaragua; Costa Rica; Panama"),
    "Panamanian" = c("region" = "Central America", "countries" = "Panama"),
    "Costa Rican" = c("region" = "Central America", "countries" = "Costa Rica"),
    
    # African regional forms
    "Sub-Saharan" = c("region" = "Sub-Saharan Africa", "countries" = "Multiple African countries"),
    "Saharan" = c("region" = "Sahara", "countries" = "Multiple North African countries"),
    "Sahelian" = c("region" = "Sahel", "countries" = "Multiple West African countries"),
    "Guinean" = c("region" = "Guinea region", "countries" = "Guinea; Guinea-Bissau"),
    "Congolese" = c("region" = "Congo Basin", "countries" = "Democratic Republic of the Congo; Republic of the Congo"),
    "East African" = c("region" = "East Africa", "countries" = "Kenya; Tanzania; Uganda; Rwanda; Burundi; Ethiopia"),
    "West African" = c("region" = "West Africa", "countries" = "Nigeria; Ghana; Senegal; Mali; Burkina Faso"),
    "Southern African" = c("region" = "Southern Africa", "countries" = "South Africa; Zimbabwe; Botswana; Namibia; Zambia"),
    
    # Asian regional forms
    "Southeast Asian" = c("region" = "Southeast Asia", "countries" = "Thailand; Vietnam; Malaysia; Indonesia; Philippines; Myanmar; Cambodia; Laos"),
    "Indochinese" = c("region" = "Indochina", "countries" = "Vietnam; Cambodia; Laos; Thailand; Myanmar"),
    "Malayan" = c("region" = "Malay Peninsula", "countries" = "Malaysia; Singapore; Thailand"),
    "Indonesian" = c("region" = "Indonesian Archipelago", "countries" = "Indonesia"),
    "Bornean" = c("region" = "Borneo", "countries" = "Indonesia; Malaysia; Brunei"),
    "Sumatran" = c("region" = "Sumatra", "countries" = "Indonesia"),
    "Himalayan" = c("region" = "Himalayas", "countries" = "Nepal; India; Bhutan; China; Pakistan"),
    "Tibetan" = c("region" = "Tibetan Plateau", "countries" = "China; India; Nepal; Bhutan"),
    
    # European regional forms
    "Mediterranean" = c("region" = "Mediterranean Basin", "countries" = "Spain; Italy; Greece; Turkey; France"),
    "Scandinavian" = c("region" = "Scandinavia", "countries" = "Sweden; Norway; Denmark; Finland; Iceland"),
    "Baltic" = c("region" = "Baltic region", "countries" = "Estonia; Latvia; Lithuania; Poland; Germany"),
    "Iberian" = c("region" = "Iberian Peninsula", "countries" = "Spain; Portugal"),
    "Balkan" = c("region" = "Balkans", "countries" = "Greece; Bulgaria; Albania; Serbia; Bosnia and Herzegovina; Croatia"),
    
    # North American regional forms
    "Appalachian" = c("region" = "Appalachian Mountains", "countries" = "United States; Canada"),
    "Californian" = c("region" = "California", "countries" = "United States"),
    "Canadian" = c("region" = "Canada", "countries" = "Canada"),
    "Alaskan" = c("region" = "Alaska", "countries" = "United States"),
    
    # Oceanian regional forms
    "Australasian" = c("region" = "Australasia", "countries" = "Australia; New Zealand"),
    "Polynesian" = c("region" = "Polynesia", "countries" = "Multiple Pacific islands"),
    "Melanesian" = c("region" = "Melanesia", "countries" = "Papua New Guinea; Fiji; Solomon Islands; Vanuatu"),
    
    # Middle Eastern forms
    "Iranian" = c("region" = "Iran", "countries" = "Iran"),
    
    # Caribbean regional forms
    "Caribbean" = c("region" = "Caribbean", "countries" = "Multiple Caribbean countries"),
    "West Indian" = c("region" = "Caribbean", "countries" = "Multiple Caribbean countries"),
    
    # Polar regional forms
    "Arctic" = c("region" = "Arctic", "countries" = "Multiple northern countries"),
    "Antarctic" = c("region" = "Antarctica", "countries" = "Antarctica"),
    "Subarctic" = c("region" = "Subarctic", "countries" = "Multiple northern countries"),
    
    # Additional specific regional terms
    "Neotropical" = c("region" = "Neotropics"),
    "Paleotropical" = c("region" = "Paleotropics"),
    "Indomalayan" = c("region" = "Indomalayan region", "countries" = "South and Southeast Asian countries"),
    "Afrotropical" = c("region" = "Afrotropical region", "countries" = "African countries"),
    "Holarctic" = c("region" = "Holarctic", "countries" = "Northern hemisphere temperate countries")
  )
}

# Vectorized wrapper that standardizes country names and optionally ensures membership
# in the canonical country list from get_country_classifications().
normalize_country_vector <- function(country_vec, ensure_in_classification = FALSE) {
  country_vec <- as.character(country_vec)
  standardized <- standardize_country_name(country_vec)

  if (ensure_in_classification) {
    canonical <- get_all_countries()
    # If an element is not found in canonical list, keep standardized value but warn once
    not_found <- setdiff(unique(standardized[!is.na(standardized)]), canonical)
    if (length(not_found) > 0) {
      warning("Some standardized country names not in classification: ", paste(not_found, collapse = ", "))
    }
  }

  return(standardized)
}


# Enhanced normalize_plant_part function with comprehensive singular/plural mapping
# This function groups singular and plural forms into canonical terms for consistent analysis
normalize_plant_part <- function(part_text) {
  # Handle vector input by processing each element
  if (length(part_text) > 1) {
    return(vapply(part_text, normalize_plant_part, FUN.VALUE = character(1), USE.NAMES = FALSE))
  }

  # Handle single value
  if (length(part_text) == 0 || is.na(part_text) || part_text == "") return(NA_character_)

  txt <- tolower(trimws(as.character(part_text)))

  # Comprehensive irregular plural -> singular mappings (expanded)
  irregulars <- c(
    # Basic plant parts
    "leaves" = "leaf", "roots" = "root", "stems" = "stem", "flowers" = "flower",
    "seeds" = "seed", "fruits" = "fruit", "buds" = "bud", "branches" = "branch",
    "twigs" = "twig", "shoots" = "shoot", "trunks" = "trunk", "crowns" = "crown",
    "canopies" = "canopy",

    # Tissues and structures
    "cortices" = "cortex", "epidermises" = "epidermis", "endodermises" = "endodermis",
    "hypodermises" = "hypodermis", "pericarps" = "pericarp", "mesocarps" = "mesocarp",
    "endocarps" = "endocarp", "exocarps" = "exocarp", "hulls" = "hull", "husks" = "husk",
    "pods" = "pod", "capsules" = "capsule", "berries" = "berry", "drupes" = "drupe",
    "achenes" = "achene", "caryopses" = "caryopsis", "samaras" = "samara", "siliques" = "silique",

    # Parenchyma and related tissues
    "parenchymas" = "parenchyma", "sclerenchymas" = "sclerenchyma", "collenchymas" = "collenchyma",
    "aerenchymas" = "aerenchyma", "chlorenchymas" = "chlorenchyma", "sheaths" = "sheath",
    "piths" = "pith", "medullas" = "medulla", "cambia" = "cambium", "periderms" = "periderm",
    "cuticles" = "cuticle", "endosperms" = "endosperm", "embryos" = "embryo",
    "cotyledons" = "cotyledon", "hypocotyls" = "hypocotyl", "epicotyls" = "epicotyl",
    "radicles" = "radicle", "plumules" = "plumule", "coleoptiles" = "coleoptile",
    "coleorhizas" = "coleorhiza",

    # Meristematic tissues
    "meristems" = "meristem", "pericycles" = "pericycle", "steles" = "stele",
    "cambial zones" = "cambial zone", "root apical meristems" = "root apical meristem",
    "shoot apical meristems" = "shoot apical meristem", "rams" = "ram", "sams" = "sam",

    # Leaf parts
    "blades" = "blade", "petioles" = "petiole", "laminas" = "lamina", "laminae" = "lamina",
    "leaflets" = "leaflet", "nodes" = "node", "internodes" = "internode", "stipules" = "stipule",
    "sheaths" = "sheath", "midribs" = "midrib", "veins" = "vein", "leaf margins" = "leaf margin",
    "leaf tips" = "leaf tip", "leaf bases" = "leaf base",

    # Root parts
    "taproots" = "taproot", "fibrous roots" = "fibrous root", "root hairs" = "root hair",
    "root caps" = "root cap", "root tips" = "root tip", "lateral roots" = "lateral root",
    "adventitious roots" = "adventitious root", "prop roots" = "prop root",
    "aerial roots" = "aerial root", "pneumatophores" = "pneumatophore",
    "root nodules" = "root nodule", "mycorrhizas" = "mycorrhiza", "mycorrhizae" = "mycorrhiza",
    "mycorrhizals" = "mycorrhizal",

    # Stem parts
    "axils" = "axil", "terminal buds" = "terminal bud", "axillary buds" = "axillary bud",
    "lenticels" = "lenticel", "stolons" = "stolon", "rhizomes" = "rhizome",
    "corms" = "corm", "tubers" = "tuber", "bulbs" = "bulb", "pseudobulbs" = "pseudobulb",
    "runners" = "runner", "offsets" = "offset",

    # Flower parts
    "petals" = "petal", "sepals" = "sepal", "stamens" = "stamen", "pistils" = "pistil",
    "anthers" = "anther", "filaments" = "filament", "ovaries" = "ovary", "ovules" = "ovule",
    "styles" = "style", "stigmas" = "stigma", "receptacles" = "receptacle", "calyces" = "calyx",
    "corollas" = "corolla", "perianths" = "perianth", "tepals" = "tepal", "nectaries" = "nectary",
    "inflorescences" = "inflorescence", "spikes" = "spike", "racemes" = "raceme",
    "panicles" = "panicle", "umbels" = "umbel", "cymes" = "cyme",

    # Fruit parts
    "pericarps" = "pericarp", "placentas" = "placenta", "funiculi" = "funiculus",
    "arils" = "aril",

    # Secretory structures
    "resin ducts" = "resin duct", "resin canals" = "resin canal", "oil ducts" = "oil duct",
    "latex vessels" = "latex vessel", "laticifers" = "laticifer", "mucilage canals" = "mucilage canal",
    "secretory cells" = "secretory cell", "glandular hairs" = "glandular hair",
    "glandular trichomes" = "glandular trichome", "salt glands" = "salt gland",
    "nectar spurs" = "nectar spur",

    # Surface structures
    "trichomes" = "trichome", "hairs" = "hair", "scales" = "scale", "papillae" = "papilla",
    "emergences" = "emergence", "prickles" = "prickle", "spines" = "spine", "thorns" = "thorn",
    "stipular spines" = "stipular spine", "tendrils" = "tendril", "bracts" = "bract",
    "bracteoles" = "bracteole",

    # Anatomical features
    "guard cells" = "guard cell", "subsidiary cells" = "subsidiary cell", "stomata" = "stoma",
    "stomatals" = "stomatal", "substomatal chambers" = "substomatal chamber",
    "intercellular spaces" = "intercellular space", "air spaces" = "air space",
    "cell walls" = "cell wall", "middle lamellae" = "middle lamella", "plasmodesmata" = "plasmodesma",

    # Wood anatomy
    "heartwoods" = "heartwood", "sapwoods" = "sapwood", "earlywoods" = "earlywood",
    "latewoods" = "latewood", "annual rings" = "annual ring", "growth rings" = "growth ring",
    "rays" = "ray", "vessels" = "vessel", "tracheids" = "tracheid", "fibers" = "fiber",
    "tyloses" = "tylosis",

    # Reproductive structures
    "cones" = "cone", "strobili" = "strobilus", "microsporangia" = "microsporangium",
    "megasporangia" = "megasporangium", "sporangia" = "sporangium", "pollen sacs" = "pollen sac",
    "pollen grains" = "pollen grain",

    # Galls and abnormal structures
    "galls" = "gall", "tumors" = "tumor", "neoplasms" = "neoplasm", "witch brooms" = "witch broom",
    "witches brooms" = "witches broom", "fasciations" = "fasciation", "cankers" = "canker",
    "lesions" = "lesion", "hyperplasias" = "hyperplasia", "hypertrophies" = "hypertrophy",
    "calli" = "callus", "proliferations" = "proliferation",

    # Axes and similar terms
    "axes" = "axis"
  )

  if (txt %in% names(irregulars)) return(irregulars[txt])

  # Enhanced rules for regular plurals and compound terms
  if (grepl("ies$", txt)) {
    candidate <- sub("ies$", "y", txt)
  } else if (grepl("ves$", txt)) {
    candidate <- sub("ves$", "f", txt)
  } else if (grepl("s$", txt)) {
    candidate <- sub("s$", "", txt)
  } else {
    candidate <- txt
  }

  # Handle compound terms with common prefixes
  compound_prefixes <- c("vascular", "leaf", "root", "stem", "flower", "seed", "fruit",
                        "plant", "shoot", "branch", "bud", "node", "internode")

  for (prefix in compound_prefixes) {
    if (grepl(paste0("^", prefix, "_"), candidate) || grepl(paste0("^", prefix, " "), candidate)) {
      # Extract the core term
      core_term <- sub(paste0("^", prefix, "_"), "", candidate)
      core_term <- sub(paste0("^", prefix, " "), "", core_term)
      if (core_term %in% names(irregulars)) {
        return(paste(prefix, irregulars[core_term]))
      }
      break
    }
  }

  # Validate candidate against known plant parts; if present, return it, else return original
  known <- tolower(get_plant_parts_keywords())
  if (candidate %in% known) return(candidate)

  # Special handling for some edge cases
  if (txt %in% c("mycorrhizal", "mycorrhizals", "mycorrhizas", "mycorrhizae")) return("mycorrhiza")
  if (txt %in% c("vascular bundles", "vascular bundle")) return("vascular bundle")
  if (txt %in% c("bundle sheaths", "bundle sheath")) return("bundle sheath")

  return(txt)
}
