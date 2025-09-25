# Load species data
species <- readRDS("models/species.rds")

# Filter for plants
plants <- species[species$kingdom %in% c("Plantae", "Viridiplantae") |
                  species$phylum %in% c("Tracheophyta", "Embryophyta"), ]

# Filter for fungi
fungi <- species[species$kingdom == "Fungi", ]

# Save subsets with xz compression (high compression ratio)
saveRDS(plants, "models/species_plants.rds", compress = "xz")
saveRDS(fungi, "models/species_fungi.rds", compress = "xz")

# Summary statistics
cat("Plants subset:\n")
cat("Row count:", nrow(plants), "\n")
cat("Memory size:", format(object.size(plants), units = "MB"), "\n\n")

cat("Fungi subset:\n")
cat("Row count:", nrow(fungi), "\n")
cat("Memory size:", format(object.size(fungi), units = "MB"), "\n\n")

cat("Files created:\n")
cat("models/species_plants.rds\n")
cat("models/species_fungi.rds\n")