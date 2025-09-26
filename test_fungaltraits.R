# Test script to understand fungaltraits package structure
library(fungaltraits)
library(dplyr)
library(tibble)

cat("=== Testing fungaltraits Package ===\n\n")

# Check the main dataset
cat("1. Exploring fungal_traits dataset...\n")
cat("   Dimensions: ", dim(fungal_traits), "\n")
cat("   Column names: ", paste(names(fungal_traits), collapse = ", "), "\n")

# Look for mycorrhizal-related columns
mycorrhizal_cols <- grep("mycorrh", names(fungal_traits), ignore.case = TRUE)
cat("\n2. Mycorrhizal-related columns found:\n")
if (length(mycorrhizal_cols) > 0) {
  for (col in names(fungal_traits)[mycorrhizal_cols]) {
    cat("   - ", col, ": ", paste(unique(fungal_traits[[col]]), collapse = ", "), "\n")
  }
} else {
  cat("   No mycorrhizal columns found with 'mycorrh' pattern\n")
}

# Check for other trait columns that might be relevant
trait_cols <- grep("primary_lifestyle|secondary_lifestyle|growth_form|fruiting_body",
                   names(fungal_traits), ignore.case = TRUE)
cat("\n3. Lifestyle-related columns:\n")
if (length(trait_cols) > 0) {
  for (col in names(fungal_traits)[trait_cols]) {
    cat("   - ", col, "\n")
    unique_vals <- unique(fungal_traits[[col]])
    cat("     Values: ", paste(unique_vals, collapse = ", "), "\n")
  }
}

# Check for taxonomy columns
taxonomy_cols <- c("species", "genus", "family", "order", "class", "phylum")
cat("\n4. Taxonomy columns:\n")
for (col in taxonomy_cols) {
  if (col %in% names(fungal_traits)) {
    cat("   - ", col, " (", sum(!is.na(fungal_traits[[col]])), " non-NA values)\n")
  }
}

# Look at some example mycorrhizal fungi
cat("\n5. Sample mycorrhizal taxa from dataset:\n")
mycorrhizal_species <- fungal_traits %>%
  filter(!is.na(primary_lifestyle)) %>%
  filter(grepl("mycorrh", primary_lifestyle, ignore.case = TRUE)) %>%
  select(species, genus, family, primary_lifestyle, secondary_lifestyle) %>%
  head(10)

if (nrow(mycorrhizal_species) > 0) {
  print(mycorrhizal_species)
} else {
  cat("   No mycorrhizal species found with current filter\n")

  # Try broader search
  broad_mycorrhizal <- fungal_traits %>%
    filter(!is.na(primary_lifestyle)) %>%
    filter(grepl("mycorrh|symbio", primary_lifestyle, ignore.case = TRUE)) %>%
    select(species, genus, family, primary_lifestyle, secondary_lifestyle) %>%
    head(5)

  print(broad_mycorrhizal)
}

# Test lookup function
cat("\n6. Testing lookup functionality...\n")
test_taxa <- c("Glomus", "Amanita", "Rhizopogon", "Pisolithus", "Unknown_species")

for (taxon in test_taxa) {
  cat("   Testing lookup for: ", taxon, "\n")

  # Try to find matches in the dataset
  matches <- fungal_traits %>%
    filter(genus == taxon | species == taxon | family == taxon)

  if (nrow(matches) > 0) {
    cat("     Found ", nrow(matches), " matches\n")
    cat("     Primary lifestyles: ", paste(unique(matches$primary_lifestyle), collapse = ", "), "\n")
  } else {
    cat("     No matches found\n")
  }
}

cat("\n7. Summary for mycorrhizal classification:\n")
cat("   - fungaltraits appears to use 'primary_lifestyle' and 'secondary_lifestyle' columns\n")
cat("   - Need to check for 'mycorrhizal', 'symbiotic', or similar terms\n")
cat("   - Can use taxonomic matching with genus/family/species columns\n")

cat("\n=== fungaltraits Testing Complete ===\n")