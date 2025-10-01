# Script to examine the funtothefun.csv dataset - looking for guild information
library(readr)
library(dplyr)
library(tidyr)

cat("=== Examining funtothefun.csv for Guilds ===\n\n")

# Read the file
file_path <- "C:/Users/beabo/OneDrive - Northern Arizona University/NAU/Sap_Sym/datasets/funtothefun.csv"

if (file.exists(file_path)) {
  cat("1. Loading dataset...\n")
  fun_data <- read_csv(file_path, show_col_types = FALSE)

  cat("   Dimensions: ", dim(fun_data), "\n")
  cat("   Column names: ", paste(names(fun_data), collapse = ", "), "\n")

  # Look for trait_name values that might contain guild or mycorrhizal info
  cat("\n2. Unique trait_name values:\n")
  unique_traits <- unique(fun_data$trait_name)
  print(unique_traits)

  # Look specifically for guild-related traits
  cat("\n3. Guild-related traits:\n")
  guild_traits <- unique_traits[grep("guild", unique_traits, ignore.case = TRUE)]
  if (length(guild_traits) > 0) {
    cat("   Found guild traits: ", paste(guild_traits, collapse = ", "), "\n")

    for (trait in guild_traits) {
      trait_data <- fun_data %>% filter(trait_name == trait)
      cat("\n   Trait: ", trait, "\n")
      cat("     Unique values (first 10): ", paste(head(unique(trait_data$value), 10), collapse = ", "), "\n")
      cat("     Number of records: ", nrow(trait_data), "\n")

      # Look for mycorrhizal values
      mycorrhizal_values <- unique(trait_data$value)[grep("mycorrh", unique(trait_data$value), ignore.case = TRUE)]
      if (length(mycorrhizal_values) > 0) {
        cat("     Mycorrhizal values found: ", paste(mycorrhizal_values, collapse = ", "), "\n")
      }
    }
  } else {
    cat("   No guild-related traits found\n")
  }

  # Look for other relevant traits
  cat("\n4. Other relevant traits:\n")
  relevant_traits <- unique_traits[grep("lifestyle|ecology|trophic|symbio", unique_traits, ignore.case = TRUE)]
  if (length(relevant_traits) > 0) {
    cat("   Found relevant traits: ", paste(relevant_traits, collapse = ", "), "\n")

    for (trait in relevant_traits) {
      trait_data <- fun_data %>% filter(trait_name == trait)
      cat("\n   Trait: ", trait, "\n")
      cat("     Unique values (first 10): ", paste(head(unique(trait_data$value), 10), collapse = ", "), "\n")
      cat("     Number of records: ", nrow(trait_data), "\n")
    }
  } else {
    cat("   No lifestyle/ecology traits found\n")
  }

  # Sample some data to understand the structure better
  cat("\n5. Sample records:\n")
  sample_data <- fun_data %>%
    head(20) %>%
    select(species, trait_name, value)
  print(sample_data)

  # Look for mycorrhizal species examples
  cat("\n6. Looking for mycorrhizal examples:\n")

  # Try to find species with mycorrhizal traits
  mycorrhizal_species <- fun_data %>%
    filter(grepl("mycorrh", value, ignore.case = TRUE)) %>%
    select(species, trait_name, value) %>%
    distinct() %>%
    head(10)

  if (nrow(mycorrhizal_species) > 0) {
    cat("   Found species with mycorrhizal traits:\n")
    print(mycorrhizal_species)
  } else {
    cat("   No species found with 'mycorrh' in trait values\n")

    # Try broader search
    symbiotic_species <- fun_data %>%
      filter(grepl("symbio", value, ignore.case = TRUE)) %>%
      select(species, trait_name, value) %>%
      distinct() %>%
      head(10)

    if (nrow(symbiotic_species) > 0) {
      cat("   Found species with symbiotic traits:\n")
      print(symbiotic_species)
    } else {
      cat("   No symbiotic traits found either\n")
    }
  }

  # Check if we can reshape this data for easier use
  cat("\n7. Data reshaping possibility:\n")
  cat("   - This is a long-format trait-value dataset\n")
  cat("   - Can be pivoted to wide format for easier species-trait matching\n")
  cat("   - Structure: species + trait_name + value -> species + trait columns\n")

} else {
  cat("❌ File not found: ", file_path, "\n")
}

cat("\n=== Dataset examination complete ===\n")