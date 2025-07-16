# Taxa Detection Improvements

This document explains the key issues with the original taxa detection approach, the improvements made, and how to use the new functions to better detect species like "Acer macrophyllum" in your abstracts.

## Key Issues Identified

After analyzing your code, I identified several issues that might be preventing proper detection of species names like "Acer macrophyllum":

1. **Regex Pattern Limitations**: The original pattern `"\\b[A-Z][a-z]+\\s+[a-z]+\\b"` doesn't match species names followed by punctuation (e.g., "Acer macrophyllum,").

2. **Tokenization Problems**: The current approach splits text on whitespace after removing punctuation, breaking up species names like "Acer macrophyllum" into separate tokens "acer" and "macrophyllum".

3. **Fuzzy Matching Disabled**: The code explicitly disables fuzzy matching, which could help catch variations or misspellings of species names.

4. **Abbreviated Genus Handling**: The current approach for expanding abbreviated genus names (e.g., "A. macrophyllum") is limited to the top 10 most common genera for that letter.

5. **Matching Process**: In `process_taxonomic_matches`, it filters valid species with `filter(tolower(resolved_name) %in% tolower(tokens_vec))`, but since the tokens are split, multi-word species names won't match.

## Improvements Made

I've created three files to address these issues:

1. `test_taxa_detection.R`: A diagnostic script that helps identify why "Acer macrophyllum" isn't being detected.
2. `improved_taxa_detection.R`: An improved version of the taxa detection functions.
3. `test_dataset.R`: A test suite with known species to validate the improvements.

The key improvements include:

### 1. Enhanced Regex Patterns

```r
# IMPROVEMENT 1: Enhanced pattern for "Genus species" that handles punctuation
genus_species <- str_extract_all(text, "\\b[A-Z][a-z]+\\s+[a-z]+\\b[,\\.;:\\)\\(]?")[[1]]
genus_species <- str_replace_all(genus_species, "[,\\.;:\\)\\(]$", "")  # Remove trailing punctuation
```

This pattern now captures species names even when followed by punctuation.

### 2. N-gram Tokenization

```r
# IMPROVEMENT 7: Create tokens and n-grams for better matching
# Create basic tokens
tokens_vec <- unlist(strsplit(gsub("[[:punct:][:digit:]]", " ", tolower(text)), "\\s+"))
tokens_vec <- tokens_vec[tokens_vec != ""]

# Create bigrams to preserve multi-word taxa names
bigrams <- character(0)
if (length(tokens_vec) > 1) {
  for (i in 1:(length(tokens_vec) - 1)) {
    bigrams <- c(bigrams, paste(tokens_vec[i], tokens_vec[i + 1]))
  }
}

# Combine tokens and bigrams for matching
all_tokens <- c(tokens_vec, bigrams)
```

This preserves multi-word taxa names during the matching process.

### 3. Re-enabled Fuzzy Matching

```r
# IMPROVEMENT 6: Fuzzy matching for unresolved names
if (use_fuzzy && length(names) > 0) {
  unresolved <- names_df %>%
    filter(!user_supplied_name %in% resolved$user_supplied_name) %>%
    filter(nchar(user_supplied_name) >= 5)  # Skip short names
  
  if (nrow(unresolved) > 0 && nrow(unresolved) <= 50) {  # Limit fuzzy matching to reasonable number
    # Implementation details...
  }
}
```

Fuzzy matching is now enabled but with controlled parameters to avoid performance issues.

### 4. Improved Abbreviation Handling

```r
# IMPROVEMENT 5: Better abbreviation expansion
expanded_names <- character(0)
abbrev_pattern <- "^([A-Z])\\.\\s+([a-z]+)$"
abbrev_matches <- names[grepl(abbrev_pattern, names, ignore.case = TRUE)]
```

The abbreviation expansion is now more robust and handles more cases.

### 5. Taxonomic Indicators

```r
# IMPROVEMENT 4: Look for taxonomic indicators
taxonomic_indicators <- c("sp\\.", "spp\\.", "var\\.", "subsp\\.", "f\\.", "cf\\.")
indicator_pattern <- paste0("\\b[a-z]+\\s+", paste(taxonomic_indicators, collapse = "|"), "\\b")
indicator_matches <- str_extract_all(text_lower, indicator_pattern)[[1]]
```

The code now looks for taxonomic indicators that might suggest species names.

## How to Use the New Functions

### Option 1: Replace Specific Functions

You can replace specific functions in your existing code with the improved versions:

1. Replace `extract_candidate_names` with the improved version
2. Replace `batch_validate_names` with the improved version
3. Replace `process_taxonomic_matches` with the improved version

### Option 2: Use the Improved Extract Function

Replace your current `extract_plant_info` function with the improved version:

```r
# In your main script
source("improved_taxa_detection.R")

# Then use the improved extract_plant_info function
results <- extract_plant_info(
  text = abstract,
  abstract_id = id,
  predicted_label = label,
  lookup_tables = lookup_tables,
  plant_parts_keywords = plant_parts_keywords
)
```

### Option 3: Test with the Test Dataset

You can use the test dataset to validate the improvements:

```r
source("test_dataset.R")
test_results <- run_comparison_test()
```

Or test with a custom abstract:

```r
test_custom_abstract("This is a study about Acer macrophyllum.")
```

## Creating a Validation Dataset

To ensure your taxa detection is working correctly, I recommend creating a validation dataset with known species:

1. Manually annotate a small set of abstracts (10-20) with the species names they contain
2. Use the `test_dataset.R` script as a template to create your validation dataset
3. Run the validation to measure precision and recall

Example:

```r
validation_abstracts <- tibble(
  id = 1:3,
  abstract = c(
    "Abstract with Acer macrophyllum",
    "Abstract with multiple species: Acer macrophyllum and Pseudotsuga menziesii",
    "Abstract with no species"
  ),
  expected_species = c(
    "Acer macrophyllum",
    "Acer macrophyllum,Pseudotsuga menziesii",
    ""
  ),
  predicted_label = c("Presence", "Presence", "Absence")
)
```

## Performance Considerations

The improved functions include several optimizations to maintain good performance:

1. Fuzzy matching is limited to a reasonable number of unresolved names (≤50)
2. Fuzzy matching only applies to names with a space (likely species names)
3. The stringdist method "jw" (Jaro-Winkler) is efficient for species name matching
4. The sample size for fuzzy matching is limited to 5000 species names

If you're processing a large number of abstracts, you might want to adjust these parameters based on your specific needs.

## Next Steps

1. Test the improved functions with your actual data
2. Adjust parameters as needed for your specific use case
3. Consider creating a more comprehensive validation dataset
4. Integrate the improvements into your main workflow

Feel free to modify the improved functions to better suit your needs. The key improvements (regex patterns, n-gram tokenization, and fuzzy matching) should significantly enhance your taxa detection capabilities.
