# Mycorrhizal Taxa Identification Component

## Overview

The `01b_mycorrhizal_check.R` component identifies papers that mention only mycorrhizal fungi, enabling researchers to filter their analysis to include or exclude mycorrhizal-only studies.

## Purpose

This midstep between species extraction (`01_extract_species.R`) and methods extraction (`02_extract_methods.R`) serves several key functions:

1. **Mycorrhizal-Only Paper Identification**: Classifies abstracts based on whether they mention ONLY mycorrhizal fungal taxa
2. **Analysis Filtering**: Enables later analyses to include/exclude mycorrhizal-only papers
3. **Taxonomic Focus**: Supports research questions about mycorrhizal vs. non-mycorrhizal endophytes

## Input/Output

- **Input**: `results/species_detection_results.csv` (from `01_extract_species.R`)
- **Output**: `results/species_detection_results_mycorrhizal.csv` (same data + `is_mycorrhizal_only` column)

## Methodology

### funtothefun Dataset Classification
- Uses the local funtothefun dataset (51,555+ fungal trait records) for mycorrhizal identification
- **Only TRUE mycorrhizal guilds are considered mycorrhizal**:
  - Ectomycorrhizal fungi (form symbiotic relationships with tree roots)
  - Arbuscular mycorrhizal fungi (form symbiotic relationships with most plants)
  - Orchid mycorrhizal fungi (specialized for orchids)
  - Ericoid mycorrhizal fungi (specialized for Ericaceae plants)
- **Endophytic fungi are NOT considered mycorrhizal** - they live inside plants but don't form mycorrhizal symbiosis
- **Other fungal guilds** (pathogens, saprotrophs, etc.) are not mycorrhizal
- **Matching strategy**: Exact species match → Genus-level match → Taxonomic fallback

### Taxonomic Fallback
- When funtothefun dataset is unavailable or taxa not found, uses taxonomic heuristics for known mycorrhizal groups:
  - **Phylum level**: Glomeromycota, Mucoromycota
  - **Family level**: Glomeraceae, Acaulosporaceae, Gigasporaceae, etc.
  - **Genus level**: Glomus, Rhizophagus, Pisolithus, etc.

### Abstract-Level Classification
- Groups species detection results by abstract ID
- For each abstract, determines if ALL fungal taxa mentioned are mycorrhizal
- Result: `is_mycorrhizal_only` column (TRUE/FALSE)

## Usage

### Standalone
```r
source("scripts/04_analysis/components/01b_mycorrhizal_check.R")
results <- identify_mycorrhizal_papers()
```

### As Part of Pipeline
```r
# Run complete pipeline including mycorrhizal check
result <- run_endophyte_pipeline()

# Run analysis stages including mycorrhizal check
result <- run_analysis_only()

# Run just the mycorrhizal checking stage
result <- run_endophyte_pipeline(stages = "mycorrhizal_check")
```

## Output Column

- **`is_mycorrhizal_only`**: Boolean indicating if the abstract mentions ONLY mycorrhizal fungi
  - `TRUE`: All fungal taxa in the abstract are TRUE mycorrhizal (Ectomycorrhizal, Arbuscular Mycorrhizal, Orchid Mycorrhizal, or Ericoid Mycorrhizal)
  - `FALSE`: Either no fungi mentioned, some endophytic fungi present, or other non-mycorrhizal fungi present
  - **Note**: Endophytic fungi are treated as non-mycorrhizal for this analysis

## Dependencies

- `tidyverse` for data manipulation
- `funtothefun.csv` dataset (located at `C:/Users/beabo/OneDrive/Documents/NAU/Sap_Sym/datasets/funtothefun.csv`)
- Input from `01_extract_species.R`

## Examples

```r
# Load results
mycorrhizal_results <- read_csv("results/species_detection_results_mycorrhizal.csv")

# Filter for mycorrhizal-only papers
mycorrhizal_only <- mycorrhizal_results %>%
  filter(is_mycorrhizal_only == TRUE)

# Filter for papers with non-mycorrhizal fungi
mixed_fungal <- mycorrhizal_results %>%
  filter(is_mycorrhizal_only == FALSE & kingdom == "Fungi")

# Summary statistics
mycorrhizal_summary <- mycorrhizal_results %>%
  group_by(is_mycorrhizal_only) %>%
  summarise(count = n_distinct(id), .groups = "drop")
```

## Integration with Pipeline

This component is automatically included in:
- `run_endophyte_pipeline()` - complete pipeline
- `run_analysis_only()` - analysis stages only
- `run_downstream_analysis()` - downstream analysis

## Performance Notes

- funtothefun dataset classification is fast and comprehensive (51,555+ records)
- Local dataset eliminates network dependencies and external package requirements
- Taxonomic fallback is faster but less comprehensive
- Results are cached to avoid re-processing
- Dataset location: `C:/Users/beabo/OneDrive/Documents/NAU/Sap_Sym/datasets/funtothefun.csv`