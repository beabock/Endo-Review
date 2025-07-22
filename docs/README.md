# Endophyte Review - Plant Names Extraction

This repository contains scripts for analyzing plant taxonomy information in scientific abstracts related to endophytic fungi. The main focus is on extracting plant names (species, genus, family) and analyzing their taxonomic representation and research methods used.

## Key Files

- `extracting_plant_names.R` - Original script for extracting plant names from abstracts
- `extracting_plant_names_optimized.R` - Optimized version with improved performance and additional features
- `optimization_summary.md` - Detailed explanation of optimizations and new features

## Features

The optimized script provides:

1. **Efficient Plant Name Extraction**
   - Extracts species, genus, and family names from scientific abstracts
   - Validates names against GBIF taxonomy database
   - 10-20x faster than the original implementation

2. **Research Methods Analysis**
   - Detects methods used in research (molecular, culture-based, microscopy, etc.)
   - Correlates methods with plant taxa
   - Analyzes trends over time when publication year data is available

3. **Taxonomic Coverage Analysis**
   - Analyzes what percentage of known plant families, genera, and species are represented
   - Provides visualizations at different taxonomic levels
   - Organizes results by phylum for better biological context

4. **Plant Parts Analysis**
   - Identifies which plant parts are most frequently mentioned
   - Normalizes singular/plural forms for accurate counting

## Requirements

- R (version 4.0 or higher recommended)
- Required packages:
  - tidyverse
  - quanteda
  - rgbif
  - furrr
  - janitor
  - vroom
  - irlba
  - stringdist

## Usage

1. **Quick Test Run**:
   ```r
   source("extracting_plant_names_optimized.R")
   # This will automatically run on a sample of 10 abstracts
   ```

2. **Full Dataset Run**:
   ```r
   # Uncomment the last line in the script:
   # results <- main()
   ```

3. **Output**:
   - All results are saved to the `Results/optimized/` directory
   - CSV files contain raw data for further analysis
   - PNG files contain visualizations ready for inclusion in reports

## Data Requirements

The script expects the following data files:
- `full_predictions_with_metadata.csv` - Abstracts with metadata and labels
- `Training_labeled_abs_5.csv` - Training data with labeled abstracts
- GBIF backbone taxonomy data (will be downloaded if not present)

## License

See the LICENSE file for details.
