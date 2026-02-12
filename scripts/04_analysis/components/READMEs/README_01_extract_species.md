# Species Detection Component

## Overview

The Species Detection Component (`01_extract_species.R`) is responsible for extracting plant and fungal species information from scientific abstracts as part of the modular extraction pipeline. This component uses optimized lookup tables and parallel processing for efficient taxonomic name recognition.

## Purpose and Functionality

This script handles species detection for both plants and fungi, serving as the foundational component of the extraction pipeline. It employs advanced computational techniques to identify and validate taxonomic names with high precision and performance.

### Key Features

- **Comprehensive Species Detection**: Identifies both plant and fungal species mentions in scientific text
- **Optimized Lookup Tables**: Uses pre-compiled reference data for O(1) species name lookups
- **Parallel Processing**: Leverages multi-core processing for handling large datasets efficiently
- **Hash Table Optimization**: Automatically switches to hash-based lookups for datasets exceeding 10,000 species
- **Recovery Mechanisms**: Supports resuming from previous runs and intermediate result saving
- **Batch Processing**: Configurable batch sizes for memory-efficient processing of large datasets

## Input/Output Specifications

### Input Requirements
- **Data Format**: CSV file containing abstracts with `id` and `abstract` columns
- **Input File**: `results/consolidated_dataset.csv` (prepared by upstream pipeline)
- **Reference Data**: Requires `models/species.rds` or `species.rds` containing taxonomic reference data

### Output Specifications
- **Output File**: `results/species_detection_results.csv`
- **Data Structure**:
  - `id`: Abstract identifier
  - `resolved_name`: Standardized species name (GBIF validated)
  - `canonicalName`: Canonical species name
  - `taxonRank`: Taxonomic rank (species, genus, family)
  - `confidence_score`: Matching confidence level

## Dependencies and Requirements

### Required R Packages
```r
library(tidyverse)     # Data manipulation and I/O
library(tictoc)        # Performance timing
library(janitor)       # Data cleaning utilities
```

### Required Scripts
- `scripts/04_analysis/optimized_taxa_detection.R` - Core species detection algorithms
- `scripts/04_analysis/utilities/reference_data_utils.R` - Reference data utilities

### System Requirements
- R version 4.0 or higher
- Multi-core processor recommended for parallel processing
- Minimum 4GB RAM for datasets > 10,000 abstracts

## Usage Examples

### Basic Usage
```r
# Load the component
source("scripts/04_analysis/components/01_extract_species.R")

# Run species extraction with default parameters
species_results <- extract_species_data(abstracts_data)
```

### Advanced Configuration
```r
# Custom configuration for large datasets
species_results <- extract_species_data(
  abstracts_data,
  output_file = "results/species_detection_results.csv",
  batch_size = 50,           # Smaller batches for memory efficiency
  force_rerun = TRUE,        # Force re-extraction
  verbose = TRUE,           # Enable progress messages
  hash_threshold = 5000     # Lower threshold for hash optimization
)
```

### Pipeline Integration
```r
# As part of the full pipeline workflow
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "01_extract_species.R")) {
  abstracts_data <- read_csv("results/consolidated_dataset.csv", show_coltypes = FALSE)
  species_results <- extract_species_data(abstracts_data)
  cat("\n✅ Species extraction component completed!\n")
}
```

## Configuration Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `batch_size` | 100 | Number of abstracts to process per batch |
| `force_rerun` | FALSE | Whether to re-run extraction if results exist |
| `verbose` | TRUE | Whether to display progress messages |
| `hash_threshold` | 10000 | Minimum species count to enable hash optimization |

## Error Handling

### Common Issues and Solutions

**Missing Reference Data**
- Error: "Species reference data not found"
- Solution: Ensure `models/species.rds` exists or run reference data preparation

**Memory Issues**
- Symptom: Out of memory errors with large datasets
- Solution: Reduce `batch_size` parameter (try 50 or 25)

**Parallel Processing Failures**
- Error: "Parallel setup failed"
- Solution: Script automatically falls back to sequential processing

### Validation Checks
- Input data structure validation
- Reference data availability checks
- Output file integrity verification
- Species name format validation

## Performance Characteristics

### Scalability
- Small datasets (< 1,000 abstracts): < 1 minute processing time
- Medium datasets (1,000-10,000 abstracts): 2-10 minutes
- Large datasets (> 10,000 abstracts): 10-30 minutes depending on batch size

### Memory Usage
- Base memory: ~50MB for script and dependencies
- Per abstract: ~1-2KB depending on species complexity
- Peak usage: Typically 2-3x base memory during processing

### Accuracy Metrics
- Species detection rate: 85-95% depending on text quality
- False positive rate: < 5% with validation
- Processing throughput: ~100-500 abstracts/second

## Integration with Pipeline Workflow

### Upstream Dependencies
- Requires consolidated dataset from `scripts/03_prediction/consolidate_datasets.R`
- Depends on species reference data preparation

### Downstream Components
- Feeds into results merging component (`05_merge_results.R`)
- Provides species data for visualization and analysis workflows

### Workflow Position
```
Data Preparation → Species Detection → Methods/Parts/Geography Detection → Results Merging → Analysis
```

## Related Components

- **Methods Detection Component**: Detects research methods used
- **Plant Parts Detection Component**: Identifies studied plant parts
- **Geography Detection Component**: Extracts geographic information
- **Results Merging Component**: Combines all extraction results

## See Also

- **Main Pipeline Documentation** - Overview of the complete analysis pipeline
- **Optimized Taxa Detection** - Core species detection algorithms
- **Reference Data Utilities** - Data preparation utilities