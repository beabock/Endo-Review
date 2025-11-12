# Analysis Utilities Directory

This directory contains specialized utility functions supporting the Endo-Review analysis pipeline.

## Utilities

### Core Utilities
- **`reference_data_utils.R`** - Data normalization and reference handling
- **`create_species_subsets.R`** - Test data generation utilities
- **`build_bloom_filters.R`** - Probabilistic filtering algorithms

## Purpose

These utilities provide specialized functionality for:
- Data preprocessing and normalization
- Performance optimization (bloom filters)
- Test data generation
- Reference data management

## Usage

Import utilities as needed:
```r
source("scripts/04_analysis/utilities/reference_data_utils.R")
source("scripts/04_analysis/utilities/build_bloom_filters.R")
```

## Dependencies

- Base R packages
- Pipeline configuration (`scripts/config/pipeline_config.R`)
- Core utility functions (`scripts/utils/`)

## Performance Features

- Bloom filters for O(1) lookups
- Memory-efficient data structures
- Optimized text processing functions

## Output

Utilities typically save optimized data structures to `models/` or `test_data/` directories.