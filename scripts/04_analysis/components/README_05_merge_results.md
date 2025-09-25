# Results Merging Component

## Overview

The Results Merging Component (`05_merge_results.R`) combines all extraction results from individual analysis components into a comprehensive dataset for downstream analysis and reporting. This component serves as the final step in the modular extraction pipeline, consolidating species, methods, plant parts, and geography data.

## Purpose and Functionality

This script merges the outputs from all individual extraction components into a single, comprehensive dataset. It handles data integration, column deduplication, and provides detailed statistics about the combined extraction results, enabling holistic analysis of the research landscape.

### Key Features

- **Comprehensive Data Integration**: Merges results from all extraction components
- **Intelligent Column Handling**: Automatically resolves duplicate columns from joins
- **Recovery Mechanisms**: Supports resuming from existing merged results
- **Detailed Statistics**: Provides comprehensive summary statistics
- **Data Validation**: Ensures data integrity across all merged components
- **Flexible Output**: Configurable output files and formats

## Input/Output Specifications

### Input Requirements
- **Component Results**: Individual CSV files from extraction components
  - `results/species_detection_results.csv`
  - `results/methods_detection_results.csv`
  - `results/plant_parts_detection_results.csv`
  - `results/geography_detection_results.csv`
- **Base Data**: `results/prepared_abstracts_for_extraction.csv`

### Output Specifications
- **Output File**: `results/comprehensive_extraction_results.csv`
- **Data Structure**: Merged dataset containing all columns from:
  - Base abstracts data
  - Species detection results
  - Methods detection results
  - Plant parts detection results
  - Geography detection results

## Dependencies and Requirements

### Required R Packages
```r
library(tidyverse)     # Data manipulation, merging, and I/O
```

### Required Scripts
- None (self-contained merging logic)

### System Requirements
- R version 4.0 or higher
- Minimum 1GB RAM for typical datasets
- Single-core processing sufficient

## Usage Examples

### Basic Usage
```r
# Load the component
source("scripts/04_analysis/components/05_merge_results.R")

# Run results merging with default parameters
comprehensive_results <- merge_extraction_results()
```

### Custom Configuration
```r
# Configure for specific output requirements
comprehensive_results <- merge_extraction_results(
  output_file = "results/custom_comprehensive_results.csv",
  force_rerun = FALSE,      # Use existing results if available
  verbose = TRUE           # Enable detailed progress reporting
)
```

### Pipeline Integration
```r
# As part of the full extraction pipeline
if (!interactive() || (interactive() && basename(sys.frame(1)$ofile) == "05_merge_results.R")) {
  comprehensive_results <- merge_extraction_results()
  cat("\n✅ Results merge component completed!\n")
}
```

## Configuration Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `force_rerun` | FALSE | Whether to re-run merge if results exist |
| `verbose` | TRUE | Whether to display detailed progress messages |

## Merged Data Structure

### Base Information
- `id`: Abstract identifier
- `abstract`: Original abstract text
- `title`: Publication title (if available)
- `year`: Publication year (if available)
- `journal`: Journal name (if available)

### Species Information
- `resolved_name`: Standardized species names
- `canonicalName`: Canonical species names
- `taxonRank`: Taxonomic rank
- `confidence_score`: Species identification confidence

### Methods Information
- `molecular_methods`: Molecular method detection flag
- `culture_based_methods`: Culture-based method detection flag
- `microscopy_methods`: Microscopy method detection flag
- `methods_summary`: Combined methods information

### Plant Parts Information
- `plant_parts_detected`: Detected plant parts
- `parts_count`: Number of plant parts mentioned

### Geography Information
- `countries_detected`: Detected countries
- `global_north_countries`: Global North countries
- `global_south_countries`: Global South countries
- `continents_detected`: Detected continents
- `regions_detected`: Detected geographic regions
- `has_coordinates`: Coordinate detection flag
- `geographic_summary`: Combined geographic information

## Error Handling

### Common Issues and Solutions

**Missing Component Files**
- Error: "No component result files found"
- Solution: Ensure all individual extraction components have been run

**Missing Base Data**
- Error: "Base abstracts data not found"
- Solution: Verify upstream data preparation is complete

**Column Conflicts**
- Symptom: Duplicate column warnings
- Solution: Automatic deduplication with intelligent column selection

### Validation Checks
- Component file existence validation
- Data integrity verification
- Column compatibility checking
- Output file creation confirmation

## Performance Characteristics

### Scalability
- Small datasets (< 1,000 abstracts): < 10 seconds
- Medium datasets (1,000-10,000 abstracts): 10-30 seconds
- Large datasets (> 10,000 abstracts): 30 seconds - 2 minutes

### Memory Usage
- Base memory: ~20MB for script and dependencies
- Per abstract: ~200 bytes
- Peak usage: Typically 1.5-2x base memory

### Processing Speed
- File loading: 5-15 seconds for typical datasets
- Data merging: 10-45 seconds depending on dataset size
- Statistics generation: 2-10 seconds

## Integration with Pipeline Workflow

### Upstream Dependencies
- Requires all individual extraction components to be completed:
  - Species detection (`01_extract_species.R`)
  - Methods detection (`02_extract_methods.R`)
  - Plant parts detection (`03_extract_plant_parts.R`)
  - Geography detection (`04_extract_geography.R`)

### Downstream Components
- Provides comprehensive dataset for analysis workflows
- Used as input for visualization and reporting scripts
- Enables integrated analysis across all extraction dimensions

### Workflow Position
```
Individual Component Extraction → Results Merging → Analysis and Visualization → Reporting
```

## Comprehensive Results Summary

The merged dataset enables analysis of:

### Research Coverage Analysis
- Taxonomic coverage across species, methods, and geography
- Methodological diversity and preferences
- Plant part focus areas
- Geographic research distribution

### Trend Analysis
- Temporal trends in research focus
- Geographic equity in research distribution
- Methodological evolution
- Taxonomic research gaps

### Quality Metrics
- Extraction success rates by component
- Data completeness assessment
- Geographic and taxonomic biases
- Methodological representation

## Related Components

- **Species Detection Component**: Provides species extraction data
- **Methods Detection Component**: Provides methods extraction data
- **Plant Parts Detection Component**: Provides plant parts extraction data
- **Geography Detection Component**: Provides geography extraction data

## See Also

- **Main Pipeline Documentation** - Complete pipeline overview
- **Analysis Workflow** - Downstream analysis documentation
- **Comprehensive Results** - Merged dataset for analysis