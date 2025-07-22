# Taxa Detection for Plant Species in Abstracts

This package provides optimized functions for detecting plant species in scientific abstracts, with special handling for abbreviated genus names (e.g., "A. macrophyllum") and taxonomic synonyms.

## Files

After consolidation, there are now just two main files:

1. **taxa_detection.R**: The main file containing all the optimized functions for taxa detection, including:
   - Improved regex patterns for extracting species names
   - Better handling of abbreviated genus names
   - Proper synonym resolution
   - Parallel processing for large datasets
   - Memory-efficient batch processing

2. **test_taxa_detection.R**: A test script that demonstrates the key functionality and performance of the taxa detection functions.

## Getting Started

### Prerequisites

Make sure you have the following R packages installed:

```r
install.packages(c("tidyverse", "rgbif", "stringr", "furrr", "future"))
```

You also need a `species.rds` file containing the taxonomic data. This should be a data frame with at least the following columns:
- `taxonID`: A unique identifier for each taxon
- `canonicalName`: The scientific name of the taxon
- `taxonomicStatus`: The status of the taxon (e.g., "accepted", "synonym")
- `acceptedNameUsageID`: For synonyms, the taxonID of the accepted name
- `kingdom`, `phylum`, `family`, `genus`: Taxonomic hierarchy information

### Basic Usage

```r
# Load the taxa detection functions
source("taxa_detection.R")

# Run the taxa detection on your data
results <- run_taxa_detection(
  input_file = "full_predictions_with_metadata.csv",
  output_file = "taxa_info_results.csv"
)
```

### Testing the Functions

To test if the functions are working correctly:

```r
source("test_taxa_detection.R")
```

This will run tests for:
- Standard species names
- Abbreviated genus names (e.g., "A. macrophyllum")
- Multiple species in the same abstract
- Species names with punctuation
- Synonym resolution

### Finding the Optimal Batch Size

To find the optimal batch size for your system:

```r
source("taxa_detection.R")

optimal_batch_size <- benchmark_batch_sizes(
  input_file = "full_predictions_with_metadata.csv",
  sample_size = 100
)
```

### Processing a Large Dataset

For processing a large dataset (e.g., 10,000+ abstracts):

```r
source("taxa_detection.R")
results <- run_taxa_detection(
  input_file = "full_predictions_with_metadata.csv",
  output_file = "taxa_info_results.csv",
  batch_size = 50  # Adjust based on your system's performance
)
```

## Performance Considerations

- **Memory Usage**: Processing 10,000 abstracts may require 2-6GB of memory, depending on your data and system.
- **Processing Time**: With 4 CPU cores, processing 10,000 abstracts typically takes 8-16 minutes.
- **Batch Size**: A smaller batch size (e.g., 10-25) uses less memory but may be slightly slower. A larger batch size (e.g., 50-100) may be faster but uses more memory.

### Automatic Hardware Optimization

The code now includes automatic hardware detection and optimization. It can detect your CPU model and available RAM to suggest optimal settings:

```r
# Get optimal settings for your hardware
optimal_settings <- get_optimal_settings()

# Run with automatically optimized settings
results <- run_taxa_detection(
  input_file = "full_predictions_with_metadata.csv",
  output_file = "taxa_info_results.csv",
  batch_size = optimal_settings$batch_size,
  workers = optimal_settings$workers,
  memory_limit = optimal_settings$memory_limit,
  low_memory_mode = optimal_settings$low_memory_mode
)
```

#### For High-Performance Systems (like your i7-1255U with 15.7GB RAM)

The automatic optimization will:
1. Use more worker processes (up to 10 for i7/i9 processors)
2. Set larger batch sizes (100-150) for faster processing
3. Allocate more memory (12-13GB) for in-memory processing
4. Disable low memory mode for maximum performance

#### For Systems with Limited Resources

For systems with less RAM or fewer CPU cores, the optimization will:
1. Use fewer worker processes
2. Set smaller batch sizes
3. Enable low memory mode if RAM is limited
4. Provide appropriate memory limits

### Manual Memory Optimization

If you prefer to manually set memory parameters:

```r
# Run with custom memory settings
results <- run_taxa_detection(
  input_file = "full_predictions_with_metadata.csv",
  output_file = "taxa_info_results.csv",
  batch_size = 10,  # Use a smaller batch size
  memory_limit = 5000,  # Set memory limit to 5GB
  low_memory_mode = TRUE  # Enable low memory mode
)
```

The code includes automatic memory monitoring and will:
1. Track memory usage throughout processing
2. Automatically switch to low memory mode if usage gets too high
3. Save intermediate results to disk to free up memory
4. Provide memory usage statistics to help you optimize

## Key Improvements

1. **Abbreviated Genus Names**: The code now correctly detects abbreviated genus names like "A. macrophyllum" and expands them to their full form.

2. **Synonym Handling**: When an old name (synonym) is detected, it's properly connected to the current accepted name in the results.

3. **Parallel Processing**: The code uses multiple CPU cores to process abstracts in parallel, significantly improving performance.

4. **Memory Efficiency**: Abstracts are processed in batches to manage memory usage, allowing for processing of very large datasets.

## Example Output

The output CSV file will contain the following columns:

- `id`: The abstract ID
- `predicted_label`: The predicted label for the abstract
- `match_type`: The type of match (species, genus, family, or none)
- `resolved_name`: The resolved scientific name
- `status`: The status of the match (ACCEPTED, SYNONYM, or NO_MATCH)
- `acceptedScientificName`: The accepted scientific name (same as resolved_name for accepted names)
- Various plant part indicators (e.g., leaf, stem, root)

