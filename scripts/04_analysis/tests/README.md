# Analysis Tests Directory

This directory contains test scripts for the Endo-Review analysis pipeline components. These tests validate the functionality, performance, and robustness of individual pipeline modules.

## Test Categories

### Core Component Tests
- **`test_extract_species.R`** - Validates species name extraction algorithms
- **`test_pipeline_workflow.R`** - Tests the complete pipeline workflow
- **`test_pipeline.R`** - Comprehensive pipeline testing

### Specialized Tests
- **`test_bloom_filters.R`** - Tests probabilistic filtering performance
- **`test_geographic_detection.R`** - Validates location extraction
- **`test_geography_australia.R`** - Australian-specific geography tests
- **`test_memory_optimized_species_mycorrhizal.R`** - Memory optimization tests
- **`test_optimized_taxa_detection.R`** - Enhanced taxa detection validation

### Utility Tests
- **`test_plot_utils.R`** - Plotting utilities validation
- **`test_country_codes.R`** - Geographic data standardization
- **`test_enhanced_geography_detection.R`** - Improved location parsing
- **`test_enhanced_mycorrhizal_output.R`** - Mycorrhizal filtering tests

## Running Tests

Individual tests can be run directly:
```r
source("scripts/04_analysis/tests/test_extract_species.R")
```

For comprehensive testing, use the main test scripts:
```r
source("scripts/04_analysis/test_pipeline_workflow.R")  # Component validation
source("scripts/04_analysis/quick_start_testing.R")     # Quick pipeline test
```

## Test Data

Tests use the `test_data/` directory containing:
- Subset datasets for performance testing
- Expected results for validation
- Reference data for comparison

## Dependencies

All tests require:
- Pipeline configuration (`scripts/config/pipeline_config.R`)
- Utility functions (`scripts/utils/`)
- Test data (`test_data/`)

## Output

Test results are saved to `test_results/` directory with detailed logs and performance metrics.

## Contributing

When adding new tests:
1. Follow naming convention: `test_[component].R`
2. Include comprehensive error handling
3. Document test purpose and expected outcomes
4. Add results validation
5. Update this README