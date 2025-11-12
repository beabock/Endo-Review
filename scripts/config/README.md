# Configuration Directory

This directory contains configuration files that centralize settings, file paths, and parameters for the Endo-Review analysis pipeline.

## Configuration Files

### `pipeline_config.R` - Main Configuration
Centralized configuration file containing:

#### File Paths and Directories
- **Input Files**: Training data, reference data, and raw datasets
- **Output Directories**: Results, plots, models, and temporary files
- **Model Files**: Trained model storage locations

#### Data Processing Parameters
- **Column Mappings**: Standard column names and harmonization rules
- **Batch Sizes**: Memory-efficient processing chunk sizes
- **Parallel Processing**: Worker counts and parallel configuration

#### Model Parameters
- **Relevance Thresholds**: Classification decision boundaries
- **Ensemble Weights**: Optimized model combination weights
- **Cross-validation**: Training validation parameters

#### Validation Settings
- **Manual Validation**: Sample sizes and stratification parameters
- **Quality Control**: Data integrity and completeness checks

## Usage

The configuration is automatically loaded by pipeline scripts:

```r
source("scripts/config/pipeline_config.R")
```

Access configuration values:
```r
# File paths
training_data <- INPUT_FILES$training_labeled

# Model parameters
ensemble_weights <- ENSEMBLE_WEIGHTS

# Processing settings
batch_size <- BATCH_SIZES$extraction
```

## Configuration Functions

### File Management
- `check_required_files()` - Verify required data files exist
- `ensure_output_dirs()` - Create output directories
- `harmonize_column_names()` - Standardize column names
- `safe_read_csv()` - Robust file reading with fallbacks

### Utility Functions
- `manage_memory()` - Memory management during processing
- `validate_dataframe()` - Data structure validation

## Modification Guidelines

When modifying configuration:

1. **Test Changes**: Run pipeline tests after modifications
2. **Document Changes**: Update comments explaining parameter purposes
3. **Version Control**: Track configuration changes with git
4. **Backwards Compatibility**: Ensure changes don't break existing scripts

## Dependencies

- No external package dependencies (uses base R)
- Automatically creates required output directories on load
- Provides fallback mechanisms for missing files

## Integration

The configuration file is sourced by all major pipeline components:
- Model training scripts
- Data processing pipelines
- Analysis workflows
- Utility functions

This ensures consistent parameter usage across the entire project.