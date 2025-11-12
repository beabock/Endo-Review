# Archive Scripts Directory

This directory contains archived R scripts from previous iterations of the Endo-Review analysis pipeline.

## Organization

### Subdirectories

- **`ml_models/`** - Archived machine learning model scripts and training approaches
- **`taxa_detection/`** - Previous versions of taxa detection algorithms
- **`pipeline_versions/`** - Complete pipeline implementations from different development stages
- **`deprecated/`** - Scripts that are no longer used but preserved for reference

### File Naming Convention

Archived files follow the pattern:
- `{component}_{version}_{description}.R`
- `{date}_{component}_{changes}.R`

## Purpose

- **Version Control**: Preserve different iterations of algorithms and approaches
- **Reproducibility**: Maintain exact code used for published results
- **Development History**: Document evolution of methods and improvements
- **Reference**: Provide examples of alternative implementations

## Usage Guidelines

### When to Use Archive Scripts
- **Historical Analysis**: Reproduce results from specific publications or time periods
- **Method Comparison**: Compare performance of different algorithmic approaches
- **Debugging**: Reference previous implementations when troubleshooting

### When NOT to Use Archive Scripts
- **New Analysis**: Use current scripts in `scripts/` directory
- **Production Work**: Archive scripts may contain bugs fixed in current versions
- **Model Training**: Use latest optimized versions for new model development

## Key Archived Components

### Machine Learning Models (`ml_models/`)
- `ML_compare_models_subset.R` - Original model comparison framework
- `apply_models_old.R` - Legacy model application scripts
- `debug_model_structure.R` - Model debugging utilities

### Taxa Detection (`taxa_detection/`)
- `extracting_plant_names.R` - Original plant name extraction
- `taxa_detection.R` - Early taxa detection framework
- `improved_taxa_detection.R` - Enhanced detection algorithms

### Pipeline Versions (`pipeline_versions/`)
- Complete pipeline implementations from different development phases
- Includes data processing, model training, and analysis workflows

## Maintenance

### Archiving Process
1. Move outdated scripts to appropriate archive subdirectory
2. Update version numbers in current scripts
3. Document changes and reasons for archiving
4. Update this README with new archive contents

### Archive Integrity
- Archive files should remain unmodified once archived
- Each archive should include documentation of what it accomplishes
- Version history should be maintained in script headers

## Performance Notes

Archived scripts may:
- Use different performance optimizations than current versions
- Have different memory requirements
- Produce results in different formats
- Require different dependency versions

For current optimized performance, use scripts in the main `scripts/` directory.