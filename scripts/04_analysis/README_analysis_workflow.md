# Analysis Workflow Documentation

## Overview

This document describes the organized analysis workflow for the endophyte research project. The analysis components have been reorganized into a modular structure that integrates with the existing test pipeline workflow.

## Directory Structure

```
scripts/04_analysis/
├── analysis_workflow.R              # Unified analysis workflow
├── test_pipeline_workflow.R         # Updated test pipeline (includes analysis)
├── README_analysis_workflow.md      # This documentation
├── validation/
│   ├── absence_evidence_detection.R # Absence evidence detection
│   └── manual_validation_sample.R   # Validation sample generation
├── temporal/
│   └── temporal_trend_analysis.R    # Temporal trend analysis
├── visualization/
│   └── run_taxa_visualizations.R    # Taxa visualization wrapper
└── components/                      # Extraction pipeline components
    ├── 01_extract_species.R
    ├── 02_extract_methods.R
    ├── 03_extract_plant_parts.R
    ├── 04_extract_geography.R
    └── 05_merge_results.R
```

## Analysis Components

### 1. Validation Components (`validation/`)

#### Absence Evidence Detection
- **File**: `absence_evidence_detection.R`
- **Purpose**: Identify studies reporting absence of fungal endophytes
- **Key Features**:
  - Multi-category absence pattern detection
  - Confidence scoring (Very High, High, Medium, Low, None)
  - Context snippet extraction
  - Methodological validation
- **Output**: Comprehensive absence analysis with confidence levels

#### Manual Validation Sampling
- **File**: `manual_validation_sample.R`
- **Purpose**: Create stratified samples for manual validation
- **Key Features**:
  - Stratified sampling by confidence and prediction type
  - Balanced representation across categories
  - Automated validation template generation
  - Progress tracking
- **Output**: Validation sample with instructions and tracking

### 2. Temporal Analysis (`temporal/`)

#### Temporal Trend Analysis
- **File**: `temporal_trend_analysis.R`
- **Purpose**: Analyze research patterns over time
- **Key Features**:
  - Publication volume trends
  - Research method evolution
  - Geographic research patterns
  - Species detection trends
  - Information completeness analysis
- **Output**: Multiple CSV files and comprehensive trend reports

### 3. Visualization (`visualization/`)

#### Taxa Visualization
- **File**: `run_taxa_visualizations.R`
- **Purpose**: Generate taxa representation visualizations
- **Key Features**:
  - Calls main visualization script
  - Configurable file paths
  - Plot generation and organization
- **Output**: Visualization plots in specified directory

## Unified Analysis Workflow

The `analysis_workflow.R` provides a unified interface for running all analysis components:

```r
source("scripts/04_analysis/analysis_workflow.R")

# Run full analysis suite
run_full_analysis()

# Run specific components
run_analysis_workflow(
  analysis_components = c("absence", "temporal")
)

# Quick analysis (absence + validation only)
run_quick_analysis()
```

## Integration with Test Pipeline

The analysis components are now integrated into the test pipeline workflow:

```r
source("scripts/04_analysis/test_pipeline_workflow.R")

# Test with analysis components
test_pipeline_workflow(
  subset_sizes = c(100, 500),
  test_components = c("data_prep", "species", "methods", "parts", "geography", "merge", "analysis")
)
```

## Usage Examples

### Complete Analysis Pipeline

```r
# 1. Test pipeline with analysis components
source("scripts/04_analysis/test_pipeline_workflow.R")
test_pipeline_workflow(
  subset_sizes = c(100, 500),
  test_components = c("data_prep", "species", "methods", "parts", "geography", "merge", "analysis")
)

# 2. Run full analysis on comprehensive results
source("scripts/04_analysis/analysis_workflow.R")
run_full_analysis()
```

### Individual Component Usage

```r
# Absence detection
source("scripts/04_analysis/validation/absence_evidence_detection.R")

# Validation sampling
source("scripts/04_analysis/validation/manual_validation_sample.R")

# Temporal analysis
source("scripts/04_analysis/temporal/temporal_trend_analysis.R")

# Visualization
source("scripts/04_analysis/visualization/run_taxa_visualizations.R")
```

### Analysis on Subsets

```r
# Run analysis on test subset
run_analysis_workflow(
  input_file = "test_results/test_results_random_100/comprehensive_results.csv",
  analysis_components = c("absence", "validation")
)
```

## Output Structure

```
results/
├── analysis/                          # Analysis results directory
│   ├── integrated_analysis_summary.txt # Summary report
│   ├── absence_evidence_analysis.csv   # Absence detection results
│   ├── high_confidence_absence_evidence.csv
│   ├── all_papers_with_absence_matches.csv
│   ├── absence_evidence_report.txt
│   ├── validation_sample_for_manual_review.csv
│   ├── VALIDATION_INSTRUCTIONS.md
│   ├── validation_progress_tracker.csv
│   ├── temporal_trends_summary.csv
│   ├── annual_publication_counts.csv
│   ├── research_method_trends.csv
│   └── temporal_trends_report.txt
├── plots/                             # Visualization outputs
└── comprehensive_extraction_results.csv # Input for analysis
```

## Dependencies

All analysis scripts require:
- `tidyverse` package
- `stringr` package (for absence detection)
- `lubridate` package (for temporal analysis)
- `ggplot2` and `viridis` packages (for temporal plots)
- `progress` package (for progress bars)

## Best Practices

### Testing Strategy
1. **Start Small**: Test analysis components on 100-abstract subsets first
2. **Component Isolation**: Test individual components before running the full suite
3. **Error Handling**: Check for missing dependencies and data files
4. **Output Validation**: Review intermediate results for quality

### Performance Considerations
- **Memory Usage**: Analysis components may require additional memory for large datasets
- **Processing Time**: Temporal analysis is the most time-intensive component
- **File I/O**: Multiple CSV outputs are generated; ensure sufficient disk space

### Quality Control
- **Manual Review**: Always review high-confidence absence cases manually
- **Cross-Validation**: Compare results across different subset sizes
- **Error Checking**: Monitor for missing data or processing errors

## Troubleshooting

### Common Issues

**Missing Dependencies**:
```r
install.packages(c("tidyverse", "stringr", "lubridate", "ggplot2", "viridis", "progress"))
```

**File Not Found Errors**:
- Ensure `comprehensive_extraction_results.csv` exists in `results/` directory
- Check file paths in analysis scripts
- Verify working directory is set correctly

**Memory Issues**:
- Process data in smaller chunks for large datasets
- Clear intermediate objects when possible
- Monitor memory usage during processing

**Long Processing Times**:
- Start with smaller subsets for initial testing
- Consider running components individually
- Check for infinite loops in data processing

### Validation Queries

**Check analysis results**:
```r
# View analysis summary
readLines("results/analysis/integrated_analysis_summary.txt")

# Check absence detection results
absence_data <- read_csv("results/analysis/absence_evidence_analysis.csv")
table(absence_data$confidence_level)
```

**Monitor validation progress**:
```r
# Check validation sample
validation_data <- read_csv("results/analysis/validation_sample_for_manual_review.csv")
nrow(validation_data)  # Sample size
table(validation_data$predicted_label)  # Prediction distribution
```

## Future Enhancements

### Planned Improvements
- **Automated Quality Control**: Statistical validation of analysis results
- **Interactive Dashboards**: Web-based visualization of analysis results
- **Batch Processing**: Parallel processing for large datasets
- **Configuration Files**: YAML-based configuration for analysis parameters
- **Result Caching**: Intelligent caching to avoid redundant computations

### Integration Opportunities
- **Manuscript Generation**: Automated report generation from analysis results
- **Database Integration**: Direct connection to publication databases
- **API Development**: RESTful API for analysis components
- **Workflow Orchestration**: Integration with workflow management systems

## Contributing

When adding new analysis components:
1. Follow the established directory structure
2. Include comprehensive documentation
3. Add error handling and logging
4. Update this README with component details
5. Test integration with the unified workflow
6. Update dependency lists if new packages are required

## Support

For questions about specific analysis components:
- Review individual script documentation
- Check error messages and logs
- Validate input data format and completeness
- Test with smaller datasets first

This organized structure ensures that analysis components are maintainable, testable, and easily integrated into the broader research workflow.
