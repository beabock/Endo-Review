# Analysis Validation Directory

This directory contains validation and quality assurance scripts for the Endo-Review pipeline outputs.

## Validation Scripts

### Absence Evidence Validation
- **`absence_evidence_detection.R`** - Automated absence claim detection and validation
- **`find_all_plants_statement.R`** - Universal ubiquity claim analysis
- **`manual_validation_sample.R`** - Validation sample generation for expert review

## Purpose

These scripts validate pipeline outputs by:
- Detecting and verifying absence claims in abstracts
- Analyzing ubiquity statements across the literature
- Generating samples for manual expert validation
- Ensuring classification accuracy and reliability

## Usage

Run validation checks:
```r
source("scripts/04_analysis/validation/absence_evidence_detection.R")
source("scripts/04_analysis/validation/find_all_plants_statement.R")
```

Generate validation samples:
```r
source("scripts/04_analysis/validation/manual_validation_sample.R")
```

## Validation Methodology

1. **Automated Detection**: Rule-based algorithms identify absence statements
2. **Expert Review**: Stratified sampling for manual validation
3. **Cross-verification**: Multiple methods confirm findings
4. **Quality Assurance**: Comprehensive error checking and reporting

## Output

Validation results saved to:
- `results/validation_comparison_report.txt`
- `results/manual_validation/` directory
- Comprehensive validation logs and samples

## Dependencies

- Pipeline results from main analysis
- Reference data and configuration
- Utility functions for text processing

## Importance

Validation ensures the reliability of the "99.5% presence" finding and confirms that apparent absences are methodological artifacts rather than genuine natural absences.