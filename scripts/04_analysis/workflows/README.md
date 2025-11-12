# Analysis Workflows Directory

This directory contains workflow orchestration scripts that coordinate multiple analysis components for the Endo-Review pipeline.

## Workflows

### Main Orchestrators
- **`analysis_workflow.R`** - Primary analysis workflow coordinator
- **`run_pipeline.R`** - Complete pipeline execution script
- **`run_extraction_pipeline.R`** - Data extraction pipeline runner

## Purpose

These scripts provide high-level control over the analysis pipeline, managing:
- Component sequencing
- Error handling across modules
- Result aggregation
- Progress monitoring

## Usage

Main workflow execution:
```r
source("scripts/04_analysis/workflows/analysis_workflow.R")
```

Pipeline orchestration:
```r
source("scripts/04_analysis/workflows/run_pipeline.R")
```

## Dependencies

Requires all pipeline components to be properly configured:
- Component scripts in `components/`
- Utility functions in `utilities/`
- Configuration in `config/`
- Reference data loaded

## Output

Workflow results are saved to `results/` directory with comprehensive logging.