# Temporal Analysis Directory

This directory contains scripts for temporal trend analysis of the endophyte literature.

## Temporal Analysis Scripts

- **`temporal_trend_analysis.R`** - Comprehensive temporal analysis of publication patterns, methods evolution, and geographic/taxonomic biases over time

## Purpose

The temporal analysis examines how endophyte research has evolved by:
- Tracking publication volumes over time
- Analyzing methodological changes (molecular vs. traditional techniques)
- Monitoring geographic and taxonomic coverage shifts
- Identifying persistent biases across decades

## Usage

Run temporal trend analysis:
```r
source("scripts/04_analysis/temporal/temporal_trend_analysis.R")
```

## Key Findings Generated

- Publication volume trends (1926-2025)
- Evolution of research methods (molecular techniques adoption)
- Geographic representation changes over time
- Taxonomic coverage expansion
- Persistent sampling biases despite methodological advances

## Output

Temporal analysis results saved to:
- `results/temporal_trends_summary.csv`
- `results/temporal_trends_report.txt`
- Temporal visualizations in `plots/`

## Dependencies

- Processed pipeline results
- Geographic and taxonomic extraction data
- Visualization utilities (`scripts/utils/plot_utils.R`)

## Research Context

Temporal analysis demonstrates that while research methods have modernized (36.8% molecular techniques) and publication volumes increased, the fundamental geographic (77% Global North) and taxonomic (0.8% species coverage) biases have remained remarkably consistent over three decades.