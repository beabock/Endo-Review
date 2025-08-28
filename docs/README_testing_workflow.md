# Pipeline Testing Workflow

## Overview

This testing framework provides a systematic approach to validate your extraction pipeline before running it on the full dataset. The workflow creates representative subsets, tests each component individually, and generates detailed reports to ensure everything works correctly.

## Quick Start

### 1. Create Test Subsets
```r
source("scripts/04_analysis/create_test_subset.R")

# Create random subsets of different sizes
create_test_subset(sample_sizes = c(100, 500, 1000))
```

### 2. Run Pipeline Tests
```r
source("scripts/04_analysis/test_pipeline_workflow.R")

# Test on subsets (recommended approach)
test_pipeline_workflow(
  subset_sizes = c(100, 500),
  test_components = c("data_prep", "species", "methods", "parts", "geography")
)
```

### 3. Run Full Pipeline on Subset
```r
source("scripts/04_analysis/run_extraction_pipeline.R")

# Test full pipeline on largest subset
run_extraction_pipeline(
  input_file = "test_data/test_subset_random_1000.csv",
  force_rerun = TRUE
)
```

## Step-by-Step Workflow

### Step 1: Create Representative Subsets

**Random Sampling** (Fastest, good for most cases):
```r
create_test_subset(
  sample_sizes = c(100, 500, 1000),
  sampling_method = "random"
)
```

**Stratified Sampling** (Ensures class representation):
```r
create_test_subset(
  sample_sizes = c(200, 1000),
  sampling_method = "stratified",
  stratification_column = "final_classification"
)
```

**Balanced Sampling** (Equal representation per class):
```r
create_test_subset(
  sample_sizes = c(150, 750),
  sampling_method = "balanced"
)
```

### Step 2: Test Individual Components

**Quick Test** (Species + Data Prep only):
```r
run_quick_test(subset_size = 100)
```

**Component-by-Component Test**:
```r
test_pipeline_workflow(
  subset_sizes = c(100),
  test_components = c("species", "geography")
)
```

**Full Component Test**:
```r
run_full_test(subset_sizes = c(100, 500))
```

### Step 3: Validate Results

Check the generated reports:
- `test_results/test_results_100.txt` - Individual subset results
- `test_results/comparison_report.txt` - Cross-subset comparison
- `test_data/subset_summary.txt` - Subset creation details

### Step 4: Scale Up Testing

1. **Start Small**: Test on 100 abstracts
2. **Increase Size**: Test on 500, then 1000 abstracts
3. **Check Consistency**: Compare detection rates across sizes
4. **Monitor Performance**: Watch for memory issues or scaling problems
5. **Validate Quality**: Spot-check results for accuracy

### Step 5: Run on Full Dataset

Once confident:
```r
# Run the full pipeline
run_extraction_pipeline()

# Or run individual components as needed
source("scripts/04_analysis/components/01_extract_species.R")
extract_species_data(abstracts_data)
```

## Output Structure

```
project_root/
├── test_data/
│   ├── test_subset_random_100.csv
│   ├── test_subset_random_500.csv
│   ├── test_subset_random_1000.csv
│   └── subset_summary.txt
├── test_results/
│   ├── test_results_random_100/
│   │   ├── species_results.csv
│   │   ├── methods_results.csv
│   │   ├── plant_parts_results.csv
│   │   ├── geography_results.csv
│   │   └── comprehensive_results.csv
│   ├── test_results_100.txt
│   ├── test_results_500.txt
│   └── comparison_report.txt
└── results/
    └── (final results when ready)
```

## Testing Checklist

### Pre-Flight Checks
- [ ] Test subsets created successfully
- [ ] Required columns present (id, article_title, abstract)
- [ ] No missing or empty abstracts
- [ ] Classification distribution looks reasonable

### Component Testing
- [ ] Data preparation runs without errors
- [ ] Species detection finds reasonable number of species
- [ ] Methods detection identifies techniques appropriately
- [ ] Plant parts detection working
- [ ] Geography detection finds locations
- [ ] Results merging combines all data correctly

### Quality Validation
- [ ] Detection rates consistent across subset sizes
- [ ] No obvious errors in sample results
- [ ] Memory usage acceptable
- [ ] Processing times scale linearly
- [ ] Output files created and readable

### Performance Monitoring
- [ ] Time per abstract reasonable
- [ ] Memory usage stable
- [ ] No unexpected crashes
- [ ] Progress reporting works
- [ ] Error handling functions properly

## Troubleshooting

### Common Issues

**"Input file not found"**
```r
# Ensure your data file exists
list.files("results/", pattern = "*.csv")
```

**"Missing required columns"**
```r
# Check column names in your data
data <- read_csv("results/relevant_abstracts_with_pa_predictions.csv")
names(data)
```

**Memory Issues**
```r
# Reduce batch size for testing
test_pipeline_workflow(
  subset_sizes = c(50, 100),
  batch_size = 25
)
```

**Slow Performance**
```r
# Use exact matching only for faster testing
run_extraction_pipeline(
  input_file = "test_data/test_subset_random_100.csv",
  use_fuzzy_matching = FALSE
)
```

### Validation Queries

**Check species detection quality:**
```r
species_results <- read_csv("test_results/test_results_random_100/species_results.csv")
head(species_results$resolved_name, 10)  # Check detected species
```

**Check geography detection:**
```r
geo_results <- read_csv("test_results/test_results_random_100/geography_results.csv")
table(geo_results$countries_detected)  # Check country detection
```

**Compare detection rates:**
```r
# Load comparison report
readLines("test_results/comparison_report.txt")
```

## Performance Expectations

### Typical Runtimes (on modern hardware):

| Subset Size | Data Prep | Species | Other Components | Total |
|-------------|-----------|---------|------------------|-------|
| 100 abstracts | < 1 sec | ~30 sec | ~10 sec | ~45 sec |
| 500 abstracts | < 1 sec | ~3 min | ~30 sec | ~4 min |
| 1000 abstracts | < 2 sec | ~6 min | ~1 min | ~8 min |

### Scaling Notes:
- **Species detection**: Scales linearly, most time-consuming
- **Other components**: Scale linearly, much faster
- **Memory usage**: ~100MB for 1000 abstracts
- **Full dataset**: Estimate 1-2 hours total (vs. 4+ hours monolithic)

## Best Practices

### Testing Strategy
1. **Start Small**: Always test on 100 abstracts first
2. **Test Incrementally**: Add components one by one
3. **Compare Results**: Use multiple subset sizes to check consistency
4. **Monitor Resources**: Watch memory and time usage
5. **Validate Quality**: Don't just check if it runs, check if results are good

### Error Handling
- Components are designed to fail gracefully
- Check error messages in the console
- Review intermediate results when components fail
- Use `force_rerun = TRUE` to retry after fixing issues

### Optimization Tips
- Use exact matching for faster initial testing
- Increase batch sizes on powerful machines
- Test components individually to isolate issues
- Monitor memory usage on large subsets

## Integration with Main Pipeline

### After Successful Testing:
```r
# Run on full dataset
source("scripts/04_analysis/run_extraction_pipeline.R")
run_extraction_pipeline()

# Or run individual components
source("scripts/04_analysis/components/01_extract_species.R")
source("scripts/04_analysis/components/02_extract_methods.R")
# etc.
```

### Configuration for Production:
```r
# Optimized for full dataset
run_extraction_pipeline(
  batch_size = 2000,  # Larger batches for efficiency
  use_fuzzy_matching = TRUE,  # Full accuracy
  force_rerun = FALSE  # Use cached results when available
)
```

This testing framework ensures your pipeline works correctly before committing to the full dataset runtime, saving you significant time and frustration! 🧪✅
