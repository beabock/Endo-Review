# HPC Deployment Checklist for Species Mycorrhizal Analysis

## Pre-Deployment Checklist

### ✅ Data Files Required (All in Root Directory)
- [ ] `consolidated_dataset.csv` (20,000 abstracts)
- [ ] `funtothefun.csv` (FUNGuild dataset)  
- [ ] `lookup_tables.rds` (pre-computed lookup tables)
- [ ] `species.rds` (species reference data)
- [ ] Optional: `species_hash.rds`, `genus_hash.rds`, `family_hash.rds`

### ✅ Script Dependencies (All in Root Directory)
- [ ] `optimized_taxa_detection.R`
- [ ] `reference_data_utils.R`
- [ ] `memory_optimization.R` (optional)

### ✅ R Packages Required
```r
# Core packages
library(tidyverse)  
library(tictoc)
library(janitor)
library(parallel)
library(purrr)
```

## Resource Recommendations

### Option 1: Conservative (Recommended for first run)
```bash
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64GB  
#SBATCH --time=12:00:00
```

### Option 2: High-Performance
```bash
#SBATCH --nodes=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=128GB
#SBATCH --time=8:00:00  
```

## Performance Estimates (20,000 abstracts)

Based on test run (50 abstracts in 1.1 minutes):

| Mode | Cores | Est. Time | Memory | Output Size |
|------|-------|-----------|---------|-------------|
| Sequential | 1 | ~7.5 hours | 8-16GB | ~12MB |
| Parallel | 16 | ~3-4 hours | 32-64GB | ~12MB |  
| Parallel | 32 | ~2-3 hours | 64-128GB | ~12MB |

## Configuration Changes for Production

### 1. Enable Full Dataset Processing
In `01_species_mycorrhizal_hpc_optimized.R`, ensure test mode is commented out:
```r
# PRODUCTION MODE: Process all abstracts
# abstracts_data <- abstracts_data[1:50, ]  # Keep this commented for full run
```

### 2. Enable Parallel Processing  
Uncomment parallel processing section and comment out sequential mode

### 3. Adjust Batch Sizes for Scale
- Sequential: 50-100 abstracts per batch
- Parallel: 100-200 abstracts per batch

## Monitoring and Checkpointing

- ✅ Automatic checkpointing every 3-5 batches
- ✅ Progress logging to `.log` files
- ✅ Memory usage monitoring
- ✅ Timeout protection (6 hours max per job)

## Expected Output

For 20,000 abstracts:
- **Records**: ~100,000 species detection records
- **File size**: ~10-15 MB CSV file
- **Processing rate**: ~45 abstracts/minute (sequential), ~150-200/minute (parallel)

## Troubleshooting

### Common Issues:
1. **Out of memory**: Reduce batch size, increase memory request
2. **Timeout**: Request longer walltime or enable parallel processing  
3. **Missing files**: Check all dependencies are uploaded
4. **R package errors**: Ensure all packages installed in HPC environment

### Debug Mode:
Add debug output by uncommenting debug sections in the script

## Job Submission

```bash
# Upload files to HPC
scp -r * username@hpc-system:/path/to/project/

# Submit job
sbatch submit_hpc_job.sh

# Monitor progress  
squeue -u username
tail -f species_mycorrhizal_results_optimized_processing.log
```