# HPC-Optimized Species Detection Script

## Overview

The `01_species_mycorrhizal_hpc.R` script is an HPC-optimized version of the species detection and mycorrhizal classification pipeline, designed to leverage high-performance computing resources for faster processing of large datasets.

## Key HPC Optimizations

### 🚀 Enhanced Parallel Processing
- **Automatic core detection**: Uses all available cores minus 2 (reserving some for system processes)
- **Dynamic core allocation**: Adjusts based on detected HPC environment (32+ cores for large nodes, 16+ for medium, 8+ for small)
- **Parallel cluster management**: Robust error handling for cluster creation and worker management
- **Windows compatibility**: Automatically falls back to sequential processing on Windows systems
- **Graceful degradation**: Continues with optimized sequential processing if parallel setup fails

### 💾 Periodic Checkpointing
- **Configurable checkpoint intervals**: Saves progress every N batches (default: 10-25 depending on HPC size)
- **Automatic resume**: Detects and loads latest checkpoint on restart
- **Checkpoint cleanup**: Removes old checkpoints to save disk space
- **Fault tolerance**: Continues processing even if individual batches fail
- **Timeout protection**: 30-minute timeout per batch prevents infinite hangs
- **Emergency checkpoints**: Automatic emergency saves if timeout occurs

### 🧠 Memory Management
- **HPC-aware memory limits**: Automatically detects and respects node memory constraints
- **Aggressive garbage collection**: Periodic cleanup to prevent memory bloat
- **Chunked processing**: Processes large datasets in memory-efficient chunks
- **Memory monitoring**: Optional integration with existing memory monitoring utilities

### 📁 Root-Relative File Paths
- **Simplified deployment**: All file paths are relative to the root directory
- **Required files in root**:
  - `species.rds` - Species reference data
  - `consolidated_dataset.csv` - Input abstracts data
  - `funtothefun.csv` - FUNGuild dataset for mycorrhizal classification

## HPC Environment Detection

The script automatically detects your HPC environment and configures itself accordingly:

| Environment | Cores | Memory | Batch Size | Checkpoint Interval |
|-------------|-------|--------|------------|-------------------|
| **Large HPC Node** | 48 | 128GB+ | 200 | 10 batches |
| **Medium HPC Node** | 24 | 64GB+ | 150 | 15 batches |
| **Small HPC Node** | 12 | 32GB+ | 100 | 20 batches |
| **Desktop/Local** | 4 | <32GB | 50 | 25 batches |

## HPC Resource Requests

When submitting jobs to your HPC system, use these guidelines based on your dataset size:

### Small Datasets (< 10,000 abstracts)
```bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32GB
#SBATCH --time=02:00:00
#SBATCH --job-name=mycorrhizal_small
```

### Medium Datasets (10,000 - 50,000 abstracts)
```bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem=96GB
#SBATCH --time=06:00:00
#SBATCH --job-name=mycorrhizal_medium
```

**For 20,000 abstracts (your dataset size):**
```bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64GB
#SBATCH --time=03:00:00
#SBATCH --job-name=mycorrhizal_20k
```

### Large Datasets (50,000 - 200,000 abstracts)
```bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=192GB
#SBATCH --time=12:00:00
#SBATCH --job-name=mycorrhizal_large
```

### Very Large Datasets (> 200,000 abstracts)
```bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem=256GB
#SBATCH --time=24:00:00
#SBATCH --job-name=mycorrhizal_xlarge
```

### Interactive Session Example
```bash
# For testing and development
srun --nodes=1 --ntasks=1 --cpus-per-task=16 --mem=64GB --time=04:00:00 --job-name=mycorrhizal_test R
```

### Resource Estimation Tips

- **Memory**: Plan for ~2-4GB per 1,000 abstracts + 20GB base overhead
- **Time**: ~1-2 minutes per 1,000 abstracts on modern HPC nodes
- **Cores**: More cores = faster processing, but memory per core decreases
- **Start conservative**: Begin with medium resources and scale up if needed

**Example for 100,000 abstracts:**
- Expected runtime: 2-4 hours
- Recommended: 32 cores, 128GB memory, 6-hour time limit

## Usage

### Basic Usage
```r
# Run the HPC-optimized script
source("01_species_mycorrhizal_hpc.R")
```

### Advanced Usage with Custom Configuration
```r
# Load data
abstracts_data <- read_csv("consolidated_dataset.csv")

# Custom HPC configuration (optional)
hpc_config <- list(
  cores = 32,
  batch_size = 150,
  memory_limit_gb = 64,
  checkpoint_interval = 10
)

# Run with custom config
results <- extract_species_mycorrhizal_data_hpc(
  abstracts_data,
  output_file = "species_mycorrhizal_results.csv",
  hpc_config = hpc_config,
  verbose = TRUE
)
```

## File Requirements

Ensure these files are present in your root directory:

1. **`species.rds`** - Species reference database
2. **`consolidated_dataset.csv`** - Preprocessed abstracts with id and abstract columns
3. **`funtothefun.csv`** - FUNGuild dataset for mycorrhizal classification

## HPC Deployment Tips

### SLURM Interactive Session
```bash
# Start interactive RStudio session on HPC
srun --nodes=1 --ntasks=1 --cpus-per-task=48 --mem=128GB --time=08:00:00 --job-name=mycorrhizal_analysis R

# Within R:
source("01_species_mycorrhizal_hpc.R")
```

### Batch Processing
```bash
#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=128GB
#SBATCH --time=12:00:00
#SBATCH --job-name=mycorrhizal_hpc

Rscript 01_species_mycorrhizal_hpc.R
```

## Performance Improvements

Compared to the original script, you can expect:

- **3-10x faster processing** on multi-core HPC nodes
- **Better memory utilization** with automatic chunking and cleanup
- **Fault tolerance** with automatic checkpointing and resume
- **Scalability** to very large datasets (100k+ abstracts)

## Monitoring Progress

The script provides comprehensive progress monitoring and logging:

### 📊 Progress Tracking
- **Real-time batch progress** with species detection counts
- **Memory usage monitoring** at key stages
- **Checkpoint save notifications** with file sizes
- **Performance timing** for each major operation
- **Progress file** (`_progress.txt`) for external monitoring

### 📋 Comprehensive Logging
- **Detailed log file** (`_processing.log`) with timestamps
- **Performance metrics** for each operation
- **Memory usage tracking** throughout processing
- **Error logging** with context and troubleshooting info
- **Timeout monitoring** with emergency procedures

### 🚨 Debugging Features
- **Batch timeout protection** (30-minute limit per batch)
- **Emergency checkpoint saving** if timeout occurs
- **Detailed error messages** with suggested solutions
- **Processing bottleneck identification**

### 📈 Sample Output
```
🚀 === HPC-OPTIMIZED SPECIES DETECTION ===
📂 Loading consolidated dataset...
⏱️ Dataset loading completed in 2.3 minutes
✅ Loaded 19380 abstracts from consolidated_dataset.csv
🔍 Final HPC memory check before processing...
🧠 Memory usage (Pre-processing): 4.2GB
🚀 Starting HPC-optimized species detection for 19380 abstracts
📊 Processing 19380 abstracts in 97 batches
⚙️ Batch size: 200 abstracts per batch
💾 Checkpoint interval: 10 batches
🚀 Batch 10 of 97 (200 abstracts)
   🚀 Processing with HPC parallel optimization
   📊 Progress: 10% (1,247 species found)
   💾 Saving checkpoint at batch 10
   📊 Total results collected so far: 10 batches
   Size: 2.5 MB
```

## Troubleshooting

### Common Issues

1. **Memory errors**: Try reducing batch size or use a node with more memory
2. **Checkpoint loading errors**: Delete old checkpoint files manually if needed
3. **Missing files**: Ensure all required files are in the root directory
4. **Parallel processing issues**: The script falls back to sequential processing if parallel setup fails
5. **Windows compatibility**: On Windows, the script automatically uses sequential processing with optimized settings

### Platform-Specific Notes

**On HPC (Linux/macOS):**
- Uses full parallel processing with all available cores
- FORK clusters for optimal performance
- Best for large datasets and production runs

**On Windows (RStudio):**
- FORK clusters don't work on Windows (OS limitation)
- Automatically falls back to sequential processing
- PSOCK clusters often fail due to R environment setup
- Still provides optimized batch processing and checkpointing

**Expected Output:**
```
🚀 Setting up HPC parallel processing with 48 cores
⚠️ Full parallel setup failed: [error message]
✅ Running in sequential mode with optimized settings
```

### Why Parallel Processing Failed

**Root Causes:**
1. **OS Limitation**: Windows doesn't support FORK clusters (Unix/Linux only)
2. **PSOCK Issues**: Windows PSOCK clusters often fail due to:
   - R environment configuration
   - Firewall/network settings
   - User permissions
   - Conflicting R sessions/processes
3. **Function Compatibility**: Underlying functions may override parallel settings

### Solutions for HPC Environment

**When running on actual HPC (Linux):**
1. **Use FORK clusters** (automatically selected on Unix systems)
2. **Ensure proper modules**: `module load R` (if using module system)
3. **Request proper resources** in SLURM script
4. **Test parallel setup** before running full dataset

**Example HPC Run:**
```bash
#!/bin/bash
#SBATCH --nodes=1 --ntasks=1 --cpus-per-task=16 --mem=64GB --time=03:00:00
module load R/4.2.0  # or your R version
Rscript 01_species_mycorrhizal_hpc.R
```

### Getting Help

- **Check log files**: `species_mycorrhizal_results_processing.log` and `_progress.txt`
- **Monitor progress**: Use `tail -f _progress.txt` to watch real-time progress
- **Debug timeouts**: Check for 30-minute batch timeout warnings
- **Memory issues**: Look for memory usage warnings and garbage collection messages
- **Empty checkpoints**: Check the log for "No results to save" or timeout messages

### Log Files Created

**`_processing.log`** - Comprehensive technical log with:
- Detailed error messages and stack traces
- Performance timing for each operation
- Memory usage at key checkpoints
- Batch processing status and results

**`_progress.txt`** - Simple progress updates for:
- Real-time monitoring during long runs
- Species detection counts by batch
- Checkpoint save confirmations

**`species_mycorrhizal_results_checkpoint_batch_X_of_Y.csv`** - Periodic backups

### Debugging Checklist

1. **Check if script started**: Look for "HPC SPECIES DETECTION - PROCESSING LOG" in log file
2. **Monitor progress**: Use `tail -f _progress.txt` to watch real-time updates
3. **Check for timeouts**: Look for "timed out after" messages
4. **Verify data loading**: Confirm "Loaded X abstracts" message appears
5. **Check memory usage**: Look for memory warnings or errors
6. **Examine checkpoints**: Verify checkpoint files have actual data rows (not just headers)

### Getting Help

- Ensure all required R packages are installed (tidyverse, parallel, janitor, tictoc)
- Verify file permissions and paths in your HPC environment
- For Windows: Sequential mode is normal and expected
- Check HPC job logs for system-level errors (memory, disk space, etc.)