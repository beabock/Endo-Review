# Optimized HPC Workflow for Species Detection

## 🚀 Quick Start (3 Steps)

### Step 1: Pre-compute Lookup Tables (Run Once)
```bash
# Copy the pre-computation script to HPC
cp 02_precompute_lookup_tables.R /path/to/hpc/root/

# Run it (takes a few minutes, but only needs to be done once)
cd /path/to/hpc/root
Rscript 02_precompute_lookup_tables.R
```

**Expected output files:**
- `lookup_tables.rds` (complete lookup tables)
- `species_hash.rds`, `genus_hash.rds`, `family_hash.rds` (hash tables)
- `species_names_vector.rds`, etc. (vector files)
- `lookup_tables_creation.log` (creation log)

### Step 2: Run Optimized HPC Script
```bash
# Copy the optimized script to HPC
cp 01_species_mycorrhizal_hpc_optimized.R /path/to/hpc/root/

# Run the optimized version (much faster!)
cd /path/to/hpc/root
Rscript 01_species_mycorrhizal_hpc_optimized.R
```

### Step 3: Monitor Progress
```bash
# Watch real-time progress
tail -f species_mycorrhizal_results_optimized_progress.txt

# Check processing log
tail -f species_mycorrhizal_results_optimized_processing.log

# Monitor system resources
htop
```

## 📊 Performance Improvements

| Feature | Original Script | Optimized Script |
|---------|----------------|------------------|
| **Lookup Table Creation** | 60+ seconds every run | < 1 second (pre-computed) |
| **Batch Size** | 200 abstracts | 50-100 abstracts (more stable) |
| **Checkpoint Interval** | 10-25 batches | 5-15 batches (more frequent) |
| **Memory Management** | Basic | Enhanced with pre-computed tables |
| **Error Recovery** | Good | Enhanced with better timeout handling |

## 🔧 Key Optimizations

### ✅ **Pre-computed Lookup Tables**
- Eliminates the 60-second bottleneck identified in debugging
- Creates hash tables once instead of every run
- Saves memory by loading only what's needed

### ✅ **Conservative HPC Settings**
- Smaller batch sizes for better stability
- More frequent checkpoints for better recovery
- Better timeout handling (10 minutes per batch, 6 hours total)

### ✅ **Enhanced Debugging**
- Better error messages and progress tracking
- Real-time progress files for monitoring
- Memory usage tracking throughout processing

### ✅ **Improved Parallel Processing**
- Better error handling for cluster setup
- Graceful fallback to sequential processing
- Timeout protection for individual batches

## 📁 Required Files in HPC Root Directory

**Scripts:**
- `01_species_mycorrhizal_hpc_optimized.R` (main script)
- `optimized_taxa_detection.R` (species detection functions)
- `reference_data_utils.R` (utility functions)
- `memory_optimization.R` (memory management, optional)

**Data Files:**
- `species.rds` (species reference data)
- `consolidated_dataset.csv` (input abstracts)
- `funtothefun.csv` (FUNGuild data for mycorrhizal classification)

**Pre-computed Files (created by Step 1):**
- `lookup_tables.rds` (complete lookup tables)
- `species_hash.rds` (species hash table)
- `genus_hash.rds` (genus hash table)
- `family_hash.rds` (family hash table)

## 🚨 Troubleshooting

### If you see errors:

1. **"Missing lookup table files"**
   ```bash
   # Make sure to run the pre-computation script first
   Rscript 02_precompute_lookup_tables.R
   ```

2. **Parallel processing hangs**
   ```bash
   # The script will automatically fall back to sequential mode
   # Check the processing log for details
   tail -f species_mycorrhizal_results_optimized_processing.log
   ```

3. **Memory issues**
   ```bash
   # Monitor memory usage
   htop
   # The script has enhanced memory management and will show warnings
   ```

## 📈 Expected Performance

**For 19,380 abstracts:**
- **Original script**: Often hangs at lookup table creation (60+ seconds)
- **Optimized script**: Should complete in 10-30 minutes with proper monitoring
- **Memory usage**: 1-4 GB (more predictable with pre-computed tables)
- **Progress tracking**: Real-time updates every 5% of progress

## 🔍 Monitoring Your Run

**Real-time progress:**
```bash
tail -f species_mycorrhizal_results_optimized_progress.txt
```

**Detailed logs:**
```bash
tail -f species_mycorrhizal_results_optimized_processing.log
```

**Check for checkpoint files:**
```bash
ls -la species_mycorrhizal_results_optimized_checkpoint_*.csv
```

The optimized script should be much more stable and provide better visibility into the processing status!