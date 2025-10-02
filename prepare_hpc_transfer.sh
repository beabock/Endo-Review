#!/bin/bash
# =============================================================================
# prepare_hpc_transfer.sh - Organize Files for HPC Transfer
# =============================================================================
#
# Purpose: Copy all required files to a single directory for HPC transfer
#
# Usage: ./prepare_hpc_transfer.sh
#        Creates hpc_transfer/ directory with all files in root
#
# =============================================================================

echo "🚀 Preparing files for HPC transfer..."

# Create transfer directory
mkdir -p hpc_transfer
cd hpc_transfer

echo "📂 Copying files to single directory structure..."

# Main scripts
echo "  • Main processing script..."
cp ../01_species_mycorrhizal_hpc_optimized.R .
cp ../submit_hpc_job.sh .
cp ../check_hpc_setup.R .

# Data files
echo "  • Data files..."
cp ../results/consolidated_dataset.csv ./consolidated_dataset.csv 2>/dev/null || echo "    ⚠️  consolidated_dataset.csv not found in results/"
cp ../funtothefun.csv . 2>/dev/null || echo "    ⚠️  funtothefun.csv not found in root"

# Model files  
echo "  • Model files..."
cp ../models/species.rds ./species.rds 2>/dev/null || echo "    ⚠️  species.rds not found in models/"
cp ../models/lookup_tables.rds ./lookup_tables.rds 2>/dev/null || echo "    ⚠️  lookup_tables.rds not found in models/"

# Optional model files
cp ../models/species_hash.rds ./species_hash.rds 2>/dev/null || echo "    ℹ️  species_hash.rds not found (optional)"
cp ../models/genus_hash.rds ./genus_hash.rds 2>/dev/null || echo "    ℹ️  genus_hash.rds not found (optional)"  
cp ../models/family_hash.rds ./family_hash.rds 2>/dev/null || echo "    ℹ️  family_hash.rds not found (optional)"

# Script dependencies
echo "  • Script dependencies..."
cp ../scripts/04_analysis/optimized_taxa_detection.R ./optimized_taxa_detection.R 2>/dev/null || echo "    ⚠️  optimized_taxa_detection.R not found"
cp ../scripts/04_analysis/utilities/reference_data_utils.R ./reference_data_utils.R 2>/dev/null || echo "    ⚠️  reference_data_utils.R not found"
cp ../scripts/utils/memory_optimization.R ./memory_optimization.R 2>/dev/null || echo "    ℹ️  memory_optimization.R not found (optional)"

cd ..

echo ""
echo "📋 Verifying HPC transfer directory..."
cd hpc_transfer

# Run setup check
if [ -f "check_hpc_setup.R" ]; then
    echo "🔍 Running setup verification..."
    R --no-restore --no-save -e "source('check_hpc_setup.R')" 2>/dev/null
else
    echo "⚠️  Setup verification script not found"
fi

echo ""
echo "📦 Transfer directory contents:"
ls -la

echo ""
echo "✅ Files ready for HPC transfer!"
echo "📁 All files are now in: $(pwd)"
echo ""
echo "🚀 Next steps:"
echo "   1. Upload entire hpc_transfer/ directory to HPC system:"
echo "      scp -r hpc_transfer/ username@hpc-system:/path/to/project/"
echo "   2. On HPC, adjust submit_hpc_job.sh for your system"
echo "   3. Submit job: sbatch submit_hpc_job.sh"