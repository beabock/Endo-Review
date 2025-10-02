#!/bin/bash
#SBATCH --job-name=species_mycorrhizal
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64GB
#SBATCH --time=12:00:00
#SBATCH --output=species_mycorrhizal_%j.out
#SBATCH --error=species_mycorrhizal_%j.err
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=your_email@nau.edu

# Load required modules (adjust for your HPC system)
module load R/4.3.0  # Adjust version as needed

# Change to your project directory (adjust this path to your actual project location)
cd $SLURM_SUBMIT_DIR
# Alternative: cd /path/to/your/Endo-Review/project

# Verify we're in the correct directory
if [ ! -f "01_species_mycorrhizal_hpc_optimized.R" ]; then
    echo "ERROR: Not in correct project directory. Looking for 01_species_mycorrhizal_hpc_optimized.R"
    echo "Current directory: $(pwd)"
    ls -la
    exit 1
fi

# Set up R environment
export R_LIBS_USER=$HOME/R/packages  # Default R package location

# Create output directory if it doesn't exist
mkdir -p results logs

# Run the analysis
echo "Starting species mycorrhizal analysis at $(date)"
echo "Processing on node: $SLURMD_NODENAME"
echo "Job ID: $SLURM_JOB_ID"
echo "Cores allocated: $SLURM_CPUS_PER_TASK"

# Execute R script with error handling
Rscript 01_species_mycorrhizal_hpc_optimized.R 2>&1 | tee logs/processing_${SLURM_JOB_ID}.log

# Check if output was created
if [ -f "species_mycorrhizal_results_optimized.csv" ]; then
    echo "SUCCESS: Output file created"
    ls -lh species_mycorrhizal_results_optimized.csv
    wc -l species_mycorrhizal_results_optimized.csv
else
    echo "ERROR: Output file not found"
    exit 1
fi

echo "Job completed at $(date)"