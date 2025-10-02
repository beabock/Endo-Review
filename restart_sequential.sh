#!/bin/bash
# =============================================================================
# restart_sequential.sh - Kill hanging job and restart with sequential
# =============================================================================

echo "🛑 Killing any running species detection jobs..."

# Find and kill any running jobs with species in the name
job_ids=$(squeue -u $USER --format="%.10i %.20j" --noheader | grep -i species | awk '{print $1}')

if [ -n "$job_ids" ]; then
    echo "Found running jobs: $job_ids"
    for job_id in $job_ids; do
        echo "Cancelling job $job_id..."
        scancel $job_id
    done
    echo "✅ Jobs cancelled"
else
    echo "No running species jobs found"
fi

# Wait a moment for jobs to clean up
sleep 5

# Submit new sequential job
echo "🚀 Submitting sequential processing job..."

sbatch << 'EOF'
#!/bin/bash
#SBATCH --job-name=species_sequential
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32GB
#SBATCH --time=8:00:00
#SBATCH --output=species_sequential_%j.out
#SBATCH --error=species_sequential_%j.err

echo "Starting sequential species detection at $(date)"
echo "Running on node: $SLURMD_NODENAME"
echo "Job ID: $SLURM_JOB_ID"

# Load R module (adjust for your system)
module load R/4.3.0 || module load R || echo "No R module to load"

# Run sequential version
cd $SLURM_SUBMIT_DIR
Rscript 01_species_mycorrhizal_hpc_sequential.R

echo "Sequential processing completed at $(date)"
EOF

echo "✅ Sequential job submitted!"
echo "Monitor with: squeue -u $USER"
echo "Check output with: tail -f species_sequential_*.out"