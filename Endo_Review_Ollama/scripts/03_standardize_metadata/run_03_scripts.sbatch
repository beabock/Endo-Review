#!/bin/bash
#SBATCH --job-name=endo_metadata_pipeline
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=results/logs/downstream_pipeline_%j.out

set -euo pipefail

# Navigate to project root
cd /scratch/bmb646/Endo-Review/Endo_Review_Ollama

# Load necessary modules
module purge
module load python/3.11
module load R/4.3.1  # Or the version available on Monsoon

echo "--- Starting Python Processing ---"
echo "Step 1: Standardizing Metadata..."
python scripts/03_standardize_metadata/01_standardize_metadata.py

echo "Step 2: Filtering Dataset..."
python scripts/03_standardize_metadata/02_dataset_filtering.py

echo "--- Starting R Geographic Processing ---"
echo "Step 3: Geographic Standardizing (ISO Mapping)..."
Rscript scripts/03_standardize_metadata/03_geographic_standardizing.R

echo "Step 4: Country Enrichment (GDP/Centroids)..."
Rscript scripts/03_standardize_metadata/04_country_enrichment.R

echo "--- Final Audit ---"
echo "Step 5: Generating Dataset Summary..."
python scripts/03_standardize_metadata/05_dataset_summary.py

echo "Pipeline complete. Check the 'data' folder for enriched CSVs."