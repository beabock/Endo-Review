#!/bin/bash
#SBATCH --job-name=taxa_syn_resolve
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=06:00:00
#SBATCH --output=results/logs/taxa_synonym_resolution_%j.out
#SBATCH --error=results/logs/taxa_synonym_resolution_%j.err

set -euo pipefail

# Run from repository root before submitting:
# sbatch scripts/02_taxa_resolution/slurm/run_taxa_synonym_resolution.sbatch

module purge
module load python/3.11

# Optional: activate your Monsoon env if needed
# source ~/miniconda3/etc/profile.d/conda.sh
# conda activate endo-review

python scripts/02_taxa_resolution/taxa_synonym_resolution.py \
  --input-csv data/Ollama_cleaned.csv \
  --taxon-tsv data/Reference_datasets/gbif_backbone/Taxon.tsv \
  --output-csv data/Ollama_cleaned_synresolved.csv \
  --unresolved-csv results/manual_validation/taxa_unresolved_review.csv \
  --checkpoint-json results/logs/taxa_synonym_resolution_checkpoint.json \
  --checkpoint-interval 1000 \
  --log-interval 1000 \
  --resume
