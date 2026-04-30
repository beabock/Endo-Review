#!/bin/bash
#SBATCH --job-name=endo_metadata_pipeline
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=results/logs/downstream_pipeline_%j.out

set -euo pipefail

# Navigate to project root (prefer submit directory when available)
PROJECT_ROOT="${SLURM_SUBMIT_DIR:-/scratch/bmb646/Endo-Review/Endo_Review_Ollama}"
cd "$PROJECT_ROOT"

# Ensure log/output directory exists before writing job logs/artifacts
mkdir -p results/logs

# Load necessary modules
module purge
module load R/4.5.2  # Use an available Monsoon R module version

# Prefer the project virtual environment if it exists; otherwise bootstrap one.
if [ -x ".venv/bin/python" ]; then
    PYTHON_BIN=".venv/bin/python"
elif [ -x ".venv/Scripts/python.exe" ]; then
    PYTHON_BIN=".venv/Scripts/python.exe"
else
    module load python/3.11
    python -m venv .venv
    PYTHON_BIN=".venv/bin/python"
fi

# Ensure pandas is available in the interpreter used by the pipeline.
if ! "$PYTHON_BIN" -c "import pandas" >/dev/null 2>&1; then
    "$PYTHON_BIN" -m pip install --upgrade pip pandas
fi

echo "--- Environment Preflight ---"
"$PYTHON_BIN" - <<'PY'
import sys
print(f"Python executable: {sys.executable}")
print(f"Python version: {sys.version.split()[0]}")
try:
    import pandas  # noqa: F401
    print("Python dependency check: pandas OK")
except Exception as exc:
    raise SystemExit(f"Python dependency check failed (pandas): {exc}")
PY

Rscript -e "cat('R version:', as.character(getRversion()), '\n')"
Rscript -e "if (!requireNamespace('dplyr', quietly=TRUE)) stop('Missing R package: dplyr')"
Rscript -e "if (!requireNamespace('stringr', quietly=TRUE)) stop('Missing R package: stringr')"
Rscript -e "if (!requireNamespace('jsonlite', quietly=TRUE)) stop('Missing R package: jsonlite')"
# Ensure geographic/R spatial packages are available; attempt non-interactive install if missing.
# Note: 'sf' may require system dependencies; installation will fail if system libs are missing.
Rscript -e "if (!requireNamespace('sf', quietly=TRUE)) { try(install.packages('sf', repos='https://cloud.r-project.org'), silent=FALSE); if (!requireNamespace('sf', quietly=TRUE)) stop('Missing R package: sf (and automatic install failed)') }"
Rscript -e "if (!requireNamespace('rnaturalearth', quietly=TRUE)) { install.packages('rnaturalearth', repos='https://cloud.r-project.org') }"
Rscript -e "if (!requireNamespace('rnaturalearthdata', quietly=TRUE)) { install.packages('rnaturalearthdata', repos='https://cloud.r-project.org') }"

echo "--- Starting Python Processing ---"
echo "Step 1: Standardizing Metadata..."
"$PYTHON_BIN" scripts/03_standardize_metadata/01_standardize_metadata.py

echo "Step 2: Filtering Dataset..."
"$PYTHON_BIN" scripts/03_standardize_metadata/02_dataset_filtering.py

echo "--- Starting R Geographic Processing ---"
echo "Step 3: Geographic Standardizing (ISO Mapping)..."
Rscript scripts/03_standardize_metadata/03_geographic_standardizing.R

echo "Step 4: Country Enrichment (GDP/Centroids)..."
Rscript scripts/03_standardize_metadata/04_country_enrichment.R

echo "--- Final Audit ---"
echo "Step 5: Generating Dataset Summary..."
"$PYTHON_BIN" scripts/03_standardize_metadata/05_dataset_summary.py

echo "Step 6: Running pre-analysis QA check..."
"$PYTHON_BIN" scripts/03_standardize_metadata/06_preanalysis_check.py

echo "Pipeline complete. Check the 'data' folder for enriched CSVs."