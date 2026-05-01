#!/bin/bash
#SBATCH --job-name=endo_ubiquity_claims
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=08:00:00
#SBATCH --output=results/logs/ubiquity_claims_%j.out

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if [ -n "${SLURM_SUBMIT_DIR:-}" ] && [ -d "${SLURM_SUBMIT_DIR}/scripts/04_analyses" ]; then
    PROJECT_ROOT="${SLURM_SUBMIT_DIR}"
fi

cd "$PROJECT_ROOT"
mkdir -p results/logs

# Defaults can be overridden via sbatch --export
INPUT_DIR="${INPUT_DIR:-/scratch/bmb646/Bea_Nick_papers_endoreview}"
OUTPUT_DIR="${OUTPUT_DIR:-results/ubiquity_claims}"
OLLAMA_HOST="${OLLAMA_HOST:-http://127.0.0.1:11434}"
OLLAMA_MODEL="${OLLAMA_MODEL:-}"
MAX_FILES="${MAX_FILES:-0}"
SOURCE_MODE="${SOURCE_MODE:-paired}"
ABSTRACT_CSV="${ABSTRACT_CSV:-/scratch/bmb646/Abstracts_for_Monsoon.csv}"
ABSTRACT_TEXT_COL="${ABSTRACT_TEXT_COL:-}"
ABSTRACT_ID_COL="${ABSTRACT_ID_COL:-}"

if [ -z "$OLLAMA_MODEL" ]; then
    echo "ERROR: OLLAMA_MODEL is required."
    echo "Example: sbatch --export=OLLAMA_MODEL=your_model_name scripts/04_analyses/run_06_ubiquity_claims_ollama.sbatch"
    exit 1
fi

# Python environment setup
if [ -x ".venv/bin/python" ]; then
    PYTHON_BIN=".venv/bin/python"
elif [ -x ".venv/Scripts/python.exe" ]; then
    PYTHON_BIN=".venv/Scripts/python.exe"
else
    module purge
    module load python/3.11
    python -m venv .venv
    PYTHON_BIN=".venv/bin/python"
fi

# Ensure required dependency for PDF parsing
"$PYTHON_BIN" -m pip install --upgrade pip >/dev/null
"$PYTHON_BIN" -m pip install pypdf >/dev/null

echo "--- Ubiquity Claim Detection Run ---"
echo "Project root: $PROJECT_ROOT"
echo "Input dir: $INPUT_DIR"
echo "Output dir: $OUTPUT_DIR"
echo "Ollama host: $OLLAMA_HOST"
echo "Ollama model: $OLLAMA_MODEL"
echo "Max files: $MAX_FILES"
echo "Source mode: $SOURCE_MODE"
echo "Abstract CSV: $ABSTRACT_CSV"

"$PYTHON_BIN" scripts/04_analyses/06_detect_ubiquity_claims_ollama.py \
  --input-dir "$INPUT_DIR" \
  --output-dir "$OUTPUT_DIR" \
    --source-mode "$SOURCE_MODE" \
    --abstract-csv "$ABSTRACT_CSV" \
    --abstract-text-col "$ABSTRACT_TEXT_COL" \
    --abstract-id-col "$ABSTRACT_ID_COL" \
  --model "$OLLAMA_MODEL" \
  --ollama-host "$OLLAMA_HOST" \
  --max-files "$MAX_FILES"

echo "Done. Outputs are in: $OUTPUT_DIR"
