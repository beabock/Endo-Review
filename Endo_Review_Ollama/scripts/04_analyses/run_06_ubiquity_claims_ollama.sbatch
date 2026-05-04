#!/bin/bash
#SBATCH --job-name=endo_ubiquity_claims
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --gres=gpu:a100:1
#SBATCH --time=08:00:00
#SBATCH --output=results/logs/ubiquity_claims_%j.out
#
# Ollama-based ubiquity claims detection for endophyte abstracts and full texts.
# Requires Monsoon module system: module load ollama
# Models pre-staged at: /scratch/bmb646/ollama_models
#
# Usage:
#   sbatch run_06_ubiquity_claims_ollama.sbatch
#   sbatch --export=OLLAMA_MODEL=mistral run_06_ubiquity_claims_ollama.sbatch
#   sbatch --export=SOURCE_MODE=fulltext run_06_ubiquity_claims_ollama.sbatch

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if [ -n "${SLURM_SUBMIT_DIR:-}" ] && [ -d "${SLURM_SUBMIT_DIR}/scripts/04_analyses" ]; then
    PROJECT_ROOT="${SLURM_SUBMIT_DIR}"
fi

cd "$PROJECT_ROOT"
mkdir -p results/logs

# Load Ollama module from Monsoon HPC
echo "Loading Ollama module..."
module purge
module load ollama
echo "Ollama version: $(ollama --version)"

# Defaults can be overridden via sbatch --export
INPUT_DIR="${INPUT_DIR:-/scratch/bmb646/Bea_Nick_papers_endoreview}"
OUTPUT_DIR="${OUTPUT_DIR:-results/ubiquity_claims}"
OLLAMA_HOST="${OLLAMA_HOST:-http://127.0.0.1:11434}"
OLLAMA_MODEL="${OLLAMA_MODEL:-mistral}"
MAX_FILES="${MAX_FILES:-0}"
SOURCE_MODE="${SOURCE_MODE:-paired}"
ABSTRACT_CSV="${ABSTRACT_CSV:-/scratch/bmb646/Abstracts_for_Monsoon.csv}"
ABSTRACT_TEXT_COL="${ABSTRACT_TEXT_COL:-}"
ABSTRACT_ID_COL="${ABSTRACT_ID_COL:-}"
AUTO_START_OLLAMA="${AUTO_START_OLLAMA:-1}"
OLLAMA_SERVE_LOG="${OLLAMA_SERVE_LOG:-results/logs/ollama_serve_${SLURM_JOB_ID:-manual}.log}"
OLLAMA_MODELS="${OLLAMA_MODELS:-/scratch/bmb646/ollama_models}"

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
echo "Auto-start Ollama: $AUTO_START_OLLAMA"
echo "Ollama models dir: $OLLAMA_MODELS"

# Verify ollama is available from module
if ! command -v ollama >/dev/null 2>&1; then
    echo "ERROR: ollama command not found after module load."
    echo "Verify: module load ollama was successful."
    exit 1
fi

echo "Ollama binary path: $(command -v ollama)"

check_ollama_up() {
    "$PYTHON_BIN" - <<'PY'
import os
import sys
import urllib.request

host = os.environ.get("OLLAMA_HOST", "http://127.0.0.1:11434").rstrip("/")
url = host + "/api/tags"
try:
        with urllib.request.urlopen(url, timeout=5) as resp:
                ok = resp.status == 200
except Exception:
        ok = False

sys.exit(0 if ok else 1)
PY
}

if ! check_ollama_up; then
    echo "Ollama is not reachable at $OLLAMA_HOST"
    if [ "$AUTO_START_OLLAMA" = "1" ]; then
        echo "Attempting to start Ollama server (module-provided binary)..."
        export OLLAMA_MODELS="$OLLAMA_MODELS"
        nohup ollama serve > "$OLLAMA_SERVE_LOG" 2>&1 &
        echo "Started ollama serve with PID $!"
        echo "Log file: $OLLAMA_SERVE_LOG"
        
        # Wait up to 30 seconds for server to be ready
        for i in {1..30}; do
            if check_ollama_up; then
                echo "Ollama server is ready!"
                break
            fi
            echo "Waiting for Ollama to be ready... ($i/30)"
            sleep 1
        done
        
        if ! check_ollama_up; then
            echo "ERROR: Ollama still unreachable after 30 seconds."
            echo "Check log: $OLLAMA_SERVE_LOG"
            tail -20 "$OLLAMA_SERVE_LOG"
            exit 1
        fi
    else
        echo "ERROR: AUTO_START_OLLAMA=0 and Ollama host is unreachable."
        echo "Ensure Ollama server is running: ollama serve &"
        exit 1
    fi
else
    echo "Ollama server is already running at $OLLAMA_HOST"
fi

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
