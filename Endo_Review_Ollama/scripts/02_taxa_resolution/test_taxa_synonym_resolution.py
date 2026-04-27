#!/usr/bin/env python3
"""Pre-Monsoon smoke test for taxa synonym resolution.

This script runs the resolver on a capped sample and validates:
- input/output row preservation
- required output columns
- unresolved review file creation
- checkpoint file creation

Usage:
    python scripts/02_taxa_resolution/test_taxa_synonym_resolution.py
"""

from __future__ import annotations

import argparse
import csv
import os
import subprocess
import sys
from importlib import util
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
RESOLVER = ROOT / "scripts" / "02_taxa_resolution" / "taxa_synonym_resolution.py"
INPUT_CSV = ROOT / "data" / "Ollama_cleaned.csv"
TAXON_TSV = ROOT / "data" / "Reference_datasets" / "gbif_backbone" / "Taxon.tsv"
TEMP_DIR = ROOT / "results" / "temp" / "taxa_synonym_resolution_test"
TAXON_FIXTURE = TEMP_DIR / "Taxon_fixture.tsv"
OUTPUT_CSV = TEMP_DIR / "Ollama_cleaned_synresolved_test.csv"
UNRESOLVED_CSV = TEMP_DIR / "taxa_unresolved_review_test.csv"
CHECKPOINT_JSON = TEMP_DIR / "taxa_synonym_resolution_checkpoint_test.json"

REQUIRED_OUTPUT_COLUMNS = {
    "paper_id",
    "interaction_id",
    "fungal_taxon_raw",
    "fungal_taxon_resolved",
    "fungal_taxon_status",
    "fungal_taxon_resolution_method",
    "fungal_taxon_confidence",
    "fungal_taxon_accepted_ids",
    "plant_host_raw",
    "plant_host_resolved",
    "plant_host_status",
    "plant_host_resolution_method",
    "plant_host_confidence",
    "plant_host_accepted_ids",
}

REQUIRED_UNRESOLVED_COLUMNS = {
    "row_index",
    "paper_id",
    "interaction_id",
    "field_name",
    "raw_token",
    "cleaned_token",
    "resolution_method",
    "is_ambiguous",
    "ambiguity_count",
    "confidence",
}


def count_csv_rows(path: Path) -> int:
    with path.open("r", encoding="utf-8", newline="") as handle:
        return max(sum(1 for _ in csv.reader(handle)) - 1, 0)


def read_header(path: Path) -> list[str]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.reader(handle)
        return next(reader)


def ensure(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def load_resolver_module():
    spec = util.spec_from_file_location("taxa_synonym_resolution", RESOLVER)
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load resolver module from {RESOLVER}")

    resolver = util.module_from_spec(spec)
    sys.modules[spec.name] = resolver
    spec.loader.exec_module(resolver)
    return resolver


def build_taxon_fixture(sample_rows: int) -> Path:
    resolver = load_resolver_module()
    TEMP_DIR.mkdir(parents=True, exist_ok=True)

    with INPUT_CSV.open("r", encoding="utf-8", newline="") as handle:
        input_reader = csv.DictReader(handle)
        sampled_rows = [row for _, row in zip(range(sample_rows), input_reader)]

    target_names: set[str] = set()
    for row in sampled_rows:
        for field in ("fungal_taxon", "plant_host"):
            for token in resolver.split_taxa_cell(row.get(field, "")):
                cleaned = resolver.canonicalize_taxon_token(token)
                if cleaned:
                    target_names.add(cleaned)

    selected_rows: list[dict[str, str]] = []
    selected_taxon_ids: set[str] = set()
    accepted_ids_to_include: set[str] = set()

    def row_matches(row: dict[str, str]) -> bool:
        canonical = resolver.normalize_key(row.get("canonicalName", ""))
        scientific = resolver.normalize_key(row.get("scientificName", ""))
        genus = resolver.normalize_key(row.get("genus", ""))
        return bool({canonical, scientific, genus} & target_names)

    with TAXON_TSV.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle, delimiter="\t")
        fieldnames = reader.fieldnames or []
        for row in reader:
            if row_matches(row):
                taxon_id = row.get("taxonID", "")
                if taxon_id and taxon_id not in selected_taxon_ids:
                    selected_rows.append(row)
                    selected_taxon_ids.add(taxon_id)
                if "synonym" in resolver.normalize_text(row.get("taxonomicStatus", "")).lower():
                    accepted_id = resolver.normalize_text(row.get("acceptedNameUsageID", ""))
                    if accepted_id:
                        accepted_ids_to_include.add(accepted_id)

    if accepted_ids_to_include:
        with TAXON_TSV.open("r", encoding="utf-8", newline="") as handle:
            reader = csv.DictReader(handle, delimiter="\t")
            for row in reader:
                taxon_id = resolver.normalize_text(row.get("taxonID", ""))
                if taxon_id in accepted_ids_to_include and taxon_id not in selected_taxon_ids:
                    selected_rows.append(row)
                    selected_taxon_ids.add(taxon_id)

    ensure(selected_rows, "Taxon fixture would be empty; inspect target-name matching")

    with TAXON_FIXTURE.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, delimiter="\t")
        writer.writeheader()
        writer.writerows(selected_rows)

    print(f"Built taxon fixture with {len(selected_rows)} rows and {len(target_names)} target names")
    return TAXON_FIXTURE


def run_resolver(sample_rows: int) -> None:
    TEMP_DIR.mkdir(parents=True, exist_ok=True)
    for path in (OUTPUT_CSV, UNRESOLVED_CSV, CHECKPOINT_JSON):
        if path.exists():
            path.unlink()

    fixture = build_taxon_fixture(sample_rows)

    command = [
        sys.executable,
        str(RESOLVER),
        "--input-csv",
        str(INPUT_CSV),
        "--taxon-tsv",
        str(fixture),
        "--output-csv",
        str(OUTPUT_CSV),
        "--unresolved-csv",
        str(UNRESOLVED_CSV),
        "--checkpoint-json",
        str(CHECKPOINT_JSON),
        "--checkpoint-interval",
        "5",
        "--log-interval",
        "5",
        "--max-rows",
        str(sample_rows),
    ]

    print("Running resolver test command:\n", " ".join(command), sep="")
    subprocess.run(command, check=True, cwd=ROOT)


def validate_outputs(sample_rows: int) -> None:
    ensure(OUTPUT_CSV.exists(), f"Missing output CSV: {OUTPUT_CSV}")
    ensure(UNRESOLVED_CSV.exists(), f"Missing unresolved CSV: {UNRESOLVED_CSV}")
    ensure(CHECKPOINT_JSON.exists(), f"Missing checkpoint JSON: {CHECKPOINT_JSON}")

    output_header = set(read_header(OUTPUT_CSV))
    unresolved_header = set(read_header(UNRESOLVED_CSV))

    ensure(
        REQUIRED_OUTPUT_COLUMNS.issubset(output_header),
        f"Output CSV missing required columns: {sorted(REQUIRED_OUTPUT_COLUMNS - output_header)}",
    )
    ensure(
        REQUIRED_UNRESOLVED_COLUMNS.issubset(unresolved_header),
        f"Unresolved CSV missing required columns: {sorted(REQUIRED_UNRESOLVED_COLUMNS - unresolved_header)}",
    )

    output_rows = count_csv_rows(OUTPUT_CSV)
    unresolved_rows = count_csv_rows(UNRESOLVED_CSV)
    ensure(
        output_rows == sample_rows,
        f"Expected {sample_rows} output rows, found {output_rows}",
    )
    ensure(output_rows > 0, "Output CSV is empty")
    ensure(unresolved_rows >= 0, "Unresolved CSV row count failed")

    print(f"Validated {output_rows} output rows")
    print(f"Validated {unresolved_rows} unresolved token rows")
    print("Required columns present")
    print(f"Checkpoint written to {CHECKPOINT_JSON}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Pre-Monsoon taxa synonym resolution test")
    parser.add_argument(
        "--sample-rows",
        type=int,
        default=25,
        help="Number of rows to process during the smoke test",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    if not RESOLVER.exists():
        print(f"Resolver script not found: {RESOLVER}", file=sys.stderr)
        return 1
    if not INPUT_CSV.exists():
        print(f"Input CSV not found: {INPUT_CSV}", file=sys.stderr)
        return 1
    if not TAXON_TSV.exists():
        print(f"Taxon TSV not found: {TAXON_TSV}", file=sys.stderr)
        return 1

    try:
        run_resolver(args.sample_rows)
        validate_outputs(args.sample_rows)
    except subprocess.CalledProcessError as exc:
        print(f"Resolver execution failed with exit code {exc.returncode}", file=sys.stderr)
        return exc.returncode
    except AssertionError as exc:
        print(f"Validation failed: {exc}", file=sys.stderr)
        return 1

    print("Taxa synonym resolution test passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
