#!/usr/bin/env python3
"""Resolve fungal_taxon and plant_host names against GBIF backbone.

This script is designed for non-interactive CLI/HPC execution and preserves the
input row count (one output row per input row).
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import re
import sys
from collections import defaultdict
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple


# GBIF backbone rows can contain fields larger than Python's default CSV limit.
csv.field_size_limit(min(sys.maxsize, 10**9))


NA_TOKENS = {
    "",
    "na",
    "n/a",
    "none",
    "not provided",
    "not specified",
    "unknown",
    "not applicable",
    "null",
    "unspecified",
    "not mentioned",
    "not explicitly mentioned",
    "woody hosts",  # non-specific
    "host plants",  # non-specific
}

# Map common names and clade terms to their standardized taxa
COMMON_TAXA_SYNONYMS = {
    "grasses": "poaceae",
    "pteridophytes": "pteridophyta",
    "ferns": "polypodiaceae",
    "mosses": "bryophyta",
    "legumes": "fabaceae",
    "composites": "asteraceae",
    "umbellifers": "apiaceae",
    "crucifers": "brassicaceae",
    "solanaceae family": "solanaceae",
    "grass": "poaceae",
    "fern": "polypodiaceae",
    "moss": "bryophyta",
    "legume": "fabaceae",
}

FIELD_SPECIFIC_SYNONYMS = {
    "plant_host": COMMON_TAXA_SYNONYMS,
    "fungal_taxon": {
        "epichlo": "epichloe",
        "name fusarium": "fusarium",
        "name aspergillus": "aspergillus",
        "ascomycetes": "ascomycota",
    },
}

FIELD_SPECIFIC_NON_TAXON = {
    "fungal_taxon": {
        "arbuscular mycorrhizal",
        "endophytic fungi",
        "endophytic fungus",
        "endophytes",
        "endophyte",
        "fungal endophytes",
        "ectomycorrhizal fungi",
        "mycorrhizal fungi",
        "vesicular-arbuscular mycorrhizal",
        "dark septate",
        "scientific name",
        "latin name",
        "common name",
        "primary guild",
        "multiple",
        "multiple endophytic",
        "multiple fungal",
        "various",
        "tissue not",
        "not fungi",
    }
}

RANK_CONFIDENCE = {
    "SPECIES": 1.0,
    "SUBSPECIES": 0.95,
    "VARIETY": 0.9,
    "FORM": 0.9,
    "GENUS": 0.7,
    "FAMILY": 0.5,
    "ORDER": 0.45,
    "CLASS": 0.4,
    "PHYLUM": 0.35,
}

FIELD_COLUMNS = ("fungal_taxon", "plant_host")


@dataclass
class ResolveResult:
    raw_token: str
    cleaned_token: str
    resolved_name: str
    taxonomic_status: str
    resolution_method: str
    confidence: float
    is_ambiguous: bool
    ambiguity_count: int
    taxon_rank: str
    accepted_taxon_id: str
    kingdom: str


@dataclass
class TaxonRecord:
    taxon_id: str
    canonical_name: str
    taxon_rank: str
    kingdom: str
    phylum: str
    class_name: str
    order: str
    family: str
    genus: str


def normalize_text(value: str) -> str:
    text = (value or "").strip()
    text = re.sub(r"\s+", " ", text)
    return text


def normalize_key(value: str) -> str:
    text = normalize_text(value).lower()
    return "" if text in NA_TOKENS else text


def canonicalize_taxon_token(token: str) -> str:
    text = normalize_text(token)
    # Remove parenthetical common names and bracketed text.
    text = re.sub(r"\([^)]*\)", "", text)
    text = re.sub(r"\[[^\]]*\]", "", text)
    text = re.sub(r"\s+", " ", text).strip()

    # Strip authority authors (botanist initials/names) like "schreb.", "L.", "Pers.", "Mill."
    # Match patterns like " schreb.", " L.", " Mill." at end of string
    text = re.sub(r"\s+(?:[A-Z][a-z]{2,}\.?|[A-Z]\.)\s*$", "", text)
    
    # Keep only biological name pieces and separators.
    text = re.sub(r"[^A-Za-z\-\. xX]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()

    if not text:
        return ""

    parts = text.split(" ")
    if len(parts) >= 2 and re.match(r"^[A-Za-z]\.??$", parts[0]):
        return f"{parts[0][0]}. {parts[1].lower()}"

    # Map genus sp./spp. to genus-level matching.
    if len(parts) >= 2 and parts[1].lower() in {"sp", "sp.", "spp", "spp."}:
        return parts[0].lower()

    # Handle hybrid notation like "Fragaria x ananassa".
    if len(parts) >= 3 and parts[1].lower() in {"x", "×"}:
        genus = parts[0].lower()
        epithet = parts[2].lower()
        return f"{genus} {epithet}"

    # Use genus + epithet for matching species names.
    if len(parts) >= 2:
        genus = parts[0].lower()
        epithet = parts[1].lower()
        return f"{genus} {epithet}"

    return parts[0].lower()


def apply_field_specific_synonyms(cleaned_token: str, field_name: str) -> str:
    synonyms = FIELD_SPECIFIC_SYNONYMS.get(field_name, {})
    return synonyms.get(cleaned_token, cleaned_token)


def is_field_specific_non_taxon(cleaned_token: str, field_name: str) -> bool:
    return cleaned_token in FIELD_SPECIFIC_NON_TAXON.get(field_name, set())


def split_taxa_cell(cell: str) -> List[str]:
    text = normalize_text(cell)
    if not text:
        return []

    # Common multi-name separators in extracted data.
    pieces = re.split(r"\s*(?:;|\||/|,| and )\s*", text, flags=re.IGNORECASE)
    tokens = [p.strip() for p in pieces if normalize_key(p)]
    return tokens


def build_paper_id(row: Dict[str, str], row_index: int) -> str:
    doi = normalize_key(row.get("doi_clean") or row.get("doi") or "")
    if doi:
        return f"doi:{doi}"

    source_file = normalize_key(row.get("source_file", ""))
    if source_file:
        return f"source:{source_file}"

    return f"row:{row_index}"


def build_interaction_id(row: Dict[str, str], row_index: int) -> str:
    stable_payload = "||".join(
        [
            str(row_index),
            normalize_text(row.get("source_file", "")),
            normalize_text(row.get("doi", "")),
            normalize_text(row.get("fungal_taxon", "")),
            normalize_text(row.get("plant_host", "")),
        ]
    )
    digest = hashlib.sha1(stable_payload.encode("utf-8")).hexdigest()[:16]
    return f"int_{digest}"


def load_taxonomy_index(
    taxon_tsv: str,
) -> Tuple[
    Dict[str, TaxonRecord],
    Dict[str, str],
    Dict[str, List[str]],
    Dict[str, str],
    Dict[str, str],
    Dict[str, TaxonRecord],
]:
    accepted_by_canonical: Dict[str, TaxonRecord] = {}
    accepted_by_id: Dict[str, TaxonRecord] = {}
    synonym_to_accepted_id: Dict[str, str] = {}
    genus_by_letter: Dict[str, List[str]] = defaultdict(list)
    genus_name_to_taxon_id: Dict[str, str] = {}
    family_by_name: Dict[str, TaxonRecord] = {}  # New: family-level look-up

    with open(taxon_tsv, "r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle, delimiter="\t")
        for row in reader:
            kingdom = normalize_text(row.get("kingdom", ""))
            if kingdom not in {"Fungi", "Plantae"}:
                continue

            rank = normalize_text(row.get("taxonRank", "")).upper()
            # Support higher-rank fallback when species-level names are unavailable.
            if rank not in {"SPECIES", "GENUS", "SUBSPECIES", "VARIETY", "FORM", "FAMILY", "ORDER", "CLASS", "PHYLUM"}:
                continue

            canonical = normalize_key(row.get("canonicalName", ""))
            if not canonical:
                continue

            status = normalize_text(row.get("taxonomicStatus", "")).lower()
            taxon_id = normalize_text(row.get("taxonID", ""))
            if not taxon_id:
                continue

            if status == "accepted":
                record = TaxonRecord(
                    taxon_id=taxon_id,
                    canonical_name=canonical,
                    taxon_rank=rank,
                    kingdom=kingdom,
                    phylum=normalize_text(row.get("phylum", "")),
                    class_name=normalize_text(row.get("class", "")),
                    order=normalize_text(row.get("order", "")),
                    family=normalize_text(row.get("family", "")),
                    genus=normalize_text(row.get("genus", "")),
                )
                accepted_by_id[taxon_id] = record
                accepted_by_canonical[canonical] = record

                if rank == "GENUS":
                    genus_name_to_taxon_id[canonical] = taxon_id
                    if canonical:
                        genus_by_letter[canonical[0]].append(canonical)
                elif rank == "FAMILY":
                    family_by_name[canonical] = record

            elif status == "synonym":
                accepted_id = normalize_text(row.get("acceptedNameUsageID", ""))
                if accepted_id:
                    synonym_to_accepted_id[canonical] = accepted_id

    for key in list(genus_by_letter.keys()):
        genus_by_letter[key] = sorted(set(genus_by_letter[key]))

    return (
        accepted_by_canonical,
        synonym_to_accepted_id,
        genus_by_letter,
        genus_name_to_taxon_id,
        accepted_by_id,
        family_by_name,
    )


def detect_abbreviation(cleaned_token: str) -> Optional[Tuple[str, str]]:
    match = re.match(r"^([a-zA-Z])\.\s+([a-z\-]+)$", cleaned_token)
    if not match:
        return None
    return match.group(1).lower(), match.group(2).lower()


def resolve_token(
    raw_token: str,
    field_name: str,
    accepted_by_canonical: Dict[str, TaxonRecord],
    synonym_to_accepted_id: Dict[str, str],
    accepted_by_id: Dict[str, TaxonRecord],
    genus_by_letter: Dict[str, List[str]],
    context_genera: Set[str],
    family_by_name: Dict[str, TaxonRecord],
) -> ResolveResult:
    cleaned = canonicalize_taxon_token(raw_token)
    cleaned = apply_field_specific_synonyms(cleaned, field_name)

    if is_field_specific_non_taxon(cleaned, field_name):
        return ResolveResult(
            raw_token=raw_token,
            cleaned_token=cleaned,
            resolved_name="",
            taxonomic_status="UNRESOLVED",
            resolution_method="descriptor_non_taxon",
            confidence=0.0,
            is_ambiguous=False,
            ambiguity_count=0,
            taxon_rank="",
            accepted_taxon_id="",
            kingdom="",
        )

    if not normalize_key(cleaned):
        return ResolveResult(
            raw_token=raw_token,
            cleaned_token=cleaned,
            resolved_name="",
            taxonomic_status="UNRESOLVED",
            resolution_method="empty",
            confidence=0.0,
            is_ambiguous=False,
            ambiguity_count=0,
            taxon_rank="",
            accepted_taxon_id="",
            kingdom="",
        )

    accepted = accepted_by_canonical.get(cleaned)
    if accepted:
        rank = accepted.taxon_rank.upper()
        confidence = RANK_CONFIDENCE.get(rank, 0.5)
        method = "exact_accepted" if rank in {"SPECIES", "SUBSPECIES", "VARIETY", "FORM"} else f"{rank.lower()}_exact"
        return ResolveResult(
            raw_token=raw_token,
            cleaned_token=cleaned,
            resolved_name=accepted.canonical_name,
            taxonomic_status="ACCEPTED",
            resolution_method=method,
            confidence=confidence,
            is_ambiguous=False,
            ambiguity_count=0,
            taxon_rank=accepted.taxon_rank,
            accepted_taxon_id=accepted.taxon_id,
            kingdom=accepted.kingdom,
        )

    syn_id = synonym_to_accepted_id.get(cleaned)
    if syn_id and syn_id in accepted_by_id:
        accepted = accepted_by_id[syn_id]
        return ResolveResult(
            raw_token=raw_token,
            cleaned_token=cleaned,
            resolved_name=accepted.canonical_name,
            taxonomic_status="SYNONYM",
            resolution_method="synonym_map",
            confidence=0.95,
            is_ambiguous=False,
            ambiguity_count=0,
            taxon_rank=accepted.taxon_rank,
            accepted_taxon_id=accepted.taxon_id,
            kingdom=accepted.kingdom,
        )

    abbr = detect_abbreviation(cleaned)
    if abbr:
        initial, epithet = abbr

        context_candidates = sorted([g for g in context_genera if g.startswith(initial)])
        global_candidates = genus_by_letter.get(initial, [])

        # Context-first deterministic order.
        tested_genera = context_candidates + [g for g in global_candidates if g not in set(context_candidates)]

        matches: List[TaxonRecord] = []
        for genus in tested_genera:
            expanded = f"{genus} {epithet}"
            if expanded in accepted_by_canonical:
                matches.append(accepted_by_canonical[expanded])

        if matches:
            unique = {m.canonical_name: m for m in matches}
            ordered = [unique[k] for k in sorted(unique.keys())]
            chosen = ordered[0]
            is_ambiguous = len(ordered) > 1
            return ResolveResult(
                raw_token=raw_token,
                cleaned_token=cleaned,
                resolved_name=chosen.canonical_name,
                taxonomic_status="ACCEPTED",
                resolution_method="abbreviation_context_first",
                confidence=0.75 if not is_ambiguous else 0.55,
                is_ambiguous=is_ambiguous,
                ambiguity_count=len(ordered),
                taxon_rank=chosen.taxon_rank,
                accepted_taxon_id=chosen.taxon_id,
                kingdom=chosen.kingdom,
            )

    # Genus-level fallback.
    parts = cleaned.split(" ")
    if len(parts) == 1 and cleaned in accepted_by_canonical:
        genus_hit = accepted_by_canonical[cleaned]
        return ResolveResult(
            raw_token=raw_token,
            cleaned_token=cleaned,
            resolved_name=genus_hit.canonical_name,
            taxonomic_status="ACCEPTED",
            resolution_method="genus_exact",
            confidence=0.7,
            is_ambiguous=False,
            ambiguity_count=0,
            taxon_rank=genus_hit.taxon_rank,
            accepted_taxon_id=genus_hit.taxon_id,
            kingdom=genus_hit.kingdom,
        )

    # Family-level fallback: try to match against known families
    family_hit = family_by_name.get(cleaned)
    if family_hit:
        return ResolveResult(
            raw_token=raw_token,
            cleaned_token=cleaned,
            resolved_name=family_hit.canonical_name,
            taxonomic_status="ACCEPTED",
            resolution_method="family_exact",
            confidence=0.5,  # Lower confidence for family-level matches
            is_ambiguous=False,
            ambiguity_count=0,
            taxon_rank=family_hit.taxon_rank,
            accepted_taxon_id=family_hit.taxon_id,
            kingdom=family_hit.kingdom,
        )

    return ResolveResult(
        raw_token=raw_token,
        cleaned_token=cleaned,
        resolved_name="",
        taxonomic_status="UNRESOLVED",
        resolution_method="no_match",
        confidence=0.0,
        is_ambiguous=False,
        ambiguity_count=0,
        taxon_rank="",
        accepted_taxon_id="",
        kingdom="",
    )


def aggregate_results(results: Sequence[ResolveResult]) -> Tuple[str, str, str, str, str]:
    if not results:
        return "", "Unresolved", "none", "0.00", ""

    resolved = sorted(set([r.resolved_name for r in results if r.resolved_name]))
    methods = sorted(set([r.resolution_method for r in results if r.resolution_method]))

    resolved_count = sum(1 for r in results if r.resolved_name)
    if resolved_count == 0:
        status = "Unresolved"
    elif resolved_count < len(results):
        status = "Partially_Resolved"
    else:
        status = "Resolved"

    avg_conf = sum(r.confidence for r in results) / float(len(results))

    accepted_ids = sorted(set([r.accepted_taxon_id for r in results if r.accepted_taxon_id]))

    return (
        "; ".join(resolved),
        status,
        "; ".join(methods),
        f"{avg_conf:.2f}",
        "; ".join(accepted_ids),
    )


def gather_context_genera_for_paper(rows: Sequence[Dict[str, str]]) -> Set[str]:
    genera: Set[str] = set()
    for row in rows:
        for col in FIELD_COLUMNS:
            for token in split_taxa_cell(row.get(col, "")):
                cleaned = canonicalize_taxon_token(token)
                parts = cleaned.split(" ")
                if len(parts) >= 2 and len(parts[0]) > 1 and not parts[0].endswith("."):
                    genera.add(parts[0])
    return genera


def count_processed_rows(output_csv: str) -> int:
    if not os.path.exists(output_csv):
        return 0

    with open(output_csv, "r", encoding="utf-8", newline="") as handle:
        reader = csv.reader(handle)
        # Exclude header.
        return max(sum(1 for _ in reader) - 1, 0)


def read_input_rows(input_csv: str) -> List[Dict[str, str]]:
    with open(input_csv, "r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return list(reader)


def get_input_fieldnames(input_csv: str) -> List[str]:
    with open(input_csv, "r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames or []


def write_checkpoint(path: str, payload: Dict[str, str]) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2)


def run_resolution(args: argparse.Namespace) -> None:
    (
        accepted_by_canonical,
        synonym_to_accepted_id,
        genus_by_letter,
        _genus_name_to_taxon_id,
        accepted_by_id,
        family_by_name,
    ) = load_taxonomy_index(args.taxon_tsv)

    input_fieldnames = get_input_fieldnames(args.input_csv)
    rows = read_input_rows(args.input_csv)
    if args.max_rows is not None and args.max_rows > 0:
        rows = rows[: args.max_rows]

    papers: Dict[str, List[Dict[str, str]]] = defaultdict(list)
    for i, row in enumerate(rows):
        row["_row_index"] = str(i)
        row["paper_id"] = build_paper_id(row, i)
        row["interaction_id"] = build_interaction_id(row, i)
        papers[row["paper_id"]].append(row)

    paper_context: Dict[str, Set[str]] = {
        paper_id: gather_context_genera_for_paper(prows)
        for paper_id, prows in papers.items()
    }

    processed_already = count_processed_rows(args.output_csv) if args.resume else 0
    if processed_already >= len(rows):
        print("Output already complete; nothing to process.")
        return

    os.makedirs(os.path.dirname(args.output_csv), exist_ok=True)
    os.makedirs(os.path.dirname(args.unresolved_csv), exist_ok=True)
    os.makedirs(os.path.dirname(args.checkpoint_json), exist_ok=True)

    passthrough = [f for f in input_fieldnames if f not in {"paper_id", "interaction_id"}]

    output_extra = [
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
    ]

    unresolved_fields = [
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
    ]

    write_mode = "a" if processed_already > 0 else "w"
    unresolved_mode = "a" if (processed_already > 0 and os.path.exists(args.unresolved_csv)) else "w"

    with open(args.output_csv, write_mode, encoding="utf-8", newline="") as out_handle, open(
        args.unresolved_csv, unresolved_mode, encoding="utf-8", newline=""
    ) as unresolved_handle:
        out_writer = csv.DictWriter(out_handle, fieldnames=passthrough + output_extra)
        unresolved_writer = csv.DictWriter(unresolved_handle, fieldnames=unresolved_fields)

        if write_mode == "w":
            out_writer.writeheader()
        if unresolved_mode == "w":
            unresolved_writer.writeheader()

        for i, row in enumerate(rows):
            if i < processed_already:
                continue

            context_genera = paper_context.get(row["paper_id"], set())

            field_results: Dict[str, List[ResolveResult]] = {}
            for field in FIELD_COLUMNS:
                tokens = split_taxa_cell(row.get(field, ""))
                token_results = [
                    resolve_token(
                        raw_token=token,
                        field_name=field,
                        accepted_by_canonical=accepted_by_canonical,
                        synonym_to_accepted_id=synonym_to_accepted_id,
                        accepted_by_id=accepted_by_id,
                        genus_by_letter=genus_by_letter,
                        context_genera=context_genera,
                        family_by_name=family_by_name,
                    )
                    for token in tokens
                ]
                field_results[field] = token_results

                for res in token_results:
                    if not res.resolved_name or res.is_ambiguous:
                        unresolved_writer.writerow(
                            {
                                "row_index": i,
                                "paper_id": row["paper_id"],
                                "interaction_id": row["interaction_id"],
                                "field_name": field,
                                "raw_token": res.raw_token,
                                "cleaned_token": res.cleaned_token,
                                "resolution_method": res.resolution_method,
                                "is_ambiguous": str(res.is_ambiguous),
                                "ambiguity_count": res.ambiguity_count,
                                "confidence": f"{res.confidence:.2f}",
                            }
                        )

            fungal_agg = aggregate_results(field_results["fungal_taxon"])
            plant_agg = aggregate_results(field_results["plant_host"])

            out_row = {k: v for k, v in row.items() if k in passthrough}
            out_row.update(
                {
                    "paper_id": row["paper_id"],
                    "interaction_id": row["interaction_id"],
                    "fungal_taxon_raw": row.get("fungal_taxon", ""),
                    "fungal_taxon_resolved": fungal_agg[0],
                    "fungal_taxon_status": fungal_agg[1],
                    "fungal_taxon_resolution_method": fungal_agg[2],
                    "fungal_taxon_confidence": fungal_agg[3],
                    "fungal_taxon_accepted_ids": fungal_agg[4],
                    "plant_host_raw": row.get("plant_host", ""),
                    "plant_host_resolved": plant_agg[0],
                    "plant_host_status": plant_agg[1],
                    "plant_host_resolution_method": plant_agg[2],
                    "plant_host_confidence": plant_agg[3],
                    "plant_host_accepted_ids": plant_agg[4],
                }
            )
            out_writer.writerow(out_row)

            processed_now = i + 1
            if processed_now % args.checkpoint_interval == 0:
                write_checkpoint(
                    args.checkpoint_json,
                    {
                        "processed_rows": str(processed_now),
                        "total_rows": str(len(rows)),
                        "output_csv": args.output_csv,
                        "unresolved_csv": args.unresolved_csv,
                    },
                )

            if processed_now % args.log_interval == 0:
                print(f"Processed {processed_now}/{len(rows)} rows", flush=True)

    write_checkpoint(
        args.checkpoint_json,
        {
            "processed_rows": str(len(rows)),
            "total_rows": str(len(rows)),
            "output_csv": args.output_csv,
            "unresolved_csv": args.unresolved_csv,
            "status": "complete",
        },
    )
    print(f"Done. Wrote {args.output_csv} and {args.unresolved_csv}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Resolve fungal_taxon and plant_host synonyms against GBIF backbone."
    )
    parser.add_argument(
        "--input-csv",
        default="data/processed/Ollama_cleaned.csv",
        help="Input CSV path",
    )
    parser.add_argument(
        "--taxon-tsv",
        default="data/gbif_backbone/Taxon.tsv",
        help="GBIF Taxon TSV path",
    )
    parser.add_argument(
        "--output-csv",
        default="data/processed/Ollama_cleaned_synresolved.csv",
        help="Output CSV path",
    )
    parser.add_argument(
        "--unresolved-csv",
        default="results/manual_validation/taxa_unresolved_review.csv",
        help="Unresolved/ambiguous token review CSV path",
    )
    parser.add_argument(
        "--checkpoint-json",
        default="results/logs/taxa_synonym_resolution_checkpoint.json",
        help="Checkpoint metadata JSON path",
    )
    parser.add_argument(
        "--checkpoint-interval",
        type=int,
        default=1000,
        help="Write checkpoint metadata every N processed rows",
    )
    parser.add_argument(
        "--log-interval",
        type=int,
        default=1000,
        help="Emit progress every N processed rows",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume from existing output by skipping already-written rows",
    )
    parser.add_argument(
        "--max-rows",
        type=int,
        default=None,
        help="Optional row cap for smoke tests",
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    if not os.path.exists(args.input_csv):
        print(f"ERROR: Input not found: {args.input_csv}", file=sys.stderr)
        return 1
    if not os.path.exists(args.taxon_tsv):
        print(f"ERROR: Taxon TSV not found: {args.taxon_tsv}", file=sys.stderr)
        return 1

    run_resolution(args)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
