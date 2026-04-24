#!/usr/bin/env python3
"""Centralized taxa token mapping for resolution scripts.

Keep field-specific normalization rules here so multiple pipeline scripts can
reuse the same curated mappings.
"""

from __future__ import annotations

from typing import Dict

from utils.na_mapping import SHARED_NA_TOKENS


# Additional NA-like tokens specific to extracted taxa fields.
TAXA_NA_TOKENS = {
    *SHARED_NA_TOKENS,
    "not explicitly named",
}


# Field-aware manual synonym overrides applied before GBIF lookups.
FIELD_SPECIFIC_SYNONYMS: Dict[str, Dict[str, str]] = {
    "fungal_taxon": {
        "dark septate": "ascomycota",
        "acremonium coenophialum": "epichloe coenophiala",
        "neotyphodium coenophialum": "epichloe coenophiala",
    },
    "plant_host": {
        "tall fescue": "lolium arundinaceum",
        "perennial ryegrass": "lolium perenne",
        "broad bean": "vicia faba",
        "wheat": "triticum aestivum",
        "maize": "zea mays",
        "grasses": "poaceae",
    },
}


FIELD_SPECIFIC_NON_TAXON = {
    "fungal_taxon": {
        "arbuscular mycorrhizal",
        "arbuscular mycorrhiza",
        "common name",
        "endophytic",
        "endophyte",
        "endophytes",
        "endophytic fungi",
        "endophytic fungus",
        "fungal endophytes",
        "fungal endophyte",
        "latin name",
        "primary guild",
        "scientific name",
        "perennial ryegrass",
        "tissue not",
    },
    "plant_host": {
        "arbuscular mycorrhizal",
        "arbuscular mycorrhiza",
        "dark septate",
        "endophytic",
        "endophyte",
        "endophytes",
        "common name",
        "latin name",
        "primary guild",
        "scientific name",
        "tissue not",
    },
}


FIELD_SPECIFIC_NON_TARGET_TAXA = {
    "fungal_taxon": {
        "streptomyces",
        "pseudomonas",
        "bacillus",
    },
    "plant_host": {
        "streptomyces",
        "pseudomonas",
        "bacillus",
    },
}


def apply_taxa_synonym(cleaned_token: str, field_name: str) -> str:
    """Apply curated field-specific synonym mapping to a normalized token."""
    return FIELD_SPECIFIC_SYNONYMS.get(field_name, {}).get(cleaned_token, cleaned_token)


def is_field_specific_non_taxon(cleaned_token: str, field_name: str) -> bool:
    """Return True when a token is known to be non-taxonomic for the field."""
    return cleaned_token in FIELD_SPECIFIC_NON_TAXON.get(field_name, set())


def is_field_specific_non_target_taxon(cleaned_token: str, field_name: str) -> bool:
    """Return True when a token is taxonomic but outside target kingdoms."""
    return cleaned_token in FIELD_SPECIFIC_NON_TARGET_TAXA.get(field_name, set())
