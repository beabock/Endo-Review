#!/usr/bin/env python3
"""Shared NA-like text mappings used across pipeline scripts."""

from __future__ import annotations


# Substring phrases used in metadata standardization.
NA_PHRASES = (
    "not specified",
    "not provided",
    "unknown",
    "unkown",
    "n/a",
    "uncertain",
    "vulnerability disclosure",
    "hhs",
    "empty",
    "not applicable",
    "not_provided",
    "not_specified",
    "plant tissues",
    "aerial parts",
    "not mentioned",
    "not stated",
    "not explicitly",
    "unspecified",
    "terrestrial",
    "not-provided",
    "not provided in text",
    "text extract",
    "brief message",
)


# Exact tokens for strict checks in token-based scripts like synonym resolution.
SHARED_NA_TOKENS = frozenset(
    {
        "",
        "na",
        "n/a",
        "none",
        "null",
        "unknown",
        "not applicable",
    }
    | {phrase.strip().lower() for phrase in NA_PHRASES}
)
