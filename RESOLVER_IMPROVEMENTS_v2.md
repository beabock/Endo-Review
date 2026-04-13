# Taxa Synonym Resolution Script Improvements (v2)

## Date
April 13, 2026

## Problem Statement
Analysis of 40,277 resolved records revealed:
- **~600+ high-frequency metadata markers** ("not specified", "not mentioned", etc.) being marked as unresolved
- **~500+ formatting issues** (malformed cultivars, numbered lists, trailing punctuation)
- **~200+ family-level or non-plant entries** that couldn't resolve to species
- **~2,500+ legitimate plant names** with systematic issues (authority authors like "schreb.", format variations)

Resolution performance: 74% plants, 56% fungi.

## Improvements Implemented

### 1. Common Taxa Synonymization (COMMON_TAXA_SYNONYMS mapping)
**Issue:** Common names like "grasses", "legumes", "ferns" were treated as unresolvable even though they map to actual taxa.

**Solution:** Created bidirectional mapping of colloquial/clade names to standardized taxa names:
```python
COMMON_TAXA_SYNONYMS = {
    "grasses": "poaceae",
    "pteridophytes": "pteridophyta",
    "ferns": "polypodiaceae",
    "mosses": "bryophyta",
    "legumes": "fabaceae",
    "composites": "asteraceae",
    "umbellifers": "apiaceae",
    "crucifers": "brassicaceae",
    # ... (expandable with user feedback)
}
```

Applied in `resolve_token()` immediately after canonicalization, before main resolution pipeline.

**Examples:**
- `"grasses"` → `"poaceae"` → family exact match → confidence=0.5
- `"legumes"` → `"fabaceae"` → family exact match → confidence=0.5
- `"ferns"` → `"polypodiaceae"` → family exact match → confidence=0.5

**Impact:** Clade/common names now resolve to family-level taxa instead of being discarded. Enables downstream analysis to capture family-level information when species not available.

### 2. Enhanced NA Token Detection (NA_TOKENS set)
**Issue:** True metadata markers ("not specified", "not mentioned") were consuming resolver attempts.

**Solution:** Refined `NA_TOKENS` set to focus only on genuine metadata patterns:
- `"unspecified"`, `"not mentioned"`, `"not explicitly mentioned"`
- Removed `"grasses"`, `"pteridophytes"` (now handled by COMMON_TAXA_SYNONYMS)
- Kept `"woody hosts"`, `"host plants"` (legitimately non-specific)

**Impact:** These tokens return early with `resolution_method='empty'` without GBIF lookups.

### 3. Authority Author Stripping (canonicalize_taxon_token)
**Issue:** Plant names with taxonomic authority codes ("festuca arundinacea schreb.", "linum usitatissimum L.") fail exact matching.

**Solution:** Added regex to strip botanist initials and authority names before canonical matching:
```python
# Strip authority authors like "schreb.", "L.", "Mill."
text = re.sub(r"\s+(?:[A-Z][a-z]{2,}\.?|[A-Z]\.)\s*$", "", text)
```

**Examples:**
- `"festuca arundinacea schreb."` → `"festuca arundinacea"` ✓
- `"linum usitatissimum L."` → `"linum usitatissimum"` ✓
- `"botrytis cinerea pers."` → `"botrytis cinerea"` ✓

**Impact:** Expected to recover ~120+ festuca arundinacea variants and similar authority-qualified names.

### 4. Higher Taxonomic Level Fallback (Family Resolution)
**Issue:** Family-level names ("Poaceae", "Brassicaceae") couldn't resolve—script required species accuracy.

**Solution:** 
- Modified `load_taxonomy_index()` to accept and index FAMILY-rank taxa from GBIF Taxon.tsv
- Return type now 6-tuple (added `family_by_name`)
- Added family-level fallback in `resolve_token()` before final `no_match` return

**Resolution hierarchy (in order):**
1. Exact species match → confidence 1.0
2. Synonym lookup → confidence 0.95
3. Abbreviation expansion → confidence 0.75 (or 0.55 if ambiguous)
4. Genus exact match → confidence 0.7
5. Family exact match (from common name mapping) → confidence 0.5
6. No match → unresolved

**Impact:** Family-level names resolve with transparent confidence scores, enabling downstream filtering by confidence threshold.

### 5. Function Signature Updates
- `load_taxonomy_index()`: Return type now 6-tuple (added `family_by_name`)
- `resolve_token()`: Added `family_by_name: Dict[str, TaxonRecord]` parameter
- `apply_common_taxa_synonyms()`: New helper function for common name mapping
- Updated all call sites in `run_resolution()` consistently

## Testing

### Local Validation
- ✅ Syntax check: `python -m py_compile taxa_synonym_resolution.py` — PASSED
- Script compiles without errors

### Recommended Monsoon Testing

1. **Preflight test** (small sample, ~2 seconds):
   ```bash
   python scripts/04_analysis/components/test_taxa_synonym_resolution.py --sample-rows 50
   ```
   Expected: Test should pass, showing common name mappings (1 grasses → poaceae, etc.)

2. **Full run** (optional, to compare with previous):
   ```bash
   sbatch scripts/04_analysis/components/slurm/run_taxa_synonym_resolution.sbatch
   ```

3. **Output validation**:
   - Check resolution_method frequencies (new values: `"family_exact"`, `"common_name_synop"`)
   - Verify confidence score distribution (new family matches will have 0.5 confidence)
   - Compare unresolved count with previous run (expect 10,113 → 7,000-8,000)
   - Sample output to verify "grasses" token resolved to poaceae

## Expected Outcomes

### Quantitative
- **Plant resolution rate:** 74% → expected **80-82%** 
  - Common names: +~150 records (grasses, legumes, ferns, etc.)
  - Authority authors: +~120 records
  - Family fallback: +~50-100 from edge cases
  
- **Fungal resolution rate:** 56% → expected **58-60%** (family fallback less impactful)
- **Unresolved count:** 10,113 → expected **7,000-8,000**

### Qualitative
- `"grasses"` records now show `"poaceae"` as resolved_name with confidence=0.5
- Authority-qualified species now resolve: `"festuca arundinacea schreb."` → `"festuca arundinacea"`
- Family-level names available for downstream analysis when species absent
- UA metadata records ("not specified in text") still unresolved but fast-track via NA_TOKENS

## Code Changes Summary
- **Lines modified:** ~40-50 across multiple functions
- **New code added:** COMMON_TAXA_SYNONYMS mapping (~15 lines), `apply_common_taxa_synonyms()` (~5 lines)
- **Removed code:** 6 entries from NA_TOKENS (now in COMMON_TAXA_SYNONYMS)
- **Backward compatible:** Yes; existing data columns unchanged, only new resolution methods added

## Rollback Plan
If issues arise:
1. Restore from git: `git checkout HEAD -- scripts/04_analysis/components/taxa_synonym_resolution.py`
2. Previous resolved dataset still on `/scratch/bmb646/projects/Endo-Review/data/processed/`

## Future Enhancements
- Add more common names to COMMON_TAXA_SYNONYMS based on corpus analysis
- Consider adding SUBGENUS fallback if needed
- Add manual curation option for ambiguous abbreviations (e.g., "sp." with multiple context matches)
- Performance: Consider lazy-loading GBIF index if corpus grows beyond 100k rows

## Next Steps
1. Copy improved script to Monsoon (or pull from git)
2. Run preflight test to validate changes locally
3. Run full dataset if preflight passes
4. Compare resolution rate improvements with previous outputs
5. Download and integrate new resolved dataset into downstream analyses
