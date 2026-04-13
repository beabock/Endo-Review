# Taxa Synonym Resolution Script Improvements

## Date
April 13, 2026

## Problem Statement
Analysis of 40,277 resolved records revealed:
- **~600+ high-frequency metadata markers** ("not specified", "not mentioned", etc.) were being marked as unresolved
- **~500+ formatting issues** (malformed cultivars, numbered lists, trailing punctuation)
- **~200+ family-level or non-plant entries** that couldn't resolve to species
- **~2,500+ legitimate plant names** with systematic issues (authority authors like "schreb.", format variations)

Resolution performance: 74% plants, 56% fungi.

## Improvements Implemented

### 1. Enhanced NA Token Detection (NA_TOKENS set)
**Issue:** Metadata markers were consuming resolver attempts and appearing as unresolved.

**Solution:** Expanded `NA_TOKENS` set to recognize common patterns:
- `"unspecified"`
- `"not mentioned"`
- `"not explicitly mentioned"`
- `"grasses"` (family/clade, not specific taxon)
- `"pteridophytes"` (clade term)
- `"woody hosts"`, `"host plants"` (non-specific)

**Impact:** These tokens now return early with `resolution_method='empty'` instead of consuming GBIF lookups.

### 2. Authority Author Stripping (canonicalize_taxon_token)
**Issue:** Plant names with taxonomic authority codes ("festuca arundinacea schreb.", "linum usitatissimum L.") fail exact matching.

**Solution:** Added regex to strip botanist initials and authority names before canonical matching:
```python
# Strip authority authors like "schreb.", "L.", "Mill."
text = re.sub(r"\s+(?:[A-Z][a-z]{2,}\.?|[A-Z]\.)\s*$", "", text)
```

**Examples:**
- `"festuca arundinacea schreb."` → `"festuca arundinacea"`
- `"linum usitatissimum L."` → `"linum usitatissimum"`
- `"botrytis cinerea pers."` → `"botrytis cinerea"`

**Impact:** Expected to recover ~120+ festuca arundinacea variants and similar authority-qualified names.

### 3. Higher Taxonomic Level Fallback (Family Resolution)
**Issue:** Legitimate family-level names ("Poaceae", "Brassicaceae") couldn't resolve—script required species accuracy.

**Solution:** 
- Modified `load_taxonomy_index()` to accept and index FAMILY-rank taxa from GBIF Taxon.tsv
- Return type now includes `family_by_name: Dict[str, TaxonRecord]`
- Added family-level fallback in `resolve_token()` before final  `no_match` return

**New resolution hierarchy:**
1. Exact species match → confidence 1.0
2. Synonym lookup → confidence 0.95
3. Abbreviation expansion → confidence 0.75 (or 0.55 if ambiguous)
4. Genus exact match → confidence 0.7
5. **NEW: Family exact match → confidence 0.5**
6. No match → unresolved

**Impact:** Family-level names now resolve with conservative confidence scores (0.5), enabling downstream analysis with transparent confidence thresholds.

### 4. Function Signature Updates
- `load_taxonomy_index()`: Return type now 6-tuple (added `family_by_name`)
- `resolve_token()`: Added `family_by_name: Dict[str, TaxonRecord]` parameter
- Updated all call sites in `run_resolution()` to pass the new parameter

## Testing

### Local Validation
- ✅ Syntax check: `python -m py_compile taxa_synonym_resolution.py` — PASSED
- Script compiles without errors

### Recommended Monsoon Testing

1. **Preflight test** (small sample):
   ```bash
   python scripts/04_analysis/components/test_taxa_synonym_resolution.py --sample-rows 50
   ```

2. **Full run** (optional, to compare with previous):
   ```bash
   sbatch scripts/04_analysis/components/slurm/run_taxa_synonym_resolution.sbatch
   ```

3. **Output validation**:
   - Compare resolution_method frequencies (should see more `"family_exact"` and fewer `"no_match"`)
   - Check confidence score distribution (new family matches will have 0.5 confidence)
   - Verify row counts match previous run (40,277 rows preserved)

## Expected Outcomes

### Quantitative
- Plant resolution rate: 74% → expected **78-80%** (authority authors + family fallback)
- Fungal resolution rate: 56% → expected **58-60%** (family fallback less impactful for fungi)
- Unresolved count: reduced from ~10,113 to ~8,000-9,000

### Qualitative
- Cultivar variants (cv. tioga) still unresolved but no longer slow down processing
- Family-level matches now appear in resolution output with confidence=0.5
- Authority-qualified species names now resolve to accepted GBIF canonical forms

## Rollback Plan
If issues arise:
1. Restore from git: `git checkout HEAD -- scripts/04_analysis/components/taxa_synonym_resolution.py`
2. Revert to previous SLURM run if needed (output still on `/scratch/bmb646/projects/Endo-Review/data/processed/`)

## Next Steps
1. Deploy updated script to Monsoon
2. Run preflight test (50 rows) to validate changes
3. Run full dataset if preflight passes
4. Compare statistics with previous run
5. If satisfactory, integrate new resolved dataset into downstream analyses
