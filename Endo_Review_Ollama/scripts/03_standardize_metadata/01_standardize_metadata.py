#!/usr/bin/env python3
import csv
import os
import re
import sys
import itertools
from functools import lru_cache

# Add parent directory to path to import utils
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from utils.country_mapping import (
    extract_all_countries, extract_tissue_values, extract_guild_values,
    extract_biome_values, ALIAS_TO_COUNTRY
)

INPUT_FILE = 'data/Ollama_cleaned_synresolved_standardized.csv'
OUTPUT_FILE = 'data/Ollama_cleaned_synresolved_standardized_final.csv'

# Aggressive NA detection for extraction noise and technical journal artifacts
NA_PHRASES = [
    'not specified', 'not provided', 'unknown', 'unkown', 'n/a', 
    'uncertain', 'vulnerability disclosure', 'hhs', 'empty',
    'not applicable', 'not_provided', 'not_specified', 'plant tissues',
    'aerial parts', 'not mentioned', 'not stated', 'not explicitly',
    'unspecified', 'terrestrial', 'not-provided', 'not provided in text',
    'text extract', 'brief message'
]

TISSUE_MAP = {
    'inner tissue': 'NA',
    'husk': 'seed',
    'aerial tissue': 'stem',
    'thallus': 'leaf',
    'gametophyte': 'leaf',
    'petiole': 'leaf',
    'healthy tissue': 'NA',
    'grape berries': 'fruit',
    'bulb': 'root',
    'corms': 'root',
    'root': 'root', 'rhizosphere': 'root', 'rhizome': 'root', 'tuber': 'root', 'nodule': 'root',
    'leaf': 'leaf', 'leaves': 'leaf', 'foliar': 'leaf', 'needle': 'leaf', 'foliage': 'leaf', 
    'phyllosphere': 'leaf', 'petiole': 'leaf', 'seaweed': 'leaf',
    'stem': 'stem', 'culm': 'stem', 'shoot': 'stem', 'wood': 'stem', 'bark': 'stem', 
    'twig': 'stem', 'branch': 'stem', 'trunk': 'stem', 'xylem': 'stem',
    'seed': 'seed', 'grain': 'seed', 'kernel': 'seed',
    'fruit': 'fruit', 'berry': 'fruit', 'flower': 'reproductive', 'reproductive': 'reproductive'
}

GUILD_MAP = {
    'plant growth promoting': 'pgpr',
    'endophytic': 'endophyte',
    'biological control': 'biocontrol',
    'pgpr': 'pgpr',
    'biological control agent': 'biocontrol',
    'nematophagous': 'biocontrol',
    'endophyte': 'endophyte', 'endophytic': 'endophyte',
    'pathogen': 'pathogen', 'pathogenic': 'pathogen', 'phytopathogen': 'pathogen',
    'mycorrhiza': 'mycorrhiza', 'mycorrhizal': 'mycorrhiza', 'ectomycorrhiza': 'mycorrhiza',
    'biocontrol': 'biocontrol', 'antagonist': 'biocontrol', 'antifungal': 'biocontrol',
    'pgpr': 'pgpr', 'growth-promoting': 'pgpr', 'growth promoting': 'pgpr',
    'saprotroph': 'saprotroph', 'decomposer': 'saprotroph', 'saprobic': 'saprotroph',
    'mutualist': 'mutualist', 'symbiotic': 'symbiotic', 'symbiont': 'symbiotic'
}

BIOME_MAP = {
    'xishuangbanna': 'forest',
    'citrus': 'agriculture',
    'botanical garden': 'agriculture',
    'nursery': 'agriculture',
    'ghats': 'mountain',
    'volcanic belt': 'mountain',
    'queensland': 'NA',
    'western ghats': 'mountain',
    'northeast-iran': 'desert',
    'tropics': 'tropical forest',
    'agricultur': 'agriculture', 'field': 'agriculture', 'orchard': 'agriculture', 
    'vineyard': 'agriculture', 'viticulture': 'agriculture', 'farmland': 'agriculture',
    'agroecosystem': 'agriculture', 'nursery': 'agriculture',
    'forest': 'forest', 'woodland': 'forest', 'rainforest': 'forest',
    'tropical': 'tropical forest', 'mangrove': 'mangrove',
    'marine': 'marine', 'ocean': 'marine', 'aquatic': 'marine', 'estuarine': 'marine',
    'grassland': 'grassland', 'prairie': 'grassland', 'pasture': 'grassland', 
    'mountain': 'mountain', 'alpine': 'mountain', 'desert': 'desert', 'arid': 'desert',
    'tundra': 'tundra', 'urban': 'urban', 'wetland': 'wetland', 'salt marsh': 'wetland',
    'savanna': 'savanna', 'cerrado': 'savanna', 'antarctic': 'antarctic', 'antarctica': 'antarctic'
}

# Use comprehensive country mapping from shared utility (replaces old minimal COUNTRY_MAP)
COUNTRY_MAP = ALIAS_TO_COUNTRY

DOC_TYPE_MAP = {
    'abstract': 'abstract',
    'full-text': 'full-text',
    'review': 'review',
    'article': 'full-text',
    'title': 'title'
}

GBIF_TAXON_TSV = os.path.join('data', 'Reference_datasets', 'gbif_backbone', 'Taxon.tsv')

DEFAULT_FUNGAL_PHYLUM_TERMS = {
    'ascomycota', 'basidiomycota', 'chytridiomycota', 'glomeromycota',
    'mucoromycota', 'mortierellomycota', 'blastocladiomycota',
    'neocallimastigomycota', 'microsporidia', 'kickxellomycota',
    'aphelidiomycota', 'zoopagomycota', 'entomophthoromycota',
    'olpidiomycota', 'rozellomycota'
}

DEFAULT_PLANT_PHYLUM_TERMS = {
    'tracheophyta', 'marchantiophyta', 'bryophyta', 'anthocerotophyta',
    'magnoliophyta', 'pinophyta', 'pteridophyta', 'lycophyta',
    'lycopodiophyta', 'chlorophyta', 'charophyta'
}

DEFAULT_FUNGAL_CLASS_TERMS = {
    'agaricomycetes', 'ascomycetes', 'basidiomycetes', 'dothideomycetes',
    'eurotiomycetes', 'exobasidiomycetes', 'glomeromycetes',
    'leotiomycetes', 'mucoromycetes', 'microbotryomycetes',
    'pezizomycetes', 'sordariomycetes', 'ustilaginomycetes'
}

DEFAULT_PLANT_CLASS_TERMS = {
    'magnoliopsida', 'liliopsida', 'bryopsida', 'marchantiopsida',
    'anthocerotopsida', 'lycopodiopsida', 'polypodiopsida', 'pinopsida',
    'ginkgoopsida', 'equisetopsida'
}


@lru_cache(maxsize=1)
def load_gbif_taxon_whitelists(taxon_tsv=GBIF_TAXON_TSV):
    """Load accepted GBIF fungal and plant phylum/class terms from Taxon.tsv."""
    fungal_phyla = set(DEFAULT_FUNGAL_PHYLUM_TERMS)
    plant_phyla = set(DEFAULT_PLANT_PHYLUM_TERMS)
    fungal_classes = set(DEFAULT_FUNGAL_CLASS_TERMS)
    plant_classes = set(DEFAULT_PLANT_CLASS_TERMS)

    if not os.path.exists(taxon_tsv):
        return {
            'fungal_phyla': frozenset(fungal_phyla),
            'plant_phyla': frozenset(plant_phyla),
            'fungal_classes': frozenset(fungal_classes),
            'plant_classes': frozenset(plant_classes),
        }

    try:
        with open(taxon_tsv, 'r', encoding='utf-8', newline='') as handle:
            reader = csv.DictReader(handle, delimiter='\t')
            for row in reader:
                if standardize_value(row.get('taxonomicStatus', '')).lower() != 'accepted':
                    continue

                kingdom = standardize_value(row.get('kingdom', ''))
                if kingdom not in {'Fungi', 'Plantae'}:
                    continue

                phylum = normalize_taxon_label(row.get('phylum', ''))
                class_name = normalize_taxon_label(row.get('class', ''))

                if kingdom == 'Fungi':
                    if phylum:
                        fungal_phyla.add(phylum)
                    if class_name:
                        fungal_classes.add(class_name)
                else:
                    if phylum:
                        plant_phyla.add(phylum)
                    if class_name:
                        plant_classes.add(class_name)
    except (OSError, csv.Error):
        return {
            'fungal_phyla': frozenset(fungal_phyla),
            'plant_phyla': frozenset(plant_phyla),
            'fungal_classes': frozenset(fungal_classes),
            'plant_classes': frozenset(plant_classes),
        }

    return {
        'fungal_phyla': frozenset(fungal_phyla),
        'plant_phyla': frozenset(plant_phyla),
        'fungal_classes': frozenset(fungal_classes),
        'plant_classes': frozenset(plant_classes),
    }


def normalize_taxon_label(val):
    if not val:
        return ''

    text = clean_parentheticals(str(val)).lower().strip()
    text = re.sub(r'\s+', ' ', text)
    return text.strip("()_ .,;[]{}")


def detect_taxon_kingdom(val, field_name=''):
    """Return the most likely kingdom for a taxon label, if it can be inferred."""
    text = normalize_taxon_label(val)
    if not text or text in {'na', 'none', 'null', 'unknown', 'unresolved'}:
        return None

    field_lower = (field_name or '').lower()
    taxon_whitelists = load_gbif_taxon_whitelists()

    if field_lower.endswith('_phylum'):
        if text in taxon_whitelists['fungal_phyla'] or text.endswith('mycota'):
            return 'Fungi'
        if text in taxon_whitelists['plant_phyla'] or text.endswith('phyta'):
            return 'Plantae'

    if field_lower.endswith('_class'):
        if text in taxon_whitelists['fungal_classes'] or text.endswith('mycetes') or text.endswith('omycetes'):
            return 'Fungi'
        if text in taxon_whitelists['plant_classes'] or text.endswith('opsida'):
            return 'Plantae'

    if field_lower.endswith('_kingdom'):
        if text in {'fungi', 'fungal'}:
            return 'Fungi'
        if text in {'plantae', 'plant'}:
            return 'Plantae'

    return None


def recover_taxonomy_misplacements(row, headers):
    """Create a corrected copy when fungal/plant taxon values are clearly swapped.

    The original row is preserved. A second row is emitted only when a value
    can be confidently moved from the fungal side to the plant side or vice
    versa based on phylum/class kingdom cues.
    """
    col_indices = {name: idx for idx, name in enumerate(headers)}
    corrected_row = row[:]
    changed = False

    field_pairs = [
        ('fungal_taxon_kingdom', 'plant_host_kingdom'),
        ('fungal_taxon_phylum', 'plant_host_phylum'),
        ('fungal_taxon_class', 'plant_host_class'),
    ]

    for fungal_col, plant_col in field_pairs:
        if fungal_col not in col_indices or plant_col not in col_indices:
            continue

        fungal_idx = col_indices[fungal_col]
        plant_idx = col_indices[plant_col]
        fungal_val = row[fungal_idx] if fungal_idx < len(row) else ''
        plant_val = row[plant_idx] if plant_idx < len(row) else ''

        fungal_kingdom = detect_taxon_kingdom(fungal_val, fungal_col)
        plant_kingdom = detect_taxon_kingdom(plant_val, plant_col)

        # Strong signal: values clearly belong on opposite sides.
        if fungal_kingdom == 'Plantae' and plant_kingdom == 'Fungi':
            corrected_row[fungal_idx] = plant_val
            corrected_row[plant_idx] = fungal_val
            changed = True
            continue

        # We only move one-sided values when the opposite side is blank/unknown.
        if fungal_kingdom == 'Plantae' and plant_kingdom is None:
            corrected_row[fungal_idx] = 'NA'
            corrected_row[plant_idx] = fungal_val
            changed = True
            continue

        if plant_kingdom == 'Fungi' and fungal_kingdom is None:
            corrected_row[plant_idx] = 'NA'
            corrected_row[fungal_idx] = plant_val
            changed = True

    return [row, corrected_row] if changed else [row]


def expand_taxonomy_recovery_rows(rows, headers):
    """Expand rows to preserve original records and add corrected taxonomy copies."""
    expanded_rows = []

    for row in rows:
        expanded_rows.extend(recover_taxonomy_misplacements(row, headers))

    return expanded_rows

def expand_multi_value_rows(rows, headers):
    """
    Expand rows when multiple unique values detected across columns.
    Handles: country, tissue, primary_guild, biome.
    
    Creates separate rows for each unique value found (handles displaced values from LLM).
    Example: Paper with Canada in country column + Mexico in plant_host -> 2 rows
             Paper with leaf in tissue + stem in interaction_notes -> 2 rows
    
    Args:
        rows: list of row lists
        headers: list of column headers
    
    Returns:
        Expanded list of rows with duplicates for multi-value papers
    """
    expanded_rows = []
    col_indices = {name: idx for idx, name in enumerate(headers)}
    
    # Define extraction functions for each target column
    target_cols = {
        'country': (col_indices.get('country'), extract_all_countries),
        'tissue': (col_indices.get('tissue'), extract_tissue_values),
        'primary_guild': (col_indices.get('primary_guild'), extract_guild_values),
        'biome': (col_indices.get('biome'), extract_biome_values),
    }
    
    for row in rows:
        # Extract all possible values for each target column from all source columns
        all_extractions = {}
        expansion_needed = False
        
        for col_name, (col_idx, extract_func) in target_cols.items():
            if col_idx is None:
                continue  # Column doesn't exist in this dataset
                
            values = extract_func(row, headers)
            if values:
                # Get unique values (first element of tuple is the value)
                unique_vals = list(dict.fromkeys([val for val, _ in values]))
                all_extractions[col_idx] = unique_vals
                if len(unique_vals) > 1:
                    expansion_needed = True
        
        if not expansion_needed:
            # No expansion needed, just apply any extracted values
            for col_idx, values in all_extractions.items():
                if values:
                    row[col_idx] = values[0]
            expanded_rows.append(row)
        else:
            # Need to expand: build all combinations of multi-valued fields
            multi_cols = {idx: vals for idx, vals in all_extractions.items() if len(vals) > 1}
            single_vals = {idx: vals[0] for idx, vals in all_extractions.items() if len(vals) == 1}
            
            if multi_cols:
                # Generate all combinations using itertools.product
                col_idxs = list(multi_cols.keys())
                col_val_lists = [multi_cols[idx] for idx in col_idxs]
                
                for value_combo in itertools.product(*col_val_lists):
                    row_copy = row[:]
                    for col_idx, val in zip(col_idxs, value_combo):
                        row_copy[col_idx] = val
                    # Apply single extracted values
                    for col_idx, val in single_vals.items():
                        row_copy[col_idx] = val
                    expanded_rows.append(row_copy)
            else:
                expanded_rows.append(row)
    
    return expanded_rows


def clean_parentheticals(val):
    """Removes parentheticals like 'endophytic (diplodia allocellula)' to leave just 'endophytic'."""
    return re.sub(r'\(.*?\)', '', val).strip()


def standardize_value(val, mapping=None):
    if not val:
        return 'NA'
    
    # 1. Strip technical parentheticals first
    val = clean_parentheticals(val)
    
    clean_val = val.lower().strip().strip('()_ ')
    
    # 2. Broad substring check for NA phrases
    if any(phrase in clean_val for phrase in NA_PHRASES):
        return 'NA'
    
    if mapping:
        # 3. Keyword check (e.g., "twigs" matches "twig" in TISSUE_MAP)
        for key, standardized in mapping.items():
            if key in clean_val:
                return standardized
    
    # 4. Final safety for short strings
    if len(clean_val) < 2 or clean_val in ['na', 'none']:
        return 'NA'
        
    return clean_val

def run_standardization():
    if not os.path.exists(INPUT_FILE):
        print(f"Error: {INPUT_FILE} not found.")
        return

    print("Starting standardization...")
    with open(INPUT_FILE, 'r', encoding='utf-8') as f_in:
        reader = csv.reader(f_in)
        headers = next(reader)
        h_idx = {name: i for i, name in enumerate(headers)}
        
        # Load all rows first (needed for multi-country expansion)
        all_rows = []
        print(f"Processing {INPUT_FILE}...")
        
        for row_num, row in enumerate(reader):
            if row_num % 1000 == 0:
                print(f"  Row {row_num}...")
                
            if 'tissue' in h_idx:
                row[h_idx['tissue']] = standardize_value(row[h_idx['tissue']], TISSUE_MAP)
            if 'primary_guild' in h_idx:
                row[h_idx['primary_guild']] = standardize_value(row[h_idx['primary_guild']], GUILD_MAP)
            if 'biome' in h_idx:
                row[h_idx['biome']] = standardize_value(row[h_idx['biome']], BIOME_MAP)
            if 'country' in h_idx:
                row[h_idx['country']] = standardize_value(row[h_idx['country']], COUNTRY_MAP)
            if 'doc_type_ai' in h_idx:
                row[h_idx['doc_type_ai']] = standardize_value(row[h_idx['doc_type_ai']], DOC_TYPE_MAP)

            # Taxonomy cleaning: Force literal "EMPTY" or artifacts to "NA"
            tax_cols = ['fungal_taxon_phylum', 'fungal_taxon_class', 'plant_host_phylum', 'plant_host_class']
            for col in tax_cols:
                if col in h_idx:
                    row[h_idx[col]] = standardize_value(row[h_idx[col]])
            
            all_rows.append(row)
        
        # First recover clear fungal/plant taxonomy swaps, then expand multi-value fields.
        taxonomy_recovered_rows = expand_taxonomy_recovery_rows(all_rows, headers)
        taxonomy_rows_added = len(taxonomy_recovered_rows) - len(all_rows)

        expanded_rows = expand_multi_value_rows(taxonomy_recovered_rows, headers)
        
        # Write all rows (including expanded ones)
        with open(OUTPUT_FILE, 'w', encoding='utf-8') as f_out:
            writer = csv.writer(f_out, quoting=csv.QUOTE_ALL)
            writer.writerow(headers)
            writer.writerows(expanded_rows)
    
    original_count = len(all_rows)
    taxonomy_count = len(taxonomy_recovered_rows)
    expanded_count = len(expanded_rows)
    new_rows_added = expanded_count - original_count
    
    print(f"Standardization complete:")
    print(f"  Original rows: {original_count}")
    print(f"  Rows after taxonomy recovery: {taxonomy_count} (+{taxonomy_rows_added})")
    print(f"  Expanded rows: {expanded_count}")
    print(f"  New rows added (multi-value expansion): {new_rows_added}")
    print(f"  Saved to: {OUTPUT_FILE}")
    print(f"  Values searched: country, tissue, primary_guild, biome, taxonomy recovery")

if __name__ == "__main__":
    run_standardization()