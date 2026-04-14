#!/usr/bin/env python3
import csv
import os
import re
import sys

# Add parent directory to path to import utils
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from utils.country_mapping import consolidate_country_data

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

COUNTRY_MAP = {
    'usa': 'united states', 'united states': 'united states',
    'p.r. china': 'china', 'peoples republic of china': 'china',
    'republic of korea': 'korea', 'south korea': 'korea',
    'u.k.': 'united kingdom', 'uk': 'united kingdom'
}

DOC_TYPE_MAP = {
    'abstract': 'abstract',
    'full-text': 'full-text',
    'review': 'review',
    'article': 'full-text',
    'title': 'title'
}

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

    with open(INPUT_FILE, 'r', encoding='utf-8') as f_in, \
         open(OUTPUT_FILE, 'w', encoding='utf-8') as f_out:
        
        reader = csv.reader(f_in)
        headers = next(reader)
        h_idx = {name: i for i, name in enumerate(headers)}
        
        writer = csv.writer(f_out, quoting=csv.QUOTE_ALL)
        writer.writerow(headers)
        
        for row in reader:
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
            
            # Check other columns for country information and consolidate
            row = consolidate_country_data(row, headers)
            
            writer.writerow(row)
            
    print(f"Standardization complete. Saved to: {OUTPUT_FILE}")

if __name__ == "__main__":
    run_standardization()