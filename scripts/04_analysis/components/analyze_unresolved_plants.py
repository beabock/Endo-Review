#!/usr/bin/env python3
"""
Analyze unresolved plant taxa from the synonym resolution output.
Run this on Monsoon: python scripts/04_analysis/components/analyze_unresolved_plants.py
"""
import csv
import collections
import sys

def main():
    unresolved_review = 'results/manual_validation/taxa_unresolved_review.csv'
    
    try:
        with open(unresolved_review, encoding='utf-8') as f:
            reader = csv.DictReader(f)
            rows = list(reader)
    except FileNotFoundError:
        print(f"Error: {unresolved_review} not found")
        sys.exit(1)
    
    # Filter for unresolved plant tokens (field_name == 'plant_host')
    unresolved_plant_tokens = [
        row.get('raw_token', '')
        for row in rows
        if row.get('field_name') == 'plant_host' and row.get('raw_token')
    ]
    
    # Count frequencies
    counts = collections.Counter(unresolved_plant_tokens)
    
    print("\n" + "="*70)
    print("TOP 50 UNRESOLVED PLANT TOKENS BY FREQUENCY")
    print("="*70)
    print(f"{'Count':>6}  {'Plant Token':<60}")
    print("-"*70)
    
    for name, count in counts.most_common(50):
        print(f"{count:>6}  {name:<60}")
    
    print("-"*70)
    print(f"Total unique unresolved plant tokens: {len(counts)}")
    print(f"Total unresolved plant token occurrences: {len(unresolved_plant_tokens)}")
    print("="*70 + "\n")
    
    # Export sample unresolved plant records for manual review
    sample_file = 'unresolved_plants_review_sample.csv'
    sample_rows = [row for row in rows if row.get('field_name') == 'plant_host'][:500]
    
    if sample_rows:
        with open(sample_file, 'w', encoding='utf-8', newline='') as f:
            fieldnames = ['row_index', 'paper_id', 'interaction_id', 'raw_token', 'cleaned_token', 'resolution_method', 'is_ambiguous', 'ambiguity_count', 'confidence']
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            for row in sample_rows:
                writer.writerow({k: row.get(k, '') for k in fieldnames})
        print(f"✓ Exported {len(sample_rows)} unresolved plant token records to {sample_file}\n")

if __name__ == '__main__':
    main()
