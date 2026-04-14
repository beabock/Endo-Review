import csv
import sys
sys.path.insert(0, 'scripts')
from utils.country_mapping import extract_all_countries

with open('data/Ollama_cleaned_synresolved_standardized.csv', 'r', encoding='utf-8') as f:
    reader = csv.reader(f)
    headers = next(reader)
    
    # Check country column index
    country_idx = headers.index('country')
    plant_host_idx = headers.index('plant_host')
    
    # Get first 10 rows and show country and plant_host values
    for i, row in enumerate(reader):
        if i >= 10:
            break
        country_val = row[country_idx] if country_idx < len(row) else "N/A"
        plant_val = row[plant_host_idx] if plant_host_idx < len(row) else "N/A"
        print(f"Row {i}:")
        print(f"  country: {country_val[:50] if country_val else 'EMPTY'}")
        print(f"  plant_host: {plant_val[:50] if plant_val else 'EMPTY'}")
        
        # Try extraction
        countries = extract_all_countries(row, headers)
        if countries:
            print(f"  EXTRACTION FOUND: {countries}")
print("\nDone")
