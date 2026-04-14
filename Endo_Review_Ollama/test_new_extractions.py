import csv
import sys
sys.path.insert(0, 'scripts')
from utils.country_mapping import extract_tissue_values, extract_guild_values, extract_biome_values

with open('data/Ollama_cleaned_synresolved_standardized.csv', 'r', encoding='utf-8') as f:
    reader = csv.reader(f)
    headers = next(reader)
    
    # Get first 20 rows and test extractions
    for i, row in enumerate(reader):
        if i >= 20:
            break
        
        tissues = extract_tissue_values(row, headers)
        guilds = extract_guild_values(row, headers)
        biomes = extract_biome_values(row, headers)
        
        if tissues or guilds or biomes:
            print(f"Row {i}:")
            if tissues:
                print(f"  Tissues found: {tissues}")
            if guilds:
                print(f"  Guilds found: {guilds}")
            if biomes:
                print(f"  Biomes found: {biomes}")

print("\nExtractions test complete")
