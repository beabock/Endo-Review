import csv
from collections import Counter

# Set the path to your downloaded file
FILE_PATH = 'data/Ollama_cleaned_synresolved_filtered.csv'

# Define the columns we want to summarize
CATEGORICAL_COLUMNS = [
    'fungal_taxon_phylum',
    'fungal_taxon_class',
    'plant_host_phylum',
    'plant_host_class',
    'tissue',
    'primary_guild',
    'relevance',
    'doc_type_ai',
    'fungal_taxon_status',
    'plant_host_status'
]

def summarize_data():
    try:
        with open(FILE_PATH, mode='r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            
            # Initialize counters for each column
            stats = {col: Counter() for col in CATEGORICAL_COLUMNS}
            total_rows = 0

            for row in reader:
                total_rows += 1
                for col in CATEGORICAL_COLUMNS:
                    val = row.get(col, 'MISSING').strip()
                    if not val:
                        val = 'EMPTY'
                    stats[col][val] += 1

        print("="*60)
        print(f"DATABASE SUMMARY: {total_rows:,} total interactions")
        print("="*60)

        for col in CATEGORICAL_COLUMNS:
            print(f"\nTOP VALUES FOR: {col}")
            print("-" * 30)
            
            # Get top 10 most common values
            top_values = stats[col].most_common(10)
            
            for val, count in top_values:
                percentage = (count / total_rows) * 100
                print(f"{count:7,d} ({percentage:5.1f}%) | {val}")
                
    except FileNotFoundError:
        print(f"Error: Could not find '{FILE_PATH}'. Make sure the script is in the same folder as your CSV.")
    except Exception as e:
        print(f"An error occurred: {e}")

# Columns to audit for geographic and environmental trends
ENV_COLUMNS = [
    'country',
    'biome',
    'data_source'
]

def summarize_environment():
    try:
        with open(FILE_PATH, mode='r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            stats = {col: Counter() for col in ENV_COLUMNS}
            total_rows = 0

            for row in reader:
                total_rows += 1
                for col in ENV_COLUMNS:
                    val = row.get(col, 'NA').strip()
                    if not val or val.lower() in ['na', 'none', 'null', 'unknown']:
                        val = 'NA'
                    stats[col][val] += 1

        print("="*60)
        print(f"ENVIRONMENTAL AUDIT: {total_rows:,} total interactions")
        print("="*60)

        for col in ENV_COLUMNS:
            print(f"\nTOP VALUES FOR: {col}")
            print("-" * 30)
            # Showing top 10 to catch variations in naming
            top_values = stats[col].most_common(10)
            for val, count in top_values:
                percentage = (count / total_rows) * 100
                print(f"{count:7,d} ({percentage:5.1f}%) | {val}")
                
    except FileNotFoundError:
        print(f"Error: Could not find '{FILE_PATH}'.")


if __name__ == "__main__":
    summarize_data()
    summarize_environment()

