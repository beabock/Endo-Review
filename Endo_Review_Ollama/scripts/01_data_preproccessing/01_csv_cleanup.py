import re
import csv
import sys

input_file = "data/Ollama_extraction_all.csv"
output_file = "data/Ollama_python_healed.csv"

# The exact 15 columns we need to lock in
HEADERS = [
    "relevance", "doc_type_ai", "doc_type_pages", "page_count", "doi", 
    "plant_host", "fungal_taxon", "tissue", "presence_absence", 
    "primary_guild", "interaction_notes", "biome", "country", 
    "data_source", "source_file"
]

def heal_and_align():
    print(f"Reading raw data from {input_file}...")
    
    with open(input_file, 'r', encoding='utf-8', errors='replace') as f:
        raw_text = f.read()

    # 1. PRE-PARSER HEALING
    # Stitch newlines where the AI hit 'enter' mid-sentence (newline followed by lowercase letter)
    healed_text = re.sub(r'\n([a-z])', r' \1', raw_text)
    # Annihilate all double quotes and backslashes to prevent CSV parser explosions
    healed_text = healed_text.replace('"', '').replace('\\', '')
    
    lines = healed_text.split('\n')
    data_lines = [line for line in lines if line.strip()][1:] # Drop header and empty lines
    
    clean_rows = []
    
    print("Aligning drifting columns...")
    for line in data_lines:
        parts = [p.strip() for p in line.split(',')]
        
        # Initialize an empty 15-slot row
        row = ["NA"] * 15
        
        # --- LEFT ANCHORS (Cols 0 to 4) ---
        # We know Relevance and Doc Types are at the start. 
        # We look for the DOI to anchor column 4.
        doi_index = -1
        for i, part in enumerate(parts[:8]): # DOI should be in the first few cols
            if part.startswith("10."):
                doi_index = i
                break
                
        if doi_index != -1:
            # Safely map whatever came before the DOI
            for i in range(min(doi_index, 4)):
                row[i] = parts[i]
            row[4] = parts[doi_index] # Lock DOI
            parts = parts[doi_index+1:] # Remaining parts
        else:
            # If no DOI found, just dump the first 5 parts
            for i in range(min(len(parts), 5)):
                row[i] = parts[i]
            parts = parts[5:] if len(parts) > 5 else []

        # --- RIGHT ANCHORS (Cols 13 & 14) ---
        # The last columns are always data_source (e.g. abstract-csv) and source_file (e.g. doi_...)
        if len(parts) >= 2:
            row[14] = parts[-1]
            row[13] = parts[-2]
            parts = parts[:-2]
        elif len(parts) == 1:
            row[14] = parts[-1]
            parts = []

        # --- MIDDLE MESS (Cols 5 to 12) ---
        # We now have the remaining parts that belong in Host, Taxon, Tissue, etc.
        # If there are exactly 8 parts left, perfect! 1-to-1 mapping.
        if len(parts) == 8:
            for i in range(8):
                row[5+i] = parts[i]
        
        # If there are MORE than 8 parts, the AI hallucinated commas.
        # We glue the "extra" parts into the interaction_notes (Col 10)
        elif len(parts) > 8:
            row[5] = parts[0] # Host
            row[6] = parts[1] # Taxon
            row[7] = parts[2] # Tissue
            row[8] = parts[3] # Presence/Absence
            row[9] = parts[4] # Guild
            
            # The "overflow" goes into Interaction Notes
            overflow_count = len(parts) - 8
            notes_end = 5 + overflow_count
            row[10] = "; ".join(parts[5:notes_end+1])
            
            # The last 2 parts go to Biome and Country
            row[11] = parts[-2]
            row[12] = parts[-1]
            
        # If there are FEWER than 8 parts, the AI skipped columns. 
        # Just map them left-to-right until we run out.
        else:
            for i in range(len(parts)):
                row[5+i] = parts[i]

        clean_rows.append(row)

    # Write the beautifully aligned data to a new CSV using Python's native CSV writer
    # This automatically adds quotes around fields ONLY when necessary.
    print(f"Writing {len(clean_rows)} aligned rows to {output_file}...")
    with open(output_file, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(HEADERS)
        writer.writerows(clean_rows)
        
    print("Done! The dataset is structurally perfect.")

if __name__ == "__main__":
    heal_and_align()