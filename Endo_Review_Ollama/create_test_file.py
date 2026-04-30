import csv

# Create a test file with first 1000 rows
with open('data/Ollama_cleaned_synresolved.csv', 'r', encoding='utf-8') as f_in:
    reader = csv.reader(f_in)
    with open('data/Ollama_test_1000.csv', 'w', encoding='utf-8', newline='') as f_out:
        writer = csv.writer(f_out)
        for i, row in enumerate(reader):
            writer.writerow(row)
            if i >= 1000:
                break

print("Created test file: data/Ollama_test_1000.csv")
