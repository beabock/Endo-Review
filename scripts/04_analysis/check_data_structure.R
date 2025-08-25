# Check data structure script
setwd("c:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

# Check species data structure
species <- readRDS("models/species.rds")
print("Species data structure:")
print(str(species))
print("First 5 rows:")
print(head(species, 5))

# Check families data structure
families <- readRDS("models/families.rds")
print("\nFamilies data structure:")
print(str(families))
print("First 5 rows:")
print(head(families, 5))

# Check genera data structure
genera <- readRDS("models/genera.rds")
print("\nGenera data structure:")
print(str(genera))
print("First 5 rows:")
print(head(genera, 5))
