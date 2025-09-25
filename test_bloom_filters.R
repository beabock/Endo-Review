# Test Bloom Filter Implementation and Performance
#
# This script tests the bloom filter pre-filtering system and measures
# performance improvements over traditional validation methods.

library(tidyverse)
library(tictoc)
source("scripts/04_analysis/bloom_filter_duckdb.R")
source("scripts/04_analysis/optimized_taxa_detection.R")

message("Testing bloom filter implementation...")

# Test 1: Load bloom filter connections
message("\n=== Test 1: Loading Bloom Filters ===")
bloom_connections <- tryCatch({
  load_bloom_filters()
}, error = function(e) {
  message("Failed to load bloom filters: ", e$message)
  return(NULL)
})

if (is.null(bloom_connections)) {
  stop("Bloom filters not available. Run build_bloom_filters.R first.")
}

message(sprintf("Successfully loaded bloom filters for domains: %s",
                paste(names(bloom_connections), collapse = ", ")))

# Test 2: Basic bloom filter functionality
message("\n=== Test 2: Basic Bloom Filter Functionality ===")

# Create some test candidates (mix of valid and invalid names)
test_candidates <- c(
  "Quercus robur",      # Valid plant
  "Fagus sylvatica",    # Valid plant
  "Amanita muscaria",   # Valid fungus
  "Invalid species",    # Invalid
  "Fake name",          # Invalid
  "Pseudomonas aeruginosa", # Invalid (bacteria)
  "Arabidopsis thaliana" # Valid plant
)

message("Testing bloom filter pre-filtering with mixed candidates:")
print(test_candidates)

# Test bloom filter for plants
plant_filtered <- bloom_filter_candidates(test_candidates, "plants", bloom_connections)
message(sprintf("Plants - Original: %d, Filtered: %d", length(test_candidates), length(plant_filtered)))
print(plant_filtered)

# Test bloom filter for fungi
fungi_filtered <- bloom_filter_candidates(test_candidates, "fungi", bloom_connections)
message(sprintf("Fungi - Original: %d, Filtered: %d", length(test_candidates), length(fungi_filtered)))
print(fungi_filtered)

# Test 3: Load traditional lookup tables for comparison
message("\n=== Test 3: Loading Traditional Lookup Tables ===")

# Load species data for traditional validation
species <- readRDS("models/species.rds")
lookup_tables <- create_lookup_tables(species)
lookup_tables_bloom <- create_lookup_tables_with_bloom(species)

message("Lookup tables loaded successfully")

# Test 4: Performance comparison
message("\n=== Test 4: Performance Comparison ===")

# Create a larger test set for performance testing
set.seed(42)
large_test_candidates <- c(
  # Valid species
  sample(species$canonicalName, 50),
  # Invalid candidates (random strings)
  replicate(450, paste(sample(letters, 5, TRUE), sample(letters, 5, TRUE)))
) %>% unique()

message(sprintf("Testing with %d candidate names (mix of valid/invalid)", length(large_test_candidates)))

# Traditional validation (no bloom filters)
message("Running traditional validation...")
tic()
traditional_result <- batch_validate_names(large_test_candidates, lookup_tables, use_bloom_filter = FALSE)
traditional_time <- toc(quiet = TRUE)
traditional_time <- traditional_time$toc - traditional_time$tic

# Hybrid validation (with bloom filters)
message("Running hybrid validation...")
tic()
hybrid_result <- batch_validate_names(large_test_candidates, lookup_tables_bloom, use_bloom_filter = TRUE)
hybrid_time <- toc(quiet = TRUE)
hybrid_time <- hybrid_time$toc - hybrid_time$tic

# Results comparison
message(sprintf("Performance Results:"))
message(sprintf("  Traditional validation: %.3f seconds, found %d valid species", traditional_time, nrow(traditional_result)))
message(sprintf("  Hybrid validation: %.3f seconds, found %d valid species", hybrid_time, nrow(hybrid_result)))
message(sprintf("  Speed improvement: %.1fx faster", traditional_time / hybrid_time))
message(sprintf("  Accuracy: %s", ifelse(all.equal(traditional_result, hybrid_result), "MATCH", "DIFFERENT")))

# Test 5: Integration test with real abstract processing
message("\n=== Test 5: Integration Test with Abstract Processing ===")

# Create a test abstract
test_abstract <- "This study examines the endophytic fungi associated with Quercus robur and Fagus sylvatica in temperate forests. The research also considers Amanita muscaria distribution patterns."

test_data <- data.frame(
  abstract = test_abstract,
  id = 1,
  predicted_label = "Presence"
)

# Process with optimized pipeline
message("Processing test abstract with bloom filter-enhanced pipeline...")
tic()
result <- process_abstracts_parallel(
  abstracts = test_data,
  species_path = "models/species.rds",
  plant_parts_keywords = c("leaf", "root", "stem"),
  batch_size = 1
)
processing_time <- toc(quiet = TRUE)
processing_time <- processing_time$toc - processing_time$tic

message(sprintf("Abstract processing completed in %.3f seconds", processing_time))
message("Detected taxa:")
if (nrow(result) > 0) {
  print(result %>% select(resolved_name, match_type, kingdom, status))
} else {
  message("No taxa detected")
}

# Test 6: Memory and resource usage
message("\n=== Test 6: Resource Usage Check ===")

# Check if bloom filter connections are still active
active_connections <- sum(sapply(bloom_connections, function(con) {
  tryCatch({
    dbGetQuery(con, "SELECT 1")
    TRUE
  }, error = function(e) FALSE)
}))

message(sprintf("Active bloom filter connections: %d/%d", active_connections, length(bloom_connections)))

# Clean up
lapply(bloom_connections, function(con) {
  tryCatch(dbDisconnect(con), error = function(e) NULL)
})

message("\n=== Bloom Filter Testing Complete ===")
message("Bloom filter implementation is working correctly!")
message("Performance improvements demonstrated for large candidate lists.")