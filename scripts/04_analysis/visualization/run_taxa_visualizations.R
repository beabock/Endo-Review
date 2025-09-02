# Simple script to run the taxa representation visualizations
# This script calls the main function with proper parameters

# Set working directory
setwd("c:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

# Source the visualization script
source("visualize_taxa_results.R")

# Run the analysis with your specific file paths
message("Starting taxa representation analysis...")

results <- create_taxa_representation_visualizations(
  results_file = "results/comprehensive_extraction_results.csv",
  species_file = "models/species.rds",
  pbdb_file = "data/raw/pbdb_all.csv",
  output_dir = "plots"
)

message("Analysis complete! Check the plots/ directory for results.")
message("Generated files:")
list.files("plots", pattern = "*representation*")
