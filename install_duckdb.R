# Install DuckDB package for bloom filter implementation
install.packages("duckdb")

# Verify installation
if (!require("duckdb", character.only = TRUE)) {
  stop("Failed to install duckdb package")
}

message("DuckDB package installed successfully")