# Debug the species.rds file to understand its format
setwd("c:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

rds_file <- "models/species.rds"

message("=== Debugging species.rds file ===")

# Check file size and basic info
file_info <- file.info(rds_file)
message("File size: ", file_info$size, " bytes")
message("Is RDS file: ", tools::file_ext(rds_file) == "rds")

# Try to read raw bytes
message("\n=== First 100 raw bytes ===")
raw_data <- readBin(rds_file, "raw", n = 100)
message(paste(raw_data[1:50], collapse = " "))

# Try different approaches to read the file
message("\n=== Trying different read methods ===")

# Method 1: Try as RDS
tryCatch({
  data1 <- readRDS(rds_file)
  message("RDS read successful")
  message("Type: ", typeof(data1))
  message("Length: ", length(data1))
  if (is.data.frame(data1)) {
    message("Data frame dimensions: ", dim(data1))
    message("Column names: ", paste(colnames(data1), collapse = ", "))
    message("First few values:")
    print(head(data1, 3))
  } else {
    message("First few elements:")
    print(head(data1, 3))
  }
}, error = function(e) {
  message("RDS read failed: ", e$message)
})

# Method 2: Try as raw text
message("\n=== Reading as text ===")
tryCatch({
  text_data <- readLines(rds_file, n = 5, warn = FALSE)
  message("First 5 lines as text:")
  for (i in 1:length(text_data)) {
    # Show first 100 chars of each line
    line_preview <- substr(text_data[i], 1, 100)
    message("Line ", i, ": ", line_preview)
  }
}, error = function(e) {
  message("Text read failed: ", e$message)
})

# Method 3: Check if it's a compressed file
message("\n=== Checking file compression ===")
tryCatch({
  con <- gzfile(rds_file, "rb")
  gz_data <- readBin(con, "raw", n = 50)
  close(con)
  message("First 50 bytes from gzfile:")
  message(paste(gz_data, collapse = " "))
}, error = function(e) {
  message("Gzfile read failed: ", e$message)
})

# Method 4: Check file signature
message("\n=== File signature analysis ===")
file_signature <- readBin(rds_file, "raw", n = 10)
message("File signature (hex): ", paste(sprintf("%02x", as.integer(file_signature)), collapse = " "))

# RDS files typically start with "RDX2" or "RDX3"
rds_signature <- charToRaw("RDX")
if (identical(file_signature[1:3], rds_signature)) {
  message("This appears to be a valid RDS file")
} else {
  message("This does NOT appear to be a standard RDS file")
}

# Check if it might be a CSV or TSV file saved with .rds extension
message("\n=== Checking if it's actually a delimited file ===")
tryCatch({
  csv_data <- read.table(rds_file, sep = "\t", header = TRUE, nrows = 3, stringsAsFactors = FALSE)
  message("Successfully read as TSV:")
  print(csv_data)
}, error = function(e) {
  message("TSV read failed: ", e$message)
})

tryCatch({
  csv_data <- read.table(rds_file, sep = ",", header = TRUE, nrows = 3, stringsAsFactors = FALSE)
  message("Successfully read as CSV:")
  print(csv_data)
}, error = function(e) {
  message("CSV read failed: ", e$message)
})
