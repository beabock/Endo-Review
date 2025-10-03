# Count lines in WoS files

library(readr)

files <- list.files("data/raw/All_abstracts_8-14-25/WoS/", pattern = "*.txt", full.names = TRUE)

total_lines <- 0

for (file in files) {

  lines <- length(read_lines(file))

  total_lines <- total_lines + lines

}

cat("Total lines in WoS files:", total_lines, "\n")