# Simple check of the species.rds file
setwd("c:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

# Try to read the species.rds file
data <- readRDS("models/species.rds")
print("File read successfully")
print("Type:")
print(typeof(data))
print("Class:")
print(class(data))

if (is.data.frame(data)) {
  print("Dimensions:")
  print(dim(data))
  print("Column names:")
  print(colnames(data))

  if (ncol(data) == 1 && names(data)[1] == "X") {
    print("Single column named 'X' detected")
    print("First few values:")
    print(head(data$X, 5))

    # Check if it looks like tab-delimited data
    first_row <- data$X[1]
    tab_count <- stringr::str_count(first_row, "\t")
    print(paste("Number of tabs in first row:", tab_count))

    if (tab_count > 0) {
      print("This looks like tab-delimited data")
      parsed <- str_split(first_row, "\t")[[1]]
      print("Parsed columns:")
      print(length(parsed))
      print("Column names:")
      print(parsed)
    }
  }
} else {
  print("Not a data frame")
  print("Length:")
  print(length(data))
  print("First few elements:")
  print(head(data, 5))
}
