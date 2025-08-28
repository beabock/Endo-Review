# Debug script to examine model structure
# Load required libraries
library(Matrix)

# Load the models
cat("Loading models...\n")
relevance_model_info <- readRDS("best_model_svmLinear_relevance_classification.rds")
presence_absence_model_info <- readRDS("best_model_svmLinear_presence_vs_absence.rds")

# Extract models
model_relevance <- if (inherits(relevance_model_info, "train")) {
  relevance_model_info
} else if (is.list(relevance_model_info) && !is.null(relevance_model_info$model)) {
  relevance_model_info$model
} else {
  relevance_model_info
}

model_presence_absence <- if (inherits(presence_absence_model_info, "train")) {
  presence_absence_model_info
} else if (is.list(presence_absence_model_info) && !is.null(presence_absence_model_info$model)) {
  presence_absence_model_info$model
} else {
  presence_absence_model_info
}

# Function to extract terms from a model
extract_model_terms <- function(model, model_name) {
  cat("\n=== Examining", model_name, "===\n")
  cat("Model class:", class(model), "\n")
  
  # Try different methods to extract terms
  terms <- NULL
  
  if (!is.null(model$trainingData)) {
    terms <- colnames(model$trainingData)
    cat("Found training data with", length(terms), "columns\n")
    cat("First 10 terms:", paste(head(terms, 10), collapse=", "), "\n")
    cat("Last 10 terms:", paste(tail(terms, 10), collapse=", "), "\n")
    
    # Check for fungal_endophyte specifically
    if ("fungal_endophyte" %in% terms) {
      cat("*** fungal_endophyte FOUND in training data ***\n")
    } else {
      cat("fungal_endophyte NOT found in training data\n")
    }
    
    # Check for related terms
    fungal_related <- terms[grepl("fungal|endophyte|fungi", terms, ignore.case = TRUE)]
    if (length(fungal_related) > 0) {
      cat("Related terms found:", paste(fungal_related, collapse=", "), "\n")
    }
    
    # Remove .outcome column for counting
    terms_without_outcome <- terms[terms != ".outcome"]
    cat("Terms without .outcome:", length(terms_without_outcome), "\n")
    
    return(terms_without_outcome)
  } else {
    cat("No trainingData found\n")
    return(character(0))
  }
}

# Extract terms from both models
relevance_terms <- extract_model_terms(model_relevance, "Relevance Model")
presence_terms <- extract_model_terms(model_presence_absence, "Presence/Absence Model")

# Compare terms
cat("\n=== COMPARISON ===\n")
cat("Relevance model terms:", length(relevance_terms), "\n")
cat("Presence/absence model terms:", length(presence_terms), "\n")

common_terms <- intersect(relevance_terms, presence_terms)
cat("Common terms:", length(common_terms), "\n")

unique_relevance <- setdiff(relevance_terms, presence_terms)
unique_presence <- setdiff(presence_terms, relevance_terms)

cat("Unique to relevance:", length(unique_relevance), "\n")
cat("Unique to presence/absence:", length(unique_presence), "\n")

if (length(unique_relevance) > 0 && length(unique_relevance) <= 20) {
  cat("Terms unique to relevance:", paste(unique_relevance, collapse=", "), "\n")
}
if (length(unique_presence) > 0 && length(unique_presence) <= 20) {
  cat("Terms unique to presence/absence:", paste(unique_presence, collapse=", "), "\n")
}

# Check for problematic terms
all_terms <- unique(c(relevance_terms, presence_terms))
problematic <- all_terms[grepl("fungal_endophyte", all_terms)]
if (length(problematic) > 0) {
  cat("*** PROBLEMATIC TERMS FOUND ***:", paste(problematic, collapse=", "), "\n")
} else {
  cat("No problematic terms found\n")
}

cat("\nDebug complete.\n")
