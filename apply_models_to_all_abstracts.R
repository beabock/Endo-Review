# Apply trained models to All_Abstracts.csv - Optimized for Speed
# Date: 7/14/2025

library(tidyverse)
library(tidytext)
library(caret)
library(Matrix)
library(text)
library(tm)
library(janitor)
library(doParallel)
library(foreach)

# Set up parallel processing
cores <- parallel::detectCores() - 2  # Leave two cores free for doing other things while this runs
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
cat("Using", cores, "cores for parallel processing\n")

# Function to time operations
time_operation <- function(expr, message) {
  start_time <- Sys.time()
  result <- eval(expr)
  end_time <- Sys.time()
  cat(message, "completed in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
  return(result)
}

# Load the trained models
cat("Loading trained models...\n")
model_relevance <- readRDS("best_model_svmLinear_relevance_classification.rds")
model_presence_absence <- readRDS("best_model_svmLinear_presence_vs_absence.rds")

# Load All_Abstracts.csv
cat("Loading All_Abstracts.csv...\n")
all_abstracts <- time_operation(
  read.csv("All_Abstracts.csv", stringsAsFactors = FALSE) %>%
    clean_names(),
  "Loading All_Abstracts.csv"
)

# Check the size of the dataset
cat("Dataset size:", nrow(all_abstracts), "rows,", ncol(all_abstracts), "columns\n")

# If the dataset is very large, consider sampling for testing
# Use a sample for faster testing
#all_abstracts <- all_abstracts[sample(nrow(all_abstracts), min(100, nrow(all_abstracts))), ]

# Check if abstract column exists, if not, try to find it
if (!"abstract" %in% colnames(all_abstracts)) {
  # Look for column names that might contain abstracts
  possible_abstract_cols <- grep("abstract", colnames(all_abstracts), ignore.case = TRUE)
  if (length(possible_abstract_cols) > 0) {
    cat("Abstract column not found, using column:", colnames(all_abstracts)[possible_abstract_cols[1]], "\n")
    colnames(all_abstracts)[possible_abstract_cols[1]] <- "abstract"
  } else {
    stop("Could not find abstract column in All_Abstracts.csv")
  }
}

# Add ID column if not present
if (!"id" %in% colnames(all_abstracts)) {
  all_abstracts$id <- 1:nrow(all_abstracts)
}

# Preprocess abstracts in the same way as training data
cat("Preprocessing abstracts...\n")

# Function to process a chunk of abstracts
process_chunk <- function(chunk) {
  # Tokenize unigrams
  unigrams <- chunk %>%
    unnest_tokens(word, abstract, token = "words") %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "\\d")) %>%
    mutate(word = str_to_lower(word)) %>%
    count(id, word, sort=TRUE)
  
  # Tokenize bigrams
  bigrams <- chunk %>%
    unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!str_detect(word1, "\\d")) %>%
    filter(!str_detect(word2, "\\d")) %>%
    unite(bigram, word1, word2, sep = "_") %>%
    count(id, bigram, sort=TRUE) %>%
    rename(word = bigram)
  
  return(list(unigrams = unigrams, bigrams = bigrams))
}

# Determine chunk size based on dataset size
chunk_size <- min(1000, ceiling(nrow(all_abstracts) / cores))
chunks <- split(all_abstracts, ceiling(seq_along(1:nrow(all_abstracts)) / chunk_size))
cat("Processing data in", length(chunks), "chunks of size", chunk_size, "\n")

# Process chunks in parallel
results <- time_operation({
  foreach(chunk = chunks, .combine = c, .packages = c("tidyverse", "tidytext")) %dopar% {
    list(process_chunk(chunk))
  }
}, "Parallel tokenization")

# Combine results
dtm_unigrams <- bind_rows(lapply(results, function(x) x$unigrams))
dtm_bigrams <- bind_rows(lapply(results, function(x) x$bigrams))

# Combine unigrams and bigrams and create DTM
dtm_combined <- time_operation({
  bind_rows(dtm_unigrams, dtm_bigrams) %>%
    # Apply TF-IDF weighting
    bind_tf_idf(word, id, n) %>%
    select(id, word, tf_idf) %>%
    cast_dtm(document = id, term = word, value = tf_idf)
}, "Creating document-term matrix")

dtm <- dtm_combined
dtm_mat <- as.matrix(dtm)
rownames(dtm_mat) <- dtm$dimnames$Docs

# Extract the terms used by the models
cat("Extracting model terms...\n")

# Function to extract terms from a model - optimized
extract_model_terms <- function(model) {
  # Try the most direct approach first
  if (!is.null(model$trainingData)) {
    terms <- colnames(model$trainingData)
    # Remove the last column which is the response variable
    return(terms[-length(terms)])
  } else if (!is.null(model$terms)) {
    return(attr(model$terms, "term.labels"))
  } else if (!is.null(model$finalModel$terms)) {
    return(attr(model$finalModel$terms, "term.labels"))
  } else {
    # Fallback
    warning("Could not extract terms from model, using empty vector")
    return(character(0))
  }
}

# Create a prediction-ready data frame from the DTM - optimized
create_prediction_df <- function(dtm_mat, model) {
  # Get model terms
  model_terms <- extract_model_terms(model)
  
  # Convert DTM to sparse matrix for efficiency
  if (!is(dtm_mat, "sparseMatrix")) {
    dtm_mat <- Matrix(dtm_mat, sparse = TRUE)
  }
  
  # Create a data frame with only the needed columns
  common_terms <- intersect(colnames(dtm_mat), model_terms)
  df <- as.data.frame(as.matrix(dtm_mat[, common_terms, drop = FALSE]))
  
  # Add missing terms efficiently
  missing_terms <- setdiff(model_terms, common_terms)
  if (length(missing_terms) > 0) {
    cat("Adding", length(missing_terms), "missing terms to the data frame...\n")
    missing_df <- matrix(0, nrow = nrow(dtm_mat), ncol = length(missing_terms))
    colnames(missing_df) <- missing_terms
    df <- cbind(df, missing_df)
  }
  
  # Ensure column order matches model terms
  df <- df[, intersect(model_terms, colnames(df)), drop = FALSE]
  
  return(df)
}

# Create prediction-ready data frames
cat("Creating prediction-ready data frames...\n")
relevance_df <- time_operation(
  create_prediction_df(dtm_mat, model_relevance),
  "Creating relevance data frame"
)
presence_absence_df <- time_operation(
  create_prediction_df(dtm_mat, model_presence_absence),
  "Creating presence/absence data frame"
)

# Apply Stage 1: Relevance Classification
cat("Applying Stage 1: Relevance Classification...\n")
# Use tryCatch to handle potential errors
relevance_result <- time_operation({
  tryCatch({
    # Process in batches for large datasets
    batch_size <- 5000
    n_batches <- ceiling(nrow(relevance_df) / batch_size)
    
    if (n_batches > 1) {
      cat("Processing relevance classification in", n_batches, "batches\n")
      all_probs <- list()
      
      for (i in 1:n_batches) {
        start_idx <- (i-1) * batch_size + 1
        end_idx <- min(i * batch_size, nrow(relevance_df))
        cat("  Processing batch", i, "of", n_batches, "(rows", start_idx, "to", end_idx, ")\n")
        
        batch_df <- relevance_df[start_idx:end_idx, , drop = FALSE]
        batch_probs <- predict(model_relevance, newdata = batch_df, type = "prob")
        all_probs[[i]] <- batch_probs
      }
      
      relevance_probs <- do.call(rbind, all_probs)
    } else {
      relevance_probs <- predict(model_relevance, newdata = relevance_df, type = "prob")
    }
    
    relevance_preds <- ifelse(relevance_probs$Relevant > 0.4, "Relevant", "Irrelevant")
    list(probs = relevance_probs, preds = relevance_preds)
  }, error = function(e) {
    cat("Error in relevance prediction:", e$message, "\n")
    # Create dummy results
    dummy_probs <- data.frame(
      Irrelevant = rep(0.5, nrow(relevance_df)),
      Relevant = rep(0.5, nrow(relevance_df))
    )
    dummy_preds <- rep("Relevant", nrow(relevance_df))
    list(probs = dummy_probs, preds = dummy_preds)
  })
}, "Relevance classification")

relevance_probs <- relevance_result$probs
relevance_preds <- relevance_result$preds

# Apply Stage 2: Presence vs. Absence Classification (only for relevant abstracts)
cat("Applying Stage 2: Presence vs. Absence Classification...\n")
# Use tryCatch to handle potential errors
presence_absence_result <- time_operation({
  tryCatch({
    # Process in batches for large datasets
    batch_size <- 5000
    n_batches <- ceiling(nrow(presence_absence_df) / batch_size)
    
    if (n_batches > 1) {
      cat("Processing presence/absence classification in", n_batches, "batches\n")
      all_probs <- list()
      
      for (i in 1:n_batches) {
        start_idx <- (i-1) * batch_size + 1
        end_idx <- min(i * batch_size, nrow(presence_absence_df))
        cat("  Processing batch", i, "of", n_batches, "(rows", start_idx, "to", end_idx, ")\n")
        
        batch_df <- presence_absence_df[start_idx:end_idx, , drop = FALSE]
        batch_probs <- predict(model_presence_absence, newdata = batch_df, type = "prob")
        all_probs[[i]] <- batch_probs
      }
      
      presence_absence_probs <- do.call(rbind, all_probs)
    } else {
      presence_absence_probs <- predict(model_presence_absence, newdata = presence_absence_df, type = "prob")
    }
    
    presence_absence_preds <- ifelse(presence_absence_probs$Presence > 0.5, "Presence", "Absence")
    list(probs = presence_absence_probs, preds = presence_absence_preds)
  }, error = function(e) {
    cat("Error in presence/absence prediction:", e$message, "\n")
    # Create dummy results
    dummy_probs <- data.frame(
      Absence = rep(0.5, nrow(presence_absence_df)),
      Presence = rep(0.5, nrow(presence_absence_df))
    )
    dummy_preds <- rep("Presence", nrow(presence_absence_df))
    list(probs = dummy_probs, preds = dummy_preds)
  })
}, "Presence/absence classification")

presence_absence_probs <- presence_absence_result$probs
presence_absence_preds <- presence_absence_result$preds

# Combine predictions
cat("Combining predictions...\n")
final_preds <- ifelse(relevance_preds == "Relevant", presence_absence_preds, "Irrelevant")

# Add predictions to the original data
all_abstracts$relevance_prediction <- relevance_preds
all_abstracts$relevance_confidence <- ifelse(relevance_preds == "Relevant", 
                                           relevance_probs$Relevant, 
                                           relevance_probs$Irrelevant)
all_abstracts$presence_absence_prediction <- ifelse(relevance_preds == "Relevant", 
                                                  presence_absence_preds, 
                                                  NA)
all_abstracts$presence_absence_confidence <- ifelse(relevance_preds == "Relevant",
                                                  ifelse(presence_absence_preds == "Presence", 
                                                         presence_absence_probs$Presence, 
                                                         presence_absence_probs$Absence),
                                                  NA)
all_abstracts$final_prediction <- final_preds

# Save results
output_file <- "all_abstracts_predictions.csv"
cat("Saving results to", output_file, "...\n")
time_operation(
  write.csv(all_abstracts, output_file, row.names = FALSE),
  "Saving results"
)

# Summary statistics
cat("\nSummary of predictions:\n")
cat("Total abstracts:", nrow(all_abstracts), "\n")
cat("Relevant abstracts:", sum(relevance_preds == "Relevant"), "\n")
cat("Irrelevant abstracts:", sum(relevance_preds == "Irrelevant"), "\n")
cat("Presence abstracts (among relevant):", sum(presence_absence_preds == "Presence" & relevance_preds == "Relevant"), "\n")
cat("Absence abstracts (among relevant):", sum(presence_absence_preds == "Absence" & relevance_preds == "Relevant"), "\n")

# Clean up
stopCluster(cl)
cat("\nDone!\n")
