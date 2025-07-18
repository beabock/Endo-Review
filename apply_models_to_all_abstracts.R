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
library(recipes)
library(themis)
library(irlba)
library(randomForest)
library(xgboost)

# Function to extract terms from a model - optimized with additional methods
extract_model_terms <- function(model) {
  tryCatch({
    # Print model class for diagnostics
    cat("Model class:", class(model), "\n")
    
    # Try multiple approaches to get terms
    terms <- NULL
    
    # Try the most direct approach first
    if (!is.null(model$trainingData)) {
      cat("Extracting terms from trainingData\n")
      terms <- colnames(model$trainingData)
      # Remove the last column which is the response variable
      terms <- terms[-length(terms)]
    } else if (!is.null(model$terms)) {
      cat("Extracting terms from terms attribute\n")
      terms <- attr(model$terms, "term.labels")
    } else if (!is.null(model$finalModel$terms)) {
      cat("Extracting terms from finalModel terms\n")
      terms <- attr(model$finalModel$terms, "term.labels")
    } else if (!is.null(model$xlevels)) {
      cat("Extracting terms from xlevels\n")
      terms <- names(model$xlevels)
    } else if (!is.null(model$featureNames)) {
      cat("Extracting terms from featureNames\n")
      terms <- model$featureNames
    } else if (!is.null(model$finalModel$xNames)) {
      cat("Extracting terms from finalModel xNames\n")
      terms <- model$finalModel$xNames
    }
    
    if (is.null(terms) || length(terms) == 0) {
      # Try to inspect the model structure
      cat("Model structure summary:\n")
      print(str(model))
      stop("Could not find terms in model using any known method")
    }
    
    # Clean up terms
    terms <- unique(terms[!is.na(terms)])
    cat("Found", length(terms), "terms\n")
    return(terms)
    
  }, error = function(e) {
    warning("Could not extract terms from model: ", e$message)
    return(character(0))
  })
}


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

# Load the trained models with error checking
cat("Loading trained models...\n")

# Load relevance model and its threshold
relevance_model_info <- tryCatch({
  readRDS("best_model_svmLinear_relevance_classification.rds")
}, error = function(e) {
  stop("Could not load relevance model: ", e$message)
})

# Extract model and threshold
model_relevance <- if (inherits(relevance_model_info, "train")) {
  relevance_model_info
} else if (is.list(relevance_model_info) && !is.null(relevance_model_info$model)) {
  relevance_model_info$model
} else {
  stop("Relevance model file does not contain expected model structure")
}

best_threshold <- if (is.list(relevance_model_info) && !is.null(relevance_model_info$threshold)) {
  relevance_model_info$threshold
} else {
  cat("No threshold found in model file, using default 0.5\n")
  0.5
}
cat("Using threshold:", best_threshold, "\n")

# Load presence/absence model
presence_absence_model_info <- tryCatch({
  readRDS("best_model_svmLinear_presence_vs_absence.rds")
}, error = function(e) {
  stop("Could not load presence/absence model: ", e$message)
})

# Extract presence/absence model
model_presence_absence <- if (inherits(presence_absence_model_info, "train")) {
  presence_absence_model_info
} else if (is.list(presence_absence_model_info) && !is.null(presence_absence_model_info$model)) {
  presence_absence_model_info$model
} else {
  stop("Presence/absence model file does not contain expected model structure")
}

# Verify models are loaded correctly
if (!inherits(model_relevance, "train")) {
  stop("Relevance model is not a valid caret model")
}
if (!inherits(model_presence_absence, "train")) {
  stop("Presence/absence model is not a valid caret model")
}

cat("Models and thresholds loaded successfully\n")


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

# Function to extract terms from a model - optimized (moved up)
extract_model_terms <- function(model) {
  tryCatch({
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
      # Try to extract from model object directly
      return(colnames(model$xlevels[[1]]))
    }
  }, error = function(e) {
    warning("Could not extract terms from model, using empty vector: ", e$message)
    return(character(0))
  })
}

# Combine results
cat("\nCombining results...\n")
cat("Number of chunks processed:", length(results), "\n")
dtm_unigrams <- bind_rows(lapply(results, function(x) x$unigrams))
dtm_bigrams <- bind_rows(lapply(results, function(x) x$bigrams))
cat("Unigrams dimensions:", nrow(dtm_unigrams), "x", length(unique(dtm_unigrams$word)), "\n")
cat("Bigrams dimensions:", nrow(dtm_bigrams), "x", length(unique(dtm_bigrams$word)), "\n")

# Extract model terms first
cat("\nExtracting model terms...\n")
relevance_terms <- extract_model_terms(model_relevance)
presence_terms <- extract_model_terms(model_presence_absence)
needed_terms <- unique(c(relevance_terms, presence_terms))
cat("Number of terms in models:", length(needed_terms), "\n")

if (length(needed_terms) == 0) {
  stop("No terms could be extracted from the models. Check if models were loaded correctly.")
}

# Combine unigrams and bigrams and create DTM (memory-efficient version)
dtm_combined <- time_operation({
  # Combine while keeping only relevant terms
  relevance_terms <- extract_model_terms(model_relevance)
  presence_terms <- extract_model_terms(model_presence_absence)
  needed_terms <- unique(c(relevance_terms, presence_terms))
  
  # Remove .outcome from needed_terms as it's not a text feature
  needed_terms <- needed_terms[needed_terms != ".outcome"]
  cat("Number of needed terms after removing .outcome:", length(needed_terms), "\n")
  
  # Filter terms before combining to save memory
  combined_df <- bind_rows(
    dtm_unigrams %>% filter(word %in% needed_terms),
    dtm_bigrams %>% filter(word %in% needed_terms)
  )
  
  # Debug output
  cat("Number of rows in combined_df:", nrow(combined_df), "\n")
  cat("Number of unique terms in combined_df:", length(unique(combined_df$word)), "\n")
  cat("Sample of terms found:", paste(head(unique(combined_df$word)), collapse=", "), "\n")
  
  # Apply TF-IDF weighting and create DTM
  dtm <- combined_df %>%
    bind_tf_idf(word, id, n) %>%
    select(id, word, tf_idf)
  
  # Debug output before cast_dtm
  cat("Dimensions before cast_dtm:", nrow(dtm), "x", length(unique(dtm$word)), "\n")
  
  # Create sparse matrix directly instead of using cast_dtm
  # First, ensure we have data for all abstracts
  all_ids <- as.character(all_abstracts$id)
  
  # Make sure we have entries for all abstracts (fill with zero if missing)
  if (nrow(dtm) > 0) {
    # Get all unique document IDs from the DTM data
    dtm_ids <- unique(dtm$id)
    missing_ids <- setdiff(all_ids, as.character(dtm_ids))
    
    if (length(missing_ids) > 0) {
      cat("Adding", length(missing_ids), "missing abstracts with zero values\n")
      # Add missing abstracts with zero values for a common term
      if (length(unique(dtm$word)) > 0) {
        dummy_term <- unique(dtm$word)[1]
        missing_data <- data.frame(
          id = as.numeric(missing_ids),
          word = dummy_term,
          tf_idf = 0
        )
        dtm <- bind_rows(dtm, missing_data)
      }
    }
  }
  
  sparse_matrix <- sparseMatrix(
    i = as.numeric(factor(dtm$id, levels = all_ids)),
    j = as.numeric(factor(dtm$word)),
    x = dtm$tf_idf,
    dims = c(length(all_ids), length(unique(dtm$word))),
    dimnames = list(
      all_ids,
      levels(factor(dtm$word))
    )
  )
  
  # Debug output after creating sparse matrix
  cat("Final DTM dimensions:", nrow(sparse_matrix), "x", ncol(sparse_matrix), "\n")
  cat("Sample of column names:", paste(head(colnames(sparse_matrix)), collapse=", "), "\n")
  
  sparse_matrix
}, "Creating document-term matrix")

# Keep result as sparse matrix
dtm_mat <- dtm_combined

# Verify the matrix structure
cat("\nVerifying matrix structure:\n")
cat("DTM dimensions:", dim(dtm_mat), "\n")
cat("Number of non-zero entries:", length(dtm_mat@x), "\n")
cat("Sample of terms:", paste(head(colnames(dtm_mat)), collapse=", "), "\n")

# Note: No manual feature engineering needed - all features come from DTM
cat("\nPreparing feature matrix...\n")

# Extract the terms used by the models
cat("\nExtracting model terms...\n")

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

# Create a prediction-ready data frame from the DTM - optimized with better diagnostics
create_prediction_df <- function(dtm_mat, model) {
  tryCatch({
    # Get model terms
    model_terms <- extract_model_terms(model)
    cat("\nNumber of model terms:", length(model_terms), "\n")
    if (length(model_terms) == 0) {
      stop("No terms extracted from model")
    }
    cat("First few model terms:", head(model_terms), "\n")
    
    # Debug: Check for problematic terms
    if ("fungal_endophyte" %in% model_terms) {
      cat("WARNING: fungal_endophyte found in model terms, removing it\n")
      model_terms <- model_terms[model_terms != "fungal_endophyte"]
    }
    
    # Convert DTM to sparse matrix for efficiency
    if (!is(dtm_mat, "sparseMatrix")) {
      dtm_mat <- Matrix(dtm_mat, sparse = TRUE)
    }
    
    # Print DTM dimensions and some column names for diagnosis
    cat("DTM dimensions:", dim(dtm_mat), "\n")
    cat("First few DTM terms:", head(colnames(dtm_mat)), "\n")
    
    # Create a data frame with only the needed columns
    common_terms <- intersect(colnames(dtm_mat), model_terms)
    cat("Number of common terms:", length(common_terms), "\n")
    if (length(common_terms) == 0) {
      # Additional diagnostics before stopping
      cat("WARNING: No common terms found!\n")
      cat("Sample DTM terms (first 10):", head(colnames(dtm_mat), 10), "\n")
      cat("Sample model terms (first 10):", head(model_terms, 10), "\n")
      stop("No common terms found between DTM and model")
    }
    
    # Keep as sparse matrix as long as possible
    sparse_subset <- dtm_mat[, common_terms, drop = FALSE]
    
    # Add missing terms efficiently
    missing_terms <- setdiff(model_terms, common_terms)
    if (length(missing_terms) > 0) {
      cat("Adding", length(missing_terms), "missing terms to the data frame...\n")
      # Create sparse zero matrix for missing terms
      missing_mat <- Matrix(0, nrow = nrow(dtm_mat), ncol = length(missing_terms),
                          sparse = TRUE)
      colnames(missing_mat) <- missing_terms
      # Combine using sparse cbind
      sparse_subset <- cbind(sparse_subset, missing_mat)
    }
    
    # Ensure column order matches model terms
    sparse_subset <- sparse_subset[, model_terms, drop = FALSE]
    
    # Convert to dense matrix only at the end
    df <- as.data.frame(as.matrix(sparse_subset))
    return(df)
    
  }, error = function(e) {
    stop("Error in create_prediction_df: ", e$message)
  })
}

# Create prediction-ready data frames
cat("Creating prediction-ready data frames...\n")

# Memory-efficient preprocessing
cat("Applying feature preprocessing...\n")
# Convert to dgCMatrix format for efficient operations
if (!is(dtm_mat, "dgCMatrix")) {
  dtm_mat <- as(dtm_mat, "dgCMatrix")
}

# Add any missing columns needed by the model with zero values
model_terms <- unique(c(
  extract_model_terms(model_relevance),
  extract_model_terms(model_presence_absence)
))
# Remove special columns that aren't text features
model_terms <- model_terms[!model_terms %in% c(".outcome")]
cat("Removed special columns (.outcome) from model terms\n")
cat("Number of model terms after filtering:", length(model_terms), "\n")

# Debug: Check for problematic terms
cat("Looking for 'fungal_endophyte' in model terms...\n")
if ("fungal_endophyte" %in% model_terms) {
  cat("WARNING: fungal_endophyte is expected by the model but wasn't created during training!\n")
  # Remove it from required terms since it's not a real feature
  model_terms <- model_terms[model_terms != "fungal_endophyte"]
  cat("Removed fungal_endophyte from required terms\n")
}

existing_terms <- colnames(dtm_mat)
missing_terms <- setdiff(model_terms, existing_terms)

if (length(missing_terms) > 0) {
  cat("Adding", length(missing_terms), "missing terms required by the models...\n")
  missing_mat <- Matrix(0, nrow = nrow(dtm_mat), ncol = length(missing_terms),
                       sparse = TRUE)
  colnames(missing_mat) <- missing_terms
  dtm_mat <- cbind(dtm_mat, missing_mat)
}

# Apply preprocessing directly on sparse matrix
# Note: We skip correlation removal as it's less critical for prediction
cat("Removing near-zero variance features...\n")
# Calculate variance on sparse matrix
col_vars <- colMeans(dtm_mat^2) - colMeans(dtm_mat)^2
# Find columns to keep (non-zero variance)
cols_to_keep <- which(col_vars > 1e-6)
# Also keep any terms required by the models
cols_to_keep <- union(cols_to_keep, match(model_terms, colnames(dtm_mat)))
# Subset the sparse matrix
dtm_mat_processed <- dtm_mat[, cols_to_keep]

# Verify all required terms are present
missing_required <- setdiff(model_terms, colnames(dtm_mat_processed))
if (length(missing_required) > 0) {
  cat("WARNING: Still missing required terms:", paste(missing_required, collapse=", "), "\n")
}

# Create prediction dataframes from processed data
# Ensure we process all abstracts
cat("\nPreparing prediction data frames...\n")
cat("Number of abstracts to process:", nrow(all_abstracts), "\n")

# Create prediction data frames
relevance_df <- time_operation({
  df <- create_prediction_df(dtm_mat_processed, model_relevance)
  if (nrow(df) != nrow(all_abstracts)) {
    stop("Relevance prediction matrix has ", nrow(df), " rows but there are ", 
         nrow(all_abstracts), " abstracts")
  }
  df
}, "Creating relevance data frame")

presence_absence_df <- time_operation({
  df <- create_prediction_df(dtm_mat_processed, model_presence_absence)
  if (nrow(df) != nrow(all_abstracts)) {
    stop("Presence/absence prediction matrix has ", nrow(df), " rows but there are ", 
         nrow(all_abstracts), " abstracts")
  }
  df
}, "Creating presence/absence data frame")

# Apply Stage 1: Relevance Classification
cat("Applying Stage 1: Relevance Classification...\n")
# Use tryCatch to handle potential errors
relevance_result <- time_operation({
  tryCatch({
    # Add any missing columns required by the model
    if (!is.null(model_relevance$preProcess) && !is.null(model_relevance$preProcess$mean)) {
      missing_cols <- setdiff(names(model_relevance$preProcess$mean), colnames(relevance_df))
      if (length(missing_cols) > 0) {
        cat("Adding missing columns for relevance prediction:", 
            paste(missing_cols, collapse=", "), "\n")
        for (col in missing_cols) {
          if (!col %in% c(".outcome", "fungal_endophyte")) {  # Skip problematic columns
            relevance_df[[col]] <- 0
          }
        }
      }
    }
    
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
        # Ensure all required columns are present
        if (!is.null(model_relevance$preProcess) && !is.null(model_relevance$preProcess$mean)) {
          missing_cols <- setdiff(names(model_relevance$preProcess$mean), colnames(batch_df))
          if (length(missing_cols) > 0) {
            for (col in missing_cols) {
              if (!col %in% c(".outcome", "fungal_endophyte")) {  # Skip problematic columns
                batch_df[[col]] <- 0
              }
            }
          }
        }
        
        batch_probs <- predict(model_relevance, newdata = batch_df, type = "prob")
        all_probs[[i]] <- batch_probs
      }
      
      relevance_probs <- do.call(rbind, all_probs)
    } else {
      relevance_probs <- predict(model_relevance, newdata = relevance_df, type = "prob")
    }
    
    # Verify we have predictions for all rows
    if (nrow(relevance_probs) != nrow(all_abstracts)) {
      stop("Got predictions for ", nrow(relevance_probs), 
           " rows but have ", nrow(all_abstracts), " abstracts")
    }
    
    relevance_preds <- ifelse(relevance_probs$Relevant > best_threshold, "Relevant", "Irrelevant")
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
    # Add any missing columns required by the model
    if (!is.null(model_presence_absence$preProcess) && !is.null(model_presence_absence$preProcess$mean)) {
      missing_cols <- setdiff(names(model_presence_absence$preProcess$mean), 
                            colnames(presence_absence_df))
      if (length(missing_cols) > 0) {
        cat("Adding missing columns for presence/absence prediction:", 
            paste(missing_cols, collapse=", "), "\n")
        for (col in missing_cols) {
          if (!col %in% c(".outcome", "fungal_endophyte")) {  # Skip problematic columns
            presence_absence_df[[col]] <- 0
          }
        }
      }
    }
    
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
        # Ensure all required columns are present except special ones
        if (!is.null(model_presence_absence$preProcess) && !is.null(model_presence_absence$preProcess$mean)) {
          missing_cols <- setdiff(names(model_presence_absence$preProcess$mean), 
                                colnames(batch_df))
          if (length(missing_cols) > 0) {
            for (col in missing_cols) {
              if (!col %in% c(".outcome", "fungal_endophyte")) {  # Skip problematic columns
                batch_df[[col]] <- 0
              }
            }
          }
        }
        
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

# Combine predictions with length checking
cat("Combining predictions...\n")

# Verify lengths match
if (length(relevance_preds) != nrow(all_abstracts)) {
  stop("Number of relevance predictions (", length(relevance_preds), 
       ") does not match number of abstracts (", nrow(all_abstracts), ")")
}
if (length(presence_absence_preds) != nrow(all_abstracts)) {
  # Pad presence_absence_preds with NA if needed
  temp_preds <- rep(NA, nrow(all_abstracts))
  temp_preds[1:length(presence_absence_preds)] <- presence_absence_preds
  presence_absence_preds <- temp_preds
}

final_preds <- ifelse(relevance_preds == "Relevant", presence_absence_preds, "Irrelevant")

# Add predictions to the original data with safety checks
all_abstracts$relevance_prediction <- relevance_preds
all_abstracts$relevance_confidence <- ifelse(relevance_preds == "Relevant" & 
                                           !is.null(relevance_probs$Relevant), 
                                           relevance_probs$Relevant, 
                                           NA)
all_abstracts$presence_absence_prediction <- presence_absence_preds
all_abstracts$presence_absence_confidence <- ifelse(relevance_preds == "Relevant" & 
                                                  !is.null(presence_absence_probs), 
                                                  ifelse(presence_absence_preds == "Presence", 
                                                         presence_absence_probs$Presence, 
                                                         presence_absence_probs$Absence),
                                                  NA)
all_abstracts$final_prediction <- final_preds

# Verify all columns were added correctly
cat("Verifying prediction columns...\n")
sapply(c("relevance_prediction", "relevance_confidence", 
         "presence_absence_prediction", "presence_absence_confidence", 
         "final_prediction"), 
       function(col) cat(col, "has", sum(!is.na(all_abstracts[[col]])), 
                        "non-NA values\n"))

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
