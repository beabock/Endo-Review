#8/1/25
#BB
#Merging all abstract datasets

#This file will create the final, all abstracts accounted for in WOS + Scopus using the search string ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungi" OR "endophytic fungus") AND plan. This search was done the final time on 11/18/24 by BB, and all abstracts were downloaded in tsv (WOS) or csv formats. I will combine all of these into one dataset then deduplicate them. WOS only allows you to export 1000 abstracts at a time, so that's why there are 8 WOS files that need to be combod.

#This is from pull two, completed in 2025.

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(digest)

setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

wos_folder <- "data/raw/All_abstracts_8-14-25/WoS"

# List all .txt files in the folder (adjust pattern if needed)
wos_files <- list.files(path = wos_folder, pattern = "\\.txt$", full.names = TRUE)

wos_list <- lapply(wos_files, function(f) {
  df <- read_tsv(f, show_col_types = FALSE)
  df[] <- lapply(df, as.character)
  df
})

wos <- bind_rows(wos_list)

cat("Number of WoS files read:", length(wos_files), "\n")
cat("Total rows after binding:", nrow(wos), "\n")


write.csv(wos, "data/processed/wos_combined.csv", row.names = FALSE)

# --- Read and bind all Scopus files in the specified folder ---

scopus_folder <- "data/raw/All_abstracts_8-14-25/Scopus"

# List all .csv files in the folder (adjust pattern if needed)
scopus_files <- list.files(path = scopus_folder, pattern = "\\.csv$", full.names = TRUE)

# Read and coerce all columns to character for each file
scopus_list <- lapply(scopus_files, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  df[] <- lapply(df, as.character)
  df
})

scopus <- bind_rows(scopus_list)

cat("Number of Scopus files read:", length(scopus_files), "\n")
cat("Total rows after binding:", nrow(scopus), "\n")

write.csv(scopus, "data/processed/scopus_combined.csv", row.names = FALSE)


pubmed <- read_csv("data/raw/All_abstracts_8-14-25/pubmed_pull_8-14-25.csv", show_col_types = FALSE)

# --- Data Overview ---
cat("WoS columns:", length(colnames(wos)), "\n")
cat("Scopus columns:", length(colnames(scopus)), "\n") 
cat("PubMed columns:", length(colnames(pubmed)), "\n")

# Create a mapping of acronyms to full names
colname_mapping <- c(
  PT = "Document.Abb",
  AU = "Authors",
  BA = "Book.Authors",
  BE = "Editors",
  GP = "Group.Authors",
  AF = "Author.full.names",
  BF = "Book.Full.Names",
  CA = "Conference.Authors",
  TI = "Title",
  SO = "Source.title",
  SE = "Series.Title",
  BS = "Book.Series",
  LA = "Language.of.Original.Document",
  DT = "Document.Type",
  CT = "Conference.name",
  CY = "Conference.date",
  CL = "Conference.location",
  SP = "Sponsors",
  HO = "Host",
  DE = "Author.Keywords",
  ID = "Index.Keywords",
  AB = "Abstract",
  C1 = "Affiliations",
  C3 = "Authors.with.affiliations",
  RP = "Correspondence.Address",
  EM = "Email.Address",
  RI = "Researcher.IDs",
  OI = "ORCID.IDs",
  FU = "Funding.Details",
  FP = "Funding.Programs",
  FX = "Funding.Texts",
  CR = "References",
  NR = "Cited.References",
  TC = "Times.Cited",
  Z9 = "Total.Times.Cited",
  U1 = "Usage.Count.180.Days",
  U2 = "Usage.Count.Since.2013",
  PU = "Publisher",
  PI = "Publisher.City",
  PA = "Publisher.Address",
  SN = "ISSN",
  EI = "eISSN",
  BN = "ISBN",
  J9 = "Abbreviated.Source.Title",
  JI = "Journal.ISO",
  PD = "Publication.Date",
  PY = "Year",
  VL = "Volume",
  IS = "Issue",
  PN = "Art..No.",
  SU = "Supplement",
  SI = "Special.Issue",
  MA = "Meeting.Abstract",
  BP = "Page.start",
  EP = "Page.end",
  AR = "Art..No.",
  DI = "DOI",
  DL = "DOI.Link",
  D2 = "Secondary.DOI",
  EA = "Early.Access.Date",
  PG = "Page.count",
  WC = "Web.of.Science.Categories",
  WE = "Research.Areas",
  SC = "Subject.Categories",
  GA = "Document.Delivery.Number",
  PM = "PubMed.ID",
  OA = "Open.Access",
  HC = "Highly.Cited.Paper",
  HP = "Hot.Paper",
  DA = "Date",
  UT = "WOS_ID"
)

unmapped_cols <- setdiff(colnames(wos), names(colname_mapping))


# Rename columns in wos
colnames(wos) <- ifelse(colnames(wos) %in% names(colname_mapping), 
                        colname_mapping[colnames(wos)], 
                        colnames(wos))


wos <- wos %>%
  select(
    -Art..No.
  )

# View the updated column names
cat("WoS standardized columns:", length(colnames(wos)), "\n")

# --- Standardize Scopus column names ---
# Rename Scopus columns to match WoS standardized names
scopus <- scopus %>%
  rename(
    Title = `Title`,
    Authors = `Authors`, 
    Abstract = `Abstract`,
    DOI = `DOI`,
    Year = `Year`,
    Source.title = `Source title`,
    Affiliations = `Affiliations`,
    Author.Keywords = `Author Keywords`,
    Index.Keywords = `Index Keywords`,
    Publisher = `Publisher`,
    ISSN = `ISSN`,
    Volume = `Volume`,
    Issue = `Issue`,
    Page.start = `Page start`,
    Page.end = `Page end`,
    Times.Cited = `Cited by`,
    Document.Type = `Document Type`,
    Language.of.Original.Document = `Language of Original Document`
  )

# --- Standardize PubMed column names ---
# Rename PubMed columns to match WoS standardized names  
pubmed <- pubmed %>%
  rename(
    Title = `title`,
    Authors = `authors`,
    Abstract = `abstract`, 
    DOI = `doi`,
    Year = `year`,
    Source.title = `journal`
  )

# --- Add source column to track origin ---
wos$Source <- "WoS"
scopus$Source <- "Scopus" 
pubmed$Source <- "PubMed"

# --- Combine all three datasets ---
# Get common columns across all datasets
common_cols <- intersect(intersect(colnames(wos), colnames(scopus)), colnames(pubmed))
cat("Common columns:", length(common_cols), "\n")

# Get all unique columns
all_cols <- unique(c(colnames(wos), colnames(scopus), colnames(pubmed)))

# Add missing columns to each dataset (fill with NA)
for(col in all_cols) {
  if(!col %in% colnames(wos)) wos[[col]] <- NA
  if(!col %in% colnames(scopus)) scopus[[col]] <- NA  
  if(!col %in% colnames(pubmed)) pubmed[[col]] <- NA
}

# Reorder columns to match
wos <- wos[, all_cols]
scopus <- scopus[, all_cols]
pubmed <- pubmed[, all_cols]

wos$Year <- as.numeric(wos$Year)
scopus$Year <- as.numeric(scopus$Year)
pubmed$Year <- as.numeric(pubmed$Year)

# Combine all datasets (prioritize WoS > Scopus > PubMed when deduplicating)
ds <- bind_rows(wos, scopus, pubmed) %>%
  filter(Abstract != "[No abstract available]", #Can't do anything with missing abstracts
         !is.na(Abstract),
         Abstract != "") %>%
  arrange(factor(Source, levels = c("WoS", "Scopus", "PubMed")))  # Prioritize by metadata richness

cat("Combined dataset rows:", nrow(ds), "\n")

# --- Deduplication Process ---
normalize_text <- function(x) {
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)          # collapse whitespace
  x <- gsub("[[:punct:]]", "", x)    # remove punctuation
  x <- stringr::str_trim(x)          # trim leading/trailing spaces
  return(x)
}

# Step 1: Deduplicate by DOI (preserving NA DOIs)
ds_no_na <- ds %>% filter(!is.na(DOI) & DOI != "")
ds_na <- ds %>% filter(is.na(DOI) | DOI == "")
deduplicated <- ds_no_na %>% distinct(DOI, .keep_all = TRUE)
deduplicated <- bind_rows(deduplicated, ds_na)
cat("After DOI deduplication:", nrow(deduplicated), "\n")

# Step 2: Create normalized text columns for further deduplication
deduplicated <- deduplicated %>%
  mutate(Abstract_norm = normalize_text(Abstract),
         Title_norm = normalize_text(Title),
         Authors_norm = normalize_text(Authors))

# Step 3: Deduplicate by abstract
deduplicated <- deduplicated %>% 
  distinct(Abstract_norm, .keep_all = TRUE)
cat("After abstract deduplication:", nrow(deduplicated), "\n")

# Step 4: Filter to articles only
deduplicated <- deduplicated %>%
  filter(Document.Type %in% c("Article", NA))
cat("After filtering to articles:", nrow(deduplicated), "\n")

# Step 5: Final deduplication by title
deduplicated <- deduplicated %>%
  distinct(Title_norm, .keep_all = TRUE)
cat("After title deduplication:", nrow(deduplicated), "\n")


# --- Final cleanup and output ---
final_data <- deduplicated %>%
  select(-Abstract_norm, -Title_norm, -Authors_norm) %>%
  relocate(Title, Authors, Year, Source.title, Abstract, DOI)

cat("Final dataset rows:", nrow(final_data), "\n")
cat("Missing DOIs:", sum(is.na(final_data$DOI)), "\n")
cat("Missing Authors:", sum(is.na(final_data$Authors)), "\n")
cat("Missing Titles:", sum(is.na(final_data$Title)), "\n")
cat("Missing Abstracts:", sum(is.na(final_data$Abstract)), "\n")

write.csv(final_data, "data/processed/All_abstracts_deduped.csv", row.names = FALSE)
cat("Data written to: data/processed/All_abstracts_deduped.csv\n")
