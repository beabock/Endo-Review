#8/1/25
#BB
#Merging all abstract datasets

#This file will create the final, all abstracts accounted for in WOS + Scopus using the search string ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungi" OR "endophytic fungus") AND plan. This search was done the final time on 11/18/24 by BB, and all abstracts were downloaded in tsv (WOS) or csv formats. I will combine all of these into one dataset then deduplicate them. WOS only allows you to export 1000 abstracts at a time, so that's why there are 8 WOS files that need to be combod.

#This is from pull two, completed in 2025.

library(readr)
library(dplyr)
library(tidyr)



setwd("C:/Users/beabo/OneDrive/Documents/NAU/Endo-Review")

wos1 <- read_tsv("All_Abstracts-11-18-24/savedrecs.txt")
wos2 <- read_tsv("All_Abstracts-11-18-24/savedrecs(1).txt")
wos3 <- read_tsv("All_Abstracts-11-18-24/savedrecs(2).txt")
wos4 <- read_tsv("All_Abstracts-11-18-24/savedrecs(3).txt")
wos5 <- read_tsv("All_Abstracts-11-18-24/savedrecs(4).txt")
wos6 <- read_tsv("All_Abstracts-11-18-24/savedrecs(5).txt")
wos7 <- read_tsv("All_Abstracts-11-18-24/savedrecs(6).txt")
wos8 <- read_tsv("All_Abstracts-11-18-24/savedrecs(7).txt")

wos <- rbind(wos1, wos2, wos3, wos4, wos5, wos6, wos7, wos8)

scop <- read.csv("All_Abstracts-11-18-24/scopus.csv")

colnames(wos)
colnames(scop)

# Create a mapping of acronyms to full names
colname_mapping <- c(
  PT = "Document.Type",
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
    -Document.Type,
    -Art..No.
  )

# View the updated column names
colnames(wos)

ds <- full_join(wos, scop)%>%
  filter(Abstract != "[No abstract available]",
         !is.na(Abstract))

# Step 2: Count duplicated DOIs
n_duplicates <- sum(duplicated(ds$DOI)) #6373 duplicated dois
cat("Number of duplicated DOIs:", n_duplicates, "\n")

# Step 3: Remove duplicates, keeping the first occurrence
deduplicated <- ds %>% distinct(DOI, .keep_all = TRUE)
#9814 now

duplicated_titles <- deduplicated[duplicated(deduplicated$Title) | duplicated(deduplicated$Title, fromLast = TRUE), ]

# View the rows with duplicated titles
View(duplicated_titles) #61'

deduplicated <- deduplicated %>%
  group_by(Title) %>%
  # Use row-wise logic to combine duplicates while prioritizing NA first
  summarize(across(everything(), ~ {
    if(any(is.na(.))) {
      # Pick the first non-NA value, else pick the first available value
      return(first(na.omit(.)))
    } else {
      return(first(.))
    }
  }))
#9783

duplicated_abs <- deduplicated[duplicated(deduplicated$Abstract) | duplicated(deduplicated$Abstract, fromLast = TRUE), ]

deduplicated <- deduplicated %>%
  group_by(Abstract) %>%
  # Use row-wise logic to combine duplicates while prioritizing NA first
  summarize(across(everything(), ~ {
    if(any(is.na(.))) {
      # Pick the first non-NA value, else pick the first available value
      return(first(na.omit(.)))
    } else {
      return(first(.))
    }
  }))
#9778

duplicated_auth <- deduplicated[duplicated(deduplicated$Authors) | duplicated(deduplicated$Authors, fromLast = TRUE), ]
#View this


deduplicated <- deduplicated %>%
  filter(!is.na(Authors)) #Not helpful without authors
#9778 total

write.csv(deduplicated, "All_Abstracts.csv")
