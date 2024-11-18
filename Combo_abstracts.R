#11/18/24
#BB
#Merging all abstract datasets

#This file will create the final, all abstracts accounted for in WOS + Scopus using the search string ("fungal endophyte" OR "fungal endophytes" OR "endophytic fungi" OR "endophytic fungus") AND plan. This search was done the final time on 11/18/24 by BB, and all abstracts were downloaded in tsv (WOS) or csv formats. I will combine all of these into one dataset then deduplicate them. WOS only allows you to export 1000 abstracts at a time, so that's why there are 8 WOS files that need to be combod.

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

scop <- read.csv("scopus.csv")

ds <- full_join(wos, scop)

# Step 2: Count duplicated DOIs
n_duplicates <- sum(duplicated(combined$doi))
cat("Number of duplicated DOIs:", n_duplicates, "\n")

# Step 3: Remove duplicates, keeping the first occurrence
deduplicated <- combined %>% distinct(doi, .keep_all = TRUE)

ds %>%
  group_by()%>%#Check authors, titles, dois, and other info. Keep each check.
  summarize(n=n())
