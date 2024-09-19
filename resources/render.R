install.packages("jsonlite", repos = "https://cloud.r-project.org")

library(optparse) # make_option OptionParser parse_args
library(jsonlite) # fromJSON
library(here)
library(tidyverse)
library(magrittr)

# --------- Get the output from GHA ---------

# Look for the data_in argument
option_list <- list(
  optparse::make_option(
    c("--data_in"),
    type = "character",
    default = NULL,
    help = "Sheet Results (json)",
  )
)

# Read the results provided as command line argument
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)
jsonResults <- opt$data_in

# --------- Interpret the JSON data ---------

# Pull the data itself from the API results
df <- fromJSON(jsonResults)
df <- df$results$result$formatted[[2]]

# Repair if you have column names on your spreadsheet
colnames(df) <- df[1, ] # colnames taken from first row of data
df <- df[-1, ] # remove the first row of data (original column names)
df <- tibble::as_tibble(df)

df[df==""]<-NA #make no responses NAs

print(dim(df))

# --------- Any analysis/tidying you want to do ---------

# ------- Tidying Step: simplify column names ----------
simplifiedColNames <-
  read_delim(here("resources/codebook.txt"),
             delim = "\t",
             col_select = SimplifiedColNames)
df <-
  df %>% `colnames<-`(unlist(simplifiedColNames))

# ------- Tidying Step: Drop any column which contains all NAs ----


df <- df[, colSums(is.na(df)) != nrow(df)]
print(ncol(df))

# ------ Tidying step: If an email is duplicated, keep the latest response -----

df %<>% mutate(Email = str_replace(Email, ".eud", ".edu"))

print(sum(is.na(df$Email)))

tabulatedEmails <- table(df$Email, useNA = "no")

if (sum(tabulatedEmails > 1) > 0) {
  duplicatedEmails <-
    names(tabulatedEmails)[which(tabulatedEmails > 1)]
  IDXs_to_remove <-
    unlist(lapply(1:length(duplicatedEmails), function(x)
      head(
        which(df$Email == duplicatedEmails[x]),-1
      )))
  print(length(IDXs_to_remove)) # I think it's removing the responses with NAs, wondering how those are encoded
  df <- df[-IDXs_to_remove, ]
}

print(nrow(df))

# ------ Tidying step: Combine columns for overlapping questions -------

df %<>% mutate(
  CombinedInterestEthicalAI =
    c(
      EthicalAIInterest[which(df$Timestamp < "2024-02-27")],
      InterestEthicalAI[which(df$Timestamp > "2024-02-27")]
      ),
  CombinedInterestAIPolicy =
    c(
      AIPolicyInterest[which(df$Timestamp < "2024-02-27")],
      InterestAIPolicy[which(df$Timestamp > "2024-02-27")]
      )
  ) %>%
  select(-c(EthicalAIInterest, InterestEthicalAI, AIPolicyInterest, InterestAIPolicy))

print(ncol(df))

# ----- Tidying step: Change typeof is list columns to not be -------

df %<>% mutate(across(starts_with(
    "HowUseful"
), as.character)) %>%
    unnest(starts_with("HowUseful"), keep_empty = TRUE)

# ----- Tidying step: replace certain strings with NA ------

df %<>% mutate(Timestamp = as.factor(Timestamp)) #must change type from double so next replacements run without error

df[df == 'n/a'] <- NA
df[df == 'NULL'] <- NA

print(unique(df$CombinedInterestAIPolicy))

print(table(df$HowUsefulMetricminer))

# --------- Render web pages from Rmd docs ---------

rmarkdown::render_site(
  input = "pages",
  envir = new.env(parent = globalenv()) # enable use of 'df' inside the Rmd being rendered
)
