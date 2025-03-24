# Cal-ref: Matching EIA refineries to NEI 
# vthivier@uottawa.ca
# created: 09/07/2022
# updated: 24/03/2025

# set up environment

`%notin%` <- Negate(`%in%`)

NA_empty <- c("",NA,"NA")

# Packages

#RODBC doesn't load anymore
# packages <- c("rstudioapi", "xlsx", "gdata", "openxlsx", "cowplot", "janitor", "rvest",
#               "dplyr","tidyr", "stringr", "ggplot2", "data.table", 
#               "concordance", "stargazer", "plm", "readr", "haven", "janitor",
#               "stringr", "lfe", "readxl", "utilities", "tictoc", "sandwich",
#               "lfe", "stringdist", "plm", "benchmarkme","babynames","msm", "purrr",
#               "estimatr","backports","fixest","modelsummary","DescTools")

packages <- c("rstudioapi", "xlsx", "gdata", "openxlsx", "cowplot", "janitor", "rvest",
              "dplyr","tidyr", "stringr", "ggplot2", "data.table", 
              "concordance", "stargazer", "plm", "readr", "haven", "janitor",
              "stringr", "lfe", "readxl", "sandwich",
              "lfe", "stringdist", "plm", "babynames","msm", "purrr",
              "backports","fixest","modelsummary")

for (i in packages) {
  if (!(i %in% installed.packages())) {
    install.packages(i,dependencies = TRUE,repos="http://mirror1.csvd.census.gov/CRAN")
  }
  if (!(i %in% .packages())) {
    library(i, character.only = TRUE)
  }
}

