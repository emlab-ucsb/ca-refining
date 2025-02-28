###### NCOMMS REVISIONS 

rm(list = ls())

list.of.packages <- c("dplyr", "data.table", "lubridate", "tidyr", "readxl", "fixest", "modelsummary", "flextable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

library(lubridate)
library(tidyr)
library(dplyr)
library(data.table)
library(readxl)
library(stringr)
library(readr)
library(fixest)
library(modelsummary)
library(flextable)
library(svglite)
library(sf)
library(ggplot2)
library(tigris)
library(cowplot)
library(janitor)


# setwd('G:/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures')
setwd("~/Dropbox/ou/ncomms-revisions")

### define function for "not in"
"%!in%" <- function(x, y) !("%in%"(x, y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)


# IMPORT LODES DATA 

df <- fread('ca_od_main_JT02_2020.csv') %>%
  filter(w_geocode == 60379800021006 |
           w_geocode == 60379800301000 |
           w_geocode == 60133780001024 |
           w_geocode == 60952521022025 |
           w_geocode == 60290024011034 |
           w_geocode == 60379800141083 |
           w_geocode == 60379800051007 |
           w_geocode == 60290005072025 |
           w_geocode == 60133200011067 |
           w_geocode == 60133150001075 |
           w_geocode == 60133580005003 |
           w_geocode == 60379800151000 |
           w_geocode == 60790123061000 | 
           w_geocode == 60375535021000 |
           w_geocode == 60290005072077) %>%
  dplyr::select(w_geocode,h_geocode,S000,SI01) %>% 
  mutate(refinery_name = ifelse(w_geocode == 60379800021006,"Marathon Carson",
                                ifelse(w_geocode == 60379800301000,"Chevron El Segundo",
                                       ifelse(w_geocode == 60133780001024,"Chevron Richmond",
                                              ifelse(w_geocode == 60952521022025,"Valero Benicia",
                                                     ifelse(w_geocode == 60290024011034,"Kern Oil Bakersfield",
                                                            ifelse(w_geocode == 60379800141083,"Valero Wilmington",
                                                                   ifelse(w_geocode == 60379800051007,"PBF Torrance",
                                                                          ifelse(w_geocode == 60290005072025,"San Joaquin Bakersfield",
                                                                                 ifelse(w_geocode == 60133200011067,"PBF Martinez",
                                                                                        ifelse(w_geocode == 60133150001075,"Marathon Golden Eagle",
                                                                                               ifelse(w_geocode == 60133580005003,"Phillips 66 Rodeo",
                                                                                                      ifelse(w_geocode == 60379800151000,"Phillips 66 Wilmington",
                                                                                                              ifelse(w_geocode == 60790123061000,"Phillips 66 Santa Maria",
                                                                                                                     ifelse(w_geocode == 60375535021000,"AltAir Paramount",
                                                                                                                            ifelse(w_geocode == 60290005072077,"Global Clean Energy",NA))))))))))))))))
str(df)
summary(df$SI01)
summary(df$S000)

df.refinery <- group_by(df,refinery_name) %>% 
  summarize(SI01 = sum(SI01), 
            S000 = sum(S000)) %>% 
  ungroup()
summary(df.refinery$SI01)
summary(df.refinery$S000)



