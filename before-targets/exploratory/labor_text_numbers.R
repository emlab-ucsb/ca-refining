
###### CHECK LABOR OUTPUTS

rm(list=ls())

list.of.packages <- c("dplyr","data.table","lubridate","tidyr","readxl","fixest","modelsummary","flextable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

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


#setwd('C:/Users/mall0065/Dropbox/calepa/refining-labor')
setwd('~/Library/CloudStorage/GoogleDrive-cmalloy@ucsb.edu/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures')

### define function for "not in" 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)

# IMPORT DISAGGREGATED RESULTS 

df <- fread('2022-12-update/fig-csv-files/state_labor_levels_fig_gaps_pmil_inputs.csv')
str(df)

df.2020 <- filter(df, year==2020)
