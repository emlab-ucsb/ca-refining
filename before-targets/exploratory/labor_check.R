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
#library(ggsn)


#setwd('C:/Users/mall0065/Dropbox/Downloads')
setwd('~/Downloads')

### define function for "not in" 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)


# READ IN LABOR IMPACTS 

df <- fread('labor_result_for_review.csv')
str(df)

## CALCULATE DIFFERENCE FROM BAU AND AGGREGATE 

df.agg <- group_by(df,
                   demand_scenario,refining_scenario,year) %>% 
  summarize(total_comp_usd19_h = sum(total_comp_usd19_h,na.rm = TRUE),
            total_comp_usd19_l =  sum(total_comp_usd19_l,na.rm = TRUE),
            total_emp = sum(total_emp,na.rm = TRUE),
            total_emp_revised =  sum(total_emp_revised,na.rm = TRUE)) %>% 
  ungroup()
summary(df.agg$total_comp_usd19_h)
summary(df.agg$total_comp_usd19_l)
summary(df.agg$total_emp)
summary(df.agg$total_emp_revised)


df.bau <- filter(df,
                 demand_scenario=="BAU" & refining_scenario=="historic exports") %>% 
  group_by(demand_scenario,refining_scenario,year) %>% 
  summarize(total_comp_usd19_h = sum(total_comp_usd19_h,na.rm = TRUE),
            total_comp_usd19_l =  sum(total_comp_usd19_l,na.rm = TRUE),
            total_emp = sum(total_emp,na.rm = TRUE),
            total_emp_revised =  sum(total_emp_revised,na.rm = TRUE)) %>% 
  rename(total_comp_usd19_h_bau = total_comp_usd19_h,
         total_comp_usd19_l_bau = total_comp_usd19_l,
         total_emp_bau = total_emp,
         total_emp_revised_bau = total_emp_revised) %>% 
  ungroup() %>% 
  dplyr::select(-demand_scenario,-refining_scenario)


df.agg <- left_join(df.agg,df.bau,by=c("year")) %>% 
  mutate(total_comp_usd19_h = total_comp_usd19_h-total_comp_usd19_h_bau,
         total_comp_usd19_l = total_comp_usd19_l-total_comp_usd19_l_bau,
         total_emp = total_emp-total_emp_bau,
         total_emp_revised = total_emp_revised-total_emp_revised_bau)

summary(df.agg$total_comp_usd19_h)
summary(df.agg$total_comp_usd19_l)
summary(df.agg$total_emp)
summary(df.agg$total_emp_revised)
