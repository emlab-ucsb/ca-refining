
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


#setwd('G:/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures')
setwd('~/Library/CloudStorage/GoogleDrive-cmalloy@ucsb.edu/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures')

### define function for "not in" 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)

# IMPORT DISAGGREGATED RESULTS 

df <- fread('2022-12-update/fig-csv-files/labor_high_low_annual_outputs.csv') 
str(df)
summary(df$sum_demo_emp)
summary(df$sum_demo_emp_revised)

# IMPORT COUNTY LEVEL RESULTS

df.county <- fread('2022-12-update/fig-csv-files/labor_county_outputs.csv')
str(df.county)

# CALCULATE PERCENT REDUCTION RELATIVE TO BAU 

## AGGREGATE IMPACTS ACROSS DEMOGRAPHIC GROUPS AND YEARS, SELECT 1 DEMOGRAPHIC CATEGORY SO IMPACTS ARENT DUPLICATED
df <- pivot_wider(df, 
                  id_cols=c("scenario","demo_cat","demo_group","demand_scenario","refining_scenario","year"),
                  names_from = c("metric_name","estimate"),
                  values_from = value)
str(df)

df.agg <- filter(df,
                 demo_cat=="DAC") %>% 
  group_by(scenario,year) %>% 
  summarize(sum_demo_emp = sum(employment_high),
            sum_demo_emp_revised = sum(employment_low),
            sum_demo_comp_pv_h = sum(compensation_pv_high),
            sum_demo_comp_pv_l = sum(compensation_pv_low)) %>% 
  ungroup()

## SUBTRACT BAU FROM SCENARIO IMPACT AND DIVIDE BY BAU FOR PERCENT REDUCTION 

df.lc1 <- filter(df.agg,
                 scenario=="Low demand - low exports")

df.bau <- filter(df.agg,
                 scenario=="BAU demand - historical production") %>% 
  rename(sum_demo_emp_bau = sum_demo_emp,
         sum_demo_emp_revised_bau = sum_demo_emp_revised,
         sum_demo_comp_pv_h_bau = sum_demo_comp_pv_h,
         sum_demo_comp_pv_l_bau = sum_demo_comp_pv_l) %>% 
  dplyr::select(-scenario)

df.agg <- filter(df.agg,
                 scenario != "BAU demand - historical production") %>% 
  left_join(df.bau, by="year") %>% 
  group_by(scenario) %>% 
  summarize(sum_demo_emp = sum(sum_demo_emp),
            sum_demo_emp_revised = sum(sum_demo_emp_revised),
            sum_demo_comp_pv_h = sum(sum_demo_comp_pv_h),
            sum_demo_comp_pv_l = sum(sum_demo_comp_pv_l),
            sum_demo_emp_bau = sum(sum_demo_emp_bau),
            sum_demo_emp_revised_bau = sum(sum_demo_emp_revised_bau),
            sum_demo_comp_pv_h_bau = sum(sum_demo_comp_pv_h_bau),
            sum_demo_comp_pv_l_bau = sum(sum_demo_comp_pv_l_bau)) %>%
  ungroup() %>%
  mutate(perc_emp = sum_demo_emp/sum_demo_emp_bau,
         perc_emp_revised = sum_demo_emp_revised/sum_demo_emp_revised_bau,
         perc_comp_h = sum_demo_comp_pv_h/sum_demo_comp_pv_h_bau,
         perc_comp_l = sum_demo_comp_pv_l/sum_demo_comp_pv_l_bau,
         gap_emp = sum_demo_emp-sum_demo_emp_bau,
         gap_emp_revised = sum_demo_emp_revised-sum_demo_emp_revised_bau,
         gap_comp_h = sum_demo_comp_pv_h-sum_demo_comp_pv_h_bau,
         gap_comp_l = sum_demo_comp_pv_l-sum_demo_comp_pv_l_bau)

summary(df.agg$perc_emp)
summary(df.agg$perc_emp_revised)
summary(df.agg$perc_comp_h)
summary(df.agg$perc_comp_l)


# CALCULATE SHARE OF TOTAL IMPACT FROM EACH COUNTY 

df.total <- group_by(df.county,
                     demand_scenario,refining_scenario,metric_name,estimate) %>% 
  summarize(total_value = sum(value)) %>% 
  ungroup()

df.county.agg <- group_by(df.county,
                          county,demand_scenario,refining_scenario,metric_name,estimate) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() 

df.county.agg <- left_join(df.county.agg,df.total,by=c("demand_scenario","refining_scenario","metric_name","estimate")) %>%
  mutate(share = value/total_value) %>% 
  arrange(demand_scenario,refining_scenario,metric_name,estimate,-share)
summary(df.county.agg$share)


# AVERAGE ANNUAL JOB LOSSES (DAN AND NON-DAC) SHOULD THIS BE ADJUSTED FOR FIRST FEW YRS?

df.agg2 <- filter(df,
                 demo_cat=="DAC") %>% 
  group_by(scenario,year) %>% 
  summarize(sum_demo_emp = sum(employment_high),
            sum_demo_emp_revised = sum(employment_low),
            sum_demo_comp_pv_h = sum(compensation_pv_high),
            sum_demo_comp_pv_l = sum(compensation_pv_low)) %>% 
  ungroup()



df.agg2 <- filter(df.agg2,
                 scenario != "BAU demand - historical production") %>% 
  left_join(df.bau, by="year") %>% 
  mutate(gap_emp = sum_demo_emp-sum_demo_emp_bau,
         gap_emp_revised = sum_demo_emp_revised-sum_demo_emp_revised_bau,
         gap_comp_h = sum_demo_comp_pv_h-sum_demo_comp_pv_h_bau,
         gap_comp_l = sum_demo_comp_pv_l-sum_demo_comp_pv_l_bau) %>%
  group_by(scenario) %>% 
  summarize(gap_emp = mean(gap_emp),
            gap_emp_revised = mean(gap_emp_revised),
            gap_comp_h = mean(gap_comp_h),
            gap_comp_l = mean(gap_comp_l)) %>%
  ungroup() 
  

summary(df.agg$gap_emp)
summary(df.agg$gap_emp_revised)
summary(df.agg$gap_comp_h)
summary(df.agg$gap_comp_l)

