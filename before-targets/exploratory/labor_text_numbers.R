
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

df <- fread('2024-08-update/fig-csv-files/labor_high_low_annual_outputs.csv') 
str(df)
summary(df$sum_demo_emp)
summary(df$sum_demo_emp_revised)

# IMPORT COUNTY LEVEL RESULTS

df.county <- fread('2024-08-update/fig-csv-files/labor_county_outputs.csv')
str(df.county)

# CALCULATE PERCENT REDUCTION RELATIVE TO BAU 

## AGGREGATE IMPACTS ACROSS DEMOGRAPHIC GROUPS AND YEARS, SELECT 1 DEMOGRAPHIC CATEGORY SO IMPACTS ARENT DUPLICATED
df <- pivot_wider(df, 
                  id_cols=c("scenario","demo_cat","demo_group","demand_scenario","refining_scenario","oil_price_scenario","year"),
                  names_from = c("metric_name","estimate"),
                  values_from = value) %>% 
  filter(oil_price_scenario=="reference case")
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
  mutate(perc_emp = 1-sum_demo_emp/sum_demo_emp_bau,
         perc_emp_revised = 1-sum_demo_emp_revised/sum_demo_emp_revised_bau,
         perc_comp_h = 1-sum_demo_comp_pv_h/sum_demo_comp_pv_h_bau,
         perc_comp_l = 1-sum_demo_comp_pv_l/sum_demo_comp_pv_l_bau,
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
                 demo_cat=="Poverty" & year==2045) %>% 
  group_by(scenario,year,demo_group) %>% 
  summarize(sum_demo_emp = sum(employment_pmil_high),
            sum_demo_emp_revised = sum(employment_pmil_low),
            sum_demo_comp_pv_h = sum(compensation_pv_pmil_high),
            sum_demo_comp_pv_l = sum(compensation_pv_pmil_low)) %>% 
  ungroup()

df.bau2 <- filter(df,
                  demo_cat=="Poverty" & 
                    scenario=="BAU demand - historical production" & year==2045) %>% 
  group_by(year,demo_group) %>% 
  summarize(sum_demo_emp_bau = sum(employment_pmil_high),
            sum_demo_emp_revised_bau = sum(employment_pmil_low),
            sum_demo_comp_pv_h_bau = sum(compensation_pv_pmil_high),
            sum_demo_comp_pv_l_bau = sum(compensation_pv_pmil_low)) %>% 
  ungroup() 



# TREND IN EMPLOYMENT IMPACT DIFFERENCE BETWEEN DAC AND NON-DAC
df.year <- filter(df.agg2,
                  scenario != "BAU demand - historical production") %>% 
  left_join(df.bau2, by=c("year","demo_group")) %>% 
  mutate(gap_emp = sum_demo_emp-sum_demo_emp_bau,
         gap_emp_revised = sum_demo_emp_revised-sum_demo_emp_revised_bau,
         gap_comp_h = sum_demo_comp_pv_h-sum_demo_comp_pv_h_bau,
         gap_comp_l = sum_demo_comp_pv_l-sum_demo_comp_pv_l_bau) 


fig1 <- filter(df.year, scenario=="Low demand - low exports") %>% 
  ggplot(aes(y=gap_emp,x=year,group=demo_group)) + 
  geom_line(aes(color=demo_group))
fig1


## COMPARE TO FIG 4 CSV FILE 

df.f4 <- fread('2022-12-update/fig-csv-files/state_labor_levels_fig_gaps_inputs.csv')

# AVERAGE ANNUAL JOB LOSSES (DAN AND NON-DAC) SHOULD THIS BE ADJUSTED FOR FIRST FEW YRS?

df.agg2 <- filter(df.agg2,
                  scenario != "BAU demand - historical production") %>% 
  left_join(df.bau2, by=c("year","demo_group")) %>% 
  mutate(gap_emp = sum_demo_emp-sum_demo_emp_bau,
         gap_emp_revised = sum_demo_emp_revised-sum_demo_emp_revised_bau,
         gap_comp_h = sum_demo_comp_pv_h-sum_demo_comp_pv_h_bau,
         gap_comp_l = sum_demo_comp_pv_l-sum_demo_comp_pv_l_bau) %>%
  group_by(scenario,demo_group) %>% 
  summarize(gap_emp = mean(gap_emp),
            gap_emp_revised = mean(gap_emp_revised),
            gap_comp_h = mean(gap_comp_h),
            gap_comp_l = mean(gap_comp_l)) %>%
  ungroup() 




# IMPORT DISAGGREGATED RESULTS WITH 3 DIFFERENT OIL PRICE PATHWAYS 

df <- fread('2024-08-update/fig-csv-files/labor_high_low_annual_outputs.csv') 
str(df)
summary(df$sum_demo_emp)
summary(df$sum_demo_emp_revised)


# KEEP JUST 2045 AND COMPUTE GAP BETWEEN SCENARIO AND BAU FOR EACH OIL PRICE PATHWAY 

df <- filter(df,year==2045) 

## AGGREGATE IMPACTS ACROSS DEMOGRAPHIC GROUPS AND YEARS, SELECT 1 DEMOGRAPHIC CATEGORY SO IMPACTS ARENT DUPLICATED
df <- pivot_wider(df, 
                  id_cols=c("scenario","demo_cat","demo_group","demand_scenario","refining_scenario","oil_price_scenario","year"),
                  names_from = c("metric_name","estimate"),
                  values_from = value) %>% 
  filter(demo_cat=="DAC") %>% 
  group_by(scenario,oil_price_scenario,year) %>% 
  summarize(compensation_pv_pmil_high = sum(compensation_pv_pmil_high),
            compensation_pv_pmil_low  = sum(compensation_pv_pmil_low), 
            employment_pmil_high = sum(employment_pmil_high),
            employment_pmil_low = sum(employment_pmil_low)) %>% 
  ungroup()

## BAU 

df.bau <- filter(df, scenario=="BAU demand - historical production") %>% 
  rename(compensation_pv_pmil_high_bau = compensation_pv_pmil_high,
         compensation_pv_pmil_low_bau = compensation_pv_pmil_low,
         employment_pmil_high_bau =  employment_pmil_high, 
         employment_pmil_low_bau =  employment_pmil_low) %>% 
  dplyr::select(-scenario)


## JOIN BAU TO DF AND COMPUTE GAPS 

df <- filter(df, scenario != "BAU demand - historical production") %>% 
  left_join(df.bau,by=c("oil_price_scenario","year")) %>% 
  mutate(gap_comp_pv_pmil_high = compensation_pv_pmil_high - compensation_pv_pmil_high_bau, 
         gap_comp_pv_pmil_low = compensation_pv_pmil_low - compensation_pv_pmil_low_bau, 
         gap_emp_pmil_high = employment_pmil_high - employment_pmil_high_bau, 
         gap_emp_pmil_low = employment_pmil_low - employment_pmil_low_bau)
 

## NEEDS: COMP AND EMP IN 2019 (FROM IMPLAN), COMP AND EMP IN 2045 HIGH AND LOW ESTIMATES FOR EACH SCENARIO

## FROM IMPLAN: SPREADSHEETS ARE AT /Dropbox/calepa/refining-labor/2019-baseline-ica
fte.per.emp <- 0.991150442477876

direct.emp.2019 <- 11456.6380044314*fte.per.emp
indirect.emp.2019 <- 70118.2031857474*fte.per.emp
induced.emp.2019 <- 43395.822784118*fte.per.emp
total.emp.2019 <- direct.emp.2019+indirect.emp.2019+induced.emp.2019

direct.comp.2019 <- 3028965833
indirect.comp.2019 <- 4812737405.89
induced.comp.2019 <- 2273210360.05
total.comp.2019 <- direct.comp.2019+indirect.comp.2019+induced.comp.2019 


## READ IN IMPACTS 

# IMPORT DISAGGREGATED RESULTS ADD ACROSS COUNTIES

df <- fread('2024-08-update/fig-csv-files/labor_result_for_review.csv') %>% 
  filter(oil_price_scenario == "reference case") %>% 
  group_by(demand_scenario,refining_scenario,year) %>% 
  summarize(total_emp = sum(total_emp),
            total_emp_revised = sum(total_emp_revised),
            total_comp_usd19_h = sum(total_comp_usd19_h),
            total_comp_usd19_l = sum(total_comp_usd19_l)) %>% 
  ungroup()
str(df)


# KEEP JUST 2045 AND COMPUTE 1-SHARE OF 2019 EMPLOMENT OR COMPENSATION FOR W/REEMPLOYMENT (HIGH)

df.2045 <- filter(df,year==2045) 

df.high <- mutate(df.2045,
                  emp_perc_h = 1-(total_emp/total.emp.2019),
                  emp_comp_h = 1-(total_comp_usd19_h/total.comp.2019))



# DIRECT,INDIRECT,INDUCED SHARES RELATIVE TO 2019 

df <- fread('2024-08-update/fig-csv-files/labor_result_x_impact_type.csv')
str(df)
unique(df$oil_price_scenario)

df.review <- fread('2024-08-update/fig-csv-files/labor_result_x_impact_type_for_review.csv')
str(df.review)
unique(df.review$oil_price_scenario)

df <- filter(df,
             oil_price_scenario=="reference case") %>% 
  group_by(demand_scenario,refining_scenario,impact_type,year) %>% 
  summarize(total_emp = sum(total_emp),
            total_emp_revised = sum(total_emp_revised),
            total_comp_usd19_h = sum(total_comp_usd19_h),
            total_comp_usd19_l = sum(total_comp_usd19_l)) %>% 
  ungroup() 

# KEEP JUST 2045 AND COMPUTE 1-SHARE OF 2019 EMPLOMENT OR COMPENSATION FOR W/REEMPLOYMENT (HIGH)

df.2045 <- filter(df,year==2045) 

df.high <- mutate(df.2045,
                  emp_perc_h = ifelse(impact_type=="direct",1-(total_emp/direct.emp.2019),
                                      ifelse(impact_type=="indirect",1-(total_emp/indirect.emp.2019),
                                             ifelse(impact_type=="induced",1-(total_emp/induced.emp.2019),NA))),
                  comp_perc_h = ifelse(impact_type=="direct",1-(total_comp_usd19_h/direct.comp.2019),
                                      ifelse(impact_type=="indirect",1-(total_comp_usd19_h/indirect.comp.2019),
                                             ifelse(impact_type=="induced",1-(total_comp_usd19_h/induced.comp.2019),NA))))

  
