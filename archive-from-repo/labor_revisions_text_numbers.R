###### NCOMMS REVISIONS--in-text numbers 

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

PATH.CM <- '~/Dropbox/ca-refining/outputs/rev-submission/cuf=0.6_beta-scenario=main/'
setwd(PATH.CM)

### define function for "not in"
"%!in%" <- function(x, y) !("%in%"(x, y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)


# Direct impacts: Figure 1 
df <- fread('./results/figures/figure-1/fig1_labor_inputs.csv')
str(df)

filter(df,
             demand_scenario=="BAU" & refining_scenario=="historic production" & oil_price_scenario=="reference case" & product_scenario=="2020 prices" & is.na(year)==FALSE) %>% 
  group_by() %>% 
  summarize(value = sum(value)/9130)


# Overall impact (direct + indirect + induced): Figure 3 
df <- fread('./intermediate/labor/state_annual_labor_outputs.csv') %>% 
  filter(product_scenario=="2020 prices" & indirect_induced_scenario=="bartik-corrected" & oil_price_scenario=="reference case") 

df %>% 
  filter((year==2020 | year==2045)) %>% 
  dplyr::select(demand_scenario,refining_scenario,product_scenario,indirect_induced_scenario,oil_price_scenario,year,empl_all_impacts_h,state_comp_PV_h)
# reference case emp reduced by 7,100, comp by 687 million

# Total impacts relative to reference 
bau <- filter(df,
              demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  dplyr::select(year,emp_all_impacts_l,comp_all_impacts_PV_l,comp_all_impacts_PV_h,state_comp_PV_l,state_emp_l,state_comp_PV_h) %>% 
  rename(bau_emp_all_impacts_l = emp_all_impacts_l,
         bau_comp_all_impacts_PV_l = comp_all_impacts_PV_l,
         bau_state_comp_PV_l = state_comp_PV_l,
         bau_state_comp_PV_h = state_comp_PV_h,
         bau_state_emp_l = state_emp_l,
         bau_comp_all_impacts_PV_h = comp_all_impacts_PV_h)

left_join(df,bau,by=c("year")) %>% 
  mutate(emp_rel_l = emp_all_impacts_l - bau_emp_all_impacts_l,
         comp_pv_rel_l = comp_all_impacts_PV_l - bau_comp_all_impacts_PV_l,
         comp_pv_rel_h = comp_all_impacts_PV_h - bau_comp_all_impacts_PV_h,
         dir_emp_rel_l = state_emp_l - bau_state_emp_l,
         dir_comp_pv_rel_h = state_comp_PV_h-bau_state_comp_PV_h,
         dir_comp_pv_rel_l = state_comp_PV_l-bau_state_comp_PV_l) %>% 
  dplyr::select(demand_scenario,refining_scenario,year,emp_all_impacts_l,bau_emp_all_impacts_l,emp_rel_l,empl_all_impacts_h,comp_all_impacts_PV_l,comp_all_impacts_PV_h,bau_comp_all_impacts_PV_l,comp_pv_rel_l,dir_emp_rel_l,dir_comp_pv_rel_l,dir_comp_pv_rel_h,comp_pv_rel_h) %>% 
  group_by(demand_scenario,refining_scenario) %>% 
  summarize(emp_rel_l = sum(emp_rel_l),
            comp_pv_rel_l = sum(comp_pv_rel_l),
            dir_emp_rel_l = sum(dir_emp_rel_l),
            dir_comp_pv_rel_l = sum(dir_comp_pv_rel_l),
            dir_comp_pv_rel_h = sum(dir_comp_pv_rel_h),
            comp_pv_rel_h = sum(comp_pv_rel_h))

# Total cumulative NPV compensation losses in reference scenario 
df %>% 
  filter(demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  mutate(f1.comp_all_impacts_PV_h = lag(comp_all_impacts_PV_h,n=1),
         f1.comp_all_impacts_PV_h = ifelse(year==2020,0,f1.comp_all_impacts_PV_h),
         level_comp_all_impacts_PV_l = f1.comp_all_impacts_PV_h+comp_all_impacts_PV_l,
         f1.state_comp_PV_h = lag(state_comp_PV_h,n=1),
         f1.state_comp_PV_h = ifelse(year==2020,0,f1.state_comp_PV_h),
         level_state_comp_PV_l = f1.state_comp_PV_h+state_comp_PV_l) %>% 
  group_by() %>% 
  summarize(comp_all_impacts_PV_h = sum(comp_all_impacts_PV_h),
            level_comp_all_impacts_PV_l = sum(level_comp_all_impacts_PV_l),
            comp_all_impacts_PV_l = sum(comp_all_impacts_PV_l),
            state_comp_PV_h = sum(state_comp_PV_h),
            level_state_comp_PV_l = sum(level_state_comp_PV_l),
            state_comp_PV_l = sum(state_comp_PV_l)) %>% 
  ungroup()

rm(bau,df)

# County-level numbers (direct impacts only)
df <- fread('./tables/labor/labor_county_outputs.csv') %>% 
  filter(product_scenario=="2020 prices" & oil_price_scenario=="reference case" & demo_cat=="DAC") 
  
str(df)

bau <- filter(df,
              demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  dplyr::select(demo_cat,demo_group,county,metric_name,estimate,value) %>% 
  rename(bau_value = value)

df2 <- left_join(df,bau,by=c("demo_cat","demo_group","county","metric_name","estimate")) %>% 
  mutate(gap_value = value-bau_value) %>% 
  group_by(demand_scenario,refining_scenario,county,metric_name,estimate) %>% 
  summarize(gap_value = sum(gap_value))

ttl_impact <- group_by(df2,
                       demand_scenario,refining_scenario,metric_name,estimate) %>% 
  filter(gap_value < 0) %>% 
  summarize(ttl_value = sum(gap_value)) %>% 
  ungroup()

df2 %>% 
  filter(gap_value<0) %>% 
  left_join(ttl_impact,by=c("demand_scenario","refining_scenario","metric_name","estimate")) %>% 
  mutate(share = gap_value/ttl_value) %>% 
  filter(metric_name=="compensation_pv" & estimate=="low" & demand_scenario=="LC1" & refining_scenario=="low exports") %>% 
  arrange(-share) %>%
  print(n=57)

# top 2 counties 
0.479+0.125
# next 8 counties
0.096+0.0841+0.0574+0.0359+0.0244+0.0194+0.0075+0.0067
# remaining compensation losses 
1-(0.479+0.125+0.096+0.0841+0.0574+0.0359+0.0244+0.0194+0.0075+0.0067)


# county-level results: share of direct compensation impact over total county compensation 
## import IMPLAN zip code tables, sum compensation to county level

load.implan.zip <- function(x){
  df <- fread(paste0("~/Dropbox/ou/ncomms-revisions/implan-zip/",x)) 
  names(df) <- make.names(names(df))
  separated.string <- str_split(x,"_")
  
  df <- dplyr::select(df,
                      Description,Employee.Compensation) %>% 
    filter(Description != "" & Description != "* Employment and payroll of federal govt, military" & Description != "* Employment and payroll of federal govt, non-military" & Description=="Petroleum refineries") %>% 
    mutate(zip = str_extract(separated.string[[1]][4],pat <- "(\\d)+"),
           county = separated.string[[1]][3],
           Employee.Compensation = as.numeric(str_remove_all(Employee.Compensation,"[$,]"))) %>% 
    group_by(county) %>% 
    summarize(ec_ttl = sum(Employee.Compensation,na.rm=TRUE)) %>% 
    ungroup()
}

file.names <- list.files(path="~/Dropbox/ou/ncomms-revisions/implan-zip",pattern="*.csv")

output <- lapply(file.names,load.implan.zip) %>% 
  bind_rows() %>% 
  group_by(county) %>% 
  summarize(ec_ttl = sum(ec_ttl,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(county = str_remove_all(county," County"))
summary(output$ec_ttl)

output %>% 
  group_by() %>% 
  summarize(ec_ttl = sum(ec_ttl))

filter(df2, gap_value<0) %>% 
  filter(metric_name=="compensation_pv" & estimate=="low" & demand_scenario=="LC1" & refining_scenario=="low exports") %>%
  left_join(output,by=c("county")) %>% 
  mutate(share = abs(gap_value)/ec_ttl) %>% 
  arrange(-share) %>%
  print(n=57)

rm(df,df2,ttl_impact,bau,output,file.names)

# Demographic numbers 

## Figure 4 
df <- fread('./tables/labor/labor_high_low_annual_outputs.csv')
str(df)

df <- filter(df,
             product_scenario=="2020 prices" & oil_price_scenario=="reference case" & metric_name != "") 



### difference with bau 
bau <- filter(df,
              demand_scenario == "BAU" & refining_scenario=="historical production") %>%
  rename(bau_value = value) %>%
  dplyr::select(demo_cat,demo_group,metric_name,estimate,bau_value,year)

df_pmil <- filter(df,
             scenario != "BAU demand - historical production") %>%
  left_join(bau, by=c("demo_cat","demo_group","metric_name","estimate","year")) %>%
  mutate(gap_value = value-bau_value) %>% 
  group_by(demand_scenario,refining_scenario,demo_cat,demo_group,metric_name,estimate) %>% 
  summarize(min_value = min(gap_value),
            max_value = max(gap_value),
            gap_value = sum(gap_value)) %>% 
  ungroup() %>% 
  filter(metric_name == "employment_pmil" & estimate=="high" & demo_cat=="Race")
print(df_pmil,n=60)


# test difference in overall outputs
df2 <- fread('~/Downloads/labor_high_low_annual_outputs(1).csv') %>% 
  filter(product_scenario=="2020 prices" & oil_price_scenario=="reference case" & metric_name != "")

test <- left_join(df,df2,by=c("demo_cat","demo_group","scenario","demand_scenario","refining_scenario","product_scenario","oil_price_scenario","year","metric_name","estimate")) %>% 
  mutate(gap = value-value_temp) %>% 
  filter(estimate=="low" & metric_name=="employment_pmil")
summary(test$gap)

bau2 <- filter(df2,
               demand_scenario == "BAU" & refining_scenario=="historical production") %>%
  rename(bau_value = value) %>%
  dplyr::select(demo_cat,demo_group,metric_name,estimate,bau_value,year)

df_pmil2 <- filter(df2,
                   scenario != "BAU demand - historical production") %>%
  left_join(bau2, by=c("demo_cat","demo_group","metric_name","estimate","year")) %>%
  mutate(gap_value = value-bau_value) %>% 
  group_by(demand_scenario,refining_scenario,demo_cat,demo_group,metric_name,estimate) %>% 
  summarize(min_value = min(gap_value),
            max_value = max(gap_value),
            gap_value = sum(gap_value)) %>% 
  ungroup() %>% 
  filter(metric_name == "employment_pmil" & estimate=="high" & demo_cat=="Race")
print(df_pmil2,n=60)


temp <- left_join(df_pmil,df_pmil2,by=c("demand_scenario","refining_scenario","demo_cat","demo_group","metric_name","estimate")) %>% 
  mutate(gap = gap_value.x-gap_value.y,
         share.x = gap/gap_value.x,
         share.y = gap/gap_value.y)
summary(temp$gap)
summary(temp$share.x)
summary(temp$share.y)

rm(df_pmil)

## figure 5: NPV per capita for each scenario
df_npvpc <-  filter(df,
                    scenario != "BAU demand - historical production") %>%
  left_join(bau, by=c("demo_cat","demo_group","metric_name","estimate","year")) %>%
  mutate(gap_value = value-bau_value) %>% 
  group_by(demand_scenario,refining_scenario,demo_cat,demo_group,metric_name,estimate) %>% 
  summarize(gap_value = sum(gap_value)) %>% 
  ungroup() %>% 
  filter(metric_name == "compensation_pv_pc" & estimate=="low" & demo_cat=="DAC")
print(df_npvpc,n=60)

## check difference with 2019 commuting patterns 
df_npvpc2 <-  filter(df2,
                    scenario != "BAU demand - historical production") %>%
  left_join(bau, by=c("demo_cat","demo_group","metric_name","estimate","year")) %>%
  mutate(gap_value = value-bau_value) %>% 
  group_by(demand_scenario,refining_scenario,demo_cat,demo_group,metric_name,estimate) %>% 
  summarize(gap_value = sum(gap_value)) %>% 
  ungroup() %>% 
  filter(metric_name == "compensation_pv_pc" & estimate=="low" & demo_cat=="DAC")
print(df_npvpc,n=60)

temp <- left_join(df_npvpc,df_npvpc2,by=c("demand_scenario","refining_scenario","demo_cat","demo_group","metric_name","estimate")) %>% 
  mutate(gap = gap_value.x-gap_value.y,
         share.x = gap/gap_value.x,
         share.y = gap/gap_value.y)
summary(temp$gap)
summary(temp$share.x)
summary(temp$share.y)


# calculate impact share by demo_cat 

group_impact <- filter(df,
                     scenario != "BAU demand - historical production") %>%
  left_join(bau, by=c("demo_cat","demo_group","metric_name","estimate","year")) %>%
  mutate(gap_value = value-bau_value) %>% 
  group_by(demand_scenario,refining_scenario,demo_cat,demo_group,metric_name,estimate) %>% 
  summarize(gap_value = sum(gap_value)) %>% 
  ungroup() %>% 
  filter(metric_name == "compensation_pv" & estimate=="low")

ttl_impact <- filter(group_impact,
                     demo_cat=="DAC") %>% 
  group_by(demand_scenario,refining_scenario,metric_name,estimate) %>% 
  summarize(ttl_gap_value = sum(gap_value)) %>% 
  ungroup() 

group_impact <- left_join(group_impact,ttl_impact,by=c("demand_scenario","refining_scenario","metric_name","estimate")) %>% 
  mutate(share = gap_value/ttl_gap_value)
print(group_impact,n=60)

rm(group_impact,ttl_impact,df_npvpc,df_pmil)

# check the increase in job years relative to reference for non-DAC, above-poverty line, white hispanic and asian

df_emp <- filter(df,
                  scenario != "BAU demand - historical production") %>%
  left_join(bau, by=c("demo_cat","demo_group","metric_name","estimate","year")) %>%
  mutate(gap_value = value-bau_value) %>% 
  filter(year <= 2026 & metric_name=="employment" & estimate=="high")

df_product <- fread('~/Downloads/indiv_prod_output.csv') %>% 
  filter(year>=2020 & year<=2026 & demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  group_by(demand_scenario,refining_scenario,site_id,refinery_name,year) %>% 
  summarize(value = sum(value)) %>% 
  mutate(prev_value = lag(value,n=1),
         delta_value = value - prev_value) %>% 
  #filter(delta_value > 0) %>% 
  ungroup() 
str(df_product)
unique(df_product$refinery_name)


# check the relative reduction in compensation for low vs high estimate 
df <- fread('./results/figures/figure-3/state_npv_fig_inputs_labor.csv') %>% 
  filter(metric=="forgone_wages_bil" & indirect_induced_scenario=="bartik-corrected" & product_scenario=="2020 prices")
print(df1)




















# Import disaggregated results by year, demo group, state 
#df <- fread('./results/figures/figure-4/state_labor_levels_fig_gaps_pmil_inputs.csv')

df <- fread('./intermediate/labor/state_annual_labor_outputs.csv')
df <- fread('~/Downloads/state_annual_labor_outputs(1).csv') 

str(df)

filter(df,
       product_scenario=="2020 prices" & oil_price_scenario=="reference case" & (metric_name=="compensation_pv" | metric_name=="employment") & estimate=="low" & demo_cat=="DAC") %>% 
  group_by(demand_scenario,refining_scenario,metric_name) %>% 
  summarize(value = sum(value)) %>% 
  mutate(value = ifelse(metric_name=="compensation_pv", value - 1143228362, value - 9841))

df2 <- filter(df, 
             product_scenario=="2020 prices" & indirect_induced_scenario=="bartik-corrected" & oil_price_scenario=="reference case") 

# direct impacts in 2020--figure 1 
filter(df2,  year==2020 & demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  dplyr::select(state_comp_PV_l,state_emp_h)

# total impacts in 2020 
filter(df2,  year==2020 & demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  dplyr::select(comp_all_impacts_PV_l,empl_all_impacts_h)

# cumulative job losses in each scenario between 2020-2045 
filter(df2,  (year==2045 | year==2020)) %>% 
  dplyr::select(demand_scenario,refining_scenario,year,comp_all_impacts_PV_h,empl_all_impacts_h) 

# cumulative impacts relative to reference 
## actual figure inputs 
test <- fread('./results/figures/figure-3/state_npv_fig_inputs_labor.csv')

bau <- filter(df2,
              product_scenario=="2020 prices" & indirect_induced_scenario=="bartik-corrected" & oil_price_scenario=="reference case" & demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  dplyr::select(year,emp_all_impacts_l,comp_all_impacts_PV_l) %>% 
  rename(bau_emp_all_impacts_l = emp_all_impacts_l,
         bau_comp_all_impacts_PV_l = comp_all_impacts_PV_l)

df3 <- left_join(df2,bau,by=c("year")) %>% 
  mutate(emp_rel_l = emp_all_impacts_l - bau_emp_all_impacts_l,
         comp_pv_rel_l = comp_all_impacts_PV_l - bau_comp_all_impacts_PV_l) %>% 
  dplyr::select(demand_scenario,refining_scenario,year,emp_all_impacts_l,bau_emp_all_impacts_l,emp_rel_l,empl_all_impacts_h,comp_all_impacts_PV_l,comp_all_impacts_PV_h,bau_comp_all_impacts_PV_l,comp_pv_rel_l) %>% 
  group_by(demand_scenario,refining_scenario) %>% 
  summarize(emp_rel_l = sum(emp_rel_l),
            comp_pv_rel_l = sum(comp_pv_rel_l))


df4 <- fread('~/Downloads/state_labor_levels_fig_gaps_pmil_inputs_old.csv') %>% 
  filter(product_scenario=="2020 prices" & oil_price_scenario=="reference case" & demo_cat=="DAC") %>% 
  group_by(demand_scenario,refining_scenario) %>% 
  summarize(sum_demo_comp_pv_l = sum(sum_demo_comp_pv_l),
            sum_demo_comp_pv_h = sum(sum_demo_comp_pv_h),
            sum_demo_emp_revised = sum(sum_demo_emp_revised),
            sum_demo_emp = sum(sum_demo_emp)) %>% 
  ungroup()
str(df4)

df5 <- fread('~/Downloads/state_labor_levels_fig_gaps_pmil_inputs_new.csv') %>% 
  filter(product_scenario=="2020 prices" & oil_price_scenario=="reference case" & demo_cat=="DAC") %>% 
  group_by(demand_scenario,refining_scenario) %>% 
  summarize(gap_emp = sum(gap_emp),
            sum_demo_comp_pv_l = sum(sum_demo_comp_pv_l),
            sum_demo_comp_pv_h = sum(sum_demo_comp_pv_h),
            sum_demo_emp_revised = sum(sum_demo_emp_revised),
            sum_demo_emp = sum(sum_demo_emp)) %>% 
  ungroup()
str(df5)

df6 <- fread('~/Downloads/state_levels_labor_pmil_fig_inputs.csv') %>% 
  filter(product_scenario=="2020 prices" & oil_price_scenario=="reference case" & demo_cat=="DAC") %>% 
  group_by(demand_scenario,refining_scenario) %>% 
  summarize(#gap_emp = sum(gap_emp),
            sum_demo_comp_pv_l = sum(sum_demo_comp_pv_l),
            sum_demo_comp_pv_h = sum(sum_demo_comp_pv_h),
            #sum_demo_emp_revised = sum(sum_demo_emp_revised),
            sum_demo_emp = sum(sum_demo_emp)) %>% 
  ungroup()
str(df6)








# filter to 2020 prices, low estimates, 2019 pv, labor only 
df <- filter(df,
             segment=="labor" & metric != "forgone_wages_h" & product_scenario=="2020 prices") %>%
  group_by(demand_scenario,refining_scenario,metric,demo_cat) %>%
  summarize(value = sum(value)) %>%
  ungroup()
print(df,n=40)

# intermediate file for debugging 
t1 <- fread('~/Downloads/step_8_output_for_review.csv')
str(t1)
unique(is.na(t1$state_comp_all_impacts_l))
test2 <- filter(t1, is.na(state_comp_emp_li)==TRUE & year != 2020 & oil_price_scenario=="reference case" & product_scenario=="2020 prices")

t2 <- fread('~/Downloads/step_8_output_for_review (5).csv')
test3 <- filter(t2, 
                indirect_induced_scenario=="bartik-corrected" & product_scenario=="2020 prices" & oil_price_scenario=="reference case" & demand_scenario=="BAU" & refining_scenario=="historic exports")
# 
# df <- filter(df,segment=="health") %>%
#   group_by(demand_scenario,refining_scenario,metric,demo_cat) %>%
#   summarize(value = sum(value)) %>%
#   ungroup()
# 

# # 



df <- filter(df,
             product_scenario=="2020 prices" & oil_price_scenario=="reference case" & (metric_name == "compensation_pv" | metric_name=="employment")) %>% 
  group_by(demand_scenario,refining_scenario,demo_cat,metric_name,estimate,year) %>% 
  summarize(value = sum(value)) %>% 
  ungroup()

bau <- filter(df,
              demand_scenario == "BAU" & refining_scenario=="historical production") %>%
  rename(bau_value = value) %>% 
  dplyr::select(-demand_scenario, -refining_scenario)

pop_df <- read_excel('~/Downloads/Intercensal_Excel_California.xlsx',
                     sheet="California-State") %>% 
  dplyr::select(Sex,`Race/ethnicity recode`,`Age (0-100+)`, `July 1, 2019`) 
names(pop_df) <- make_clean_names(names(pop_df))

pop_df <- group_by(pop_df,
                   race_ethnicity_recode) %>% 
  summarize(pop = sum(july_1_2019)) %>% 
  ungroup() 
pop_df

pop_white <- pop_df$pop[7]
pop_black <- pop_df$pop[3]
pop_am_ind <- pop_df$pop[1]
pop_asian <- pop_df$pop[2]
pop_aapi <- pop_df$pop[6]
pop_more2 <- pop_df$pop[5]
pop_hispanic <- pop_df$pop[4]

df <- filter(df,
             demand_scenario != "BAU" & refining_scenario != "historical production") %>% 
  left_join(bau, by=c("demo_cat","metric_name","estimate","year")) %>% 
  mutate(gap_value = value - bau_value) %>% 
  group_by(demand_scenario,refining_scenario,demo_cat,metric_name,estimate) %>% 
  summarize(gap_value = sum(gap_value)) %>% 
  ungroup() %>% 
  mutate(white = (0.53*gap_value)/pop_white, 
         black = (0.07*gap_value)/pop_black,
         am_ind = (0.002*gap_value)/pop_am_ind,
         asian = (0.12*gap_value)/pop_asian,
         aapi = (0.006*gap_value)/pop_aapi,
         more_2 = (0.02*gap_value)/pop_more2,
         hispanic = (0.25*gap_value)/pop_hispanic)
print(df,n=30)





