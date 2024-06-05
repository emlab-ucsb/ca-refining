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
setwd('~/Downloads')

### define function for "not in" 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)


# READ IN LABOR MULTIPLIERS 

multipliers <- fread('20240524-1million_la-Detail Economic Indicators.csv') 

multipliers <- janitor::clean_names(multipliers)

multipliers <- multipliers[, .(employment = sum(employment),
             emp_comp = sum(employee_compensation)), .(origin_region, destination_region, impact_type)]

multipliers[, impact_type := tolower(impact_type)]

multipliers[, county := str_remove(origin_region, " County, CA Group")]

setnames(multipliers, c("origin_region", "destination_region"), c("origin","destination"))

multipliers <- multipliers[, .(county, origin, destination, impact_type, employment, emp_comp)]
  
str(multipliers)

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



### PATHWAYS

#### FIG 1: COMPENSATION LOWER BOUND
fig1 <- filter(df.agg, 
               demand_scenario=="BAU" & refining_scenario=="low exports" & year>2020) %>%
  ggplot(aes(y=total_comp_usd19_l/1000, x=year)) + 
  geom_line(size=1, color="#841617") +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(0,20)) +
  labs(y="Compensation (2019 USD)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig1       

#### FIG 2: COMPENSATION UPPER BOUND
fig2 <- filter(df.agg, 
               demand_scenario=="LC1" & refining_scenario=="low exports") %>%
  ggplot(aes(y=total_comp_usd19_h/1000, x=year)) + 
  geom_line(size=1, color="#841617") +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(0,20)) +
  labs(y="Compensation (2019 USD)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig2       

#### FIG 3: FTE JOBS 
fig3 <- filter(df.agg, 
               demand_scenario=="LC1" & refining_scenario=="historic exports" & year>2020) %>%
  ggplot(aes(y=total_emp_revised/1000, x=year)) + 
  geom_line(size=1, color="#841617") +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(-60,60)) +
  labs(y="Labor FTE Job-years (Thousands)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig3  

