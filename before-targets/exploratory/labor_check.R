
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
setwd('~/Dropbox/calepa/refining-labor')

### define function for "not in" 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)


# READ IN LABOR MULTIPLIERS 

multipliers <- fread('20240605-census_regions-Detail Economic Indicators.csv') 

multipliers <- janitor::clean_names(multipliers)

multipliers <- multipliers[, .(employment = sum(employment),
                               emp_comp = sum(employee_compensation)), .(origin_region, destination_region, impact_type)]

multipliers[, impact_type := tolower(impact_type)]

multipliers[, county := str_remove(origin_region, " County, CA Group")]

setnames(multipliers, c("origin_region", "destination_region"), c("origin","destination"))

multipliers <- multipliers[, .(county, origin, destination, impact_type, employment, emp_comp)]

str(multipliers)

multipliers %>% 
  filter(destination=="Los Angeles County, CA") %>% 
  summary()

multipliers %>% 
  filter(destination != "Los Angeles County, CA") %>% 
  summary()

# READ IN LABOR IMPACTS 

df <- fread('labor_result_for_review (2).csv')
str(df)

## CALCULATE DIFFERENCE FROM BAU AND AGGREGATE 

df.agg <- group_by(df,
                   demand_scenario,refining_scenario,year) %>% 
  summarize(total_comp_usd19_h = sum(total_comp_usd19_h,na.rm = TRUE),
            total_comp_usd19_l =  sum(total_comp_usd19_l,na.rm = TRUE),
            total_emp = sum(total_emp,na.rm = TRUE),
            total_emp_revised =  sum(total_emp_revised,na.rm = TRUE),
            total_revenue = sum(total_revenue,na.rm=TRUE),
            total_production_bbl = sum(total_production_bbl,na.rm=TRUE)) %>% 
  ungroup()
summary(df.agg$total_comp_usd19_h)
summary(df.agg$total_comp_usd19_l)
summary(df.agg$total_emp)
summary(df.agg$total_emp_revised)


df.bau <- filter(df,
                 demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  group_by(demand_scenario,refining_scenario,year) %>% 
  summarize(total_comp_usd19_h = sum(total_comp_usd19_h,na.rm = TRUE),
            total_comp_usd19_l =  sum(total_comp_usd19_l,na.rm = TRUE),
            total_emp = sum(total_emp,na.rm = TRUE),
            total_emp_revised =  sum(total_emp_revised,na.rm = TRUE),
            total_revenue = sum(total_revenue,na.rm=TRUE),
            total_production_bbl = sum(total_production_bbl,na.rm=TRUE)) %>% 
  rename(total_comp_usd19_h_bau = total_comp_usd19_h,
         total_comp_usd19_l_bau = total_comp_usd19_l,
         total_emp_bau = total_emp,
         total_emp_revised_bau = total_emp_revised,
         total_revenue_bau = total_revenue,
         total_production_bbl_bau = total_production_bbl) %>% 
  ungroup() %>% 
  dplyr::select(-demand_scenario,-refining_scenario)

df.county.bau <- filter(df,
                        demand_scenario=="BAU" & refining_scenario=="historic production") %>% 
  group_by(demand_scenario,refining_scenario,year,destination) %>% 
  summarize(total_comp_usd19_h = sum(total_comp_usd19_h,na.rm = TRUE),
            total_comp_usd19_l =  sum(total_comp_usd19_l,na.rm = TRUE),
            total_emp = sum(total_emp,na.rm = TRUE),
            total_emp_revised =  sum(total_emp_revised,na.rm = TRUE),
            total_revenue = sum(total_revenue,na.rm=TRUE),
            total_production_bbl = sum(total_production_bbl,na.rm=TRUE)) %>% 
  rename(total_comp_usd19_h_bau = total_comp_usd19_h,
         total_comp_usd19_l_bau = total_comp_usd19_l,
         total_emp_bau = total_emp,
         total_emp_revised_bau = total_emp_revised,
         total_revenue_bau = total_revenue,
         total_production_bbl_bau = total_production_bbl) %>% 
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
summary(df.agg$total_revenue)
summary(df.agg$total_production_bbl)

df.county <- left_join(df,df.county.bau,by=c("destination","year")) %>% 
  mutate(total_comp_usd19_h = total_comp_usd19_h-total_comp_usd19_h_bau,
         total_comp_usd19_l = total_comp_usd19_l-total_comp_usd19_l_bau,
         total_emp = total_emp-total_emp_bau,
         total_emp_revised = total_emp_revised-total_emp_revised_bau,
         total_revenue_gap = total_revenue-total_revenue_bau,
         total_production_bbl_gap = total_production_bbl - total_production_bbl_bau)

### PATHWAYS

#### FIG 1: COMPENSATION LOWER BOUND
fig1 <- filter(df.agg, 
               demand_scenario=="LC1" & refining_scenario=="low exports") %>%
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
               demand_scenario=="LC1" & refining_scenario=="low exports") %>%
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


## FIG 4: FTE JOBS BY COUNTY

fig4 <- filter(df.county, 
               demand_scenario=="LC1" & refining_scenario=="low exports") %>%
  ggplot(aes(y=total_emp_revised/1000, x=year, color=destination)) + 
  geom_line(size=1, aes(color=destination)) +
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
fig4

ggsave('emp_check.png',fig4,width = 10,height=5,units = "in",dpi=300,bg="white")   


## FIG 5: Compensation BY COUNTY

fig5 <- filter(df.county, 
               demand_scenario=="LC1" & refining_scenario=="low exports") %>%
  ggplot(aes(y=total_comp_usd19_l/1000000, x=year, color=destination)) + 
  geom_line(size=1, aes(color=destination)) +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(-60,60)) +
  labs(y="Compensation (Millions)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig5

ggsave('comp_check.png',fig5,width = 10,height=5,units = "in",dpi=300,bg="white")   


#### FIG 6: COMPENSATION LOWER BOUND
fig6 <- filter(df.county, 
               demand_scenario=="LC1" & refining_scenario=="low exports" & year>2022 & destination=="Siskiyou County, CA") %>%
  ggplot(aes(y=total_comp_usd19_l/1000000, x=year)) + 
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
fig6       

#### FIG 7: FTE JOBS 
fig7 <- filter(df.county, 
               demand_scenario=="LC1" & refining_scenario=="historic exports") %>%
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
fig7 


#### FIG 8: REVENUE
fig8 <- filter(df.county, 
               demand_scenario=="LC1" & refining_scenario=="low exports") %>%
  ggplot(aes(y=total_revenue_gap/10^9, x=year,color=destination)) + 
  geom_line(size=1, aes(color=destination)) +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(-60,60)) +
  labs(y="Revenue (Billions of Dollars, Difference with BAU)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig8

ggsave('revenue_check.png',fig8,width = 10,height=5,units = "in",dpi=300,bg="white")   

#### FIG 9: PRODUCTION GAP

fig9 <- filter(df.county, 
               demand_scenario=="LC1" & refining_scenario=="low exports") %>%
  ggplot(aes(y=total_production_bbl_gap/10^9, x=year,color=destination)) + 
  geom_line(size=1, aes(color=destination)) +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(-60,60)) +
  labs(y="Total Production (Billions of Barrels, Difference with BAU)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig9

#### FIG 10: PRODUCTION LEVEL
fig10 <- filter(df.agg, 
                demand_scenario=="LC1" & refining_scenario=="historic exports") %>%
  ggplot(aes(y=total_production_bbl_bau/10^9, x=year)) + 
  geom_line(size=1, color="#841617") +
  geom_line(aes(y=total_production_bbl/10^9),size=1, color="blue") +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(0,20)) +
  labs(y="Production (Billions of Barrels)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig10


#### FIG 11: REVENUE GAP, JUST LA
fig11 <- filter(df.county, 
                demand_scenario=="LC1" & refining_scenario=="low exports" & destination=="Los Angeles") %>%
  ggplot(aes(y=total_revenue/10^9, x=year)) + 
  geom_line(size=1, color="#841617") +
  geom_line(aes(y=total_revenue_bau/10^9),size=1, color="blue") +
  theme_cowplot(12) +
  scale_x_continuous(limits = c(2020, 2045)) + 
  #scale_y_continuous(limits = c(-60,60)) +
  labs(y="Revenue (Billions of Dollars)", x = "Year",color="",linetype="") +
  theme(panel.background = element_blank(),
        legend.position = "top", legend.justification = "center",
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
fig11

