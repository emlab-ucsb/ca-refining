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

#PATH.CM <- '/Users/chrismalloy/Library/CloudStorage/GoogleDrive-cmalloy@ucsb.edu/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/labor/ncomms-revisions'
PATH.CM <- '~/Dropbox/ou/ncomms-revisions'
setwd(PATH.CM)

### define function for "not in"
"%!in%" <- function(x, y) !("%in%"(x, y))

# Check that string doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z*-]", x)


# EMPLOYMENT MULTIPLIERS (FROM IMPLAN)
#multipliers/statewide/output-value/
rev.statewide <- fread('implan-multipliers/20250423-statewide_ca_refining-Detail Economic Indicators_2020dollars_OUTPUT.csv') %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation) %>% 
  rename(emp.rev = Employment, 
         ec.rev = EmployeeCompensation)
#multipliers/statewide/labor-income/
li.statewide <- fread('implan-multipliers/20250423-statewide_ca_refining-Detail Economic Indicators_2020dollars_LI.csv') %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation) %>% 
  rename(emp.li = Employment, 
         ec.li = EmployeeCompensation)
statewide <- left_join(rev.statewide,li.statewide, by=c("OriginRegion","DestinationRegion","IndustryCode","ImpactType"))

rm(rev.statewide,li.statewide)

# IMPLAN JOB-YEARS TO FTE CONVERSION 

fte <- read_excel('Emp_FTE and W&S_EC_546 Industry Scheme.xlsx',sheet="2022",skip=1) %>% 
  dplyr::select(Implan546Index,ECtoWSInc,FTEperTotalEmp) 

## CONVERT JOB-YEARS TO FTE JOB-YEARS AND COMPENSATION TO WAGE AND SALARY INCOME FOR LABOR INCOME MULTIPLIER
statewide <- left_join(statewide,fte,by=c("IndustryCode"="Implan546Index")) %>% 
  mutate(emp.rev = emp.rev*FTEperTotalEmp,
         emp.li = emp.li*FTEperTotalEmp,
         ec.li = ec.li/ECtoWSInc, 
         emp.li = emp.li/ECtoWSInc) %>% 
  dplyr::select(-FTEperTotalEmp,-ECtoWSInc)


### ADD UP WORKERS FROM BLOCK TO TRACT LEVEL

df.tract <- fread('ca_od_main_JT02_2019.csv') %>%
  mutate(w_tract_geocode = substr(as.character(w_geocode),1,10), 
         h_tract_geocode = substr(as.character(h_geocode),1,10)) %>% 
  filter(h_tract_geocode != "6037980004" & h_tract_geocode != "6037980030" & h_tract_geocode != "6111980000") %>% 
  dplyr::select(w_tract_geocode, h_tract_geocode,S000,SI01,SI02,SI03) %>%
  group_by(w_tract_geocode,h_tract_geocode) %>% 
  summarize(S000 = sum(S000,na.rm=TRUE),
            SI01 = sum(SI01,na.rm=TRUE),
            SI02 = sum(SI02,na.rm=TRUE),
            SI03 = sum(SI03,na.rm=TRUE)) %>%
  ungroup() 

df.tract <- mutate(df.tract,
                   w_tract_geocode = str_pad(as.character(w_tract_geocode),11,pad="0"),
                   h_tract_geocode = str_pad(as.character(h_tract_geocode),11,pad="0"))



###########################################################################

# DIRECT IMPACTS--IMPLAN MULTIPLIER * SHARE OF OUTPUT AT EACH REFINERY CENSUS TRACT * LODES SHARES AT TRACT LEVEL
## IMPLAN MULTIPLIERS
statewide.direct <- filter(statewide,ImpactType=="Direct")

## SHARE WORKERS IN EACH REFINERY CENSUS TRACT LIVING IN RESIDENTIAL CENSUS TRACT R

w.tract.total <- group_by(df.tract,
                          w_tract_geocode) %>% 
  summarize(S000_w = sum(S000,na.rm=TRUE),
            SI01_w = sum(SI01,na.rm=TRUE)) %>% 
  ungroup()

wh.tract.share <- left_join(df.tract,w.tract.total,by="w_tract_geocode") %>% 
  mutate(S000_share = ifelse(S000_w > 0, S000/S000_w, 0),
         SI01_share = ifelse(SI01_w > 0, SI01/SI01_w, 0)) %>% 
  filter(w_tract_geocode == "06037980002" |
           w_tract_geocode == "06037980030" |
           w_tract_geocode == "06013378000" |
           w_tract_geocode == "06095252102" |
           w_tract_geocode == "06029002401" |
           w_tract_geocode == "06037980014" |
           w_tract_geocode == "06037980005" |
           w_tract_geocode == "06029000507" |
           w_tract_geocode == "06013320001" |
           w_tract_geocode == "06013315000" |
           w_tract_geocode == "06013358000" |
           w_tract_geocode == "06037980015" |
           w_tract_geocode == "06079012306" | 
           w_tract_geocode == "06037553502" |
           w_tract_geocode == "06029000507") %>%
  dplyr::select(w_tract_geocode,h_tract_geocode,S000_share,SI01_share) %>% 
  mutate(refinery_name = ifelse(w_tract_geocode == "06037980002","Marathon Carson",
                                ifelse(w_tract_geocode == "06037980030","Chevron El Segundo",
                                       ifelse(w_tract_geocode == "06013378000","Chevron Richmond",
                                              ifelse(w_tract_geocode == "06095252102","Valero Benicia",
                                                     ifelse(w_tract_geocode == "06029002401","Kern Oil Bakersfield",
                                                            ifelse(w_tract_geocode == "06037980014","Valero Wilmington",
                                                                   ifelse(w_tract_geocode == "06037980005","PBF Torrance",
                                                                          ifelse(w_tract_geocode == "06029000507","San Joaquin Bakersfield",
                                                                                 ifelse(w_tract_geocode == "06013320001","PBF Martinez",
                                                                                        ifelse(w_tract_geocode == "06013315000","Marathon Golden Eagle",
                                                                                               ifelse(w_tract_geocode == "06013358000","Phillips 66 Rodeo",
                                                                                                      ifelse(w_tract_geocode == "06037980015","Phillips 66 Wilmington",
                                                                                                             ifelse(w_tract_geocode == "06079012306","Phillips 66 Santa Maria",
                                                                                                                    ifelse(w_tract_geocode == "06037553502","AltAir Paramount",
                                                                                                                           ifelse(w_tract_geocode == "06029000507","Global Clean Energy",NA))))))))))))))),
         cluster = ifelse((refinery_name=="Marathon Carson" | refinery_name=="Chevron El Segundo" | refinery_name=="Valero Wilmington" | 
                             refinery_name=="PBF Torrance" | refinery_name=="Phillips 66 Wilmington" | refinery_name=="AltAir Paramount"),"south","north")) %>% 
  filter(SI01_share>0) 
summary(wh.tract.share$S000_share)
summary(wh.tract.share$SI01_share)


## DIRECT MULTIPLIERS BY TRACT
direct.multipliers <- wh.tract.share 
direct.multipliers$emp.rev <- statewide.direct$emp.rev
direct.multipliers$ec.rev <- statewide.direct$ec.rev

direct.multipliers <- mutate(direct.multipliers,
                             emp.rev = emp.rev*SI01_share,
                             ec.rev = ec.rev*SI01_share) %>% 
  group_by(w_tract_geocode,h_tract_geocode) %>% 
  summarize(emp.rev = sum(emp.rev),
            ec.rev = sum(ec.rev),
            cluster = first(cluster)) %>% 
  ungroup()

summary(direct.multipliers$emp.rev)
summary(direct.multipliers$ec.rev)


## ADD MULTIPLIERS ACROSS INDUSTRIES TO THE STATE X IMPACT TYPE LEVEL
statewide.indir.indu <- filter(statewide, ImpactType != "Direct") %>% 
  group_by(DestinationRegion,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev),
            ec.rev = sum(ec.rev),
            emp.li = sum(emp.li),
            ec.li = sum(ec.li)) %>% 
  ungroup() 

test <- group_by(wh.tract.share,
                 w_tract_geocode,refinery_name) %>% 
  summarize(S000_share = sum(S000_share),
            SI01_share = sum(SI01_share))
summary(test$SI01_share)

# EXPORT FILES TO CSV FOR TRACEY 

fwrite(direct.multipliers,'direct_multipliers_tract_2019.csv')
fwrite(statewide.indir.indu,'indirect_induced_multipliers_state.csv')


