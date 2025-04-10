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


# EMPLOYMENT MULTIPLIERS (FROM IMPLAN)
rev.mrio <- fread('multipliers/mrio/output-value/20250402-ns_cluster_mrio-Detail Economic Indicators.csv') %>% 
  mutate(cluster = ifelse(DestinationRegion=="north_cluster (2023)","north","south")) %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation,cluster) %>% 
  rename(emp.rev = Employment, 
         ec.rev = EmployeeCompensation)
li.mrio <- fread('multipliers/mrio/labor-income/20250402-ns_cluster_mrio-Detail Economic Indicators.csv') %>% 
  mutate(cluster = ifelse(DestinationRegion=="north_cluster (2023)","north","south")) %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation,cluster) %>% 
  rename(emp.li = Employment, 
         ec.li = EmployeeCompensation)
mrio <- left_join(rev.mrio,li.mrio, by=c("OriginRegion","DestinationRegion","IndustryCode","ImpactType","cluster"))

rev.not.mrio <- fread('multipliers/non-mrio/output-value/20250402-ns_cluster-Detail Economic Indicators.csv') %>% 
  mutate(cluster = ifelse(DestinationRegion=="north_cluster (2023)","north","south")) %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation,cluster) %>% 
  rename(emp.rev = Employment, 
         ec.rev = EmployeeCompensation)
li.not.mrio <- fread('multipliers/non-mrio/labor-income/20250402-ns_cluster-Detail Economic Indicators.csv') %>% 
  mutate(cluster = ifelse(DestinationRegion=="north_cluster (2023)","north","south")) %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation,cluster) %>% 
  rename(emp.li = Employment, 
         ec.li = EmployeeCompensation)
not.mrio <- left_join(rev.not.mrio,li.not.mrio, by=c("OriginRegion","DestinationRegion","IndustryCode","ImpactType","cluster"))

rev.statewide <- fread('multipliers/statewide/output-value/20250402-statewide_ca_refining-Detail Economic Indicators.csv') %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation) %>% 
  rename(emp.rev = Employment, 
         ec.rev = EmployeeCompensation)
li.statewide <- fread('multipliers/statewide/labor-income/20250402-statewide_ca_refining-Detail Economic Indicators.csv') %>% 
  dplyr::select(OriginRegion,DestinationRegion,IndustryCode,ImpactType,Employment,EmployeeCompensation) %>% 
  rename(emp.li = Employment, 
         ec.li = EmployeeCompensation)
statewide <- left_join(rev.statewide,li.statewide, by=c("OriginRegion","DestinationRegion","IndustryCode","ImpactType"))

rm(rev.mrio,li.mrio,rev.not.mrio,li.not.mrio,rev.statewide,li.statewide)


### ADD UP WORKERS FROM BLOCK TO TRACT LEVEL

df.tract <- fread('ca_od_main_JT02_2020.csv') %>%
  mutate(w_tract_geocode = substr(as.character(w_geocode),1,10), 
         h_tract_geocode = substr(as.character(h_geocode),1,10)) %>% 
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

######################################## 
# Census Tract to Zip Code Xwalk

df <- read_excel('TRACT_ZIP_122024.xlsx') %>% 
  filter(USPS_ZIP_PREF_STATE=="CA") %>% 
  dplyr::select(TRACT,ZIP,BUS_RATIO) 
str(df)
summary(df$BUS_RATIO)


# Crosswalk work census tract to work zip code 

df.zip <- left_join(df.tract,df,by=c("w_tract_geocode"="TRACT")) 

df.zip <- mutate(df.zip, 
              S000 = ifelse(is.na(BUS_RATIO)==FALSE,S000*BUS_RATIO,S000),
              SI01 = ifelse(is.na(BUS_RATIO)==FALSE,SI01*BUS_RATIO,SI01),
              SI02 = ifelse(is.na(BUS_RATIO)==FALSE,SI02*BUS_RATIO,SI02),
              SI03 = ifelse(is.na(BUS_RATIO)==FALSE,SI03*BUS_RATIO,SI03)) %>% 
  filter(is.na(ZIP)==FALSE) %>% 
  group_by(ZIP,h_tract_geocode) %>% 
  summarize(S000 = sum(S000,na.rm=TRUE),
            SI01 = sum(SI01,na.rm=TRUE),
            SI02 = sum(SI02,na.rm=TRUE),
            SI03 = sum(SI03,na.rm=TRUE)) %>% 
  ungroup()

total.zip <- group_by(df.zip,
                      ZIP) %>% 
  summarize(S000_w = sum(S000,na.rm=TRUE),
            SI01_w = sum(SI01,na.rm=TRUE),
            SI02_w = sum(SI02,na.rm=TRUE),
            SI03_w = sum(SI03,na.rm=TRUE)) %>% 
  ungroup()

df.zip <- left_join(df.zip,total.zip, by="ZIP") %>%
  mutate(S000_share = ifelse(S000_w > 0, S000/S000_w, 0),
         SI01_share = ifelse(SI01_w > 0, SI01/SI01_w, 0),
         SI02_share = ifelse(SI02_w > 0, SI02/SI02_w, 0),
         SI03_share = ifelse(SI03_w > 0, SI03/SI03_w, 0))
summary(df.zip$S000_share)
summary(df.zip$SI01_share)
summary(df.zip$SI02_share)
summary(df.zip$SI03_share)

rm(total.zip)

######################################## 
# IMPLAN zip code tables 
# 
# load.implan.zip.agg <- function(x){
#   df <- fread(paste0("implan-zip/",x)) 
#   names(df) <- make.names(names(df))
#   separated.string <- str_split(x,"_")
#   
#   df <- dplyr::select(df,
#                       Industry.Code,Description,Total.Output) %>% 
#     filter(Description != "" & Description != "* Employment and payroll of federal govt, military" & Description != "* Employment and payroll of federal govt, non-military") %>% 
#     mutate(zip = str_extract(separated.string[[1]][4],pat <- "(\\d)+"),
#            county = separated.string[[1]][3],
#            Total.Output = as.numeric(str_remove_all(Total.Output,"[$,]"))) 
#   
# 
#   df <- left_join(df,implan.naics.xwalk, by=c("Industry.Code"="Implan546Index")) %>% 
#     mutate(ind_type = ifelse((substr(as.character(`2017NaicsCode`),1,2)=="11" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="21" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="23" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="31" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="32" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="33" | 
#                                 Description == "Construction of new commercial structures, including farm structures" | 
#                                 Description == "Construction of new multifamily residential structures" | 
#                                 Description == "Construction of other new residential structures" | 
#                                 Description == "Maintenance and repair construction of nonresidential structures" | 
#                                 Description == "Maintenance and repair construction of residential structures" | 
#                                 Description == "Maintenance and repair construction of highways, streets, bridges, and tunnels" | 
#                                 Description == "Dog and cat food manufacturing" | 
#                                 Description == "Other animal food manufacturing" | 
#                                 Description == "Flour milling" | 
#                                 Description == "Rice milling" | 
#                                 Description == "Malt manufacturing" | 
#                                 Description == "Construction of other new nonresidential structures" | 
#                                 Description == "Construction of new single-family residential structures"), "SI01",
#                              ifelse((substr(as.character(`2017NaicsCode`),1,2)=="42" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="44" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="45" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="48" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="49" |
#                                        substr(as.character(`2017NaicsCode`),1,2)=="22"), "SI02","SI03")),
#            ind_type = ifelse(Description=="Veterinary services","SI03",ind_type),
#            cluster = ifelse((county == "Ventura County" | 
#                                county == "Los Angeles County" | 
#                                county == "San Bernardino County" | 
#                                county == "Orange County" |
#                                county == "Riverside County" |
#                                county == "Imperial County" |
#                                county == "San Diego County"), "south", "north")) %>% 
#     dplyr::select(-`2017NaicsCode`,-Description,-Industry.Code,-NaicsDescription) %>% 
#     group_by(zip,county,cluster,ind_type) %>% 
#     summarize(Total.Output = sum(Total.Output,na.rm=TRUE)) %>% 
#     ungroup() %>% 
#     pivot_wider(id_cols=c("zip","county","cluster"),values_from = "Total.Output", names_from = "ind_type",names_prefix = "output_")
#   
# }
# 
# 
# # disaggregated by IMPLAN industry and zip 
# 
# load.implan.zip <- function(x){
#   df <- fread(paste0("implan-zip/",x)) 
#   names(df) <- make.names(names(df))
#   separated.string <- str_split(x,"_")
#   
#   df <- dplyr::select(df,
#                       Industry.Code,Description,Total.Output) %>% 
#     filter(Description != "" & Description != "* Employment and payroll of federal govt, military" & Description != "* Employment and payroll of federal govt, non-military") %>% 
#     mutate(zip = str_extract(separated.string[[1]][4],pat <- "(\\d)+"),
#            county = separated.string[[1]][3],
#            Total.Output = as.numeric(str_remove_all(Total.Output,"[$,]"))) 
#   
#   
#   df <- left_join(df,implan.naics.xwalk, by=c("Industry.Code"="Implan546Index")) %>% 
#     mutate(ind_type = ifelse((substr(as.character(`2017NaicsCode`),1,2)=="11" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="21" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="23" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="31" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="32" | 
#                                 substr(as.character(`2017NaicsCode`),1,2)=="33" | 
#                                 Description == "Construction of new commercial structures, including farm structures" | 
#                                 Description == "Construction of new multifamily residential structures" | 
#                                 Description == "Construction of other new residential structures" | 
#                                 Description == "Maintenance and repair construction of nonresidential structures" | 
#                                 Description == "Maintenance and repair construction of residential structures" | 
#                                 Description == "Maintenance and repair construction of highways, streets, bridges, and tunnels" | 
#                                 Description == "Dog and cat food manufacturing" | 
#                                 Description == "Other animal food manufacturing" | 
#                                 Description == "Flour milling" | 
#                                 Description == "Rice milling" | 
#                                 Description == "Malt manufacturing" | 
#                                 Description == "Construction of other new nonresidential structures" | 
#                                 Description == "Construction of new single-family residential structures"), "SI01",
#                              ifelse((substr(as.character(`2017NaicsCode`),1,2)=="42" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="44" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="45" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="48" | 
#                                        substr(as.character(`2017NaicsCode`),1,2)=="49" |
#                                        substr(as.character(`2017NaicsCode`),1,2)=="22"), "SI02","SI03")),
#            ind_type = ifelse(Description=="Veterinary services","SI03",ind_type),
#            cluster = ifelse((county == "Ventura County" | 
#                                county == "Los Angeles County" | 
#                                county == "San Bernardino County" | 
#                                county == "Orange County" |
#                                county == "Riverside County" |
#                                county == "Imperial County" |
#                                county == "San Diego County"), "south", "north")) %>% 
#     dplyr::select(-`2017NaicsCode`) 
# }
# 
# file.names <- list.files(path="implan-zip",pattern="*.csv")
# implan.naics.xwalk <- read_xlsx("Bridge_2017NaicsToImplan546.xlsx") %>% 
#   dplyr::select(Implan546Index,`2017NaicsCode`) %>% 
#   mutate(`2017NaicsCode` = substr(`2017NaicsCode`,1,2)) %>% 
#   group_by(Implan546Index,`2017NaicsCode`) %>% 
#   summarize() %>% 
#   ungroup()
# 
# 
# output <- lapply(file.names,load.implan.zip) %>% 
#   bind_rows()
# 
# fwrite(output,'implan_zip_output.csv')

## IMPORT ZIP CODE TABLES, CALCULATE SHARE OF CLUSTER OR STATEWIDE OUTPUT ATTRIBUTABLE TO EACH ZIP CODE 
zip.output <- fread('implan_zip_output.csv')

cluster.output <- group_by(zip.output,
                           cluster,Industry.Code) %>% 
  summarize(cluster_output = sum(Total.Output,na.rm=TRUE)) %>% 
  ungroup()

state.output <- group_by(zip.output,
                         Industry.Code) %>% 
  summarize(state_output = sum(Total.Output,na.rm=TRUE)) %>% 
  ungroup()

zip.output <- left_join(zip.output,cluster.output,by=c("cluster","Industry.Code")) %>% 
  mutate(share_output = ifelse(cluster_output > 0, Total.Output/cluster_output,0)) %>% 
  dplyr::select(-cluster_output)
summary(zip.output$share_output)

zip.output <- left_join(zip.output,state.output,by=c("Industry.Code")) %>% 
  mutate(share_output_state = ifelse(state_output > 0, Total.Output/state_output,0)) %>% 
  dplyr::select(-state_output)
summary(zip.output$share_output_state)

rm(cluster.output,state.output)

## JOIN ZIP CODE SHARE OF OUTPUT TO IMPLAN MULTIPLIERS, CALCULATE EFFECT FOR EACH ZIP CODE, ADD UP ACROSS INDUSTRIES

### MRIO 
mrio.multipliers <- left_join(mrio,zip.output,by=c("IndustryCode"="Industry.Code","cluster")) %>% 
  mutate(emp.rev = emp.rev*share_output,
         ec.rev = ec.rev*share_output,
         emp.li = emp.li*share_output,
         ec.li = ec.li*share_output) %>% 
  group_by(OriginRegion,cluster,zip,ImpactType,county,ind_type) %>% 
  summarize(emp.rev = sum(emp.rev, na.rm=TRUE),
            ec.rev = sum(ec.rev, na.rm=TRUE),
            emp.li = sum(emp.li, na.rm=TRUE),
            ec.li = sum(ec.li, na.rm=TRUE)) %>% 
  ungroup()


mrio.multipliers <- pivot_wider(mrio.multipliers,
                    id_cols=c("zip","county","cluster","OriginRegion","ImpactType"),values_from = c("emp.rev","ec.rev","emp.li","ec.li"), names_from = "ind_type") %>% 
  mutate(emp.rev_SI01 = ifelse(is.na(emp.rev_SI01)==TRUE,0,emp.rev_SI01),
         emp.rev_SI02 = ifelse(is.na(emp.rev_SI02)==TRUE,0,emp.rev_SI02),
         emp.rev_SI03 = ifelse(is.na(emp.rev_SI03)==TRUE,0,emp.rev_SI03),
         ec.rev_SI01 = ifelse(is.na(ec.rev_SI01)==TRUE,0,ec.rev_SI01),
         ec.rev_SI02 = ifelse(is.na(ec.rev_SI02)==TRUE,0,ec.rev_SI02),
         ec.rev_SI03 = ifelse(is.na(ec.rev_SI03)==TRUE,0,ec.rev_SI03),
         emp.li_SI01 = ifelse(is.na(emp.li_SI01)==TRUE,0,emp.li_SI01),
         emp.li_SI02 = ifelse(is.na(emp.li_SI02)==TRUE,0,emp.li_SI02),
         emp.li_SI03 = ifelse(is.na(emp.li_SI03)==TRUE,0,emp.li_SI03),
         ec.li_SI01 = ifelse(is.na(ec.li_SI01)==TRUE,0,ec.li_SI01),
         ec.li_SI02 = ifelse(is.na(ec.li_SI02)==TRUE,0,ec.li_SI02),
         ec.li_SI03 = ifelse(is.na(ec.li_SI03)==TRUE,0,ec.li_SI03))

### NOT MRIO 
not.mrio.multipliers <- left_join(not.mrio,zip.output,by=c("IndustryCode"="Industry.Code","cluster")) %>% 
  mutate(emp.rev = emp.rev*share_output,
         ec.rev = ec.rev*share_output,
         emp.li = emp.li*share_output,
         ec.li = ec.li*share_output) %>% 
  group_by(OriginRegion,cluster,zip,ImpactType,county,ind_type) %>% 
  summarize(emp.rev = sum(emp.rev, na.rm=TRUE),
            ec.rev = sum(ec.rev, na.rm=TRUE),
            emp.li = sum(emp.li, na.rm=TRUE),
            ec.li = sum(ec.li, na.rm=TRUE)) %>% 
  ungroup()


not.mrio.multipliers <- pivot_wider(not.mrio.multipliers,
                                id_cols=c("zip","county","cluster","OriginRegion","ImpactType"),values_from = c("emp.rev","ec.rev","emp.li","ec.li"), names_from = "ind_type") %>% 
  mutate(emp.rev_SI01 = ifelse(is.na(emp.rev_SI01)==TRUE,0,emp.rev_SI01),
         emp.rev_SI02 = ifelse(is.na(emp.rev_SI02)==TRUE,0,emp.rev_SI02),
         emp.rev_SI03 = ifelse(is.na(emp.rev_SI03)==TRUE,0,emp.rev_SI03),
         ec.rev_SI01 = ifelse(is.na(ec.rev_SI01)==TRUE,0,ec.rev_SI01),
         ec.rev_SI02 = ifelse(is.na(ec.rev_SI02)==TRUE,0,ec.rev_SI02),
         ec.rev_SI03 = ifelse(is.na(ec.rev_SI03)==TRUE,0,ec.rev_SI03),
         emp.li_SI01 = ifelse(is.na(emp.li_SI01)==TRUE,0,emp.li_SI01),
         emp.li_SI02 = ifelse(is.na(emp.li_SI02)==TRUE,0,emp.li_SI02),
         emp.li_SI03 = ifelse(is.na(emp.li_SI03)==TRUE,0,emp.li_SI03),
         ec.li_SI01 = ifelse(is.na(ec.li_SI01)==TRUE,0,ec.li_SI01),
         ec.li_SI02 = ifelse(is.na(ec.li_SI02)==TRUE,0,ec.li_SI02),
         ec.li_SI03 = ifelse(is.na(ec.li_SI03)==TRUE,0,ec.li_SI03))

### STATEWIDE 

state.multipliers <- left_join(statewide,zip.output,by=c("IndustryCode"="Industry.Code")) %>% 
  mutate(emp.rev = emp.rev*share_output_state,
         ec.rev = ec.rev*share_output_state,
         emp.li = emp.li*share_output_state,
         ec.li = ec.li*share_output_state) %>% 
  group_by(OriginRegion,zip,ImpactType,county,ind_type) %>% 
  summarize(emp.rev = sum(emp.rev, na.rm=TRUE),
            ec.rev = sum(ec.rev, na.rm=TRUE),
            emp.li = sum(emp.li, na.rm=TRUE),
            ec.li = sum(ec.li, na.rm=TRUE)) %>% 
  ungroup()


state.multipliers <- pivot_wider(state.multipliers,
                                id_cols=c("zip","county","OriginRegion","ImpactType"),values_from = c("emp.rev","ec.rev","emp.li","ec.li"), names_from = "ind_type") %>% 
  mutate(emp.rev_SI01 = ifelse(is.na(emp.rev_SI01)==TRUE,0,emp.rev_SI01),
         emp.rev_SI02 = ifelse(is.na(emp.rev_SI02)==TRUE,0,emp.rev_SI02),
         emp.rev_SI03 = ifelse(is.na(emp.rev_SI03)==TRUE,0,emp.rev_SI03),
         ec.rev_SI01 = ifelse(is.na(ec.rev_SI01)==TRUE,0,ec.rev_SI01),
         ec.rev_SI02 = ifelse(is.na(ec.rev_SI02)==TRUE,0,ec.rev_SI02),
         ec.rev_SI03 = ifelse(is.na(ec.rev_SI03)==TRUE,0,ec.rev_SI03),
         emp.li_SI01 = ifelse(is.na(emp.li_SI01)==TRUE,0,emp.li_SI01),
         emp.li_SI02 = ifelse(is.na(emp.li_SI02)==TRUE,0,emp.li_SI02),
         emp.li_SI03 = ifelse(is.na(emp.li_SI03)==TRUE,0,emp.li_SI03),
         ec.li_SI01 = ifelse(is.na(ec.li_SI01)==TRUE,0,ec.li_SI01),
         ec.li_SI02 = ifelse(is.na(ec.li_SI02)==TRUE,0,ec.li_SI02),
         ec.li_SI03 = ifelse(is.na(ec.li_SI03)==TRUE,0,ec.li_SI03))

rm(zip.output)

## JOIN ZIP CODE EFFECTS TO LODES DATA AND MULTIPLY BY THE SHARE OF WORKERS IN EACH ZIP CODE WORKING IN DIFFERENT CENSUS TRACTS 
df.zip <- mutate(df.zip,
                 ZIP = as.numeric(ZIP))

### MRIO
mrio.multipliers <- left_join(mrio.multipliers,df.zip,by=c("zip"="ZIP")) %>% 
  mutate(emp.rev_SI01 = emp.rev_SI01*SI01_share,
         emp.rev_SI02 = emp.rev_SI02*SI02_share,
         emp.rev_SI03 = emp.rev_SI03*SI03_share,
         ec.rev_SI01 = ec.rev_SI01*SI01_share,
         ec.rev_SI02 = ec.rev_SI02*SI02_share,
         ec.rev_SI03 = ec.rev_SI03*SI03_share,
         emp.rev = emp.rev_SI01+emp.rev_SI02+emp.rev_SI03,
         ec.rev = ec.rev_SI01+ec.rev_SI02+ec.rev_SI03,
         emp.li_SI01 = emp.li_SI01*SI01_share,
         emp.li_SI02 = emp.li_SI02*SI02_share,
         emp.li_SI03 = emp.li_SI03*SI03_share,
         ec.li_SI01 = ec.li_SI01*SI01_share,
         ec.li_SI02 = ec.li_SI02*SI02_share,
         ec.li_SI03 = ec.li_SI03*SI03_share,
         emp.li = emp.li_SI01+emp.li_SI02+emp.li_SI03,
         ec.li = ec.li_SI01+ec.li_SI02+ec.li_SI03) %>%
  group_by(OriginRegion,cluster,h_tract_geocode,county,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev,na.rm=TRUE),
            ec.rev = sum(ec.rev,na.rm=TRUE),
            emp.li = sum(emp.li,na.rm=TRUE),
            ec.li = sum(ec.li,na.rm=TRUE)) 

### NOT MRIO 
not.mrio.multipliers <- left_join(not.mrio.multipliers,df.zip,by=c("zip"="ZIP")) %>% 
  mutate(emp.rev_SI01 = emp.rev_SI01*SI01_share,
         emp.rev_SI02 = emp.rev_SI02*SI02_share,
         emp.rev_SI03 = emp.rev_SI03*SI03_share,
         ec.rev_SI01 = ec.rev_SI01*SI01_share,
         ec.rev_SI02 = ec.rev_SI02*SI02_share,
         ec.rev_SI03 = ec.rev_SI03*SI03_share,
         emp.rev = emp.rev_SI01+emp.rev_SI02+emp.rev_SI03,
         ec.rev = ec.rev_SI01+ec.rev_SI02+ec.rev_SI03,
         emp.li_SI01 = emp.li_SI01*SI01_share,
         emp.li_SI02 = emp.li_SI02*SI02_share,
         emp.li_SI03 = emp.li_SI03*SI03_share,
         ec.li_SI01 = ec.li_SI01*SI01_share,
         ec.li_SI02 = ec.li_SI02*SI02_share,
         ec.li_SI03 = ec.li_SI03*SI03_share,
         emp.li = emp.li_SI01+emp.li_SI02+emp.li_SI03,
         ec.li = ec.li_SI01+ec.li_SI02+ec.li_SI03) %>%
  group_by(OriginRegion,cluster,h_tract_geocode,county,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev,na.rm=TRUE),
            ec.rev = sum(ec.rev,na.rm=TRUE),
            emp.li = sum(emp.li,na.rm=TRUE),
            ec.li = sum(ec.li,na.rm=TRUE))

### STATEWIDE 
state.multipliers <- left_join(state.multipliers,df.zip,by=c("zip"="ZIP")) %>% 
  mutate(emp.rev_SI01 = emp.rev_SI01*SI01_share,
         emp.rev_SI02 = emp.rev_SI02*SI02_share,
         emp.rev_SI03 = emp.rev_SI03*SI03_share,
         ec.rev_SI01 = ec.rev_SI01*SI01_share,
         ec.rev_SI02 = ec.rev_SI02*SI02_share,
         ec.rev_SI03 = ec.rev_SI03*SI03_share,
         emp.rev = emp.rev_SI01+emp.rev_SI02+emp.rev_SI03,
         ec.rev = ec.rev_SI01+ec.rev_SI02+ec.rev_SI03,
         emp.li_SI01 = emp.li_SI01*SI01_share,
         emp.li_SI02 = emp.li_SI02*SI02_share,
         emp.li_SI03 = emp.li_SI03*SI03_share,
         ec.li_SI01 = ec.li_SI01*SI01_share,
         ec.li_SI02 = ec.li_SI02*SI02_share,
         ec.li_SI03 = ec.li_SI03*SI03_share,
         emp.li = emp.li_SI01+emp.li_SI02+emp.li_SI03,
         ec.li = ec.li_SI01+ec.li_SI02+ec.li_SI03) %>%
  group_by(OriginRegion,h_tract_geocode,county,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev,na.rm=TRUE),
            ec.rev = sum(ec.rev,na.rm=TRUE),
            emp.li = sum(emp.li,na.rm=TRUE),
            ec.li = sum(ec.li,na.rm=TRUE))


###########################################################################

# DIRECT IMPACTS--IMPLAN MULTIPLIER * SHARE OF OUTPUT AT EACH REFINERY CENSUS TRACT * LODES SHARES AT TRACT LEVEL
## IMPLAN MULTIPLIERS
mrio <- filter(mrio, ImpactType=="Direct")
statewide <- filter(statewide,ImpactType=="Direct")

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
                             refinery_name=="PBF Torrance" | refinery_name=="Phillips 66 Wilmington" | refinery_name=="AltAir Paramount"),"south","north"))
summary(wh.tract.share$S000_share)
summary(wh.tract.share$SI01_share)

mrio <- dplyr::select(mrio,
                      emp.rev,ec.rev,cluster) %>% 
  rename(emp.rev.mrio = emp.rev, 
         ec.rev.mrio = ec.rev)
direct.multipliers <- left_join(wh.tract.share,mrio,by="cluster") 
direct.multipliers$emp.rev.state <- statewide$emp.rev
direct.multipliers$ec.rev.state <- statewide$ec.rev

direct.multipliers <- mutate(direct.multipliers,
                             emp.rev.mrio = emp.rev.mrio*SI01_share,
                             ec.rev.mrio = ec.rev.mrio*SI01_share,
                             emp.rev.state = emp.rev.state*SI01_share,
                             ec.rev.state = ec.rev.state*SI01_share) %>% 
  dplyr::select(-S000_share, -SI01_share)

summary(direct.multipliers$emp.rev.mrio)
summary(direct.multipliers$ec.rev.mrio)
summary(direct.multipliers$emp.rev.state)
summary(direct.multipliers$ec.rev.state)



# COMPARE TO OLD MULTIPLIERS

mrio.multipliers <- filter(mrio.multipliers,
                           ImpactType != "Direct") %>% 
  ungroup() %>% 
  mutate(county = str_remove_all(county," County"),
         region = ifelse((county=="Butte" | county=="Colusa" | county=="El Dorado" | county=="Glenn" | county=="Lassen" | 
                            county=="Modoc" | county=="Nevada" | county=="Placer" | county=="Plumas" | county=="Sacramento" |
                            county=="Shasta" | county=="Sierra" | county=="Siskiyou" | county=="Sutter" | county=="Tehama" |
                            county=="Yolo" | county=="Yuba"),"1",
                          ifelse((county=="Del Norte" | county=="Humboldt" | county=="Lake" | county=="Mendocino" | county=="Napa" |
                                    county=="Sonoma" | county=="Trinity"),"2",
                                 ifelse((county=="Alameda" | county=="Marin" | county=="San Francisco" | county=="San Mateo" | county=="Santa Clara"),"3",
                                        ifelse((county=="Alpine" | county=="Amador" | county=="Calaveras" | county=="Madera" | county=="Mariposa" |
                                                  county=="Merced" | county=="Mono" | county=="San Joaquin" | county=="Stanislaus" | county=="Tuolumne"),"4",
                                               ifelse((county=="Monterey" | county=="San Benito" | county=="Santa Barbara" | county=="Santa Cruz" | county=="Ventura"),"5",
                                                       ifelse((county=="Fresno" | county=="Inyo" | county=="Kings" | county=="Tulare"),"6",
                                                              ifelse((county=="Riverside" | county=="San Bernardino"),"7",
                                                                     ifelse(county=="Orange","9",
                                                                            ifelse((county=="Imperial" | county=="San Diego"),"10",county)))))))))) %>% 
  group_by(region,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev,na.rm = FALSE),
            ec.rev = sum(ec.rev,na.rm = FALSE)) %>% 
  ungroup()
  

not.mrio.multipliers <- filter(not.mrio.multipliers,
                           ImpactType != "Direct") %>% 
  ungroup() %>% 
  mutate(county = str_remove_all(county," County"),
         region = ifelse((county=="Butte" | county=="Colusa" | county=="El Dorado" | county=="Glenn" | county=="Lassen" | 
                            county=="Modoc" | county=="Nevada" | county=="Placer" | county=="Plumas" | county=="Sacramento" |
                            county=="Shasta" | county=="Sierra" | county=="Siskiyou" | county=="Sutter" | county=="Tehama" |
                            county=="Yolo" | county=="Yuba"),"1",
                         ifelse((county=="Del Norte" | county=="Humboldt" | county=="Lake" | county=="Mendocino" | county=="Napa" |
                                   county=="Sonoma" | county=="Trinity"),"2",
                                ifelse((county=="Alameda" | county=="Marin" | county=="San Francisco" | county=="San Mateo" | county=="Santa Clara"),"3",
                                       ifelse((county=="Alpine" | county=="Amador" | county=="Calaveras" | county=="Madera" | county=="Mariposa" |
                                                 county=="Merced" | county=="Mono" | county=="San Joaquin" | county=="Stanislaus" | county=="Tuolumne"),"4",
                                              ifelse((county=="Monterey" | county=="San Benito" | county=="Santa Barbara" | county=="Santa Cruz" | county=="Ventura"),"5",
                                                     ifelse((county=="Fresno" | county=="Inyo" | county=="Kings" | county=="Tulare"),"6",
                                                            ifelse((county=="Riverside" | county=="San Bernardino"),"7",
                                                                   ifelse(county=="Orange","9",
                                                                          ifelse((county=="Imperial" | county=="San Diego"),"10",county)))))))))) %>% 
  group_by(region,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev,na.rm = FALSE),
            ec.rev = sum(ec.rev,na.rm = FALSE)) %>% 
  ungroup()

state.multipliers <- filter(state.multipliers,
                           ImpactType != "Direct") %>% 
  ungroup() %>% 
  mutate(county = str_remove_all(county," County"),
         region = ifelse((county=="Butte" | county=="Colusa" | county=="El Dorado" | county=="Glenn" | county=="Lassen" | 
                            county=="Modoc" | county=="Nevada" | county=="Placer" | county=="Plumas" | county=="Sacramento" |
                            county=="Shasta" | county=="Sierra" | county=="Siskiyou" | county=="Sutter" | county=="Tehama" |
                            county=="Yolo" | county=="Yuba"),"1",
                         ifelse((county=="Del Norte" | county=="Humboldt" | county=="Lake" | county=="Mendocino" | county=="Napa" |
                                   county=="Sonoma" | county=="Trinity"),"2",
                                ifelse((county=="Alameda" | county=="Marin" | county=="San Francisco" | county=="San Mateo" | county=="Santa Clara"),"3",
                                       ifelse((county=="Alpine" | county=="Amador" | county=="Calaveras" | county=="Madera" | county=="Mariposa" |
                                                 county=="Merced" | county=="Mono" | county=="San Joaquin" | county=="Stanislaus" | county=="Tuolumne"),"4",
                                              ifelse((county=="Monterey" | county=="San Benito" | county=="Santa Barbara" | county=="Santa Cruz" | county=="Ventura"),"5",
                                                     ifelse((county=="Fresno" | county=="Inyo" | county=="Kings" | county=="Tulare"),"6",
                                                            ifelse((county=="Riverside" | county=="San Bernardino"),"7",
                                                                   ifelse(county=="Orange","9",
                                                                          ifelse((county=="Imperial" | county=="San Diego"),"10",county)))))))))) %>% 
  group_by(region,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev,na.rm = FALSE),
            ec.rev = sum(ec.rev,na.rm = FALSE)) %>% 
  ungroup()
  

lodes.xwalk <- fread('ca_xwalk.csv') %>% 
  dplyr::select(trct,ctyname) %>% 
  mutate(trct = str_pad(as.character(trct),11,pad="0"),
         ctyname = str_remove_all(ctyname," County,CA"))
str(lodes.xwalk)

## where are new impacts actually felt? all 58 counties 
direct.v2 <- inner_join(direct.multipliers,lodes.xwalk,by=c("h_tract_geocode"="trct")) %>% 
  group_by(ctyname) %>% 
  summarize(emp.rev.mrio = sum(emp.rev.mrio,na.rm = FALSE),
            ec.rev.mrio = sum(ec.rev.mrio,na.rm = FALSE),
            emp.rev.state = sum(emp.rev.state,na.rm = FALSE),
            ec.rev.state = sum(ec.rev.state,na.rm = FALSE)) %>% 
  ungroup() %>% 
  mutate(ec.rev.mrio.per.worker = ec.rev.mrio/emp.rev.mrio)
summary(direct.v2$ec.rev.mrio.per.worker)

fwrite(direct.v2, 'direct_impact_by_home_county.csv')

direct.multipliers <- mutate(direct.multipliers,
               county = ifelse((refinery_name=="Marathon Golden Eagle" | refinery_name=="Chevron Richmond" | 
                                  refinery_name=="PBF Martinez" | refinery_name=="Phillips 66 Rodeo"),"Contra Costa",
                                ifelse((refinery_name=="Marathon Carson" | refinery_name=="Chevron El Segundo" |
                                          refinery_name=="Valero Wilmington" | refinery_name=="PBF Torrance" |
                                          refinery_name=="Phillips 66 Wilmington" | refinery_name=="AltAir Paramount"), "Los Angeles",
                                       ifelse((refinery_name=="Valero Benicia"),"Solano",
                                              ifelse((refinery_name=="Kern Oil Bakersfield" | refinery_name=="San Joaquin Bakersfield"),"Kern",
                                                     ifelse(refinery_name=="Phillips 66 Santa Maria","San Luis Obispo",NA)))))) %>% 
  group_by(county) %>% 
  summarize(emp.rev.mrio = sum(emp.rev.mrio,na.rm = FALSE),
            ec.rev.mrio = sum(ec.rev.mrio,na.rm = FALSE),
            emp.rev.state = sum(emp.rev.state,na.rm = FALSE),
            ec.rev.state = sum(ec.rev.state,na.rm = FALSE)) %>% 
  mutate(n_refineries = ifelse(county=="Contra Costa",4,
                               ifelse(county=="Los Angeles",6,
                                      ifelse(county=="Solano",1,
                                             ifelse(county=="Kern",2,
                                                    ifelse(county=="San Luis Obispo",1,NA)))))) %>% 
  ungroup()

### COMPARISON IS OLD MULTIPLIERS 
old.mult <- fread('old_multipliers.csv') %>% 
  filter(ImpactType=="Direct") %>% 
  rename(old_emp = Employment, 
         old_ec = EmployeeCompensation) %>% 
  mutate(county = str_remove_all(DestinationRegion," County, CA"),
         county = trimws(county)) %>% 
  dplyr::select(county, old_emp, old_ec) 
str(old.mult)

old.mult.indir.indu <- fread('old_multipliers.csv') %>% 
  filter(ImpactType=="Indirect" | ImpactType=="Induced") %>% 
  rename(old_emp = Employment, 
         old_ec = EmployeeCompensation) %>% 
  mutate(county = str_remove_all(DestinationRegion," County, CA"),
         county = trimws(county)) %>% 
  dplyr::select(county,ImpactType, old_emp, old_ec) %>% 
  group_by(county,ImpactType) %>% 
  summarize(old_emp = sum(old_emp,na.rm=TRUE),
            old_ec = sum(old_ec, na.rm=TRUE)) %>% 
  ungroup()
str(old.mult.indir.indu)

comparison.df <- left_join(direct.multipliers,old.mult,by="county")
fwrite(comparison.df,'multiplier_comparison.csv')

comparison.df.indir.indu <- left_join(mrio.multipliers,old.mult.indir.indu,by=c("region"="county","ImpactType"))


statewide.indir.indu <- filter(rev.statewide, ImpactType != "Direct") %>% 
  group_by(DestinationRegion,ImpactType) %>% 
  summarize(emp.rev = sum(emp.rev, na.rm=TRUE),
            ec.rev = sum(ec.rev, na.rm=TRUE))

li.statewide <- filter(statewide, ImpactType=="Induced") %>% 
  group_by(DestinationRegion) %>% 
  summarize(emp.li = sum(emp.li, na.rm=TRUE),
            ec.li = sum(ec.li, na.rm=TRUE))


  
