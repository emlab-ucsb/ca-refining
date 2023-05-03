# Goal: Import raw data on population relative to the poverty line from NHGIS 
rm(list=ls())

library(readr) 
library(magrittr)
library(dplyr)

#Set wd 

path <- '/Users/chrismalloy/Library/CloudStorage/GoogleDrive-cmalloy@ucsb.edu/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/raw'

############################################################################################

setwd(path) 

nhgis_df <- read_csv('nhgis0003_csv/nhgis0003_ds254_20215_county.csv', 
                     col_types = "cccllccccllllllllllllllllllllllllllllcllcciiiiiiiiciiiiiiii") %>% 
  filter(STATE=="California") %>% 
  select(TL_GEO_ID,STATE,COUNTY,AOXWE001,AOXWE002,AOXWE003,AOXWE004,AOXWE005,AOXWE006,AOXWE007,AOXWE008) %>% 
  rename(fips = TL_GEO_ID)
str(nhgis_df)

## NOTES: 
### AOXWE001: total population in a county 
### AOXWE002: population under .50 of the poverty line in CA
### AOXWE003: population between .50 and .99 of the poverty line in CA
### AOXWE004: population between 1.00 to 1.24 of the poverty line in CA
### AOXWE005: population between 1.25 to 1.49 of the poverty line in CA
### AOXWE006: population between 1.50 to 1.84 of the poverty line in CA
### AOXWE007: population between 1.85 to 1.99 of the poverty line in CA
### AOXWE008: population between 2.00 and over of the poverty line in CA

# Compute the share of total population in each county that is in each bin relative to the poverty line 

nhgis_df <- mutate(nhgis_df, 
                   share_aoxwe002 = AOXWE002/AOXWE001, 
                   share_aoxwe003 = AOXWE003/AOXWE001,
                   share_aoxwe004 = AOXWE004/AOXWE001,
                   share_aoxwe005 = AOXWE005/AOXWE001,
                   share_aoxwe006 = AOXWE006/AOXWE001,
                   share_aoxwe007 = AOXWE007/AOXWE001,
                   share_aoxwe008 = AOXWE008/AOXWE001)

summary(nhgis_df$share_aoxwe002)
summary(nhgis_df$share_aoxwe003)
summary(nhgis_df$share_aoxwe004)
summary(nhgis_df$share_aoxwe005)
summary(nhgis_df$share_aoxwe006)
summary(nhgis_df$share_aoxwe007)
summary(nhgis_df$share_aoxwe008)



