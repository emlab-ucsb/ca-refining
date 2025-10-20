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


# IMPORT LODES DATA--FILTERED USING CENSUS BLOCK IDs

df <- fread('ca_od_main_JT02_2020.csv') %>%
  filter(w_geocode == 60379800021006 |
           w_geocode == 60379800301000 |
           w_geocode == 60133780001024 |
           w_geocode == 60952521022025 |
           w_geocode == 60290024011034 |
           w_geocode == 60379800141083 |
           w_geocode == 60379800051007 |
           w_geocode == 60290005072025 |
           w_geocode == 60133200011067 |
           w_geocode == 60133150001075 |
           w_geocode == 60133580005003 |
           w_geocode == 60379800151000 |
           w_geocode == 60790123061000 | 
           w_geocode == 60375535021000 |
           w_geocode == 60290005072077) %>%
  dplyr::select(w_geocode,h_geocode,S000,SI01) %>% 
  mutate(refinery_name = ifelse(w_geocode == 60379800021006,"Marathon Carson",
                                ifelse(w_geocode == 60379800301000,"Chevron El Segundo",
                                       ifelse(w_geocode == 60133780001024,"Chevron Richmond",
                                              ifelse(w_geocode == 60952521022025,"Valero Benicia",
                                                     ifelse(w_geocode == 60290024011034,"Kern Oil Bakersfield",
                                                            ifelse(w_geocode == 60379800141083,"Valero Wilmington",
                                                                   ifelse(w_geocode == 60379800051007,"PBF Torrance",
                                                                          ifelse(w_geocode == 60290005072025,"San Joaquin Bakersfield",
                                                                                 ifelse(w_geocode == 60133200011067,"PBF Martinez",
                                                                                        ifelse(w_geocode == 60133150001075,"Marathon Golden Eagle",
                                                                                               ifelse(w_geocode == 60133580005003,"Phillips 66 Rodeo",
                                                                                                      ifelse(w_geocode == 60379800151000,"Phillips 66 Wilmington",
                                                                                                             ifelse(w_geocode == 60790123061000,"Phillips 66 Santa Maria",
                                                                                                                    ifelse(w_geocode == 60375535021000,"AltAir Paramount",
                                                                                                                           ifelse(w_geocode == 60290005072077,"Global Clean Energy",NA))))))))))))))))
str(df)
summary(df$SI01)
summary(df$S000)

df.refinery <- group_by(df,refinery_name) %>% 
  summarize(SI01 = sum(SI01), 
            S000 = sum(S000)) %>% 
  ungroup()
summary(df.refinery$SI01)
summary(df.refinery$S000)



# IMPORT LODES -- FILTER USING CENSUS TRACT IDS

df.tract <- fread('ca_od_main_JT02_2020.csv') %>%
  filter(substr(as.character(w_geocode),1,10) == 6037980002 |
           substr(as.character(w_geocode),1,10) == 6037980030 |
           substr(as.character(w_geocode),1,10) == 6013378000 |
           substr(as.character(w_geocode),1,10) == 6095252102 |
           substr(as.character(w_geocode),1,10) == 6029002401 |
           substr(as.character(w_geocode),1,10) == 6037980014 |
           substr(as.character(w_geocode),1,10) == 6037980005 |
           substr(as.character(w_geocode),1,10) == 6029000507 |
           substr(as.character(w_geocode),1,10) == 6013320001 |
           substr(as.character(w_geocode),1,10) == 6013315000 |
           substr(as.character(w_geocode),1,10) == 6013358000 |
           substr(as.character(w_geocode),1,10) == 6037980015 |
           substr(as.character(w_geocode),1,10) == 6079012306 | 
           substr(as.character(w_geocode),1,10) == 6037553502 |
           substr(as.character(w_geocode),1,10) == 6029000507) %>%
  mutate(w_tract_geocode = substr(as.character(w_geocode),1,10), 
         h_tract_geocode = substr(as.character(h_geocode),1,10)) %>%
  dplyr::select(w_geocode,h_geocode,w_tract_geocode,h_tract_geocode,S000,SI01) %>% 
  mutate(refinery_name = ifelse(substr(as.character(w_geocode),1,10) == 6037980002,"Marathon Carson",
                                ifelse(substr(as.character(w_geocode),1,10) == 6037980030,"Chevron El Segundo",
                                       ifelse(substr(as.character(w_geocode),1,10) == 6013378000,"Chevron Richmond",
                                              ifelse(substr(as.character(w_geocode),1,10) == 6095252102,"Valero Benicia",
                                                     ifelse(substr(as.character(w_geocode),1,10) == 6029002401,"Kern Oil Bakersfield",
                                                            ifelse(substr(as.character(w_geocode),1,10) == 6037980014,"Valero Wilmington",
                                                                   ifelse(substr(as.character(w_geocode),1,10) == 6037980005,"PBF Torrance",
                                                                          ifelse(substr(as.character(w_geocode),1,10) == 6029000507,"San Joaquin Bakersfield",
                                                                                 ifelse(substr(as.character(w_geocode),1,10) == 6013320001,"PBF Martinez",
                                                                                        ifelse(substr(as.character(w_geocode),1,10) == 6013315000,"Marathon Golden Eagle",
                                                                                               ifelse(substr(as.character(w_geocode),1,10) == 6013358000,"Phillips 66 Rodeo",
                                                                                                      ifelse(substr(as.character(w_geocode),1,10) == 6037980015,"Phillips 66 Wilmington",
                                                                                                             ifelse(substr(as.character(w_geocode),1,10) == 6079012306,"Phillips 66 Santa Maria",
                                                                                                                    ifelse(substr(as.character(w_geocode),1,10) == 6037553502,"AltAir Paramount",
                                                                                                                           ifelse(substr(as.character(w_geocode),1,10) == 6029000507,"Global Clean Energy",NA))))))))))))))))
str(df)
summary(df$SI01)
summary(df$S000)

df.refinery <- group_by(df.tract,refinery_name) %>% 
  summarize(SI01 = sum(SI01), 
            S000 = sum(S000)) %>% 
  ungroup()
summary(df.refinery$SI01)
summary(df.refinery$S000)


# ADD UP JOBS BY WORK-RESIDENCE TRACT PAIRS 

df.tract <- group_by(df.tract,
                     w_tract_geocode,h_tract_geocode) %>% 
  summarize(SI01 = sum(SI01),
            refinery_name = first(refinery_name)) %>% 
  filter(SI01 > 0) %>%
  ungroup()

df.tract <- mutate(df.tract,
                   w_tract_geocode = str_pad(as.character(w_tract_geocode),11,pad="0"),
                   h_tract_geocode = str_pad(as.character(h_tract_geocode),11,pad="0"))

# IMPORT CENSUS TRACTS FOR CA 

ca.tracts <- tracts(state="CA",year=2020) 

# JOIN SHAPEFILE TO LODES BY RESIDENTIAL TRACT 

df.tract.r <- left_join(df.tract,ca.tracts,by=c("h_tract_geocode"="GEOID"))
unique(is.na(df.tract.r$STATEFP)) # FALSE FOR ALL OBSERVATIONS 

df.tract.r <- st_as_sf(df.tract.r,
                       crs=4269,
                       sf_column_name = "geometry")


sf1 <- filter(df.tract.r, refinery_name=="Marathon Carson" | refinery_name=="Chevron El Segundo" | 
                refinery_name=="Valero Wilmington" | refinery_name=="PBF Torrance" | 
                refinery_name=="Phillips 66 Wilmington" | refinery_name=="AltAir Paramount") %>% 
  mutate(refinery_name = as.factor(refinery_name)) %>% 
  rename(South_Cluster=SI01)
sf2 <- filter(df.tract.r, refinery_name!="Marathon Carson" & refinery_name!="Chevron El Segundo" & 
                refinery_name!="Valero Wilmington" & refinery_name!="PBF Torrance" & 
                refinery_name!="Phillips 66 Wilmington" & refinery_name!="AltAir Paramount") %>% 
  mutate(refinery_name = as.factor(refinery_name)) %>% 
  rename(North_Cluster=SI01)

library(ggnewscale)
fig1 <- ggplot(data=sf1) + 
  geom_sf(aes(color=South_Cluster)) +
  scale_color_viridis_c(option = "D") +
  new_scale_color() + 
  geom_sf(data=sf2,aes(color=North_Cluster)) +
  scale_color_viridis_c(option = "C") +
  scale_fill_gradient() +
  geom_sf(data=ca.tracts,fill=NA) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank()) 
fig1          

sf1 <- mutate(sf1, South_Cluster = 1,North_Cluster=0)
sf2 <- mutate(sf2, North_Cluster = 1,South_Cluster=0)

sf3 <- bind_rows(sf1,sf2)

fig2 <- ggplot(data=sf3) + 
  geom_sf(aes(fill=as.factor(South_Cluster))) +
  geom_sf(data=ca.tracts,fill=NA) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank()) 
fig2  

sf4 <- filter(df.tract.r, SI01>1)

sf1 <- filter(sf4, refinery_name=="Marathon Carson" | refinery_name=="Chevron El Segundo" | 
                refinery_name=="Valero Wilmington" | refinery_name=="PBF Torrance" | 
                refinery_name=="Phillips 66 Wilmington" | refinery_name=="AltAir Paramount") %>% 
  mutate(refinery_name = as.factor(refinery_name)) %>% 
  rename(South_Cluster=SI01)
sf2 <- filter(sf4, refinery_name!="Marathon Carson" & refinery_name!="Chevron El Segundo" & 
                refinery_name!="Valero Wilmington" & refinery_name!="PBF Torrance" & 
                refinery_name!="Phillips 66 Wilmington" & refinery_name!="AltAir Paramount") %>% 
  mutate(refinery_name = as.factor(refinery_name)) %>% 
  rename(North_Cluster=SI01)

sf1 <- mutate(sf1, South_Cluster = 1,North_Cluster=0)
sf2 <- mutate(sf2, North_Cluster = 1,South_Cluster=0)

sf3 <- bind_rows(sf1,sf2)

fig3 <- ggplot(data=sf3) + 
  geom_sf(aes(fill=as.factor(South_Cluster))) +
  geom_sf(data=ca.tracts,fill=NA) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank()) 
fig3  

ggsave('ns_emp_map.png',fig1)
ggsave('ns_emp_map_v2.png',fig2)
ggsave('ns_emp_map_v3.png',fig3)
rm(fig1,fig2,fig3,sf1,sf2,sf3,sf4,df.tract.r,df.tract,df.refinery,df,ca.tracts)

# SHARE OF ZIP CODE OUTPUT VALUE FROM REFINING 
filenames <- list.files(".", pattern="Industry Detail_9*", full.names=TRUE)
zips <- as.numeric(gsub(".*?([0-9]+).*", "\\1", filenames))
zips <- rep(zips,times=c(rep(528,12)))
ldf <- lapply(filenames, fread)
df <- rbindlist(ldf) %>% 
  filter(is.na(V1)==FALSE)
df$zcta <- zips 
names(df) <- make.names(names(df))
str(df)
rm(zips,filenames,ldf)

df <- mutate(df,
             Total.Output = str_remove_all(Total.Output,"[$,]"),
             Total.Output = as.numeric(Total.Output)) 

df_total <- group_by(df,
                     zcta) %>% 
  summarize(Total.Output = sum(Total.Output)) %>% 
  ungroup()

df_refining <- group_by(df,
                        zcta) %>% 
  filter(Industry.Code==146) %>% 
  summarize(refinery.output = sum(Total.Output)) %>% 
  ungroup() %>% 
  left_join(df_total,by="zcta") %>% 
  mutate(share = refinery.output/Total.Output)

summary(df_refining$share)

rm(df,df_total)

# SHARE OF CLUSTER AND STATEWIDE OUTPUT 

df_nc <- fread('Industry Detail_north_cluster.csv') 
names(df_nc) <- make.names(names(df_nc))
df_nc <- filter(df_nc,
                Industry.Code==146 & is.na(V1)==FALSE) %>% 
  mutate(Total.Output = str_remove_all(Total.Output,"[$,]"),
         Total.Output = as.numeric(Total.Output)) %>% 
  summarize(total_north = sum(Total.Output))
str(df_nc)

df_sc <- fread('Industry Detail_south_cluster.csv') 
names(df_sc) <- make.names(names(df_sc))
df_sc <- filter(df_sc,
                Industry.Code==146 & is.na(V1)==FALSE) %>% 
  mutate(Total.Output = str_remove_all(Total.Output,"[$,]"),
         Total.Output = as.numeric(Total.Output)) %>% 
  summarize(total_south = sum(Total.Output))
str(df_sc)


df_refining$total_north <- rep(df_nc$total_north,12)
df_refining$total_south <- rep(df_sc$total_south,12)
df_refining <- mutate(df_refining, 
                      share_north = refinery.output/total_north,
                      share_south = refinery.output/total_south,
                      share_state = refinery.output/(total_north+total_south))
summary(df_refining$share_north)
summary(df_refining$share_south)
summary(df_refining$share_state)




### SHARE OF WORKERS IN ESTABLISHMENT TRACT THAT LIVE IN HOME TRACT 

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

df.work.total <- group_by(df.tract,
                          w_tract_geocode) %>%
  summarize(S000_w = sum(S000,na.rm=TRUE),
            SI01_w = sum(SI01,na.rm=TRUE),
            SI02_w = sum(SI02,na.rm=TRUE),
            SI03_w = sum(SI03,na.rm=TRUE)) %>%
  ungroup()

df.tract <- left_join(df.tract,df.work.total, by="w_tract_geocode") %>%
  mutate(S000_share = S000/S000_w,
         SI01_share = SI01/SI01_w,
         SI02_share = SI02/SI02_w,
         SI03_share = SI03/SI03_w)
summary(df.tract$S000_share)
summary(df.tract$SI01_share)
summary(df.tract$SI02_share)
summary(df.tract$SI03_share)

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

df2 <- left_join(df.tract,df,by=c("w_tract_geocode"="TRACT")) 

df2 <- mutate(df2, 
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

total.zip <- group_by(df2,
                      ZIP) %>% 
  summarize(S000_w = sum(S000,na.rm=TRUE),
            SI01_w = sum(SI01,na.rm=TRUE),
            SI02_w = sum(SI02,na.rm=TRUE),
            SI03_w = sum(SI03,na.rm=TRUE)) %>% 
  ungroup()

df2 <- left_join(df2,total.zip, by="ZIP") %>%
  mutate(S000_share = S000/S000_w,
         SI01_share = SI01/SI01_w,
         SI02_share = SI02/SI02_w,
         SI03_share = SI03/SI03_w)
summary(df2$S000_share)
summary(df2$SI01_share)
summary(df2$SI02_share)
summary(df2$SI03_share)

######################################## 
# IMPLAN zip code tables 

load.implan.zip.agg <- function(x){
  df <- fread(paste0("implan-zip/",x)) 
  names(df) <- make.names(names(df))
  separated.string <- str_split(x,"_")
  
  df <- dplyr::select(df,
                      Industry.Code,Description,Total.Output) %>% 
    filter(Description != "" & Description != "* Employment and payroll of federal govt, military" & Description != "* Employment and payroll of federal govt, non-military") %>% 
    mutate(zip = str_extract(separated.string[[1]][4],pat <- "(\\d)+"),
           county = separated.string[[1]][3],
           Total.Output = as.numeric(str_remove_all(Total.Output,"[$,]"))) 
  
  
  df <- left_join(df,implan.naics.xwalk, by=c("Industry.Code"="Implan546Index")) %>% 
    mutate(ind_type = ifelse((substr(as.character(`2017NaicsCode`),1,2)=="11" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="21" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="23" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="31" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="32" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="33" | 
                                Description == "Construction of new commercial structures, including farm structures" | 
                                Description == "Construction of new multifamily residential structures" | 
                                Description == "Construction of other new residential structures" | 
                                Description == "Maintenance and repair construction of nonresidential structures" | 
                                Description == "Maintenance and repair construction of residential structures" | 
                                Description == "Maintenance and repair construction of highways, streets, bridges, and tunnels" | 
                                Description == "Dog and cat food manufacturing" | 
                                Description == "Other animal food manufacturing" | 
                                Description == "Flour milling" | 
                                Description == "Rice milling" | 
                                Description == "Malt manufacturing" | 
                                Description == "Construction of other new nonresidential structures" | 
                                Description == "Construction of new single-family residential structures"), "SI01",
                             ifelse((substr(as.character(`2017NaicsCode`),1,2)=="42" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="44" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="45" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="48" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="49" |
                                       substr(as.character(`2017NaicsCode`),1,2)=="22"), "SI02","SI03")),
           ind_type = ifelse(Description=="Veterinary services","SI03",ind_type),
           cluster = ifelse((county == "Ventura County" | 
                               county == "Los Angeles County" | 
                               county == "San Bernardino County" | 
                               county == "Orange County" |
                               county == "Riverside County" |
                               county == "Imperial County" |
                               county == "San Diego County"), "south", "north")) %>% 
    dplyr::select(-`2017NaicsCode`,-Description,-Industry.Code,-NaicsDescription) %>% 
    group_by(zip,county,cluster,ind_type) %>% 
    summarize(Total.Output = sum(Total.Output,na.rm=TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols=c("zip","county","cluster"),values_from = "Total.Output", names_from = "ind_type",names_prefix = "output_")
  
}


# disaggregated by IMPLAN industry and zip 

load.implan.zip <- function(x){
  df <- fread(paste0("implan-zip/",x)) 
  names(df) <- make.names(names(df))
  separated.string <- str_split(x,"_")
  
  df <- dplyr::select(df,
                      Industry.Code,Description,Total.Output) %>% 
    filter(Description != "" & Description != "* Employment and payroll of federal govt, military" & Description != "* Employment and payroll of federal govt, non-military") %>% 
    mutate(zip = str_extract(separated.string[[1]][4],pat <- "(\\d)+"),
           county = separated.string[[1]][3],
           Total.Output = as.numeric(str_remove_all(Total.Output,"[$,]"))) 
  
  
  df <- left_join(df,implan.naics.xwalk, by=c("Industry.Code"="Implan546Index")) %>% 
    mutate(ind_type = ifelse((substr(as.character(`2017NaicsCode`),1,2)=="11" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="21" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="23" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="31" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="32" | 
                                substr(as.character(`2017NaicsCode`),1,2)=="33" | 
                                Description == "Construction of new commercial structures, including farm structures" | 
                                Description == "Construction of new multifamily residential structures" | 
                                Description == "Construction of other new residential structures" | 
                                Description == "Maintenance and repair construction of nonresidential structures" | 
                                Description == "Maintenance and repair construction of residential structures" | 
                                Description == "Maintenance and repair construction of highways, streets, bridges, and tunnels" | 
                                Description == "Dog and cat food manufacturing" | 
                                Description == "Other animal food manufacturing" | 
                                Description == "Flour milling" | 
                                Description == "Rice milling" | 
                                Description == "Malt manufacturing" | 
                                Description == "Construction of other new nonresidential structures" | 
                                Description == "Construction of new single-family residential structures"), "SI01",
                             ifelse((substr(as.character(`2017NaicsCode`),1,2)=="42" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="44" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="45" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="48" | 
                                       substr(as.character(`2017NaicsCode`),1,2)=="49" |
                                       substr(as.character(`2017NaicsCode`),1,2)=="22"), "SI02","SI03")),
           ind_type = ifelse(Description=="Veterinary services","SI03",ind_type),
           cluster = ifelse((county == "Ventura County" | 
                               county == "Los Angeles County" | 
                               county == "San Bernardino County" | 
                               county == "Orange County" |
                               county == "Riverside County" |
                               county == "Imperial County" |
                               county == "San Diego County"), "south", "north")) %>% 
    dplyr::select(-`2017NaicsCode`) 
}

file.names <- list.files(path="implan-zip",pattern="*.csv")
implan.naics.xwalk <- read_xlsx("Bridge_2017NaicsToImplan546.xlsx") %>% 
  dplyr::select(Implan546Index,`2017NaicsCode`) %>% 
  mutate(`2017NaicsCode` = substr(`2017NaicsCode`,1,2)) %>% 
  group_by(Implan546Index,`2017NaicsCode`) %>% 
  summarize() %>% 
  ungroup()


output <- lapply(file.names,load.implan.zip) %>% 
  bind_rows()

fwrite(output,'implan_zip_output.csv')


