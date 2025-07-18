## Directory

library(tidyverse)
library(sf)
library(data.table)


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #In scripts folder
# setwd('../../..') #Goes back to home project directory
# getwd()

## paths 
main_path        <- "data"
sp_data_path     <- file.path(main_path, "data-staged-for-deletion/GIS/raw")
health_data_path <- file.path(main_path, "data-staged-for-deletion/health/source_receptor_matrix/inmap_output_srm")
srm_save_path    <- file.path(main_path, "data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm")

## Read census tract shp file
census_tract <- read_sf(file.path(sp_data_path,"census-tract/tl_2019_06_tract.shp")) %>%
  select(-STATEFP:-TRACTCE,-NAME:-INTPTLON)%>%
  st_transform(crs=3310)

## counties, no islands
CA_counties <- st_read(file.path(sp_data_path, "CA_counties_noislands/CA_Counties_TIGER2016_noislands.shp")) %>%
  st_transform(crs=3310) %>%
  select(OBJECTID, GEOID)

## remove islands
CA_counties_noisl <- CA_counties %>%
  filter(!OBJECTID %in% c(3, 49)) %>%
  select(- OBJECTID)

# county_shp <- read_sf("./data/inmap/census-tract/tl_2019_06_tract.shp")%>%
#   select(-STATEFP:-TRACTCE,-NAME:-INTPTLON)%>%
#   st_transform(crs=3310)



#Select sector
# sector <- "extraction/"
sector <- "refining"

## set spatial resolutoin for SRM
# sp_res <- "county"
sp_res <- "cesnsus-tract"

if(sp_res == "county") {
  
  sp_res_path <- "county"
  shp_int <- CA_counties_noisl
  
} else {
  
  sp_res_path <- "census-tract"
  shp_int <- census_tract
  
}


## read in files
inmap_files_raw <- list.files(file.path(health_data_path, sector))
inmap_files <- ifelse(stringr::str_sub(inmap_files_raw,-3,-1) == "shp", inmap_files_raw, 0)
inmap_files <- inmap_files[!inmap_files %in% c(0)]

## create save directory for sp resolution
dir.create(file.path(srm_save_path, sector, sp_res_path), showWarnings = TRUE)

## create directories for each pollutant
pollutants_vec <- c("nh3", "nox", "pm25", "sox", "voc")

for(i in 1:length(pollutants_vec)) {
  
  dir.create(file.path(srm_save_path, sector, sp_res_path, pollutants_vec[i]), showWarnings = FALSE)

}

pattern_vec <- paste0(c("nh3", "nox", "pm25", "sox", "voc"), collapse = "|")

inmap_process_func <- function(x) {
  
  pol_tmp <- str_extract(x, pattern = pattern_vec)
  
  tmp <- read_sf(file.path(health_data_path, sector, x)) %>%
    st_transform(crs = 3310) %>%
    select(-BasePM25:-SOx,-TotalPop, -WindSpeed) %>%
    st_intersection(shp_int) %>%
    mutate(area = as.numeric(st_area(.)))%>%
    group_by(GEOID)%>%
    mutate(weight = area/sum(area))%>%
    summarize(totalpm25 = mean(TotalPM25, na.rm = T),
              totalpm25_aw = sum(weight * TotalPM25, na.rm = T))%>%
    data.frame()%>%
    select(-geometry) 
  
    setDT(tmp)
  
    fwrite(tmp, file = file.path(srm_save_path, sector, pol_tmp, paste0(substr(x, 1, nchar(x)-4), ".csv")))

}
  
## run function
map(inmap_files, inmap_process_func)








# inmap_files <- subset(inmap_files, inmap_files != 0);inmap_fileslapply(unique(inmap_files), function(x)
#   
#   pol_tmp <- str_extract(x, pattern)
#   
#   read_sf(paste0(health_data_path, sector, x)) %>%
#     st_transform(crs=3310) %>%
#     select(-BasePM25:-SOx,-TotalPop, -WindSpeed) %>%
#     st_intersection(shp_int) %>%
#     mutate(area = as.numeric(st_area(.)))%>%
#     group_by(GEOID)%>%
#     mutate(weight = area/sum(area))%>%
#     summarize(totalpm25 = mean(TotalPM25, na.rm = T),
#               totalpm25_aw = sum(weight * TotalPM25, na.rm = T))%>%
#     data.frame()%>%
#     select(-geometry) %>%
#     write.csv(paste0(srm_save_path, sector, sp_res_path, pol_tmp, "/", substr(x,1,nchar(x)-4),".csv", sep=""), row.names = FALSE)
# )
# 
