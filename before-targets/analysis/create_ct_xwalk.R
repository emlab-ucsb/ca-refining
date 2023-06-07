## Tracey Mangin
## May 27, 2023
## Create crosswalk 

## libraries
library(tidyverse)
library(sf)
library(data.table)
library(plotly)

## paths
main_path        <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
main_path        <- '/Users/traceymangin/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/'
sp_data_path     <- paste0(main_path, "data/GIS/raw/")
save_path        <- paste0(main_path, "project-materials/refining-paper/model-prep/census-xwalk/")

## file names - use cartographic boundaries, not tigerlines
prev_ct <- "ct-cartographic-boundaries/cb_2019_06_tract_500k/cb_2019_06_tract_500k.shp"
ct_2020 <- "ct-cartographic-boundaries/nhgis0030_shapefile_tl2020_us_tract_2020/US_tract_2020.shp"

## crs NAD83 / California Albers
ca_crs <- 3310

## previous version of cts
census_tract19 <- read_sf(paste0(sp_data_path, prev_ct)) %>%
  select(GEOID)%>%
  st_transform(crs = ca_crs)

## california id
ca_code <- "06"

census_tract20 <- read_sf(paste0(sp_data_path, ct_2020)) %>%
  filter(STATEFP == ca_code) %>%
  select(GEOID) %>%
  st_transform(crs = ca_crs)

census_tract20 <- st_make_valid(census_tract20)

## make a crosswalk for census tracts, tract percentage overlap
## -------------------------------

## 2020 cts merged with 2019, all matches
ct_merged <- census_tract20 %>%
  rename(GEOID_2020 = GEOID) %>%
  mutate(GEOID_2020_area = st_area(.)) %>%
  st_intersection(census_tract19) %>%
  mutate(intersect_area = st_area(.)) %>%
  arrange(GEOID_2020, intersect_area) %>%
  group_by(GEOID_2020) %>%
  mutate(sum_intersect_area = sum(intersect_area)) %>%
  ungroup() %>%
  mutate(rel_intersect = intersect_area / sum_intersect_area) %>%
  rename(GEOID_2019 = GEOID) %>%
  mutate(rel_intersect = units::drop_units(rel_intersect)) %>%
  select(GEOID_2020, GEOID_2020_area, GEOID_2019, intersect_area, sum_intersect_area, rel_intersect) %>%
  st_drop_geometry()

## save
fwrite(ct_merged, paste0(save_path, "ct_xwalk.csv"))
