## tracey mangin
## may 2, 2023
## compare census tracts

## libraries
library(tidyverse)
library(sf)
library(maps)
library(data.table)
library(plotly)

## paths
main_path        <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
main_path        <- '/Users/traceymangin/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/'
sp_data_path     <- paste0(main_path, "data/GIS/raw/")

## file names
prev_ct <- "census-tract/tl_2019_06_tract.shp"
ct_2020 <- "nhgis0030_shapefile_tl2020_us_tract_2020/US_tract_2020.shp"

## crs NAD83 / California Albers
ca_crs <- 3310

## previous version of cts
census_tract19 <- read_sf(paste0(sp_data_path, prev_ct)) %>%
  select(-STATEFP:-TRACTCE,-NAME:-INTPTLON)%>%
  st_transform(crs = ca_crs)

## ca FIPS code
ca_fips_cd <- "06"

census_tract20 <- read_sf(paste0(sp_data_path, ct_2020)) %>%
  filter(STATEFP == ca_fips_cd) %>%
  select(GEOID) %>%
  st_transform(crs = ca_crs)

## number of unique census tracts
length(unique(census_tract19$GEOID)) ## 8057
length(unique(census_tract20$GEOID)) ## 9109

## antijoin for non-matching geoids
missing_19 <- anti_join(census_tract20 %>% st_drop_geometry(), census_tract19 %>% st_drop_geometry())
missing_20 <- anti_join(census_tract19 %>% st_drop_geometry(), census_tract20 %>% st_drop_geometry())

## make dfs of missing cts for plotting
missing_19 <- missing_19 %>%
  left_join(census_tract20)

missing_20 <- missing_20 %>%
  left_join(census_tract19)

## california
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

## plot
fig_missing <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
  geom_sf(data = missing_20, mapping = aes(geometry = geometry), fill = "blue", lwd = 0.2, color = "blue", alpha = 0.5, show.legend = TRUE) +
  geom_sf_text(data = missing_20, aes(geometry = geometry, label = GEOID)) +
  theme_void() 


ggplotly(fig_missing)

fig_missing2 <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
  geom_sf(data = missing_19, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "#C0C0C0", alpha = 0.5, show.legend = TRUE) +
  geom_sf_text(data = missing_19, aes(geometry = geometry, label = GEOID)) +
  theme_void() 

ggplotly(fig_missing2)


test <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
  geom_sf(data = missing_19, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", alpha = 0.7, show.legend = TRUE) +
  geom_sf(data = missing_20, mapping = aes(geometry = geometry), fill = NA, lwd = 0.2, color = "#28536B", alpha = 0.9, show.legend = TRUE) +
  # geom_sf_text(data = missing_19, aes(geometry = geometry, label = GEOID)) +
  theme_void() 

ggplotly(test)

test <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
  geom_sf(data = census_tract19, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", alpha = 0.7, show.legend = TRUE) 

ggplotly(test)

