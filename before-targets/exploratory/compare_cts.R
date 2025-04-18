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
# main_path        <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
# main_path        <- '/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/'
main_path <- "/Users/traceymangin/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
sp_data_path <- paste0(main_path, "data/GIS/raw/")
save_path <- paste0(main_path, "project-materials/refining-paper/model-prep/census-xwalk/")

## file names - use cartographic boundaries, not tigerlines
prev_ct <- "ct-cartographic-boundaries/cb_2019_06_tract_500k/cb_2019_06_tract_500k.shp"
ct_2020 <- "ct-cartographic-boundaries/nhgis0030_shapefile_tl2020_us_tract_2020/US_tract_2020.shp"

## crs NAD83 / California Albers
ca_crs <- 3310

## previous version of cts
census_tract19 <- read_sf(paste0(sp_data_path, prev_ct)) %>%
  select(GEOID) %>%
  st_transform(crs = ca_crs)

## california id
ca_code <- "06"

census_tract20 <- read_sf(paste0(sp_data_path, ct_2020)) %>%
  filter(STATEFP == ca_code) %>%
  select(GEOID) %>%
  st_transform(crs = ca_crs)

## number of unique census tracts
length(unique(census_tract19$GEOID)) ## 8041
length(unique(census_tract20$GEOID)) ## 9129

## antijoin for non-matching geoids
missing_19 <- anti_join(census_tract20 %>% st_drop_geometry(), census_tract19 %>% st_drop_geometry())
missing_20 <- anti_join(census_tract19 %>% st_drop_geometry(), census_tract20 %>% st_drop_geometry())
match_cts <- inner_join(census_tract19 %>% st_drop_geometry(), census_tract20 %>% st_drop_geometry())

## make dfs of missing cts for plotting
missing_19 <- missing_19 %>%
  left_join(census_tract20) %>%
  rename(GEOID_2020 = GEOID) %>%
  st_as_sf()

missing_20 <- missing_20 %>%
  left_join(census_tract19) %>%
  rename(GEOID_2019 = GEOID) %>%
  st_as_sf()

## california
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>%
  filter(ID == "california") %>%
  st_transform(ca_crs)

# ## figure: 2019 cts not in 2020
# fig_missing <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
#   geom_sf(data = missing_20, mapping = aes(geometry = geometry), fill = "blue", lwd = 0.2, color = "blue", alpha = 0.5, show.legend = TRUE) +
#   geom_sf_text(data = missing_20, aes(geometry = geometry, label = GEOID_2019)) +
#   theme_void()
#
#
# ggplotly(fig_missing)
#
# fig_missing2 <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
#   geom_sf(data = missing_19, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "#C0C0C0", alpha = 0.5, show.legend = TRUE) +
#   geom_sf_text(data = missing_19, aes(geometry = geometry, label = GEOID_2020)) +
#   theme_void()
#
# ggplotly(fig_missing2)
#
#
# test <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
#   geom_sf(data = missing_19, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", alpha = 0.7, show.legend = TRUE) +
#   geom_sf(data = missing_20, mapping = aes(geometry = geometry), fill = NA, lwd = 0.2, color = "#28536B", alpha = 0.9, show.legend = TRUE) +
#   # geom_sf_text(data = missing_19, aes(geometry = geometry, label = GEOID)) +
#   theme_void()
#
# ggplotly(test)
#
# test <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = missing_19 , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", show.legend = TRUE) +
#   geom_sf(data = census_tract19, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0.2, color = "black", alpha = 0.7, show.legend = TRUE)
#
# ggplotly(test)

## for cts with matches, check to see if they overlap between years
## -----------------------------------------------------------------------

## match df
match_2019 <- census_tract19 %>%
  filter(GEOID %in% c(match_cts$GEOID)) %>%
  rename(GEOID_2019 = GEOID) %>%
  mutate(GEOID_2019_area = st_area(.)) %>%
  st_as_sf() %>%
  st_intersection(census_tract20) %>%
  mutate(
    intersect_area = st_area(.),
    rel_intersect = intersect_area / GEOID_2019_area,
    rel_intersect = units::drop_units(rel_intersect)
  )

## cutoff
intersect_cutoff <- 0.99

## find max overlap for each ct
match_max_overlap <- match_2019 %>%
  group_by(GEOID_2019) %>%
  filter(rel_intersect == max(rel_intersect)) %>%
  ungroup() %>%
  filter(rel_intersect < intersect_cutoff)

nrow(match_max_overlap) ## 1823
nrow(match_max_overlap) / nrow(match_cts) ## 0.27

## save for review
match_max_overlap_save <- match_max_overlap %>%
  st_drop_geometry() %>%
  rename(GEOID_2020 = GEOID)

fwrite(match_max_overlap_save, paste0(save_path, "cts_2019_overlap_review.csv"))


## hhistogram of coverage
ggplot(match_max_overlap, aes(x = rel_intersect)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "max relative intersection with a 2020 census tract") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_bw()

## for how many is the max overlap ct NOT the same geoid?
match_max_overlap <- match_max_overlap %>%
  mutate(same_geoid = ifelse(GEOID_2019 == GEOID, 1, 0))

sum(match_max_overlap$same_geoid) / nrow(match_max_overlap)
## all match

## plot geoids from this subset
max_overlap_less_cutoff <- census_tract19 %>%
  filter(GEOID %in% match_max_overlap$GEOID_2019) %>%
  mutate(year = "2019") %>%
  rbind(census_tract20 %>%
    filter(GEOID %in% match_max_overlap$GEOID) %>%
    mutate(year = "2020"))
## plot
match_test_fig <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = max_overlap_less_90, mapping = aes(geometry = geometry, fill = year, color = year), lwd = 0.2, alpha = 0.5, show.legend = TRUE) +
  geom_sf_text(data = max_overlap_less_90, aes(geometry = geometry, label = GEOID, color = year), size = 0.2, show.legend = TRUE) +
  theme_void()

ggplotly(match_test_fig)




## make a crosswalk for census tracts, tract percentage overlap
## -------------------------------

## missing in 20 merged with missing in 2019
ct_merged <- missing_20 %>%
  mutate(GEOID_2019_area = st_area(.)) %>%
  st_intersection(census_tract20) %>%
  mutate(
    intersect_area = st_area(.),
    rel_intersect = intersect_area / GEOID_2019_area
  ) %>%
  rename(GEOID_2020 = GEOID) %>%
  mutate(rel_intersect = units::drop_units(rel_intersect)) %>%
  filter(rel_intersect > 0)

## plot examples
##  -------------------------------

## example: one 2019 census tract matching with more than one 2020 tract
ex1_ct_id <- "06001403300"

ex1_fig <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = ct_merged %>% filter(GEOID_2019 == ex1_ct_id), mapping = aes(geometry = geometry, fill = GEOID_2020, color = GEOID_2020), lwd = 0, color = NA, alpha = 0.8, show.legend = TRUE) +
  geom_sf(data = census_tract19 %>% filter(GEOID == ex1_ct_id), mapping = aes(geometry = geometry, color = GEOID), fill = NA, lwd = 0.9, show.legend = TRUE) +
  scale_color_manual(values = c("red")) +
  labs(color = "GEOID_2019") +
  theme_bw()

ex1_fig

## save example 1
ggsave(ex1_fig,
  filename = file.path(save_path, "xwalk_example1.png"),
  width = 180,
  height = 150,
  units = "mm"
)

## example: one 2019 census tract matching with more than one 2020 tract, small but not zero intersections
ex2_ct_id <- "06047002600"

ex2_fig <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = ct_merged %>% filter(GEOID_2019 == ex2_ct_id), mapping = aes(geometry = geometry, fill = GEOID_2020, color = GEOID_2020), lwd = 0, color = NA, alpha = 0.8, show.legend = TRUE) +
  geom_sf(data = census_tract19 %>% filter(GEOID == ex2_ct_id), mapping = aes(geometry = geometry, color = GEOID), fill = NA, lwd = 0.9, show.legend = TRUE) +
  scale_color_manual(values = c("blue")) +
  labs(color = "GEOID_2019") +
  theme_bw()

ex2_fig

## save example 2
ggsave(ex2_fig,
  filename = file.path(save_path, "xwalk_example2.png"),
  width = 180,
  height = 150,
  units = "mm"
)

## plot census tracts taht don't overlap with any 2019
na_vec <- c(
  "06037137000", "06037137203", "06037137301", "06037137302",
  "06037137402", "06037800204", "06037800206"
)

na_fig <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = census_tract19, mapping = aes(geometry = geometry), fill = "lightblue", lwd = 0, show.legend = FALSE) +
  geom_sf(data = census_tract20 %>% filter(GEOID %in% na_vec), mapping = aes(geometry = geometry), lwd = 0, color = NA, fill = "red", alpha = 0.5, show.legend = TRUE) +
  # geom_sf(data = census_tract19 %>% filter(GEOID == ex2_ct_id), mapping = aes(geometry = geometry, color = GEOID), fill = NA, lwd = 0.9, show.legend = TRUE) +
  # scale_color_manual(values = c("blue")) +
  # labs(color = "GEOID_2019") +
  theme_bw()

ggplotly(na_fig)








## need to look for cases where area in 2020 does not intersect with any area in 2019
## ------------------------------------------------------------------------------------


#
#
# ## test census tract
# test_ct_df <- ct_merged %>%
#   # filter(GEOID_2019 == "06019007901")
#   filter(GEOID_2019 == "06019007902")
#
# figtest <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = census_tract19, mapping = aes(geometry = geometry, fill = GEOID), lwd = 0.2, alpha = 0.4, show.legend = FALSE) +
#   geom_sf(data = ct_merged %>% filter(GEOID_2019 == "06019007902"), mapping = aes(geometry = geometry, fill = GEOID_2020), lwd = 0.2, alpha = 0.4, show.legend = FALSE) +
#   geom_sf(data = test_ct_df, mapping = aes(geometry = geometry, fill = GEOID_2020), lwd = 0.2, alpha = 0.4, show.legend = TRUE)
#
# ggplotly(figtest)
#

# # plot(field_boundaries %>% dplyr::select(doc_field_code),
# #      xlim = xcheck,
# #      ylim = ycheck,
# #      axes = TRUE)
# #
# # plot(field_coverage_df_1000 %>% dplyr::select(doc_field_code),
# #      xlim = xcheck,
# #      ylim = ycheck,
# #      axes = TRUE,
# #      add = TRUE,
# #      color = "yellow")
#
# field_coverage_df_1000_2 <- field_coverage_df_1000 %>%
#   mutate(setback_area = st_area(field_coverage_df_1000),
#          setback_1000 = setback_area / orig_area_m2) %>%
#   dplyr::select(doc_field_code, setback_1000) %>%
#   st_drop_geometry() %>%
#   as.tibble()
