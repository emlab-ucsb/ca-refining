## february 26, 2024
## pm2.5 figure

## pulse fig info x refinery
## --------------------------------------------------

create_srm_xwalk <- function(main_path,
                             srm_weighted_pm25,
                             ct_xwalk,
                             raw_counties,
                             raw_ct_2020_all) {
  
  srm_pm25_df <- copy(srm_weighted_pm25)
  srm_pm25_df[, GEOID := paste0("0", GEOID)]
  
  srm_pm25_df[, weighted_total_pm25 := weighted_totalpm25_nh3 + weighted_totalpm25_nox + weighted_totalpm25_pm25 +
                weighted_totalpm25_sox + weighted_totalpm25_voc]
  
  ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012
  ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
  srm_pm25_df[, GEOID := ifelse(GEOID == "06037137000", "06037930401", GEOID)]
  
  ## select relevant columns from xwalk
  ct_xwalk_df <- copy(ct_xwalk)
    
  ## select relevant columns from xwalk
  setDT(ct_xwalk_df)
  ct_xwalk_df <- ct_xwalk_df[, .(GEOID_2020, GEOID_2019, rel_intersect)]
  
  ## prepare pollution output
  setnames(srm_pm25_df, "GEOID", "GEOID_2019")
  
  ## merge
  srm_pm25_df = merge(srm_pm25_df, ct_xwalk, 
                          by = c("GEOID_2019"),
                          all = TRUE,
                          allow.cartesian = TRUE)
  
  ## calculate pm2.5 for 2020 census tract, weight by rel_intersection
  srm_pm25_df <- srm_pm25_df[, .(weighted_total_pm25 = weighted.mean(weighted_total_pm25, rel_intersect, na.rm = T)), 
                                     by = .(site_id, GEOID_2020)]
  
  ## filter out NA GEOID_2020, NA pm2.5
  srm_pm25_df <- srm_pm25_df[!is.na(GEOID_2020)]
  
  ## rename columns
  setnames(srm_pm25_df, 
           c("GEOID_2020", "weighted_total_pm25"), 
           c("census_tract", "total_pm25"))
  
  ## county names
  county_names <- raw_counties %>% 
    select(COUNTYFP, NAME) %>% 
    st_drop_geometry() %>%
    unique()
  
  ## geoid to census tract 
  county_df <- raw_ct_2020_all %>% 
    filter(STATEFP == "06") %>%
    select(census_tract = GEOID, COUNTYFP, ALAND) %>%
    st_drop_geometry() %>%
    left_join(county_names) %>%
    select(census_tract, COUNTYFP, NAME)
  
  ## add counties to srm
  srm_pm25_df = merge(srm_pm25_df, county_df, 
                      by = c("census_tract"),
                      all = TRUE,
                      allow.cartesian = TRUE)
  
  
  ## save pm2.5 exposure by refinery
  fwrite(srm_pm25_df, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "srm_pm25_refinery_level.csv"))
  
  return(srm_pm25_df)
  
}

create_srm_ct <- function(main_path,
                          refinery_pm25_srm) {
  
  pm25_srm <- copy(refinery_pm25_srm)
  
  pm25_srm <- pm25_srm[, .(total_pm25 = sum(total_pm25)), 
                      by = .(census_tract, COUNTYFP, NAME)]
  
  ## save pm2.5 exposure for each ct
  fwrite(pm25_srm, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "srm_pm25_ct.csv"))
  
  return(pm25_srm)
  
}
# 
# create_pulse_fig <- function(main_path,
#                              refinery_pm25_srm,
#                              ct_pm25_srm,
#                              raw_counties,
#                              raw_ct_2020_all,
#                              refin_locs) {
# 
#   ## Refineries plus
#   refin_new_locations <- copy(refin_locs)
#   
#   
# 
# 
#    }




# ## crop
# ## -----------------------------------
# disp_win_la_wgs84 <- st_sfc(st_point(c(-118.5, 33.6)), st_point(c(-117.8, 34.2)),
#                             crs = 4326)
# 
# disp_win_la_trans <- st_transform(disp_win_la_wgs84, crs = ca_crs)
# 
# disp_win_la_coord <- st_coordinates(disp_win_la_trans)
# 
# zoom_coord_df <- as.data.frame(disp_win_la_coord)
# 
# county_crop <- st_crop(CA_counties_noisl, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
# ct_cropped <- st_crop(ct_map_county, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
# 
# ## only include census tracts that are in the crop
# ct_intersect <- st_intersection(ct_map_county, county_crop)


