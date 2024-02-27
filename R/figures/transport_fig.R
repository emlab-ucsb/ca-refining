## february 26, 2024
## pm2.5 figure

## pulse fig info
## --------------------------------------------------

create_srm_xwalk <- function(srm_weighted_pm25,
                             ct_xwalk) {
  
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
                                     by = .(GEOID_2020)]
  
  ## filter out NA GEOID_2020, NA pm2.5
  srm_pm25_df <- srm_pm25_df[!is.na(GEOID_2020)]
  
  ## rename columns
  setnames(srm_pm25_df, 
           c("GEOID_2020", "weighted_total_pm25"), 
           c("census_tract", "total_pm25"))
  


  
  
  
}


## Census tracts
CA_ct <- st_read(paste0(main_path, "data/GIS/raw/census-tract/tl_2019_06_tract.shp")) %>%
  st_transform(ca_crs)

## refining sites
sites_vector <- c(97, 119, 164, 202, 209, 226, 271, 279, 332, 342, 343, 800, 3422, 34222, 99999)

read_refining <- function(buff_site){
  
  bsite <- buff_site
  
  nh3 <- read_csv(paste0(main_path, 'data/', inmapRefFiles,"/nh3/srm_nh3_site",bsite,".csv",sep="")) %>% mutate(poll="nh3")
  nox <- read_csv(paste0(main_path, 'data/', inmapRefFiles,"/nox/srm_nox_site",bsite,".csv",sep="")) %>% mutate(poll="nox")
  pm25 <- read_csv(paste0(main_path, 'data/', inmapRefFiles,"/pm25/srm_pm25_site",bsite,".csv",sep="")) %>% mutate(poll="pm25")
  sox <- read_csv(paste0(main_path, 'data/', inmapRefFiles,"/sox/srm_sox_site",bsite,".csv",sep="")) %>% mutate(poll="sox")
  voc <- read_csv(paste0(main_path, 'data/', inmapRefFiles,"/voc/srm_voc_site",bsite,".csv",sep="")) %>% mutate(poll="voc")
  
  all_polls<-rbind(nh3, nox, pm25, sox, voc)
  
  all_polls$site = bsite
  
  tmp <- as.data.frame(all_polls) 
  
  return(tmp)
  
}

#DO THE FUNCTION
refining_srm <- map_df(sites_vector, read_refining) %>% bind_rows()
refining_srm <- dplyr::rename(refining_srm, weighted_totalpm25 = totalpm25_aw)

refining_srm_reshape <- dcast(refining_srm, site + GEOID ~ poll, value.var = "weighted_totalpm25")
srm_all_pollutants_refining <- dplyr::rename(refining_srm_reshape, 
                                             weighted_totalpm25nh3 = nh3, 
                                             weighted_totalpm25nox = nox,
                                             weighted_totalpm25pm25 = pm25,
                                             weighted_totalpm25sox = sox,
                                             weighted_totalpm25voc = voc,
                                             site_id = site)

srm_all_pollutants_refining$total_pm25 = srm_all_pollutants_refining$weighted_totalpm25nh3 + srm_all_pollutants_refining$weighted_totalpm25nox+srm_all_pollutants_refining$weighted_totalpm25pm25+srm_all_pollutants_refining$weighted_totalpm25sox+srm_all_pollutants_refining$weighted_totalpm25voc
ct_map <- left_join(CA_ct,srm_all_pollutants_refining,by=c("GEOID"))



##DACS

# dac_population <- read.csv(paste0(main_path, "data/health/raw/ces3results_part.csv"), stringsAsFactors = FALSE) %>%
#   subset(sb535_dac=="Yes")%>%
#   dplyr::rename(GEOID=census_tract)

CA_ct$GEOID = as.double(CA_ct$GEOID)

# dac_map <- left_join(CA_ct, dac_population, by=c("GEOID"))
# dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes" & COUNTYFP=="037")
# dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes")

## merge counties to census tracts
## -----------------------------------
county_code <- CA_counties %>%
  select(COUNTYFP, NAME) %>%
  st_drop_geometry() %>%
  unique() %>%
  rename(county_name = NAME)

ct_map_county <- ct_map %>%
  left_join(county_code)

## crop
## -----------------------------------
disp_win_la_wgs84 <- st_sfc(st_point(c(-118.5, 33.6)), st_point(c(-117.8, 34.2)),
                            crs = 4326)

disp_win_la_trans <- st_transform(disp_win_la_wgs84, crs = ca_crs)

disp_win_la_coord <- st_coordinates(disp_win_la_trans)

zoom_coord_df <- as.data.frame(disp_win_la_coord)

county_crop <- st_crop(CA_counties_noisl, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
ct_cropped <- st_crop(ct_map_county, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])

## only include census tracts that are in the crop
ct_intersect <- st_intersection(ct_map_county, county_crop)


