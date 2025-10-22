

renewables_info_altair <- tar_read(renewables_info_altair)
dt_ef_ref <- tar_read(dt_ef_ref)
dt_ef <- tar_read(dt_ef)
dt_refcap <- tar_read(dt_refcap)

ref_ei <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/refinery_emission_factor.csv"
                , stringsAsFactors = F)


#refining <- copy(refining_sites_cons_ghg_2019_2045)
refining <- fread("C:/Users/vince/Desktop/refining_sites_cons_ghg_2019_2045_for_review.csv", stringsAsFactors = F)

cluster_cw <- dt_refcap %>%
  dplyr::select(site_id, region) %>%
  mutate(site_id = as.character(site_id)) %>%
  bind_rows(renewables_info_altair %>% dplyr::select(site_id, region)) %>%
  distinct()

refining <- merge(refining, cluster_cw, by = "site_id", all.x = T, allow.cartesian = T, no.dups = T)

refining[, site_id := ifelse(site_id == "t-800", "800", site_id)]
refining[, site_id := ifelse(site_id == "342-2", "34222", site_id)]
refining[, site_id := as.numeric(site_id)]

# Cluster-level emission factors -----------------------------------------
dt_ef <- dt_ef %>%
  mutate(ton_bbl = kg_bbl / 1000) %>%
  dplyr::select(-kg_bbl) %>%
  spread(pollutant_code, ton_bbl)

# refining <- merge(refining, dt_ef, by.x = "region", by.y = "cluster", all.x = T, allow.cartesian = T, no.dups = T)
# 
# refining <- refining %>%
#   mutate(
#     nh3 = bbls_consumed * NH3,
#     nox = bbls_consumed * NOX,
#     pm25 = bbls_consumed * `PM25-PRI`,
#     sox = bbls_consumed * SO2,
#     voc = bbls_consumed * VOC
#   ) %>%
#   dplyr::select(-NH3:-VOC)

# Refinery-level emission factors -----------------------------------------

dt_ef_ref <- dt_ef_ref %>%
  mutate(ton_bbl = kg_bbl / 1000) %>%
  dplyr::select(-kg_bbl) %>%
  spread(pollutant_code, ton_bbl)%>% #0 for NH3 for facility 271 (San Joaquin Refining Company Inc., Bakersfield Refinery)
  mutate(NH3 = replace_na(NH3,0)) # didnt report any for NEI 2011, 2014 and 2017  

refining <- merge(refining, dt_ef_ref, by.x = "site_id", by.y = "id1", all.x = T, allow.cartesian = T, no.dups = T)
refining <- merge(refining, dt_ef, by.x = "region", by.y = "cluster", all.x = T, allow.cartesian = T, no.dups = T)

#Assign cluster-specific EF for renewable fuel refineries
refining <- refining%>%
  mutate(NH3 = coalesce(NH3.x,NH3.y),
         NOX = coalesce(NOX.x,NOX.y),
         `PM25-PRI` = coalesce(`PM25-PRI.x`,`PM25-PRI.y`),
         SO2 = coalesce(SO2.x,SO2.y),
         VOC = coalesce(VOC.x,VOC.y))

refining <- refining %>%
  mutate(
    nh3 = bbls_consumed * NH3,
    nox = bbls_consumed * NOX,
    pm25 = bbls_consumed * `PM25-PRI`,
    sox = bbls_consumed * SO2,
    voc = bbls_consumed * VOC
  ) %>%
  dplyr::select(-NH3.x:-VOC)