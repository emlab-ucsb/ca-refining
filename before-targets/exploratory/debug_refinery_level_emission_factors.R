

tar_read(renewables_info_altair)
dt_ef <- tar_read(dt_ef_ref)

refining <- copy(refining_sites_cons_ghg_2019_2045)

cluster_cw <- dt_refcap %>%
  dplyr::select(site_id, region) %>%
  mutate(site_id = as.character(site_id)) %>%
  bind_rows(renewables_info_altair %>% dplyr::select(site_id, region)) %>%
  distinct()

refining <- merge(refining, cluster_cw, by = "site_id", all.x = T, allow.cartesian = T, no.dups = T)

refining[, site_id := ifelse(site_id == "t-800", "800", site_id)]
refining[, site_id := ifelse(site_id == "342-2", "34222", site_id)]
refining[, site_id := as.numeric(site_id)]

# updated emission factors -----------------------------------------
dt_ef <- dt_ef %>%
  mutate(ton_bbl = kg_bbl / 1000) %>%
  dplyr::select(-kg_bbl) %>%
  spread(pollutant_code, ton_bbl)

refining <- merge(refining, dt_ef, by.x = "region", by.y = "cluster", all.x = T, allow.cartesian = T, no.dups = T)

refining <- refining %>%
  mutate(
    nh3 = bbls_consumed * NH3,
    nox = bbls_consumed * NOX,
    pm25 = bbls_consumed * `PM25-PRI`,
    sox = bbls_consumed * SO2,
    voc = bbls_consumed * VOC
  ) %>%
  dplyr::select(-NH3:-VOC)