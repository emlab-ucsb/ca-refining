get_ces_county <- function(raw_ces) {
  
  dt = raw_ces[, c("Census Tract", "California County")]
  setnames(dt, c("Census Tract", "California County"), c("census_tract", "county"))
  dt[, census_tract := paste0("0", census_tract, sep = "")]
  dt
  
}

get_county_dac <- function(dt_ces, ces_county) {
  
    processed_ces = dt_ces[, .(census_tract, population, CES3_score, disadvantaged)]
    processed_ces[, census_tract := paste0("0", census_tract, sep="")]
    
    ces3_county = merge(processed_ces, ces_county, by = c("census_tract"))
    
    return(ces3_county)
    
}



# get_county_dac_share <- function(dt_ces, ces_county){
# 
#   processed_ces = dt_ces[, .(census_tract, population, CES3_score, disadvantaged)]
#   processed_ces[, census_tract := paste0("0", census_tract, sep="")]
# 
#   ces3_county = merge(processed_ces, ces_county, by = c("census_tract"))
# 
#   county_dac= dcast(ces3_county, county + census_tract ~ disadvantaged, value.var = "population")
#   county_dac[, Yes := fifelse(is.na(Yes), 0, Yes)]
#   county_dac[, No := fifelse(is.na(No), 0, No)]
#   county_dac[, total_pop := No + Yes]
#   county_dac[, dac_share := Yes / total_pop]
# 
#   mean_county_dac = county_dac[, .(dac_share = weighted.mean(dac_share, total_pop, na.rm = T)), by = .(county)]
# 
#   mean_county_dac
# 
# }

get_median_household_income <- function(raw_income_house) {
  
  dt = raw_income_house[, .(GEOID, estimate)]
  dt[, census_tract := paste0("0", GEOID)]
  setnames(dt, "estimate", "median_hh_income")
  dt = dt[, .(census_tract, median_hh_income)]
  dt
  
}

get_median_county_income <- function(raw_income_county) {
  
  dt <- raw_income_county[, .(county, estimate)]
  setnames(dt, "estimate", "median_hh_income")
  dt
  
}

## right now this is in the refining_mortality function
## -------------------------------------------------------

# get_census_population_weighted_incidence_rate <- function(dt_population) {
#   
#   dt = dt_population[, .(gisjoin, lower_age, upper_age, year, pop, incidence_2015)]
#   dt[, ct_id := paste0(stringr::str_sub(gisjoin, 2, 3),
#                        stringr::str_sub(gisjoin, 5, 7),
#                        stringr::str_sub(gisjoin, 9, 14))]
#   dt = dt[, .(ct_id, lower_age, upper_age, year, pop, incidence_2015)]
#   
#   dt_over_29 = dt[lower_age > 29]
#   dt_over_29[, ct_pop := sum(pop, na.rm = T), by = .(ct_id, year)]
#   dt_over_29[, share := pop/ct_pop, by = .(ct_id, year)]
#   dt_over_29[, weighted_incidence := sum(share * incidence_2015, na.rm = T), by = .(ct_id, year)]
#   
#   agg = dt_over_29[, .(weighted_incidence = mean(weighted_incidence),
#                        pop = sum(pop)), by = .(ct_id, year)]
#   agg
# 
# }


# get_refinery_site_ids <- function(dt_refcap) {
#   
#   dt = dt_refcap[, .(site_id, county)]
#   dt
#   
# }

get_site_level_refining_cons <- function(indiv_cons_output) {
  
  dt = indiv_cons_output[type == "consumption" & 
                           fuel == "crude" &
                           source == "total" &
                           boundary == "complete", .(demand_scenario, refining_scenario, site_id, year, fuel, type, value)]
  
  dt
  
}

get_site_level_refining_ghg <- function(indiv_ghg_output) {
  
  dt = indiv_ghg_output[type == "ghg" & 
                          source == "total" &
                          boundary == "complete", .(demand_scenario, refining_scenario, site_id, year, fuel, type, value)]
  
  dt
  
}

combine_refining_site_cons_ghg <- function(refining_site_consumption, refining_site_ghg) {
  
  rbindlist(list(refining_site_consumption, refining_site_ghg), use.names = T)
  
}

create_all_sites_scenarios_df <- function(refining_site_output) {
  
  all_scens = unique(refining_site_output[, .(demand_scenario, refining_scenario)])
  all_scens[, scen_id := paste(demand_scenario, refining_scenario)]
  
  all_sites = unique(refining_site_output[, .(site_id)])
  
  skeleton_scens_sites_years = expand.grid(scen_id = unique(all_scens[, scen_id]),
                                           site_id = unique(all_sites[, site_id]),
                                           year = seq(2019, 2045, 1))
  
  setDT(skeleton_scens_sites_years)
  
  df_sites_scens = merge(skeleton_scens_sites_years, all_scens,
                         by = c("scen_id"),
                         all.x = T)
  
  setcolorder(df_sites_scens, c("scen_id", "demand_scenario", "refining_scenario", "site_id", "year"))
  
  df_sites_scens

}

# create_2019_consumption_ghg_outputs <- function(all_refining_sites_scenarios, dt_site_2019) {
#   
#   refining_site_scenarios_2019 = all_refining_sites_scenarios[year == 2019]
#   
#   full_site_2019 = merge(refining_site_scenarios_2019, dt_site_2019,
#                          by = c("site_id", "year"),
#                          all.x = T)
#   
#   full_site_2019 = full_site_2019[, .(scen_id, demand_scenario, refining_scenario,
#                                       site_id, type, fuel, year, value_bbls)]
#   
#   setnames(full_site_2019, "value_bbls", "bbls_consumed")
#   full_site_2019[, fuel := NULL]
#   full_site_2019[, type := NULL]
#   full_site_2019[, ghg_kg := NA]
#   
#   setorder(full_site_2019, demand_scenario, refining_scenario, site_id)
#   full_site_2019
#   
# }

organize_consumption_ghg_outputs <- function(refining_sites_scenarios,
                                             dt_site_2019,
                                             refining_site_output, 
                                             indiv_cons_output) {
  
  # get 2019 outputs 
  refining_site_scenarios_2019 = refining_sites_scenarios[year == 2019]
  
  full_site_2019 = merge(refining_site_scenarios_2019, dt_site_2019,
                         by = c("site_id", "year"),
                         all.x = T)
  
  full_site_2019 = full_site_2019[, .(scen_id, demand_scenario, refining_scenario,
                                      site_id, type, fuel, year, value_bbls)]
  
  setnames(full_site_2019, "value_bbls", "bbls_consumed")
  full_site_2019[, fuel := NULL]
  full_site_2019[, type := NULL]
  full_site_2019[, ghg_kg := NA]
  
  setorder(full_site_2019, demand_scenario, refining_scenario, site_id)
  
  # get projected outputs
  dt_proj = refining_sites_scenarios[year > 2019]
  dt_type = data.table(type = unique(refining_site_output[, type]))
  
  dt_scens_types = crossing(dt_proj, dt_type)
  setDT(dt_scens_types)  
  
  full_site_proj = merge(dt_scens_types, refining_site_output,
                            by = c('demand_scenario', 'refining_scenario', 
                                   'site_id', 'year', 'type'),
                            all.x = T)
  full_site_proj[, fuel := NULL]
  full_site_proj[, value := fifelse(is.na(value), 0, value)]
  
  full_site_proj_wide = dcast(full_site_proj, 
                              scen_id + demand_scenario + refining_scenario + site_id + year ~ type, 
                              value.var = "value")
  
  setnames(full_site_proj_wide, c("consumption", "ghg"), c("bbls_consumed", "ghg_kg"))
  
  # bind 2019 to projected
  full_site_out = rbind(full_site_2019, full_site_proj_wide, use.names = T)
  setorder(full_site_out, demand_scenario, refining_scenario, year)
  
  # refinery names
  refinery_names = unique(indiv_cons_output[, .(site_id, refinery_name)])
  
  full_site_out_names = merge(full_site_out, refinery_names,
                              by = "site_id",
                              all.x = T)
  
  setcolorder(full_site_out_names, 
              c('scen_id', 'demand_scenario', 'refining_scenario', 
                'site_id', 'refinery_name', 'year', 'bbls_consumed', 'ghg_kg'))
  
  
  full_site_out_names
  
}


process_weighted_pm25 <- function(dt_inmap_re) {
  
  dt = copy(dt_inmap_re)
  setnames(dt, "totalpm25_aw", "weighted_totalpm25")
  
  dt_wide = dcast(dt, GEOID + site ~ pollutant, value.var = "weighted_totalpm25")
  setnames(dt_wide, c("nh3", "nox", "pm25", "sox", "voc"), paste0("weighted_totalpm25_", c("nh3", "nox", "pm25", "sox", "voc")))
  setnames(dt_wide, "site", "site_id")
  
  dt_wide = unique(dt_wide)
  
  dt_wide
  
}


create_ct_xwalk = function(raw_ct_2019,
                           raw_ct_2020){
  
  ct_xwalk_df <- raw_ct_2020 %>%
    rename(GEOID_2020 = GEOID) %>%
    mutate(GEOID_2020_area = st_area(.)) %>%
    st_intersection(raw_ct_2019) %>%
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

}


calculate_census_tract_emissions = function(refining_sites_cons_ghg_2019_2045,
                                            srm_weighted_pm25,
                                            county_dac,
                                            med_house_income,
                                            dt_ef,
                                            dt_refcap,
                                            renewables_info_altair){
  
  refining = copy(refining_sites_cons_ghg_2019_2045)
  
  cluster_cw <- dt_refcap%>%
    dplyr::select(site_id, region)%>%
    mutate(site_id = as.character(site_id))%>%
    bind_rows(renewables_info_altair %>% dplyr::select(site_id, region))%>%
    distinct()

  refining = merge(refining, cluster_cw, by = "site_id", all.x = T, allow.cartesian = T, no.dups = T)
  
  refining[ , site_id := ifelse(site_id == "t-800", "800", site_id)]
  refining[ , site_id := ifelse(site_id == "342-2", "34222", site_id)]
  refining[ , site_id := as.numeric(site_id)]
  
  # ## previous emission factors  ------------------------------------
  # ef_nh3 = 0.00056
  # ef_nox = 0.01495
  # ef_pm25 = 0.00402
  # ef_sox = 0.00851
  # ef_voc = 0.01247
  # 
  # refining[, `:=` (nh3 = bbls_consumed * ef_nh3 / 1000,
  #                  nox = bbls_consumed * ef_nox / 1000,
  #                  pm25 = bbls_consumed * ef_pm25 / 1000,
  #                  sox = bbls_consumed * ef_sox / 1000,
  #                  voc = bbls_consumed * ef_voc / 1000)]
  # 
  # updated emission factors -----------------------------------------
  dt_ef <- dt_ef %>%
    mutate(ton_bbl = kg_bbl/1000)%>%
    dplyr::select(-kg_bbl)%>%
    spread(pollutant_code,ton_bbl)

  refining = merge(refining, dt_ef, by.x = "region", by.y = "cluster", all.x = T, allow.cartesian = T, no.dups = T)

  refining <- refining%>%
    mutate(nh3 = bbls_consumed * NH3,
           nox = bbls_consumed * NOX,
           pm25 = bbls_consumed * `PM25-PRI`,
           sox = bbls_consumed * SO2,
           voc = bbls_consumed * VOC)%>%
    dplyr::select(-NH3:-VOC)
  # -------------------------------------------------------------------
  
  srm_weighted_pm25 <- srm_weighted_pm25 %>% mutate(GEOID = as.character(GEOID))
  
  srm_weighted_census = copy(srm_weighted_pm25)
  srm_weighted_census[, GEOID := paste0("0", GEOID, sep = "")]
  
  print("joining refining outputs with PM2.5 data...")
  ref_health = merge(refining, srm_weighted_census, by = "site_id", all.y = T, allow.cartesian = T, no.dups = T)
  
  ref_health[, `:=` (tot_nh3 = weighted_totalpm25_nh3 * nh3,
                     tot_nox = weighted_totalpm25_nox * nox,
                     tot_sox = weighted_totalpm25_sox * sox,
                     tot_pm25 = weighted_totalpm25_pm25 * pm25,
                     tot_voc = weighted_totalpm25_voc * voc)]
  ref_health[, total_pm25 := tot_nh3 + tot_nox + tot_pm25 + tot_sox + tot_voc]
  ref_health[, prim_pm25 := tot_pm25]
  
  ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012
  ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
  ref_health[, GEOID := ifelse(GEOID == "06037137000", "06037930401", GEOID)]

  ref_health_agg = ref_health[, .(total_pm25 = sum(total_pm25, na.rm = T),
                                  prim_pm25 = sum(prim_pm25, na.rm = T)),
                              by = .(GEOID, year, scen_id, demand_scenario, refining_scenario)]

  setnames(ref_health_agg, "GEOID", "census_tract")
  setorder(ref_health_agg, "census_tract", "year")
  
  ## convert all census tracts to character
  # ref_health_agg[, census_tract := as.character(census_tract)]
  # county_dac[, census_tract := as.character(census_tract)]
  
  ## add ces score, income, and dac
  health_ces = merge(ref_health_agg,
                     county_dac[, .(census_tract, population, CES3_score, disadvantaged)],
                     by = c("census_tract"),
                     all.x = T)
  
  ## add income
  health_income = merge(health_ces, med_house_income,
                        by = c("census_tract"),
                        all.x = T)

  ## set order
  setorder(health_income, "demand_scenario", "refining_scenario", "census_tract", "year")

  ## set column order
  setcolorder(health_income,
              c("scen_id", "demand_scenario", "refining_scenario",
                "census_tract", "year", 
                "population", "disadvantaged", "CES3_score",
                "median_hh_income", "total_pm25"))

  health_income[, prim_pm25 := NULL]

  return(health_income)

}


calculate_weighted_census_tract_emissions = function(ct_xwalk,
                                                     refining_health_income,
                                                     raw_dac) {
  
  ## select dac columns
  dac_dt <- raw_dac[, .(census_tract, ces4_score, disadvantaged)]
  
  ## select relevant columns from xwalk
  setDT(ct_xwalk)
  ct_xwalk <- ct_xwalk[, .(GEOID_2020, GEOID_2019, rel_intersect)]
  
  ## prepare pollution output
  setnames(refining_health_income, "census_tract", "GEOID_2019")
  
  ## merge
  health_weighted = merge(refining_health_income, ct_xwalk, 
                          by = c("GEOID_2019"),
                          all = TRUE,
                          allow.cartesian = TRUE)
  
  ## calculate pm2.5 for 2020 census tract, weight by rel_intersection
  health_weighted <- health_weighted[, .(weighted_total_pm25 = weighted.mean(total_pm25, rel_intersect, na.rm = T)), 
                                     by = .(scen_id, demand_scenario, refining_scenario, GEOID_2020, year)]
  
  ## filter out NA GEOID_2020, NA pm2.5
  health_weighted <- health_weighted[!is.na(GEOID_2020)]
  health_weighted <- health_weighted[!is.na(scen_id)]
  
  ## rename columns
  setnames(health_weighted, 
           c("GEOID_2020", "weighted_total_pm25"), 
           c("census_tract", "total_pm25"))
  
  ## merge with dac
  health_weighted = merge(health_weighted, dac_dt, 
                          by = c("census_tract"),
                          all.x = TRUE)
  
  ## fill in disadvantaged
  health_weighted[, disadvantaged := fifelse(is.na(disadvantaged), "No", disadvantaged)]
  
  return(health_weighted)
  
}


## calculate race disparities and DAC/non DAC
calculate_race_disp = function(health_weighted,
                               raw_pop_income_2020) {
  
  pop_income_2020 <- copy(raw_pop_income_2020)
  
  ## extract census tract, process
  pop_income_2020[, census_tract := as.character(substr(geoid, 8, nchar(geoid)))]
  pop_income_2020[, year := NULL]
  
  ## merge with health output
  merged_data <- merge(health_weighted, pop_income_2020, 
                       all.x = TRUE)
 
  ## add non-minority column, make longer, add percentages
  merged_data[, minority := hispanic + black + aialnative + asian]
  merged_data[, non_minority := total_pop - minority]

  merged_data <- merged_data %>%
    pivot_longer(cols = c(hispanic, white, black, aialnative, asian, minority, non_minority),
                 names_to = "group",
                 values_to = "pop") %>%
    as.data.table()

  merged_data[, pct := pop / total_pop]
  merged_data[, num := total_pm25 * pct * total_pop]
  merged_data[, den := pct * total_pop]
  
  ## create DAC and non-DAC variables
  merged_data[, dac_population := ifelse(disadvantaged == "Yes", total_pop, 0)]
  merged_data[, dac_num := total_pm25 * dac_population]
  merged_data[, dac_den := dac_population]
  merged_data[, nodac_population := ifelse(disadvantaged == "No", total_pop, 0)]
  merged_data[, nodac_num := total_pm25 * nodac_population]
  merged_data[, nodac_den := nodac_population]
  
  
  ## filter out any census tracts with zero pop
  merged_data <- merged_data[total_pop != 0]
  
  ## perform the collapse operation
  collapsed_data <- merged_data[, .(sum_num = sum(num),
                                    sum_den = sum(den),
                                    dac_num = sum(dac_num),
                                    dac_den = sum(dac_den),
                                    nodac_num = sum(nodac_num),
                                    nodac_den = sum(nodac_den)),
                                by = .(scen_id, demand_scenario, refining_scenario, year, group)]

  ## calculate other variables
  collapsed_data[, num_over_den := sum_num / sum_den]
  collapsed_data[, dac := dac_num / dac_den]
  collapsed_data[, no_dac := nodac_num / nodac_den]
  
  ## non-minority value for comparisons
  non_minority_df <- collapsed_data[group == "non_minority"]
  non_minority_df <- non_minority_df[, .(scen_id, demand_scenario, refining_scenario, year,
                                         num_over_den)]
  non_minority_df[, nm_num_over_den := num_over_den]
  non_minority_df[, num_over_den := NULL]
 
  ## merge
  merge_collapsed_df <- collapsed_data %>%
    left_join(non_minority_df) %>%
    ## remove white group
    filter(group != "white") %>%
    mutate(stat = num_over_den - nm_num_over_den,
           stat_dac = dac - no_dac) %>%
    as.data.table()
  
  ## return
  return(merge_collapsed_df)
  
  
}


calculate_poverty_disp <- function(raw_pop_poverty,
                                   health_weighted) {

  ## copy raw_pop_poverty
  pop_poverty <- copy(raw_pop_poverty)

  ## extract census tract, process
  pop_poverty[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  pop_poverty[, year := NULL]

  ## merge with health output
  merged_pov_data <- merge(health_weighted, pop_poverty,
                       all.x = TRUE)
  
  ## pivot longer
  merged_pov_data <- merged_pov_data %>%
    pivot_longer(cols = c(total_above_poverty, total_below_poverty),
                 names_to = "group",
                 values_to = "group_pop") %>%
    as.data.table()
  
  ## filter out census tracts with pop = 0
  merged_pov_data <- merged_pov_data[total_pop != 0]
  
  ## add columns
  merged_pov_data[, group_pct := group_pop / total_pop]
  ## why not just use the group pop?
  merged_pov_data[, group_num := total_pm25 * group_pct * total_pop]
  ## why recalculating this?
  merged_pov_data[, group_den := group_pct * total_pop]
  
  ## summarise
  summary_pov_df <- merged_pov_data[, .(
                                        # total_group_pop = sum(group_pop),
                                        total_group_num = sum(group_num),
                                        total_group_den = sum(group_den)),
                              by = .(scen_id, demand_scenario, refining_scenario, year, group)]
  
  summary_pov_df[, num_over_den := total_group_num / total_group_den]

  ## select columns, pivot wider
  summary_pov_df <- summary_pov_df %>%
    select(scen_id:group, num_over_den) %>%
    pivot_wider(names_from = group,
                values_from = num_over_den) %>%
    mutate(stat_pnp = total_below_poverty -  total_above_poverty) %>%
    as.data.table()
  
  return(summary_pov_df)
  

}



#calculate_census_tract_mortality = function(health_income,
calculate_census_tract_mortality = function(beta,
                                            se,
                                            vsl_2015,
                                            vsl_2019,
                                            income_elasticity_mort,
                                            discount_rate,
                                            health_weighted,
                                            ct_inc_45,
                                            growth_rates){
  
  ## is this in a separate function?
  #1 Calculate census-tract level population-weighted incidence rate (for age>29) 
  ct_inc_pop_45_weighted <- ct_inc_45%>%
    select(GEO_ID:end_age, year, pop, incidence_2015)%>%
    filter(start_age > 29) %>%
    group_by(GEO_ID, year) %>%
    mutate(ct_pop = sum(pop, na.rm = T),
           share = pop/ct_pop,
           weighted_incidence = sum(share * incidence_2015, na.rm = T)) %>%
    summarize(weighted_incidence = unique(weighted_incidence),
              pop = unique(ct_pop)) %>%
    ungroup()%>%
    mutate(GEO_ID = str_remove(GEO_ID, "US"))
  
  #for monetary mortality impact - growth in income for use in WTP function
  growth_rates <- growth_rates %>%
    filter(year > 2019) %>%
    mutate(cum_growth = cumprod(1 + growth_2030)) %>%
    select(-growth_2030)
  
  #Function to grow WTP
  future_WTP <- function(elasticity, growth_rate, WTP){
    return(elasticity * growth_rate * WTP + WTP) 
  }
  
  #  Delta of pollution change
  
  #refining pm25 BAU
  refining_BAU<-subset(health_weighted ,(scen_id=="BAU historic production"))%>%
  #refining_BAU<-subset(health_income,(scen_id=="BAU historic production"))%>%
    rename(bau_total_pm25=total_pm25)#%>%
    #mutate(census_tract = paste0("0",census_tract))
  
  #refining pm25 difference
  deltas_refining<- health_weighted%>%
  #deltas_refining<- health_income%>%
    #mutate(census_tract = paste0("0",census_tract))%>%
    #left_join(refining_BAU %>% select(-scen_id,-demand_scenario,-refining_scenario,-population:-median_hh_income),by=c("census_tract", "year"))%>%
    left_join(refining_BAU %>% select(-scen_id,-demand_scenario,-refining_scenario,-ces4_score,-disadvantaged),by=c("census_tract", "year"))%>%
    mutate(delta_total_pm25=total_pm25-bau_total_pm25)%>%
    select(census_tract,scen_id:year,total_pm25:delta_total_pm25)
  
  ## Merge demographic data to pollution scenarios
  
  ct_incidence_ca_poll <- deltas_refining %>%
    right_join(ct_inc_pop_45_weighted, by = c("census_tract"="GEO_ID", "year"="year"))%>%
    drop_na(scen_id) #CURRENTLY DROPPING ALL THE MISMATCHED 2010/2022 GEOIDs
  
  #Mortality impact fold adults (>=29 years old)
  ct_health <- ct_incidence_ca_poll %>%
    mutate(mortality_delta = ((exp(beta*delta_total_pm25)-1))*weighted_incidence*pop,
           mortality_level = ((exp(beta*total_pm25)-1))*weighted_incidence*pop)
  
  #Calculate the cost per premature mortality
  
  ct_mort_cost <- ct_health %>%
    mutate(VSL_2019 = vsl_2019)%>%
    left_join(growth_rates, by = c("year"="year"))%>%
    mutate(VSL = future_WTP(income_elasticity_mort, 
                            (cum_growth-1),
                            VSL_2019),
           cost_2019 = mortality_delta * VSL_2019,
           cost = mortality_delta*VSL)%>%
    group_by(year)%>%
    mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
           cost_PV = cost/((1+discount_rate)^(year-2019)))
  
  return(ct_mort_cost) 
  
}


