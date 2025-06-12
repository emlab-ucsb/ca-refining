tar_load(ct_inc_45)
tar_load(growth_cap_rates)
tar_load(vsl_2019)
tar_load(health_weighted)
tar_load(beta)
tar_load(dt_age_vsl)
tar_load(income_elasticity_mort)
tar_load(discount_rate)

###############################################################

# for monetary mortality impact - growth in income for use in WTP function
growth_rates <- growth_cap_rates %>%
  filter(year > 2019) %>%
  mutate(cum_growth = cumprod(1 + growth_2035)) %>%
  select(-growth_2035)%>%
  drop_na(year)

# Function to grow WTP
future_WTP <- function(elasticity, growth_rate, WTP) {
  return(elasticity * growth_rate * WTP + WTP)
}

#### VSL age cross-walk

vsl_cross_walk <- dt_age_vsl %>%
  filter(age_min > 29)%>%
  select(-year)%>%
  fuzzyjoin::fuzzy_left_join(
    ct_inc_45 %>%
      select(start_age, end_age) %>%
      distinct()%>%
      filter(start_age > 29),
    by = c("age_min" = "start_age",
           "age_max" = "end_age"),
    match_fun = list(`>=`, `<=`))%>%
  group_by(start_age)%>%
  summarise(end_age = first(end_age),
            age_VSL_2019 = mean(age_VSL_2019))%>%
  drop_na(start_age)%>%
  ungroup()
  

########### Add VSL 2019 by age group

ct_inc_45_temp <- ct_inc_45 %>%
  select(GEO_ID:end_age, year, pop, incidence_2015) %>%
  filter(start_age > 29)%>%
  left_join(vsl_cross_walk, by = c("start_age", "end_age"))%>%
  mutate(age_VSL_2019 = ifelse(start_age>64, 
                               dt_age_vsl %>% filter(age_min>61) %>% select(age_VSL_2019) %>% pull(),
                               age_VSL_2019))

########### Grow VSL 

# for monetary mortality impact - growth in income for use in WTP function
growth_rates <- growth_cap_rates %>%
  filter(year > 2019) %>%
  mutate(cum_growth = cumprod(1 + growth_2035)) %>%
  select(-growth_2035)%>%
  drop_na(year)

# Function to grow WTP
future_WTP <- function(elasticity, growth_rate, WTP) {
  return(elasticity * growth_rate * WTP + WTP)
}

# Merge growth factors 
ct_inc_45_temp <- ct_inc_45_temp%>%
  mutate(VSL_2019 = vsl_2019) %>%
  left_join(growth_rates, by = c("year" = "year")) %>%
  mutate(
    VSL = future_WTP(
      income_elasticity_mort,
      (cum_growth - 1),
      VSL_2019
    ),
    age_VSL = future_WTP(
      income_elasticity_mort,
      (cum_growth - 1),
      age_VSL_2019
    ))

########### Aggregate at census tract and year level 

ct_inc_pop_45_weighted <- ct_inc_45_temp %>%
  group_by(GEO_ID, year) %>%
  mutate(
    ct_pop = sum(pop, na.rm = T),
    share = pop / ct_pop,
    weighted_incidence = sum(share * incidence_2015, na.rm = T),
    weighted_monetized_incidence = sum(share * incidence_2015 * VSL, na.rm = T),
    weighted_monetized_age_incidence = sum(share * incidence_2015 * age_VSL, na.rm = T),
    weighted_monetized_age_incidence_2019 = sum(share * incidence_2015 * age_VSL_2019, na.rm = T)) %>%
  summarize(
    weighted_incidence = unique(weighted_incidence),
    weighted_monetized_incidence = unique(weighted_monetized_incidence),
    weighted_monetized_age_incidence = unique(weighted_monetized_age_incidence),
    weighted_monetized_age_incidence_2019 = unique(weighted_monetized_age_incidence),
    pop = unique(ct_pop)
  ) %>%
  ungroup() %>%
  mutate(GEO_ID = str_remove(GEO_ID, "US"))

#  Delta of pollution change ######################################

# refining pm25 BAU
refining_BAU <- subset(health_weighted, (scen_id == "BAU historic production")) %>%
  # refining_BAU<-subset(health_income,(scen_id=="BAU historic production"))%>%
  rename(bau_total_pm25 = total_pm25) # %>%
# mutate(census_tract = paste0("0",census_tract))

# refining pm25 difference
deltas_refining <- health_weighted %>%
  # deltas_refining<- health_income%>%
  # mutate(census_tract = paste0("0",census_tract))%>%
  # left_join(refining_BAU %>% select(-scen_id,-demand_scenario,-refining_scenario,-population:-median_hh_income),by=c("census_tract", "year"))%>%
  left_join(refining_BAU %>% select(-scen_id, -demand_scenario, -refining_scenario, -ces4_score, -disadvantaged), by = c("census_tract", "year")) %>%
  mutate(delta_total_pm25 = total_pm25 - bau_total_pm25) %>%
  select(census_tract, scen_id:year, total_pm25:delta_total_pm25)

## Merge demographic data to pollution scenarios

ct_incidence_ca_poll <- deltas_refining %>%
  right_join(ct_inc_pop_45_weighted, by = c("census_tract" = "GEO_ID", "year" = "year")) %>%
  drop_na(scen_id) # CURRENTLY DROPPING ALL THE MISMATCHED 2010/2022 GEOIDs

# Monetized mortality for adults (>=29 years old) by age-based VSL

ct_mort_cost <- ct_incidence_ca_poll %>%
  mutate(
    mortality_delta = ((exp(beta * delta_total_pm25) - 1)) * weighted_incidence * pop,
    mortality_level = ((exp(beta * total_pm25) - 1)) * weighted_incidence * pop,
    benefit_delta = ((exp(beta * delta_total_pm25) - 1)) * weighted_monetized_incidence * pop,
    benefit_level = ((exp(beta * total_pm25) - 1)) * weighted_monetized_incidence * pop,
    benefit_age_delta = ((exp(beta * delta_total_pm25) - 1)) * weighted_monetized_age_incidence * pop,
    benefit_age_delta_2019 = ((exp(beta * delta_total_pm25) - 1)) * weighted_monetized_age_incidence_2019 * pop,
    benefit_age_level = ((exp(beta * total_pm25) - 1)) * weighted_monetized_age_incidence * pop
  )%>%
  group_by(year) %>%
  mutate(
    cost_2019_PV = benefit_age_delta_2019 / ((1 + discount_rate)^(year - 2019)),
    cost_PV = benefit_age_delta / ((1 + discount_rate)^(year - 2019))
  )%>%
  ungroup()

# # #### Calculate the cost per premature mortality (old way)################
# 
# ct_health <- ct_incidence_ca_poll %>%
#   mutate(
#     mortality_delta = ((exp(beta * delta_total_pm25) - 1)) * weighted_incidence * pop,
#     mortality_level = ((exp(beta * total_pm25) - 1)) * weighted_incidence * pop
#   )
# 
# ct_mort_cost <- ct_health %>%
#   mutate(VSL_2019 = vsl_2019) %>%
#   left_join(growth_rates, by = c("year" = "year")) %>%
#   mutate(
#     VSL = future_WTP(
#       income_elasticity_mort,
#       (cum_growth - 1),
#       VSL_2019
#     ),
#     cost_2019 = mortality_delta * VSL_2019,
#     cost = mortality_delta * VSL
#   ) %>%
#   group_by(year) %>%
#   mutate(
#     cost_2019_PV = cost_2019 / ((1 + discount_rate)^(year - 2019)),
#     cost_PV = cost / ((1 + discount_rate)^(year - 2019))
#   )


