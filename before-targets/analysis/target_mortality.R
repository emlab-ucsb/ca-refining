# CalEPA: Prep 2020 incidence and population data at census tract level

setwd("G://Shared drives/emlab/projects/current-projects/calepa-cn")

options(scipen = 999)

library(data.table)
# library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(janitor)
library(sf)
# library(cowplot)

# load all datasets

# census tract county name cross walk

# county_fips <- read_xlsx("data/Census/all-geocodes-v2020.xlsx", skip = 3)%>%
#   clean_names()%>%
#   filter(state_code_fips == "06")%>%
#   select(state_code_fips,county_code_fips,area_name_including_legal_statistical_area_description)%>%
#   distinct()%>%
#   rename(county_name = area_name_including_legal_statistical_area_description)%>%
#   filter(!(county_name %in% "California"))
#
# ct <- read_sf("data/GIS/raw/ct-cartographic-boundaries/nhgis0030_shapefile_tl2020_us_tract_2020/US_tract_2020.shp")%>%
#   st_drop_geometry()%>%
#   filter(STATEFP == "06")%>%
#   select(GISJOIN:GEOID)
#
# county_ct <- ct %>%
#   left_join(county_fips, by = c("COUNTYFP"="county_code_fips"))%>%
#   distinct()
#
# write.csv(county_ct, "data/health/processed/county_name_ct.csv", row.names = F)

county_ct <- fread("data/health/processed/county_name_ct.csv", stringsAsFactors = F, colClasses = c("character"))

# unique set of CA geoids
ca_geoid <- fread("data/health/processed/pop_CA_geoid.csv", stringsAsFactors = F) %>%
  select(gisjoin, geoid) %>%
  distinct() %>%
  mutate(geoid = paste0("US", str_split_fixed(geoid, "US", n = 2)[, 2]))

# age group descriptions
ct_age_desc <- fread("data/benmap/raw/age_group_desc.csv", stringsAsFactors = F)

# county names from benmap
county <- read_sf("data/benmap/raw/County_def.shp") %>%
  st_drop_geometry() %>%
  clean_names()

# county level mortality rates from benmap (yearly mortality value per person)
incidence_ca <- fread("data/benmap/raw/Mortality Incidence (2020).csv", stringsAsFactors = F) %>%
  filter(Endpoint == "Mortality, All Cause") %>%
  clean_names() %>%
  select(-endpoint_group, -race:-ethnicity, -type) %>%
  left_join(county, by = c("column" = "col", "row" = "row")) %>%
  filter(state_name %in% "California") %>%
  select(-endpoint, -column, -row, -state_name, -state_fips, -cnty_fips)

# #Create 25-29 and 30-34 age groups

temp_split <- incidence_ca %>%
  filter(start_age == 25) %>%
  mutate(start_age = 30)

incidence_ca <- incidence_ca %>%
  mutate(end_age = ifelse(end_age == 34, 29, end_age)) %>%
  bind_rows(temp_split) %>%
  arrange(start_age)

# census tract 2020 pop

ct_pop_desc <- fread("data/health/raw/DECENNIALDHC2020.PCT12-Data.csv",
  stringsAsFactors = F, header = T,
  nrows = 1
) %>%
  select(-V421, -NAME) %>%
  gather(var, desc, PCT12_001N:PCT12_209NA) %>%
  filter(nchar(var) < 11)

temp_desc <- as.data.frame(str_split_fixed(ct_pop_desc$desc, pattern = "!!", n = 4))

ct_pop_desc <- ct_pop_desc %>%
  bind_cols(temp_desc) %>%
  select(-V1, -V2, -GEO_ID, -desc) %>%
  filter(!(V4 %in% ""))

ct_ca <- fread("data/health/raw/DECENNIALDHC2020.PCT12-Data.csv",
  stringsAsFactors = F, header = T,
  nrows = Inf
) %>%
  mutate(GEO_ID = paste0("US", str_split_fixed(GEO_ID, "US", n = 2)[, 2])) %>%
  filter(GEO_ID %in% ca_geoid$geoid) %>%
  select(-V421, -NAME) %>%
  gather(group, pop, PCT12_001N:PCT12_209NA) %>%
  filter(group %in% ct_pop_desc$var) %>%
  left_join(ct_pop_desc, by = c("group" = "var")) %>%
  rename(age_group = V4) %>%
  # mutate(age_group = ifelse(age_group %in% c("100 to 104 years", "105 to 109 years", "110 years and over"),100, age_group))%>%
  mutate(age_group = ifelse(age_group %in% c("100 to 104 years", "105 to 109 years", "110 years and over"), 99, age_group)) %>%
  group_by(age_group, GEO_ID) %>%
  summarise(pop = sum(as.numeric(pop), na.rm = T)) %>%
  ungroup()

ct_ca <- ct_ca %>%
  mutate(
    lower_age = ifelse(age_group %in% "Under 1 year", 0, readr::parse_number(age_group)),
    # lower_age = ifelse(age_group %in% "100 to 104 years",100, lower_age),
    # lower_age = ifelse(age_group %in% "105 to 109 years",105, lower_age),
    upper_age = ifelse(age_group %in% "Under 1 year", 0, readr::parse_number(age_group))
  ) %>%
  # upper_age = ifelse(age_group %in% "100 to 104 years",104, upper_age),
  # upper_age = ifelse(age_group %in% "105 to 109 years",109, upper_age))%>%
  select(-age_group)

# CDOF demographic projections
cdof_raw <- fread("data/benmap/raw/CDOF_p2_Age_1yr_Nosup.csv", stringsAsFactors = FALSE, blank.lines.skip = TRUE) %>%
  select(-Column1:-Column16331) %>%
  gather(year, pop, "2010":"2060") %>%
  mutate(
    pop = as.numeric(str_replace(pop, ",", "")),
    County = str_replace(County, " County", ""),
    year = as.numeric(year),
    Age = as.numeric(Age),
    Age = replace_na(Age, 99)
  ) %>%
  clean_names()

# age_group_ct <- ct_ca %>%
#   group_by(lower_age,upper_age)%>%
#   summarize()

age_group_benmap <- incidence_ca %>%
  group_by(start_age, end_age) %>%
  summarize() %>%
  ungroup()

# yearly growth rate for each year county and age group
cdof_pred <- cdof_raw %>%
  filter(year < 2046 & year > 2019) %>%
  fuzzyjoin::fuzzy_left_join(
    age_group_benmap,
    by = c(
      "age" = "start_age",
      "age" = "end_age"
    ),
    match_fun = list(`>=`, `<=`)
  ) %>%
  group_by(county, year, start_age) %>%
  summarize(end_age = unique(end_age), pop = sum(pop)) %>%
  group_by(county, start_age) %>%
  arrange(year) %>%
  mutate(change_pct = (pop - dplyr::lag(pop)) / dplyr::lag(pop)) %>%
  arrange(county, start_age)

# cdof_pred <- cdof_raw %>%
#   filter(year<2046 & year>2019)%>%
#   group_by(county,age)%>%
#   arrange(year)%>%
#   mutate(change_pct = (pop - dplyr::lag(pop))/dplyr::lag(pop))%>%
#   arrange(county,age) # INFINITY OR COLLAPSE TO 0

# Merge mortality incidence rates to population [NOT SURE WHY NEED TO DO FUZZY MATCH TWICE]

temp_age_match <- ct_ca %>%
  select(lower_age, upper_age) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_left_join(
    age_group_benmap,
    by = c(
      "lower_age" = "start_age",
      "upper_age" = "end_age"
    ),
    match_fun = list(`>=`, `<=`)
  )

ct_incidence_ca <- ct_ca %>%
  left_join(temp_age_match, by = c("lower_age", "upper_age")) %>%
  group_by(GEO_ID, start_age, end_age) %>%
  summarise(pop = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  left_join(county_ct %>% mutate(GEOID = paste0("US", GEOID)), by = c("GEO_ID" = "GEOID")) %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  select(-county_name) %>%
  left_join(incidence_ca,
    by = c("fips", "start_age", "end_age")
  ) %>%
  rename(incidence = value)

## Projected population data and mortality incidence

cdof_pred_sp <- cdof_pred %>%
  filter(year > 2020) %>%
  select(-pop) %>%
  spread(year, change_pct)

ct_inc_45_temp <- ct_incidence_ca %>%
  left_join(cdof_pred_sp, by = c("start_age", "end_age", "name" = "county")) %>%
  mutate(pop_2020 = pop)

for (i in 2021:2045) {
  j <- i - 1

  ct_inc_45_temp <- ct_inc_45_temp %>%
    # mutate("pop_{i}" := "pop_{j}" * 2)
    mutate("pop_{i}" := get(paste0("pop_", i - 1, sep = "")) * (1 + get(paste0(i, sep = ""))))
  # mutate(!!paste0("pop_",i,sep="") := get(paste0("pop_",i-1,sep=""))*(1+get(paste0(i,sep=""))))
  # mutate(temp_var = get(paste0("pop_",i-1,sep=""))*(1+get(paste0(i,sep=""))))
}

ct_inc_45 <- ct_inc_45_temp %>%
  select(-pop, -`2021`:-`2045`) %>%
  gather(year, pop, pop_2020:pop_2045) %>%
  mutate(year = as.numeric(str_remove(year, "pop_"))) %>%
  filter(year > 2019) %>%
  rename(incidence_2015 = incidence) %>%
  drop_na(pop) %>% # drops the ~20 census tracts with no population in 2020
  distinct()

# Debugging

rm(list = setdiff(ls(), "ct_inc_45"))
gc()

# ct_inc_45 %>%
#   group_by(year,start_age)%>%
#   summarise(pop = sum(pop))%>%
#   ungroup()%>%
#   ggplot(aes(x=year,y=base::log(pop), color = as.factor(start_age), group = as.factor(start_age)))+
#   geom_line()+
#   geom_point()+
#   theme_classic()

## Output final population and mortality incidence data

write.csv(ct_inc_45, "data/health/processed/ct_inc_45_2020.csv", row.names = F)
# write.csv(ct_inc_45, "data/health/processed/ct_inc_45_2020_old.csv", row.names = F)

# # MORTALITY IMPACTS
#
# #1 load census tract population and mortality incidence rates,
# #  and calculate census-tract level population-weighted incidence rate (for age>29)
#
# ct_inc_pop_45_weighted <- fread("data/health/processed/ct_inc_45_2020.csv", stringsAsFactors = F)%>%
#   select(GEO_ID:end_age, year, pop, incidence_2015)%>%
#   filter(start_age > 29) %>%
#   group_by(GEO_ID, year) %>%
#   mutate(ct_pop = sum(pop, na.rm = T),
#          share = pop/ct_pop,
#          weighted_incidence = sum(share * incidence_2015, na.rm = T)) %>%
#   summarize(weighted_incidence = unique(weighted_incidence),
#             pop = unique(ct_pop)) %>%
#   ungroup()%>%
#   mutate(GEO_ID = str_remove(GEO_ID, "US"))
#
# #2 Coefficients from Krewski et al (2009) for mortality impact
# beta <- 0.00582
# se <- 0.0009628
#
# #3 for monetary mortality impact - growth in income for use in WTP function
# growth_rates <- read.csv("data/benmap/processed/growth_rates.csv", stringsAsFactors = FALSE) %>%
#   filter(year > 2019) %>%
#   mutate(cum_growth = cumprod(1 + growth_2030)) %>%
#   select(-growth_2030)
#
# #4 Parameters for monetized health impact
# VSL_2015 <- 8705114.25462459
# VSL_2019 <- VSL_2015 * 107.8645906/100 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
# income_elasticity_mort <- 0.4
# discount_rate <- 0.03
#
# #5 Function to grow WTP
# future_WTP <- function(elasticity, growth_rate, WTP){
#   return(elasticity * growth_rate * WTP + WTP)
# }
#
# #6 Delta of pollution change
#
# #refining pm25 BAU
# refining_BAU<-subset(health_income,(scen_id=="BAU historic production"))%>%
#   rename(bau_total_pm25=total_pm25)
#
# #refining pm25 difference
# deltas_refining<- health_income%>%
#   left_join(refining_BAU %>% select(-scen_id,-demand_scenario,-refining_scenario,-population:-median_hh_income),by=c("census_tract", "year"))%>%
#   mutate(delta_total_pm25=total_pm25-bau_total_pm25)%>%
#   select(scen_id:year,total_pm25:delta_total_pm25)
#
# ## Merge demographic data to pollution scenarios
#
# ct_incidence_ca_poll <- deltas_refining %>%
#   right_join(ct_inc_pop_45_weighted, by = c("census_tract"="GEO_ID", "year"="year"))%>%
#   drop_na(scen_id);ct_incidence_ca_poll #CURRENTLY DROPPING ALL THE MISMATCHED 2010/2022 GEOIDs
#
# #Mortality impact fold adults (>=29 years old)
# ct_health <- ct_incidence_ca_poll %>%
#   mutate(mortality_delta = ((exp(beta*delta_total_pm25)-1))*weighted_incidence*pop,
#          mortality_level = ((exp(beta*total_pm25)-1))*weighted_incidence*pop)
#
# #Calculate the cost per premature mortality
#
# ct_mort_cost <- ct_health %>%
#   mutate(VSL_2019 = VSL_2019)%>%
#   left_join(growth_rates, by = c("year"="year"))%>%
#   mutate(VSL = future_WTP(income_elasticity_mort,
#                           (cum_growth-1),
#                           VSL_2019),
#          cost_2019 = mortality_delta*VSL_2019,
#          cost = mortality_delta*VSL)%>%
#   group_by(year)%>%
#   mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
#          cost_PV = cost/((1+discount_rate)^(year-2019)))
#
