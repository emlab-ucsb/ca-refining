
setwd("G://Shared drives/emlab/projects/current-projects/calepa-cn")

options(scipen=999)

# load all datasets

#census tract county name cross walk

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

#unique set of CA geoids
ca_geoid <- fread("data/health/processed/pop_CA_geoid.csv", stringsAsFactors = F)%>%
  select(gisjoin,geoid)%>%
  distinct()%>%
  mutate(geoid = paste0("US",str_split_fixed(geoid,"US",n=2)[,2]))

#age group descriptions
ct_age_desc <- fread("data/benmap/raw/age_group_desc.csv", stringsAsFactors = F)
  
#county names from benmap
county <- read_sf("data/benmap/raw/County_def.shp")%>%
  st_drop_geometry()%>%
  clean_names()

#county level mortality rates from benmap (yearly mortality value per person)
incidence_ca <- fread("data/benmap/raw/Mortality Incidence (2020).csv", stringsAsFactors = F) %>%
  filter(Endpoint == "Mortality, All Cause") %>%
  clean_names()%>%
  select(-endpoint_group,-race:-ethnicity, -type)%>%
  left_join(county, by = c("column"="col","row"="row"))%>%
  filter(state_name %in% "California")%>%
  select(-endpoint,-column,-row,-state_name,-state_fips,-cnty_fips)  

#census tract 2020 pop
 
ct_pop_desc <- fread("data/health/raw/DECENNIALDHC2020.PCT12-Data.csv", stringsAsFactors = F, header = T,
                    nrows=1)%>%
  select(-V421, -NAME)%>%
  gather(var,desc,PCT12_001N:PCT12_209NA)%>%
  filter(nchar(var)<11)

temp_desc <- as.data.frame(str_split_fixed(ct_pop_desc$desc, pattern = "!!",n=4))

ct_pop_desc <- ct_pop_desc%>%
  bind_cols(temp_desc)%>%
  select(-V1,-V2,-GEO_ID,- desc)%>%
  filter(!(V4 %in% ""))

ct_ca <- fread("data/health/raw/DECENNIALDHC2020.PCT12-Data.csv", stringsAsFactors = F, header = T,
                nrows=Inf)%>%
  mutate(GEO_ID = paste0("US",str_split_fixed(GEO_ID,"US",n=2)[,2]))%>%
  filter(GEO_ID %in% ca_geoid$geoid)%>%
  select(-V421, -NAME)%>%
  gather(group,pop,PCT12_001N:PCT12_209NA)%>%
  filter(group %in% ct_pop_desc$var)%>%
  left_join(ct_pop_desc, by = c("group"="var"))%>%
  rename(age_group = V4)%>%
  #mutate(age_group = ifelse(age_group %in% c("100 to 104 years", "105 to 109 years", "110 years and over"),100, age_group))%>%
  mutate(age_group = ifelse(age_group %in% c("100 to 104 years", "105 to 109 years", "110 years and over"),99, age_group))%>%
  group_by(age_group,GEO_ID)%>%
  summarise(pop = sum(as.numeric(pop), na.rm = T))%>%
  ungroup()

ct_ca <- ct_ca %>%
  mutate(lower_age = ifelse(age_group %in% "Under 1 year",0,  readr::parse_number(age_group)),
         #lower_age = ifelse(age_group %in% "100 to 104 years",100, lower_age),
         #lower_age = ifelse(age_group %in% "105 to 109 years",105, lower_age),
         upper_age = ifelse(age_group %in% "Under 1 year",0,  readr::parse_number(age_group)))%>%
         #upper_age = ifelse(age_group %in% "100 to 104 years",104, upper_age),
         #upper_age = ifelse(age_group %in% "105 to 109 years",109, upper_age))%>%
  select(-age_group)
  
#CDOF demographic projections
cdof_raw <- fread("data/benmap/raw/CDOF_p2_Age_1yr_Nosup.csv", stringsAsFactors = FALSE, blank.lines.skip = TRUE)%>%
  select(-Column1:-Column16331)%>%
  gather(year,pop,'2010':'2060')%>%
  mutate(pop = as.numeric(str_replace(pop,",","")),
         County = str_replace(County," County",""),
         year = as.numeric(year),
         Age = as.numeric(Age),
         Age = replace_na(Age,99))%>%
  clean_names()

# age_group_ct <- ct_ca %>%
#   group_by(lower_age,upper_age)%>%
#   summarize()

age_group_benmap <- incidence_ca%>%
  group_by(start_age, end_age)%>%
  summarize()%>%
  ungroup()

#yearly growth rate for each year county and age group
cdof_pred <- cdof_raw %>%
  filter(year<2046 & year>2019)%>%
  fuzzyjoin::fuzzy_left_join(
    age_group_benmap,
    by = c("age" = "start_age",
           "age" = "end_age"),
    match_fun = list(`>=`, `<=`))%>%
  group_by(county,year,start_age)%>%
  summarize(end_age = unique(end_age),pop=sum(pop))%>%
  group_by(county,start_age)%>%
  arrange(year)%>%
  mutate(change_pct = (pop - dplyr::lag(pop))/dplyr::lag(pop))%>%
  arrange(county,start_age)

# cdof_pred <- cdof_raw %>%
#   filter(year<2046 & year>2019)%>%
#   group_by(county,age)%>%
#   arrange(year)%>%
#   mutate(change_pct = (pop - dplyr::lag(pop))/dplyr::lag(pop))%>%
#   arrange(county,age) # INFINITY OR COLLAPSE TO 0

# Merge mortality incidence rates to population [NOT SURE WHY NEED TO DO FUZZY MATCH TWICE]

tic()
temp_age_match <- ct_ca %>%
  select(lower_age,upper_age)%>%
  distinct()%>%
  fuzzyjoin::fuzzy_left_join(
    age_group_benmap,
    by = c("lower_age" = "start_age",
           "upper_age" = "end_age"),
    match_fun = list(`>=`, `<=`))
toc()

ct_incidence_ca <- ct_ca %>%
  left_join(temp_age_match, by = c("lower_age","upper_age"))%>%
  group_by(GEO_ID, start_age, end_age)%>%
  summarise(pop = sum(pop, na.rm=T))%>%
  ungroup()%>%
  left_join(county_ct %>% mutate(GEOID = paste0("US",GEOID)), by = c("GEO_ID"="GEOID"))%>%
  mutate(fips = paste0(STATEFP,COUNTYFP))%>%
  select(-county_name)%>%
  left_join(incidence_ca,
            by = c("fips","start_age", "end_age"))%>%
  rename(incidence = value)

# next steps: grow population, and merge pollution scenarios
