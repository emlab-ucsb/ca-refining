# CalEPA: Homemade refining emission factors from NEI and XXX data
# vthivierge@ucsb.edu
# created: 04/05/2023
# updated: 04/14/2025

# set up environment

rm(list = ls())
`%notin%` <- Negate(`%in%`)

options(scipen = 999)

## Packages

packages <- c(
  "data.table", "dplyr", "janitor", "stringr", "ggplot2", "cowplot",
  "forcats", "readxl", "forcats", "tidyr"
)

for (i in packages) {
  if (require(i, character.only = TRUE) == FALSE) {
    install.packages(i, repos = "http://cran.us.r-project.org")
  } else {
    require(i, character.only = TRUE)
  }
}

## Directory

# wd <- c("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/DataFiles_EmissionsFactors") #Vincent's WD
# setwd(wd)
# getwd()

# temporary working direction

wd <- c("H:/My Drive/UCSB/research/current/efficiency/data") # Vincent's WD
setwd(wd)

## 2017 NEI

# poll_sub_ghg <- c(poll_sub, "CH4", "CO2","N2O","SF6")
# poll_ghg <- c("CH4", "CO2","N2O","SF6")

nei_2017_raw <- fread(paste0(wd, "/raw/NEI/emis_sum_fac_15420.csv", sep = "")) %>%
  clean_names() %>%
  mutate(reporting_year = 2017) %>%
  # filter(pollutant_code %in% poll_sub_ghg)%>%
  filter(pollutant_type_s %in% "CAP")

## 2014 NEI

nei_2014_raw <- fread(paste0(wd, "/raw/NEI/2014v2facilities.csv", sep = "")) %>%
  clean_names()

nei_2014 <- nei_2014_raw %>%
  filter(st_usps_cd %in% "CA") %>%
  mutate(
    naics6 = str_sub(naics_cd, 1, 6),
    reporting_year = 2014
  ) %>%
  filter(naics6 %in% c("324110")) %>%
  filter(pollutant_cd %in% c("NOX", "SO2", "NH3", "PM25-PRI", "VOC")) %>%
  select(eis_facility_site_id, reporting_year, naics6, facility_site_name, county_name, total_emissions, pollutant_cd) %>%
  distinct() %>%
  filter(eis_facility_site_id %notin% c("15859711", "4789411", "10296811", "2534511")) %>%
  rename("eis_facility_id" = "eis_facility_site_id", "site_name" = "facility_site_name", "county" = "county_name", "pollutant_code" = "pollutant_cd")

## 2011 NEI

nei_2011_raw <- fread(paste0(wd, "/raw/NEI/2011neiv2_facility.csv", sep = "")) %>%
  clean_names()

nei_2011 <- nei_2011_raw %>%
  filter(st_usps_cd %in% "CA") %>%
  mutate(
    naics6 = str_sub(naics_cd, 1, 6),
    reporting_year = 2011
  ) %>%
  filter(naics6 %in% c("324110")) %>%
  filter(pollutant_cd %in% c("NOX", "SO2", "NH3", "PM25-PRI", "VOC")) %>%
  select(eis_facility_site_id, reporting_year, naics6, facility_site_name, county_name, total_emissions, pollutant_cd) %>%
  distinct() %>%
  #filter(eis_facility_site_id %notin% c("15859711", "4789411", "10296811", "2534511")) %>%
  rename("eis_facility_id" = "eis_facility_site_id", "site_name" = "facility_site_name", "county" = "county_name", "pollutant_code" = "pollutant_cd")


# Load refining-level data

ref_analysis <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/stocks-flows/processed/refinery_loc_cap_manual.csv") %>%
  clean_names()
ref_analysis

ref_analysis %>%
  select(cluster, county) %>%
  table()

ref_renewable <- read_excel("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/stocks-flows/processed/renewable_refinery_capacity.xlsx") %>%
  clean_names()
ref_renewable

ref_alt <- read_excel("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/stocks-flows/raw/altair_refinery_capacity.xlsx") %>%
  clean_names()
ref_alt

# Load cluster level production data

ref_prod <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/stocks-flows/processed/fuel_watch_data.csv") %>%
  clean_names() %>%
  filter(stock_flow == "Refinery Input") %>%
  # filter(stock_flow == "Refinery Production")%>%
  group_by(year, region) %>%
  summarise(thous_barrels = sum(thous_barrels, na.rm = T)) %>%
  ungroup() %>%
  filter(year %in% c(2014, 2017))

# Process NEI data: restrict to refining & California

nei_ca_ref <- nei_2017_raw %>%
  filter(state %in% "CA") %>%
  mutate(
    naics6 = str_sub(naics_code, 1, 6),
    naics4 = str_sub(naics_code, 1, 4)
  ) %>%
  # filter(naics6 %in% c("324110","325193"))%>% #Do not include ethanol refineries
  filter(naics6 %in% c("324110")) %>%
  # filter(naics4 %in% c("3241"))%>%
  filter(pollutant_code %in% c("NOX", "SO2", "NH3", "PM25-PRI", "VOC")) %>%
  select(eis_facility_id, reporting_year, naics6, company_name, site_name, address, city, county, total_emissions, pollutant_code) %>%
  bind_rows(nei_2014)

# Debugging the matching of refineries #####################################

ref_analysis %>%
  select(site_id, company, corp, refinery_name, county) %>%
  arrange(county)

nei_ca_ref %>%
  select(eis_facility_id, naics6, company_name, site_name, county, total_emissions, pollutant_code) %>%
  filter(pollutant_code %in% "NOX") %>%
  distinct() %>%
  arrange(county)

nei_ca_ref %>%
  select(eis_facility_id, naics6, company_name, site_name, county) %>%
  distinct() %>%
  arrange(county)

ref_analysis %>%
  select(site_id, company, corp, refinery_name, county) %>%
  arrange(county) %>%
  filter(county %in% "Los Angeles") %>%
  arrange(refinery_name) # %>% write.csv("C://Users/User/Desktop/ref.csv", row.names = F)

nei_ca_ref %>%
  select(-reporting_year, -total_emissions) %>%
  distinct() %>%
  filter(pollutant_code %in% "NOX") %>%
  distinct() %>%
  arrange(county) %>%
  filter(county %in% "Los Angeles") %>%
  arrange(site_name) # %>% write.csv("C://Users/User/Desktop/nei.csv", row.names = F)

# Drop renewable and asphalt refineries ############################

# From capacity data

ref_analysis_clean <- ref_analysis %>%
  filter(site_id %notin% c("489", "550", "191")) # remove asphalt refineries

# From NEI data

nei_ref_clean <- nei_ca_ref %>%
  filter(eis_facility_id %notin% c("15859711", "4789411", "10296811", "2534511", "5683611", "13703511", "365011")) %>% # drop renewable, dehydration, and asphalt refineries
  filter(eis_facility_id %notin% c("5786211")) %>% # drop what I believe is a duplicate from id 17922111
  mutate(county = ifelse(county %in% "Solano", "Solano County", county))

# Emission factors at the cluster level #################################################

# capacity at cluster level
cluster_cap_2019 <- ref_analysis_clean %>%
  group_by(cluster) %>%
  summarise(barrels_per_day = sum(barrels_per_day, na.rm = T)) %>%
  ungroup()

cluster_factors <- nei_ref_clean %>%
  left_join(ref_analysis %>% select(cluster, county) %>% distinct(), by = c("county")) %>%
  group_by(cluster, pollutant_code, reporting_year) %>%
  summarise(total_emissions = sum(total_emissions, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(cluster_cap_2019, by = c("cluster")) %>%
  left_join(ref_prod, by = c("cluster" = "region", "reporting_year" = "year")) %>%
  mutate(
    prod_per_day = (thous_barrels * 1000) / 365,
    capacity_util = prod_per_day / barrels_per_day
  ) %>%
  mutate(
    bbl_year = thous_barrels * 1000,
    emission_kg = total_emissions * 1000, # Ton to kg
    kg_bbl = emission_kg / bbl_year,
    kg_bbl_cap = emission_kg / (barrels_per_day * 365)
  )

#### State-wide share of point source emissions ##############

ref_tot <- nei_ref_clean %>%
  filter(reporting_year %in% 2017) %>%
  group_by(pollutant_code) %>%
  summarise(total_emissions = sum(total_emissions))

nei_2017_raw %>%
  filter(state %in% "CA") %>%
  filter(pollutant_code %in% c("NOX", "SO2", "NH3", "PM25-PRI", "VOC")) %>%
  group_by(pollutant_code) %>%
  summarise(total_emissions = sum(total_emissions)) %>%
  left_join(ref_tot, by = c("pollutant_code")) %>%
  mutate(total_emissions.y / total_emissions.x)

## Plot emission factors compared to Jaramillo and Muller (2016) #############################

JM <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/DataFiles_EmissionsFactors/emission_factors_final.csv")

# poll <-c("pm25","nox","sox","voc","nh3")
# names(poll) <- c("PM25-PRI","NOX","SO2","VOC","NH3")
poll <- c("PM25-PRI", "NOX", "SO2", "VOC", "NH3")
names(poll) <- c("pm25", "nox", "sox", "voc", "nh3")


JM_ref <- JM %>%
  mutate(
    pollutant = str_replace_all(pollutant, pattern = poll),
    cluster = "JM (2016)",
    quantity_kg_bbl = quantity_ton_bbl * 1000
  ) %>%
  filter(process %in% "refining")


cluster_factors %>%
  mutate(ton_bbl = kg_bbl / 1000) %>%
  left_join(JM_ref %>% select(-cluster), by = c("pollutant_code" = "pollutant")) %>%
  ggplot(aes(fct_reorder(pollutant_code, -emission_kg), y = kg_bbl)) +
  geom_col() +
  geom_col(data = JM_ref, aes(x = pollutant, y = quantity_kg_bbl)) +
  facet_grid(reporting_year ~ cluster) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Criteria pollutant", y = "Emission factor (kg/bbl)",
    title = "Figure 1: Refining emission factors"
  )

cluster_factors %>%
  ggplot(aes(x = reporting_year, y = kg_bbl, group = cluster, color = cluster)) +
  geom_line() +
  geom_point() +
  facet_wrap(~pollutant_code) +
  theme_cowplot() +
  labs(
    x = "Year", y = "Emission factor (kg/bbl)",
    title = "Figure 2: Refining emission factors by year",
    color = "Cluster"
  ) +
  scale_x_continuous(breaks = c(2014, 2017))

## Output emission factors ########################

cluster_factors <- cluster_factors %>%
  filter(reporting_year %in% 2017) %>%
  select(cluster, pollutant_code, kg_bbl)
 
cluster_factors%>% 
  #write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/ref_emission_factor.csv", row.names = F)
  write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/cluster_emission_factor_v2.csv", row.names = F) #(without that exxon duplicate)

################################################################################################################
## Facility-level emission factors ##########################################################################
################################################################################################################

# intial work

ref_analysis_clean %>% 
  select(site_id,company,corp,site,cluster,county)%>%
  write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_to_match_eia.csv", row.names = F)

nei_ref_clean %>% 
  filter(reporting_year %in% "2017")%>%
  select(eis_facility_id, company_name, site_name, city, county)%>%
  distinct()%>%
  write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_to_match_nei.csv", row.names = F)

# (matching algorithm outside of here in 0_matching_algo_v1.0_NEI.R)

#rerfinery share of cluster production

ref_prod_cap <- ref_analysis_clean %>%
  select(site_id, cluster,barrels_per_day)%>%
  group_by(cluster) %>%
  mutate(barrels_per_day_cluster = sum(barrels_per_day, na.rm = T)) %>%
  ungroup()%>%
  mutate(share_cluster_cap = barrels_per_day/barrels_per_day_cluster)%>%
  left_join(ref_prod %>% filter(year == 2017) %>% select(-year), by = c("cluster" = "region"))%>%
  mutate(ref_prod_share = share_cluster_cap*thous_barrels)%>%
  mutate(site_id= as.character(site_id))
  

ref_ei_2017 <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_matches.csv", 
             stringsAsFactors = F,
             colClasses = "character")%>%
  distinct()%>%
  left_join(ref_prod_cap %>% select(site_id, ref_prod_share, cluster), by = c("id1" = "site_id"))%>%
  left_join(nei_ref_clean %>% 
              filter(reporting_year %in% "2017") %>% 
              select(eis_facility_id, pollutant_code, total_emissions)%>%
              mutate(eis_facility_id= as.character(eis_facility_id)), by = c("id2" = "eis_facility_id"))%>%
  group_by(id1, pollutant_code)%>%
  summarize(total_emissions = sum(total_emissions, na.rm = T),
            ref_prod_share = first(ref_prod_share),
            cluster = first(cluster))%>%
  ungroup()%>%
  mutate(bbl_year = ref_prod_share * 1000,
  emission_kg = total_emissions * 1000, # Ton to kg
  kg_bbl = emission_kg / bbl_year)%>%
  left_join(cluster_factors %>% rename(cluster_kg_bbl=kg_bbl), by = c("cluster", "pollutant_code"))%>%
  left_join(ref_analysis %>% select(refinery_name,site_id) %>% distinct() %>% mutate(refinery_name_short = gsub(",.*$", "", refinery_name),
                                                                                     site_id = as.character(site_id)),
            by = c("id1"="site_id"))%>%
  left_join(JM_ref %>% select(pollutant,quantity_kg_bbl) %>% rename(JM_kg_bbl= quantity_kg_bbl), by = c("pollutant_code" = "pollutant"))%>%
  mutate(year = 2017)
  

ref_ei_2017%>% 
  write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/refinery_emission_factor.csv", row.names = F) #(without that exxon duplicate)

ref_ei_2017 %>%
  ggplot(aes(x=kg_bbl)) +
  geom_density(adjust=1.5) +
  theme_cowplot() +
  #facet_grid(pollutant_code~cluster, scales= "free")
  #facet_wrap(cluster~pollutant_code, scales= "free")+
  geom_vline(data = ref_ei_2017, aes(xintercept = cluster_kg_bbl, color = "red"))+
  facet_wrap(pollutant_code~cluster, scales= "free")+ 
  guides(color="none")

#North
ref_ei_2017 %>%
  mutate(refinery_name = str_remove_all(refinery_name, " Refinery"))%>%
  filter(cluster == "North")%>%
  ggplot(aes(y=kg_bbl, x= fct_reorder(refinery_name,-kg_bbl))) +
  geom_point()+
  theme_cowplot()+
  facet_wrap(~pollutant_code) + 
  theme(axis.text.x = element_text(angle = 75,
                                   vjust=1,
                                   hjust=1,
                                   lineheight=1))+
  labs(x="", y = "Emission factor (kg/bbl)", title = "North cluster emission intensities (2017)")+
  geom_hline(data = ref_ei_2017%>% filter(cluster == "North"), aes(yintercept = cluster_kg_bbl, color = "red"))+ 
  geom_hline(data = ref_ei_2017, aes(yintercept = JM_kg_bbl, color = "blue"))+ 
  labs(color="")+ 
  scale_colour_discrete(labels=c("Mean","JM"))
  
#South
ref_ei_2017 %>%
  mutate(refinery_name = str_remove_all(refinery_name, " Refinery"))%>%
  filter(cluster == "South")%>%
  ggplot(aes(y=kg_bbl, x= fct_reorder(refinery_name,-kg_bbl))) +
  geom_point()+
  theme_cowplot()+
  facet_wrap(~pollutant_code) + 
  theme(axis.text.x = element_text(angle = 75,
                                   vjust=1,
                                   hjust=1,
                                   lineheight=1))+
  labs(x="", y = "Emission factor (kg/bbl)", title = "South cluster emission intensities (2017)")+
  geom_hline(data = ref_ei_2017%>% filter(cluster == "South"), aes(yintercept = cluster_kg_bbl, color = "red"))+ 
  geom_hline(data = ref_ei_2017, aes(yintercept = JM_kg_bbl, color = "blue"))+ 
  labs(color="")+ 
  scale_colour_discrete(labels=c("Mean","JM"))

### Debug match

fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_matches.csv", 
                stringsAsFactors = F,
                colClasses = "character")%>%
  distinct()%>%
  left_join(ref_analysis %>% select(refinery_name,site_id) %>% distinct() %>% mutate(site_id = as.character(site_id)),
            by = c("id1"="site_id"))%>%
  left_join(nei_ca_ref %>% select(eis_facility_id, site_name)%>% distinct() %>% mutate(eis_facility_id = as.character(eis_facility_id)),
            by = c("id2"="eis_facility_id"))%>%
  arrange(id1)

#########################
#2014 PM2.5 weights
########################

weights <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining/nh3/srm_nh3_site97.csv")%>%
  mutate(pollutant_code = "NH3")%>%
  bind_rows(fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining/nox/srm_nox_site97.csv")%>%
              mutate(pollutant_code = "NOx"))%>%
  bind_rows(fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining/pm25/srm_pm25_site97.csv")%>%
              mutate(pollutant_code = "PM25"))%>%
  bind_rows(fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining/sox/srm_sox_site97.csv")%>%
              mutate(pollutant_code = "SOx"))%>%
  bind_rows(fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining/voc/srm_voc_site97.csv")%>%
              mutate(pollutant_code = "VOC"))%>%
  select(-totalpm25_aw)%>%
  group_by(pollutant_code)%>%
  summarise(totalpm25_mean = mean(totalpm25,na.rm = T),
            totalpm25_sum = sum(totalpm25,na.rm = T))%>%
  ungroup()%>%
  arrange(totalpm25_mean)

#########################
#2014 refineries
########################

match_2017 <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_matches.csv", 
      stringsAsFactors = F,
      colClasses = "character")%>%
  distinct()

match_2014 <- match_2017%>%
  mutate(id2 = ifelse(id1 %in% "226", "5786211",id2))

#13507511 (pipeline)

nei_2014_raw %>%
  #filter(locality %in% "TORRANCE")%>%
  filter(county_name %in% "Los Angeles")%>%
  filter(tolower(facility_site_name) %in% str_subset(tolower(facility_site_name), "exxon"))%>%
  select(facility_site_name,eis_facility_site_id,naics_cd,locality)%>%
  distinct()%>%
  arrange(naics_cd)

#match same refineries
match_2014%>%
  left_join(nei_2014_raw %>% mutate(eis_facility_site_id = as.character(eis_facility_site_id)) %>%
              mutate(reporting_year=2014),
            by = c("id2"="eis_facility_site_id"))%>%
  select(id1,id2,reporting_year)%>%
  distinct()

ref_prod_cap_2014 <- ref_analysis_clean %>%
  select(site_id, cluster,barrels_per_day)%>%
  group_by(cluster) %>%
  mutate(barrels_per_day_cluster = sum(barrels_per_day, na.rm = T)) %>%
  ungroup()%>%
  mutate(share_cluster_cap = barrels_per_day/barrels_per_day_cluster)%>%
  left_join(ref_prod %>% filter(year == 2014) %>% select(-year), by = c("cluster" = "region"))%>%
  mutate(ref_prod_share = share_cluster_cap*thous_barrels)%>%
  mutate(site_id= as.character(site_id))

ref_ei_2014 <- match_2014%>%
  left_join(nei_2014_raw %>% mutate(eis_facility_site_id = as.character(eis_facility_site_id)) %>%
              mutate(reporting_year=2014),
            by = c("id2"="eis_facility_site_id"))%>%
  filter(pollutant_cd %in% c("NOX", "SO2", "NH3", "PM25-PRI", "VOC"))%>%
  select(id1,id2,reporting_year, pollutant_cd,total_emissions)%>%
  rename(pollutant_code= pollutant_cd)%>%
  distinct()%>%
  left_join(ref_prod_cap_2014 %>% select(site_id, ref_prod_share, cluster), by = c("id1" = "site_id"))%>%
  group_by(id1, pollutant_code)%>%
  summarize(total_emissions = sum(total_emissions, na.rm = T),
            ref_prod_share = first(ref_prod_share),
            cluster = first(cluster),
            year = first(reporting_year))%>%
  ungroup()%>%
  mutate(bbl_year = ref_prod_share * 1000,
         emission_kg = total_emissions * 1000, # Ton to kg
         kg_bbl = emission_kg / bbl_year)%>%
  left_join(cluster_factors %>% rename(cluster_kg_bbl=kg_bbl), by = c("cluster", "pollutant_code"))%>%
  left_join(ref_analysis %>% select(refinery_name,site_id) %>% distinct() %>% mutate(refinery_name_short = gsub(",.*$", "", refinery_name),
                                                                                     site_id = as.character(site_id)),
            by = c("id1"="site_id"))%>%
  left_join(JM_ref %>% select(pollutant,quantity_kg_bbl) %>% rename(JM_kg_bbl= quantity_kg_bbl), by = c("pollutant_code" = "pollutant"))


#North
ref_ei_2014 %>%
  mutate(refinery_name = str_remove_all(refinery_name, " Refinery"))%>%
  filter(cluster == "North")%>%
  #ggplot(aes(y=kg_bbl, x= fct_reorder(refinery_name,-kg_bbl))) +
  ggplot(aes(y=kg_bbl, x= refinery_name)) +
  geom_point()+
  theme_cowplot()+
  facet_wrap(~pollutant_code) + 
  theme(axis.text.x = element_text(angle = 75,
                                   vjust=1,
                                   hjust=1,
                                   lineheight=1))+
  labs(x="", y = "Emission factor (kg/bbl)", title = "North cluster emission intensities (2014)")+
  geom_hline(data = ref_ei_2014%>% filter(cluster == "North"), aes(yintercept = cluster_kg_bbl, color = "red"))+ 
  geom_hline(data = ref_ei_2014, aes(yintercept = JM_kg_bbl, color = "blue"))+ 
  labs(color="")+ 
  scale_colour_discrete(labels=c("Mean","JM"))

#South
ref_ei_2014 %>%
  mutate(refinery_name = str_remove_all(refinery_name, " Refinery"))%>%
  filter(cluster == "South")%>%
  #ggplot(aes(y=kg_bbl, x= fct_reorder(refinery_name,-kg_bbl))) +
  ggplot(aes(y=kg_bbl, x= refinery_name)) +
  geom_point()+
  theme_cowplot()+
  facet_wrap(~pollutant_code) + 
  theme(axis.text.x = element_text(angle = 75,
                                   vjust=1,
                                   hjust=1,
                                   lineheight=1))+
  labs(x="", y = "Emission factor (kg/bbl)", title = "South cluster emission intensities (2014)")+
  geom_hline(data = ref_ei_2014%>% filter(cluster == "South"), aes(yintercept = cluster_kg_bbl, color = "red"))+ 
  geom_hline(data = ref_ei_2014, aes(yintercept = JM_kg_bbl, color = "blue"))+ 
  labs(color="")+ 
  scale_colour_discrete(labels=c("Mean","JM"))

###########################
##### 2014 vs 2017
############################
#  bind_rows(ref_ei_2014 %>% mutate(kg_bbl_2014 = kg_bbl))%>%

ref_ei_2017 %>% 
  mutate(kg_bbl_2017 = kg_bbl)%>%
  select(id1,kg_bbl_2017,pollutant_code)%>%
  left_join(ref_ei_2014 %>% mutate(kg_bbl_2014 = kg_bbl) %>%  select(id1,kg_bbl_2014,pollutant_code), 
            by = c("id1","pollutant_code"))%>%
  ggplot(aes(kg_bbl_2014,kg_bbl_2017))+
  geom_point()+
  facet_wrap(~pollutant_code)+ 
  theme_cowplot()+
  geom_abline(intercept = 0, slope = 1, size = 0.5)+
  labs(x= "Emission intensity (2014)", y="Emission intensity (2017)", 
       title = "Emission intensities 2014 vs 2017")
  
  
#######################################################
##### Average between 2014 and 2017
########################################################

ref_ei <- ref_ei_2017 %>% 
  bind_rows(ref_ei_2014)%>%
  group_by(id1,pollutant_code)%>%
  summarise(kg_bbl = mean(kg_bbl, na.rm = T),
            cluster = first(cluster),
            refinery_name = first(refinery_name))

#North
ref_ei %>%
  mutate(refinery_name = str_remove_all(refinery_name, " Refinery"))%>%
  filter(cluster == "North")%>%
  #ggplot(aes(y=kg_bbl, x= fct_reorder(refinery_name,-kg_bbl))) +
  ggplot(aes(y=kg_bbl, x= refinery_name)) +
  geom_point()+
  theme_cowplot()+
  facet_wrap(~pollutant_code) + 
  theme(axis.text.x = element_text(angle = 75,
                                   vjust=1,
                                   hjust=1,
                                   lineheight=1))+
  labs(x="", y = "Emission factor (kg/bbl)", title = "North cluster emission intensities")+
  geom_hline(data = ref_ei_2014%>% filter(cluster == "North"), aes(yintercept = cluster_kg_bbl, color = "red"))+ 
  geom_hline(data = ref_ei_2014, aes(yintercept = JM_kg_bbl, color = "blue"))+ 
  labs(color="")+ 
  scale_colour_discrete(labels=c("Mean","JM"))

#South
ref_ei %>%
  mutate(refinery_name = str_remove_all(refinery_name, " Refinery"))%>%
  filter(cluster == "South")%>%
  #ggplot(aes(y=kg_bbl, x= fct_reorder(refinery_name,-kg_bbl))) +
  ggplot(aes(y=kg_bbl, x= refinery_name)) +
  geom_point()+
  theme_cowplot()+
  facet_wrap(~pollutant_code) + 
  theme(axis.text.x = element_text(angle = 75,
                                   vjust=1,
                                   hjust=1,
                                   lineheight=1))+
  labs(x="", y = "Emission factor (kg/bbl)", title = "South cluster emission intensities")+
  geom_hline(data = ref_ei_2014%>% filter(cluster == "South"), aes(yintercept = cluster_kg_bbl, color = "red"))+ 
  geom_hline(data = ref_ei_2014, aes(yintercept = JM_kg_bbl, color = "blue"))+ 
  labs(color="")+ 
  scale_colour_discrete(labels=c("Mean","JM"))

#Output

ref_ei%>%
  select(id1, pollutant_code, kg_bbl)%>%
  distinct()%>%
  write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/refinery_emission_factor.csv", row.names = F) #(without that exxon duplicate)



