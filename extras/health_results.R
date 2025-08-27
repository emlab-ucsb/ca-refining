# CalEPA: Summary stats from health results
# vincent.thivierge@uottawa.ca
# created: 07/08/2025
# updated: 07/08/2025

# set up environment

rm(list = ls())
`%notin%` <- Negate(`%in%`)
gc()

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

wd <- c("C:\\git\\ca-refining\\outputs\\rev-submission\\cuf=0.6_beta-scenario=main\\tables\\health") #Vincent's WD
setwd(wd)
getwd()

## State-wide mortality results

fread("./cumulative_avoided_mortality.csv", stringsAsFactors = F)%>%
  filter(demo_cat%in%"DAC")%>%
  group_by(scen_id)%>%
  summarise(cumul_mort_level = sum(cumul_mort_level))%>%
  ungroup()%>%
  mutate(diff = cumul_mort_level[scen_id == "BAU historic production"]-cumul_mort_level,
         percent = (cumul_mort_level/cumul_mort_level[scen_id == "BAU historic production"])-1)

## State-wide monetized mortality results

fread("./cumulative_health_x_county.csv", stringsAsFactors = F)%>%
  filter(demo_cat%in%"DAC")%>%
  group_by(scen_id)%>%
  summarise(mortality_pv = sum(mortality_pv_dem)/1000000000)%>%
  ungroup()%>%
  mutate(diff_pv = mortality_pv[scen_id == "BAU historic production"]-mortality_pv)

  
## State-wide monetized mortality by county (new results)

fread("./cumulative_health_x_county.csv", stringsAsFactors = F)%>%
  filter(demo_cat%in%"DAC")%>%
  filter(scen_id %in% c("BAU historic production","LC1 low exports"))%>%
  group_by(scen_id,NAME)%>%
  summarize(mortality_pv_dem = sum(mortality_pv_dem))%>%
  ungroup()%>%  
  group_by(scen_id)%>%
  mutate(mortality_pv = sum(mortality_pv_dem))%>%
  ungroup()%>%
  mutate(diff_pv_state = mortality_pv[scen_id == "BAU historic production"]-mortality_pv,
         diff_pv = mortality_pv_dem[scen_id == "BAU historic production"]-mortality_pv_dem)%>%
  mutate(share_pv = (diff_pv/diff_pv_state))%>%
  #filter(scen_id %in% c("LC1 low exports"))%>%
  arrange(-share_pv)

## By demographic

fig5 <- fread("C:\\git\\ca-refining\\outputs\\rev-submission\\cuf=0.6_beta-scenario=main\\results\\figures\\figure-5\\state_disaggregated_npv_pc_fig_inputs.csv",
              stringsAsFactors = F) 

fread("C:\\git\\ca-refining\\outputs\\rev-submission\\cuf=0.6_beta-scenario=main\\results\\figures\\figure-5\\state_disaggregated_npv_pc_fig_inputs.csv",
      stringsAsFactors = F)%>%
  filter(scen_id %in% c("BAU historical production","LC1 low exports") & demo_cat %in% "DAC" & segment %in% "health")

## Sensitivity analyses

#Refinery-level
fread("./state_npv_fig_inputs_health_ref.csv", stringsAsFactors = F)%>%
  filter(unit_desc %in% "USD billion (2019 VSL)")%>%
  #filter(scen_id %in% c("BAU historical production","LC1 low exports") & unit_desc %in% "USD billion (2019 VSL)")%>%
  select(scen_id,value)%>%
  distinct()
  
#Growing age-based VSL
fread("C:\\git\\ca-refining\\outputs\\rev-submission\\cuf=0.6_beta-scenario=main\\results\\figures\\figure-3\\state_npv_fig_inputs_health.csv",
      stringsAsFactors = F)%>%
  filter(scen_id %in% c("BAU historical production","LC1 low exports") & unit_desc %in% "USD billion (annual VSL)")
  
#Constant VSL
fread("./state_npv_fig_inputs_health_non_age_vsl.csv", stringsAsFactors = F)%>%
  filter(scen_id %in% c("BAU historical production","LC1 low exports") & unit_desc %in% "USD billion (2019 VSL)")

########################################################################################################################
############# DEBUGGGING ########################################################################################################################
########################################################################################################################


fread("./cumulative_health_x_county.csv", stringsAsFactors = F)%>%
  filter(demo_cat%in%"DAC")%>%
  filter(scen_id %in% c("BAU historic production","LC1 low exports"))%>%
  group_by(scen_id,NAME)%>%
  summarize(mortality_pv_dem = sum(mortality_pv_dem),
            mortality_level_dem = sum(mortality_level_dem))%>%
  ungroup()%>%  
  group_by(scen_id)%>%
  mutate(mortality_pv = sum(mortality_pv_dem))%>%
  ungroup()%>%
  mutate(diff_pv_state = mortality_pv[scen_id == "BAU historic production"]-mortality_pv,
         diff_pv = mortality_pv_dem[scen_id == "BAU historic production"]-mortality_pv_dem)%>%
  mutate(share_pv = (diff_pv/diff_pv_state))%>%
  #filter(scen_id %in% c("LC1 low exports"))%>%
  arrange(-share_pv)%>%
  filter(NAME %in% c("Solano","San Bernardino"))
  

## State-wide monetized mortality by county (old results)

fread("C:/Users/vince/Desktop/cumulative_health_x_county.csv", stringsAsFactors = F)%>%
  filter(demo_cat%in%"DAC")%>%
  filter(scen_id %in% c("BAU historic production","LC1 low exports"))%>%
  group_by(scen_id,NAME)%>%
  summarize(mortality_pv_dem = sum(mortality_pv_dem),
            mortality_level_dem = sum(mortality_level_dem))%>%
  ungroup()%>%  
  group_by(scen_id)%>%
  mutate(mortality_pv = sum(mortality_pv_dem))%>%
  ungroup()%>%
  mutate(diff_pv_state = mortality_pv[scen_id == "BAU historic production"]-mortality_pv,
         diff_pv = mortality_pv_dem[scen_id == "BAU historic production"]-mortality_pv_dem)%>%
  mutate(share_pv = (diff_pv/diff_pv_state))%>%
  #filter(scen_id %in% c("LC1 low exports"))%>%
  arrange(-share_pv)

fread("C:/Users/vince/Desktop/cumulative_health_x_county.csv", stringsAsFactors = F)%>%
  filter(demo_cat%in%"DAC")%>%
  filter(scen_id %in% c("BAU historic production","LC1 low exports"))%>%
  group_by(scen_id,NAME)%>%
  summarize(mortality_pv_dem = sum(mortality_pv_dem),
            mortality_level_dem = sum(mortality_level_dem))%>%
  ungroup()%>%  
  group_by(scen_id)%>%
  mutate(mortality_pv = sum(mortality_pv_dem))%>%
  ungroup()%>%
  mutate(diff_pv_state = mortality_pv[scen_id == "BAU historic production"]-mortality_pv,
         diff_pv = mortality_pv_dem[scen_id == "BAU historic production"]-mortality_pv_dem)%>%
  mutate(share_pv = (diff_pv/diff_pv_state))%>%
  #filter(scen_id %in% c("LC1 low exports"))%>%
  arrange(-share_pv)%>%
  filter(NAME %in% c("Solano","San Bernardino"))


