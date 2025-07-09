# CalEPA: Age-based VSL based on Aldy and Viscusi (2008)
# vincent.thivierge@uottawa.ca
# created: 05/02/2025
# updated: 05/12/2025

# set up environment

rm(list = ls())
`%notin%` <- Negate(`%in%`)

options(scipen = 999)


income_elasticity_mort <- 0.4

future_WTP <- function(elasticity, growth_rate, WTP) {
  return(elasticity * growth_rate * WTP + WTP)
}


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

wd <- c("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/") #Vincent's WD
setwd(wd)
getwd()

## Load age-VSL

age_vsl_data <- fread("./Aldy & Viscusi 2008 Figure 1 Data.csv", stringsAsFactors = F)%>%
  mutate(age_vsl_2000 = vslcohort3*1000000,
         age_min  = age, 
         age_max = ifelse(age<62,age,100))%>%
  select(age_min,age_max,age_vsl_2000)%>%
  mutate(year=2019)

## Load real income growth rates

# #GDP
# gdp_growth <- fread("./BEA_GDP.csv", stringsAsFactors = F)%>%
#   mutate(growth_rate = (GDP_2017/lag(GDP_2017))-1)
# 
# growth_rates <- gdp_growth %>%
#   filter(year>2000)%>%
#   mutate(cum_growth = cumprod(1 + growth_rate))

#GDP per cap
gdp_growth <- fread("./FRED_gdp_cap.csv", stringsAsFactors = F)%>%
  drop_na(GDP_per_cap_2017)%>%
  mutate(growth_rate = (GDP_per_cap_2017/lag(GDP_per_cap_2017))-1,
         year = as.integer(str_sub(observation_date,1,4)))%>%
  select(year,growth_rate)

growth_rates <- gdp_growth %>%
  filter(year>2000)%>%
  mutate(cum_growth = cumprod(1 + growth_rate))

## Combine

age_vsl_2019 <- age_vsl_data%>%
  left_join(growth_rates, by = c("year" = "year"))%>%
  mutate(
    age_VSL_2019 = future_WTP(
      income_elasticity_mort,
      (cum_growth - 1),
      age_vsl_2000)*1.484654)

age_vsl_2019 %>%
  summarise(age_VSL_2019 = mean(age_VSL_2019))

age_vsl_2019 %>%
  ggplot(aes(x=age_min, y = age_VSL_2019/1000000))+
  geom_point()+
  geom_line()+
  theme_cowplot(16)+
  labs(x= "Age", y = "2019 Cohort-Adjusted VSL (million 2019 $) from Aldy & Viscusi (2008)")

age_vsl_2019%>%
  select(age_min, age_max,year,age_VSL_2019)%>%
  write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/processed/age_based_VSL_2019.csv", row.names = F)


################################################################      
# what about relative to the average vsp we are using^!^!^!^
#################################################################

## adjust our baseline value from our current version

#assume $1997 as baseline year (https://www.epa.gov/sites/default/files/2017-09/documents/ee-0568-22.pdf)

growth_9719 <- gdp_growth %>%
  filter(year>1997)%>%
  mutate(cum_growth = cumprod(1 + growth_rate))%>%
  filter(year==2019)%>%
  select(cum_growth) %>% pull()

# (https://fred.stlouisfed.org/series/CPALTT01USA661S)
  
CPI_9719 <- 107.8645906/67.72369
VSL_9719 = (0.4 * growth_9719-1 * 5.8 + 5.8)*CPI_9719

#from CalEPA numbers

#9 million in 2013$ and income

growth_1319 <- gdp_growth %>%
  filter(year>2013)%>%
  mutate(cum_growth = cumprod(1 + growth_rate))%>%
  filter(year==2019)%>%
  select(cum_growth) %>% pull()

CPI_1319 <- 107.8645906/98.28708
VSL_1319 = (0.4 * growth_1319-1 * 9 + 9)*CPI_1319

#### BEN MAP GROWTH FACTORS

benmap_growth <- fread("./EPA Standard Income Growth.csv", stringsAsFactors = F)%>%
  filter(EndpointGroup %in% "Mortality")

fread("./FRED_gdp_cap.csv", stringsAsFactors = F)%>%
  drop_na(GDP_per_cap_2017)%>%
  mutate(year = as.integer(str_sub(observation_date,1,4)),
         gdp_cap_1995 = ifelse(year==1995,GDP_per_cap_2017,NA))%>%
  fill(gdp_cap_1995, .direction = "downup")%>%
  mutate(gdpcap_growth_95 = GDP_per_cap_2017/gdp_cap_1995)%>%
  filter(year>1994)

