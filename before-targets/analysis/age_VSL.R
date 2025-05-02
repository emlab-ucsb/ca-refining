# CalEPA: Age-based VSL based on Aldy and Viscusi (2008)
# vincent.thivierge@uottawa.ca
# created: 05/02/2025
# updated: 05/02/2025

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

gdp_growth <- fread("./BEA_GDP.csv", stringsAsFactors = F)%>%
  mutate(growth_rate = (GDP_2017/lag(GDP_2017))-1)
  
growth_rates <- gdp_growth %>%
  filter(year>2000)%>%
  mutate(cum_growth = cumprod(1 + growth_rate))

## Combine

growth_rates %>% 
  group_by(year)%>%
  cbind(age_vsl_data)

age_vsl_data%>%
  left_join(growth_rates, by = c("year" = "year"))%>%
  mutate(
    VSL = future_WTP(
      income_elasticity_mort,
      (cum_growth - 1),
      age_vsl_2000))
      
# IS IT IN THE RIGHT DOLLARS? STILL 2000 DOLLARS? PROBABLY?!?!?!
# what about relative to the average vsp we are using^!^!^!^
  