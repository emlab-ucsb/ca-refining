### Cumulative mortality

cum_m <- fread("cumulative_avoided_mortality.csv"  , stringsAsFactors = F) ; cum_m

cum_m%>%
  filter(demo_cat %in% c("Race"))%>%
  group_by(scen_id)%>%
  summarise(cumul_mort_level = sum(cumul_mort_level ))
filter(scen_id %in% c("BAU historical production","LC1 historical exports","LC1 low exports", "BAU historical exports"))
