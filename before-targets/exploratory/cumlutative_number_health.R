### Cumulative NPV for health

setwd("G://Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures/2024-08-update/fig-csv-files")

cum_h <- fread("state_npv_fig_inputs_health.csv"  , stringsAsFactors = F)

cum_h%>%
  filter(metric %in% "avoided_health_cost")%>%
  filter(scen_id %in% c("BAU historical production","LC1 historical exports","LC1 low exports"))%>%
  select(scen_id,value,unit_desc,unit)
