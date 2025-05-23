### Cumulative NPV for health

# setwd("G://Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures/2024-08-update/fig-csv-files")
setwd("H://Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures/2024-08-update/fig-csv-files")

cum_h <- fread("state_npv_fig_inputs_health.csv", stringsAsFactors = F)

cum_h %>%
  filter(metric %in% "avoided_health_cost") %>%
  filter(scen_id %in% c("BAU historical production", "BAU historical exports", "LC1 low exports")) %>%
  select(scen_id, value, unit_desc, unit)

# cumulative mortality and pop

# cum_m <- fread("cumulative_avoided_mortality.csv"  , stringsAsFactors = F) ; cum_m
#
# cum_m%>%
#   filter(demo_cat %in% c("Race"))%>%
#   group_by(scen_id)%>%
#   summarise(cumul_mort_level = sum(cumul_mort_level ))
# filter(scen_id %in% c("BAU historical production","LC1 historical exports","LC1 low exports", "BAU historical exports"))

# cumulative pop

# fread("state_disaggregated_npv_pc_fig_inputs.csv"  , stringsAsFactors = F)

### SENSITIVITY ANALYSIS TO DIFF BETA

# Krewski et al Beta (0.0058) and SE (0.0009628)

# 90% CI [5 percentile; 95 percentile]

beta_95 <- 0.0058 + 1.64 * 0.000963
beta_95
beta_5 <- 0.0058 - 1.64 * 0.000963
beta_5

## High

setwd("H://Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures/2024-08-beta-adj-high/fig-csv-files")
cum_h <- fread("state_npv_fig_inputs_health.csv", stringsAsFactors = F)

cum_h %>%
  filter(metric %in% "avoided_health_cost") %>%
  filter(scen_id %in% c("BAU historical production", "BAU historical exports", "LC1 low exports")) %>%
  select(scen_id, value, unit_desc, unit)


## High

setwd("H://Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures/2024-08-beta-adj-low/fig-csv-files")
cum_h <- fread("state_npv_fig_inputs_health.csv", stringsAsFactors = F)

cum_h %>%
  filter(metric %in% "avoided_health_cost") %>%
  filter(scen_id %in% c("BAU historical production", "BAU historical exports", "LC1 low exports")) %>%
  select(scen_id, value, unit_desc, unit)
