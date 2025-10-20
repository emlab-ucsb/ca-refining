main_path <- "/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/"

tar_load(annual_labor)
tar_load(county_pop_ratios)

labor_tmp <- annual_labor %>%
  mutate(scenario = paste0(demand_scenario, "-", refining_scenario))


ggplot(labor_tmp, aes(x = year, y = revenue / 1e9, color = county)) +
  geom_line() +
  facet_wrap(~scenario) +
  labs(
    x = NULL,
    y = "Revenue (USD billion)",
    color = NULL
  ) +
  theme_line

## dac
## --------------------------------------------------------------

tar_load(ref_labor_demog_yr)

dac_df <- copy(ref_labor_demog_yr)

## change scenario names, factor
dac_df[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
dac_df[, scenario := gsub("LC1.", "Low ", scenario)]

## scenarios for filtering
remove_scen <- c("Low demand - historic production")

## add scenario title
dac_df[, scenario_title := str_replace(scenario, " - ", "\n")]

## refactor
dac_df$scenario_title <- factor(dac_df$scenario_title, levels = c(
  "BAU demand\nhistoric production",
  "BAU demand\nhistoric exports",
  "BAU demand\nlow exports",
  "Low demand\nhistoric exports",
  "Low demand\nlow exports",
  "Low demand\nhistoric production"
))


## refactor
dac_df$scenario <- factor(dac_df$scenario, levels = c(
  "BAU demand - historic production",
  "BAU demand - historic exports",
  "BAU demand - low exports",
  "Low demand - historic exports",
  "Low demand - low exports",
  "Low demand - historic production"
))

dac_fig <- ggplot(dac_df %>% filter(
  !scenario %in% remove_scen,
  demo_cat == "DAC"
), aes(x = year, y = demo_emp / 1000, lty = title, color = county)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
  facet_grid(demo_cat ~ scenario_title) +
  labs(
    x = NULL,
    y = "Labor: FTE jobs (thousand)"
  ) +
  # ylim(c(0, 35)) +
  scale_x_continuous(
    breaks = c(2020, 2045), # Specify tick mark positions
    labels = c(2020, 2045)
  ) + # Specify tick mark labels
  theme_line +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
    # strip.text.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    # axis.text.x = element_blank(),
    axis.ticks.length.y = unit(0.1, "cm"),
    axis.ticks.length.x = unit(0.1, "cm")
  )

ggsave(
  plot = dac_fig,
  device = "png",
  filename = "dac_labor_levels.png",
  path = paste0(main_path, "outputs/academic-out/refining/figures/2022-12-update/labor/"),
  width = 12,
  height = 4,
  dpi = 600
)



## gaps
## -------------------------------------------------------------

dac_gaps_df <- copy(ref_labor_demog_yr)

## change scenario names, factor
dac_gaps_df[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
dac_gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

## scenarios for filtering
remove_scen <- c("Low demand - historic production", "BAU demand - historic production")

## refactor
dac_gaps_df[, scenario_title := scenario]
dac_gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

dac_gaps_df$scenario <- factor(dac_gaps_df$scenario, levels = c(
  "BAU demand - historic production",
  "BAU demand - historic exports",
  "BAU demand - low exports",
  "Low demand - historic exports",
  "Low demand - low exports",
  "Low demand - historic production"
))

dac_gaps_df$scenario_title <- factor(dac_gaps_df$scenario_title, levels = c(
  "BAU demand\nhistoric production",
  "BAU demand\nhistoric exports",
  "BAU demand\nlow exports",
  "Low demand\nhistoric exports",
  "Low demand\nlow exports",
  "Low demand\nhistoric production"
))
## filter for dac
dac_gaps_df <- dac_gaps_df[demo_cat == "DAC"]


## calculate gaps (BAU - scenario)
dac_bau_gaps_df <- dac_gaps_df[scenario == "BAU demand - historic production"]
dac_bau_gaps_df <- dac_bau_gaps_df[, c("county", "year", "demo_cat", "demo_group", "title", "demo_emp")]
setnames(dac_bau_gaps_df, "demo_emp", "bau_demo_emp")

dac_gaps_df <- merge(dac_gaps_df, dac_bau_gaps_df,
  by = c("county", "year", "demo_cat", "demo_group", "title"),
  all.x = T
)

dac_gaps_df[, gap := demo_emp - bau_demo_emp]

## fig
dac_gap_fig_b <- ggplot(dac_gaps_df %>% filter(
  !scenario %in% remove_scen,
  demo_cat == "DAC"
), aes(x = year, y = gap / 1000, lty = title, color = county)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
  facet_grid(demo_cat ~ scenario_title) +
  labs(
    x = NULL,
    y = "Labor: FTE jobs difference from reference (thousand)"
  ) +
  # ylim(c(-0.31, 0)) +
  scale_x_continuous(
    breaks = c(2020, 2045), # Specify tick mark positions
    labels = c(2020, 2045)
  ) + # Specify tick mark labels
  theme_line +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
    # strip.text.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    # axis.text.x = element_blank(),
    axis.ticks.length.y = unit(0.1, "cm"),
    axis.ticks.length.x = unit(0.1, "cm")
  )

ggsave(
  plot = dac_gap_fig_b,
  device = "png",
  filename = "dac_labor_gaps.png",
  path = paste0(main_path, "outputs/academic-out/refining/figures/2022-12-update/labor/"),
  width = 12,
  height = 4,
  dpi = 600
)
