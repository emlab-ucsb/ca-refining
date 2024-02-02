## health and labor figures

## NPV figure
plot_npv_health_labor <- function(main_path,
                                  refining_mortality,
                                  state_ghg_output,
                                  dt_ghg_2019,
                                  annual_labor) {

  npv_df <- refining_mortality %>% as.data.table()

  ## state level
  state_npv_df <- npv_df[, .(sum_cost_2019_pv = sum(cost_2019_PV), ## constant VSL
                             sum_cost_pv = sum(cost_PV)), ## changing VSL
                             by = .(scen_id, demand_scenario, refining_scenario)]

  ## add column
  state_npv_df[, sum_cost_2019_pv_b := sum_cost_2019_pv / 1e9]
  state_npv_df[, sum_cost_pv_b := sum_cost_pv / 1e9]


  ## add ghg emission reduction
  ## 2019 ghg 
  ghg_2019_val <- dt_ghg_2019$mtco2e[1]
  
  ## 2045 vs 2019 ghg
  ghg_2045 <- state_ghg_output[year == 2045 & source == "total"]
  setnames(ghg_2045, "value", "ghg_kg")
  ghg_2045[, ghg_2045 := (ghg_kg / 1000) / 1e6]
  ghg_2045[, ghg_2019 := ghg_2019_val]
  ghg_2045[, perc_diff := (ghg_2045 - ghg_2019) / ghg_2019]
  
  perc_diff_df <- ghg_2045[, .(demand_scenario, refining_scenario, ghg_2045, ghg_2019, perc_diff)]
  
  ## summarize by scenario, filter for total
  state_ghg_df <- state_ghg_output[source == "total", .(total_ghg = sum(value)),
                                       by = .(demand_scenario, refining_scenario)]

  state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

  ## reference
  ref_df <- state_ghg_df[demand_scenario == "BAU" & refining_scenario == "historic production", .(total_ghg_mmt)]
  setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
  ref_value <- ref_df$ref_ghg_mmt[1]
  
  ## merge with summarized df
  state_ghg_df[, ref_ghg := ref_value]
  state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]
  
  ## merge with health
  health_ghg_df <- merge(state_npv_df, state_ghg_df[, .(demand_scenario, refining_scenario, total_ghg_mmt, ref_ghg, avoided_ghg)],
                         by = c("demand_scenario", "refining_scenario"),
                         all.x = T)
  
  ## summarize labor for state
  state_labor <- annual_labor[, .(sum_total_emp = sum(total_emp),
                                  sum_total_comp_pv = sum(total_comp_PV)),
                              by = .(demand_scenario, refining_scenario)]
  
  
  ## ref labor
  ref_labor <- state_labor[demand_scenario == "BAU" & refining_scenario == "historic production"]
  setnames(ref_labor, c("sum_total_emp", "sum_total_comp_pv"), c("ref_total_emp", "ref_total_comp_pv"))
  
  ## add values to labor
  state_labor[, `:=` (ref_total_emp = ref_labor$ref_total_emp[1],
                       ref_total_comp_pv = ref_labor$ref_total_comp_pv[1])]
  
  state_labor[, forgone_wages_bil := (sum_total_comp_pv - ref_total_comp_pv) / 1e9]

  ## merge with health and ghg
  health_labor_ghg_df <- merge(health_ghg_df, state_labor[, .(demand_scenario, refining_scenario, sum_total_comp_pv, ref_total_comp_pv, forgone_wages_bil)],
                         by = c("demand_scenario", "refining_scenario"),
                         all.x = T)
  
  ## add ghg perc reduction
  health_labor_ghg_df <- merge(health_labor_ghg_df, perc_diff_df,
                               by = c("demand_scenario", "refining_scenario"),
                               all.x = T)
  
  ## prepare to plot
  plot_df <- health_labor_ghg_df[, .(scen_id, demand_scenario, refining_scenario, sum_cost_pv_b,
                                                                         sum_cost_2019_pv_b, forgone_wages_bil, avoided_ghg, perc_diff)]

  setnames(plot_df, "perc_diff", "ghg_perc_diff")
  
  ## add values / avoided ghgs
  plot_df[, avoided_health_cost := sum_cost_2019_pv_b * -1]
  plot_df[, avoided_health_cost_annual_vsl := sum_cost_pv_b * -1]
  plot_df[, sum_cost_2019_pv_b := NULL]
  plot_df[, sum_cost_pv_b := NULL]
  
  plot_df[, `:=` (avoided_health_cost_ghg = avoided_health_cost / avoided_ghg,
                  avoided_health_cost_ghg_vsl2 = avoided_health_cost_annual_vsl / avoided_ghg,
                  forgone_wages_bil_ghg = forgone_wages_bil / avoided_ghg)]
  
  plot_df_health <- plot_df %>%
    select(scen_id, demand_scenario, refining_scenario, ghg_perc_diff, avoided_health_cost, avoided_health_cost_annual_vsl,
           avoided_health_cost_ghg, avoided_health_cost_ghg_vsl2) %>%
    pivot_longer(avoided_health_cost:avoided_health_cost_ghg_vsl2, names_to = "metric", values_to = "value")
  
  ## add column for vsl
  plot_df_health <- plot_df_health %>%
    mutate(segment = "health",
           unit_desc = ifelse(metric == "avoided_health_cost", "USD billion (2019 VSL)",
                            ifelse(metric == "avoided_health_cost_annual_vsl", "USD billion (annual VSL)",
                                   ifelse(metric == "avoided_health_cost_ghg", "USD billion per GHG (2019 VSL)", "USD billion per GHG (annual VSL)"))),
           metric = ifelse(metric %in% c("avoided_health_cost", "avoided_health_cost_annual_vsl"), "avoided_health_cost", "avoided_health_cost_ghg"))
  
  
  plot_df_labor <- plot_df %>%
    select(scen_id, demand_scenario, refining_scenario, ghg_perc_diff, forgone_wages_bil, forgone_wages_bil_ghg) %>%
    pivot_longer(forgone_wages_bil:forgone_wages_bil_ghg, names_to = "metric", values_to = "value") %>%
    mutate(segment = "labor",
           unit_desc = ifelse(metric == "forgone_wages_bil", "USD billion", "USD billion per GHG"))
  
  plot_df_long <- rbind(plot_df_health, plot_df_labor)
  
  plot_df_long <- plot_df_long %>%
    mutate(title = ifelse(metric == "avoided_health_cost", "Health: avoided mortality",
                                  ifelse(metric == "avoided_health_cost_ghg", "Health: avoided mortality per avoided GHG",
                                         ifelse(metric == "forgone_wages_bil", "Labor: forgone wages", "Labor: forgone wages per avoided GHG"))))
  
  plot_df_long$title <- factor(plot_df_long$title, levels = c("Health: avoided mortality", "Labor: forgone wages", 
                                                                "Health: avoided mortality per avoided GHG", "Labor: forgone wages per avoided GHG"))
  
  ## rename
  setDT(plot_df_long)
  plot_df_long[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  plot_df_long[, scenario := gsub('LC1.', 'Low ', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]
  
  ## refactor
  plot_df_long$scenario <- factor(plot_df_long$scenario, levels = c('BAU demand - historic production',
                                                                    'BAU demand - historic exports', 
                                                                    'BAU demand - low exports', 
                                                                    'Low demand - historic exports',
                                                                    'Low demand - low exports',
                                                                    'Low demand - historic production'))
  
  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_long[, value := fifelse(metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"), value * 1000, value)]
  plot_df_long[, metric := fifelse(metric =="forgone_wages_bil_ghg", "forgone_wages_ghg", metric)]
  plot_df_long[, unit := fifelse(metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"), 
                                 "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)", 
                                 "NPV (2019 USD billion)")]
  
  ## save figure inputs
  fwrite(plot_df_long, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_npv_fig_inputs.csv"))
  
  
  ## scenarios for filtering
  remove_scen <- c('LC1 historic production', 'BAU historic production')
  bau_scen <- 'BAU historic production'
  
  ## make the plot
  ## ---------------------------------------------------
  
  ## color for refining scenario
  refin_colors <- c('historic exports' = '#2F4858', 'historic production' = '#F6AE2D', 'low exports' = '#F26419')
  
  # # fig
  # fig_benefit_x_metric <- ggplot(plot_df_long, aes(x = ghg_perc_diff * - 100, y = value, color = refining_scenario, shape = demand_scenario)) +
  #   geom_point(size = 4, alpha = 0.8) +
  #   geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
  #   geom_vline(xintercept = 0, color = "darkgray", linewidth = 0.5) +
  #   labs(color = "Refining scenario",
  #        shape = "Demand scenario",
  #        y = "NPV USD 2019",
  #        x = "GHG emissions reduction target (%, 2045 vs 2019)",) +
  #   facet_wrap(~title, scales = "free", ncol = 2) +
  #   # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  #   # scale_x_continuous(limits = c(0, NA)) +
  #   # scale_color_manual(values = policy_colors_subset) +
  #   theme_line +
  #   # theme_bw() +
  #   theme(legend.position = "bottom",
  #         # legend.box = "vertical",
  #         # legend.key.width= unit(1, 'cm'),
  #         axis.text = element_text(size = 14),
  #         strip.text = element_text(size = 14),
  #         axis.text.x = element_text(vjust = 0.5, hjust = 1),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm')) 
  # 
  # fig_benefit_x_metric

  
  ## revised version, make them separately
  ## -------------------------------------------------------------------
  
  hist_prod = as.data.table(plot_df_long %>% filter(scen_id == bau_scen,
                                                    unit == "NPV (2019 USD billion)",
                                                    unit_desc == "USD billion (2019 VSL)"))
  
  fig_bxm_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Health: avoided mortality", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              title == "Health: avoided mortality",
                                              unit == "NPV (2019 USD billion)",
                                              unit_desc == "USD billion (2019 VSL)",
                                              !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value,  color = refining_scenario, shape = demand_scenario),
               size = 3, alpha = 0.8) +
    labs(color = "Refing scenario",
         shape = "Demand scenario",
         title = "A. Health: avoided mortality",
         y = "NPV (2019 USD billion)",
         x = NULL) +
    ylim(0, 25) +
    xlim(0, 80) +
    scale_color_manual(values = refin_colors) +
    theme_line +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm')) 
  
  fig_bxm_b <- ggplot() + 
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                        title == "Labor: forgone wages",
                                        unit == "NPV (2019 USD billion)",
                                        !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    labs(color = "Policy",
         title = "B. Labor: forgone wages",
         y = NULL,
         x = NULL) +
    ylim(-25, 0) +
    xlim(0, 80) +
    scale_color_manual(values = refin_colors) +
    theme_line +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm')) 
  
  # fig_bxm_c <- ggplot() +
  #   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  #   geom_vline(xintercept = hist_prod[title == "Climate: avoided damage", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
  #   geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
  #                                       title == "Climate: avoided damage",
  #                                       unit == "NPV (2019 USD billion)",
  #                                       !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  #   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  #   labs(color = "Policy",
  #        title = "C. Climate: avoided damage",
  #        y = NULL,
  #        x = NULL) +
  #   ylim(-1, 20) +
  #   xlim(0, 80) +
  #   scale_color_manual(values = refin_colors) +
  #   theme_line +
  #   theme(legend.position = "none",
  #         plot.title = element_text(hjust = 0),
  #         axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm')) 
  
  fig_bxm_c <- ggplot() + 
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Health: avoided mortality", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                        title == "Health: avoided mortality per avoided GHG",
                                        unit == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
                                        unit_desc == "USD billion per GHG (2019 VSL)",
                                        !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    labs(color = "Policy",
         title = "C.",
         y = bquote('NPV (2019 USD million)\nper avoided GHG MtCO'[2]~e),
         x = "GHG emissions reduction (%, 2045 vs 2019)") +
    scale_color_manual(values = refin_colors) +
    ylim(0, 125) +
    xlim(0, 80) +
    theme_line +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  fig_bxm_d <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Labor: forgone wages per avoided GHG", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                        title == "Labor: forgone wages per avoided GHG",
                                        unit == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
                                        !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    labs(color = "Policy",
         title = "D.",
         y = NULL,
         # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
         x = "GHG emissions reduction (%, 2045 vs 2019)") +
    scale_color_manual(values = refin_colors) +
    theme_line +
    xlim(0, 80) +
    ylim(-125, 0) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  # fig_bxm_f <- ggplot() +
  #   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  #   geom_vline(xintercept = hist_prod[title == "Health: avoided mortality", ghg_2045_perc_reduction], color = "darkgray", lty = 2) +
  #   geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
  #                                       title == "Climate: avoided damage",
  #                                       measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
  #                                       !refining_scenario == "historic production"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  #   labs(color = "Policy",
  #        title = "F.",
  #        y = NULL,
  #        # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
  #        x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  #   scale_color_manual(values = refin_colors) +
  #   theme_line +
  #   ylim(0, 80) +
  #   xlim(0, 80) +
  #   theme(legend.position = "none",
  #         axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm'))
  # 
  ## extract legend
  # legend_fig <- ggplot() +
  #   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  #   geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
  #                                        title == "Labor: forgone wages",
  #                                        measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = scenario, shape = scenario), size = 3, alpha = 0.8) +
  #   labs(title = "",
  #        y = NULL,
  #        # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
  #        x = "GHG emissions reduction target (%, 2045 vs 2019)",
  #        color = NULL,
  #        shape = NULL) +
  #   # scale_shape_manual(values = c(16, 16, 16, 17, 17)) +
  #   scale_color_manual(name = "",
  #                      labels = c("BAU demand - historic exports",
  #                                 "BAU demand - historic production",
  #                                 "BAU demand - low exports",
  #                                 "Low C. demand - historic exports",
  #                                 "Low C. demand - low exports"),
  #                      values = c("BAU demand - historic exports" = "#2F4858",
  #                                 "BAU demand - historic production" = "#F6AE2D",
  #                                 "BAU demand - low exports" = "#F26419",
  #                                 "Low C. demand - historic exports" = "#2F4858",
  #                                 "Low C. demand - low exports" = "#F26419")) +
  #   scale_shape_manual(name = "",
  #                         labels = c("BAU demand - historic exports",
  #                                    "BAU demand - historic production",
  #                                    "BAU demand - low exports",
  #                                    "Low C. demand - historic exports",
  #                                    "Low C. demand - low exports"),
  #                         values = c(16, 16, 16, 17, 17)) +
  #   theme_line +
  #   theme(legend.position = "bottom",
  #         axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm')) +
  #   guides(color = guide_legend(nrow = 2, byrow = TRUE))
  
  legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                        title == "Labor: forgone wages per avoided GHG",
                                        unit == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
                                        !refining_scenario == "historic production"), 
               aes(x = ghg_perc_diff * -100, y = value, color = scenario, shape = scenario), size = 3, alpha = 0.8) +
    labs(title = "",
         y = NULL,
         # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
         x = "GHG emissions reduction (%, 2045 vs 2019)",
         color = NULL,
         shape = NULL) +
    scale_color_manual(name = "",
                       labels = c("BAU demand - historic exports",
                                  "BAU demand - low exports",
                                  "Low demand - historic exports",
                                  "Low demand - low exports"),
                       values = c("BAU demand - historic exports" = "#2F4858",
                                  "BAU demand - low exports" = "#F26419",
                                  "Low demand - historic exports" = "#2F4858",
                                  "Low demand - low exports" = "#F26419")) +
    scale_shape_manual(name = "",
                       labels = c("BAU demand - historic exports",
                                  "BAU demand - low exports",
                                  "Low demand - historic exports",
                                  "Low demand - low exports"),
                       values = c(16, 16, 17, 17)) +
    theme_line +
    theme(legend.position = "bottom",
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm')) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  
  
  
  legend_fig_3 <- get_legend(
    legend_fig + 
      theme(legend.title = element_text(size = 8),
            legend.text = element_text(size = 8))
    
  )
  
  
  ## combine figure
  ## ---------------------------------
  
  ## shared x axis
  xaxis_lab <- ggdraw() + draw_label("GHG emissions reduction (%, 2045 vs 2019)", size = 7)
  
  fig3_plot_grid <- plot_grid(
    fig_bxm_a,
    fig_bxm_b,
    # fig_bxm_c,
    fig_bxm_c + labs(x = NULL),
    fig_bxm_d + labs(x = NULL),
    # fig_bxm_f+ labs(x = NULL),
    align = 'vh',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 2,
    rel_widths = c(1, 1, 1, 1, 1, 1)
  )
  
  fig3_plot_grid2 <- plot_grid(
    fig3_plot_grid,
    xaxis_lab,
    legend_fig_3,
    align = "v",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 1,
    rel_heights = c(0.85, 0.05, 0.1)
    # rel_widths = c(1, 1),
  )
  
  
 

}


plot_health_levels <- function(main_path,
                               health_grp) {
  
  fig2_df <- copy(health_grp)
  
  ## change scenario names, factor
  fig2_df[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
  # fig2_df[, scenario := gsub('BAU', 'Reference', scenario)]
  fig2_df[, scenario := gsub('LC1.', 'Low ', scenario)]
  
  ## scenarios for filtering
  # remove_scen <- c('LC1 historic production', 'BAU low exports', 'LC1 historic exports')
  remove_scen <- c('LC1 historic production')
  
  ## add scenario title
  fig2_df[, scenario_title := str_replace(scenario, " - ", "\n")]
  
  ## refactor
  fig2_df$scenario_title <- factor(fig2_df$scenario_title, levels = c('BAU demand\nhistoric production',
                                                          'BAU demand\nhistoric exports', 
                                                          'BAU demand\nlow exports', 
                                                          'Low demand\nhistoric exports',
                                                          'Low demand\nlow exports',
                                                          'Low demand\nhistoric production'))
  
  
  ## refactor
  fig2_df$scenario <- factor(fig2_df$scenario, levels = c('BAU demand - historic production',
                                                          'BAU demand - historic exports', 
                                                          'BAU demand - low exports', 
                                                          'Low demand - historic exports',
                                                          'Low demand - low exports',
                                                          'Low demand - historic production'))
  
  
  ## save figure inputs
  fwrite(fig2_df, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_levels_fig_inputs.csv"))
  

  # health_level_fig <- ggplot(fig2_df %>% filter(!scen_id %in% remove_scen), aes(x = year, y = num_over_den, color = group)) +
  #   geom_line(linewidth = 1, alpha = 0.8) +
  #   facet_grid(type ~ scenario) +
  #   labs(x = NULL,
  #        y = "num_over_den") +
  #   theme_line +
  #   theme(legend.position = "bottom",
  #         axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm'))
  
  ##
  
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")
  
  
  health_level_fig_a <- ggplot(fig2_df %>% filter(!scen_id %in% remove_scen,
                                                  title %in% fig_title_vec,
                                                  demo_cat == "Race")  %>%
                                 mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))), 
                               aes(x = year, y = num_over_den, color = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "Asian",
                                  "white"),
                       values = c("#002147",
                                  "#721817",
                                  "#40826D",
                                  "#FFBA00")) +
    labs(x = NULL,
         y = NULL) +
    ylim(c(0, 0.4)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text.x = element_blank(),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figa <- health_level_fig_a + theme(legend.position = "right")
  
  legend_a <- get_legend(
    legend_figa + 
      theme(legend.text = element_text(size = 8)))
  
  ##
  health_level_fig_b <- ggplot(fig2_df %>% filter(!scen_id %in% remove_scen,
                                                  demo_cat == "DAC"), aes(x = year, y = num_over_den, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(x = NULL,
         y = NULL) +
    ylim(c(0, 0.4)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          # strip.text.x = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figb <- health_level_fig_b + theme(legend.position = "right")
  
  legend_b <- get_legend(
    legend_figb + 
      theme(legend.text = element_text(size = 8)))
  
  ##
  health_level_fig_c <- ggplot(fig2_df %>% filter(!scen_id %in% remove_scen,
                                                  demo_cat == "Poverty") %>%
                                 mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))), 
                               aes(x = year, y = num_over_den, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = c("Above poverty line" = "dashed",
                                     "Below poverty line" = "solid")) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(x = NULL,
         y = NULL) +
    ylim(c(0, 0.4)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.key.width = unit(10, "mm"),
          strip.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figc <- health_level_fig_c + theme(legend.position = "right")
  
  legend_c <- get_legend(
    legend_figc + 
      theme(legend.text = element_text(size = 8)))

  ## shared y lab
  yaxis_lab <- ggdraw() + draw_label(expression(paste("Population-weighted PM"[2.5], " (",mu,"g ", m^{-3},")")), 
                                     size = 8, angle = 90)
  

  # ## plot together
  # fig2l_a <- plot_grid(
  #   health_level_fig_a,
  #   legend_a,
  #   align = 'h',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   ncol = 2,
  #   rel_widths = c(0.95, 0.5),
  #   rel_heighs = c(1, 1)
  # )
  # 
  # 
  
  fig2_plot_grid <- plot_grid(
    health_level_fig_b,
    health_level_fig_c,
    health_level_fig_a,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )
  
  fig2_plot_grid2 <- plot_grid(
    yaxis_lab,
    fig2_plot_grid,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )
  
  
  fig2_plot_grid2

}


plot_health_levels_gaps <- function(main_path,
                                    health_grp) {
  
  gaps_df <- copy(health_grp)
  
  ## change scenario names, factor
  gaps_df[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  gaps_df[, scenario := gsub('LC1.', 'Low ', scenario)]
  
  ## scenarios for filtering
  remove_scen <- c('LC1 historic production', 'BAU historic production')

  ## refactor
  gaps_df[, scenario_title := scenario]
  gaps_df[, scenario_title := str_replace(scenario_title, ' - ', '\n')]
  
  gaps_df$scenario <- factor(gaps_df$scenario, levels = c('BAU demand - historic production',
                                                          'BAU demand - historic exports', 
                                                          'BAU demand - low exports', 
                                                          'Low demand - historic exports',
                                                          'Low demand - low exports',
                                                          'Low demand - historic production'))
  
  gaps_df$scenario_title <- factor(gaps_df$scenario_title, levels = c('BAU demand\nhistoric production',
                                                                      'BAU demand\nhistoric exports', 
                                                                      'BAU demand\nlow exports', 
                                                                      'Low demand\nhistoric exports',
                                                                      'Low demand\nlow exports',
                                                                      'Low demand\nhistoric production'))
  
  
  ## calculate gaps (BAU - scenario)
  bau_gaps_df <- gaps_df[scen_id == "BAU historic production"]
  bau_gaps_df <- bau_gaps_df[, c("year", "demo_cat", "demo_group", "title", "num_over_den")]
  setnames(bau_gaps_df, "num_over_den", "bau_num_over_den")
  
  gaps_df <- merge(gaps_df, bau_gaps_df,
                   by = c("year", "demo_cat", "demo_group", "title"),
                   all.x = T)
  
  gaps_df[, gap :=  num_over_den - bau_num_over_den]
  
  ## save figure inputs
  fwrite(gaps_df, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_levels_fig_gaps_inputs.csv"))
  
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")
  
  
  health_gap_fig_a <- ggplot(gaps_df %>% filter(!scen_id %in% remove_scen,
                                                  title %in% fig_title_vec,
                                                  demo_cat == "Race")  %>%
                               mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))), 
                             aes(x = year, y = gap, color = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "Asian",
                                  "white"),
                       values = c("#002147",
                                  "#721817",
                                  "#40826D",
                                  "#FFBA00")) +
    labs(x = NULL,
         y = NULL) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    ylim(c(-0.31, 0)) +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text.x = element_blank(),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figa <- health_gap_fig_a + theme(legend.position = "right")
  
  legend_a <- get_legend(
    legend_figa + 
      theme(legend.text = element_text(size = 8)))
  
  ##
  health_gap_fig_b <- ggplot(gaps_df %>% filter(!scen_id %in% remove_scen,
                                                  demo_cat == "DAC"), aes(x = year, y = gap, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(x = NULL,
         y = NULL) +
    ylim(c(-0.31, 0)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          # strip.text.x = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figb <- health_gap_fig_b + theme(legend.position = "right")
  
  legend_b <- get_legend(
    legend_figb + 
      theme(legend.text = element_text(size = 8)))
  
  ##
  health_gap_fig_c <- ggplot(gaps_df %>% 
                               filter(!scen_id %in% remove_scen,
                                                  demo_cat == "Poverty") %>%
                               mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))),
                               aes(x = year, y = gap, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = c("Above poverty line" = "dashed",
                                     "Below poverty line" = "solid")) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(x = NULL,
         y = NULL) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    ylim(c(-0.31, 0)) +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          legend.key.width = unit(10, "mm"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figc <- health_gap_fig_c + theme(legend.position = "right")
  
  legend_c <- get_legend(
    legend_figc + 
      theme(legend.text = element_text(size = 8)))
  
  ## shared y lab
  yaxis_lab <- ggdraw() + draw_label(expression(paste("Population-weighted PM"[2.5], " (",mu,"g ", m^{-3},")", " difference from reference")), 
                                     size = 8, angle = 90)
  
  
  # ## plot together
  # fig2l_a <- plot_grid(
  #   health_level_fig_a,
  #   legend_a,
  #   align = 'h',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   ncol = 2,
  #   rel_widths = c(0.95, 0.5),
  #   rel_heighs = c(1, 1)
  # )
  # 
  # 
  
  gaps_plot_grid <- plot_grid(
    health_gap_fig_b,
    health_gap_fig_c,
    health_gap_fig_a,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )
  
  gaps_plot_grid2 <- plot_grid(
    yaxis_lab,
    gaps_plot_grid,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )
  
  
  gaps_plot_grid2
  
}

###########################################################################
## plot labor 
###########################################################################

plot_labor_levels <- function(main_path,
                              ref_labor_demog_yr) {

  fig2_l_df <- copy(ref_labor_demog_yr)

  ## change scenario names, factor
  fig2_l_df[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
  # fig2_l_df[, scenario := gsub('BAU', 'Reference', scenario)]
  fig2_l_df[, scenario := gsub('LC1.', 'Low ', scenario)]

  ## scenarios for filtering
  # remove_scen <- c('LC1 historic production', 'BAU low exports', 'LC1 historic exports')
  remove_scen <- c('Low demand - historic production')

  ## add scenario title
  fig2_l_df[, scenario_title := str_replace(scenario, " - ", "\n")]

  ## refactor
  fig2_l_df$scenario_title <- factor(fig2_l_df$scenario_title, levels = c('BAU demand\nhistoric production',
                                                                          'BAU demand\nhistoric exports',
                                                                          'BAU demand\nlow exports',
                                                                          'Low demand\nhistoric exports',
                                                                          'Low demand\nlow exports',
                                                                          'Low demand\nhistoric production'))


  ## refactor
  fig2_l_df$scenario <- factor(fig2_l_df$scenario, levels = c('BAU demand - historic production',
                                                              'BAU demand - historic exports',
                                                              'BAU demand - low exports',
                                                              'Low demand - historic exports',
                                                              'Low demand - low exports',
                                                              'Low demand - historic production'))
  
  ## sum for state
  fig2_l_df <- fig2_l_df[, .(sum_demo_emp = sum(demo_emp)),
                         by = .(year, demand_scenario, refining_scenario,
                                scenario, scenario_title, demo_cat, demo_group, title)]
  
  
  ## select columns
  fig2_l_df <- fig2_l_df[, .(year, demand_scenario, refining_scenario,
                             scenario, scenario_title, demo_cat, demo_group, title, sum_demo_emp)]


  ## save figure inputs
  fwrite(fig2_l_df, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_levels_labor_fig_inputs.csv"))

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")


  labor_level_fig_a <- ggplot(fig2_l_df %>% filter(!scenario %in% remove_scen,
                                                  title %in% fig_title_vec,
                                                  demo_cat == "Race")  %>%
                                 mutate(title = factor(title, levels = c("Black", "Asian", "white", "Hispanic"))),
                               aes(x = year, y = sum_demo_emp / 1000, color = title, group = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Asian",
                                  "white",
                                  "Hispanic"),
                       values = c("#002147",
                                  "#40826D",
                                  "#FFBA00",
                                  "#721817")) +
    labs(x = NULL,
         y = NULL) +
    ylim(c(0, 20)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text.x = element_blank(),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  # legend_figa <- labor_level_fig_a + theme(legend.position = "right")
  # 
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_b <- ggplot(fig2_l_df %>% filter(!scenario %in% remove_scen,
                                                  demo_cat == "DAC"), aes(x = year, y = sum_demo_emp / 1000, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(x = NULL,
         y = NULL) +
    ylim(c(0, 35)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          # strip.text.x = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  # legend_figb <- labor_level_fig_b + theme(legend.position = "right")
  # 
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_c <- ggplot(fig2_l_df %>% filter(!scenario %in% remove_scen,
                                                  demo_cat == "Poverty") %>%
                                 mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))),
                               aes(x = year, y = sum_demo_emp / 1000, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = c("Above poverty line" = "dashed",
                                     "Below poverty line" = "solid")) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(x = NULL,
         y = NULL) +
    ylim(c(0, 45)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.key.width = unit(10, "mm"),
          strip.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  # legend_figc <- health_level_fig_c + theme(legend.position = "right")
  # 
  # legend_c <- get_legend(
  #   legend_figc +
  #     theme(legend.text = element_text(size = 8)))

  ## shared y lab
  yaxis_lab <- ggdraw() + draw_label("Labor: FTE jobs (thousand)", size = 8, angle = 90)


  # ## plot together
  # fig2l_a <- plot_grid(
  #   health_level_fig_a,
  #   legend_a,
  #   align = 'h',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   ncol = 2,
  #   rel_widths = c(0.95, 0.5),
  #   rel_heighs = c(1, 1)
  # )
  #
  #

  fig2_l_plot_grid <- plot_grid(
    labor_level_fig_b,
    labor_level_fig_c,
    labor_level_fig_a,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )

  fig2_l_plot_grid2 <- plot_grid(
    yaxis_lab,
    fig2_l_plot_grid,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )


  fig2_l_plot_grid2

}


plot_labor_levels_gaps <- function(main_path,
                                    ref_labor_demog_yr) {

  l_gaps_df <- copy(ref_labor_demog_yr)

  ## change scenario names, factor
  l_gaps_df[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  l_gaps_df[, scenario := gsub('LC1.', 'Low ', scenario)]

  ## scenarios for filtering
  remove_scen <- c('Low demand - historic production', 'BAU demand - historic production')

  ## refactor
  l_gaps_df[, scenario_title := scenario]
  l_gaps_df[, scenario_title := str_replace(scenario_title, ' - ', '\n')]

  l_gaps_df$scenario <- factor(l_gaps_df$scenario, levels = c('BAU demand - historic production',
                                                          'BAU demand - historic exports',
                                                          'BAU demand - low exports',
                                                          'Low demand - historic exports',
                                                          'Low demand - low exports',
                                                          'Low demand - historic production'))

  l_gaps_df$scenario_title <- factor(l_gaps_df$scenario_title, levels = c('BAU demand\nhistoric production',
                                                                      'BAU demand\nhistoric exports',
                                                                      'BAU demand\nlow exports',
                                                                      'Low demand\nhistoric exports',
                                                                      'Low demand\nlow exports',
                                                                      'Low demand\nhistoric production'))

  ## sum for state
  l_gaps_df <- l_gaps_df[, .(sum_demo_emp = sum(demo_emp)),
                         by = .(year, demand_scenario, refining_scenario,
                                scenario, scenario_title, demo_cat, demo_group, title)]
  
  
  ## select columns
  l_gaps_df <- l_gaps_df[, .(year, demand_scenario, refining_scenario,
                             scenario, scenario_title, demo_cat, demo_group, title, sum_demo_emp)]
  

  ## calculate gaps (BAU - scenario)
  l_bau_gaps_df <- l_gaps_df[scenario == "BAU demand - historic production"]
  l_bau_gaps_df <- l_bau_gaps_df[, c("year", "demo_cat", "demo_group", "title", "sum_demo_emp")]
  setnames(l_bau_gaps_df, "sum_demo_emp", "bau_sum_demo_emp")

  l_gaps_df <- merge(l_gaps_df, l_bau_gaps_df,
                   by = c("year", "demo_cat", "demo_group", "title"),
                   all.x = T)

  l_gaps_df[, gap :=  sum_demo_emp - bau_sum_demo_emp]

  ## save figure inputs
  fwrite(l_gaps_df, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_labor_levels_fig_gaps_inputs.csv"))

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")


  labor_gap_fig_a <- ggplot(l_gaps_df %>% filter(!scenario %in% remove_scen,
                                                title %in% fig_title_vec,
                                                demo_cat == "Race")  %>%
                               mutate(title = factor(title, levels = c("Black", "Asian", "white", "Hispanic"))),
                             aes(x = year, y = gap / 1000, color = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "white",
                                  "Asian"),
                       values = c("#002147",
                                  "#721817",
                                  "#FFBA00",
                                  "#40826D")) +
    labs(x = NULL,
         y = NULL) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    # ylim(c(-0.31, 0)) +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text.x = element_blank(),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  # legend_figa <- health_gap_fig_a + theme(legend.position = "right")
  # 
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_gap_fig_b <- ggplot(l_gaps_df %>% filter(!scenario %in% remove_scen,
                                                demo_cat == "DAC"), aes(x = year, y = gap / 1000, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(x = NULL,
         y = NULL) +
    # ylim(c(-0.31, 0)) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          # strip.text.x = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          # axis.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  # legend_figb <- health_gap_fig_b + theme(legend.position = "right")
  # 
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_gap_fig_c <- ggplot(l_gaps_df %>%
                               filter(!scenario %in% remove_scen,
                                      demo_cat == "Poverty") %>%
                               mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))),
                             aes(x = year, y = gap / 100, lty = title)) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = c("Above poverty line" = "dashed",
                                     "Below poverty line" = "solid")) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(x = NULL,
         y = NULL) +
    scale_x_continuous(breaks = c(2020, 2045),  # Specify tick mark positions
                       labels = c(2020, 2045)) +  # Specify tick mark labels
    theme_line +
    # ylim(c(-0.31, 0)) +
    theme_line +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          legend.key.width = unit(10, "mm"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text.x = element_blank(),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
# 
#   legend_figc <- health_gap_fig_c + theme(legend.position = "right")
# 
#   legend_c <- get_legend(
#     legend_figc +
#       theme(legend.text = element_text(size = 8)))

  ## shared y lab
  yaxis_lab <- ggdraw() + draw_label("Labor: FTE jobs difference from reference (thousand)", size = 8, angle = 90)


  # ## plot together
  # fig2l_a <- plot_grid(
  #   health_level_fig_a,
  #   legend_a,
  #   align = 'h',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   ncol = 2,
  #   rel_widths = c(0.95, 0.5),
  #   rel_heighs = c(1, 1)
  # )
  #
  #

  l_gaps_plot_grid <- plot_grid(
    labor_gap_fig_b,
    labor_gap_fig_c,
    labor_gap_fig_a,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )

  l_gaps_plot_grid2 <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid,
    align = 'v',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )


  l_gaps_plot_grid2

}
# 




############################################################################
############################################################################

plot_hl_levels_df <- function(main_path,
                              ref_mortality_demog,
                              ref_labor_demog,
                              state_ghg_output,
                              dt_ghg_2019) {

   health_df <- copy(ref_mortality_demog)

   ## group by scenario, demo_cat, demo_group, title, and sum
   health_df <- health_df[, .(sum_cost_2019_pv = sum(demo_cost_2019_PV, na.rm = T), ## constant VSL
                              sum_cost_pv = sum(demo_cost_PV,  na.rm = T)), ## changing VSL
                          by = .(scen_id, demand_scenario, refining_scenario, demo_cat, demo_group, title)]

   ## multiply by -1
   health_df[, sum_cost_2019_pv := sum_cost_2019_pv * -1]
   health_df[, sum_cost_pv := sum_cost_pv * -1]

   ## add ghg emission reduction ----------------------------
   ## 2019 ghg
   ghg_2019_val <- dt_ghg_2019$mtco2e[1]

   ## 2045 vs 2019 ghg
   ghg_2045 <- state_ghg_output[year == 2045 & source == "total"]
   setnames(ghg_2045, "value", "ghg_kg")
   ghg_2045[, ghg_2045 := (ghg_kg / 1000) / 1e6]
   ghg_2045[, ghg_2019 := ghg_2019_val]
   ghg_2045[, perc_diff := (ghg_2045 - ghg_2019) / ghg_2019]

   perc_diff_df <- ghg_2045[, .(demand_scenario, refining_scenario, ghg_2045, ghg_2019, perc_diff)]

   ## summarize by scenario, filter for total
   state_ghg_df <- state_ghg_output[source == "total", .(total_ghg = sum(value)),
                                    by = .(demand_scenario, refining_scenario)]

   state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

   ## reference
   ref_df <- state_ghg_df[demand_scenario == "BAU" & refining_scenario == "historic production", .(total_ghg_mmt)]
   setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
   ref_value <- ref_df$ref_ghg_mmt[1]

   ## merge with summarized df
   state_ghg_df[, ref_ghg := ref_value]
   state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]

   ## merge with health
   health_ghg_df <- merge(health_df, state_ghg_df[, .(demand_scenario, refining_scenario, total_ghg_mmt, ref_ghg, avoided_ghg)],
                          by = c("demand_scenario", "refining_scenario"),
                          all.x = T)
   
   ## labor
   labor_df <- copy(ref_labor_demog)
   
   ## ref labor
   ref_labor <- labor_df[demand_scenario == "BAU" & refining_scenario == "historic production"]
   setnames(ref_labor, c("sum_demo_emp", "sum_demo_comp_pv"), c("ref_total_emp", "ref_total_comp_pv"))
   ref_labor <- ref_labor[, .(demo_cat, demo_group, title, ref_total_emp, ref_total_comp_pv)]
   
   ## add values to labor
   labor_df <- merge(labor_df, ref_labor,
                     by = c("demo_cat", "demo_group", "title"))
   
   ## calculate difference
   labor_df[, forgone_wages := (sum_demo_comp_pv - ref_total_comp_pv)]
   
   ## merge with health and ghg
   health_labor_ghg_df <- merge(health_ghg_df, labor_df[, .(demand_scenario, refining_scenario, demo_cat, demo_group, title, sum_demo_comp_pv, ref_total_comp_pv, forgone_wages)],
                                by = c("demand_scenario", "refining_scenario", "demo_cat", "demo_group", "title"),
                                all.x = T)
   
   ## add ghg perc reduction
   health_labor_ghg_df <- merge(health_labor_ghg_df, perc_diff_df,
                                by = c("demand_scenario", "refining_scenario"),
                                all.x = T)
   
   ## prepare to plot
   plot_df <- health_labor_ghg_df[, .(scen_id, demand_scenario, refining_scenario, demo_cat, demo_group, title,
                                      sum_cost_pv, sum_cost_2019_pv, forgone_wages, avoided_ghg, perc_diff)]
   
   setnames(plot_df, "perc_diff", "ghg_perc_diff")
   
   ## pivot longer
   plot_df <- plot_df %>%
     select(scen_id:title, ghg_perc_diff, sum_cost_pv, sum_cost_2019_pv, forgone_wages) %>%
     pivot_longer(sum_cost_pv:forgone_wages, names_to = "metric", values_to = "value")
   
   ## add column for vsl
   plot_df_health <- plot_df %>%
     filter(metric %in% c("sum_cost_pv", "sum_cost_2019_pv")) %>%
     mutate(segment = "health",
            unit_desc = ifelse(metric == "sum_cost_2019_pv", "USD (2019 VSL)", "USD (annual VSL)"),
            metric_desc = "avoided_health_cost")
          
   plot_df_labor <- plot_df %>%
     filter(metric == "forgone_wages") %>%
     mutate(segment = "labor",
            unit_desc = "USD",
            metric_desc = "forgone_wages")
   
   plot_df_long <- rbind(plot_df_health, plot_df_labor)
   
   plot_df_long <- plot_df_long %>%
     mutate(seg_title = ifelse(segment == "health", "Health: avoided mortality", "Labor: forgone wages"))
   
    ## rename
   setDT(plot_df_long)
   plot_df_long[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
   # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
   plot_df_long[, scenario := gsub('LC1.', 'Low ', scenario)]
   # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
   # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]
   
   ## refactor
   plot_df_long$scenario <- factor(plot_df_long$scenario, levels = c('BAU demand - historic production',
                                                                     'BAU demand - historic exports', 
                                                                     'BAU demand - low exports', 
                                                                     'Low demand - historic exports',
                                                                     'Low demand - low exports',
                                                                     'Low demand - historic production'))
   
   ## titles for plotting
   plot_df_long[, demand_title := ifelse(demand_scenario == "BAU", "BAU demand", "Low demand")]
   plot_df_long[, scen_title := paste0(demand_title, "\n", str_to_sentence(refining_scenario))]
   
   plot_df_long$scen_title <- factor(plot_df_long$scen_title, levels = c('BAU demand\nHistoric production',
                                                                        'BAU demand\nHistoric exports', 
                                                                        'BAU demand\nLow exports', 
                                                                        'Low demand\nHistoric exports',
                                                                        'Low demand\nLow exports',
                                                                        'Low demand\nHistoric production'))
   
   ## save figure inputs
   fwrite(plot_df_long, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_disaggregated_npv_fig_inputs.csv"))
   
   return(plot_df_long)
   
}
   
 
plot_hl_levels <- function(demographic_npv_df) {
  
  plot_df_long <- copy(demographic_npv_df)
  
   ## create the figure ---------------------------------------------
   ##---------------------------------------------------------------
   
   ## scenarios for filtering
   remove_scen <- c('LC1 historic production', 'BAU historic production')
   bau_scen <- 'BAU historic production'
   
   fig_title_vec <- c("Asian", "Black", "Hispanic", "white")
   
   ## health fig - race
   health_level_fig_a <- ggplot() +
     geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
     geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                               demo_cat == "Race",
                                               unit_desc == "USD (2019 VSL)",
                                               title %in% fig_title_vec)  %>%
                  mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))), 
                aes(x = scen_title, y = value / 1e9, color = title),
                size = 3, alpha = 0.8) +
     facet_wrap(~seg_title) +
     scale_color_manual(name = "",
                        labels = c("Black",
                                   "Hispanic",
                                   "Asian",
                                   "white"),
                        values = c("#002147",
                                   "#721817",
                                   "#40826D",
                                   "#FFBA00")) +
     ylim(0, 12) +
     labs(y = "NPV (USD billion)",
          x = NULL,
          color = NULL) +
     theme_line +
     theme(legend.position = "none",
           legend.title = element_blank(),
           axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           axis.ticks.length.y = unit(0.1, 'cm'),
           axis.ticks.length.x = unit(0.1, 'cm'))
   
   legend_figa <- health_level_fig_a + theme(legend.position = "bottom")

   legend_a <- get_legend(
     legend_figa +
       theme(legend.text = element_text(size = 8)))
   
   ## labor fig - race
   labor_level_fig_a <- ggplot() +
     geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
     geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                               demo_cat == "Race",
                                               segment == "labor",
                                               title %in% fig_title_vec)%>%
                  mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))), 
                aes(x = scen_title, y = value / 1e9, color = title),
                size = 3, alpha = 0.8) +
     facet_wrap(~seg_title) +
     scale_color_manual(name = "",
                        labels = c("Black",
                                   "Hispanic",
                                   "Asian",
                                   "white"),
                        values = c("#002147",
                                   "#721817",
                                   "#40826D",
                                   "#FFBA00")) +
     ylim(-12, 0) +
     labs(y = "NPV (USD billion)",
          x = NULL,
          color = NULL) +
     theme_line +
     theme(legend.position = "none",
           legend.title = element_blank(),
           axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           axis.ticks.length.y = unit(0.1, 'cm'),
           axis.ticks.length.x = unit(0.1, 'cm'))

   ## health fig - poverty
   health_level_fig_b <- ggplot() +
     geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                               demo_cat == "Poverty",
                                               unit_desc == "USD (2019 VSL)") %>%
                  mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))), 
                aes(x = scen_title, y = value / 1e9, shape = title),
                color = "black", size = 3, alpha = 0.8) +
     geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
     scale_shape_manual(values = c("Above poverty line" = 19,
                                   "Below poverty line" = 17)) +
     facet_wrap(~seg_title) +
     labs(y = "NPV (USD billion)",
          x = NULL,
          color = NULL) +
     theme_line +
     ylim(0, 20) +
     theme(legend.position = "none",
           legend.title = element_blank(),
           # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           axis.ticks.length.y = unit(0.1, 'cm'),
           axis.ticks.length.x = unit(0.1, 'cm'))
   
   legend_figb <- health_level_fig_b + theme(legend.position = "bottom")
   
   legend_b <- get_legend(
     legend_figb +
       theme(legend.text = element_text(size = 8)))
   
   ## labor fig - poverty
   labor_level_fig_b <- ggplot() +
     geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                               demo_cat == "Poverty",
                                               segment == "labor") %>%
                  mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))), 
                aes(x = scen_title, y = value / 1e9, shape = title),
                color = "black", size = 3, alpha = 0.8) +
     scale_shape_manual(values = c("Above poverty line" = 19,
                                   "Below poverty line" = 17)) +
     geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
     facet_wrap(~seg_title) +
     labs(y = "NPV (USD billion)",
          x = NULL,
          color = NULL) +
     ylim(-20, 0) +
     theme_line +
     theme(legend.position = "none",
           legend.title = element_blank(),
           # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           axis.ticks.length.y = unit(0.1, 'cm'),
           axis.ticks.length.x = unit(0.1, 'cm'))
   
   
   ## health fig - DAC
   health_level_fig_c <- ggplot() +
     geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                               demo_cat == "DAC",
                                               unit_desc == "USD (2019 VSL)"), aes(x = scen_title, y = value / 1e9, shape = title),
                color = "black", size = 3, alpha = 0.8) +
     geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
     scale_shape_manual(values = c("DAC" = 15,
                                   "Non-DAC" = 4)) +
     facet_wrap(~seg_title) +
     labs(y = "NPV (USD billion)",
          x = NULL,
          color = NULL) +
     ylim(0, 15) +
     theme_line +
     theme(legend.position = "none",
           legend.title = element_blank(),
           # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           axis.ticks.length.y = unit(0.1, 'cm'),
           axis.ticks.length.x = unit(0.1, 'cm'))
   
   legend_figc <- health_level_fig_c + theme(legend.position = "bottom")
   
   legend_c <- get_legend(
     legend_figc +
       theme(legend.text = element_text(size = 8)))
   
   ## labor fig - DAC
   labor_level_fig_c <- ggplot() +
     geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                               demo_cat == "DAC",
                                               segment == "labor"), aes(x = scen_title, y = value / 1e9, shape = title),
                color = "black", size = 3, alpha = 0.8) +
     geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
     scale_shape_manual(values = c("DAC" = 15,
                                   "Non-DAC" = 4)) +
     facet_wrap(~seg_title) +
     labs(y = "NPV (USD billion)",
          x = NULL,
          color = NULL) +
     ylim(-15, 0) +
     theme_line +
     theme(legend.position = "none",
           legend.title = element_blank(),
           # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           axis.ticks.length.y = unit(0.1, 'cm'),
           axis.ticks.length.x = unit(0.1, 'cm'))
   
   
   ## combine figure
   ## ---------------------------------

   ## race
   hl_plot_grid_a <- plot_grid(
     health_level_fig_a + theme(strip.text.x = element_blank()),
     labor_level_fig_a + labs(y = NULL) + theme(strip.text.x = element_blank()),
     align = 'vh',
     # labels = c("A", "B", "C", "D", "E", "F"),
     # # labels = 'AUTO',
     # label_size = 10,
     hjust = -1,
     nrow = 1,
     rel_widths = c(1, 1)
   )

   ## add race legend
   hl_plot_grid_a <- plot_grid(
     hl_plot_grid_a,
     legend_a,
     ncol = 1, 
     rel_heights = c(0.95, 0.05)
   )
   
   ## poverty
   hl_plot_grid_b <- plot_grid(
     health_level_fig_b + theme(axis.text.x = element_blank(),
                                strip.text.x = element_blank()),
     labor_level_fig_b + labs(y = NULL) + theme(axis.text.x = element_blank(),
                               strip.text.x = element_blank()),
     align = 'vh',
     # labels = c("A", "B", "C", "D", "E", "F"),
     # # labels = 'AUTO',
     # label_size = 10,
     hjust = -1,
     nrow = 1,
     rel_widths = c(1, 1)
   )
   
   ## add poverty legend
   hl_plot_grid_b <- plot_grid(
     hl_plot_grid_b,
     legend_b,
     ncol = 1, 
     rel_heights = c(0.95, 0.05)
   )
   
   ## DAC
   hl_plot_grid_c <- plot_grid(
     health_level_fig_c + theme(axis.text.x = element_blank()),
     # + theme(strip.text.x = element_blank()),
     labor_level_fig_c + labs(y = NULL) + theme(axis.text.x = element_blank()), 
     align = 'vh',
     # labels = c("A", "B", "C", "D", "E", "F"),
     # # labels = 'AUTO',
     # label_size = 10,
     hjust = -1,
     nrow = 1,
     rel_widths = c(1, 1)
   )
   
   ## add DAC legend
   hl_plot_grid_c <- plot_grid(
     hl_plot_grid_c,
     legend_c,
     ncol = 1, 
     rel_heights = c(0.95, 0.05)
   )
   
   
   ## all together now
   hl_plot_grid <- plot_grid(
     hl_plot_grid_c,
     NULL,
     hl_plot_grid_b,
     NULL,
     hl_plot_grid_a,
     align = "v",
     # labels = c("(A)", "(B)", "(C)", ""),
     # # labels = 'AUTO',
     # label_size = 10,
     # hjust = -1,
     ncol = 1,
     rel_heights = c(1, 0.1, 1, 0.1, 1)
     # rel_widths = c(1, 1, 1)
   )


return(hl_plot_grid)



}

plot_hl_levels_pc <- function(demographic_npv_df,
                              refining_mortality,
                              pop_ratios,
                              main_path) {
  
  ## copy npv results
  plot_df_long <- copy(demographic_npv_df)
  
  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()
  
  pop_2020[, demo_pop := pop * pct]
  
  ## summarize by demographic group
  pop_2020 <- pop_2020[, .(pop_2020 = sum(demo_pop)),
                                   by = .(demo_group, demo_cat)]
  
  ## merge population back with results
  plot_df_long <- merge(plot_df_long, pop_2020,
                        by = c("demo_group", "demo_cat"),
                        all.x = T)
  
  ## calculate per capita
  plot_df_long[, value := value / pop_2020]
  
  ## save figure inputs
  fwrite(plot_df_long, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_disaggregated_npv_pc_fig_inputs.csv"))
  
  
  
  ## create the figure ---------------------------------------------
  ##---------------------------------------------------------------
  
  ## scenarios for filtering
  remove_scen <- c('LC1 historic production', 'BAU historic production')
  bau_scen <- 'BAU historic production'
  
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")
  
  ## health fig - race
  health_level_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Race",
                                              unit_desc == "USD (2019 VSL)",
                                              title %in% fig_title_vec) %>%
                 mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))), 
               aes(x = scen_title, y = value, color = title),
               size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "Asian",
                                  "white"),
                       values = c("#002147",
                                  "#721817",
                                  "#40826D",
                                  "#FFBA00")) +
    labs(y = "NPV per capita (USD)",
         x = NULL,
         color = NULL) +
    scale_y_continuous(label = comma, limits = c(0, 2000)) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figa <- health_level_fig_a + theme(legend.position = "bottom")
  
  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8)))
  
  ## labor fig - race
  labor_level_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Race",
                                              segment == "labor",
                                              title %in% fig_title_vec)  %>%
                 mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))), 
               aes(x = scen_title, y = value, color = title),
               size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "Asian",
                                  "white"),
                       values = c("#002147",
                                  "#721817",
                                  "#40826D",
                                  "#FFBA00")) +
    labs(y = "NPV per capita (USD)",
         x = NULL,
         color = NULL) +
    scale_y_continuous(label = comma, limits = c(-2000, 0)) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  ## health fig - poverty
  health_level_fig_b <- ggplot() +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Poverty",
                                              unit_desc == "USD (2019 VSL)") %>%
                 mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))), 
               aes(x = scen_title, y = value, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = c("Above poverty line" = 19,
                                  "Below poverty line" = 17)) +
    facet_wrap(~seg_title) +
    labs(y = "NPV per capita (USD)",
         x = NULL,
         color = NULL) +
    theme_line +
    scale_y_continuous(label = comma, limits = c(0, 2000)) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figb <- health_level_fig_b + theme(legend.position = "bottom")
  
  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8)))
  
  ## labor fig - poverty
  labor_level_fig_b <- ggplot() +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Poverty",
                                              segment == "labor") %>%
                 mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))), 
               aes(x = scen_title, y = value, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = c("Above poverty line" = 19,
                                  "Below poverty line" = 17)) +
    facet_wrap(~seg_title) +
    labs(y = "NPV per capita (USD)",
         x = NULL,
         color = NULL) +
    scale_y_continuous(label = comma, limits = c(-2000, 0)) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  
  ## health fig - DAC
  health_level_fig_c <- ggplot() +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "DAC",
                                              unit_desc == "USD (2019 VSL)"), aes(x = scen_title, y = value, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = c("DAC" = 15,
                                  "Non-DAC" = 4)) +
    facet_wrap(~seg_title) +
    labs(y = "NPV per capita (USD)",
         x = NULL,
         color = NULL) +
    theme_line +
    scale_y_continuous(label = comma, limits = c(0, 2000)) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  legend_figc <- health_level_fig_c + theme(legend.position = "bottom")
  
  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8)))
  
  ## labor fig - DAC
  labor_level_fig_c <- ggplot() +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "DAC",
                                              segment == "labor"), aes(x = scen_title, y = value, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = c("DAC" = 15,
                                  "Non-DAC" = 4)) +
    facet_wrap(~seg_title) +
    labs(y = "NPV per capita (USD)",
         x = NULL,
         color = NULL) +
    scale_y_continuous(label = comma, limits = c(-2000,0)) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  
  ## combine figure
  ## ---------------------------------
  
  ## race
  hl_plot_grid_a <- plot_grid(
    health_level_fig_a + theme(strip.text.x = element_blank()),
    labor_level_fig_a + labs(y = NULL) + theme(strip.text.x = element_blank()),
    align = 'vh',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )
  
  ## add race legend
  hl_plot_grid_a <- plot_grid(
    hl_plot_grid_a,
    legend_a,
    ncol = 1, 
    rel_heights = c(0.95, 0.05)
  )
  
  ## poverty
  hl_plot_grid_b <- plot_grid(
    health_level_fig_b + theme(axis.text.x = element_blank(),
                               strip.text.x = element_blank()),
    labor_level_fig_b + labs(y = NULL) + theme(axis.text.x = element_blank(),
                                               strip.text.x = element_blank()),
    align = 'vh',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )
  
  ## add poverty legend
  hl_plot_grid_b <- plot_grid(
    hl_plot_grid_b,
    legend_b,
    ncol = 1, 
    rel_heights = c(0.95, 0.05)
  )
  
  ## DAC
  hl_plot_grid_c <- plot_grid(
    health_level_fig_c + theme(axis.text.x = element_blank()),
    labor_level_fig_c + labs(y = NULL) + theme(axis.text.x = element_blank()),
    align = 'vh',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )
  
  ## add DAC legend
  hl_plot_grid_c <- plot_grid(
    hl_plot_grid_c,
    legend_c,
    ncol = 1, 
    rel_heights = c(0.95, 0.05)
  )
  
  
  ## all together now
  hl_plot_grid_pc <- plot_grid(
    hl_plot_grid_c,
    NULL,
    hl_plot_grid_b,
    NULL,
    hl_plot_grid_a,
    align = "v",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 1,
    rel_heights = c(1, 0.1, 1, 0.1, 1)
    # rel_widths = c(1, 1, 1)
  )
  
  
  return(hl_plot_grid_pc)
  
  
  
}


## npv shares
## ----------------------------------------------------------------------------

plot_hl_shares <- function(main_path,
                           demographic_npv_df,
                           state_pop_ratios) {

  plot_df_long <- copy(demographic_npv_df)

  ## calculate shares
  plot_df_long[, total_value := sum(value),
               by = .(scen_id, demand_scenario, refining_scenario, demo_cat, metric, segment, unit_desc,
                      metric_desc, seg_title, scenario, demand_title, scen_title)]

  plot_df_long[, share := value / total_value]
  
  ## shares
  pct_df <- copy(state_pop_ratios)
  
  pct_df[, scen_title := "population"]
  
  
  ## create one df for plotting
  share_df <- plot_df_long[, .(scen_id, demand_scenario, refining_scenario, demo_cat, title, metric, unit_desc, segment,
                                  metric_desc, seg_title, scenario, demand_title, scen_title, share)]
  
  pop_share_df <- copy(share_df)
  pop_share_df[,`:=`(scen_id = "Population",
                     demand_scenario = NA,
                     refining_scenario = NA,
                     metric = "general_pop_share",
                     unit_desc = NA,
                     segment = "general",
                     metric_desc = "general",
                     seg_title = "State",
                     scenario = "State",
                     demand_title = NA,
                     scen_title = "State\npopulation",
                     share = NULL)]

  
  pop_share_df <- unique(pop_share_df)
  
  ## merge
  pop_share_df <- merge(pop_share_df, pct_df[, .(demo_cat, title, pct)],
                                             by = c("demo_cat", "title"))
  
  setnames(pop_share_df, "pct", "share")
  
  ## bind
  share_df <- rbind(share_df, pop_share_df)

  ## save figure inputs
  fwrite(share_df, file.path(main_path, "outputs/academic-out/refining/figures/2022-12-update/fig-csv-files/", "state_disaggreated_npv_share_fig_inputs.csv"))

  

  ## create the figure ---------------------------------------------
  ##---------------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c('LC1 historic production', 'BAU historic production')
  bau_scen <- 'BAU historic production'

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  ## health fig - race
  health_share_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Race",
                                              unit_desc == "USD (2019 VSL)",
                                              title %in% fig_title_vec)  %>%
                 mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))),
               aes(x = scen_title, y = share, color = title),
               size = 3, alpha = 0.8) +
    facet_wrap(~seg_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "Asian",
                                  "white"),
                       values = c("#002147",
                                  "#721817",
                                  "#40826D",
                                  "#FFBA00")) +
    ylim(0, 0.5) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  legend_figa <- health_share_fig_a + theme(legend.position = "bottom")

  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8)))

  ## labor fig - race
  labor_share_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Race",
                                              segment == "labor",
                                              title %in% fig_title_vec)%>%
                 mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))),
               aes(x = scen_title, y = share, color = title),
               size = 3, alpha = 0.8) +
    facet_wrap(~seg_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "Asian",
                                  "white"),
                       values = c("#002147",
                                  "#721817",
                                  "#40826D",
                                  "#FFBA00")) +
    ylim(0, 0.5) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  ## state fig - race
  state_share_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Race",
                                              segment == "general",
                                              title %in% fig_title_vec)%>%
                 mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))),
               aes(x = scen_title, y = share, color = title),
               size = 3, alpha = 0.8) +
    facet_wrap(~seg_title) +
    scale_color_manual(name = "",
                       labels = c("Black",
                                  "Hispanic",
                                  "Asian",
                                  "white"),
                       values = c("#002147",
                                  "#721817",
                                  "#40826D",
                                  "#FFBA00")) +
    ylim(0, 0.5) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  ## health fig - poverty
  health_share_fig_b <- ggplot() +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Poverty",
                                              unit_desc == "USD (2019 VSL)") %>%
                 mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))),
               aes(x = scen_title, y = share, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    scale_shape_manual(values = c("Above poverty line" = 17,
                                  "Below poverty line" = 19)) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    theme_line +
    ylim(0, 0.9) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  legend_figb <- health_share_fig_b + theme(legend.position = "bottom")

  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8)))

  ## labor fig - poverty
  labor_share_fig_b <- ggplot() +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "Poverty",
                                              segment == "labor") %>%
                 mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))),
               aes(x = scen_title, y = share, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    scale_shape_manual(values = c("Above poverty line" = 17,
                                  "Below poverty line" = 19)) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    ylim(0, 0.9) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  ## state - poverty
  state_share_fig_b <- ggplot() +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                          demo_cat == "Poverty",
                                          segment == "general") %>%
                 mutate(title = factor(title, levels = c("Below poverty line", "Above poverty line"))),
               aes(x = scen_title, y = share, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    scale_shape_manual(values = c("Above poverty line" = 17,
                                  "Below poverty line" = 19)) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    ylim(0, 0.9) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))


  ## health fig - DAC
  health_share_fig_c <- ggplot() +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "DAC",
                                              unit_desc == "USD (2019 VSL)"), aes(x = scen_title, y = share, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = c("DAC" = 15,
                                  "Non-DAC" = 4)) +
    facet_wrap(~seg_title) +
    ylim(0, 0.85) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))

  legend_figc <- health_share_fig_c + theme(legend.position = "bottom")

  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8)))

  ## labor fig - DAC
  labor_share_fig_c <- ggplot() +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                              demo_cat == "DAC",
                                              segment == "labor"), aes(x = scen_title, y = share, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = c("DAC" = 15,
                                  "Non-DAC" = 4)) +
    facet_wrap(~seg_title) +
    ylim(0, 0.85) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    # ylim(-15, 0) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  ## general fig - DAC
  state_share_fig_c <- ggplot() +
    geom_point(data = share_df %>% filter(!scen_id %in% remove_scen,
                                          demo_cat == "DAC",
                                          segment == "general"), aes(x = scen_title, y = share, shape = title),
               color = "black", size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = c("DAC" = 15,
                                  "Non-DAC" = 4)) +
    facet_wrap(~seg_title) +
    ylim(0, 0.85) +
    labs(y = "NPV share",
         x = NULL,
         color = NULL) +
    # ylim(-15, 0) +
    theme_line +
    theme(legend.position = "none",
          legend.title = element_blank(),
          # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))


  ## combine figure
  ## ---------------------------------

  ## race
  hl_share_plot_grid_a <- plot_grid(
    health_share_fig_a + theme(strip.text.x = element_blank()),
    state_share_fig_a + labs(y = NULL) + theme(strip.text.x = element_blank()),
    labor_share_fig_a + labs(y = NULL) + theme(strip.text.x = element_blank()),
    align = 'vh',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 0.25, 1)
  )

  ## add race legend
  hl_share_plot_grid_a <- plot_grid(
    hl_share_plot_grid_a,
    legend_a,
    ncol = 1,
    rel_heights = c(0.95, 0.05)
  )

  ## poverty
  hl_share_plot_grid_b <- plot_grid(
    health_share_fig_b + theme(axis.text.x = element_blank(),
                               strip.text.x = element_blank()),
    state_share_fig_b + labs(y = NULL) + theme(axis.text.x = element_blank(),
                                              strip.text.x = element_blank()),
    labor_share_fig_b + labs(y = NULL) + theme(axis.text.x = element_blank(),
                                               strip.text.x = element_blank()),
    align = 'vh',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 0.25, 1)
  )

  ## add poverty legend
  hl_share_plot_grid_b <- plot_grid(
    hl_share_plot_grid_b,
    legend_b,
    ncol = 1,
    rel_heights = c(0.95, 0.05)
  )

  ## DAC
  hl_share_plot_grid_c <- plot_grid(
    health_share_fig_c + theme(axis.text.x = element_blank()),
    # + theme(strip.text.x = element_blank()),
    state_share_fig_c + labs(y = NULL) + theme(axis.text.x = element_blank()),
    labor_share_fig_c + labs(y = NULL) + theme(axis.text.x = element_blank()),
    align = 'vh',
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 0.25, 1)
  )

  ## add DAC legend
  hl_share_plot_grid_c <- plot_grid(
    hl_share_plot_grid_c,
    legend_c,
    ncol = 1,
    rel_heights = c(0.95, 0.05)
  )


  ## all together now
  hl_share_plot_grid <- plot_grid(
    hl_share_plot_grid_c,
    NULL,
    hl_share_plot_grid_b,
    NULL,
    hl_share_plot_grid_a,
    align = "v",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 1,
    rel_heights = c(1, 0.1, 1, 0.1, 1)
    # rel_widths = c(1, 1, 1)
  )


  return(hl_share_plot_grid)



}




