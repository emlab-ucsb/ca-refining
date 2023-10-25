## health and labor figures

## NPV figure
plot_npv_health_labor <- function(refining_mortality,
                                  state_ghg_output,
                                  dt_ghg_2019,
                                  annual_labor) {

  npv_df <- refining_mortality %>% as.data.table()

  ## state level
  state_npv_df <- npv_df[, .(sum_cost_2019_pv = sum(cost_2019_PV)),
                             by = .(scen_id, demand_scenario, refining_scenario)]

  ## add column
  state_npv_df[, sum_cost_2019_pv_b := sum_cost_2019_pv / 1e9]


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
  plot_df <- health_labor_ghg_df[scen_id != "BAU historic production", .(scen_id, demand_scenario, refining_scenario,
                                                                         sum_cost_2019_pv_b, forgone_wages_bil, avoided_ghg, perc_diff)]
  
  setnames(plot_df, "perc_diff", "ghg_perc_diff")
  
  ## add values / avoided ghgs
  plot_df[, avoided_health_cost := sum_cost_2019_pv_b * -1]
  plot_df[, sum_cost_2019_pv_b := NULL]
  
  plot_df[, `:=` (avoided_health_cost_ghg = avoided_health_cost / avoided_ghg,
                  forgone_wages_bil_ghg = forgone_wages_bil / avoided_ghg)]
  
  plot_df_health <- plot_df %>%
    select(scen_id, demand_scenario, refining_scenario, ghg_perc_diff, avoided_health_cost, avoided_health_cost_ghg) %>%
    pivot_longer(avoided_health_cost:avoided_health_cost_ghg, names_to = "metric", values_to = "value")
  
  plot_df_labor <- plot_df %>%
    select(scen_id, demand_scenario, refining_scenario, ghg_perc_diff, forgone_wages_bil, forgone_wages_bil_ghg) %>%
    pivot_longer(forgone_wages_bil:forgone_wages_bil_ghg, names_to = "metric", values_to = "value")
  
  plot_df_long <- rbind(plot_df_health, plot_df_labor)
  
  plot_df_long <- plot_df_long %>%
    mutate(title = ifelse(metric == "avoided_health_cost", "Health: avoided mortality",
                                  ifelse(metric == "avoided_health_cost_ghg", "Health: avoided mortality per avoided GHG",
                                         ifelse(metric == "forgone_wages_bil", "Labor: forgone wages", "Labor: forgone wages per avoided GHG"))))
  
  plot_df_long$title <- factor(plot_df_long$title, levels = c("Health: avoided mortality", "Labor: forgone wages", 
                                                                "Health: avoided mortality per avoided GHG", "Labor: forgone wages per avoided GHG"))
  
  ## make the plot
  ## ---------------------------------------------------
  
  # fig
  fig_benefit_x_metric <- ggplot(plot_df_long, aes(x = ghg_perc_diff * - 100, y = value, color = refining_scenario, shape = demand_scenario)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(xintercept = 0, color = "darkgray", linewidth = 0.5) +
    labs(color = "Refining scenario",
         shape = "Demand scenario",
         y = "NPV USD 2019",
         x = "GHG emissions reduction target (%, 2045 vs 2019)",) +
    facet_wrap(~title, scales = "free", ncol = 2) +
    # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
    # scale_x_continuous(limits = c(0, NA)) +
    # scale_color_manual(values = policy_colors_subset) +
    theme_line +
    # theme_bw() +
    theme(legend.position = "bottom",
          # legend.box = "vertical",
          # legend.key.width= unit(1, 'cm'),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, hjust = 1),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm')) 
  
  fig_benefit_x_metric

  # ## revised version, make them separately
  # ## -------------------------------------------------------------------
  # fig_text_size <- 7
  # 
  # fig_a <- ggplot(plot_df_long %>% filter(metric == "avoided_health_cost"), aes(x = ghg_perc_diff, y = value, color = refining_scenario, shape = demand_scenario)) +
  #   geom_point(size = 2, alpha = 0.8) +
  #   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  #   labs(color = "Refining scenario",
  #        title = "a. Health: avoided mortality",
  #        y = "NPV (2019 USD billion)",
  #        x = NULL) +
  #   ylim(0, 25) +
  #   # scale_color_manual(values = policy_colors_subset) +
  #   # theme_line +
  #   theme_bw() +
  #   theme(
  #         legend.position = "none",
  #         plot.title = element_text(hjust = 0, size = fig_text_size),
  #         axis.text.x = element_text(vjust = 0.5, hjust = 1, size = fig_text_size),
  #         axis.text.y = element_text(size = fig_text_size),
  #         axis.title.y = element_text(size = fig_text_size),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm'))
  # 
  # legend_fig_3 <- get_legend(
  #   legend_fig + 
  #     theme(legend.title = element_text(size = fig3_text_size),
  #           legend.text = element_text(size = fig3_text_size))
  #   
  # )
  # 
  # 
  # 
  # ## combine figure
  # ## ---------------------------------
  # 
  # ## shared x axis
  # xaxis_lab <- ggdraw() + draw_label("GHG emissions reduction target (%, 2045 vs 2019)", size = 7)
  # 
  # fig3_plot_grid <- plot_grid(
  #   fig_bxm_a,
  #   fig_bxm_b,
  #   fig_bxm_c,
  #   fig_bxm_d + labs(x = NULL),
  #   fig_bxm_e + labs(x = NULL),
  #   fig_bxm_f+ labs(x = NULL),
  #   align = 'vh',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 2,
  #   rel_widths = c(1, 1, 1, 1, 1, 1)
  # )
  # 
  # fig3_plot_grid2 <- plot_grid(
  #   fig3_plot_grid,
  #   xaxis_lab,
  #   legend_fig_3,
  #   align = "v",
  #   # labels = c("(A)", "(B)", "(C)", ""),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   # hjust = -1,
  #   ncol = 1,
  #   rel_heights = c(1, 0.05, 0.05)
  #   # rel_widths = c(1, 1),
  # )
  
  
  
  
  
  


}