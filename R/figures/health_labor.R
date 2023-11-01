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
  plot_df <- health_labor_ghg_df[, .(scen_id, demand_scenario, refining_scenario,
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
  
  ## rename
  setDT(plot_df_long)
  plot_df_long[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
  plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  plot_df_long[, scenario := gsub('LC1.', 'Low carbon ', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]
  
  ## refactor
  plot_df_long$scenario <- factor(plot_df_long$scenario, levels = c('Reference demand - historic production',
                                                                    'Reference demand - historic exports', 
                                                                    'Reference demand - low exports', 
                                                                    'Low carbon demand - historic exports',
                                                                    'Low carbon demand - low exports',
                                                                    'Low carbon demand - historic production'))
  
  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_long[, value := fifelse(metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"), value * 1000, value)]
  plot_df_long[, metric := fifelse(metric =="forgone_wages_bil_ghg", "forgone_wages_ghg", metric)]
  plot_df_long[, unit := fifelse(metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"), 
                                 "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)", 
                                 "NPV (2019 USD billion)")]
  
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
                                                    unit == "NPV (2019 USD billion)"))
  
  fig_bxm_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Health: avoided mortality", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
                                              title == "Health: avoided mortality",
                                              unit == "NPV (2019 USD billion)",
                                              !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value,  color = refining_scenario, shape = demand_scenario),
               size = 3, alpha = 0.8) +
    labs(color = "Refing scenario",
         shape = "Demand scenario",
         title = "A. Health: avoided mortality",
         y = "NPV (2019 USD billion)",
         x = NULL) +
    ylim(-1, 25) +
    xlim(0, 80) +
    scale_color_manual(values = refin_colors) +
    theme_line_n +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm')) 
  
  fig_bxm_b <- ggplot() + 
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_2045_perc_reduction], color = "darkgray", lty = 2) +
    geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                        title == "Labor: forgone wages",
                                        measure == "NPV (2019 USD billion)",
                                        !refining_scenario == "historic production"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    labs(color = "Policy",
         title = "B. Labor: forgone wages",
         y = NULL,
         x = NULL) +
    ylim(-30, 0) +
    xlim(0, 80) +
    scale_color_manual(values = refin_colors) +
    theme_line_n +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm')) 
  
  fig_bxm_c <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Climate: avoided damage", ghg_2045_perc_reduction], color = "darkgray", lty = 2) +
    geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                        title == "Climate: avoided damage",
                                        measure == "NPV (2019 USD billion)",
                                        !refining_scenario == "historic production"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    labs(color = "Policy",
         title = "C. Climate: avoided damage",
         y = NULL,
         x = NULL) +
    ylim(-1, 20) +
    xlim(0, 80) +
    scale_color_manual(values = refin_colors) +
    theme_line_n +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm')) 
  
  fig_bxm_d <- ggplot() + 
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Health: avoided mortality", ghg_2045_perc_reduction], color = "darkgray", lty = 2) +
    geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                        title == "Health: avoided mortality",
                                        measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
                                        !refining_scenario == "historic production"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    labs(color = "Policy",
         title = "D.",
         y = bquote('NPV (2019 USD million)\nper avoided GHG MtCO'[2]~e),
         x = "GHG emissions reduction target (%, 2045 vs 2019)") +
    scale_color_manual(values = refin_colors) +
    ylim(0, 200) +
    xlim(0, 80) +
    theme_line_n +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  fig_bxm_e <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_2045_perc_reduction], color = "darkgray", lty = 2) +
    geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                        title == "Labor: forgone wages",
                                        measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
                                        !refining_scenario == "historic production"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    labs(color = "Policy",
         title = "E.",
         y = NULL,
         # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
         x = "GHG emissions reduction target (%, 2045 vs 2019)") +
    scale_color_manual(values = refin_colors) +
    theme_line_n +
    xlim(0, 80) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
  fig_bxm_f <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(xintercept = hist_prod[title == "Health: avoided mortality", ghg_2045_perc_reduction], color = "darkgray", lty = 2) +
    geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                        title == "Climate: avoided damage",
                                        measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
                                        !refining_scenario == "historic production"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
    labs(color = "Policy",
         title = "F.",
         y = NULL,
         # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
         x = "GHG emissions reduction target (%, 2045 vs 2019)") +
    scale_color_manual(values = refin_colors) +
    theme_line_n +
    ylim(0, 80) +
    xlim(0, 80) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'))
  
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
  #   theme_line_n +
  #   theme(legend.position = "bottom",
  #         axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm')) +
  #   guides(color = guide_legend(nrow = 2, byrow = TRUE))
  
  legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                        title == "Labor: forgone wages",
                                        measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
                                        !refining_scenario == "historic production"), 
               aes(x = ghg_2045_perc_reduction, y = value, color = scenario, shape = scenario), size = 3, alpha = 0.8) +
    labs(title = "",
         y = NULL,
         # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
         x = "GHG emissions reduction target (%, 2045 vs 2019)",
         color = NULL,
         shape = NULL) +
    scale_color_manual(name = "",
                       labels = c("Reference demand - historic exports",
                                  "Reference demand - low exports",
                                  "Low carbon demand - historic exports",
                                  "Low carbon demand - low exports"),
                       values = c("Reference demand - historic exports" = "#2F4858",
                                  "Reference demand - low exports" = "#F26419",
                                  "Low carbon demand - historic exports" = "#2F4858",
                                  "Low carbon demand - low exports" = "#F26419")) +
    scale_shape_manual(name = "",
                       labels = c("Reference demand - historic exports",
                                  "Reference demand - low exports",
                                  "Low carbon demand - historic exports",
                                  "Low carbon demand - low exports"),
                       values = c(16, 16, 17, 17)) +
    theme_line_n +
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
  xaxis_lab <- ggdraw() + draw_label("GHG emissions reduction target (%, 2045 vs 2019)", size = 7)
  
  fig3_plot_grid <- plot_grid(
    fig_bxm_a,
    fig_bxm_b,
    fig_bxm_c,
    fig_bxm_d + labs(x = NULL),
    fig_bxm_e + labs(x = NULL),
    fig_bxm_f+ labs(x = NULL),
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
  
  
  ## save figure 3
  ggsave(fig3_plot_grid2,
         filename = file.path(main_path, fig_path, 'health_labor_climate_impacts_fig.png'),
         width = 180,
         height = 160,
         units = "mm")
  
  ggsave(fig3_plot_grid2,
         filename = file.path(main_path, fig_path, 'health_labor_climate_impacts_fig.pdf'),
         width = 180,
         height = 160,
         units = "mm",
         device = 'pdf')
  
  embed_fonts(paste0(main_path, fig_path, 'health_labor_climate_impacts_fig.pdf'),
              outfile = paste0(main_path, fig_path, 'health_labor_climate_impacts_fig.pdf'))
  
  
  
  
  


}


# plot_health_levels <- function(refining_mortality) {
#  
#   total_mort_level <- refining_mortality %>%
#     group_by(scen_id, demand_scenario, refining_scenario, year) %>%
#     summarise(mortality_level = sum(mortality_level)) %>%
#     ungroup()
#   
#   health_level_fig <- ggplot(total_mort_level, aes(x = year, y = mortality_level, color = scen_id)) +
#     geom_line(linewidth = 1)
#   
#   health_level_fig 
#   
# }


