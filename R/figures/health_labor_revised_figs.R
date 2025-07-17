## health and labor figures

## Helper functions for file operations
## -----------------------------------------------------------------------------

#' Ensure directory exists before saving file
#' @param dir Directory to create if it doesn't exist
ensure_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Created directory: ", dir)
  }
}

#' Safe write file with directory creation
#' @param data Data to write
#' @param main_path Base path (not used, included for compatibility)
#' @param save_path Save path
#' @param subdir Subdirectory name
#' @param filename Filename
safe_write_file <- function(data, save_path, subdir, filename) {
  full_dir <- file.path(save_path, subdir)
  ensure_dir(full_dir)
  full_path <- file.path(full_dir, filename)
  fwrite(data, full_path)
  message("Saved: ", full_path)
  return(full_path)
}

## labor SI figure
## -----------------------------------------------------------------------------

## NPV figure
plot_npv_labor_oilpx <- function(
  main_path,
  save_path,
  state_ghg_output,
  dt_ghg_2019,
  annual_all_impacts_labor,
  variant = "base"
) {
  # Ensure output directories exist
  fig_csv_dir <- file.path(save_path, "results", "figures", "extra")
  legends_dir <- file.path(save_path, "results", "figures", "extra")
  ensure_dir(fig_csv_dir)
  ensure_dir(legends_dir)
  ## add ghg emission reduction
  ## 2019 ghg
  ghg_2019_val <- dt_ghg_2019$mtco2e[1]

  ## 2045 vs 2019 ghg
  ghg_2045 <- state_ghg_output[year == 2045 & source == "total"]
  setnames(ghg_2045, "value", "ghg_kg")
  ghg_2045[, ghg_2045 := (ghg_kg / 1000) / 1e6]
  ghg_2045[, ghg_2019 := ghg_2019_val]
  ghg_2045[, perc_diff := (ghg_2045 - ghg_2019) / ghg_2019]

  perc_diff_df <- ghg_2045[, .(
    demand_scenario,
    refining_scenario,
    ghg_2045,
    ghg_2019,
    perc_diff
  )]

  ## summarize by scenario, filter for total
  state_ghg_df <- state_ghg_output[
    source == "total",
    .(total_ghg = sum(value)),
    by = .(demand_scenario, refining_scenario)
  ]

  state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

  ## reference
  ref_df <- state_ghg_df[
    demand_scenario == "BAU" & refining_scenario == "historic production",
    .(total_ghg_mmt)
  ]
  setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
  ref_value <- ref_df$ref_ghg_mmt[1]

  ## merge with summarized df
  state_ghg_df[, ref_ghg := ref_value]
  state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]

  ## summarize labor for state
  state_labor <- annual_all_impacts_labor[,
    .(
      # sum_total_emp = sum(total_emp),
      sum_total_comp_pv_h = sum(comp_all_impacts_PV_h),
      sum_total_comp_pv_l = sum(comp_all_impacts_PV_l, na.rm = T)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario
    )
  ]

  ## ref labor
  ref_labor <- state_labor[
    demand_scenario == "BAU" & refining_scenario == "historic production"
  ]
  setnames(
    ref_labor,
    c("sum_total_comp_pv_h", "sum_total_comp_pv_l"),
    c("ref_total_comp_pv_h", "ref_total_comp_pv_l")
  )
  ref_labor <- ref_labor[, .(
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    ref_total_comp_pv_l,
    ref_total_comp_pv_h
  )]

  ## add values to labor
  state_labor_oil_px <- merge(
    state_labor,
    ref_labor,
    by = c(
      "oil_price_scenario",
      "product_scenario",
      "indirect_induced_scenario"
    )
  )

  ## compute forgone wages high and low
  state_labor_oil_px[,
    forgone_wages_bil_h := (sum_total_comp_pv_h - ref_total_comp_pv_h) / 1e9
  ]
  state_labor_oil_px[,
    forgone_wages_bil_l := (sum_total_comp_pv_l - ref_total_comp_pv_l) / 1e9
  ]

  ## merge with health and ghg
  labor_ghg_df <- merge(
    state_labor_oil_px[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      sum_total_comp_pv_h,
      ref_total_comp_pv_h,
      forgone_wages_bil_h,
      sum_total_comp_pv_l,
      ref_total_comp_pv_l,
      forgone_wages_bil_l
    )],
    state_ghg_df,
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## add ghg perc reduction
  labor_ghg_df <- merge(
    labor_ghg_df,
    perc_diff_df,
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## add scen id
  labor_ghg_df[, scen_id := paste(demand_scenario, refining_scenario)]

  ## prepare to plot
  plot_df <- labor_ghg_df[, .(
    scen_id,
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    forgone_wages_bil_h,
    forgone_wages_bil_l,
    avoided_ghg,
    perc_diff
  )]

  setnames(plot_df, "perc_diff", "ghg_perc_diff")

  plot_df[, `:=`(
    forgone_wages_bil_h_ghg = forgone_wages_bil_h / avoided_ghg,
    forgone_wages_bil_l_ghg = forgone_wages_bil_l / avoided_ghg
  )]

  plot_df_labor <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      indirect_induced_scenario,
      product_scenario,
      oil_price_scenario,
      ghg_perc_diff,
      forgone_wages_bil_h,
      forgone_wages_bil_l,
      forgone_wages_bil_h_ghg,
      forgone_wages_bil_l_ghg
    ) %>%
    pivot_longer(
      forgone_wages_bil_h:forgone_wages_bil_l_ghg,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      segment = "labor",
      unit_desc = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "USD billion",
        "USD billion per GHG"
      ),
      estimate = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_h_ghg"),
        "high",
        "low"
      ),
      metric = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "forgone_wages_bil",
        "forgone_wages_bil_ghg"
      )
    ) %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      indirect_induced_scenario,
      product_scenario,
      oil_price_scenario,
      ghg_perc_diff,
      segment,
      metric,
      unit_desc,
      estimate,
      value
    ) %>%
    pivot_wider(names_from = estimate, values_from = value)

  ## prepare labor ----------------------
  plot_df_labor <- plot_df_labor %>%
    mutate(
      title = ifelse(
        metric == "forgone_wages_bil",
        "Labor: forgone wages",
        "Labor: forgone wages per avoided GHG"
      )
    )

  plot_df_labor$title <- factor(
    plot_df_labor$title,
    levels = c("Labor: forgone wages", "Labor: forgone wages per avoided GHG")
  )

  ## rename
  setDT(plot_df_labor)
  plot_df_labor[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_labor[, scenario := gsub("LC1.", "Low ", scenario)]

  ## refactor
  plot_df_labor$scenario <- factor(
    plot_df_labor$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_labor[,
    high := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      high * 1000,
      high
    )
  ]
  plot_df_labor[,
    low := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      low * 1000,
      low
    )
  ]
  plot_df_labor[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_labor[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_labor[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_labor[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_labor[, scenario := str_replace(scenario, "historic", "historical")]

  ## has oil price label
  plot_df_labor[,
    oil_px_label := ifelse(
      oil_price_scenario == "reference case",
      "Reference",
      ifelse(oil_price_scenario == "high oil price", "High", "Low")
    )
  ]

  plot_df_labor$oil_px_label <- factor(
    plot_df_labor$oil_px_label,
    levels = c("Low", "Reference", "High")
  )

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_labor,
    folder_path = NULL,
    filename = "state_npv_fig_inputs_labor_all_oilpx.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "labor"
  )

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  ## make the plot
  ## ---------------------------------------------------

  ## color for refining scenario
  refin_colors <- c(
    "LC1 low exports" = "#729b79",
    "LC1 historical exports" = "#2F4858",
    "BAU low exports" = "#F6AE2D",
    "BAU historical exports" = "#F26419"
  )

  refin_labs <- c(
    "LC1 low exports" = "Low demand, low exports",
    "LC1 historical exports" = "Low demand, historical exports",
    "BAU low exports" = "BAU demand, low exports",
    "BAU historical exports" = "BAU demand, historical exports"
  )

  ## figs - make each separately
  ## -------------------------------------------------------------------

  hist_prod <- as.data.table(
    plot_df_labor %>%
      filter(
        scen_id == bau_scen,
        oil_price_scenario == "reference case",
        unit == "NPV (2019 USD billion)"
      )
  )

  ## 2020 prices
  forgone_wages_all_oil_px_fig_2020ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod$ghg_perc_diff * -100,
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historical production",
          indirect_induced_scenario == "baseline",
          product_scenario != "changing prices",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    facet_wrap(~oil_px_label) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = "NPV (2019 USD billion)",
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    ylim(-90, 20) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  # simple_ggsave_repo(
  #   forgone_wages_all_oil_px_fig_2020ppx,
  #   save_path,
  #   "state_npv_labor_fig_2020ppx",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  ## 2020 prices and bartik
  forgone_wages_all_oil_px_fig_2020ppx_bc <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod$ghg_perc_diff * -100,
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historical production",
          indirect_induced_scenario != "baseline",
          product_scenario != "changing prices",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    facet_wrap(~oil_px_label) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = "NPV (2019 USD billion)",
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    ylim(-90, 20) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  # simple_ggsave_repo(
  #   forgone_wages_all_oil_px_fig_2020ppx_bc,
  #   save_path,
  #   "state_npv_labor_fig_2020ppx_bartik",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  ## changing prices and bartik
  forgone_wages_all_oil_px_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod$ghg_perc_diff * -100,
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historical production",
          product_scenario == "changing prices",
          indirect_induced_scenario == "baseline",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    facet_wrap(~oil_px_label) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = "NPV (2019 USD billion)",
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    ylim(-90, 20) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  # Return appropriate plot variant based on variant parameter
  if (variant == "2020ppx") {
    return(forgone_wages_all_oil_px_fig_2020ppx)
  } else if (variant == "bartik") {
    return(forgone_wages_all_oil_px_fig_2020ppx_bc)
  } else {
    return(forgone_wages_all_oil_px_fig)  # base variant (changing prices)
  }
}


## NPV figure
plot_npv_health_labor <- function(
  main_path,
  save_path,
  refining_mortality,
  state_ghg_output,
  dt_ghg_2019,
  annual_all_impacts_labor
) {
  npv_df <- refining_mortality %>% as.data.table()

  ## state level
  state_npv_df <- npv_df[,
    .(
      sum_cost_2019_pv = sum(cost_2019_PV), ## constant VSL
      sum_cost_pv = sum(cost_PV)
    ), ## changing VSL
    by = .(scen_id, demand_scenario, refining_scenario)
  ]

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

  perc_diff_df <- ghg_2045[, .(
    demand_scenario,
    refining_scenario,
    ghg_2045,
    ghg_2019,
    perc_diff
  )]

  ## summarize by scenario, filter for total
  state_ghg_df <- state_ghg_output[
    source == "total",
    .(total_ghg = sum(value)),
    by = .(demand_scenario, refining_scenario)
  ]

  state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

  ## reference
  ref_df <- state_ghg_df[
    demand_scenario == "BAU" & refining_scenario == "historic production",
    .(total_ghg_mmt)
  ]
  setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
  ref_value <- ref_df$ref_ghg_mmt[1]

  ## merge with summarized df
  state_ghg_df[, ref_ghg := ref_value]
  state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]

  ## merge with health
  health_ghg_df <- merge(
    state_npv_df,
    state_ghg_df[, .(
      demand_scenario,
      refining_scenario,
      total_ghg_mmt,
      ref_ghg,
      avoided_ghg
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## summarize labor for state
  state_labor <- annual_all_impacts_labor[,
    .(
      # sum_total_emp = sum(total_emp),
      sum_total_comp_pv_h = sum(comp_all_impacts_PV_h),
      sum_total_comp_pv_l = sum(comp_all_impacts_PV_l, na.rm = T)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario
    )
  ]

  state_labor <- state_labor[oil_price_scenario == "reference case", ]

  ## ref labor
  ref_labor <- state_labor[
    demand_scenario == "BAU" & refining_scenario == "historic production"
  ]
  setnames(
    ref_labor,
    c("sum_total_comp_pv_h", "sum_total_comp_pv_l"),
    c("ref_total_comp_pv_h", "ref_total_comp_pv_l")
  )
  # setnames(ref_labor, c("sum_total_emp", "sum_total_comp_pv_h", "sum_total_comp_pv_l"), c("ref_total_emp", "ref_total_comp_pv_h", "ref_total_comp_pv_l"))
  ref_labor <- ref_labor[, .(
    product_scenario,
    indirect_induced_scenario,
    ref_total_comp_pv_h,
    ref_total_comp_pv_l
  )]

  ## add values to labor
  state_labor <- merge(
    state_labor,
    ref_labor,
    by = c("product_scenario", "indirect_induced_scenario"),
    all.x = T
  )

  # state_labor[, `:=`(
  #   # ref_total_emp = ref_labor$ref_total_emp[1],
  #   ref_total_comp_pv_h = ref_labor$ref_total_comp_pv_h[1],
  #   ref_total_comp_pv_l = ref_labor$ref_total_comp_pv_l[1]
  # )]

  state_labor[,
    forgone_wages_bil_h := (sum_total_comp_pv_h - ref_total_comp_pv_h) / 1e9
  ]
  state_labor[,
    forgone_wages_bil_l := (sum_total_comp_pv_l - ref_total_comp_pv_l) / 1e9
  ]

  ## merge with health and ghg
  health_labor_ghg_df <- merge(
    health_ghg_df,
    state_labor[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      sum_total_comp_pv_h,
      ref_total_comp_pv_h,
      forgone_wages_bil_h,
      sum_total_comp_pv_l,
      ref_total_comp_pv_l,
      forgone_wages_bil_l
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## add ghg perc reduction
  health_labor_ghg_df <- merge(
    health_labor_ghg_df,
    perc_diff_df,
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## prepare to plot
  plot_df <- health_labor_ghg_df[, .(
    scen_id,
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    sum_cost_pv_b,
    sum_cost_2019_pv_b,
    forgone_wages_bil_h,
    forgone_wages_bil_l,
    avoided_ghg,
    perc_diff
  )]

  setnames(plot_df, "perc_diff", "ghg_perc_diff")

  ## add values / avoided ghgs
  plot_df[, avoided_health_cost := sum_cost_2019_pv_b * -1]
  plot_df[, avoided_health_cost_annual_vsl := sum_cost_pv_b * -1]
  plot_df[, sum_cost_2019_pv_b := NULL]
  plot_df[, sum_cost_pv_b := NULL]

  plot_df[, `:=`(
    avoided_health_cost_ghg = avoided_health_cost / avoided_ghg,
    avoided_health_cost_ghg_vsl2 = avoided_health_cost_annual_vsl / avoided_ghg,
    forgone_wages_bil_h_ghg = forgone_wages_bil_h / avoided_ghg,
    forgone_wages_bil_l_ghg = forgone_wages_bil_l / avoided_ghg
  )]

  plot_df_health <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      ghg_perc_diff,
      avoided_health_cost,
      avoided_health_cost_annual_vsl,
      avoided_health_cost_ghg,
      avoided_health_cost_ghg_vsl2
    ) %>%
    pivot_longer(
      avoided_health_cost:avoided_health_cost_ghg_vsl2,
      names_to = "metric",
      values_to = "value"
    )

  ## add column for vsl
  plot_df_health <- plot_df_health %>%
    mutate(
      segment = "health",
      unit_desc = ifelse(
        metric == "avoided_health_cost",
        "USD billion (2019 VSL)",
        ifelse(
          metric == "avoided_health_cost_annual_vsl",
          "USD billion (annual VSL)",
          ifelse(
            metric == "avoided_health_cost_ghg",
            "USD billion per GHG (2019 VSL)",
            "USD billion per GHG (annual VSL)"
          )
        )
      ),
      metric = ifelse(
        metric %in% c("avoided_health_cost", "avoided_health_cost_annual_vsl"),
        "avoided_health_cost",
        "avoided_health_cost_ghg"
      )
    )

  plot_df_labor <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      ghg_perc_diff,
      forgone_wages_bil_h,
      forgone_wages_bil_l,
      forgone_wages_bil_h_ghg,
      forgone_wages_bil_l_ghg
    ) %>%
    pivot_longer(
      forgone_wages_bil_h:forgone_wages_bil_l_ghg,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      segment = "labor",
      unit_desc = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "USD billion",
        "USD billion per GHG"
      ),
      estimate = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_h_ghg"),
        "high",
        "low"
      ),
      metric = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "forgone_wages_bil",
        "forgone_wages_bil_ghg"
      )
    ) %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      ghg_perc_diff,
      segment,
      metric,
      unit_desc,
      estimate,
      value
    ) %>%
    pivot_wider(names_from = estimate, values_from = value)

  # plot_df_long <- rbind(plot_df_health, plot_df_labor)

  ## prepare health for plotting ------------------------------
  plot_df_health <- plot_df_health %>%
    mutate(
      title = ifelse(
        metric == "avoided_health_cost",
        "Health: avoided mortality",
        "Health: avoided mortality per avoided GHG"
      )
    )

  plot_df_health$title <- factor(
    plot_df_health$title,
    levels = c(
      "Health: avoided mortality",
      "Health: avoided mortality per avoided GHG"
    )
  )

  ## rename
  setDT(plot_df_health)
  plot_df_health[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_health[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_health$scenario <- factor(
    plot_df_health$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_health[,
    value := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      value * 1000,
      value
    )
  ]
  plot_df_health[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_health[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_health[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_health[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_health[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_health,
    folder_path = NULL,
    filename = "state_npv_fig_inputs_health.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )

  ## prepare labor ----------------------
  plot_df_labor <- plot_df_labor %>%
    mutate(
      title = ifelse(
        metric == "forgone_wages_bil",
        "Labor: forgone wages",
        "Labor: forgone wages per avoided GHG"
      )
    )

  plot_df_labor$title <- factor(
    plot_df_labor$title,
    levels = c("Labor: forgone wages", "Labor: forgone wages per avoided GHG")
  )

  ## rename
  setDT(plot_df_labor)
  plot_df_labor[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_labor[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_labor$scenario <- factor(
    plot_df_labor$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_labor[,
    high := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      high * 1000,
      high
    )
  ]
  plot_df_labor[,
    low := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      low * 1000,
      low
    )
  ]
  plot_df_labor[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_labor[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_labor[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_labor[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_labor[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_labor,
    folder_path = NULL,
    filename = "state_npv_fig_inputs_labor.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "labor"
  )

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  ## make the plot
  ## ---------------------------------------------------

  ## color for refining scenario
  refin_colors <- c(
    "LC1 low exports" = "#729b79",
    "LC1 historical exports" = "#2F4858",
    "BAU low exports" = "#F6AE2D",
    "BAU historical exports" = "#F26419"
  )

  refin_labs <- c(
    "LC1 low exports" = "Low demand, low exports",
    "LC1 historical exports" = "Low demand, historical exports",
    "BAU low exports" = "BAU demand, low exports",
    "BAU historical exports" = "BAU demand, historical exports"
  )

  ## refactor
  # plot_df_health$scen_id <- factor(plot_df_health$scen_id, levels = c('LC1 low exports',
  #                                                                     'LC1 historical production',
  #                                                                     'BAU demand\nlow exports',
  #                                                                     'Low demand\nhistorical exports',
  #                                                                     'Low demand\nlow exports',
  #                                                                     'Low demand\nhistorical production'))
  #

  ## figs - make each separately
  ## -------------------------------------------------------------------

  hist_prod <- as.data.table(
    plot_df_health %>%
      filter(
        scen_id == bau_scen,
        unit == "NPV (2019 USD billion)",
        unit_desc == "USD billion (2019 VSL)"
      )
  )

  fig_bxm_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    geom_point(
      data = plot_df_health %>%
        filter(
          !scen_id %in% remove_scen,
          title == "Health: avoided mortality",
          unit == "NPV (2019 USD billion)",
          unit_desc == "USD billion (2019 VSL)",
          !refining_scenario == "historical production"
        ),
      aes(x = ghg_perc_diff * -100, y = value, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Health: avoided mortality",
      y = "NPV (2019 USD billion)",
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(0, 40),
      breaks = seq(0, 40, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  # ## make separete df for labor high and low for plotting
  # plot_df_labor_pts <- plot_df_labor %>%
  #   filter(!scen_id %in% remove_scen,
  #          title == "Labor: forgone wages",
  #          unit == "NPV (2019 USD billion)",
  #          refining_scenario != "historical production") %>%
  #   select(scen_id, demand_scenario, refining_scenario, scenario, ghg_perc_diff, high, low) %>%
  #   pivot_longer(high:low, names_to = "estimate", values_to =  "npv_2019_usd_billion")
  #
  fig_bxm_b <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  fig_bxm_b_2020_ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## 2020px, bartik correction
  fig_bxm_b_2020_ppx_bc <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-40, 0),
      breaks = seq(-40, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## legends
  low_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 1
    ) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = high, color = scen_id), shape = 1, size = 3, alpha = 0.8) +
    labs(
      color = "with re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    ylim(-60, 0) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  low_legend <- get_legend(
    low_legend_fig
  )

  ## save legends
  # Create legends directory if it doesn't exist
  legends_dir <- file.path(save_path, "results", "figures", "figure-3")
  if (!dir.exists(legends_dir)) {
    dir.create(legends_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ggsave(
    plot = low_legend,
    device = "pdf",
    filename = "fig3_low_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

  ## legends
  high_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = low, color = scen_id), shape = 16, size = 3, alpha = 1) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.8
    ) +
    labs(
      color = "no re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    ylim(-60, 0) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  high_legend <- get_legend(
    high_legend_fig
  )

  ggsave(
    plot = high_legend,
    device = "pdf",
    filename = "fig3_high_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

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

  # fig_bxm_c <- ggplot() +
  #   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  #   geom_vline(xintercept = hist_prod[title == "Health: avoided mortality", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
  #   geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
  #                                       title == "Health: avoided mortality per avoided GHG",
  #                                       unit == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
  #                                       unit_desc == "USD billion per GHG (2019 VSL)",
  #                                       !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  #   labs(color = "Policy",
  #        title = "C.",
  #        y = bquote('NPV (2019 USD million)\nper avoided GHG MtCO'[2]~e),
  #        x = "GHG emissions reduction (%, 2045 vs 2019)") +
  #   scale_color_manual(values = refin_colors) +
  #   ylim(0, 125) +
  #   xlim(0, 80) +
  #   theme_line +
  #   theme(legend.position = "none",
  #         axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm'))
  #
  # fig_bxm_d <- ggplot() +
  #   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  #   geom_vline(xintercept = hist_prod[title == "Labor: forgone wages per avoided GHG", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
  #   geom_point(data = plot_df_long %>% filter(!scen_id %in% remove_scen,
  #                                       title == "Labor: forgone wages per avoided GHG",
  #                                       unit == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
  #                                       !refining_scenario == "historic production"), aes(x = ghg_perc_diff * -100, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  #   labs(color = "Policy",
  #        title = "D.",
  #        y = NULL,
  #        # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
  #        x = "GHG emissions reduction (%, 2045 vs 2019)") +
  #   scale_color_manual(values = refin_colors) +
  #   theme_line +
  #   xlim(0, 80) +
  #   ylim(-125, 0) +
  #   theme(legend.position = "none",
  #         axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #         axis.ticks.length.y = unit(0.1, 'cm'),
  #         axis.ticks.length.x = unit(0.1, 'cm'))

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

  ## combine figure
  ## ---------------------------------

  ## shared x axis
  xaxis_lab <- ggdraw() +
    draw_label("GHG emissions reduction (%, 2045 vs 2019)", size = 12)

  fig3_plot_grid_ab_2020ppx <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig3_plot_grid_ab_2020ppx,
  #   save_path,
  #   "state_npv_fig_2020_ppx",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  ## bartik correction
  fig3_plot_grid_ab_2020ppx_bc <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx_bc,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig3_plot_grid_ab_2020ppx_bc,
  #   save_path,
  #   "state_npv_fig_2020_ppx_bartik",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  fig3_plot_grid_ab <- plot_grid(
    fig_bxm_a,
    fig_bxm_b,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # fig3_plot_grid2 <- plot_grid(
  #   fig3_plot_grid_ab,
  #   align = "v",
  #   # labels = c("(A)", "(B)", "(C)", ""),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   # hjust = -1,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  #   # rel_widths = c(1, 1),
  # )

  return(fig3_plot_grid_ab)
}


## -----------------------------------------------------------------------------
## NPV figure: refinery level emission factors
## -----------------------------------------------------------------------------

plot_npv_health_labor_ref <- function(
  main_path,
  save_path,
  refining_mortality,
  state_ghg_output,
  dt_ghg_2019,
  annual_all_impacts_labor
) {
  npv_df <- refining_mortality %>% as.data.table()

  ## state level
  state_npv_df <- npv_df[,
    .(
      sum_cost_2019_pv = sum(cost_2019_PV), ## constant VSL
      sum_cost_pv = sum(cost_PV)
    ), ## changing VSL
    by = .(scen_id, demand_scenario, refining_scenario)
  ]

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

  perc_diff_df <- ghg_2045[, .(
    demand_scenario,
    refining_scenario,
    ghg_2045,
    ghg_2019,
    perc_diff
  )]

  ## summarize by scenario, filter for total
  state_ghg_df <- state_ghg_output[
    source == "total",
    .(total_ghg = sum(value)),
    by = .(demand_scenario, refining_scenario)
  ]

  state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

  ## reference
  ref_df <- state_ghg_df[
    demand_scenario == "BAU" & refining_scenario == "historic production",
    .(total_ghg_mmt)
  ]
  setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
  ref_value <- ref_df$ref_ghg_mmt[1]

  ## merge with summarized df
  state_ghg_df[, ref_ghg := ref_value]
  state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]

  ## merge with health
  health_ghg_df <- merge(
    state_npv_df,
    state_ghg_df[, .(
      demand_scenario,
      refining_scenario,
      total_ghg_mmt,
      ref_ghg,
      avoided_ghg
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## summarize labor for state
  state_labor <- annual_all_impacts_labor[,
    .(
      # sum_total_emp = sum(total_emp),
      sum_total_comp_pv_h = sum(comp_all_impacts_PV_h),
      sum_total_comp_pv_l = sum(comp_all_impacts_PV_l, na.rm = T)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario
    )
  ]

  state_labor <- state_labor[oil_price_scenario == "reference case", ]

  ## ref labor
  ref_labor <- state_labor[
    demand_scenario == "BAU" & refining_scenario == "historic production"
  ]
  setnames(
    ref_labor,
    c("sum_total_comp_pv_h", "sum_total_comp_pv_l"),
    c("ref_total_comp_pv_h", "ref_total_comp_pv_l")
  )
  # setnames(ref_labor, c("sum_total_emp", "sum_total_comp_pv_h", "sum_total_comp_pv_l"), c("ref_total_emp", "ref_total_comp_pv_h", "ref_total_comp_pv_l"))
  ref_labor <- ref_labor[, .(
    product_scenario,
    indirect_induced_scenario,
    ref_total_comp_pv_h,
    ref_total_comp_pv_l
  )]

  ## add values to labor
  state_labor <- merge(
    state_labor,
    ref_labor,
    by = c("product_scenario", "indirect_induced_scenario"),
    all.x = T
  )

  # state_labor[, `:=`(
  #   # ref_total_emp = ref_labor$ref_total_emp[1],
  #   ref_total_comp_pv_h = ref_labor$ref_total_comp_pv_h[1],
  #   ref_total_comp_pv_l = ref_labor$ref_total_comp_pv_l[1]
  # )]

  state_labor[,
    forgone_wages_bil_h := (sum_total_comp_pv_h - ref_total_comp_pv_h) / 1e9
  ]
  state_labor[,
    forgone_wages_bil_l := (sum_total_comp_pv_l - ref_total_comp_pv_l) / 1e9
  ]

  ## merge with health and ghg
  health_labor_ghg_df <- merge(
    health_ghg_df,
    state_labor[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      sum_total_comp_pv_h,
      ref_total_comp_pv_h,
      forgone_wages_bil_h,
      sum_total_comp_pv_l,
      ref_total_comp_pv_l,
      forgone_wages_bil_l
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## add ghg perc reduction
  health_labor_ghg_df <- merge(
    health_labor_ghg_df,
    perc_diff_df,
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## prepare to plot
  plot_df <- health_labor_ghg_df[, .(
    scen_id,
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    sum_cost_pv_b,
    sum_cost_2019_pv_b,
    forgone_wages_bil_h,
    forgone_wages_bil_l,
    avoided_ghg,
    perc_diff
  )]

  setnames(plot_df, "perc_diff", "ghg_perc_diff")

  ## add values / avoided ghgs
  plot_df[, avoided_health_cost := sum_cost_2019_pv_b * -1]
  plot_df[, avoided_health_cost_annual_vsl := sum_cost_pv_b * -1]
  plot_df[, sum_cost_2019_pv_b := NULL]
  plot_df[, sum_cost_pv_b := NULL]

  plot_df[, `:=`(
    avoided_health_cost_ghg = avoided_health_cost / avoided_ghg,
    avoided_health_cost_ghg_vsl2 = avoided_health_cost_annual_vsl / avoided_ghg,
    forgone_wages_bil_h_ghg = forgone_wages_bil_h / avoided_ghg,
    forgone_wages_bil_l_ghg = forgone_wages_bil_l / avoided_ghg
  )]

  plot_df_health <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      ghg_perc_diff,
      avoided_health_cost,
      avoided_health_cost_annual_vsl,
      avoided_health_cost_ghg,
      avoided_health_cost_ghg_vsl2
    ) %>%
    pivot_longer(
      avoided_health_cost:avoided_health_cost_ghg_vsl2,
      names_to = "metric",
      values_to = "value"
    )

  ## add column for vsl
  plot_df_health <- plot_df_health %>%
    mutate(
      segment = "health",
      unit_desc = ifelse(
        metric == "avoided_health_cost",
        "USD billion (2019 VSL)",
        ifelse(
          metric == "avoided_health_cost_annual_vsl",
          "USD billion (annual VSL)",
          ifelse(
            metric == "avoided_health_cost_ghg",
            "USD billion per GHG (2019 VSL)",
            "USD billion per GHG (annual VSL)"
          )
        )
      ),
      metric = ifelse(
        metric %in% c("avoided_health_cost", "avoided_health_cost_annual_vsl"),
        "avoided_health_cost",
        "avoided_health_cost_ghg"
      )
    )

  plot_df_labor <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      ghg_perc_diff,
      forgone_wages_bil_h,
      forgone_wages_bil_l,
      forgone_wages_bil_h_ghg,
      forgone_wages_bil_l_ghg
    ) %>%
    pivot_longer(
      forgone_wages_bil_h:forgone_wages_bil_l_ghg,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      segment = "labor",
      unit_desc = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "USD billion",
        "USD billion per GHG"
      ),
      estimate = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_h_ghg"),
        "high",
        "low"
      ),
      metric = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "forgone_wages_bil",
        "forgone_wages_bil_ghg"
      )
    ) %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      ghg_perc_diff,
      segment,
      metric,
      unit_desc,
      estimate,
      value
    ) %>%
    pivot_wider(names_from = estimate, values_from = value)

  # plot_df_long <- rbind(plot_df_health, plot_df_labor)

  ## prepare health for plotting ------------------------------
  plot_df_health <- plot_df_health %>%
    mutate(
      title = ifelse(
        metric == "avoided_health_cost",
        "Health: avoided mortality",
        "Health: avoided mortality per avoided GHG"
      )
    )

  plot_df_health$title <- factor(
    plot_df_health$title,
    levels = c(
      "Health: avoided mortality",
      "Health: avoided mortality per avoided GHG"
    )
  )

  ## rename
  setDT(plot_df_health)
  plot_df_health[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_health[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_health$scenario <- factor(
    plot_df_health$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_health[,
    value := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      value * 1000,
      value
    )
  ]
  plot_df_health[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_health[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_health[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_health[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_health[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_health,
    folder_path = NULL,
    filename = "state_npv_fig_inputs_health_ref.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL
  )

  ## prepare labor ----------------------
  plot_df_labor <- plot_df_labor %>%
    mutate(
      title = ifelse(
        metric == "forgone_wages_bil",
        "Labor: forgone wages",
        "Labor: forgone wages per avoided GHG"
      )
    )

  plot_df_labor$title <- factor(
    plot_df_labor$title,
    levels = c("Labor: forgone wages", "Labor: forgone wages per avoided GHG")
  )

  ## rename
  setDT(plot_df_labor)
  plot_df_labor[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_labor[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_labor$scenario <- factor(
    plot_df_labor$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_labor[,
    high := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      high * 1000,
      high
    )
  ]
  plot_df_labor[,
    low := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      low * 1000,
      low
    )
  ]
  plot_df_labor[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_labor[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_labor[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_labor[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_labor[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  # fwrite(plot_df_labor, file.path(main_path, save_path, "state_npv_fig_inputs_labor.csv"))
  # fwrite(plot_df_labor, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_npv_fig_inputs_labor.csv"))

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  ## make the plot
  ## ---------------------------------------------------

  ## color for refining scenario
  refin_colors <- c(
    "LC1 low exports" = "#729b79",
    "LC1 historical exports" = "#2F4858",
    "BAU low exports" = "#F6AE2D",
    "BAU historical exports" = "#F26419"
  )

  refin_labs <- c(
    "LC1 low exports" = "Low demand, low exports",
    "LC1 historical exports" = "Low demand, historical exports",
    "BAU low exports" = "BAU demand, low exports",
    "BAU historical exports" = "BAU demand, historical exports"
  )

  ## refactor
  # plot_df_health$scen_id <- factor(plot_df_health$scen_id, levels = c('LC1 low exports',
  #                                                                     'LC1 historical production',
  #                                                                     'BAU demand\nlow exports',
  #                                                                     'Low demand\nhistorical exports',
  #                                                                     'Low demand\nlow exports',
  #                                                                     'Low demand\nhistorical production'))
  #

  ## figs - make each separately
  ## -------------------------------------------------------------------

  hist_prod <- as.data.table(
    plot_df_health %>%
      filter(
        scen_id == bau_scen,
        unit == "NPV (2019 USD billion)",
        unit_desc == "USD billion (2019 VSL)"
      )
  )

  fig_bxm_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    geom_point(
      data = plot_df_health %>%
        filter(
          !scen_id %in% remove_scen,
          title == "Health: avoided mortality",
          unit == "NPV (2019 USD billion)",
          unit_desc == "USD billion (2019 VSL)",
          !refining_scenario == "historical production"
        ),
      aes(x = ghg_perc_diff * -100, y = value, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Health: avoided mortality",
      y = "NPV (2019 USD billion)",
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(0, 60),
      breaks = seq(0, 60, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  # ## make separete df for labor high and low for plotting
  # plot_df_labor_pts <- plot_df_labor %>%
  #   filter(!scen_id %in% remove_scen,
  #          title == "Labor: forgone wages",
  #          unit == "NPV (2019 USD billion)",
  #          refining_scenario != "historical production") %>%
  #   select(scen_id, demand_scenario, refining_scenario, scenario, ghg_perc_diff, high, low) %>%
  #   pivot_longer(high:low, names_to = "estimate", values_to =  "npv_2019_usd_billion")
  #
  fig_bxm_b <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  fig_bxm_b_2020_ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## 2020px, bartik correction
  fig_bxm_b_2020_ppx_bc <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## legends
  low_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 1
    ) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = high, color = scen_id), shape = 1, size = 3, alpha = 0.8) +
    labs(
      color = "with re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    ylim(-60, 0) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  low_legend <- get_legend(
    low_legend_fig
  )

  # ## save legends
  # ggsave(
  #   plot = low_legend,
  #   device = "pdf",
  #   filename = "fig3_low_legend.pdf",
  #   path = file.path(main_path, save_path, "legends/"),
  #   dpi = 600
  # )

  ## legends
  high_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = low, color = scen_id), shape = 16, size = 3, alpha = 1) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.8
    ) +
    labs(
      color = "no re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  high_legend <- get_legend(
    high_legend_fig
  )

  # ## save legends
  # ggsave(
  #   plot = high_legend,
  #   device = "pdf",
  #   filename = "fig3_high_legend.pdf",
  #   path = file.path(main_path, save_path, "legends/"),
  #   dpi = 600
  # )
  #

  ## combine figure
  ## ---------------------------------

  ## shared x axis
  xaxis_lab <- ggdraw() +
    draw_label("GHG emissions reduction (%, 2045 vs 2019)", size = 12)

  fig3_plot_grid_ab_2020ppx <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig3_plot_grid_ab_2020ppx,
  #   save_path,
  #   "state_npv_fig_2020_ppx_ref",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  ## bartik correction
  fig3_plot_grid_ab_2020ppx_bc <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx_bc,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig3_plot_grid_ab_2020ppx_bc,
  #   save_path,
  #   "state_npv_fig_2020_ppx_bartik_ref",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  fig3_plot_grid_ab <- plot_grid(
    fig_bxm_a,
    fig_bxm_b,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # fig3_plot_grid2 <- plot_grid(
  #   fig3_plot_grid_ab,
  #   align = "v",
  #   # labels = c("(A)", "(B)", "(C)", ""),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   # hjust = -1,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  #   # rel_widths = c(1, 1),
  # )

  return(fig3_plot_grid_ab)
}


## -----------------------------------------------------------------------------
## NPV figure: cluster-level EFs, annual, age-based VSL
## -----------------------------------------------------------------------------

plot_npv_health_labor_annual_vsl <- function(
  main_path,
  save_path,
  refining_mortality,
  state_ghg_output,
  dt_ghg_2019,
  annual_all_impacts_labor
) {
  npv_df <- refining_mortality %>% as.data.table()

  ## state level
  state_npv_df <- npv_df[,
    .(
      sum_cost_2019_pv = sum(cost_2019_PV), ## constant VSL
      sum_cost_pv = sum(cost_PV)
    ), ## changing VSL
    by = .(scen_id, demand_scenario, refining_scenario)
  ]

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

  perc_diff_df <- ghg_2045[, .(
    demand_scenario,
    refining_scenario,
    ghg_2045,
    ghg_2019,
    perc_diff
  )]

  ## summarize by scenario, filter for total
  state_ghg_df <- state_ghg_output[
    source == "total",
    .(total_ghg = sum(value)),
    by = .(demand_scenario, refining_scenario)
  ]

  state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

  ## reference
  ref_df <- state_ghg_df[
    demand_scenario == "BAU" & refining_scenario == "historic production",
    .(total_ghg_mmt)
  ]
  setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
  ref_value <- ref_df$ref_ghg_mmt[1]

  ## merge with summarized df
  state_ghg_df[, ref_ghg := ref_value]
  state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]

  ## merge with health
  health_ghg_df <- merge(
    state_npv_df,
    state_ghg_df[, .(
      demand_scenario,
      refining_scenario,
      total_ghg_mmt,
      ref_ghg,
      avoided_ghg
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## summarize labor for state
  state_labor <- annual_all_impacts_labor[,
    .(
      # sum_total_emp = sum(total_emp),
      sum_total_comp_pv_h = sum(comp_all_impacts_PV_h),
      sum_total_comp_pv_l = sum(comp_all_impacts_PV_l, na.rm = T)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario
    )
  ]

  state_labor <- state_labor[oil_price_scenario == "reference case", ]

  ## ref labor
  ref_labor <- state_labor[
    demand_scenario == "BAU" & refining_scenario == "historic production"
  ]
  setnames(
    ref_labor,
    c("sum_total_comp_pv_h", "sum_total_comp_pv_l"),
    c("ref_total_comp_pv_h", "ref_total_comp_pv_l")
  )
  # setnames(ref_labor, c("sum_total_emp", "sum_total_comp_pv_h", "sum_total_comp_pv_l"), c("ref_total_emp", "ref_total_comp_pv_h", "ref_total_comp_pv_l"))
  ref_labor <- ref_labor[, .(
    product_scenario,
    indirect_induced_scenario,
    ref_total_comp_pv_h,
    ref_total_comp_pv_l
  )]

  ## add values to labor
  state_labor <- merge(
    state_labor,
    ref_labor,
    by = c("product_scenario", "indirect_induced_scenario"),
    all.x = T
  )

  # state_labor[, `:=`(
  #   # ref_total_emp = ref_labor$ref_total_emp[1],
  #   ref_total_comp_pv_h = ref_labor$ref_total_comp_pv_h[1],
  #   ref_total_comp_pv_l = ref_labor$ref_total_comp_pv_l[1]
  # )]

  state_labor[,
    forgone_wages_bil_h := (sum_total_comp_pv_h - ref_total_comp_pv_h) / 1e9
  ]
  state_labor[,
    forgone_wages_bil_l := (sum_total_comp_pv_l - ref_total_comp_pv_l) / 1e9
  ]

  ## merge with health and ghg
  health_labor_ghg_df <- merge(
    health_ghg_df,
    state_labor[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      sum_total_comp_pv_h,
      ref_total_comp_pv_h,
      forgone_wages_bil_h,
      sum_total_comp_pv_l,
      ref_total_comp_pv_l,
      forgone_wages_bil_l
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## add ghg perc reduction
  health_labor_ghg_df <- merge(
    health_labor_ghg_df,
    perc_diff_df,
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## prepare to plot
  plot_df <- health_labor_ghg_df[, .(
    scen_id,
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    sum_cost_pv_b,
    sum_cost_2019_pv_b,
    forgone_wages_bil_h,
    forgone_wages_bil_l,
    avoided_ghg,
    perc_diff
  )]

  setnames(plot_df, "perc_diff", "ghg_perc_diff")

  ## add values / avoided ghgs
  plot_df[, avoided_health_cost := sum_cost_2019_pv_b * -1]
  plot_df[, avoided_health_cost_annual_vsl := sum_cost_pv_b * -1]
  plot_df[, sum_cost_2019_pv_b := NULL]
  plot_df[, sum_cost_pv_b := NULL]

  plot_df[, `:=`(
    avoided_health_cost_ghg = avoided_health_cost / avoided_ghg,
    avoided_health_cost_ghg_vsl2 = avoided_health_cost_annual_vsl / avoided_ghg,
    forgone_wages_bil_h_ghg = forgone_wages_bil_h / avoided_ghg,
    forgone_wages_bil_l_ghg = forgone_wages_bil_l / avoided_ghg
  )]

  plot_df_health <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      ghg_perc_diff,
      avoided_health_cost,
      avoided_health_cost_annual_vsl,
      avoided_health_cost_ghg,
      avoided_health_cost_ghg_vsl2
    ) %>%
    pivot_longer(
      avoided_health_cost:avoided_health_cost_ghg_vsl2,
      names_to = "metric",
      values_to = "value"
    )

  ## add column for vsl
  plot_df_health <- plot_df_health %>%
    mutate(
      segment = "health",
      unit_desc = ifelse(
        metric == "avoided_health_cost",
        "USD billion (2019 VSL)",
        ifelse(
          metric == "avoided_health_cost_annual_vsl",
          "USD billion (annual VSL)",
          ifelse(
            metric == "avoided_health_cost_ghg",
            "USD billion per GHG (2019 VSL)",
            "USD billion per GHG (annual VSL)"
          )
        )
      ),
      metric = ifelse(
        metric %in% c("avoided_health_cost", "avoided_health_cost_annual_vsl"),
        "avoided_health_cost",
        "avoided_health_cost_ghg"
      )
    )

  plot_df_labor <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      ghg_perc_diff,
      forgone_wages_bil_h,
      forgone_wages_bil_l,
      forgone_wages_bil_h_ghg,
      forgone_wages_bil_l_ghg
    ) %>%
    pivot_longer(
      forgone_wages_bil_h:forgone_wages_bil_l_ghg,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      segment = "labor",
      unit_desc = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "USD billion",
        "USD billion per GHG"
      ),
      estimate = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_h_ghg"),
        "high",
        "low"
      ),
      metric = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "forgone_wages_bil",
        "forgone_wages_bil_ghg"
      )
    ) %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      ghg_perc_diff,
      segment,
      metric,
      unit_desc,
      estimate,
      value
    ) %>%
    pivot_wider(names_from = estimate, values_from = value)

  # plot_df_long <- rbind(plot_df_health, plot_df_labor)

  ## prepare health for plotting ------------------------------
  plot_df_health <- plot_df_health %>%
    mutate(
      title = ifelse(
        metric == "avoided_health_cost",
        "Health: avoided mortality",
        "Health: avoided mortality per avoided GHG"
      )
    )

  plot_df_health$title <- factor(
    plot_df_health$title,
    levels = c(
      "Health: avoided mortality",
      "Health: avoided mortality per avoided GHG"
    )
  )

  ## rename
  setDT(plot_df_health)
  plot_df_health[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_health[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_health$scenario <- factor(
    plot_df_health$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_health[,
    value := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      value * 1000,
      value
    )
  ]
  plot_df_health[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_health[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_health[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_health[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_health[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_health,
    folder_path = NULL,
    filename = "state_npv_fig_inputs_health_annual_vsl.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_npv_fig_inputs_health.csv")

  ## prepare labor ----------------------
  plot_df_labor <- plot_df_labor %>%
    mutate(
      title = ifelse(
        metric == "forgone_wages_bil",
        "Labor: forgone wages",
        "Labor: forgone wages per avoided GHG"
      )
    )

  plot_df_labor$title <- factor(
    plot_df_labor$title,
    levels = c("Labor: forgone wages", "Labor: forgone wages per avoided GHG")
  )

  ## rename
  setDT(plot_df_labor)
  plot_df_labor[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_labor[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_labor$scenario <- factor(
    plot_df_labor$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_labor[,
    high := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      high * 1000,
      high
    )
  ]
  plot_df_labor[,
    low := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      low * 1000,
      low
    )
  ]
  plot_df_labor[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_labor[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_labor[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_labor[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_labor[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  # fwrite(plot_df_labor, file.path(main_path, save_path, "state_npv_fig_inputs_labor.csv"))
  # fwrite(plot_df_labor, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_npv_fig_inputs_labor.csv"))

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  ## make the plot
  ## ---------------------------------------------------

  ## color for refining scenario
  refin_colors <- c(
    "LC1 low exports" = "#729b79",
    "LC1 historical exports" = "#2F4858",
    "BAU low exports" = "#F6AE2D",
    "BAU historical exports" = "#F26419"
  )

  refin_labs <- c(
    "LC1 low exports" = "Low demand, low exports",
    "LC1 historical exports" = "Low demand, historical exports",
    "BAU low exports" = "BAU demand, low exports",
    "BAU historical exports" = "BAU demand, historical exports"
  )

  ## refactor
  # plot_df_health$scen_id <- factor(plot_df_health$scen_id, levels = c('LC1 low exports',
  #                                                                     'LC1 historical production',
  #                                                                     'BAU demand\nlow exports',
  #                                                                     'Low demand\nhistorical exports',
  #                                                                     'Low demand\nlow exports',
  #                                                                     'Low demand\nhistorical production'))
  #

  ## figs - make each separately
  ## -------------------------------------------------------------------

  hist_prod <- as.data.table(
    plot_df_health %>%
      filter(
        scen_id == bau_scen,
        unit == "NPV (2019 USD billion)",
        unit_desc == "USD billion (2019 VSL)"
      )
  )

  fig_bxm_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    geom_point(
      data = plot_df_health %>%
        filter(
          !scen_id %in% remove_scen,
          title == "Health: avoided mortality",
          unit == "NPV (2019 USD billion)",
          unit_desc == "USD billion (annual VSL)",
          !refining_scenario == "historical production"
        ),
      aes(x = ghg_perc_diff * -100, y = value, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Health: avoided mortality",
      y = "NPV (2019 USD billion)",
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(0, 60),
      breaks = seq(0, 60, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  # ## make separete df for labor high and low for plotting
  # plot_df_labor_pts <- plot_df_labor %>%
  #   filter(!scen_id %in% remove_scen,
  #          title == "Labor: forgone wages",
  #          unit == "NPV (2019 USD billion)",
  #          refining_scenario != "historical production") %>%
  #   select(scen_id, demand_scenario, refining_scenario, scenario, ghg_perc_diff, high, low) %>%
  #   pivot_longer(high:low, names_to = "estimate", values_to =  "npv_2019_usd_billion")
  #
  fig_bxm_b <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  fig_bxm_b_2020_ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## 2020px, bartik correction
  fig_bxm_b_2020_ppx_bc <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## legends
  low_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 1
    ) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = high, color = scen_id), shape = 1, size = 3, alpha = 0.8) +
    labs(
      color = "with re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  low_legend <- get_legend(
    low_legend_fig
  )

  # ## save legends
  # ggsave(
  #   plot = low_legend,
  #   device = "pdf",
  #   filename = "fig3_low_legend.pdf",
  #   path = file.path(main_path, save_path, "legends/"),
  #   dpi = 600
  # )
  #

  ## legends
  high_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = low, color = scen_id), shape = 16, size = 3, alpha = 1) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.8
    ) +
    labs(
      color = "no re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    ylim(-60, 0) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  high_legend <- get_legend(
    high_legend_fig
  )

  # ## save legends
  # ggsave(
  #   plot = high_legend,
  #   device = "pdf",
  #   filename = "fig3_high_legend.pdf",
  #   path = file.path(main_path, save_path, "legends/"),
  #   dpi = 600
  # )
  #

  ## combine figure
  ## ---------------------------------

  ## shared x axis
  xaxis_lab <- ggdraw() +
    draw_label("GHG emissions reduction (%, 2045 vs 2019)", size = 12)

  fig3_plot_grid_ab_2020ppx <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig3_plot_grid_ab_2020ppx,
  #   save_path,
  #   "state_npv_fig_2020_ppx_annual_vsl",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  ## bartik correction
  fig3_plot_grid_ab_2020ppx_bc <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx_bc,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig3_plot_grid_ab_2020ppx_bc,
  #   save_path,
  #   "state_npv_fig_2020_ppx_bartik_annual_vsl",
  #   width = 10,
  #   height = 5,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  fig3_plot_grid_ab <- plot_grid(
    fig_bxm_a,
    fig_bxm_b,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # fig3_plot_grid2 <- plot_grid(
  #   fig3_plot_grid_ab,
  #   align = "v",
  #   # labels = c("(A)", "(B)", "(C)", ""),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   # hjust = -1,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  #   # rel_widths = c(1, 1),
  # )

  return(fig3_plot_grid_ab)
}


## -----------------------------------------------------------------------------
## NPV figure: constant VSL, non-age-based vsl, cluser EFs
## -----------------------------------------------------------------------------

plot_npv_health_labor_non_age_vsl <- function(
  main_path,
  save_path,
  refining_mortality,
  state_ghg_output,
  dt_ghg_2019,
  annual_all_impacts_labor
) {
  npv_df <- refining_mortality %>% as.data.table()

  ## state level
  state_npv_df <- npv_df[,
    .(
      sum_cost_2019_pv = sum(cost_2019_PV), ## constant VSL
      sum_cost_pv = sum(cost_PV)
    ), ## changing VSL
    by = .(scen_id, demand_scenario, refining_scenario)
  ]

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

  perc_diff_df <- ghg_2045[, .(
    demand_scenario,
    refining_scenario,
    ghg_2045,
    ghg_2019,
    perc_diff
  )]

  ## summarize by scenario, filter for total
  state_ghg_df <- state_ghg_output[
    source == "total",
    .(total_ghg = sum(value)),
    by = .(demand_scenario, refining_scenario)
  ]

  state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

  ## reference
  ref_df <- state_ghg_df[
    demand_scenario == "BAU" & refining_scenario == "historic production",
    .(total_ghg_mmt)
  ]
  setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
  ref_value <- ref_df$ref_ghg_mmt[1]

  ## merge with summarized df
  state_ghg_df[, ref_ghg := ref_value]
  state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]

  ## merge with health
  health_ghg_df <- merge(
    state_npv_df,
    state_ghg_df[, .(
      demand_scenario,
      refining_scenario,
      total_ghg_mmt,
      ref_ghg,
      avoided_ghg
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## summarize labor for state
  state_labor <- annual_all_impacts_labor[,
    .(
      # sum_total_emp = sum(total_emp),
      sum_total_comp_pv_h = sum(comp_all_impacts_PV_h),
      sum_total_comp_pv_l = sum(comp_all_impacts_PV_l, na.rm = T)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario
    )
  ]

  state_labor <- state_labor[oil_price_scenario == "reference case", ]

  ## ref labor
  ref_labor <- state_labor[
    demand_scenario == "BAU" & refining_scenario == "historic production"
  ]
  setnames(
    ref_labor,
    c("sum_total_comp_pv_h", "sum_total_comp_pv_l"),
    c("ref_total_comp_pv_h", "ref_total_comp_pv_l")
  )
  # setnames(ref_labor, c("sum_total_emp", "sum_total_comp_pv_h", "sum_total_comp_pv_l"), c("ref_total_emp", "ref_total_comp_pv_h", "ref_total_comp_pv_l"))
  ref_labor <- ref_labor[, .(
    product_scenario,
    indirect_induced_scenario,
    ref_total_comp_pv_h,
    ref_total_comp_pv_l
  )]

  ## add values to labor
  state_labor <- merge(
    state_labor,
    ref_labor,
    by = c("product_scenario", "indirect_induced_scenario"),
    all.x = T
  )

  # state_labor[, `:=`(
  #   # ref_total_emp = ref_labor$ref_total_emp[1],
  #   ref_total_comp_pv_h = ref_labor$ref_total_comp_pv_h[1],
  #   ref_total_comp_pv_l = ref_labor$ref_total_comp_pv_l[1]
  # )]

  state_labor[,
    forgone_wages_bil_h := (sum_total_comp_pv_h - ref_total_comp_pv_h) / 1e9
  ]
  state_labor[,
    forgone_wages_bil_l := (sum_total_comp_pv_l - ref_total_comp_pv_l) / 1e9
  ]

  ## merge with health and ghg
  health_labor_ghg_df <- merge(
    health_ghg_df,
    state_labor[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      sum_total_comp_pv_h,
      ref_total_comp_pv_h,
      forgone_wages_bil_h,
      sum_total_comp_pv_l,
      ref_total_comp_pv_l,
      forgone_wages_bil_l
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## add ghg perc reduction
  health_labor_ghg_df <- merge(
    health_labor_ghg_df,
    perc_diff_df,
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## prepare to plot
  plot_df <- health_labor_ghg_df[, .(
    scen_id,
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    sum_cost_pv_b,
    sum_cost_2019_pv_b,
    forgone_wages_bil_h,
    forgone_wages_bil_l,
    avoided_ghg,
    perc_diff
  )]

  setnames(plot_df, "perc_diff", "ghg_perc_diff")

  ## add values / avoided ghgs
  plot_df[, avoided_health_cost := sum_cost_2019_pv_b * -1]
  plot_df[, avoided_health_cost_annual_vsl := sum_cost_pv_b * -1]
  plot_df[, sum_cost_2019_pv_b := NULL]
  plot_df[, sum_cost_pv_b := NULL]

  plot_df[, `:=`(
    avoided_health_cost_ghg = avoided_health_cost / avoided_ghg,
    avoided_health_cost_ghg_vsl2 = avoided_health_cost_annual_vsl / avoided_ghg,
    forgone_wages_bil_h_ghg = forgone_wages_bil_h / avoided_ghg,
    forgone_wages_bil_l_ghg = forgone_wages_bil_l / avoided_ghg
  )]

  plot_df_health <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      ghg_perc_diff,
      avoided_health_cost,
      avoided_health_cost_annual_vsl,
      avoided_health_cost_ghg,
      avoided_health_cost_ghg_vsl2
    ) %>%
    pivot_longer(
      avoided_health_cost:avoided_health_cost_ghg_vsl2,
      names_to = "metric",
      values_to = "value"
    )

  ## add column for vsl
  plot_df_health <- plot_df_health %>%
    mutate(
      segment = "health",
      unit_desc = ifelse(
        metric == "avoided_health_cost",
        "USD billion (2019 VSL)",
        ifelse(
          metric == "avoided_health_cost_annual_vsl",
          "USD billion (annual VSL)",
          ifelse(
            metric == "avoided_health_cost_ghg",
            "USD billion per GHG (2019 VSL)",
            "USD billion per GHG (annual VSL)"
          )
        )
      ),
      metric = ifelse(
        metric %in% c("avoided_health_cost", "avoided_health_cost_annual_vsl"),
        "avoided_health_cost",
        "avoided_health_cost_ghg"
      )
    )

  plot_df_labor <- plot_df %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      ghg_perc_diff,
      forgone_wages_bil_h,
      forgone_wages_bil_l,
      forgone_wages_bil_h_ghg,
      forgone_wages_bil_l_ghg
    ) %>%
    pivot_longer(
      forgone_wages_bil_h:forgone_wages_bil_l_ghg,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      segment = "labor",
      unit_desc = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "USD billion",
        "USD billion per GHG"
      ),
      estimate = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_h_ghg"),
        "high",
        "low"
      ),
      metric = ifelse(
        metric %in% c("forgone_wages_bil_h", "forgone_wages_bil_l"),
        "forgone_wages_bil",
        "forgone_wages_bil_ghg"
      )
    ) %>%
    select(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      ghg_perc_diff,
      segment,
      metric,
      unit_desc,
      estimate,
      value
    ) %>%
    pivot_wider(names_from = estimate, values_from = value)

  # plot_df_long <- rbind(plot_df_health, plot_df_labor)

  ## prepare health for plotting ------------------------------
  plot_df_health <- plot_df_health %>%
    mutate(
      title = ifelse(
        metric == "avoided_health_cost",
        "Health: avoided mortality",
        "Health: avoided mortality per avoided GHG"
      )
    )

  plot_df_health$title <- factor(
    plot_df_health$title,
    levels = c(
      "Health: avoided mortality",
      "Health: avoided mortality per avoided GHG"
    )
  )

  ## rename
  setDT(plot_df_health)
  plot_df_health[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_health[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_health$scenario <- factor(
    plot_df_health$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_health[,
    value := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      value * 1000,
      value
    )
  ]
  plot_df_health[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_health[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_health[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_health[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_health[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_health,
    folder_path = NULL,
    filename = "state_npv_fig_inputs_health_non_age_vsl.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_npv_fig_inputs_health_non_age_vsl.csv")

  ## prepare labor ----------------------
  plot_df_labor <- plot_df_labor %>%
    mutate(
      title = ifelse(
        metric == "forgone_wages_bil",
        "Labor: forgone wages",
        "Labor: forgone wages per avoided GHG"
      )
    )

  plot_df_labor$title <- factor(
    plot_df_labor$title,
    levels = c("Labor: forgone wages", "Labor: forgone wages per avoided GHG")
  )

  ## rename
  setDT(plot_df_labor)
  plot_df_labor[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  plot_df_labor[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## refactor
  plot_df_labor$scenario <- factor(
    plot_df_labor$scenario,
    levels = c(
      "BAU demand - historic production",
      "BAU demand - historic exports",
      "BAU demand - low exports",
      "Low demand - historic exports",
      "Low demand - low exports",
      "Low demand - historic production"
    )
  )

  ## convert value of scaled outputs (by ghg) to millions, add unit column
  plot_df_labor[,
    high := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      high * 1000,
      high
    )
  ]
  plot_df_labor[,
    low := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_bil_ghg"),
      low * 1000,
      low
    )
  ]
  plot_df_labor[,
    metric := fifelse(
      metric == "forgone_wages_bil_ghg",
      "forgone_wages_ghg",
      metric
    )
  ]
  plot_df_labor[,
    unit := fifelse(
      metric %in% c("avoided_health_cost_ghg", "forgone_wages_ghg"),
      "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)",
      "NPV (2019 USD billion)"
    )
  ]

  ## change historic to historical
  plot_df_labor[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_labor[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_labor[, scenario := str_replace(scenario, "historic", "historical")]

  ## save figure inputs
  # fwrite(plot_df_labor, file.path(main_path, save_path, "state_npv_fig_inputs_labor.csv"))
  # fwrite(plot_df_labor, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_npv_fig_inputs_labor.csv"))

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  ## make the plot
  ## ---------------------------------------------------

  ## color for refining scenario
  refin_colors <- c(
    "LC1 low exports" = "#729b79",
    "LC1 historical exports" = "#2F4858",
    "BAU low exports" = "#F6AE2D",
    "BAU historical exports" = "#F26419"
  )

  refin_labs <- c(
    "LC1 low exports" = "Low demand, low exports",
    "LC1 historical exports" = "Low demand, historical exports",
    "BAU low exports" = "BAU demand, low exports",
    "BAU historical exports" = "BAU demand, historical exports"
  )

  ## refactor
  # plot_df_health$scen_id <- factor(plot_df_health$scen_id, levels = c('LC1 low exports',
  #                                                                     'LC1 historical production',
  #                                                                     'BAU demand\nlow exports',
  #                                                                     'Low demand\nhistorical exports',
  #                                                                     'Low demand\nlow exports',
  #                                                                     'Low demand\nhistorical production'))
  #

  ## figs - make each separately
  ## -------------------------------------------------------------------

  hist_prod <- as.data.table(
    plot_df_health %>%
      filter(
        scen_id == bau_scen,
        unit == "NPV (2019 USD billion)",
        unit_desc == "USD billion (2019 VSL)"
      )
  )

  fig_bxm_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    geom_point(
      data = plot_df_health %>%
        filter(
          !scen_id %in% remove_scen,
          title == "Health: avoided mortality",
          unit == "NPV (2019 USD billion)",
          unit_desc == "USD billion (2019 VSL)",
          !refining_scenario == "historical production"
        ),
      aes(x = ghg_perc_diff * -100, y = value, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Health: avoided mortality",
      y = "NPV (2019 USD billion)",
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(0, 60),
      breaks = seq(0, 60, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  # ## make separete df for labor high and low for plotting
  # plot_df_labor_pts <- plot_df_labor %>%
  #   filter(!scen_id %in% remove_scen,
  #          title == "Labor: forgone wages",
  #          unit == "NPV (2019 USD billion)",
  #          refining_scenario != "historical production") %>%
  #   select(scen_id, demand_scenario, refining_scenario, scenario, ghg_perc_diff, high, low) %>%
  #   pivot_longer(high:low, names_to = "estimate", values_to =  "npv_2019_usd_billion")
  #
  fig_bxm_b <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  fig_bxm_b_2020_ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario == "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## 2020px, bartik correction
  fig_bxm_b_2020_ppx_bc <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historical production",
    #                                                metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), linewidth = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          indirect_induced_scenario != "baseline",
          refining_scenario != "historical production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.9
    ) +
    labs(
      color = NULL,
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 2))

  ## legends
  low_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = low, color = scen_id),
      shape = 16,
      size = 3,
      alpha = 1
    ) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = high, color = scen_id), shape = 1, size = 3, alpha = 0.8) +
    labs(
      color = "with re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  low_legend <- get_legend(
    low_legend_fig
  )

  # ## save legends
  # ggsave(
  #   plot = low_legend,
  #   device = "pdf",
  #   filename = "fig3_low_legend.pdf",
  #   path = file.path(main_path, save_path, "legends/"),
  #   dpi = 600
  # )
  #

  ## legends
  high_legend_fig <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
    geom_vline(
      xintercept = hist_prod[
        title == "Health: avoided mortality",
        ghg_perc_diff * -100
      ],
      color = "darkgray",
      lty = 2
    ) +
    # geom_vline(xintercept = hist_prod[title == "Labor: forgone wages", ghg_perc_diff * -100], color = "darkgray", lty = 2) +
    # geom_linerange(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                                refining_scenario != "historic production"), aes(x = ghg_perc_diff * -100, ymin = high, ymax = low, color = scen_id), size = 0.5, alpha = 0.8) +
    # geom_point(data = plot_df_labor %>% filter(!scen_id %in% remove_scen,
    #                                            refining_scenario != "historic production",
    #                                            metric == "forgone_wages_bil"), aes(x = ghg_perc_diff * -100, y = low, color = scen_id), shape = 16, size = 3, alpha = 1) +
    geom_point(
      data = plot_df_labor %>%
        filter(
          !scen_id %in% remove_scen,
          refining_scenario != "historic production",
          metric == "forgone_wages_bil"
        ),
      aes(x = ghg_perc_diff * -100, y = high, color = scen_id),
      shape = 1,
      size = 3,
      alpha = 0.8
    ) +
    labs(
      color = "no re-emp:",
      title = "Labor: forgone wages",
      y = NULL,
      x = "GHG emissions reduction (%, 2045 vs 2019)"
    ) +
    scale_y_continuous(
      limits = c(-60, 0),
      breaks = seq(-60, 0, by = 10)
    ) +
    xlim(0, 80) +
    scale_color_manual(
      values = refin_colors,
      labels = refin_labs
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 11),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 11)
    ) +
    guides(color = guide_legend(nrow = 1))

  high_legend <- get_legend(
    high_legend_fig
  )

  # ## save legends
  # ggsave(
  #   plot = high_legend,
  #   device = "pdf",
  #   filename = "fig3_high_legend.pdf",
  #   path = file.path(main_path, save_path, "legends/"),
  #   dpi = 600
  # )
  #

  ## combine figure
  ## ---------------------------------

  ## shared x axis
  xaxis_lab <- ggdraw() +
    draw_label("GHG emissions reduction (%, 2045 vs 2019)", size = 12)

  fig3_plot_grid_ab_2020ppx <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  simple_ggsave_repo(
    plot = fig3_plot_grid_ab_2020ppx,
    folder_path = NULL,
    filename = "state_npv_fig_2020_ppx_non_age_vsl",
    width = 10,
    height = 5,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "extra-figure-3"
  )

  ## bartik correction
  fig3_plot_grid_ab_2020ppx_bc <- plot_grid(
    fig_bxm_a,
    fig_bxm_b_2020_ppx_bc,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  simple_ggsave_repo(
    plot = fig3_plot_grid_ab_2020ppx_bc,
    folder_path = NULL,
    filename = "state_npv_fig_2020_ppx_bartik_non_age_vsl",
    width = 10,
    height = 5,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "extra-figure-3"
  )

  fig3_plot_grid_ab <- plot_grid(
    fig_bxm_a,
    fig_bxm_b,
    align = "vh",
    labels = c("A", "B"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    rel_widths = c(1, 1)
  )

  # fig3_plot_grid2 <- plot_grid(
  #   fig3_plot_grid_ab,
  #   align = "v",
  #   # labels = c("(A)", "(B)", "(C)", ""),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   # hjust = -1,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  #   # rel_widths = c(1, 1),
  # )

  return(fig3_plot_grid_ab)
}


calc_county_pm25 <- function(
  main_path,
  save_path,
  health_weighted,
  raw_counties,
  raw_ct_2020_all,
  refining_mortality
) {
  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    as.data.table()

  health_df <- copy(health_weighted)

  health_df <- health_df[
    year == 2019 &
      scen_id == "BAU historic exports"
  ]

  county_names <- raw_counties %>%
    select(COUNTYFP, NAME) %>%
    st_drop_geometry() %>%
    unique()

  ## geoid to census tract
  county_df <- raw_ct_2020_all %>%
    filter(STATEFP == "06") %>%
    select(census_tract = GEOID, COUNTYFP, ALAND) %>%
    st_drop_geometry() %>%
    left_join(county_names) %>%
    left_join(pop_2020) %>%
    select(census_tract, COUNTYFP, NAME, pop, ALAND)

  health_df <- merge(health_df, county_df, by = "census_tract", all.x = T)

  health_county_df <- health_df %>%
    group_by(NAME, COUNTYFP, year) %>%
    summarise(
      avg_pm25_areaw = weighted.mean(total_pm25, ALAND),
      avg_pm25_popw = weighted.mean(total_pm25, pop)
    ) %>%
    ungroup() %>%
    arrange(-avg_pm25_popw)

  simple_fwrite_repo(
    health_county_df,
    folder_path = NULL,
    filename = "avg_pm25_county_2019.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "avg_pm25_county_2019.csv")

  return(health_county_df)
}


plot_health_levels <- function(main_path, save_path, health_grp) {
  fig2_df <- copy(health_grp)

  ## change scenario names, factor
  fig2_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # fig2_df[, scenario := gsub('BAU', 'Reference', scenario)]
  fig2_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## add scenario title
  fig2_df[, scenario_title := str_replace(scenario, " - ", "\n")]

  ## change historic to historical
  fig2_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  fig2_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  fig2_df[, scenario := str_replace(scenario, "historic", "historical")]
  fig2_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## refactor
  fig2_df$scenario_title <- factor(
    fig2_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## refactor
  fig2_df$scenario <- factor(
    fig2_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  ## scenarios for filtering
  # remove_scen <- c('LC1 historic production', 'BAU low exports', 'LC1 historic exports')
  remove_scen <- c("LC1 historical production")

  ## save figure inputs
  simple_fwrite_repo(
    fig2_df,
    folder_path = NULL,
    filename = "state_levels_fig_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_fig_inputs.csv")

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

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  health_level_fig_a <- ggplot(
    fig2_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = mortality_level_dem, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 0.4)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figa <- health_level_fig_a + theme(legend.position = "right")

  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_level_fig_b <- ggplot(
    fig2_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = mortality_level_dem, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 0.4)) +
    scale_linetype_manual(values = dac_lty) +
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

  legend_figb <- health_level_fig_b + theme(legend.position = "right")

  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_level_fig_c <- ggplot(
    fig2_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = mortality_level_dem, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 0.4)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.width = unit(10, "mm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figc <- health_level_fig_c + theme(legend.position = "right")

  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8))
  )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label(expression(paste("PM"[2.5], " (",mu,"g ", m^{-3},")", " per person")),
  #                                    size = 8, angle = 90)

  yaxis_lab <- ggdraw() + draw_label("Mortalities", size = 8, angle = 90)

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
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heights = c(1, 1, 1, 1.05)
  )

  fig2_plot_grid2 <- plot_grid(
    yaxis_lab,
    fig2_plot_grid,
    align = "v",
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


plot_health_levels_pc <- function(
  main_path,
  save_path,
  health_grp,
  refining_mortality,
  pop_ratios
) {
  mort_pc_df <- copy(health_grp)

  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()

  pop_2020[, demo_pop := pop * pct]

  ## summarize by demographic group
  pop_2020 <- pop_2020[,
    .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]

  ## merge population back with results
  mort_pc_df <- merge(
    mort_pc_df,
    pop_2020,
    by = c("demo_group", "demo_cat"),
    all.x = T
  )

  ## calculate per capita
  mort_pc_df[, value := mortality_level_dem / pop_2020]
  mort_pc_df[, value_pmil := value * 1e6]

  ## change scenario names, factor
  mort_pc_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # fig2_df[, scenario := gsub('BAU', 'Reference', scenario)]
  mort_pc_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## add scenario title
  mort_pc_df[, scenario_title := str_replace(scenario, " - ", "\n")]

  ## change historic to historical
  mort_pc_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  mort_pc_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  mort_pc_df[, scenario := str_replace(scenario, "historic", "historical")]
  mort_pc_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## refactor
  mort_pc_df$scenario_title <- factor(
    mort_pc_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## refactor
  mort_pc_df$scenario <- factor(
    mort_pc_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  ## scenarios for filtering
  # remove_scen <- c('LC1 historic production', 'BAU low exports', 'LC1 historic exports')
  remove_scen <- c("LC1 historical production")

  ## save figure inputs
  simple_fwrite_repo(
    mort_pc_df,
    folder_path = NULL,
    filename = "state_levels_pmil_fig_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_pmil_fig_inputs.csv")

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

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  health_level_fig_a <- ggplot(
    mort_pc_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = value_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 40)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figa <- health_level_fig_a + theme(legend.position = "right")

  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_level_fig_b <- ggplot(
    mort_pc_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = value_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 40)) +
    scale_linetype_manual(values = dac_lty) +
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

  legend_figb <- health_level_fig_b + theme(legend.position = "right")

  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_level_fig_c <- ggplot(
    mort_pc_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = value_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 40)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.width = unit(10, "mm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figc <- health_level_fig_c + theme(legend.position = "right")

  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8))
  )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label(expression(paste("PM"[2.5], " (",mu,"g ", m^{-3},")", " per person")),
  #                                    size = 8, angle = 90)

  yaxis_lab <- ggdraw() +
    draw_label("Mortalities per million people", size = 8, angle = 90)

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
    align = "v",
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
    align = "v",
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


plot_health_levels_pm25 <- function(main_path, save_path, health_grp) {
  fig2_df <- copy(health_grp)

  ## change scenario names, factor
  fig2_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # fig2_df[, scenario := gsub('BAU', 'Reference', scenario)]
  fig2_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## add scenario title
  fig2_df[, scenario_title := str_replace(scenario, " - ", "\n")]

  ## change historic to historical
  fig2_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  fig2_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  fig2_df[, scenario := str_replace(scenario, "historic", "historical")]
  fig2_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## refactor
  fig2_df$scenario_title <- factor(
    fig2_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## refactor
  fig2_df$scenario <- factor(
    fig2_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  ## save figure inputs
  simple_fwrite_repo(
    fig2_df,
    folder_path = NULL,
    filename = "state_levels_pm25_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_pm25_inputs.csv")

  ## scenarios for filtering
  # remove_scen <- c('LC1 historic production', 'BAU low exports', 'LC1 historic exports')
  remove_scen <- c("LC1 historical production")

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

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  health_level_fig_a <- ggplot(
    fig2_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = num_over_den, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 0.4)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figa <- health_level_fig_a + theme(legend.position = "right")

  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_level_fig_b <- ggplot(
    fig2_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = num_over_den, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 0.4)) +
    scale_linetype_manual(values = dac_lty) +
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

  legend_figb <- health_level_fig_b + theme(legend.position = "right")

  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_level_fig_c <- ggplot(
    fig2_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = num_over_den, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 0.4)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.width = unit(10, "mm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figc <- health_level_fig_c + theme(legend.position = "right")

  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8))
  )

  ## shared y lab
  yaxis_lab <- ggdraw() +
    draw_label(
      expression(paste(
        "PM"[2.5],
        " (",
        mu,
        "g ",
        m^{
          -3
        },
        ")",
        " per person"
      )),
      size = 8,
      angle = 90
    )

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
    align = "v",
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
    align = "v",
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


plot_health_levels_gaps <- function(main_path, save_path, health_grp) {
  gaps_df <- copy(health_grp)

  ## change scenario names, factor
  gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## refactor
  gaps_df[, scenario_title := scenario]
  gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## calculate gaps (BAU - scenario)
  bau_gaps_df <- gaps_df[scen_id == "BAU historic production"]
  bau_gaps_df <- bau_gaps_df[, c(
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "mortality_level_dem"
  )]
  setnames(bau_gaps_df, "mortality_level_dem", "bau_mortality_level_dem")

  gaps_df <- merge(
    gaps_df,
    bau_gaps_df,
    by = c("year", "demo_cat", "demo_group", "title"),
    all.x = T
  )

  gaps_df[, gap := mortality_level_dem - bau_mortality_level_dem]

  ## change historic to historical
  gaps_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  gaps_df$scenario <- factor(
    gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  gaps_df$scenario_title <- factor(
    gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## save figure inputs
  simple_fwrite_repo(
    gaps_df,
    folder_path = NULL,
    filename = "state_levels_fig_gaps_inputs.csv*", # Add asterisk for git tracking as specified in structure.md
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_fig_gaps_inputs.csv")

  ## make figures
  ## ---------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")

  ## figure a
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  health_gap_fig_a <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-0.31, 0)) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figa <- health_gap_fig_a + theme(legend.position = "right")

  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_gap_fig_b <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = gap, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-0.31, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    scale_linetype_manual(values = dac_lty) +
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

  legend_figb <- health_gap_fig_b + theme(legend.position = "right")

  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_gap_fig_c <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_linetype_manual(values = poverty_lty) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    # ylim(c(-0.31, 0)) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figc <- health_gap_fig_c + theme(legend.position = "right")

  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8))
  )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label(expression(paste("PM"[2.5], " (",mu,"g ", m^{-3},")", " per person, difference from reference")),
  #                                    size = 8, angle = 90)

  yaxis_lab <- ggdraw() +
    draw_label(
      "Avoided mortalities, difference from reference",
      size = 8,
      angle = 90
    )

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
    align = "v",
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
    align = "v",
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


plot_health_levels_gaps_pmil <- function(
  main_path,
  save_path,
  health_grp,
  refining_mortality,
  pop_ratios
) {
  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()

  pop_2020[, demo_pop := pop * pct]

  ## summarize by demographic group
  pop_2020 <- pop_2020[,
    .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]

  ## compute gaps
  ## ----------------------------------------------------
  gaps_df <- copy(health_grp)

  ## change scenario names, factor
  gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## refactor
  gaps_df[, scenario_title := scenario]
  gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## calculate gaps (BAU - scenario)
  bau_gaps_df <- gaps_df[scen_id == "BAU historic production"]
  bau_gaps_df <- bau_gaps_df[, c(
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "mortality_level_dem"
  )]
  setnames(bau_gaps_df, "mortality_level_dem", "bau_mortality_level_dem")

  gaps_df <- merge(
    gaps_df,
    bau_gaps_df,
    by = c("year", "demo_cat", "demo_group", "title"),
    all.x = T
  )

  gaps_df[, gap := mortality_level_dem - bau_mortality_level_dem]

  ## convert to per million
  gaps_df <- merge(
    gaps_df,
    pop_2020,
    by = c("demo_group", "demo_cat"),
    all.x = T
  )

  ## calculate per capita
  gaps_df[, value := gap / pop_2020]
  gaps_df[, value_pmil := value * 1e6]

  ## change historic to historical
  gaps_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  gaps_df$scenario <- factor(
    gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  gaps_df$scenario_title <- factor(
    gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## save figure inputs
  simple_fwrite_repo(
    gaps_df,
    folder_path = NULL,
    filename = "state_levels_fig_gaps_pmil_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_fig_gaps_pmil_inputs.csv")

  ## make figures
  ## ---------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")

  ## figure a
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  health_gap_fig_a <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = value_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-30, 0)) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figa <- health_gap_fig_a + theme(legend.position = "right")

  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_gap_fig_b <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = value_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-30, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    scale_linetype_manual(values = dac_lty) +
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

  legend_figb <- health_gap_fig_b + theme(legend.position = "right")

  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_gap_fig_c <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = value_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_linetype_manual(values = poverty_lty) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    # ylim(c(-30, 0)) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figc <- health_gap_fig_c + theme(legend.position = "right")

  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8))
  )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label(expression(paste("PM"[2.5], " (",mu,"g ", m^{-3},")", " per person, difference from reference")),
  #                                    size = 8, angle = 90)

  yaxis_lab <- ggdraw() +
    draw_label(
      "Avoided mortalities per million people, difference from reference",
      size = 8,
      angle = 90
    )

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
    align = "v",
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
    align = "v",
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


## plot health pm2.5 gaps

plot_health_levels_gaps_pm25 <- function(main_path, save_path, health_grp) {
  gaps_df <- copy(health_grp)

  ## change scenario names, factor
  gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## refactor
  gaps_df[, scenario_title := scenario]
  gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## calculate gaps (BAU - scenario)
  bau_gaps_df <- gaps_df[scen_id == "BAU historic production"]
  bau_gaps_df <- bau_gaps_df[, c(
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "num_over_den"
  )]
  setnames(bau_gaps_df, "num_over_den", "bau_num_over_den")

  gaps_df <- merge(
    gaps_df,
    bau_gaps_df,
    by = c("year", "demo_cat", "demo_group", "title"),
    all.x = T
  )

  gaps_df[, gap := num_over_den - bau_num_over_den]

  ## change historic to historical
  gaps_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  gaps_df$scenario <- factor(
    gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  gaps_df$scenario_title <- factor(
    gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## save figure inputs
  simple_fwrite_repo(
    gaps_df,
    folder_path = NULL,
    filename = "state_levels_fig_gaps_pm25_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_fig_gaps_pm25_inputs.csv")

  ## make figures
  ## ---------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")

  ## figure a
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  health_gap_fig_a <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-0.31, 0)) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figa <- health_gap_fig_a + theme(legend.position = "right")

  legend_a <- get_legend(
    legend_figa +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_gap_fig_b <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = gap, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-0.31, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    scale_linetype_manual(values = dac_lty) +
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

  legend_figb <- health_gap_fig_b + theme(legend.position = "right")

  legend_b <- get_legend(
    legend_figb +
      theme(legend.text = element_text(size = 8))
  )

  ##
  health_gap_fig_c <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_linetype_manual(values = poverty_lty) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    # ylim(c(-0.31, 0)) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  legend_figc <- health_gap_fig_c + theme(legend.position = "right")

  legend_c <- get_legend(
    legend_figc +
      theme(legend.text = element_text(size = 8))
  )

  ## shared y lab
  yaxis_lab <- ggdraw() +
    draw_label(
      expression(paste(
        "PM"[2.5],
        " (",
        mu,
        "g ",
        m^{
          -3
        },
        ")",
        " per person, difference from reference"
      )),
      size = 8,
      angle = 90
    )

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
    align = "v",
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
    align = "v",
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

plot_labor_levels <- function(
  main_path,
  save_path,
  ref_labor_demog_yr,
  refining_mortality,
  pop_ratios
) {
  # ## calc 2020 pop by demographic
  # pop_2020 <- refining_mortality %>%
  #   filter(year == 2020) %>%
  #   select(census_tract, year, pop) %>%
  #   unique() %>%
  #   left_join(pop_ratios) %>%
  #   as.data.table()
  #
  # pop_2020[, demo_pop := pop * pct]
  #
  # ## summarize by demographic group
  # pop_2020 <- pop_2020[, .(pop_2020 = sum(demo_pop)),
  #                      by = .(demo_group, demo_cat)]

  fig2_l_df <- copy(ref_labor_demog_yr)

  ## change scenario names, factor
  fig2_l_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # fig2_l_df[, scenario := gsub('BAU', 'Reference', scenario)]
  fig2_l_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## scenarios for filtering
  # remove_scen <- c('LC1 historic production', 'BAU low exports', 'LC1 historic exports')
  remove_scen <- c("Low demand - historical production")

  ## add scenario title
  fig2_l_df[, scenario_title := str_replace(scenario, " - ", "\n")]

  # ## sum for state
  # fig2_l_df <- fig2_l_df[, .(
  #   sum_demo_emp = sum(demo_emp),
  #   sum_demo_comp_pv_h = sum(demo_comp_pv_h),
  #   sum_demo_comp_pv_l = sum(demo_comp_pv_l)
  # ),
  # by = .(
  #   year, demand_scenario, refining_scenario, oil_price_scenario,
  #   scenario, scenario_title, demo_cat, demo_group, title
  # )
  # ]

  # ## merge with 2020 pop
  # fig2_l_df <- merge(fig2_l_df, pop_2020,
  #                    by = c("demo_cat", "demo_group"),
  #                    all.x = T)

  # ## calculate per capita
  # fig2_l_df[, demo_emp_pc := sum_demo_emp / pop_2020]
  # fig2_l_df[, demo_comp_pc_h := sum_demo_comp_pv_h / pop_2020]
  # fig2_l_df[, demo_comp_pc_l := sum_demo_comp_pv_l / pop_2020]
  #
  # ## select columns
  # fig2_l_df <- fig2_l_df[, .(year, demand_scenario, refining_scenario,
  #                            scenario, scenario_title, demo_cat, demo_group, title, sum_demo_emp,
  #                            demo_emp_pc, sum_demo_comp_pv_h, sum_demo_comp_pv_l, demo_comp_pc_h, demo_comp_pc_l)]

  ## change historic to historical
  fig2_l_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  fig2_l_df[, scenario := str_replace(scenario, "historic", "historical")]
  fig2_l_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## refactor
  fig2_l_df$scenario_title <- factor(
    fig2_l_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## refactor
  fig2_l_df$scenario <- factor(
    fig2_l_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  # ## test to see if this matches fig 5 outputs
  # test_state <- fig2_l_df %>%
  #   group_by(demand_scenario, refining_scenario, scenario, scenario_title, demo_cat, demo_group) %>%
  #   summarise(sum_demo_comp_pv = sum(sum_demo_comp_pv),
  #             demo_comp_pc = sum(demo_comp_pc)) %>%
  #   ungroup()
  #
  # bau_state <- test_state %>%
  #   filter(demand_scenario == "BAU" & refining_scenario == "historical production") %>%
  #   select(demo_cat, demo_group, sum_demo_comp_pv, demo_comp_pc) %>%
  #   rename(bau_comp = sum_demo_comp_pv,
  #          bau_comp_pc = demo_comp_pc)
  #
  # test_state <- test_state %>%
  #   left_join(bau_state) %>%
  #   mutate(diff = sum_demo_comp_pv - bau_comp, nj,
  #          diff_pc = demo_comp_pc - bau_comp_pc)
  #
  # ggplot(test_state %>% filter(!scenario_title %in% c("BAU demand\nhistorical production",
  #                                                  "Low demand\nhistorical production")), aes(y = diff_pc, x = scenario_title, color = demo_group)) +
  #   geom_point() +
  #   facet_wrap(~demo_cat, nrow = 3)
  #

  ## save figure inputs
  simple_fwrite_repo(
    fig2_l_df,
    folder_path = NULL,
    filename = "state_levels_labor_fig_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_labor_fig_inputs.csv")

  ## labor figure
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  # ## make the df longer - split in two, rbind
  # fig2_l_df_h <- fig2_l_df %>%
  #   select(-sum_demo_comp_pv_l, -demo_comp_pc_l) %>%
  #   pivot_longer(sum_demo_comp_pv_h:demo_comp_pc_h, names_to = "comp_metric", values_to = "high_est") %>%
  #   mutate(comp_metric = substr(comp_metric, 1, nchar(comp_metric) - 2))
  #
  # fig2_l_df <- fig2_l_df %>%
  #   select(-sum_demo_comp_pv_h, -demo_comp_pc_h) %>%
  #   pivot_longer(sum_demo_comp_pv_l:demo_comp_pc_l, names_to = "comp_metric", values_to = "low_est") %>%
  #   mutate(comp_metric = substr(comp_metric, 1, nchar(comp_metric) - 2)) %>%
  #   left_join(fig2_l_df_h)
  #

  ## labor fig a
  labor_level_fig_a <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        title %in% fig_title_vec,
        demo_cat == "Race",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Hispanic", "white", "Asian", "Black"))
      ),
    aes(x = year, y = sum_demo_emp / 1000, color = title, group = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 45)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figa <- labor_level_fig_a + theme(legend.position = "right")
  #
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_b <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        demo_cat == "DAC",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = sum_demo_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = dac_lty
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 45)) +
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

  # legend_figb <- labor_level_fig_b + theme(legend.position = "right")
  #
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_c <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        demo_cat == "Poverty",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = sum_demo_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_color_manual(
      name = "",
      values = poverty_lty
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 45)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.width = unit(10, "mm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figc <- health_level_fig_c + theme(legend.position = "right")
  #
  # legend_c <- get_legend(
  #   legend_figc +
  #     theme(legend.text = element_text(size = 8)))

  ## 2020 prices version
  ## labor fig a
  labor_level_fig_a_2020ppx <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        title %in% fig_title_vec,
        demo_cat == "Race",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Hispanic", "white", "Asian", "Black"))
      ),
    aes(x = year, y = sum_demo_emp / 1000, color = title, group = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 45)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ##
  labor_level_fig_b_2020ppx <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        demo_cat == "DAC",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = sum_demo_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = dac_lty
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 45)) +
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

  ##
  labor_level_fig_c_2020ppx <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        demo_cat == "Poverty",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = sum_demo_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_color_manual(
      name = "",
      values = poverty_lty
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 45)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.width = unit(10, "mm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## shared y lab
  yaxis_lab <- ggdraw() +
    draw_label("Labor: FTE job-years (thousand)", size = 8, angle = 90)

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
  fig2_l_plot_grid_2020ppx <- plot_grid(
    labor_level_fig_b_2020ppx,
    labor_level_fig_c_2020ppx,
    labor_level_fig_a_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )

  fig2_l_plot_grid2_2020ppx <- plot_grid(
    yaxis_lab,
    fig2_l_plot_grid_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig2_l_plot_grid2_2020ppx,
  #   save_path,
  #   "state_labor_levels_fig_2020ppx",
  #   width = 12,
  #   height = 8,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  ## changing prod prices
  fig2_l_plot_grid <- plot_grid(
    labor_level_fig_b,
    labor_level_fig_c,
    labor_level_fig_a,
    align = "v",
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
    align = "v",
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


plot_labor_levels_pmil <- function(
  main_path,
  save_path,
  ref_labor_demog_yr,
  refining_mortality,
  pop_ratios
) {
  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()

  pop_2020[, demo_pop := pop * pct]

  ## summarize by demographic group
  pop_2020 <- pop_2020[,
    .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]

  fig2_l_df <- copy(ref_labor_demog_yr)

  ## change scenario names, factor
  fig2_l_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # fig2_l_df[, scenario := gsub('BAU', 'Reference', scenario)]
  fig2_l_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## scenarios for filtering
  # remove_scen <- c('LC1 historic production', 'BAU low exports', 'LC1 historic exports')
  remove_scen <- c("Low demand - historical production")

  ## add scenario title
  fig2_l_df[, scenario_title := str_replace(scenario, " - ", "\n")]

  #
  #   ## sum for state
  #   fig2_l_df <- fig2_l_df[, .(
  #     sum_demo_emp = sum(demo_emp),
  #     sum_demo_comp_pv_h = sum(demo_comp_pv_h),
  #     sum_demo_comp_pv_l = sum(demo_comp_pv_l)
  #   ),
  #   by = .(
  #     year, demand_scenario, refining_scenario, oil_price_scenario,
  #     scenario, scenario_title, demo_cat, demo_group, title
  #   )
  #   ]

  ## merge with 2020 pop
  fig2_l_df <- merge(
    fig2_l_df,
    pop_2020,
    by = c("demo_cat", "demo_group"),
    all.x = T
  )

  ## calculate per capita
  fig2_l_df[, demo_emp_pc := sum_demo_emp / pop_2020]
  fig2_l_df[, demo_emp_pmil := demo_emp_pc * 1e6]
  fig2_l_df[, demo_comp_pc_h := sum_demo_comp_pv_h / pop_2020]
  fig2_l_df[, demo_comp_pc_pmil_h := demo_comp_pc_h * 1e6]
  fig2_l_df[, demo_comp_pc_l := sum_demo_comp_pv_l / pop_2020]
  fig2_l_df[, demo_comp_pc_pmil_l := demo_comp_pc_l * 1e6]

  ## select columns
  fig2_l_df <- fig2_l_df[, .(
    year,
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    scenario,
    scenario_title,
    demo_cat,
    demo_group,
    title,
    sum_demo_emp,
    demo_emp_pc,
    demo_emp_pmil,
    sum_demo_comp_pv_h,
    sum_demo_comp_pv_l,
    demo_comp_pc_pmil_h,
    demo_comp_pc_pmil_l,
    demo_comp_pc_h,
    demo_comp_pc_l
  )]

  ## change historic to historical
  fig2_l_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  fig2_l_df[, scenario := str_replace(scenario, "historic", "historical")]
  fig2_l_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## refactor
  fig2_l_df$scenario_title <- factor(
    fig2_l_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## refactor
  fig2_l_df$scenario <- factor(
    fig2_l_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  # ## test to see if this matches fig 5 outputs
  # test_state <- fig2_l_df %>%
  #   group_by(demand_scenario, refining_scenario, scenario, scenario_title, demo_cat, demo_group) %>%
  #   summarise(sum_demo_comp_pv = sum(sum_demo_comp_pv),
  #             demo_comp_pc = sum(demo_comp_pc)) %>%
  #   ungroup()
  #
  # bau_state <- test_state %>%
  #   filter(demand_scenario == "BAU" & refining_scenario == "historical production") %>%
  #   select(demo_cat, demo_group, sum_demo_comp_pv, demo_comp_pc) %>%
  #   rename(bau_comp = sum_demo_comp_pv,
  #          bau_comp_pc = demo_comp_pc)
  #
  # test_state <- test_state %>%
  #   left_join(bau_state) %>%
  #   mutate(diff = sum_demo_comp_pv - bau_comp, nj,
  #          diff_pc = demo_comp_pc - bau_comp_pc)
  #
  # ggplot(test_state %>% filter(!scenario_title %in% c("BAU demand\nhistorical production",
  #                                                  "Low demand\nhistorical production")), aes(y = diff_pc, x = scenario_title, color = demo_group)) +
  #   geom_point() +
  #   facet_wrap(~demo_cat, nrow = 3)
  #

  ## save figure inputs
  simple_fwrite_repo(
    fig2_l_df,
    folder_path = NULL,
    filename = "state_levels_labor_pmil_fig_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_levels_labor_pmil_fig_inputs.csv")

  ## labor figure
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  ## make the df longer - split in two, rbind
  fig2_l_df_h <- fig2_l_df %>%
    select(-sum_demo_comp_pv_l, -demo_comp_pc_pmil_l, -demo_comp_pc_l) %>%
    pivot_longer(
      sum_demo_comp_pv_h:demo_comp_pc_h,
      names_to = "comp_metric",
      values_to = "no_re_emp"
    ) %>%
    mutate(comp_metric = substr(comp_metric, 1, nchar(comp_metric) - 2)) %>%
    as.data.table()

  fig2_l_df <- fig2_l_df %>%
    select(-sum_demo_comp_pv_h, -demo_comp_pc_pmil_h, -demo_comp_pc_h) %>%
    pivot_longer(
      sum_demo_comp_pv_l:demo_comp_pc_l,
      names_to = "comp_metric",
      values_to = "with_re_emp"
    ) %>%
    mutate(comp_metric = substr(comp_metric, 1, nchar(comp_metric) - 2)) %>%
    left_join(fig2_l_df_h) %>%
    as.data.table()

  ## just employment
  fig2_l_df <- fig2_l_df %>%
    select(year:demo_emp_pmil) %>%
    unique()

  ## labor fig a
  labor_level_fig_a <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        title %in% fig_title_vec,
        demo_cat == "Race",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Hispanic", "white", "Asian", "Black"))
      ),
    aes(x = year, y = demo_emp_pmil, color = title, group = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 3500)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figa <- labor_level_fig_a + theme(legend.position = "right")
  #
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_b <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario == "changing prices",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = demo_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = dac_lty
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 3500)) +
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

  # legend_figb <- labor_level_fig_b + theme(legend.position = "right")
  #
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_c <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        demo_cat == "Poverty",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = demo_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_color_manual(
      name = "",
      values = poverty_lty
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 3500)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.width = unit(10, "mm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figc <- health_level_fig_c + theme(legend.position = "right")
  #
  # legend_c <- get_legend(
  #   legend_figc +
  #     theme(legend.text = element_text(size = 8)))

  ## make version with 2020 product prices
  ## --------------------------------------------------------------------

  ## labor fig a
  labor_level_fig_a_2020ppx <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        title %in% fig_title_vec,
        demo_cat == "Race",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Hispanic", "white", "Asian", "Black"))
      ),
    aes(x = year, y = demo_emp_pmil, color = title, group = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 3500)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figa <- labor_level_fig_a + theme(legend.position = "right")
  #
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_b_2020ppx <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario != "changing prices",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = demo_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = dac_lty
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 3500)) +
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

  # legend_figb <- labor_level_fig_b + theme(legend.position = "right")
  #
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_level_fig_c_2020ppx <- ggplot(
    fig2_l_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        demo_cat == "Poverty",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = demo_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_color_manual(
      name = "",
      values = poverty_lty
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(0, 3500)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.width = unit(10, "mm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## shared y lab
  yaxis_lab <- ggdraw() +
    draw_label("Labor: FTE job-years per million people", size = 8, angle = 90)

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

  fig2_l_plot_grid_2020ppx <- plot_grid(
    labor_level_fig_b_2020ppx,
    labor_level_fig_c_2020ppx,
    labor_level_fig_a_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )

  fig2_l_plot_grid2_2020ppx <- plot_grid(
    yaxis_lab,
    fig2_l_plot_grid_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )

  # simple_ggsave_repo(
  #   fig2_l_plot_grid2_2020ppx,
  #   save_path,
  #   "state_labor_levels_pmil_fig_2020ppx",
  #   width = 12,
  #   height = 8,
  #   dpi = 600
  # )
  ## NOTE: Figure save moved to _targets.R for correct path handling

  ####

  fig2_l_plot_grid <- plot_grid(
    labor_level_fig_b,
    labor_level_fig_c,
    labor_level_fig_a,
    align = "v",
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
    align = "v",
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


plot_labor_levels_gaps <- function(
  main_path,
  save_path,
  ref_labor_demog_yr,
  refining_mortality,
  pop_ratios
) {
  # ## calc 2020 pop by demographic
  # pop_2020 <- refining_mortality %>%
  #   filter(year == 2020) %>%
  #   select(census_tract, year, pop) %>%
  #   unique() %>%
  #   left_join(pop_ratios) %>%
  #   as.data.table()
  #
  # pop_2020[, demo_pop := pop * pct]
  #
  # ## summarize by demographic group
  # pop_2020 <- pop_2020[, .(pop_2020 = sum(demo_pop)),
  #                      by = .(demo_group, demo_cat)]
  #
  ## labor outputs
  l_gaps_df <- copy(ref_labor_demog_yr)
  l_gaps_df <- l_gaps_df[oil_price_scenario == "reference case", ]

  ## change scenario names, factor
  l_gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  l_gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## add scenario title
  l_gaps_df[, scenario_title := scenario]
  l_gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## change historic to historical
  l_gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  l_gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  l_gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## scenarios for filtering
  remove_scen <- c(
    "Low demand - historical production",
    "BAU demand - historical production"
  )

  l_gaps_df$scenario <- factor(
    l_gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  l_gaps_df$scenario_title <- factor(
    l_gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  # ## sum for state
  # l_gaps_df <- l_gaps_df[, .(sum_demo_emp = sum(demo_emp)),
  #   by = .(
  #     year, demand_scenario, refining_scenario, oil_price_scenario,
  #     scenario, scenario_title, demo_cat, demo_group, title
  #   )
  # ]

  ## calculate gaps (BAU - scenario)
  l_bau_gaps_df <- l_gaps_df[scenario == "BAU demand - historical production"]
  l_bau_gaps_df <- l_bau_gaps_df[, c(
    "product_scenario",
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "sum_demo_emp"
  )]
  setnames(l_bau_gaps_df, "sum_demo_emp", "bau_sum_demo_emp")

  l_gaps_df <- merge(
    l_gaps_df,
    l_bau_gaps_df,
    by = c("year", "demo_cat", "demo_group", "title", "product_scenario"),
    all.x = T
  )

  l_gaps_df[, gap_emp := sum_demo_emp - bau_sum_demo_emp]

  # ## normalize if needed
  # ## merge with 2020 pop
  # l_gaps_df <- merge(l_gaps_df, pop_2020,
  #                    by = c("demo_cat", "demo_group"),
  #                    all.x = T)
  #
  # ## calculate per capita
  # l_gaps_df[, demo_emp_pc := sum_demo_emp / pop_2020]
  #
  # ## select columns
  l_gaps_df <- l_gaps_df[, .(
    year,
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    scenario,
    scenario_title,
    demo_cat,
    demo_group,
    title,
    sum_demo_emp,
    gap_emp
  )]

  ## save figure inputs
  simple_fwrite_repo(
    l_gaps_df,
    folder_path = NULL,
    filename = "state_labor_levels_fig_gaps_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_labor_levels_fig_gaps_inputs.csv")

  ## figure a
  fig_title_vec <- c("Black", "Asian", "white", "Hispanic")

  ## make version with 2020 product ppx

  labor_gap_fig_a_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race",
        product_scenario != "changing prices",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Asian", "white", "Hispanic"))
      ),
    aes(x = year, y = gap_emp / 1000, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-40, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ##
  labor_gap_fig_b_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario != "changing prices",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-40, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-3500, 0)) +
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

  ##
  labor_gap_fig_c_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "Poverty",
        product_scenario != "changing prices",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-40, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## original version with changing prices

  labor_gap_fig_a <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race",
        product_scenario == "changing prices",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Asian", "white", "Hispanic"))
      ),
    aes(x = year, y = gap_emp / 1000, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-40, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figa <- health_gap_fig_a + theme(legend.position = "right")
  #
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_gap_fig_b <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario == "changing prices",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-40, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-3500, 0)) +
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

  # legend_figb <- health_gap_fig_b + theme(legend.position = "right")
  #
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_gap_fig_c <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "Poverty",
        product_scenario == "changing prices",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-40, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )
  #
  #   legend_figc <- health_gap_fig_c + theme(legend.position = "right")
  #
  #   legend_c <- get_legend(
  #     legend_figc +
  #       theme(legend.text = element_text(size = 8)))

  ## shared y lab
  yaxis_lab <- ggdraw() +
    draw_label(
      "Labor: FTE-jobs, difference from reference (thousand)",
      size = 8,
      angle = 90
    )

  ## save 2020 ppx version
  l_gaps_plot_grid_2020ppx <- plot_grid(
    labor_gap_fig_b_2020ppx,
    labor_gap_fig_c_2020ppx,
    labor_gap_fig_a_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )

  l_gaps_plot_grid2_2020ppx <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )

  simple_ggsave_repo(
    plot = l_gaps_plot_grid2_2020ppx,
    folder_path = NULL,
    filename = "state_labor_gaps_fig_2020ppx",
    width = 12,
    height = 8,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "labor-figures",
    extra_subfolder = "labor-figures"
  )

  ## original version

  l_gaps_plot_grid <- plot_grid(
    labor_gap_fig_b,
    labor_gap_fig_c,
    labor_gap_fig_a,
    align = "v",
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
    align = "v",
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

plot_labor_levels_gaps_pmil <- function(
  main_path,
  save_path,
  ref_labor_demog_yr,
  refining_mortality,
  pop_ratios
) {
  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()

  pop_2020[, demo_pop := pop * pct]

  ## summarize by demographic group
  pop_2020 <- pop_2020[,
    .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]

  ## labor outputs
  l_gaps_df <- copy(ref_labor_demog_yr)

  ## change scenario names, factor
  l_gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  l_gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## add scenario title
  l_gaps_df[, scenario_title := scenario]
  l_gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## change historic to historical
  l_gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  l_gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  l_gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## scenarios for filtering
  remove_scen <- c(
    "Low demand - historical production",
    "BAU demand - historical production"
  )

  l_gaps_df$scenario <- factor(
    l_gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  l_gaps_df$scenario_title <- factor(
    l_gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )
  # ## sum for state
  # l_gaps_df <- l_gaps_df[, .(sum_demo_emp = sum(demo_emp)),
  #   by = .(
  #     year, demand_scenario, refining_scenario, oil_price_scenario,
  #     scenario, scenario_title, demo_cat, demo_group, title
  #   )
  # ]

  ## select columns
  l_gaps_df <- l_gaps_df[, .(
    year,
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    scenario,
    scenario_title,
    demo_cat,
    demo_group,
    title,
    sum_demo_emp
  )]

  l_gaps_df <- l_gaps_df[oil_price_scenario == "reference case", ]

  ## calculate gaps (BAU - scenario)
  l_bau_gaps_df <- l_gaps_df[scenario == "BAU demand - historical production"]
  l_bau_gaps_df <- l_bau_gaps_df[, c(
    "product_scenario",
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "sum_demo_emp"
  )]
  setnames(l_bau_gaps_df, "sum_demo_emp", "bau_sum_demo_emp")

  l_gaps_df <- merge(
    l_gaps_df,
    l_bau_gaps_df,
    by = c("year", "demo_cat", "demo_group", "title", "product_scenario"),
    all.x = T
  )

  l_gaps_df[, gap_emp := sum_demo_emp - bau_sum_demo_emp]

  ## merge with 2020 pop
  l_gaps_df <- merge(
    l_gaps_df,
    pop_2020,
    by = c("demo_cat", "demo_group"),
    all.x = T
  )

  ## calculate per capita
  l_gaps_df[, gap_emp_pc := gap_emp / pop_2020]
  l_gaps_df[, gap_emp_pmil := gap_emp_pc * 1e6]

  ## save figure inputs
  simple_fwrite_repo(
    l_gaps_df,
    folder_path = NULL,
    filename = "state_labor_levels_fig_gaps_pmil_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_labor_levels_fig_gaps_pmil_inputs.csv")

  ## figure a
  fig_title_vec <- c("Black", "Asian", "white", "Hispanic")

  ## 2020 product prices
  labor_gap_fig_a_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        title %in% fig_title_vec,
        demo_cat == "Race",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Asian", "white", "Hispanic"))
      ),
    aes(x = year, y = gap_emp_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(-2500, 0) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ##
  labor_gap_fig_b_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        demo_cat == "DAC",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(-2500, 0) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-3500, 0)) +
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

  ##
  labor_gap_fig_c_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        demo_cat == "Poverty",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    # ylim(-2500, 0) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## original

  labor_gap_fig_a <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        title %in% fig_title_vec,
        demo_cat == "Race",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Asian", "white", "Hispanic"))
      ),
    aes(x = year, y = gap_emp_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(-2500, 0) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figa <- health_gap_fig_a + theme(legend.position = "right")
  #
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_gap_fig_b <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        demo_cat == "DAC",
        oil_price_scenario == "reference case"
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(-2500, 0) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-3500, 0)) +
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

  # legend_figb <- health_gap_fig_b + theme(legend.position = "right")
  #
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 8)))

  ##
  labor_gap_fig_c <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        demo_cat == "Poverty",
        oil_price_scenario == "reference case"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    # ylim(-2500, 0) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )
  #
  #   legend_figc <- health_gap_fig_c + theme(legend.position = "right")
  #
  #   legend_c <- get_legend(
  #     legend_figc +
  #       theme(legend.text = element_text(size = 8)))

  ## shared y lab
  yaxis_lab <- ggdraw() +
    draw_label(
      "Labor: FTE-jobs, difference from reference per million people",
      size = 8,
      angle = 90
    )

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

  ## save 2020 ppx
  l_gaps_plot_grid_2020ppx <- plot_grid(
    labor_gap_fig_b_2020ppx,
    labor_gap_fig_c_2020ppx,
    labor_gap_fig_a_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1),
    rel_heighs = c(1, 1, 1, 1.05)
  )

  l_gaps_plot_grid2_2020ppx <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heighs = c(1, 1)
  )

  simple_ggsave_repo(
    plot = l_gaps_plot_grid2_2020ppx,
    folder_path = NULL,
    filename = "state_labor_gaps_pmil_fig_2020ppx",
    width = 12,
    height = 8,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "labor-figures",
    extra_subfolder = "labor-figures"
  )

  ## original

  l_gaps_plot_grid <- plot_grid(
    labor_gap_fig_b,
    labor_gap_fig_c,
    labor_gap_fig_a,
    align = "v",
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
    align = "v",
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


############################################################################
############################################################################

plot_hl_levels_df <- function(
  main_path,
  save_path,
  ref_mortality_demog,
  ref_labor_demog,
  state_ghg_output,
  dt_ghg_2019
) {
  health_df <- copy(ref_mortality_demog)

  ## group by scenario, demo_cat, demo_group, title, and sum
  health_df <- health_df[,
    .(
      sum_cost_2019_pv = sum(demo_cost_2019_PV, na.rm = T), ## constant VSL
      sum_cost_pv = sum(demo_cost_PV, na.rm = T)
    ), ## changing VSL
    by = .(
      scen_id,
      demand_scenario,
      refining_scenario,
      demo_cat,
      demo_group,
      title
    )
  ]

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

  perc_diff_df <- ghg_2045[, .(
    demand_scenario,
    refining_scenario,
    ghg_2045,
    ghg_2019,
    perc_diff
  )]

  ## summarize by scenario, filter for total
  state_ghg_df <- state_ghg_output[
    source == "total",
    .(total_ghg = sum(value)),
    by = .(demand_scenario, refining_scenario)
  ]

  state_ghg_df[, total_ghg_mmt := (total_ghg / 1000) / 1e6]

  ## reference
  ref_df <- state_ghg_df[
    demand_scenario == "BAU" & refining_scenario == "historic production",
    .(total_ghg_mmt)
  ]
  setnames(ref_df, "total_ghg_mmt", "ref_ghg_mmt")
  ref_value <- ref_df$ref_ghg_mmt[1]

  ## merge with summarized df
  state_ghg_df[, ref_ghg := ref_value]
  state_ghg_df[, avoided_ghg := (total_ghg_mmt - ref_value) * -1]

  ## merge with health
  health_ghg_df <- merge(
    health_df,
    state_ghg_df[, .(
      demand_scenario,
      refining_scenario,
      total_ghg_mmt,
      ref_ghg,
      avoided_ghg
    )],
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## labor
  labor_df <- copy(ref_labor_demog)
  labor_df <- labor_df[oil_price_scenario == "reference case", ]

  ## summarize across years
  labor_df <- labor_df[,
    .(
      sum_demo_emp = sum(sum_demo_emp),
      sum_demo_comp_pv_h = sum(sum_demo_comp_pv_h),
      sum_demo_comp_pv_l = sum(sum_demo_comp_pv_l)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      demo_cat,
      demo_group,
      title
    )
  ]

  ## ref labor
  ref_labor <- labor_df[
    demand_scenario == "BAU" & refining_scenario == "historic production"
  ]
  setnames(
    ref_labor,
    c("sum_demo_emp", "sum_demo_comp_pv_h", "sum_demo_comp_pv_l"),
    c("ref_total_emp", "ref_total_comp_pv_h", "ref_total_comp_pv_l")
  )
  ref_labor <- ref_labor[, .(
    demo_cat,
    demo_group,
    title,
    product_scenario,
    ref_total_emp,
    ref_total_comp_pv_h,
    ref_total_comp_pv_l
  )]

  ## add values to labor
  labor_df <- merge(
    labor_df,
    ref_labor,
    by = c("demo_cat", "demo_group", "title", "product_scenario")
  )

  ## calculate difference
  labor_df[, forgone_wages_h := (sum_demo_comp_pv_h - ref_total_comp_pv_h)]
  labor_df[, forgone_wages_l := (sum_demo_comp_pv_l - ref_total_comp_pv_l)]

  ## merge with health and ghg
  health_labor_ghg_df <- merge(
    health_ghg_df,
    labor_df[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      demo_cat,
      demo_group,
      title,
      sum_demo_comp_pv_h,
      sum_demo_comp_pv_l,
      ref_total_comp_pv_h,
      ref_total_comp_pv_l,
      forgone_wages_h,
      forgone_wages_l
    )],
    by = c(
      "demand_scenario",
      "refining_scenario",
      "demo_cat",
      "demo_group",
      "title"
    ),
    all.x = T
  )

  ## add ghg perc reduction
  health_labor_ghg_df <- merge(
    health_labor_ghg_df,
    perc_diff_df,
    by = c("demand_scenario", "refining_scenario"),
    all.x = T
  )

  ## prepare to plot
  plot_df <- health_labor_ghg_df[, .(
    scen_id,
    demand_scenario,
    refining_scenario,
    product_scenario,
    demo_cat,
    demo_group,
    title,
    sum_cost_pv,
    sum_cost_2019_pv,
    forgone_wages_h,
    forgone_wages_l,
    avoided_ghg,
    perc_diff
  )]

  setnames(plot_df, "perc_diff", "ghg_perc_diff")

  ## pivot longer
  plot_df <- plot_df %>%
    select(
      scen_id:title,
      ghg_perc_diff,
      sum_cost_pv,
      sum_cost_2019_pv,
      forgone_wages_h,
      forgone_wages_l
    ) %>%
    pivot_longer(
      sum_cost_pv:forgone_wages_l,
      names_to = "metric",
      values_to = "value"
    )

  ## add column for vsl
  plot_df_health <- plot_df %>%
    filter(metric %in% c("sum_cost_pv", "sum_cost_2019_pv")) |>
    mutate(
      segment = "health",
      unit_desc = ifelse(
        metric == "sum_cost_2019_pv",
        "USD (2019 VSL)",
        "USD (annual VSL)"
      ),
      metric_desc = "avoided_health_cost",
      product_scenario = NA
    ) |>
    distinct()

  plot_df_labor <- plot_df %>%
    filter(metric %in% c("forgone_wages_h", "forgone_wages_l")) %>%
    mutate(
      segment = "labor",
      unit_desc = "USD",
      metric_desc = "forgone_wages"
    )

  plot_df_long <- rbind(plot_df_health, plot_df_labor)

  plot_df_long <- plot_df_long %>%
    mutate(
      seg_title = ifelse(
        segment == "health",
        "Health: avoided mortality",
        "Labor: forgone wages"
      )
    )

  ## rename
  setDT(plot_df_long)
  plot_df_long[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # plot_df_long[, scenario := gsub('BAU', 'Reference', scenario)]
  plot_df_long[, scenario := gsub("LC1.", "Low ", scenario)]
  # plot_df_long[, short_scen := gsub('BAU', 'Reference', short_scen)]
  # plot_df_long[, short_scen := gsub('Low C.', 'Low carbon', short_scen)]

  ## change historic to historical
  plot_df_long[, scen_id := str_replace(scen_id, "historic", "historical")]
  plot_df_long[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  plot_df_long[, scenario := str_replace(scenario, "historic", "historical")]

  ## refactor
  plot_df_long$scenario <- factor(
    plot_df_long$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  ## titles for plotting
  plot_df_long[,
    demand_title := ifelse(demand_scenario == "BAU", "BAU demand", "Low demand")
  ]
  plot_df_long[,
    scen_title := paste0(demand_title, "\n", str_to_sentence(refining_scenario))
  ]

  plot_df_long$scen_title <- factor(
    plot_df_long$scen_title,
    levels = c(
      "BAU demand\nHistorical production",
      "BAU demand\nHistorical exports",
      "BAU demand\nLow exports",
      "Low demand\nHistorical exports",
      "Low demand\nLow exports",
      "Low demand\nHistorical production"
    )
  )

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_long,
    folder_path = NULL,
    filename = "state_disaggregated_npv_fig_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health-and-labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_disaggregated_npv_fig_inputs.csv")

  return(plot_df_long)
}


plot_hl_levels <- function(main_path, save_path, demographic_npv_df) {
  plot_df_long <- copy(demographic_npv_df)

  ## create the figure ---------------------------------------------
  ## ---------------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  ## add column for defining shapes
  plot_df_long[, demo_grp_metric := paste0(demo_group, "_", metric)]

  ## health fig - race
  health_level_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Race",
          unit_desc == "USD (2019 VSL)",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Asian", "white", "Hispanic")
          )
        ),
      aes(x = scen_title, y = value / 1e9, color = title),
      size = 3,
      alpha = 0.8
    ) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    # ylim(0, 12) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - race
  labor_level_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "Race",
          segment == "labor",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Asian", "white", "Hispanic")
          )
        ),
      aes(x = scen_title, y = value / 1e9, color = title, shape = metric),
      size = 3,
      alpha = 0.8
    ) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    scale_shape_manual(
      name = "",
      values = race_shape_ptc,
      labels = high_low_labs
    ) +
    # ylim(-25, 0) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig race 2020 product prices
  labor_level_fig_a_2020ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "Race",
          segment == "labor",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Asian", "white", "Hispanic")
          )
        ),
      aes(x = scen_title, y = value / 1e9, color = title, shape = metric),
      size = 3,
      alpha = 0.8
    ) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    scale_shape_manual(
      name = "",
      values = race_shape_ptc,
      labels = high_low_labs
    ) +
    # ylim(-25, 0) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## health fig - poverty
  health_level_fig_b <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Poverty",
          unit_desc == "USD (2019 VSL)"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = value / 1e9, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = poverty_ptc_h) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    # ylim(0, 20) +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - poverty
  labor_level_fig_b <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "Poverty",
          segment == "labor"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = value / 1e9, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    scale_shape_manual(
      values = poverty_ptc_l,
      labels = poverty_hl_labs
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    # ylim(-50, 0) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - poverty product prices 2020
  labor_level_fig_b_2020ppx <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "Poverty",
          segment == "labor"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = value / 1e9, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    scale_shape_manual(
      values = poverty_ptc_l,
      labels = poverty_hl_labs
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    # ylim(-50, 0) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## health fig - DAC
  health_level_fig_c <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "DAC",
          unit_desc == "USD (2019 VSL)"
        ),
      aes(x = scen_title, y = value / 1e9, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = dac_ptc) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    # ylim(0, 15) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - DAC
  labor_level_fig_c <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "DAC",
          segment == "labor"
        ),
      aes(x = scen_title, y = value / 1e9, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = dac_hl_ptc,
      labels = dac_hl_labs
    ) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    # ylim(-40, 0) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - DAC
  labor_level_fig_c_2020ppx <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "DAC",
          segment == "labor"
        ),
      aes(x = scen_title, y = value / 1e9, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = dac_hl_ptc,
      labels = dac_hl_labs
    ) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV (USD billion)",
      x = NULL,
      color = NULL
    ) +
    # ylim(-40, 0) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## combine figure
  ## ---------------------------------

  fig_text_size <- 12

  ## health
  health_column_fig_nl <- plot_grid(
    health_level_fig_c +
      theme(
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        strip.text.x = element_text(size = fig_text_size),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size)
      ),
    health_level_fig_b +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    health_level_fig_a +
      theme(
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none"
      ),
    align = "vh",
    labels = c("A", "B", "C"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## labor
  labor_column_fig_nl <- plot_grid(
    labor_level_fig_c +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = fig_text_size),
        axis.text.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_b +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_a +
      labs(y = NULL) +
      theme(
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none"
      ),
    align = "vh",
    labels = c("D", "E", "F"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## 2020 product prices
  labor_column_fig_nl_2020ppx <- plot_grid(
    labor_level_fig_c_2020ppx +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = fig_text_size),
        axis.text.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_b_2020ppx +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_a_2020ppx +
      labs(y = NULL) +
      theme(
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none"
      ),
    align = "vh",
    labels = c("D", "E", "F"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## 2020 product prices
  hl_pc_plot_grid_nl_2020ppx <- plot_grid(
    health_column_fig_nl,
    labor_column_fig_nl_2020ppx,
    align = "h",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 2,
    rel_widths = c(1, 1)
    # rel_widths = c(1, 1, 1)
  )

  simple_ggsave_repo(
    plot = hl_pc_plot_grid_nl_2020ppx,
    folder_path = NULL,
    filename = "demographic_npv_fig_2020ppx",
    width = 11,
    height = 12,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "health-labor-figures",
    extra_subfolder = "health-labor-figures"
  )

  ## original -- all together now
  hl_pc_plot_grid_nl <- plot_grid(
    health_column_fig_nl,
    labor_column_fig_nl,
    align = "h",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 2,
    rel_widths = c(1, 1)
    # rel_widths = c(1, 1, 1)
  )

  return(hl_pc_plot_grid_nl)
}

plot_hl_levels_pc <- function(
  demographic_npv_df,
  refining_mortality,
  pop_ratios,
  main_path,
  save_path
) {
  ## copy npv results
  plot_df_long <- copy(demographic_npv_df)

  ## add column for defining shapes
  plot_df_long[, demo_grp_metric := paste0(demo_group, "_", metric)]

  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()

  pop_2020[, demo_pop := pop * pct]

  ## summarize by demographic group
  pop_2020 <- pop_2020[,
    .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]

  ## merge population back with results
  plot_df_long <- merge(
    plot_df_long,
    pop_2020,
    by = c("demo_group", "demo_cat"),
    all.x = T
  )

  ## calculate per capita
  plot_df_long[, value := value / pop_2020]

  ## save figure inputs
  simple_fwrite_repo(
    plot_df_long,
    folder_path = NULL,
    filename = "state_disaggregated_npv_pc_fig_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health-and-labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_disaggregated_npv_pc_fig_inputs.csv")

  ## create the figure ---------------------------------------------
  ## ---------------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  ## health fig - race
  health_level_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Race",
          unit_desc == "USD (2019 VSL)",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Hispanic", "Asian", "white")
          )
        ),
      aes(x = scen_title, y = value, color = title),
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      y = "NPV per capita (USD)",
      x = NULL,
      color = NULL
    ) +
    scale_y_continuous(label = comma, limits = c(0, 2000)) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      strip.text = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - race
  labor_level_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "Race",
          segment == "labor",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Hispanic", "Asian", "white")
          )
        ),
      aes(x = scen_title, y = value, color = title, shape = metric),
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    scale_shape_manual(
      name = "",
      values = race_shape_ptc,
      labels = high_low_labs
    ) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    # scale_y_continuous(label = comma, limits = c(-1000, 0)) +
    scale_y_continuous(
      limits = c(-1000, 0),
      breaks = seq(-1000, 0, by = 500)
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      strip.text = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## 2020 product prices
  ## labor fig - race
  labor_level_fig_a_2020ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "Race",
          segment == "labor",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Hispanic", "Asian", "white")
          )
        ),
      aes(x = scen_title, y = value, color = title, shape = metric),
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    scale_shape_manual(
      name = "",
      values = race_shape_ptc,
      labels = high_low_labs
    ) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    scale_y_continuous(
      limits = c(-1000, 0),
      breaks = seq(-1000, 0, by = 500)
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      strip.text = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # ## legend
  # legend_figa <- labor_level_fig_a + theme(legend.position = "bottom")
  #
  # legend_a <- get_legend(
  #   legend_figa +
  #     theme(legend.text = element_text(size = 12)) +
  #     guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)))

  # ## save version for presentation
  # hl_plot_grid_a_pres <- plot_grid(
  #   health_level_fig_a +
  #     theme(axis.title.y = element_text(size = 12),
  #           axis.text.x = element_text(size = 9),
  #           axis.text.y = element_text(size = 12),
  #           strip.text =  element_text(size = 12),
  #           plot.margin = unit(c(0, 0, 0.25, 0.1), "cm")),
  #   labor_level_fig_a + labs(y = NULL) +
  #     theme(axis.title.y = element_text(size = 12),
  #           axis.text.x = element_text(size = 9),
  #           axis.text.y = element_text(size = 12),
  #           strip.text =  element_text(size = 12),
  #           plot.margin = unit(c(0, 0, 0.25, 0.1), "cm")),
  #   align = 'vh',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   rel_widths = c(1, 1)
  # )
  #
  # ## add legend
  # hl_plot_grid_a_pres <- plot_grid(
  #   hl_plot_grid_a_pres,
  #   legend_a,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  # )
  #
  #
  #
  # ggsave(plot = hl_plot_grid_a_pres,
  #        filename = paste0(main_path, "outputs/academic-out/refining/figures/2024-08-update/presentation-figs/fig5-race.jpeg"),
  #        device = "jpeg",
  #        width = 9,
  #        height = 4,
  #        units= "in",
  #        dpi = 300)
  #

  ## health fig - poverty
  health_level_fig_b <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Poverty",
          unit_desc == "USD (2019 VSL)"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = value, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = poverty_ptc_h) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV per capita (USD)",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    scale_y_continuous(label = comma, limits = c(0, 2000)) +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - poverty
  labor_level_fig_b <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "Poverty",
          segment == "labor"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = value, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = poverty_ptc_l,
      labels = poverty_hl_labs
    ) +
    facet_wrap(~seg_title) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    scale_y_continuous(
      limits = c(-1000, 0),
      breaks = seq(-1000, 0, by = 500)
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## 2020 product prices
  ## labor fig - poverty
  labor_level_fig_b_2020ppx <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "Poverty",
          segment == "labor"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = value, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = poverty_ptc_l,
      labels = poverty_hl_labs
    ) +
    facet_wrap(~seg_title) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    scale_y_continuous(
      limits = c(-1000, 0),
      breaks = seq(-1000, 0, by = 500)
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figb <- labor_level_fig_b +
  #   theme(legend.position = "bottom")
  #
  # legend_b <- get_legend(
  #   legend_figb +
  #     theme(legend.text = element_text(size = 12)))
  #

  # ## save version for presentation
  # hl_plot_grid_b_pres <- plot_grid(
  #   health_level_fig_b +
  #     theme(axis.title.y = element_text(size = 12),
  #           axis.text.x = element_text(size = 9),
  #           axis.text.y = element_text(size = 12),
  #           strip.text =  element_text(size = 12),
  #           plot.margin = unit(c(0, 0, 0.25, 0.1), "cm")),
  #   labor_level_fig_b + labs(y = NULL) +
  #     theme(axis.title.y = element_text(size = 12),
  #           axis.text.x = element_text(size = 9),
  #           axis.text.y = element_text(size = 12),
  #           strip.text =  element_text(size = 12),
  #           plot.margin = unit(c(0, 0, 0.25, 0.1), "cm")),
  #   align = 'vh',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   rel_widths = c(1, 1)
  # )
  #
  # ## add legend
  # hl_plot_grid_b_pres <- plot_grid(
  #   hl_plot_grid_b_pres,
  #   legend_b,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  # )
  #
  # ggsave(plot = hl_plot_grid_b_pres,
  #        filename = paste0(main_path, "outputs/academic-out/refining/figures/2024-08-update/presentation-figs/fig5-income.jpeg"),
  #        device = "jpeg",
  #        width = 9,
  #        height = 4,
  #        units= "in",
  #        dpi = 300)

  ## health fig - DAC
  health_level_fig_c <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "DAC",
          unit_desc == "USD (2019 VSL)"
        ),
      aes(x = scen_title, y = value, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = dac_ptc) +
    facet_wrap(~seg_title) +
    labs(
      y = "NPV per capita (USD)",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    scale_y_continuous(
      limits = c(0, 2000)
    ) +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - DAC
  labor_level_fig_c <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "DAC",
          segment == "labor"
        ),
      aes(x = scen_title, y = value, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = dac_hl_ptc,
      labels = dac_hl_labs
    ) +
    facet_wrap(~seg_title) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    scale_y_continuous(
      limits = c(-1000, 0),
      breaks = seq(-1000, 0, by = 500)
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - DAC
  labor_level_fig_c_2020ppx <- ggplot() +
    geom_point(
      data = plot_df_long %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "DAC",
          segment == "labor"
        ),
      aes(x = scen_title, y = value, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = dac_hl_ptc,
      labels = dac_hl_labs
    ) +
    facet_wrap(~seg_title) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    scale_y_continuous(
      limits = c(-1000, 0),
      breaks = seq(-1000, 0, by = 500)
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  # legend_figc <- labor_level_fig_c + theme(legend.position = "bottom")
  #
  # legend_c <- get_legend(
  #   legend_figc +
  #     theme(legend.text = element_text(size = 12)))

  # ## save version for presentation
  # hl_plot_grid_c_pres <- plot_grid(
  #   health_level_fig_c +
  #     theme(axis.title.y = element_text(size = 12),
  #           axis.text.x = element_text(size = 9),
  #           axis.text.y = element_text(size = 12),
  #           strip.text =  element_text(size = 12),
  #           plot.margin = unit(c(0, 0, 0.25, 0.1), "cm")),
  #   labor_level_fig_c + labs(y = NULL) +
  #     theme(axis.title.y = element_text(size = 12),
  #           axis.text.x = element_text(size = 9),
  #           axis.text.y = element_text(size = 12),
  #           strip.text =  element_text(size = 12),
  #           plot.margin = unit(c(0, 0, 0.25, 0.1), "cm")),
  #   align = 'vh',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   rel_widths = c(1, 1)
  # )
  #
  # ## add legend
  # hl_plot_grid_c_pres <- plot_grid(
  #   hl_plot_grid_c_pres,
  #   legend_c,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  # )
  #
  #
  # ggsave(plot = hl_plot_grid_c_pres,
  #        filename = paste0(main_path, "outputs/academic-out/refining/figures/2024-08-update/presentation-figs/fig5-dac.jpeg"),
  #        device = "jpeg",
  #        width = 9,
  #        height = 4,
  #        units= "in",
  #        dpi = 300)

  ## combine figure
  ## ---------------------------------

  fig_text_size <- 12

  ## health
  health_column_fig_nl <- plot_grid(
    health_level_fig_c +
      theme(
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        strip.text.x = element_text(size = fig_text_size),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size)
      ),
    health_level_fig_b +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    health_level_fig_a +
      theme(
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none"
      ),
    align = "vh",
    labels = c("A", "B", "C"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## labor
  labor_column_fig_nl <- plot_grid(
    labor_level_fig_c +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = fig_text_size),
        axis.text.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_b +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_a +
      labs(y = NULL) +
      theme(
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none"
      ),
    align = "vh",
    labels = c("D", "E", "F"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## 2020 product prices
  labor_column_fig_nl_2020ppx <- plot_grid(
    labor_level_fig_c_2020ppx +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = fig_text_size),
        axis.text.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_b_2020ppx +
      labs(y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    labor_level_fig_a_2020ppx +
      labs(y = NULL) +
      theme(
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none"
      ),
    align = "vh",
    labels = c("D", "E", "F"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## all together now
  hl_pc_plot_grid_nl_2020ppx <- plot_grid(
    health_column_fig_nl,
    labor_column_fig_nl_2020ppx,
    align = "h",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 2,
    rel_widths = c(1, 1)
    # rel_widths = c(1, 1, 1)
  )

  simple_ggsave_repo(
    plot = hl_pc_plot_grid_nl_2020ppx,
    folder_path = NULL,
    filename = "demographic_npv_pc_fig_2020ppx",
    width = 11,
    height = 12,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "figure-5"
  )

  ## all together now
  hl_pc_plot_grid_nl <- plot_grid(
    health_column_fig_nl,
    labor_column_fig_nl,
    align = "h",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 2,
    rel_widths = c(1, 1)
    # rel_widths = c(1, 1, 1)
  )

  hl_pc_plot_grid_nl

  # ## race
  # hl_plot_grid_a <- plot_grid(
  #   health_level_fig_a + theme(strip.text.x = element_blank()),
  #   labor_level_fig_a + labs(y = NULL) + theme(strip.text.x = element_blank()),
  #   align = 'vh',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   rel_widths = c(1, 1)
  # )
  #
  # ## add race legend
  # hl_plot_grid_a <- plot_grid(
  #   hl_plot_grid_a,
  #   legend_a,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  # )
  #
  # ## poverty
  # hl_plot_grid_b <- plot_grid(
  #   health_level_fig_b + theme(axis.text.x = element_blank(),
  #                              strip.text.x = element_blank()),
  #   labor_level_fig_b + labs(y = NULL) + theme(axis.text.x = element_blank(),
  #                                              strip.text.x = element_blank()),
  #   align = 'vh',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   rel_widths = c(1, 1)
  # )
  #
  # ## add poverty legend
  # hl_plot_grid_b <- plot_grid(
  #   hl_plot_grid_b,
  #   legend_b,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  # )
  #
  # ## DAC
  # hl_plot_grid_c <- plot_grid(
  #   health_level_fig_c + theme(axis.text.x = element_blank()),
  #   labor_level_fig_c + labs(y = NULL) + theme(axis.text.x = element_blank()),
  #   align = 'vh',
  #   # labels = c("A", "B", "C", "D", "E", "F"),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   nrow = 1,
  #   rel_widths = c(1, 1)
  # )
  #
  # ## add DAC legend
  # hl_plot_grid_c <- plot_grid(
  #   hl_plot_grid_c,
  #   legend_c,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05)
  # )
  #
  #
  # ## all together now
  # hl_plot_grid_pc <- plot_grid(
  #   hl_plot_grid_c,
  #   NULL,
  #   hl_plot_grid_b,
  #   NULL,
  #   hl_plot_grid_a,
  #   align = "v",
  #   # labels = c("(A)", "(B)", "(C)", ""),
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   # hjust = -1,
  #   ncol = 1,
  #   rel_heights = c(1, 0.1, 1, 0.1, 1)
  #   # rel_widths = c(1, 1, 1)
  # )
  #

  return(hl_pc_plot_grid_nl)
}


## npv shares
## ----------------------------------------------------------------------------

plot_hl_shares <- function(
  main_path,
  save_path,
  demographic_npv_df,
  state_pop_ratios
) {
  fig_csv_dir <- file.path(save_path, "results", "figures", "figures-si")
  legends_dir <- file.path(save_path, "results", "figures", "extra")
  ensure_dir(fig_csv_dir)
  ensure_dir(legends_dir)

  plot_df_long <- copy(demographic_npv_df)

  ## calculate shares
  plot_df_long[,
    total_value := sum(value),
    by = .(
      scen_id,
      demand_scenario,
      refining_scenario,
      product_scenario,
      demo_cat,
      metric,
      segment,
      unit_desc,
      metric_desc,
      seg_title,
      scenario,
      demand_title,
      scen_title
    )
  ]

  plot_df_long[, share := value / total_value]

  ## shares
  pct_df <- copy(state_pop_ratios)

  pct_df[, scen_title := "population"]

  ## create one df for plotting
  share_df <- plot_df_long[, .(
    scen_id,
    demand_scenario,
    refining_scenario,
    product_scenario,
    demo_cat,
    title,
    metric,
    unit_desc,
    segment,
    metric_desc,
    seg_title,
    scenario,
    demand_title,
    scen_title,
    share
  )]

  pop_share_df <- copy(share_df)
  pop_share_df[, `:=`(
    scen_id = "Population",
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
    share = NULL
  )]

  pop_share_df <- unique(pop_share_df)

  ## merge
  pop_share_df <- merge(
    pop_share_df,
    pct_df[, .(demo_cat, title, pct)],
    by = c("demo_cat", "title")
  )

  setnames(pop_share_df, "pct", "share")

  ## bind
  share_df <- rbind(share_df, pop_share_df)

  ## add column for defining shapes
  share_df[, demo_grp_metric := paste0(title, "_", metric)]

  ## save figure inputs
  simple_fwrite_repo(
    share_df,
    folder_path = NULL,
    filename = "state_disaggreated_npv_share_fig_inputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health-and-labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_disaggreated_npv_share_fig_inputs.csv")

  ## create the figure ---------------------------------------------
  ## ---------------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")
  bau_scen <- "BAU historical production"

  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  ## health fig - race
  health_share_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Race",
          unit_desc == "USD (2019 VSL)",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Hispanic", "Asian", "white")
          )
        ),
      aes(x = scen_title, y = share, color = title),
      size = 3,
      alpha = 0.8
    ) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    # ylim(0, 0.5) +
    labs(
      y = "NPV share",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor fig - race
  labor_share_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "Race",
          segment == "labor",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Hispanic", "Asian", "white")
          )
        ),
      aes(x = scen_title, y = share, color = title, shape = metric),
      size = 3,
      alpha = 0.8
    ) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    scale_shape_manual(
      name = "",
      values = race_shape_ptc,
      labels = high_low_labs
    ) +
    # ylim(0, 0.6) +
    labs(
      y = "NPV share",
      x = NULL,
      color = "with re-emp:"
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = "none")

  ## labor fig - race - 2020 product prices
  labor_share_fig_a_2020ppx <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "Race",
          segment == "labor",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Hispanic", "Asian", "white")
          )
        ),
      aes(x = scen_title, y = share, color = title, shape = metric),
      size = 3,
      alpha = 0.8
    ) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    scale_shape_manual(
      name = "",
      values = race_shape_ptc,
      labels = high_low_labs
    ) +
    # ylim(0, 0.6) +
    labs(
      y = "NPV share",
      x = NULL,
      color = "with re-emp:"
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = "none")
  # legend_fig_labor_h <- ggplot() +
  #   geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
  #   geom_point(
  #     data = share_df %>% filter(
  #       !scen_id %in% remove_scen,
  #       demo_cat == "Race",
  #       segment == "labor",
  #       title %in% fig_title_vec,
  #       metric == "forgone_wages_h"
  #     ) %>%
  #       mutate(title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))),
  #     aes(x = scen_title, y = share, color = title, shape = metric),
  #     size = 3, alpha = 0.8
  #   ) +
  #   facet_wrap(~seg_title) +
  #   scale_color_manual(
  #     name = "no re-emp:",
  #     values = race_col_pal
  #   ) +
  #   scale_shape_manual(
  #     name = "",
  #     values = race_shape_ptc,
  #     labels = high_low_labs
  #   ) +
  #   # ylim(0, 0.6) +
  #   labs(
  #     y = "NPV share",
  #     x = NULL,
  #     color = "no re-emp:"
  #   ) +
  #   theme_line +
  #   theme(
  #     legend.position = "bottom",
  #     legend.title = element_text(size = 8),
  #     axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
  #     plot.margin = unit(c(0, 0, 0, 0), "cm"),
  #     axis.ticks.length.y = unit(0.1, "cm"),
  #     axis.ticks.length.x = unit(0.1, "cm")
  #   ) +
  #   guides(
  #     color = guide_legend(override.aes = list(shape = 21)),
  #     shape = "none"
  #   )
  #
  #
  # legend_a_h <- get_legend(
  #   legend_fig_labor_h +
  #     theme(legend.text = element_text(size = 12))
  # )

  # ## add no-emp legend to labor fig
  # labor_share_fig_a2 <- plot_grid(
  #   labor_share_fig_a,
  #   legend_a_h,
  #   ncol = 1,
  #   rel_heights = c(0.95, 0.05),
  #   rel_widths = c(1, 1)
  # )

  ## state fig - race
  state_share_fig_a <- ggplot() +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Race",
          segment == "general",
          title %in% fig_title_vec
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Black", "Hispanic", "Asian", "white")
          )
        ),
      aes(x = scen_title, y = share, color = title),
      size = 3,
      alpha = 0.8
    ) +
    facet_wrap(~seg_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    # ylim(0, 0.5) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## health fig - poverty
  health_share_fig_b <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Poverty",
          unit_desc == "USD (2019 VSL)"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = share, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    scale_shape_manual(values = poverty_ptc_h) +
    labs(
      y = "NPV share",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    # ylim(0, 0.9) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE))

  labor_share_fig_b <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "Poverty",
          segment == "labor"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          ),
          demo_grp_metric = factor(
            demo_grp_metric,
            levels = c(
              "Above poverty line_forgone_wages_l",
              "Below poverty line_forgone_wages_l",
              "Above poverty line_forgone_wages_h",
              "Below poverty line_forgone_wages_h"
            )
          )
        ),
      aes(x = scen_title, y = share, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    scale_shape_manual(
      values = poverty_pt_share_l,
      labels = poverty_hl_share_labs
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    # ylim(0, 0.9) +
    labs(
      y = "NPV share",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE))

  ## 2020 ppx
  labor_share_fig_b_2020ppx <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "Poverty",
          segment == "labor"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          ),
          demo_grp_metric = factor(
            demo_grp_metric,
            levels = c(
              "Above poverty line_forgone_wages_l",
              "Below poverty line_forgone_wages_l",
              "Above poverty line_forgone_wages_h",
              "Below poverty line_forgone_wages_h"
            )
          )
        ),
      aes(x = scen_title, y = share, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    scale_shape_manual(
      values = poverty_pt_share_l,
      labels = poverty_hl_share_labs
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    # ylim(0, 0.9) +
    labs(
      y = "NPV share",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE))

  ## state - poverty
  state_share_fig_b <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "Poverty",
          segment == "general"
        ) %>%
        mutate(
          title = factor(
            title,
            levels = c("Below poverty line", "Above poverty line")
          )
        ),
      aes(x = scen_title, y = share, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    scale_shape_manual(values = poverty_ptc_h) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_wrap(~seg_title) +
    # ylim(0, 0.9) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## health fig - DAC
  health_share_fig_c <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "DAC",
          unit_desc == "USD (2019 VSL)"
        ),
      aes(x = scen_title, y = share, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = dac_ptc) +
    facet_wrap(~seg_title) +
    # ylim(0, 0.85) +
    labs(
      y = "NPV share",
      x = NULL,
      color = NULL
    ) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE))

  ## labor fig - DAC
  labor_share_fig_c <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario == "changing prices",
          demo_cat == "DAC",
          segment == "labor"
        ) %>%
        mutate(
          demo_grp_metric = factor(
            demo_grp_metric,
            levels = c(
              "DAC_forgone_wages_l",
              "Non-DAC_forgone_wages_l",
              "DAC_forgone_wages_h",
              "Non-DAC_forgone_wages_h"
            )
          )
        ),
      aes(x = scen_title, y = share, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = dac_hl_ptc_share,
      labels = dac_hl_labs_share
    ) +
    facet_wrap(~seg_title) +
    # ylim(0, 0.85) +
    labs(
      y = "NPV share",
      x = NULL,
      color = NULL
    ) +
    # ylim(-15, 0) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE))

  ## 2020 product prices
  labor_share_fig_c_2020ppx <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          product_scenario != "changing prices",
          demo_cat == "DAC",
          segment == "labor"
        ) %>%
        mutate(
          demo_grp_metric = factor(
            demo_grp_metric,
            levels = c(
              "DAC_forgone_wages_l",
              "Non-DAC_forgone_wages_l",
              "DAC_forgone_wages_h",
              "Non-DAC_forgone_wages_h"
            )
          )
        ),
      aes(x = scen_title, y = share, shape = demo_grp_metric),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(
      values = dac_hl_ptc_share,
      labels = dac_hl_labs_share
    ) +
    facet_wrap(~seg_title) +
    # ylim(0, 0.85) +
    labs(
      y = "NPV share",
      x = NULL,
      color = NULL
    ) +
    # ylim(-15, 0) +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    ) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE))

  ## general fig - DAC
  state_share_fig_c <- ggplot() +
    geom_point(
      data = share_df %>%
        filter(
          !scen_id %in% remove_scen,
          demo_cat == "DAC",
          segment == "general"
        ),
      aes(x = scen_title, y = share, shape = title),
      color = "black",
      size = 3,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    scale_shape_manual(values = dac_ptc) +
    facet_wrap(~seg_title) +
    # ylim(0, 0.85) +
    labs(
      y = " ",
      x = NULL,
      color = NULL
    ) +
    # ylim(-15, 0) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      # axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## save legends
  ## -----------------------------------------------------------------------

  health_dac_legend <- get_legend(
    health_share_fig_c +
      theme(legend.text = element_text(size = 12))
  )

  health_poverty_legend <- get_legend(
    health_share_fig_b +
      theme(legend.text = element_text(size = 12))
  )

  health_race_legend <- get_legend(
    health_share_fig_a +
      theme(legend.text = element_text(size = 12))
  )

  ## save legends
  ggsave(
    plot = health_dac_legend,
    device = "pdf",
    filename = "health_dac_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

  ## save legends
  ggsave(
    plot = health_poverty_legend,
    device = "pdf",
    filename = "health_poverty_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

  ## save legends
  ggsave(
    plot = health_race_legend,
    device = "pdf",
    filename = "health_race_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

  labor_dac_legend <- get_legend(
    labor_share_fig_c +
      theme(legend.text = element_text(size = 12))
  )

  labor_poverty_legend <- get_legend(
    labor_share_fig_b +
      theme(legend.text = element_text(size = 12))
  )

  labor_race_legend <- get_legend(
    labor_share_fig_a +
      theme(legend.text = element_text(size = 12))
  )

  ## save legends
  ggsave(
    plot = labor_dac_legend,
    device = "pdf",
    filename = "labor_dac_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

  ## save legends
  ggsave(
    plot = labor_poverty_legend,
    device = "pdf",
    filename = "labor_poverty_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

  ## save legends
  ggsave(
    plot = labor_race_legend,
    device = "pdf",
    filename = "labor_race_legend.pdf",
    path = legends_dir,
    dpi = 600
  )

  # ## save legends
  # ggsave(
  #   plot = legend_a_h,
  #   device = "pdf",
  #   filename = "labor_race_legend_no_re-emp.pdf",
  #   path = file.path(main_path, "outputs/academic-out/refining/figures/2025-update/legends/"),
  #   dpi = 600
  # )
  #

  ## combine figure
  ## ---------------------------------

  fig_text_size <- 12

  ## health
  health_column_fig <- plot_grid(
    health_share_fig_c +
      ylim(0, 1) +
      theme(
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        strip.text.x = element_text(size = fig_text_size),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size)
      ),
    health_share_fig_b +
      ylim(0, 1) +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1)
      ),
    health_share_fig_a +
      ylim(0, 1) +
      theme(
        strip.text.x = element_blank(),
        axis.text.y = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none"
      ),
    align = "vh",
    labels = c("A", "B", "C"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## state
  state_column_fig <- plot_grid(
    state_share_fig_c +
      ylim(0, 1) +
      theme(
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        strip.text.x = element_text(size = fig_text_size),
        axis.title.y = element_text(size = fig_text_size),
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    state_share_fig_b +
      ylim(0, 1) +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    state_share_fig_a +
      ylim(0, 1) +
      theme(
        strip.text.x = element_blank(),
        axis.title.y = element_text(size = fig_text_size),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none",
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    align = "vh",
    labels = c("D", "E", "F"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## labor
  labor_column_fig <- plot_grid(
    labor_share_fig_c +
      ylim(0, 1) +
      labs(y = " ") +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    labor_share_fig_b +
      ylim(0, 1) +
      labs(y = " ") +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    labor_share_fig_a +
      ylim(0, 1) +
      labs(y = " ") +
      theme(
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none",
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    align = "vh",
    labels = c("G", "H", "I"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## labor 2020 product prices
  labor_column_fig_2020ppx <- plot_grid(
    labor_share_fig_c_2020ppx +
      ylim(0, 1) +
      labs(y = " ") +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = fig_text_size),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    labor_share_fig_b_2020ppx +
      ylim(0, 1) +
      labs(y = " ") +
      theme(
        axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 20, 1),
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    labor_share_fig_a_2020ppx +
      ylim(0, 1) +
      labs(y = " ") +
      theme(
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = fig_text_size),
        legend.position = "none",
        # axis.text.y = element_text(size = fig_text_size),
        axis.text.y = element_blank()
      ),
    align = "vh",
    labels = c("G", "H", "I"),
    # # labels = 'AUTO',
    label_size = 10,
    hjust = -1,
    nrow = 3
    # rel_widths = c(1, 0.25, 1),
    # rel_heights = c(1, 0.1, 1, 0.1, 1)
  )

  ## plot 2020 product prices and save
  hl_pc_plot_grid_nl_2020ppx <- plot_grid(
    health_column_fig,
    state_column_fig,
    labor_column_fig_2020ppx,
    align = "h",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 3,
    rel_widths = c(1, 0.3, 0.9)
    # rel_widths = c(1, 1, 1)
  )

  simple_ggsave_repo(
    plot = hl_pc_plot_grid_nl_2020ppx,
    folder_path = NULL,
    filename = "demographic_npv_shares_fig_2020ppx",
    width = 12,
    height = 12,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "figures-si"
  )

  ## original
  hl_pc_plot_grid_nl <- plot_grid(
    health_column_fig,
    state_column_fig,
    labor_column_fig,
    align = "h",
    # labels = c("(A)", "(B)", "(C)", ""),
    # # labels = 'AUTO',
    # label_size = 10,
    # hjust = -1,
    ncol = 3,
    rel_widths = c(1, 0.3, 0.9)
    # rel_widths = c(1, 1, 1)
  )

  return(hl_pc_plot_grid_nl)
}

create_health_labor_table <- function(
  main_path,
  save_path,
  demographic_npv_df,
  ref_labor_demog,
  pop_ratios,
  refining_mortality
) {
  ## create table of total health benefit (NPV), labor loss (NPV), change in job years,
  ## and avoided premature mortality (scenario x demographic group, state)

  ## NPV values
  npv_out <- demographic_npv_df[, .(
    demand_scenario,
    refining_scenario,
    product_scenario,
    demo_cat,
    demo_group,
    title,
    segment,
    metric,
    metric_desc,
    unit_desc,
    value
  )]

  ## fte-job-years
  emp_out <- ref_labor_demog %>%
    # select(-sum_demo_comp_pv_h, -sum_demo_comp_pv_l) %>%
    select(demand_scenario:title, sum_demo_emp) %>%
    rename(value = sum_demo_emp) %>%
    group_by(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      demo_cat,
      demo_group,
      title
    ) %>%
    summarise(value = sum(value)) %>%
    # summarise(value = sum(value),
    #           value_revised = sum(value_revised)) %>%
    ungroup() %>%
    mutate(
      segment = "labor",
      metric = "fte_job_years",
      metric_desc = "job_loss",
      unit_desc = "fte-jobs"
    )

  ## bau scen
  bau_emp_out <- emp_out %>%
    filter(
      demand_scenario == "BAU",
      refining_scenario == "historic production"
    ) %>%
    select(
      product_scenario,
      oil_price_scenario,
      demo_cat,
      demo_group,
      title,
      value
    ) %>%
    rename(bau_value = value)
  # ,
  #          bau_revised = value_revised)

  ## difference
  emp_out <- merge(
    emp_out,
    bau_emp_out,
    by = c(
      "product_scenario",
      "oil_price_scenario",
      "demo_cat",
      "demo_group",
      "title"
    ),
    all.x = T
  )

  emp_out <- emp_out %>%
    mutate(
      delta_value = value - bau_value
      # ,
      # delta_revised_value = value_revised - bau_revised
    ) %>%
    select(
      product_scenario:refining_scenario,
      segment,
      metric,
      metric_desc,
      unit_desc,
      delta_value
    ) %>%
    rename(value = delta_value) %>%
    mutate(
      refining_scenario = str_replace(
        refining_scenario,
        "historic",
        "historical"
      )
    ) %>%
    filter(oil_price_scenario == "reference case") %>%
    select(-oil_price_scenario)

  ## avoided mortality
  avoid_m_out <- copy(refining_mortality) %>% as.data.table()

  ## select columns
  avoid_m_out <- avoid_m_out %>%
    select(
      census_tract,
      demand_scenario,
      refining_scenario,
      year,
      mortality_delta
    )

  ## merge with pop ratios
  avoid_m_out <- merge(
    avoid_m_out,
    pop_ratios,
    by = "census_tract",
    all.x = TRUE,
    allow.cartesian = TRUE
  )

  setDT(avoid_m_out)

  ## calc value by demographic group
  avoid_m_out[, value := mortality_delta * pct]

  avoid_m_out_total <- avoid_m_out[,
    .(value = sum(value)),
    by = .(
      demand_scenario,
      refining_scenario,
      demo_cat,
      demo_group,
      title
    )
  ]

  avoid_m_out_total <- avoid_m_out_total %>%
    mutate(
      refining_scenario = str_replace(
        refining_scenario,
        "historic",
        "historical"
      ),
      segment = "health",
      metric = "avoided_mortality",
      metric_desc = "avoided_mortality",
      unit_desc = "persons"
    ) %>%
    select(
      demand_scenario,
      refining_scenario,
      demo_cat,
      demo_group,
      title,
      segment,
      metric,
      metric_desc,
      unit_desc,
      value
    )

  ## add NA
  avoid_m_out_total[, product_scenario := NA]

  ## bind
  result_output <- rbind(npv_out, emp_out, avoid_m_out_total)

  ## save figure inputs
  simple_fwrite_repo(
    result_output,
    folder_path = NULL,
    filename = "state_health_labor_ouputs.csv",
    save_path = save_path,
    file_type = "table",
    figure_number = NULL,
    extra_subfolder = "health-and-labor"
  )
  # Old path, now removed: file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/", "state_health_labor_ouputs.csv")

  return(result_output)
}


fig4_hl <- function(
  main_path,
  save_path,
  health_grp,
  ref_labor_demog_yr,
  refining_mortality,
  pop_ratios
) {
  gaps_df <- copy(health_grp)

  ## change scenario names, factor
  gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## refactor
  gaps_df[, scenario_title := scenario]
  gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## calculate gaps (BAU - scenario)
  bau_gaps_df <- gaps_df[scen_id == "BAU historic production"]
  bau_gaps_df <- bau_gaps_df[, c(
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "mortality_level_dem"
  )]
  setnames(bau_gaps_df, "mortality_level_dem", "bau_mortality_level_dem")

  gaps_df <- merge(
    gaps_df,
    bau_gaps_df,
    by = c("year", "demo_cat", "demo_group", "title"),
    all.x = T
  )

  gaps_df[, gap := mortality_level_dem - bau_mortality_level_dem]

  ## change historic to historical
  gaps_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## refactor
  gaps_df$scenario <- factor(
    gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  gaps_df$scenario_title <- factor(
    gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## make figures
  ## ---------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")

  ## figure a
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  health_gap_fig_a <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-0.31, 0)) +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## figure b
  health_gap_fig_b <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = gap, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-0.31, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    scale_linetype_manual(values = dac_lty) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      # strip.text.x = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## figure c
  health_gap_fig_c <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_linetype_manual(values = poverty_lty) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    # ylim(c(-0.31, 0)) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label(expression(paste("PM"[2.5], " (",mu,"g ", m^{-3},")", " per person, difference from reference")),
  #                                    size = 8, angle = 90)

  yaxis_lab <- ggdraw() +
    draw_label(
      "Avoided mortalities, difference from reference",
      size = 8,
      angle = 90
    )

  gaps_plot_grid_h <- plot_grid(
    health_gap_fig_b,
    health_gap_fig_c,
    health_gap_fig_a,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1),
    rel_heights = c(1.1, 0.9, 0.9)
  )

  gaps_plot_grid2 <- plot_grid(
    yaxis_lab,
    gaps_plot_grid_h,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  ## labor
  ## -------------------------------------------------------

  # ## calc 2020 pop by demographic
  # pop_2020 <- refining_mortality %>%
  #   filter(year == 2020) %>%
  #   select(census_tract, year, pop) %>%
  #   unique() %>%
  #   left_join(pop_ratios) %>%
  #   as.data.table()
  #
  # pop_2020[, demo_pop := pop * pct]
  #
  # ## summarize by demographic group
  # pop_2020 <- pop_2020[, .(pop_2020 = sum(demo_pop)),
  #                      by = .(demo_group, demo_cat)]
  #
  ## labor outputs
  l_gaps_df <- copy(ref_labor_demog_yr)

  ## change scenario names, factor
  l_gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  l_gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## add scenario title
  l_gaps_df[, scenario_title := scenario]
  l_gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## change historic to historical
  l_gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  l_gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  l_gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## scenarios for filtering
  remove_scen <- c(
    "Low demand - historical production",
    "BAU demand - historical production"
  )

  l_gaps_df$scenario <- factor(
    l_gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  l_gaps_df$scenario_title <- factor(
    l_gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## sum for state
  l_gaps_df <- l_gaps_df[,
    .(
      sum_demo_emp = sum(sum_demo_emp),
      sum_demo_emp_revised = sum(sum_demo_emp_revised)
    ),
    by = .(
      year,
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      scenario,
      scenario_title,
      demo_cat,
      demo_group,
      title
    )
  ]

  ## pivot longer
  l_gaps_df <- l_gaps_df |>
    pivot_longer(
      sum_demo_emp:sum_demo_emp_revised,
      names_to = "employment_scen",
      values_to = "emp_value"
    ) |>
    as.data.table()

  # ## merge with 2020 pop
  # l_gaps_df <- merge(l_gaps_df, pop_2020,
  #                    by = c("demo_cat", "demo_group"),
  #                    all.x = T)
  #
  # ## calculate per capita
  # l_gaps_df[, demo_emp_pc := sum_demo_emp / pop_2020]

  ## select columns
  l_gaps_df <- l_gaps_df[, .(
    year,
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    scenario,
    scenario_title,
    demo_cat,
    demo_group,
    title,
    employment_scen,
    emp_value
  )]

  # filter for oil px == reference case
  l_gaps_df <- l_gaps_df[oil_price_scenario == "reference case"]

  ## calculate gaps (BAU - scenario)
  l_bau_gaps_df <- l_gaps_df[scenario == "BAU demand - historical production"]
  l_bau_gaps_df <- l_bau_gaps_df[, c(
    "product_scenario",
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "employment_scen",
    "emp_value"
  )]
  setnames(l_bau_gaps_df, "emp_value", "bau_emp_value")
  # setnames(l_bau_gaps_df, "demo_emp_pc", "bau_demo_emp_pc")

  l_gaps_df <- merge(
    l_gaps_df,
    l_bau_gaps_df,
    by = c(
      "year",
      "demo_cat",
      "demo_group",
      "title",
      "product_scenario",
      "employment_scen"
    ),
    all.x = T
  )

  l_gaps_df[, gap_emp := emp_value - bau_emp_value]
  # l_gaps_df[, gap_emp_pc :=  demo_emp_pc - bau_demo_emp_pc]

  ## figure labor a -- changing product prices
  labor_gap_fig_a <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        ## choose with re-emp for main text
        employment_scen == "sum_demo_emp_revised",
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap_emp / 1000, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(-35, 0) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor 2020 prod prices -- MAIN TEXT
  labor_gap_fig_a_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## choose with re-emp for main text
        employment_scen == "sum_demo_emp_revised",
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap_emp / 1000, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(-35, 0) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor changing price, no-remp
  labor_gap_fig_a_2020ppx_no_reemp <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## choose without re-emp for SI
        employment_scen == "sum_demo_emp",
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap_emp / 1000, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(-35, 0) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## legend a
  legend_a <- labor_gap_fig_a +
    geom_line(linewidth = 1, alpha = 0.9) +
    theme(legend.position = "right")

  legend_a <- get_legend(
    legend_a +
      theme(legend.text = element_text(size = 6))
  )

  ## labor b
  labor_gap_fig_b <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario == "changing prices",
        ## choose with re-emp for main text
        employment_scen == "sum_demo_emp_revised",
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-35, 0)) +
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

  ## labor b -- 2020 prices, MAIN TEXT
  labor_gap_fig_b_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario != "changing prices",
        ## choose with re-emp for main text
        employment_scen == "sum_demo_emp_revised",
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-35, 0)) +
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

  ## labor b -- 2020 prices, no re-emp, SI
  labor_gap_fig_b_2020ppx_no_reemp <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario != "changing prices",
        ## choose without re-emp for SI
        employment_scen == "sum_demo_emp",
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-35, 0)) +
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

  ## legend b
  legend_b <- labor_gap_fig_b +
    geom_line(linewidth = 1, alpha = 0.9) +
    theme(legend.position = "right")

  legend_b <- get_legend(
    legend_b +
      theme(legend.text = element_text(size = 6))
  )

  ## labor c -- chaning prices
  labor_gap_fig_c <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        ## choose with re-emp for main text
        employment_scen == "sum_demo_emp_revised",
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(-35, 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor c - 2020 prices MAIN text
  labor_gap_fig_c_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## choose with re-emp for main text
        employment_scen == "sum_demo_emp_revised",
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(-35, 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor c - 2020 prices, no re-emp, SI
  labor_gap_fig_c_2020ppx_no_reemp <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## choose without re-emp for SI
        employment_scen == "sum_demo_emp",
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp / 1000, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(-35, 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## legend c
  legend_c <- labor_gap_fig_c +
    geom_line(linewidth = 1, alpha = 0.9) +
    theme(legend.position = "right")

  legend_c <- get_legend(
    legend_c +
      theme(legend.text = element_text(size = 6))
  )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label("Labor: FTE job-years, difference from reference", size = 8, angle = 90)
  yaxis_lab <- ggdraw() +
    draw_label(
      "Labor: FTE job-years, difference from reference (thousand)",
      size = 8,
      angle = 90
    )

  ## 2020 product prices, with re-emp, main text
  l_gaps_plot_grid_2020ppx <- plot_grid(
    labor_gap_fig_b_2020ppx + theme(legend.position = "none"),
    labor_gap_fig_c_2020ppx + theme(legend.position = "none"),
    labor_gap_fig_a_2020ppx + theme(legend.position = "none"),
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1),
    rel_heights = c(1.05, 0.9, 0.9)
  )

  l_gaps_plot_grid2_2020ppx <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  ## 2020 product prices, without re-emp, SI
  l_gaps_plot_grid_2020ppx_no_reemp <- plot_grid(
    labor_gap_fig_b_2020ppx_no_reemp + theme(legend.position = "none"),
    labor_gap_fig_c_2020ppx_no_reemp + theme(legend.position = "none"),
    labor_gap_fig_a_2020ppx_no_reemp + theme(legend.position = "none"),
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1),
    rel_heights = c(1.05, 0.9, 0.9)
  )

  l_gaps_plot_grid2_2020ppx_no_reemp <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid_2020ppx_no_reemp,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  l_gaps_plot_grid <- plot_grid(
    labor_gap_fig_b + theme(legend.position = "none"),
    labor_gap_fig_c + theme(legend.position = "none"),
    labor_gap_fig_a + theme(legend.position = "none"),
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1),
    rel_heights = c(1.05, 0.9, 0.9)
  )

  l_gaps_plot_grid2 <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  l_gaps_plot_grid2

  ## plot legends
  legends <- plot_grid(
    legend_b,
    legend_c,
    legend_a,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 3,
    ncol = 1,
    rel_widths = c(1, 1, 1),
    rel_heights = c(1, 1, 1)
  )

  ## plot side by side
  ## ----------------------------------------------

  ## MAIN TEXT
  health_labor_plot_2020ppx <- plot_grid(
    gaps_plot_grid2,
    l_gaps_plot_grid2_2020ppx,
    NULL,
    legends,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 4,
    rel_widths = c(1, 1, 0.05, 0.2),
    rel_heights = c(1, 1, 1, 1)
  )

  simple_ggsave_repo(
    plot = health_labor_plot_2020ppx,
    folder_path = NULL,
    filename = "health_labor_gaps_plot_2020ppx",
    width = 14,
    height = 6,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "health-labor-figures",
    extra_subfolder = "health-labor-figures"
  )

  ## SI -- no re-emp
  health_labor_plot_2020ppx_no_reemp <- plot_grid(
    gaps_plot_grid2,
    l_gaps_plot_grid2_2020ppx_no_reemp,
    NULL,
    legends,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 4,
    rel_widths = c(1, 1, 0.05, 0.2),
    rel_heights = c(1, 1, 1, 1)
  )

  simple_ggsave_repo(
    plot = health_labor_plot_2020ppx_no_reemp,
    folder_path = NULL,
    filename = "health_labor_gaps_pmil_plot_2020ppx_no_reemp",
    width = 14,
    height = 6,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "figure-4"
  )

  health_labor_plot <- plot_grid(
    gaps_plot_grid2,
    l_gaps_plot_grid2,
    NULL,
    legends,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 4,
    rel_widths = c(1, 1, 0.05, 0.2),
    rel_heights = c(1, 1, 1, 1)
  )

  return(health_labor_plot)
}


fig4_hl_pmil <- function(
  main_path,
  save_path,
  health_grp,
  ref_labor_demog_yr,
  refining_mortality,
  pop_ratios
) {
  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()

  pop_2020[, demo_pop := pop * pct]

  ## summarize by demographic group
  pop_2020 <- pop_2020[,
    .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]

  gaps_df <- copy(health_grp)

  ## change scenario names, factor
  gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## refactor
  gaps_df[, scenario_title := scenario]
  gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## calculate gaps (BAU - scenario)
  bau_gaps_df <- gaps_df[scen_id == "BAU historic production"]
  bau_gaps_df <- bau_gaps_df[, c(
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "mortality_level_dem"
  )]
  setnames(bau_gaps_df, "mortality_level_dem", "bau_mortality_level_dem")

  gaps_df <- merge(
    gaps_df,
    bau_gaps_df,
    by = c("year", "demo_cat", "demo_group", "title"),
    all.x = T
  )

  gaps_df[, gap := mortality_level_dem - bau_mortality_level_dem]

  ## convert to per million
  gaps_df <- merge(
    gaps_df,
    pop_2020,
    by = c("demo_group", "demo_cat"),
    all.x = T
  )

  ## calculate per capita
  gaps_df[, value := gap / pop_2020]
  gaps_df[, value_pmil := value * 1e6]

  ## change historic to historical
  gaps_df[, scen_id := str_replace(scen_id, "historic", "historical")]
  gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## refactor
  gaps_df$scenario <- factor(
    gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  gaps_df$scenario_title <- factor(
    gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  ## make figures
  ## ---------------------------------------------------------

  ## scenarios for filtering
  remove_scen <- c("LC1 historical production", "BAU historical production")

  ## figure a
  fig_title_vec <- c("Asian", "Black", "Hispanic", "white")

  fig_text_size <- 12

  health_gap_fig_a <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = value_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(c(-30, 0)) +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## figure b
  health_gap_fig_b <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "DAC"
      ),
    aes(x = year, y = value_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    # ylim(c(-30, 0)) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    scale_linetype_manual(values = dac_lty) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text = element_text(size = fig_text_size),
      # strip.text.x = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## figure c
  health_gap_fig_c <- ggplot(
    gaps_df %>%
      filter(
        !scen_id %in% remove_scen,
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = value_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_linetype_manual(values = poverty_lty) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    # ylim(c(-30, 0)) +
    theme_line +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label(expression(paste("PM"[2.5], " (",mu,"g ", m^{-3},")", " per person, difference from reference")),
  #                                    size = 8, angle = 90)

  yaxis_lab <- ggdraw() +
    draw_label(
      "Health: Avoided mortalities per million people (difference from reference)",
      size = fig_text_size,
      angle = 90
    )

  gaps_plot_grid_h <- plot_grid(
    NULL,
    health_gap_fig_b,
    NULL,
    health_gap_fig_c,
    NULL,
    health_gap_fig_a,
    align = "v",
    labels = c("", "A", "", "B", "", "C"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -0.5,
    vjust = 0.25,
    nrow = 6,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1, 1, 1),
    rel_heights = c(0.15, 1.1, 0.15, 0.9, 0.15, 0.9)
  )
  gaps_plot_grid_h

  gaps_plot_grid2 <- plot_grid(
    yaxis_lab,
    gaps_plot_grid_h,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  ## labor
  ## -------------------------------------------------------

  # ## calc 2020 pop by demographic
  # pop_2020 <- refining_mortality %>%
  #   filter(year == 2020) %>%
  #   select(census_tract, year, pop) %>%
  #   unique() %>%
  #   left_join(pop_ratios) %>%
  #   as.data.table()
  #
  # pop_2020[, demo_pop := pop * pct]
  #
  # ## summarize by demographic group
  # pop_2020 <- pop_2020[, .(pop_2020 = sum(demo_pop)),
  #   by = .(demo_group, demo_cat)
  # ]

  ## labor outputs
  l_gaps_df <- copy(ref_labor_demog_yr)

  l_gaps_df <- l_gaps_df[oil_price_scenario == "reference case", ]

  ## change scenario names, factor
  l_gaps_df[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  # gaps_df[, scenario := gsub('BAU', 'Reference', scenario)]
  l_gaps_df[, scenario := gsub("LC1.", "Low ", scenario)]

  ## add scenario title
  l_gaps_df[, scenario_title := scenario]
  l_gaps_df[, scenario_title := str_replace(scenario_title, " - ", "\n")]

  ## change historic to historical
  l_gaps_df[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  l_gaps_df[, scenario := str_replace(scenario, "historic", "historical")]
  l_gaps_df[,
    scenario_title := str_replace(scenario_title, "historic", "historical")
  ]

  ## scenarios for filtering
  remove_scen <- c(
    "Low demand - historical production",
    "BAU demand - historical production"
  )

  l_gaps_df$scenario <- factor(
    l_gaps_df$scenario,
    levels = c(
      "BAU demand - historical production",
      "BAU demand - historical exports",
      "BAU demand - low exports",
      "Low demand - historical exports",
      "Low demand - low exports",
      "Low demand - historical production"
    )
  )

  l_gaps_df$scenario_title <- factor(
    l_gaps_df$scenario_title,
    levels = c(
      "BAU demand\nhistorical production",
      "BAU demand\nhistorical exports",
      "BAU demand\nlow exports",
      "Low demand\nhistorical exports",
      "Low demand\nlow exports",
      "Low demand\nhistorical production"
    )
  )

  # ## sum for state
  # l_gaps_df <- l_gaps_df[, .(sum_demo_emp = sum(demo_emp)),
  #   by = .(
  #     year, demand_scenario, refining_scenario,
  #     scenario, scenario_title, demo_cat, demo_group, title
  #   )
  # ]

  ## sum for state
  l_gaps_df <- l_gaps_df[,
    .(
      sum_demo_emp = sum(sum_demo_emp),
      sum_demo_emp_revised = sum(sum_demo_emp_revised)
    ),
    by = .(
      year,
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      scenario,
      scenario_title,
      demo_cat,
      demo_group,
      title
    )
  ]

  ## pivot longer
  l_gaps_df <- l_gaps_df |>
    pivot_longer(
      sum_demo_emp:sum_demo_emp_revised,
      names_to = "employment_scen",
      values_to = "emp_value"
    ) |>
    as.data.table()

  # ## merge with 2020 pop
  # l_gaps_df <- merge(l_gaps_df, pop_2020,
  #                    by = c("demo_cat", "demo_group"),
  #                    all.x = T)
  #
  # ## calculate per capita
  # l_gaps_df[, demo_emp_pc := sum_demo_emp / pop_2020]

  ## select columns
  l_gaps_df <- l_gaps_df[, .(
    year,
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    scenario,
    scenario_title,
    demo_cat,
    demo_group,
    title,
    employment_scen,
    emp_value
  )]

  # filter for oil px == reference case
  l_gaps_df <- l_gaps_df[oil_price_scenario == "reference case"]

  ## calculate gaps (BAU - scenario)
  l_bau_gaps_df <- l_gaps_df[scenario == "BAU demand - historical production"]
  l_bau_gaps_df <- l_bau_gaps_df[, c(
    "product_scenario",
    "year",
    "demo_cat",
    "demo_group",
    "title",
    "employment_scen",
    "emp_value"
  )]
  setnames(l_bau_gaps_df, "emp_value", "bau_emp_value")
  # setnames(l_bau_gaps_df, "demo_emp_pc", "bau_demo_emp_pc")

  l_gaps_df <- merge(
    l_gaps_df,
    l_bau_gaps_df,
    by = c(
      "year",
      "demo_cat",
      "demo_group",
      "title",
      "product_scenario",
      "employment_scen"
    ),
    all.x = T
  )

  l_gaps_df[, gap_emp := emp_value - bau_emp_value]

  ## merge with 2020 pop
  l_gaps_df <- merge(
    l_gaps_df,
    pop_2020,
    by = c("demo_cat", "demo_group"),
    all.x = T
  )

  ## calculate per capita
  l_gaps_df[, gap_emp_pc := gap_emp / pop_2020]
  l_gaps_df[, gap_emp_pmil := gap_emp_pc * 1e6]

  ## figure labor a
  labor_gap_fig_a <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        ## with re-employment for main text
        employment_scen == "sum_demo_emp_revised",
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap_emp_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## figure labor a -- MAIN TEXT
  labor_gap_fig_a_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## with re-employment for main text
        employment_scen == "sum_demo_emp_revised",
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap_emp_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## figure labor a -- SI, 2020 px and no re-emp
  labor_gap_fig_a_2020ppx_no_reemp <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## without re-employment for SI
        employment_scen == "sum_demo_emp",
        title %in% fig_title_vec,
        demo_cat == "Race"
      ) %>%
      mutate(
        title = factor(title, levels = c("Black", "Hispanic", "Asian", "white"))
      ),
    aes(x = year, y = gap_emp_pmil, color = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_color_manual(
      name = "",
      values = race_col_pal
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      strip.text.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## legend a
  legend_a <- labor_gap_fig_a +
    geom_line(linewidth = 1, alpha = 0.9) +
    theme(legend.position = "right")

  legend_a <- get_legend(
    legend_a +
      theme(legend.text = element_text(size = fig_text_size))
  )

  ## labor b
  labor_gap_fig_b <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario == "changing prices",
        ## with re-employment for main text
        employment_scen == "sum_demo_emp_revised",
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
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
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      strip.text.x = element_text(size = fig_text_size),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor b -- MAIN TEXT
  labor_gap_fig_b_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario != "changing prices",
        ## with re-employment for main text
        employment_scen == "sum_demo_emp_revised"
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
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
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      strip.text.x = element_text(size = fig_text_size),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor b -- SI, without re-emp
  labor_gap_fig_b_2020ppx_no_reemp <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        demo_cat == "DAC",
        product_scenario != "changing prices",
        ## without re-employment for SI
        employment_scen == "sum_demo_emp"
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    scale_linetype_manual(values = dac_lty) +
    labs(
      x = NULL,
      y = NULL
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
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      strip.text.x = element_text(size = fig_text_size),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      # axis.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## legend b
  legend_b <- labor_gap_fig_b +
    geom_line(linewidth = 1, alpha = 0.9) +
    theme(legend.position = "right")

  legend_b <- get_legend(
    legend_b +
      theme(legend.text = element_text(size = fig_text_size))
  )

  ## labor c
  labor_gap_fig_c <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario == "changing prices",
        ## with re-employment for main text
        employment_scen == "sum_demo_emp_revised",
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(-70, 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor c -- MAIN TEXT
  labor_gap_fig_c_2020ppx <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## with re-employment for main text
        employment_scen == "sum_demo_emp_revised",
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(-70, 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## labor c -- SI, without re-emp
  labor_gap_fig_c_2020ppx_no_reemp <- ggplot(
    l_gaps_df %>%
      filter(
        !scenario %in% remove_scen,
        product_scenario != "changing prices",
        ## without re-employment for SI
        employment_scen == "sum_demo_emp",
        demo_cat == "Poverty"
      ) %>%
      mutate(
        title = factor(
          title,
          levels = c("Below poverty line", "Above poverty line")
        )
      ),
    aes(x = year, y = gap_emp_pmil, lty = title)
  ) +
    geom_line(linewidth = 1, alpha = 0.8, color = "black") +
    scale_linetype_manual(values = poverty_lty) +
    geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.5) +
    facet_grid(demo_cat ~ scenario_title) +
    labs(
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      breaks = c(2020, 2045), # Specify tick mark positions
      labels = c(2020, 2045)
    ) + # Specify tick mark labels
    theme_line +
    # ylim(-70, 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(
        vjust = 0.5,
        hjust = 0.5,
        size = fig_text_size
      ),
      axis.text.y = element_text(size = fig_text_size),
      strip.text.y = element_text(size = fig_text_size),
      legend.key.width = unit(10, "mm"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.text.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.length.x = unit(0.1, "cm")
    )

  ## legend c
  legend_c <- labor_gap_fig_c +
    geom_line(linewidth = 1, alpha = 0.9) +
    theme(legend.position = "right")

  legend_c <- get_legend(
    legend_c +
      theme(legend.text = element_text(size = fig_text_size))
  )

  ## shared y lab
  # yaxis_lab <- ggdraw() + draw_label("Labor: FTE job-years, difference from reference", size = 8, angle = 90)
  yaxis_lab <- ggdraw() +
    draw_label(
      "Labor: FTE direct employment changes per million people (difference from reference)",
      size = fig_text_size,
      angle = 90
    )

  ## 2020 product prices, MAIN TEXT
  l_gaps_plot_grid_2020ppx <- plot_grid(
    NULL,
    labor_gap_fig_b_2020ppx + theme(legend.position = "none"),
    NULL,
    labor_gap_fig_c_2020ppx + theme(legend.position = "none"),
    NULL,
    labor_gap_fig_a_2020ppx + theme(legend.position = "none"),
    align = "v",
    labels = c("", "D", "", "E", "", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -0.5,
    vjust = 0.25,
    nrow = 6,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1, 1, 1),
    rel_heights = c(0.15, 1.1, 0.15, 0.9, 0.15, 0.9)
  )

  l_gaps_plot_grid2_2020ppx <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid_2020ppx,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  ## 2020 product prices, no-remp SI
  l_gaps_plot_grid_2020ppx_no_reemp <- plot_grid(
    NULL,
    labor_gap_fig_b_2020ppx_no_reemp + theme(legend.position = "none"),
    NULL,
    labor_gap_fig_c_2020ppx_no_reemp + theme(legend.position = "none"),
    NULL,
    labor_gap_fig_a_2020ppx_no_reemp + theme(legend.position = "none"),
    align = "v",
    labels = c("", "D", "", "E", "", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -0.5,
    vjust = 0.25,
    nrow = 6,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1, 1, 1),
    rel_heights = c(0.15, 1.1, 0.15, 0.9, 0.15, 0.9)
  )

  l_gaps_plot_grid_2020ppx_no_reemp <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid_2020ppx_no_reemp,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  ## original
  l_gaps_plot_grid <- plot_grid(
    NULL,
    labor_gap_fig_b + theme(legend.position = "none"),
    NULL,
    labor_gap_fig_c + theme(legend.position = "none"),
    NULL,
    labor_gap_fig_a + theme(legend.position = "none"),
    align = "v",
    labels = c("", "D", "", "E", "", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -0.5,
    vjust = 0.25,
    nrow = 6,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1, 1, 1),
    rel_heights = c(0.15, 1.1, 0.15, 0.9, 0.15, 0.9)
  )

  l_gaps_plot_grid2 <- plot_grid(
    yaxis_lab,
    l_gaps_plot_grid,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 2,
    rel_widths = c(0.05, 1),
    rel_heights = c(1, 1)
  )

  l_gaps_plot_grid2

  ## plot legends
  legends <- plot_grid(
    NULL,
    legend_b,
    NULL,
    legend_c,
    NULL,
    legend_a,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 6,
    ncol = 1,
    rel_widths = c(1, 1, 1, 1, 1, 1),
    rel_heights = c(0.15, 1, 0.15, 1, 0.15, 1)
  )

  ## plot side by side
  ## ----------------------------------------------

  health_labor_plot_2020ppx <- plot_grid(
    gaps_plot_grid2,
    l_gaps_plot_grid2_2020ppx,
    NULL,
    legends,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 4,
    rel_widths = c(1, 1, 0.05, 0.2),
    rel_heights = c(1, 1, 1, 1)
  )

  simple_ggsave_repo(
    plot = health_labor_plot_2020ppx,
    folder_path = NULL,
    filename = "health_labor_gaps_pmil_plot_2020ppx",
    width = 18,
    height = 6,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "extra-figure-4",
    extra_subfolder = "extra-figure-4"
  )

  health_labor_plot_2020ppx_no_reemp <- plot_grid(
    gaps_plot_grid2,
    l_gaps_plot_grid_2020ppx_no_reemp,
    NULL,
    legends,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 4,
    rel_widths = c(1, 1, 0.05, 0.2),
    rel_heights = c(1, 1, 1, 1)
  )

  simple_ggsave_repo(
    plot = health_labor_plot_2020ppx_no_reemp,
    folder_path = NULL,
    filename = "health_labor_gaps_pmil_plot_2020ppx_no_reemp",
    width = 18,
    height = 6,
    dpi = 600,
    save_path = save_path,
    file_type = "figure",
    figure_number = "figure-4"
  )

  health_labor_plot <- plot_grid(
    gaps_plot_grid2,
    l_gaps_plot_grid2,
    NULL,
    legends,
    align = "v",
    # labels = c("A", "B", "C", "D", "E", "F"),
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    nrow = 1,
    ncol = 4,
    rel_widths = c(1, 1, 0.05, 0.2),
    rel_heights = c(1, 1, 1, 1)
  )

  return(health_labor_plot)
}
