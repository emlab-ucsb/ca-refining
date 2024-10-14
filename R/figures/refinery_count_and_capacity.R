get_refinery_count_and_capacity <- function(res_crude_ref_reg, res_renew_ref_reg, ei_crude, ei_gasoline) {
  agg_crude_cap <- res_crude_ref_reg[, .(
    crude_barrels_per_day = sum(barrels_per_day, na.rm = T),
    no_crude_refineries = uniqueN(site_id)
  ),
  by = .(demand_scenario, refining_scenario, region, year)
  ]
  setorder(agg_crude_cap, demand_scenario, refining_scenario, region)

  agg_renew_cap <- res_renew_ref_reg[, .(
    renew_barrels_per_day = sum(installation_capacity_bpd, na.rm = T),
    no_renew_refineries = uniqueN(refinery_name)
  ),
  by = .(demand_scenario, refining_scenario, year)
  ]
  agg_renew_cap[, region := "Renewables"]
  setorder(agg_renew_cap, demand_scenario, refining_scenario, region)

  agg_cap_all <- rbindlist(list(agg_crude_cap, agg_renew_cap), use.names = T, fill = T)
  agg_cap_all[, region := factor(region, levels = c("North", "South", "Renewables"))]

  # create single column for capacity (barrels per day)
  agg_cap_all[, capacity_barrels_per_day := ifelse(is.na(crude_barrels_per_day), renew_barrels_per_day, crude_barrels_per_day)]

  # create single column for no_refineries
  agg_cap_all[, no_refineries := ifelse(is.na(no_crude_refineries), no_renew_refineries, no_crude_refineries)]

  # drop columns
  agg_cap_all[, c("crude_barrels_per_day", "renew_barrels_per_day", "no_crude_refineries", "no_renew_refineries") := NULL]

  # order factor levels for refining_scenario: historic production, historic exports, low exports
  agg_cap_all[, refining_scenario := factor(refining_scenario, levels = c("historic production", "historic exports", "low exports"))]

  return(agg_cap_all)
}

plot_refinery_capacity <- function(res_crude_ref_reg, res_renew_ref_reg, ei_crude, ei_gasoline) {
  agg_cap_all <- get_refinery_count_and_capacity(res_crude_ref_reg, res_renew_ref_reg, ei_crude, ei_gasoline)

  fig_refinery_cap <- ggplot(agg_cap_all) +
    geom_line(aes(x = year, y = (capacity_barrels_per_day * 365 * (ei_crude / ei_gasoline)) / 1e6, color = region), linewidth = 1.3) +
    facet_wrap(demand_scenario ~ refining_scenario,
      labeller = labeller(
        demand_scenario = c(
          "BAU" = "BAU Demand",
          "LC1" = "Low Demand"
        ),
        refining_scenario = c(
          "historic exports" = "Historical Exports",
          "historic production" = "Historical Production",
          "low exports" = "Low Exports"
        )
      )
    ) +
    labs(
      title = NULL,
      x = NULL,
      y = "Annual capacity\n(million barrels of gasoline equivalent per year)",
      color = NULL,
      linetype = NULL
    ) +
    scale_x_continuous(breaks = seq(2020, 2040, 5), limits = c(2020, 2045), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 450, 50), limits = c(0, 450)) +
    scale_color_ipsum() +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 25, vjust = 0.5),
      legend.text = element_text(size = 25, vjust = 0.5),
      axis.title.y = element_text(size = 26),
      axis.title.y.right = element_text(size = 26),
      strip.text = element_text(size = 25),
      plot.subtitle = element_text(size = 25),
      plot.title = element_text(size = 25),
      plot.caption = element_text(size = 25),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24)
    )

  return(fig_refinery_cap)
}

plot_refinery_count <- function(res_crude_ref_reg, res_renew_ref_reg, ei_crude, ei_gasoline) {
  agg_cap_all <- get_refinery_count_and_capacity(res_crude_ref_reg, res_renew_ref_reg, ei_crude, ei_gasoline)

  fig_refinery_count <- ggplot(agg_cap_all) +
    geom_line(aes(x = year, y = no_refineries, color = region), linewidth = 1.3) +
    facet_wrap(demand_scenario ~ refining_scenario,
      labeller = labeller(
        demand_scenario = c(
          "BAU" = "BAU Demand",
          "LC1" = "Low Demand"
        ),
        refining_scenario = c(
          "historic exports" = "Historical Exports",
          "historic production" = "Historical Production",
          "low exports" = "Low Exports"
        )
      )
    ) +
    labs(
      title = NULL,
      x = NULL,
      y = "Number of refineries\noperational each year",
      color = NULL,
      linetype = NULL
    ) +
    scale_x_continuous(breaks = seq(2020, 2040, 5), limits = c(2020, 2045), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 10, 1), limits = c(0, 8)) +
    scale_color_ipsum() +
    theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 25, vjust = 0.5),
      legend.text = element_text(size = 25, vjust = 0.5),
      axis.title.y = element_text(size = 26),
      axis.title.y.right = element_text(size = 26),
      strip.text = element_text(size = 25),
      plot.subtitle = element_text(size = 25),
      plot.title = element_text(size = 25),
      plot.caption = element_text(size = 25),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24)
    )

  return(fig_refinery_count)
}
