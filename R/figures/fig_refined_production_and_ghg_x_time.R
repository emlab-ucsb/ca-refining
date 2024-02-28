plot_refined_products_and_ghg <- function(tot_fuel_demand_exports, state_ghg_output) {
  ## read in production file
  prod_data <- copy(tot_fuel_demand_exports)

  ## add 'fuel' after 'jet'
  prod_data[, fuel_adj := fuel]
  prod_data[fuel == "jet", fuel_adj := "jet fuel"]

  ## rename fuel
  prod_data[fuel_adj == "drop-in gasoline", fuel_adj := "renewable gasoline"]


  ## read in ghg emissions file
  ghg_data <- state_ghg_output[source == "total" & boundary == "complete"]
  ghg_data[, ghg_MtCO2 := value / 1e9]
  ghg_data[, label := "GHG emissions"]

  ## change demand scenario name
  prod_data[, demand_scenario_adj := ifelse(demand_scenario == "BAU", "Reference Demand", "Low Carbon Demand")]
  ghg_data[, demand_scenario_adj := ifelse(demand_scenario == "BAU", "Reference Demand", "Low Carbon Demand")]


  ## capitalize first letter of fuel
  prod_data[, fuel_adj := str_to_title(fuel_adj)]

  ## factor fuel
  prod_data[, fuel_adj := factor(fuel_adj, levels = rev(c(
    "Gasoline",
    "Renewable Gasoline",
    "Diesel",
    "Renewable Diesel",
    "Jet Fuel",
    "Sustainable Aviation Fuel",
    "Exports"
  )))]

  ## refactor refining scenario
  prod_data[, refining_scenario := factor(refining_scenario, levels = c(
    "historic production",
    "historic exports",
    "low exports"
  ))]
  ghg_data[, refining_scenario := factor(refining_scenario, levels = c(
    "historic production",
    "historic exports",
    "low exports"
  ))]

  ## refactor demand scenario
  prod_data[, demand_scenario_adj := factor(demand_scenario_adj, levels = c("Reference Demand", "Low Carbon Demand"))]
  ghg_data[, demand_scenario_adj := factor(demand_scenario_adj, levels = c("Reference Demand", "Low Carbon Demand"))]


  ## figure ------

  coef <- 19

  fig_refined_production_ghg <- ggplot() +
    geom_area(data = prod_data, aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)) +
    geom_line(data = ghg_data, aes(x = year, y = ghg_MtCO2 * coef, color = label), linewidth = 1.3) +
    facet_wrap(demand_scenario_adj ~ refining_scenario,
      ncol = 3,
      labeller = labeller(refining_scenario = c(
        "historic exports" = "Historic Exports",
        "historic production" = "Historic Production",
        "low exports" = "Low Exports"
      ))
    ) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "black", linewidth = 1) +
    # geom_segment(x = 2019, xend = 2019, y = 0, yend = 750, color = 'black', linetype = 2)  +
    labs(
      title = NULL,
      x = NULL,
      y = "Million barrels of gasoline equivalent",
      fill = NULL,
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(breaks = seq(2015, 2040, 5), limits = c(2014, 2045), expand = c(0, 0)) +
    scale_y_continuous(
      name = "Fuel production (Million barrels of gasoline equivalent)",
      sec.axis = sec_axis(~ . / coef, name = bquote(GHG ~ emissions ~ (MtCO[2]))),
      expand = c(0, 0),
      breaks = seq(0, 700, 100)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2)
    ) +
    scale_color_manual(values = pal_label) +
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
  fig_refined_production_ghg
}


# ggsave(fig_fuel_demand_tot,
#        filename = file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.png'),
#        width = 20,
#        height = 12,
#        dpi = 600)
#
# ggsave(fig_fuel_demand_tot,
#        filename = file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.pdf'),
#        width = 20,
#        height = 12)
#
# embed_fonts(file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.pdf'),
#             outfile = file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.pdf'))
