plot_combined_production <- function(
  its_data,
  jet_data,
  intra_data,
  fuel_demand_exports_data,
  state_ghg_data
) {
  # tar_load(dt_its)
  # tar_load(dt_jet)
  # tar_load(dt_intra)
  # tar_load(tot_fuel_demand_exports)
  # tar_load(state_ghg_output)

  # its_data <- copy(dt_its)
  # intra_data <- copy(dt_intra)
  # jet_data <- copy(dt_jet)
  # fuel_demand_exports_data <- copy(tot_fuel_demand_exports)
  # state_ghg_data <- copy(state_ghg_output)
  # rm(dt_its, dt_intra, dt_jet, tot_fuel_demand_exports, state_ghg_output)

  # dt_its <- copy(its_data)
  # dt_intra <- copy(intra_data)
  # dt_jet <- copy(jet_data)

  label_its = "Total intrastate transportation\nliquid fuels"
  label_all = "Total transportation liquid fuels\nincluding for interstate and military aviation"

  dt_its <- copy(its_data)
  dt_intra <- copy(intra_data)
  dt_jet <- copy(jet_data)

  # calculate bge
  dt_jet[, consumption_bge := total_jet_fuel_demand_gge / 42]
  dt_its[, consumption_bge := consumption_gge / 42]
  dt_intra[, consumption_bge := consumption_gge / 42]
  dt_intra[, j := 1]

  # unique set of scenarios
  dt_jet <- dt_jet[scenario == "Mid Case"]
  un_scens <- CJ(
    year = 2017:2050,
    scenario = c("BAU Demand", "Low Carbon Demand"),
    j = 1
  )

  # create full set of intrastate jet fuel demand
  dt_intra <- dt_intra[un_scens, on = .(j, year), allow.cartesian = T]
  dt_intra[, j := NULL]
  dt_intra <- dt_intra[year >= 2019]

  dt_intra_2 <- dt_intra[, .(scenario, year, consumption_bge)]
  setnames(dt_intra_2, "consumption_bge", "intra_consumption_bge")
  setorder(dt_intra_2, scenario, year)

  # seperate interstate from intrastate demand
  dt_jet <- dt_jet[, .(year, consumption_bge)]
  dt_jet[, fuel := "jet (total)"]
  dt_jet[, j := 1]

  dt_jet <- dt_jet[un_scens, on = .(j, year), allow.cartesian = T]
  dt_jet[, j := NULL]
  dt_jet <- dt_jet[year >= 2019 & year <= 2045]

  dt_jet_2 <- dt_jet[, .(scenario, year, consumption_bge)]
  setnames(dt_jet_2, "consumption_bge", "total_jet_consumption_bge")
  setorder(dt_jet_2, scenario, year)

  dt_intra_2 <- dt_intra_2[dt_jet_2, on = .(scenario, year)]
  dt_intra_2[,
    inter_consumption_bge := total_jet_consumption_bge - intra_consumption_bge
  ]

  dt_intra_all <- melt(
    dt_intra_2,
    id.vars = c("scenario", "year"),
    measure.vars = c("intra_consumption_bge", "inter_consumption_bge"),
    variable.name = "fuel",
    value.name = "consumption_bge"
  )
  dt_intra_all[fuel == "intra_consumption_bge", fuel := "jet fuel (intrastate)"]
  dt_intra_all[
    fuel == "inter_consumption_bge",
    fuel := "jet fuel (interstate + military)"
  ]

  # combine demand ------

  dt_demand <- rbindlist(list(
    dt_its[, .(scenario, year, fuel, consumption_bge)],
    dt_intra_all[, .(scenario, year, fuel, consumption_bge)]
  ))
  setorder(dt_demand, scenario, year, fuel)

  # rename fuel ------

  dt_demand[fuel == "drop-in gasoline", fuel := "renewable gasoline"]
  dt_demand[, fuel := str_to_title(fuel)]
  dt_demand[, fuel := gsub("Ldv", "LDV", fuel)]
  dt_demand[, fuel := gsub("Hdv", "HDV", fuel)]

  # rename scenario -------
  dt_demand[scenario == "BAU", scenario := "BAU Demand"]
  dt_demand[scenario == "LC1", scenario := "Low Carbon Demand"]

  # combine HDV and LDV fuels ------
  dt_demand[
    fuel %in% c("LDV Electricity", "HDV Electricity"),
    fuel := "Electricity"
  ]
  dt_demand[fuel %in% c("LDV Hydrogen", "HDV Hydrogen"), fuel := "Hydrogen"]

  # aggregate combined fuels
  dt_demand <- dt_demand[,
    .(consumption_bge = sum(consumption_bge, na.rm = TRUE)),
    by = .(scenario, year, fuel)
  ]

  # reorder factor levels ------

  dt_demand[,
    fuel := factor(
      fuel,
      levels = rev(c(
        "Gasoline",
        "Renewable Gasoline",
        "Diesel",
        "Renewable Diesel",
        "Jet Fuel (Intrastate)",
        "Sustainable Aviation Fuel",
        "Jet Fuel (Interstate + Military)",
        "Ethanol",
        "Biodiesel",
        "Renewable Natural Gas",
        "Hydrogen",
        "Electricity"
      ))
    )
  ]

  # get line of Total intrastate transportation liquid fuels demand included -------

  inc_its <- dt_demand[
    fuel %in%
      c(
        "Gasoline",
        "Renewable Gasoline",
        "Diesel",
        "Renewable Diesel",
        "Jet Fuel (Intrastate)",
        "Sustainable Aviation Fuel"
      )
  ]
  inc_its <- inc_its[,
    .(consumption_bge = sum(consumption_bge, na.rm = T)),
    by = .(scenario, year)
  ]
  inc_its[scenario == "BAU", scenario := "BAU Demand"]
  inc_its[scenario == "LC1", scenario := "Low Carbon Demand"]

  # get line of total fuels included (not exports) ---------

  inc_full <- dt_demand[
    fuel %in%
      c(
        "Gasoline",
        "Renewable Gasoline",
        "Diesel",
        "Renewable Diesel",
        "Jet Fuel (Intrastate)",
        "Sustainable Aviation Fuel",
        "Jet Fuel (Interstate + Military)"
      )
  ]
  inc_full <- inc_full[,
    .(consumption_bge = sum(consumption_bge, na.rm = T)),
    by = .(scenario, year)
  ]
  inc_full[scenario == "BAU", scenario := "BAU Demand"]
  inc_full[scenario == "LC1", scenario := "Low Carbon Demand"]

  # combine interstate+military jet fuel with intrastate jet fuel -----

  dt_demand2 <- copy(dt_demand)
  dt_demand2[,
    fuel := fifelse(fuel %like% "Jet Fuel", "Jet Fuel", as.character(fuel))
  ]
  dt_demand2 <- dt_demand2[,
    .(consumption_bge = sum(consumption_bge, na.rm = T)),
    by = .(scenario, year, fuel)
  ]

  # rename scenario -------

  dt_demand2[scenario == "BAU", scenario := "BAU Demand"]
  dt_demand2[scenario == "LC1", scenario := "Low Carbon Demand"]

  # reorder factor levels ------

  dt_demand2[,
    fuel := factor(
      fuel,
      levels = rev(c(
        "Gasoline",
        "Renewable Gasoline",
        "Diesel",
        "Renewable Diesel",
        "Jet Fuel",
        "Sustainable Aviation Fuel",
        "Ethanol",
        "Biodiesel",
        "Renewable Natural Gas",
        "Hydrogen",
        "Electricity"
      ))
    )
  ]

  # refactor scenario -------

  dt_demand2[,
    scenario := factor(scenario, levels = c("BAU Demand", "Low Carbon Demand"))
  ]
  inc_its[,
    scenario := factor(scenario, levels = c("BAU Demand", "Low Carbon Demand"))
  ]
  inc_full[,
    scenario := factor(scenario, levels = c("BAU Demand", "Low Carbon Demand"))
  ]

  # PREP PRODUCTION DATA ----------

  ## read in production file
  prod_data <- copy(fuel_demand_exports_data)

  ## add 'fuel' after 'jet'
  prod_data[, fuel_adj := fuel]
  prod_data[fuel == "jet", fuel_adj := "jet fuel"]

  ## rename fuel
  prod_data[fuel_adj == "drop-in gasoline", fuel_adj := "renewable gasoline"]

  ## read in ghg emissions file
  ghg_data <- copy(state_ghg_data)[source == "total" & boundary == "complete"]
  ghg_data[, ghg_MtCO2 := value / 1e9]
  ghg_data[, label := "GHG emissions"]

  ## change demand scenario name
  prod_data[,
    demand_scenario_adj := ifelse(
      demand_scenario == "BAU",
      "BAU Demand",
      "Low Carbon Demand"
    )
  ]
  ghg_data[,
    demand_scenario_adj := ifelse(
      demand_scenario == "BAU",
      "BAU Demand",
      "Low Carbon Demand"
    )
  ]

  ## capitalize first letter of fuel
  prod_data[, fuel_adj := str_to_title(fuel_adj)]

  ## factor fuel
  prod_data[,
    fuel_adj := factor(
      fuel_adj,
      levels = rev(c(
        "Gasoline",
        "Renewable Gasoline",
        "Diesel",
        "Renewable Diesel",
        "Jet Fuel",
        "Sustainable Aviation Fuel",
        "Exports"
      ))
    )
  ]

  ## capitalize first letter of scenario
  prod_data[, refining_scenario_adj := str_to_title(refining_scenario)]
  ghg_data[, refining_scenario_adj := str_to_title(refining_scenario)]

  ## refactor refining scenario
  prod_data[,
    refining_scenario_adj := factor(
      refining_scenario_adj,
      levels = c(
        "Historic Production",
        "Historic Exports",
        "Low Exports"
      )
    )
  ]
  ghg_data[,
    refining_scenario_adj := factor(
      refining_scenario_adj,
      levels = c(
        "Historic Production",
        "Historic Exports",
        "Low Exports"
      )
    )
  ]

  ## refactor demand scenario
  prod_data[,
    demand_scenario_adj := factor(
      demand_scenario_adj,
      levels = c("BAU Demand", "Low Carbon Demand")
    )
  ]
  ghg_data[,
    demand_scenario_adj := factor(
      demand_scenario_adj,
      levels = c("BAU Demand", "Low Carbon Demand")
    )
  ]

  # its figure theme -----

  theme_its <- theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22, vjust = 0.5),
      legend.text = element_text(size = 22, vjust = 0.5),
      legend.background = element_rect(fill = "white", color = "white"),
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      axis.title.y.right = element_text(size = 25),
      strip.text = element_text(size = 25),
      plot.subtitle = element_text(size = 25),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.caption = element_text(size = 25),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22)
    )

  # fig: reference demand + its demand ------

  f_ref_its <-
    ggplot() +
    geom_area(
      data = dt_demand2[scenario == "BAU Demand"],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel, group = fuel)
    ) +
    geom_line(
      data = inc_its[scenario == "BAU Demand"],
      aes(x = year, y = consumption_bge / 1e6, lty = "its"),
      linewidth = 1,
      color = "black"
    ) +
    geom_line(
      data = inc_full[scenario == "BAU Demand"],
      aes(x = year, y = consumption_bge / 1e6, lty = "all"),
      linewidth = 1,
      color = "black"
    ) +
    labs(
      title = "BAU Demand\n(Only)",
      subtitle = NULL,
      x = NULL,
      y = "Fuel demand\n(Million bge per year)",
      fill = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2020, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(values = pal_fuel, guide = guide_legend(nrow = 7)) +
    scale_linetype_manual(
      name = "Demand produced by oil refineries",
      labels = c(
        "its" = label_its,
        "all" = label_all
      ),
      values = c(
        "its" = 3,
        "all" = 2
      ),
      guide = guide_legend(nrow = 2, title.position = "top")
    ) +
    theme_its +
    theme(legend.position = "none")

  # fig: low carbon demand + its demand ------

  f_lc_its <-
    ggplot() +
    geom_area(
      data = dt_demand2[scenario == "Low Carbon Demand"],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel, group = fuel)
    ) +
    geom_line(
      data = inc_its[scenario == "Low Carbon Demand"],
      aes(x = year, y = consumption_bge / 1e6, lty = "its"),
      linewidth = 1,
      color = "black"
    ) +
    geom_line(
      data = inc_full[scenario == "Low Carbon Demand"],
      aes(x = year, y = consumption_bge / 1e6, lty = "all"),
      linewidth = 1,
      color = "black"
    ) +
    labs(
      title = "Low Carbon Demand\n(Only)",
      subtitle = NULL,
      x = NULL,
      y = "Fuel demand\n(Million bge per year)",
      fill = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2020, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(values = pal_fuel, guide = guide_legend(nrow = 7)) +
    scale_linetype_manual(
      name = "Demand produced by oil refineries",
      labels = c(
        "its" = label_its,
        "all" = label_all
      ),
      values = c(
        "its" = 3,
        "all" = 2
      ),
      guide = guide_legend(nrow = 2, title.position = "top")
    ) +
    theme_its +
    theme(legend.position = "none")

  # legend: its -----------
  f_its_legend <-
    ggplot() +
    geom_area(
      data = dt_demand2[!fuel %in% unique(prod_data[, fuel_adj])],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel, group = fuel)
    ) +
    geom_line(
      data = inc_its,
      aes(x = year, y = consumption_bge / 1e6, lty = "its"),
      linewidth = 1,
      color = "black"
    ) +
    geom_line(
      data = inc_full,
      aes(x = year, y = consumption_bge / 1e6, lty = "all"),
      linewidth = 1,
      color = "black"
    ) +
    facet_wrap(~scenario, nrow = 2) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = "Fuel demand\n(Million bge per year)",
      fill = "Fuels not produced by oil refineries\n(Subfigures A and B only)",
      linetype = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2020, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel,
      guide = guide_legend(nrow = 7, title.position = "top")
    ) +
    scale_linetype_manual(
      name = "Demand produced by oil refineries",
      labels = c(
        "its" = label_its,
        "all" = label_all
      ),
      values = c(
        "its" = 3,
        "all" = 2
      ),
      guide = guide_legend(nrow = 2, title.position = "top")
    ) +
    theme_its +
    theme(legend.key.width = unit(1, "cm"))

  grobs_its <- ggplotGrob(f_its_legend)$grobs
  legend_its_combined <- grobs_its[[which(
    sapply(grobs_its, function(x) x$name) == "guide-box"
  )]]

  # Helper function to extract legend
  g_legend <- function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }

  # Extract individual legends from the combined its legend
  # The combined legend contains both fill (fuels not produced) and linetype (transportation lines)
  legend_lines <- g_legend(f_its_legend + guides(fill = "none"))
  legend_fuels_not_refinery <- g_legend(
    f_its_legend + guides(linetype = "none")
  )

  # production figures settings --------

  # Calculate optimal coefficient to bring GHG line closer to area chart
  # Get max stacked fuel production value across all scenarios and years
  # This represents the total height of the stacked area chart at its peak
  max_stacked_production <- max(
    prod_data[,
      .(total_consumption = sum(consumption_bge / 1e6, na.rm = TRUE)),
      by = .(demand_scenario_adj, refining_scenario_adj, year)
    ][, total_consumption],
    na.rm = TRUE
  )

  # Get max GHG emissions value across all scenarios
  max_ghg_emissions <- max(ghg_data[, ghg_MtCO2], na.rm = TRUE)

  # Calculate coefficient to make GHG line at the top of stacked area chart
  coef <- 1.06 * (max_stacked_production / max_ghg_emissions)

  theme_prod <- theme_line +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22, vjust = 0.5),
      legend.text = element_text(size = 22, vjust = 0.5),
      legend.background = element_rect(fill = "white", color = "white"),
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      axis.title.y.right = element_text(size = 25),
      strip.text = element_text(size = 25),
      plot.subtitle = element_text(size = 25),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.caption = element_text(size = 25),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20)
    )

  # fig: reference demand + Historic production -------

  f_ref_histprod <- ggplot() +
    geom_area(
      data = prod_data[
        demand_scenario_adj == "BAU Demand" &
          refining_scenario_adj == "Historic Production"
      ],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)
    ) +
    geom_line(
      data = ghg_data[
        demand_scenario_adj == "BAU Demand" &
          refining_scenario_adj == "Historic Production"
      ],
      aes(x = year, y = ghg_MtCO2 * coef, color = label),
      linewidth = 1.3
    ) +
    geom_vline(
      xintercept = 2020,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      title = "BAU Demand\nHistorical Production",
      x = NULL,
      y = "Million bge per year",
      fill = NULL,
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2014, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Fuel production\n(Million bge per year)",
      sec.axis = sec_axis(
        ~ . / coef,
        name = bquote(GHG ~ emissions ~ (MtCO[2]))
      ),
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2)
    ) +
    scale_color_manual(values = pal_label) +
    theme_prod +
    theme(legend.position = "none")

  # fig: reference demand + Historic exports -------

  f_ref_histexp <- ggplot() +
    geom_area(
      data = prod_data[
        demand_scenario_adj == "BAU Demand" &
          refining_scenario_adj == "Historic Exports"
      ],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)
    ) +
    geom_line(
      data = ghg_data[
        demand_scenario_adj == "BAU Demand" &
          refining_scenario_adj == "Historic Exports"
      ],
      aes(x = year, y = ghg_MtCO2 * coef, color = label),
      linewidth = 1.3
    ) +
    geom_vline(
      xintercept = 2020,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      title = "BAU Demand\nHistorical Exports",
      x = NULL,
      y = "Million bge per year",
      fill = NULL,
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2014, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Fuel production\n(Million bge per year)",
      sec.axis = sec_axis(
        ~ . / coef,
        name = bquote(GHG ~ emissions ~ (MtCO[2]))
      ),
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2)
    ) +
    scale_color_manual(values = pal_label) +
    theme_prod +
    theme(legend.position = "none")

  # fig: reference demand + low exports -------

  f_ref_lowexp <- ggplot() +
    geom_area(
      data = prod_data[
        demand_scenario_adj == "BAU Demand" &
          refining_scenario_adj == "Low Exports"
      ],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)
    ) +
    geom_line(
      data = ghg_data[
        demand_scenario_adj == "BAU Demand" &
          refining_scenario_adj == "Low Exports"
      ],
      aes(x = year, y = ghg_MtCO2 * coef, color = label),
      linewidth = 1.3
    ) +
    geom_vline(
      xintercept = 2020,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      title = "BAU Demand\nLow Exports",
      x = NULL,
      y = "Million bge per year",
      fill = NULL,
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2014, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Fuel production\n(Million bge per year)",
      sec.axis = sec_axis(
        ~ . / coef,
        name = bquote(GHG ~ emissions ~ (MtCO[2]))
      ),
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2)
    ) +
    scale_color_manual(values = pal_label) +
    theme_prod +
    theme(legend.position = "none")

  # fig: low carbon demand + Historic production -------

  f_lc_histprod <- ggplot() +
    geom_area(
      data = prod_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Historic Production"
      ],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)
    ) +
    geom_line(
      data = ghg_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Historic Production"
      ],
      aes(x = year, y = ghg_MtCO2 * coef, color = label),
      linewidth = 1.3
    ) +
    geom_vline(
      xintercept = 2020,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      title = "Low Carbon Demand\nHistorical Production",
      x = NULL,
      y = "Million bge per year",
      fill = NULL,
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2014, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Fuel production\n(Million bge per year)",
      sec.axis = sec_axis(
        ~ . / coef,
        name = bquote(GHG ~ emissions ~ (MtCO[2]))
      ),
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2)
    ) +
    scale_color_manual(values = pal_label) +
    theme_prod +
    theme(legend.position = "none")

  # fig: low carbon demand + Historic exports -------

  f_lc_histexp <- ggplot() +
    geom_area(
      data = prod_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Historic Exports"
      ],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)
    ) +
    geom_line(
      data = ghg_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Historic Exports"
      ],
      aes(x = year, y = ghg_MtCO2 * coef, color = label),
      linewidth = 1.3
    ) +
    geom_vline(
      xintercept = 2020,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      title = "Low Carbon Demand\nHistorical Exports",
      x = NULL,
      y = "Million bge per year",
      fill = NULL,
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2014, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Fuel production\n(Million bge per year)",
      sec.axis = sec_axis(
        ~ . / coef,
        name = bquote(GHG ~ emissions ~ (MtCO[2]))
      ),
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2)
    ) +
    scale_color_manual(values = pal_label) +
    theme_prod +
    theme(legend.position = "none")

  # fig: low carbon demand + Historic exports -------

  f_lc_lowexp <- ggplot() +
    geom_area(
      data = prod_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Low Exports"
      ],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)
    ) +
    geom_line(
      data = ghg_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Low Exports"
      ],
      aes(x = year, y = ghg_MtCO2 * coef, color = label),
      linewidth = 1.3
    ) +
    geom_vline(
      xintercept = 2020,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      title = "Low Carbon Demand\nLow Exports",
      x = NULL,
      y = "Million bge per year",
      fill = NULL,
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2014, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Fuel production\n(Million bge per year)",
      sec.axis = sec_axis(
        ~ . / coef,
        name = bquote(GHG ~ emissions ~ (MtCO[2]))
      ),
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2)
    ) +
    scale_color_manual(values = pal_label) +
    theme_prod +
    theme(legend.position = "none")

  # legend: production -------

  f_prod_legend <- ggplot() +
    geom_area(
      data = prod_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Low Exports"
      ],
      aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)
    ) +
    geom_line(
      data = ghg_data[
        demand_scenario_adj == "Low Carbon Demand" &
          refining_scenario_adj == "Low Exports"
      ],
      aes(x = year, y = ghg_MtCO2 * coef, color = label),
      linewidth = 1.3
    ) +
    geom_vline(
      xintercept = 2020,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      title = "Low Carbon Demand\nLow Exports",
      x = NULL,
      y = "Million bge per year",
      fill = "Fuels produced by oil refineries",
      linetype = NULL,
      color = NULL
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2040, 10),
      limits = c(2014, 2045),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Fuel production\n(Million bge per year)",
      sec.axis = sec_axis(
        ~ . / coef,
        name = bquote(GHG ~ emissions ~ (MtCO[2]))
      ),
      expand = c(0, 0),
      breaks = seq(0, 700, 100),
      limits = c(0, 700)
    ) +
    scale_fill_manual(
      values = pal_fuel_title,
      guide = guide_legend(reverse = TRUE, nrow = 2, title.position = "top")
    ) +
    scale_color_manual(values = pal_label) +
    theme_prod +
    guides(fill = guide_legend(ncol = 1, title.position = "top")) +
    theme(legend.key.width = unit(1, "cm"))

  # Extract individual legends from production plot
  legend_ghg <- g_legend(f_prod_legend + guides(fill = "none"))
  legend_fuels_refinery <- g_legend(f_prod_legend + guides(color = "none"))

  # arrange plots and legends --------

  plots_ts <- plot_grid(
    f_ref_its,
    f_ref_histprod,
    f_ref_histexp,
    f_ref_lowexp,
    f_lc_its,
    f_lc_histprod,
    f_lc_histexp,
    f_lc_lowexp,
    nrow = 2,
    rel_widths = c(0.2, 0.25, 0.25, 0.25),
    labels = c("(A)", "(C)", "(E)", "(G)", "(B)", "(D)", "(F)", "(H)"),
    label_size = 22
  )

  plots_legends <- plot_grid(
    legend_lines,
    legend_fuels_not_refinery,
    legend_fuels_refinery,
    legend_ghg,
    ncol = 4,
    rel_widths = c(0.3, 0.25, 0.25, 0.2),
    axis = "lc"
  )

  # Add white background to the entire legend area
  plots_legends <- ggdraw(plots_legends) +
    theme(plot.background = element_rect(fill = "white", color = "white"))
  # plots_legends

  plots_all <- plot_grid(
    plots_ts,
    plots_legends,
    nrow = 2,
    rel_heights = c(0.8, 0.2)
  )

  # Create fuel demand summary table for 2020-2045
  demand_summary <- dt_demand2[
    year >= 2020 & year <= 2045,
    .(consumption_bge = sum(consumption_bge, na.rm = TRUE)),
    by = .(scenario, year, fuel)
  ]

  # Calculate total demand by scenario and year for percentage calculations
  total_by_scenario_year <- demand_summary[,
    .(total_consumption = sum(consumption_bge, na.rm = TRUE)),
    by = .(scenario, year)
  ]

  # Merge and calculate percentages
  demand_summary <- demand_summary[
    total_by_scenario_year,
    on = .(scenario, year)
  ]
  demand_summary[, percentage := round(consumption_bge / total_consumption, 2)]
  demand_summary[, consumption_million_bge := round(consumption_bge / 1e6, 3)]

  # Create final formatted table
  demand_table <- demand_summary[, .(
    scenario,
    year,
    fuel = as.character(fuel),
    consumption_million_bge,
    percentage
  )]

  # Sort by scenario, year, and fuel
  setorder(demand_table, scenario, year, fuel)

  return(list(
    plot = plots_all,
    demand_table = demand_table
  ))
}

# plots_all
#
#
# ggsave(plots_all,
#   filename = file.path(
#     main_path,
#     "outputs/academic-out/refining/figures/2022-12-update",
#     "combined_its_and_production_2022.png"
#   ),
#   width = 25,
#   height = 13,
#   dpi = 600,
#   units = "in",
#   device = "png"
# )
#
# ggsave(plots_all,
#   filename = file.path(
#     main_path,
#     "outputs/academic-out/refining/figures/2022-12-update",
#     "combined_its_and_production_2022.pdf"
#   ),
#   width = 25,
#   height = 13,
#   dpi = 600,
#   units = "in"
# )
