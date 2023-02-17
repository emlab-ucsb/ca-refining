plot_its_demand <- function(dt_its_input, dt_intra_input, dt_jet_input) {
  
  dt_its = copy(dt_its_input)
  dt_intra = copy(dt_intra_input)
  dt_jet = copy(dt_jet_input)
  
  # calculate bge
  dt_jet[, consumption_bge := total_jet_fuel_demand_gge/42]
  dt_its[, consumption_bge := consumption_gge/42]
  dt_intra[, consumption_bge := consumption_gge/42]
  dt_intra[, j := 1]
  
  # unique set of scenarios
  dt_jet = dt_jet[scenario == 'Mid Case']
  un_scens = CJ(year = 2017:2050,
                scenario = c('Reference Demand', 'Low Carbon Demand'),
                j = 1)
  
  # create full set of intrastate jet fuel demand 
  dt_intra = dt_intra[un_scens, on = .(j, year), allow.cartesian = T]
  dt_intra[, j := NULL]
  dt_intra = dt_intra[year >= 2019]
  
  dt_intra_2 = dt_intra[, .(scenario, year, consumption_bge)]
  setnames(dt_intra_2, 'consumption_bge', 'intra_consumption_bge')
  setorder(dt_intra_2, scenario, year)
  
  # seperate interstate from intrastate demand
  dt_jet = dt_jet[, .(year, consumption_bge)]
  dt_jet[, fuel := 'jet (total)']
  dt_jet[, j := 1]
  
  dt_jet = dt_jet[un_scens, on = .(j, year), allow.cartesian = T]
  dt_jet[, j := NULL]
  dt_jet = dt_jet[year >= 2019 & year <= 2045]
  
  dt_jet_2 = dt_jet[, .(scenario, year, consumption_bge)]
  setnames(dt_jet_2, 'consumption_bge', 'total_jet_consumption_bge')
  setorder(dt_jet_2, scenario, year)
  
  dt_intra_2 = dt_intra_2[dt_jet_2, on = .(scenario, year)]
  dt_intra_2[, inter_consumption_bge := total_jet_consumption_bge - intra_consumption_bge]
  
  dt_intra_all = melt(dt_intra_2, 
                      id.vars = c('scenario', 'year'),
                      measure.vars = c('intra_consumption_bge', 'inter_consumption_bge'),
                      variable.name = 'fuel',
                      value.name = 'consumption_bge')
  dt_intra_all[fuel == 'intra_consumption_bge', fuel := 'jet fuel (intrastate)']
  dt_intra_all[fuel == 'inter_consumption_bge', fuel := 'jet fuel (interstate + military)']
  
  # combine demand ------
  
  dt_demand = rbindlist(list(dt_its[, .(scenario, year, fuel, consumption_bge)],
                             dt_intra_all[, .(scenario, year, fuel, consumption_bge)]))
  setorder(dt_demand, scenario, year, fuel)
  
  # rename fuel ------
  
  dt_demand[fuel == 'drop-in gasoline', fuel := 'renewable gasoline']
  dt_demand[, fuel := str_to_title(fuel)]
  dt_demand[, fuel := gsub('Ldv', 'LDV', fuel)]
  dt_demand[, fuel := gsub('Hdv', 'HDV', fuel)]
  
  # rename scenario -------
  dt_demand[scenario == 'BAU', scenario := 'Reference Demand']
  dt_demand[scenario == 'LC1', scenario := 'Low Carbon Demand']
  
  # reorder factor levels ------
  
  dt_demand[, fuel := factor(fuel, levels = rev(c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                                  'Jet Fuel (Intrastate)', 'Sustainable Aviation Fuel', 'Jet Fuel (Interstate + Military)',
                                                  'Ethanol', 'Biodiesel', 'Renewable Natural Gas', 'LDV Hydrogen', 'HDV Hydrogen', 'LDV Electricity', 'HDV Electricity')))]
  
  
  # get line of Total intrastate transportation liquid fuels demand included -------
  
  inc_its = dt_demand[fuel %in% c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                  'Jet Fuel (Intrastate)', 'Sustainable Aviation Fuel')]
  inc_its = inc_its[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(scenario, year)]
  inc_its[scenario == 'BAU', scenario := 'Reference Demand']
  inc_its[scenario == 'LC1', scenario := 'Low Carbon Demand']
  
  # get line of total fuels included (not exports) ---------
  
  inc_full = dt_demand[fuel %in% c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                   'Jet Fuel (Intrastate)', 'Sustainable Aviation Fuel', 'Jet Fuel (Interstate + Military)')]
  inc_full = inc_full[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(scenario, year)]
  inc_full[scenario == 'BAU', scenario := 'Reference Demand']
  inc_full[scenario == 'LC1', scenario := 'Low Carbon Demand']
  
  # refactor scenario -------
  
  dt_demand[, scenario := factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))]
  inc_its[, scenario := factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))]
  inc_full[, scenario := factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))]
  
  fig_demand = 
    ggplot() +
    geom_area(data = dt_demand, aes(x = year, y = consumption_bge/1e6, fill = fuel, group = fuel)) +
    geom_line(data = inc_its, aes(x = year, y = consumption_bge/1e6, lty = 'its'), linewidth = 1, color = 'black') +
    geom_line(data = inc_full, aes(x = year, y = consumption_bge/1e6, lty = 'all'), linewidth = 1, color = 'black') +
    facet_wrap(~ scenario, nrow = 2) +
    # facet_wrap(vars(scenario)) +
    # facet_grid(~factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))) +
    labs(title = NULL,
         subtitle = NULL,
         x = 'Year',
         y = 'Million barrels of gasoline equivalent demanded',
         fill = NULL) +
    scale_x_continuous(breaks = c(2020, seq(2025, 2045, 5)), limits = c(2020, 2045), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) +
    scale_fill_manual(values = pal_fuel, guide = guide_legend(nrow = 7)) + 
    scale_linetype_manual(name = NULL, 
                          labels = c('its' = 'Total intrastate transportation\nliquid fuels demand supplied by oil refineries', 
                                     'all' = 'Total transportation liquid fuels\ndemand including interstate and military aviation'),
                          values = c('its' = 3,
                                     'all' = 2),
                          guide = guide_legend(nrow = 2)) +
    theme_line 
  
  fig_demand
  
  
}








# ggsave(fig_demand,
#        filename = file.path(fig_path, 'its_demand_and_production_2023.png'),
#        width = 6.5,
#        height = 8,
#        dpi = 400, 
#        units = 'in', 
#        device = 'png')
# 
# ggsave(fig_demand,
#        filename = file.path(fig_path, 'its_demand_and_production_2023.pdf'),
#        width = 6.5,
#        height = 8,
#        dpi = 400, 
#        units = 'in', 
#        device = 'pdf')
# 
# embed_fonts(file.path(fig_path, 'its_demand_and_production_2023.pdf'),
#             outfile = file.path(fig_path, 'its_demand_and_production_2023.pdf'))
