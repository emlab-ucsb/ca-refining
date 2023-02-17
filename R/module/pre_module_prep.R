# ------------------------- historic data prep & analysis -------------------------

calculate_weekly_refined_products <- function(dt_fw) {
  
  # get historic fuel production data
  prod_refined = dt_fw[stock_flow == 'Refinery Production']
  unique(prod_refined[, .(category, sub_cat)])
  
  # recategorize fuels
  prod_refined[category == 'Motor Gasoline', fuel := 'gasoline']
  prod_refined[category == 'Distillates', fuel := 'diesel']
  prod_refined[category == 'Jet Fuel: Kerosene-Naphtha', fuel := 'jet']
  prod_refined[category == 'Residual', fuel := 'residual']
  
  # adjust production values to remove ethanol from gasoline
  prod_refined[, adj_thous_barrels := ifelse(category == 'Motor Gasoline' & sub_cat == 'Reformulated', 0.9 * thous_barrels, thous_barrels)]
  
  # aggregate new adjusted production by region, week, and fuel
  prod_refined_week = prod_refined[, .(fuel_prod_bbl = sum(adj_thous_barrels, na.rm = T) * 1e3),
                                   by = .(region, date, year, fuel)]
  prod_refined_week[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet', 'residual'))]
  prod_refined_week_wide = dcast(prod_refined_week, region + date + year ~ fuel, value.var = 'fuel_prod_bbl')
  
  prod_refined_week_wide
  
}

# calculate: weekly refined crude -----

calculate_weekly_refined_crude <- function(dt_fw, prod_refined_week_wide, ei_crude, ei_gasoline, ei_diesel, ei_jet) {
  
  # aggregate crude production by region and week
  crude_input_week = dt_fw[stock_flow == 'Refinery Input', .(crude_bbl = sum(thous_barrels, na.rm = T) * 1e3),
                             by = .(region, date, year)]

  # merge weekly crude input and refined product production
  crude_refined_week = crude_input_week[prod_refined_week_wide, on = .(region, date, year)]

  # convert to gge
  crude_refined_week[, crude_gge := crude_bbl * 42 * (ei_crude/ei_gasoline)]
  crude_refined_week[, gasoline_gge := gasoline * 42]
  crude_refined_week[, diesel_gge := diesel * 42 * (ei_diesel/ei_gasoline)]
  crude_refined_week[, jet_gge := jet * 42 * (ei_jet/ei_gasoline)]

  crude_refined_week
}

# # regress crude input as function of refined product input (in units of bbl) -----
# 
# reg_crude_refined = lm(crude_bbl ~ gasoline + diesel + jet, data = crude_refined_week)
# reg_crude_refined_gge = lm(crude_gge ~ gasoline_gge + diesel_gge + jet_gge, data = crude_refined_week)
# 

# calculate: annual refined crude ------

calculate_annual_refined_crude <- function(crude_refined_week) {
  
  crude_refined_annual = crude_refined_week[, lapply(.SD, sum, na.rm = T),
                                            by = c('region', 'year'),
                                            .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
  crude_refined_annual
  
}

# calculate: total refined crude ----

calculate_total_refined_crude <- function(crude_refined_week, ei_crude, ei_gasoline, ei_diesel, ei_jet) {
  crude_refined_tot = crude_refined_week[, lapply(.SD, sum, na.rm = T), 
                                         .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
  
  crude_refined_tot[, coef := ( (crude_bbl*ei_crude) - (gasoline*ei_gasoline) - (diesel*ei_diesel) - (jet*ei_jet)  )/crude_bbl]
  crude_refined_tot
}


# calculate: 5 year average of crude input and refined product production -------

calculate_ave_refined_crude <- function(crude_refined_annual, ei_crude, ei_gasoline, ei_diesel, ei_jet) {
  
  ave_crude_refined = crude_refined_annual[year %in% 2015:2019,
                                           lapply(.SD, mean, na.rm = T),
                                           by = 'region',
                                           .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]

  ave_crude_refined = melt(ave_crude_refined,
                           id.vars = 'region',
                           measure.vars = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual'),
                           variable.name = 'fuel',
                           value.name = 'ave_hist_bbl')

  # convert to gallons
  ave_crude_refined[, ave_hist_gal := ave_hist_bbl * 42]

  # convert to gge
  ave_crude_refined[fuel == 'crude_bbl', ave_hist_gge := ave_hist_gal * (ei_crude/ei_gasoline)]
  ave_crude_refined[fuel == 'gasoline', ave_hist_gge := ave_hist_gal]
  ave_crude_refined[fuel == 'diesel', ave_hist_gge := ave_hist_gal * (ei_diesel/ei_gasoline)]
  ave_crude_refined[fuel == 'jet', ave_hist_gge := ave_hist_gal * (ei_jet/ei_gasoline)]

  # convert to bge
  ave_crude_refined[, ave_hist_bge := ave_hist_gge / 42]

  setcolorder(ave_crude_refined, c('region', 'fuel', 'ave_hist_gge', 'ave_hist_bge', 'ave_hist_gal', 'ave_hist_bbl'))
  
  ave_crude_refined
  
}


# calculate: ratio of refined products in bge to crude input in bge -------

calcilate_refined_product_to_crude_ratio <- function(ave_crude_refined) {
  # ave_crude_refined_bge = dcast(ave_crude_refined, region ~ fuel, value.var = 'ave_hist_bge')
  ave_crude_refined_bge = ave_crude_refined[fuel %in% c('gasoline', 'diesel', 'jet'), .(region, fuel, ave_hist_bge)]
  setnames(ave_crude_refined_bge, 'ave_hist_bge', 'fuel_bge')
  ave_crude_refined_bge = ave_crude_refined_bge[ave_crude_refined[fuel == 'crude_bbl', .(region, ave_hist_bge)], on = 'region']
  setnames(ave_crude_refined_bge, 'ave_hist_bge', 'crude_bge')
  ave_crude_refined_bge[, bge_perc := fuel_bge/crude_bge]
  ave_crude_refined_bge = ave_crude_refined_bge[, .(region, fuel, bge_perc)]
  
  ave_crude_refined_bge
}


# calculate: CDU based on 5 year average of crude input -------

calculate_ave_cdu <- function(dt_refcap, ave_crude_refined) {
  
  region_capacity = dt_refcap[, .(barrels_per_day = sum(barrels_per_day)), by = .(region)]
  region_capacity[, barrels_per_year := barrels_per_day * 365]
  
  ave_region_cdu = region_capacity[ave_crude_refined[fuel == 'crude_bbl'], on = .(region)]
  ave_region_cdu[, ave_hist_cdu := ave_hist_bbl/barrels_per_year]
  
  ave_region_cdu
}



# calculate: ratio between north and south crude consumption from past 5 years --------
calculate_region_fuel_ratio <- function(crude_refined_annual) {
  

  region_fuel_ratio = crude_refined_annual[year %in% 2015:2019,
                                           lapply(.SD, sum, na.rm = T),
                                           by = 'region',
                                           .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
  region_fuel_ratio = melt(region_fuel_ratio,
                           id.vars = 'region',
                           measure.vars = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual'),
                           variable.name = 'fuel',
                           value.name = 'bbl')
  region_fuel_ratio[, ratio := bbl/sum(bbl), by = 'fuel']
  region_fuel_ratio[fuel == 'crude_bbl', fuel := 'crude']
  region_fuel_ratio[, years_used := '2015-2019']
  
  region_fuel_ratio[, years_used := NULL]
  
  region_fuel_ratio
}

# # calculate: 5 year average of refined product imports ------
# 

# calculate: 5 year average of net exports -------

calculate_annual_movements <- function(dt_fpm) {
  
  # get refined product exports
  refined_exports_month = dt_fpm[code %like% 'E$']
  refined_exports_month[, year := year(ymd(date))]
  refined_exports_month[location == 'north', region := 'North']
  refined_exports_month[location == 'south', region := 'South']
  
  # get refined product imports
  refined_imports_month = dt_fpm[code %like% 'I$']
  refined_imports_month[, year := year(ymd(date))]
  refined_imports_month[location == 'north', region := 'North']
  refined_imports_month[location == 'south', region := 'South']
  
  # aggregate to annual
  refined_exports_annual = refined_exports_month[, .(export_bbl = sum(thous_bbl*1e3, na.rm = T)), by = .(region, fuel, year)]
  refined_exports_annual[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet'))]
  
  refined_imports_annual = refined_imports_month[, .(import_bbl = sum(thous_bbl*1e3, na.rm = T)), by = .(region, fuel, year)]
  refined_imports_annual[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet'))]
  
  # refined_imports_annual_wide = dcast(refined_imports_annual, region + year ~ fuel, value.var = 'import_bbl')
  
  # ave_refined_imports = refined_imports_annual_wide[year %in% 2015:2019,
  #                                                   lapply(.SD, mean, na.rm = T),
  #                                                   by = 'region',
  #                                                   .SDcols = c('gasoline', 'diesel', 'jet')]
  # ave_refined_imports = melt(ave_refined_imports,
  #                            id.vars = 'region',
  #                            measure.vars = c('gasoline', 'diesel', 'jet'),
  #                            variable.name = 'fuel',
  #                            value.name = 'import_bbl')
  # ave_refined_imports[, import_gal := import_bbl * 42]
  # 
  # # convert to gge
  # ave_refined_imports[fuel == 'gasoline', import_gge := import_gal]
  # ave_refined_imports[fuel == 'diesel', import_gge := import_gal * (ei_diesel/ei_gasoline)]
  # ave_refined_imports[fuel == 'jet', import_gge := import_gal * (ei_jet/ei_gasoline)]
  # 
  # # convert to bge
  # ave_refined_imports[, import_bge := import_gge / 42]
  # setcolorder(ave_refined_imports, c('region', 'fuel', 'import_gge', 'import_bge', 'import_gal', 'import_bbl'))
  
  # combine imports with exports
  refined_movements_annual = refined_exports_annual[refined_imports_annual, on = .(region, fuel, year)]
  refined_movements_annual[, export_bbl := abs(export_bbl)]
  refined_movements_annual = refined_movements_annual[, lapply(.SD, sum, na.rm = T), by = .(fuel, year), .SDcols = c('export_bbl', 'import_bbl') ]
  refined_movements_annual[, net_export_bbl := export_bbl - import_bbl]
  
  refined_movements_annual
  
}

calculate_ave_refined_exports <- function(refined_movements_annual, region_fuel_ratio, ei_gasoline, ei_diesel, ei_jet) {

  ave_refined_exports = refined_movements_annual[year %in% 2015:2019, lapply(.SD, mean, na.rm = T), by = .(fuel), .SDcols = c('net_export_bbl')]
  
  ave_refined_exports = ave_refined_exports[region_fuel_ratio[, .(region, fuel, ratio)], on = .(fuel), nomatch = 0]
  ave_refined_exports[, export_bbl := net_export_bbl * ratio]
  ave_refined_exports[, net_export_bbl := NULL]
  ave_refined_exports[, ratio := NULL]
  
  ave_refined_exports[, export_gal := export_bbl * 42]
  
  # convert to gge
  ave_refined_exports[fuel == 'gasoline', export_gge := export_gal]
  ave_refined_exports[fuel == 'diesel', export_gge := export_gal * (ei_diesel/ei_gasoline)]
  ave_refined_exports[fuel == 'jet', export_gge := export_gal * (ei_jet/ei_gasoline)]
  
  # convert to bge
  ave_refined_exports[, export_bge := export_gge / 42]
  
  setcolorder(ave_refined_exports, c('region', 'fuel', 'export_gge', 'export_bge', 'export_gal', 'export_bbl'))
  
  ave_refined_exports
  
}


# calculate: ratio of refinery capacity within respective region ----------

calculate_ref_region_ratio <- function(dt_refcap, crude_refined_week, ei_crude, ei_gasoline, ei_diesel, ei_jet) {
  refinery_capacity_ratio = dt_refcap[, .(site_id, refinery_name, barrels_per_day, location, region)]
  refinery_capacity_ratio[, barrels_per_year := barrels_per_day * 365]
  refinery_capacity_ratio[, capacity_ratio := barrels_per_year/sum(barrels_per_year), by = .(region)]
  sum(refinery_capacity_ratio[region == 'South', capacity_ratio])
  setcolorder(refinery_capacity_ratio, c('site_id', 'refinery_name', 'barrels_per_day', 'barrels_per_year', 'location', 'region', 'capacity_ratio'))
  
  refinery_capacity_ratio[, capacity_ratio_within_region := capacity_ratio]
  refinery_capacity_ratio[, capacity_ratio_within_state := barrels_per_year/sum(barrels_per_year)]
  
  
  # use heat content balance to get historical relationship between crude and refined products
  crude_refined_region = crude_refined_week[, lapply(.SD, sum, na.rm = T),
                                            by = c('region'),
                                            .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
  
  crude_refined_region[, coef := ( (crude_bbl*ei_crude) - (gasoline*ei_gasoline) - (diesel*ei_diesel) - (jet*ei_jet)  )/crude_bbl]
  
  crude_refined_region
}


# crude_refined_region_csv = copy(crude_refined_region)
# setnames(crude_refined_region_csv, 'gasoline', 'gasoline_bbl')
# setnames(crude_refined_region_csv, 'diesel', 'diesel_bbl')
# setnames(crude_refined_region_csv, 'jet', 'jet_bbl')
# crude_refined_region_csv[, residual := NULL]
# crude_refined_region_csv[, ei_crude_mmbtu_bbl := ei_crude]
# crude_refined_region_csv[, ei_gasoline_mmbtu_bbl := ei_gasoline]
# crude_refined_region_csv[, ei_diesel_mmbtu_bbl := ei_diesel]
# crude_refined_region_csv[, ei_jet_mmbtu_bbl := ei_jet]
# 
# setcolorder(crude_refined_region_csv, c('region', 'crude_bbl', 'gasoline_bbl', 'diesel_bbl', 'jet_bbl', 
#                                         'ei_crude_mmbtu_bbl', 'ei_gasoline_mmbtu_bbl', 'ei_diesel_mmbtu_bbl', 'ei_jet_mmbtu_bbl', 'coef'))
# 
# crude_refined_tot = crude_refined_week[, lapply(.SD, sum, na.rm = T), 
#                                        .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
# 
# crude_refined_tot[, coef := ( (crude_bbl*ei_crude) - (gasoline*ei_gasoline) - (diesel*ei_diesel) - (jet*ei_jet)  )/crude_bbl]
# 
# 
# # compare estimated crude to reported crude consumption -------
# 
# compare_hist_crude = crude_refined_annual[crude_refined_region[, .(region, coef)], on = .(region)]
# 
# compare_hist_crude[, est_crude_bbl := ((gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
# compare_hist_crude_long = melt(compare_hist_crude, 
#                                id.vars = 1:2,
#                                measure.vars = c('crude_bbl', 'est_crude_bbl'),
#                                variable.name = 'type',
#                                value.name = 'bbl')
# compare_hist_crude_long[, type := ifelse(type == 'crude_bbl', 'reported', 'estimated')]
# 
# compare_hist_crude_long_tot = compare_hist_crude_long[, .(bbl = sum(bbl, na.rm = T)), by = .(year, type)]
# 
# pal_compare = c('reported' = '#46726f',
#                 'estimated' = '#e5938c')
# 
# 

# get amount of REdiesel produced by Kern Oil refinery -----
calculate_ave_kern_rediesel <- function(dt_rediesel, ei_gasoline, ei_diesel, kern_perc) {
  
  # calculate 5 year average of RE diesel consumed in CA
  ave_kern_rediesel = dt_rediesel[year %in% 2015:2019, lapply(.SD, mean, na.rm = T), by = .(fuel), .SDcols = c('consumption_gal')]
  ave_kern_rediesel[, consumption_bbl := consumption_gal / 42]
  ave_kern_rediesel[, consumption_gge := consumption_gal * (ei_diesel/ei_gasoline)]
  ave_kern_rediesel[, consumption_bge := consumption_gge / 42]
  
  # multiply average annual REdiesel consumption with percent that can be processed at Kern Oil
  cols = c('consumption_gge', 'consumption_bge', 'consumption_gal', 'consumption_bbl')
  ave_kern_rediesel[, (cols) := lapply(.SD, function(x) x*kern_perc), .SDcols = cols]
  
  ave_kern_rediesel
  
}


# ------------------------- forecasted demand prep & analysis -------------------------

state_fuel_demand_df <- function(dt_its, 
                                 dt_jet,
                                 ei_gasoline,
                                 ei_diesel,
                                 ei_jet) {
  
  # change jet fuel demand scenario names
  
  jet_mid = dt_jet[scenario == 'Mid Case', .(year, scenario, total_jet_fuel_demand_gge)]
  dt_jet_adj = rbindlist(list(copy(jet_mid)[, adj_scenario := 'BAU'],
                              copy(jet_mid)[, adj_scenario := 'LC1']))
  dt_jet_adj[, scenario := NULL]
  dt_jet_adj[, fuel := 'jet']
  setnames(dt_jet_adj, 'adj_scenario', 'scenario')
  setnames(dt_jet_adj, 'total_jet_fuel_demand_gge', 'consumption_gge')

  # combine its, jet fuel, and renewable fuels forecasts 
  
  demand_state = rbindlist(list(dt_its, dt_jet_adj), use.names = T)
  demand_state[, fuel := factor(fuel, c('gasoline', 'diesel', 'jet',
                                        'drop-in gasoline', 'renewable diesel', 'sustainable aviation fuel', 'renewable natural gas',
                                        'ethanol', 'biodiesel', 'hdv electricity', 'hdv hydrogen', 'ldv electricity', 'ldv hydrogen'))]
  setorder(demand_state, scenario, fuel, year)
  setcolorder(demand_state, c('scenario', 'fuel', 'year', 'consumption_gge'))
  
  # keep only gasoline, diesel, and jet fuel demand
  
  demand_state = demand_state[fuel %in% c('gasoline', 'diesel', 'jet', 'drop-in gasoline', 'renewable diesel', 'sustainable aviation fuel')]
  demand_state[, consumption_bge := consumption_gge / 42]
  
  # convert units of demand from gge to bbls
  
  demand_state[fuel %like% 'gasoline', consumption_gal := consumption_gge]
  demand_state[fuel %like% 'diesel', consumption_gal := consumption_gge * (ei_gasoline/ei_diesel)]
  demand_state[fuel %like% 'jet' | fuel %like% 'aviation', consumption_gal := consumption_gge * (ei_gasoline/ei_jet)]
  demand_state[, consumption_bbl := consumption_gal/42]
  
  demand_state = demand_state[year >= 2020 & year <= 2045]
  
  # create eqivalent fuel column
  
  demand_state[fuel %like% 'gasoline', fuel_equiv := 'gasoline']
  demand_state[fuel %like% 'diesel', fuel_equiv := 'diesel']
  demand_state[fuel %like% 'jet' | fuel %like% 'aviation', fuel_equiv := 'jet']
  
  demand_state
  
}


# # split (GJD only) demand by north and south regions using ratio --------
# 
# demand_region_gjd = demand_state[region_fuel_ratio[, .(fuel, region, ratio)], on = 'fuel', nomatch = 0, allow.cartesian = T]
# demand_region_gjd[, region_consumption_gge := consumption_gge * ratio]
# demand_region_gjd[, region_consumption_bge := consumption_bge * ratio]
# demand_region_gjd[, region_consumption_gal := consumption_gal * ratio]
# demand_region_gjd[, region_consumption_bbl := consumption_bbl * ratio]
# 
# # calculate forecasted crude from GJD forecast (excl. imports) using the two methods -------
# 
# demand_region_gjd_wide = dcast(demand_region_gjd, scenario + region + year ~ fuel, value.var = 'region_consumption_bbl')
# demand_region_gjd_wide = demand_region_gjd_wide[crude_refined_region[, .(region, coef)], on = .(region)]
# 
# demand_region_gjd_wide[, heat_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
# 
# demand_region_gjd_wide[region == 'North', 
#                        regression_crude_demand_bbl := predict(lm(crude_bbl ~ gasoline + diesel + jet, data = crude_refined_annual[year < 2020 & region == 'North']), 
#                                                               demand_region_gjd_wide[region == 'North'])]
# demand_region_gjd_wide[region == 'South', 
#                        regression_crude_demand_bbl := predict(lm(crude_bbl ~ gasoline + diesel + jet, data = crude_refined_annual[year < 2020 & region == 'South']), 
#                                                               demand_region_gjd_wide[region == 'South'])]
# 
# crude_demand_compare = melt(demand_region_gjd_wide, 
#                             id.vars = c('scenario', 'region', 'year'), 
#                             measure.vars = c('gasoline', 'diesel', 'jet', 'heat_crude_demand_bbl', 'regression_crude_demand_bbl'),
#                             variable.name = 'fuel',
#                             value.name = 'consumption_bbl')
# 
# crude_demand_compare[fuel == 'heat_crude_demand_bbl', fuel := 'crude (energy intensity equation)']
# crude_demand_compare[fuel == 'regression_crude_demand_bbl', fuel := 'crude (regression)']
# 
# # convert to gge
# crude_demand_compare[fuel == 'gasoline', consumption_gge := consumption_bbl * 42]
# crude_demand_compare[fuel == 'diesel', consumption_gge := consumption_bbl * 42 * (ei_diesel/ei_gasoline)]
# crude_demand_compare[fuel == 'jet', consumption_gge := consumption_bbl * 42 * (ei_jet/ei_gasoline)]
# crude_demand_compare[fuel %like% 'crude', consumption_gge := consumption_bbl * 42 * (ei_crude/ei_gasoline)]
# 
# 
# # get annual historic crude and refined products in long form -------
# 
# crude_refined_annual_long = melt(crude_refined_annual, 
#                                  id.vars = c('region', 'year'), 
#                                  measure.vars = c('crude_bbl', 'gasoline', 'diesel', 'jet'),
#                                  variable.name = 'fuel',
#                                  value.name = 'consumption_bbl')
# crude_refined_annual_long[, scenario := 'historic']
# crude_refined_annual_long = crude_refined_annual_long[year < 2020]
# crude_refined_annual_long[fuel == 'crude_bbl', fuel := 'crude']
# 
# # convert to gge
# crude_refined_annual_long[fuel == 'gasoline', consumption_gge := consumption_bbl * 42]
# crude_refined_annual_long[fuel == 'diesel', consumption_gge := consumption_bbl * 42 * (ei_diesel/ei_gasoline)]
# crude_refined_annual_long[fuel == 'jet', consumption_gge := consumption_bbl * 42 * (ei_jet/ei_gasoline)]
# crude_refined_annual_long[fuel %like% 'crude', consumption_gge := consumption_bbl * 42 * (ei_crude/ei_gasoline)]
# 
# # combine historic and forecasted crude + GJD ------
# 
# compare_fuels = rbindlist(list(crude_refined_annual_long, crude_demand_compare), use.names = T)
# setcolorder(compare_fuels, c('scenario', 'region', 'fuel', 'year', 'consumption_bbl', 'consumption_gge'))
# 
# # combine GJD into one value ------
# 
# compare_fuels[, fuel := as.character(fuel)]
# compare_fuels[, adj_fuel := ifelse(fuel %like% 'crude', fuel, 'GJD')] 
# 
# compare_fuels_agg = compare_fuels[, .(consumption_bbl = sum(consumption_bbl, na.rm = T),
#                                       consumption_gge = sum(consumption_gge, na.rm = T)),
#                                   by = .(scenario, region, adj_fuel, year)]
# 

# create time series of exports from 2019 levels to zero in 2045 ------

create_time_series_exports <- function(refined_movements_annual, region_fuel_ratio, ei_crude, ei_gasoline, ei_diesel, ei_jet) {
  
  # create empty data table
  ts_exports = CJ(year = 2019:2045,
                  fuel = c('gasoline', 'diesel', 'jet'))
  
  # merge with 2019 exports
  ts_exports = refined_movements_annual[year == 2019][ts_exports, on = .(year, fuel)]
  
  # set 2045 exports to zero
  ts_exports[year == 2045, net_export_bbl := 0]
  
  # linearly interpolate within each group
  ts_exports_wide = dcast(ts_exports, year ~ fuel, value.var = 'net_export_bbl')
  setorder(ts_exports_wide, year)
  setcolorder(ts_exports_wide, c('year', 'gasoline', 'diesel', 'jet'))
  
  ts_exports_wide[, gasoline := ((0 - gasoline[year == 2019])/(2045-2019))*(year - 2045)]
  ts_exports_wide[, diesel := ((0 - diesel[year == 2019])/(2045-2019))*(year - 2045)]
  ts_exports_wide[, jet := ((0 - jet[year == 2019])/(2045-2019))*(year - 2045)]
  
  # get long form
  ts_exports = melt(ts_exports_wide, id.vars = c('year'), measure.vars = c('gasoline', 'diesel', 'jet'),
                    variable.name = 'fuel', value.name = 'export_bbl')
  
  # split by region
  ts_exports = ts_exports[region_fuel_ratio[, .(region, fuel, ratio)], on = .(fuel), nomatch = 0, allow.cartesian = T]
  setorder(ts_exports, fuel, year, region)
  ts_exports[, export_bbl_2 := export_bbl * ratio]
  ts_exports[, export_bbl := NULL]
  ts_exports[, ratio := NULL]
  setnames(ts_exports, 'export_bbl_2', 'export_bbl')
  
  # convert to bge
  ts_exports[fuel == 'gasoline', export_bge := export_bbl]
  ts_exports[fuel == 'diesel', export_bge := export_bbl * (ei_diesel/ei_gasoline)]
  ts_exports[fuel == 'jet', export_bge := export_bbl * (ei_jet/ei_gasoline)]
  
  ts_exports
  
}


