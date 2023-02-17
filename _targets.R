# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(data.table)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(hrbrthemes)
library(directlabels)
library(grid)
library(extrafont)
library(stringr) 
# library(unikn)

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
source("plot_settings.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # set main path
  tar_target(name = main_path, command = "/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn"),
  
  # module settings
  tar_target(name = ref_threshold, command = 0.6),
  tar_target(name = ren_threshold, command = 0.9),
  tar_target(name = pred_years, command = 2020:2045),
  tar_target(name = drop_in_perc, command = 1),
  tar_target(name = kern_perc, command = 0.9375),
  # tar_target(name = a, command = 4),
  # tar_target(name = ccs_capture_rate, command = 0.474),
  
  # energy intensities
  tar_target(name = ei_crude, command = 5.698), # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_3.pdf
  tar_target(name = ei_gasoline, command = 5.052), # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_4.pdf
  tar_target(name = ei_diesel, command = 5.770), # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf
  tar_target(name = ei_jet, command = (5.670 + 5.355)/2), # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf
  
  # scenarios and regions
  tar_target(name = dem_scens, command = c('BAU', 'LC1')),
  tar_target(name = ref_scens, command = c('historic exports', 'historic production', 'low exports')),
  tar_target(name = clus, command = c('North', 'South')),
  
  # set raw data paths
  tar_target(name = file_raw_its, command = file.path(main_path, "data/stocks-flows/raw/Study 1 - Preliminary Fuel Volumes BAU & LC1.xlsx"), format = "file"),
  tar_target(name = file_raw_avgas, command = file.path(main_path, "data/stocks-flows/raw/Distillates 10-10.xlsx"), format = "file"),
  
  # read in raw data files
  tar_target(name = raw_its_bau, command = read_raw_its_data(file_raw_its, input_sheet = "Sheet1", input_rows = c(1, 7:19), input_cols = c(2:37))),
  tar_target(name = raw_its_lc1, command = read_raw_its_data(file_raw_its, input_sheet = "Sheet1", input_rows = c(1, 23:34), input_cols = c(2:37))),
  tar_target(name = raw_avgas, command = simple_read_xlsx(file_raw_avgas, input_sheet = "Sheet1", input_rows = c(4, 16), input_cols = c(3:38))),
  tar_target(name = raw_intra_jet, command = simple_read_xlsx(file_raw_avgas, input_sheet = "Sheet1", input_rows = c(4, 14), input_cols = c(3:38))),
  
  # create processed data
  tar_target(name = dt_its, command = get_its_forecast(raw_its_bau, raw_its_lc1, raw_avgas)),
  tar_target(name = dt_intra, command = get_intrastate_jet_forecast(raw_intra_jet)),

  # set remaining file paths
  tar_target(name = file_scen, command = file.path(main_path, "project-materials/scenario-inputs/refinery_scenario_inputs.csv"), format = "file"),
  # tar_target(name = file_its, command = file.path(main_path, "outputs/fuel-demand/prelim-results/its_demand_bau_and_lc1_2020_2045.csv"), format = "file"),
  # tar_target(name = file_intra, command = file.path(main_path, "outputs/fuel-demand/prelim-results/its_demand_intrastate_jet_2020_2045.csv"), format = "file"),
  tar_target(name = file_jet, command = file.path(main_path, "outputs/fuel-demand/prelim-results/cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv"), format = "file"),
  tar_target(name = file_fpm, command = file.path(main_path, "data/stocks-flows/processed/finished_product_movements_weekly_cec.csv"), format = "file"),
  tar_target(name = file_fw, command = file.path(main_path, "data/stocks-flows/processed/fuel_watch_data.csv"), format = "file"),
  tar_target(name = file_ei, command = file.path(main_path, "data/stocks-flows/processed/fuel-energy-intensities.csv"), format = "file"),
  tar_target(name = file_dac, command = file.path(main_path, "model-development/scenario-plot/refinery_weighted_unweighted_population_fixed.csv"), format = "file"),
  tar_target(name = file_refcap, command = file.path(main_path, "data/stocks-flows/processed/refinery_loc_cap_manual.csv"), format = "file"),
  tar_target(name = file_ghgfac, command = file.path(main_path, "outputs/stocks-flows/refinery_ghg_factor_x_indiv_refinery_revised.csv"), format = "file"),
  tar_target(name = file_rediesel, command = file.path(main_path, "data/stocks-flows/processed/CARB_RE_fuels_CA_imports_figure10_053120.xlsx"), format = "file"),
  tar_target(name = file_renref, command = file.path(main_path, "data/stocks-flows/processed/renewable_refinery_capacity.xlsx"), format = "file"),
  tar_target(name = file_altair, command = file.path(main_path, "data/stocks-flows/raw/altair_refinery_capacity.xlsx"), format = "file"),
  
  # read in processed data files
  tar_target(name = dt_scen, command = simple_fread(file_scen)),
  # tar_target(name = dt_its, command = simple_fread(file_its)),
  # tar_target(name = dt_intra, command = simple_fread(file_intra)),
  tar_target(name = dt_jet, command = simple_fread(file_jet)),
  tar_target(name = dt_fpm, command = simple_fread(file_fpm)),
  tar_target(name = dt_fw, command = simple_fread(file_fw)),
  tar_target(name = dt_ei, command = simple_fread(file_ei)),
  tar_target(name = dt_dac, command = simple_fread(file_dac)),
  tar_target(name = dt_refcap, command = read_refcap_data(file_refcap)),
  tar_target(name = dt_ghgfac, command = read_ref_ghg_data(file_ghgfac, 2018)),
  tar_target(name = dt_rediesel, command = read_rediesel_data(file_rediesel, "Fig10", input_rows = c(2, 15), input_cols = seq(1, 10))),
  tar_target(name = dt_renref, command = simple_read_xlsx(file_renref, "Sheet1")[, .(installation_year, refinery_name, installation_capacity_bpd, retired_capacity_bpd)]),
  tar_target(name = renewables_info, command = simple_read_xlsx(file_renref, "Sheet1")[, .(site_id, refinery_name, location, region, cluster)]),
  tar_target(name = dt_altair, command = read_altair_data(file_altair, "Sheet1", ei_crude, ei_gasoline)),
  
  
  # prep for module
  tar_target(name = prod_refined_week_wide, command = calculate_weekly_refined_products(dt_fw)), 
  tar_target(name = crude_refined_week, command = calculate_weekly_refined_crude(dt_fw, prod_refined_week_wide, ei_crude, ei_gasoline, ei_diesel, ei_jet)), 
  tar_target(name = crude_refined_annual, command = calculate_annual_refined_crude(crude_refined_week)), 
  tar_target(name = crude_refined_tot, command = calculate_total_refined_crude(crude_refined_week, ei_crude, ei_gasoline, ei_diesel, ei_jet)), 
  tar_target(name = ave_crude_refined, command = calculate_ave_refined_crude(crude_refined_annual, ei_crude, ei_gasoline, ei_diesel, ei_jet)), 
  tar_target(name = ave_crude_refined_bge, command = calcilate_refined_product_to_crude_ratio(ave_crude_refined)), 
  tar_target(name = ave_region_cdu, command = calculate_ave_cdu(dt_refcap, ave_crude_refined)), 
  tar_target(name = region_fuel_ratio, command = calculate_region_fuel_ratio(crude_refined_annual)), 
  tar_target(name = refined_movements_annual, command = calculate_annual_movements(dt_fpm)), 
  tar_target(name = ave_refined_exports, command = calculate_ave_refined_exports(refined_movements_annual, region_fuel_ratio, ei_gasoline, ei_diesel, ei_jet)), 
  tar_target(name = crude_refined_region, command = calculate_ref_region_ratio(dt_refcap, crude_refined_week, ei_crude, ei_gasoline, ei_diesel, ei_jet)), 
  tar_target(name = ave_kern_rediesel, command = calculate_ave_kern_rediesel(dt_rediesel, ei_gasoline, ei_diesel, kern_perc)), 
  tar_target(name = demand_state, command = state_fuel_demand_df(dt_its, dt_jet, ei_gasoline, ei_diesel, ei_jet)), 
  tar_target(name = ts_exports, command = create_time_series_exports(refined_movements_annual, region_fuel_ratio, ei_crude, ei_gasoline, ei_diesel, ei_jet)), 
  
  # refining module
  tar_target(name = refining_module_outputs, command = main_refining_module(dem_scens, 
                                                                            ref_scens, 
                                                                            clus,
                                                                            pred_years,
                                                                            ref_threshold, 
                                                                            ren_threshold,
                                                                            drop_in_perc,
                                                                            dt_refcap, 
                                                                            dt_renref, 
                                                                            renewables_info, 
                                                                            ave_crude_refined, 
                                                                            ave_crude_refined_bge, 
                                                                            ave_region_cdu, 
                                                                            region_fuel_ratio,
                                                                            ave_refined_exports, 
                                                                            crude_refined_region,
                                                                            ave_kern_rediesel,
                                                                            demand_state,
                                                                            ts_exports, 
                                                                            ei_crude,
                                                                            ei_gasoline,
                                                                            ei_diesel,
                                                                            ei_jet)),
  tar_target(name = res_equiv_demand, command = refining_module_outputs[[1]]), 
  tar_target(name = res_renew_demand, command = refining_module_outputs[[2]]), 
  tar_target(name = res_final_cdu, command = refining_module_outputs[[3]]), 
  tar_target(name = res_crude_ref_reg, command = refining_module_outputs[[4]]), 
  tar_target(name = res_renew_ref_reg, command = refining_module_outputs[[5]]),
  
  # post-module processing
  tar_target(name = altair_ref, command = create_altair_ts(dt_altair, dem_scens, ref_scens)),
  tar_target(name = res_renew_ref_reg_altair, command = combine_renewables_outputs_with_altair(altair_ref, res_renew_ref_reg)),
  tar_target(name = renewables_info_altair, command = combine_renewables_info_with_altair(altair_ref, renewables_info)),
  tar_target(name = res_crude_ref_reg_capacity, command = calculate_crude_capacity_ratio(res_crude_ref_reg)),
  tar_target(name = res_renew_ref_reg_capacity, command = calculate_renewable_capacity_ratio(res_renew_ref_reg_altair)),
  
  tar_target(name = ref_crude_gjd, command = divide_gjd_demand_crude_refineries(res_equiv_demand, res_crude_ref_reg_capacity, crude_refined_region,
                                                                                ei_crude, ei_gasoline, ei_diesel, ei_jet)),
  tar_target(name = ref_crude_res_regjd, command = divide_residual_gjd_crude_refineries(res_equiv_demand, res_crude_ref_reg_capacity, crude_refined_region, 
                                                                                        ei_crude, ei_gasoline, ei_diesel, ei_jet, dem_scens, ref_scens, ave_kern_rediesel)),
  tar_target(name = ref_renew_gjd, command = divide_residual_gjd_renewable_refineries(res_renew_demand, res_renew_ref_reg_capacity, renewables_info_altair, crude_refined_tot,
                                                                                      ei_crude, ei_gasoline, ei_diesel, ei_jet)),
  tar_target(name = ref_cons_prod, command = combine_refinery_prod_cons(ref_crude_gjd, ref_crude_res_regjd, ref_renew_gjd, dt_ghgfac)), 
  
  # individual refinery level production
  tar_target(name = indiv_prod, command = gather_refinery_production(ref_cons_prod, ei_crude, ei_gasoline, ei_diesel, ei_jet)),
  tar_target(name = indiv_prod_output, command = gather_refinery_production_output(indiv_prod)),
  tar_target(name = indiv_prod_output_bge, command = gather_refinery_production_output_bge(indiv_prod)),
  
  # individual refinery level crude consumption
  tar_target(name = indiv_cons, command = gather_refinery_crude_consumption(ref_cons_prod, ei_crude, ei_gasoline, ei_diesel, ei_jet)),
  tar_target(name = indiv_cons_output, command = gather_refinery_crude_consumption_output(indiv_cons)),
  tar_target(name = indiv_cons_output_bge, command = gather_refinery_crude_consumption_output_bge(indiv_cons)),
  
  # individual refinery ghg emissions
  tar_target(name = indiv_ghg, command = gather_refinery_ghg(ref_cons_prod, indiv_cons)),
  tar_target(name = indiv_ghg_output, command = gather_refinery_ghg_output(indiv_ghg)),
  
  # cluster level outputs
  tar_target(name = clus_prod_output, command = gather_cluster_prod_output(indiv_prod_output)),
  tar_target(name = clus_cons_output, command = gather_cluster_cons_output(indiv_cons_output)),
  tar_target(name = clus_ghg_output, command = gather_cluster_cons_output(indiv_ghg_output)),

  # state level outputs
  tar_target(name = state_prod_output, command = gather_state_prod_output(indiv_prod_output)),
  tar_target(name = state_cons_output, command = gather_state_cons_output(indiv_cons_output)),
  tar_target(name = state_ghg_output, command = gather_state_cons_output(indiv_ghg_output)),
  tar_target(name = tot_fuel_demand_exports, command = combine_state_gjd_demand_and_exports(crude_refined_week, refined_movements_annual, dt_rediesel,
                                                                                            res_equiv_demand, res_renew_demand,
                                                                                            dem_scens, ref_scens, ei_crude, ei_gasoline, ei_diesel, ei_jet)),
  
  # paper figures
  tar_target(name = fig_demand, command = plot_its_demand(dt_its, dt_intra, dt_jet)), 
  tar_target(name = fig_refined_production_ghg, command = plot_refined_products_and_ghg(tot_fuel_demand_exports, state_ghg_output))
  

)
