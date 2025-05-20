# Load packages required to define the pipeline:
library(targets)
library(data.table)
library(tidyr)
library(tidyverse)
library(stringr)
library(openxlsx)
library(readxl)
library(lubridate)
library(lspline)
library(ggplot2)
library(hrbrthemes)
library(directlabels)
library(grid)
library(extrafont)
library(dplyr)
library(sf)
library(janitor)
library(readr)
library(fuzzyjoin)
library(sf)
library(cowplot)
library(tigris)
library(scales)
library(maps)

# font_import()
# loadfonts(device = "win")

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
source("extras/plot_settings.R")

# Replace the target list below with your own:
list(
  # set user
  tar_target(name = user, "vincent"), # choose: tracey, vincent, meas (add users and paths as needed)

  # list paths
  tar_target(name = list_paths, c(
    "tracey-laptop" = "/Users/traceymangin/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/",
    "tracey-desktop" = "/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/",
    "vincent" = "H://Shared drives/emlab/projects/current-projects/calepa-cn",
    "meas" = "/Users/meas/Library/CloudStorage/GoogleDrive-mmeng@ucsb.edu/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn"
  )),

  # set main path
  tar_target(
    name = main_path,
    command = list_paths[user]
  ),

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
  tar_target(name = ei_jet, command = (5.670 + 5.355) / 2), # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf
  tar_target(name = gge_to_bbls, command = 42),

  # labor analysis parameters
  tar_target(name = alpha_comp, command = 0.2), # #0-1 representing the share of each worker’s compensation that they lose when moving to a new job.
  tar_target(name = alpha_emp, command = 0), # #0-1 representing the share of jobs lost over time when losing a job in refining sector


  # health analysis parameters
  # tar_target(name = beta, command = 0.00737932), #Coefficient (high)
  # tar_target(name = beta, command = 0.00422068), #Coefficient (low)
  tar_target(name = beta, command = 0.00582), # Coefficient from Krewski et al (2009) for mortality impact
  tar_target(name = se, command = 0.0009628), # Coefficient from Krewski et al (2009) for mortality impact
  tar_target(name = vsl_2015, command = 8705114.25462459),
  #tar_target(name = vsl_2019, command = vsl_2015 * 107.8645906 / 100), # (https://fred.stlouisfed.org/series/CPALTT01USA661S)
  tar_target(name = vsl_2019, command = 14512127), # see VSL_9719 in age_VSL.R
  tar_target(name = income_elasticity_mort, command = 0.4),
  tar_target(name = discount_rate, command = 0.03),
  tar_target(name = buff_sites, command = c(97, 119, 164, 202, 209, 226, 271, 279, 332, 342, 343, 800, 3422, 34222, 99999)),

  # # emission factors
  # tar_target(name = ef_nh3, command = 0.00056),
  # tar_target(name = ef_nox, command = 0.01495),
  # tar_target(name = ef_pm25, command = 0.00402),
  # tar_target(name = ef_sox, command = 0.00851),
  # tar_target(name = ef_voc, command = 0.01247),

  ## CPI values
  # (https://fred.stlouisfed.org/series/CPALTT01USA661S)
  tar_target(name = cpi2020, command = 109.1951913),
  tar_target(name = cpi2019, command = 107.8645906),
  tar_target(name = cpi2015, command = 100),

  # scenarios and regions
  tar_target(name = dem_scens, command = c("BAU", "LC1")),
  tar_target(name = ref_scens, command = c("historic exports", "historic production", "low exports")),
  tar_target(name = clus, command = c("North", "South")),

  # crs
  tar_target(name = ca_crs, command = 3310), ## crs NAD83 / California Albers

  # set raw data paths
  tar_target(name = file_raw_its, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/raw/Study 1 - Preliminary Fuel Volumes BAU & LC1.xlsx"), format = "file"),
  tar_target(name = file_raw_avgas, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/raw/Distillates 10-10.xlsx"), format = "file"),
  tar_target(name = file_raw_cec_jet, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/raw/5-20 Jet Fuel Demand.xlsx"), format = "file"),
  tar_target(name = file_raw_mil_jet, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/raw/California Transportion Fuel Consumption - Summary 2020-06-01 GDS_rename.xlsx"), format = "file"),
  tar_target(name = file_raw_fpm, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/raw/Finished_Products_Movements.xlsx"), format = "file"),
  tar_target(name = file_refcap, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/processed/refinery_loc_cap_manual.csv"), format = "file"), # this is a manually created file
  tar_target(name = file_rediesel, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/processed/CARB_RE_fuels_CA_imports_figure10_053120.xlsx"), format = "file"),
  tar_target(name = file_renref, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/processed/renewable_refinery_capacity.xlsx"), format = "file"), # this is a manually created file
  tar_target(name = file_altair, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/raw/altair_refinery_capacity.xlsx"), format = "file"), # this is a manually created file

  tar_target(name = file_raw_ces, command = file.path(main_path, "data-staged-for-deletion/health/raw/ces3results.xlsx"), format = "file"),
  tar_target(name = file_raw_dac, command = file.path(main_path, "data-staged-for-deletion/health/raw/SB535DACresultsdatadictionary_F_2022/SB535DACresultsdatadictionary_F_2022.xlsx"), format = "file"),
  tar_target(name = file_raw_income_house, command = file.path(main_path, "data-staged-for-deletion/Census/ca-median-house-income.csv"), format = "file"), # remove from workflow
  tar_target(name = file_raw_income_county, command = file.path(main_path, "data-staged-for-deletion/Census/ca-median-house-income-county.csv"), format = "file"), # remove from workflow
  tar_target(name = file_inmap_re, command = file.path(main_path, "data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining")), # these were created upstream
  tar_target(name = file_dt_ef, command = file.path(main_path, "data-staged-for-deletion/health/processed/ref_emission_factor.csv"), format = "file"), #cluster-level emission factors
  tar_target(name = file_dt_ef_ref, command = file.path(main_path, "data-staged-for-deletion/health/processed/refinery_emission_factor.csv"), format = "file"), #refinery-level emission factors
  tar_target(name = file_dt_age_vsl, command = file.path(main_path, "data-staged-for-deletion/health/processed/age_based_VSL_2019.csv"), format = "file"), 
  tar_target(name = file_dt_ct_inc_pop, command = file.path(main_path, "data-staged-for-deletion/health/processed/ct_inc_45_2020.csv"), format = "file"),
  tar_target(name = file_dt_growth_cap_rate, command = file.path(main_path, "data-staged-for-deletion/benmap/processed/growth_per_cap.csv"), format = "file"),
  tar_target(name = file_dt_health_income, command = file.path(main_path, "outputs-staged-for-deletion/refining-2023/health/refining_health_income_2023.csv"), format = "file"),
  tar_target(name = file_raw_ct_2019, command = file.path(main_path, "data-staged-for-deletion/GIS/raw/ct-cartographic-boundaries/cb_2019_06_tract_500k/cb_2019_06_tract_500k.shp"), format = "file"),
  tar_target(name = file_raw_ct_2020, command = file.path(main_path, "data-staged-for-deletion/GIS/raw/ct-cartographic-boundaries/nhgis0030_shapefile_tl2020_us_tract_2020/US_tract_2020.shp"), format = "file"),
  tar_target(name = file_raw_census_2020, command = file.path(main_path, "data-staged-for-deletion/Census/nhgis_2020/nhgis0024_csv/nhgis0024_ds249_20205_tract.csv"), format = "file"),
  tar_target(name = file_raw_census_2021, command = file.path(main_path, "data-staged-for-deletion/Census/nhgis_2020/nhgis0024_csv/nhgis0024_ds254_20215_tract.csv"), format = "file"),
  tar_target(name = file_raw_ct_race, command = file.path(main_path, "data-staged-for-deletion/Census/nhgis0039_csv/nhgis0039_ds258_2020_tract.csv"), format = "file"),
  tar_target(name = file_raw_census_poverty, command = file.path(main_path, "data-staged-for-deletion/Census/nhgis_2020/nhgis0029_csv/nhgis0029_csv/nhgis0029_ds254_20215_tract.csv"), format = "file"),
  tar_target(name = file_df_ca_regions, command = file.path(main_path, "data-staged-for-deletion/labor/raw/ca_regions.csv"), format = "file"),
  # tar_target(name = file_df_labor, command = file.path(main_path, "data-staged-for-deletion/labor/processed/implan-results/academic-paper-multipliers/processed/ica_multipliers_v2.xlsx"), format = "file"),
  # tar_target(name = file_df_labor_dest, command = file.path(main_path, "data-staged-for-deletion/labor/processed/implan-results/academic-paper-multipliers/processed/20240524-1million_la-Detail Economic Indicators.csv"), format = "file"),
  tar_target(name = file_df_labor_dest, command = file.path(main_path, "data-staged-for-deletion/labor/processed/implan-results/academic-paper-multipliers/processed/20240623-census_regions-Detail Economic Indicators.csv"), format = "file"),
  tar_target(name = file_df_labor_fte, command = file.path(main_path, "data-staged-for-deletion/labor/processed/implan-results/academic-paper-multipliers/processed/Emp_FTE and W&S_EC_546 Industry Scheme.xlsx"), format = "file"),
  tar_target(name = file_oil_px, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/processed/oil_price_projections_revised.xlsx"), format = "file"),
  tar_target(name = file_ca_counties_sp, command = file.path(main_path, "data-staged-for-deletion/GIS/raw/CA_counties_noislands/CA_Counties_TIGER2016_noislands.shp"), format = "file"),
  tar_target(name = file_refin_locs_orig, command = file.path(main_path, "data-staged-for-deletion/GIS/raw/Petroleum_Refineries_US_EIA/Petroleum_Refineries_US_2019_v2.shp"), format = "file"),
  tar_target(name = file_refin_locs, command = file.path(main_path, "/data-staged-for-deletion/stocks-flows/processed/refinery_lat_long_revised.csv"), format = "file"),
  tar_target(name = file_labor_2019, command = file.path(main_path, "/data-staged-for-deletion/labor/implan/20241010-census_regions_2019-Detail Economic Indicators.csv"), format = "file"),


  # read in raw data files
  tar_target(name = raw_its_bau, command = read_raw_its_data(file_raw_its, input_sheet = "Sheet1", input_rows = c(1, 7:19), input_cols = c(2:37))),
  tar_target(name = raw_its_lc1, command = read_raw_its_data(file_raw_its, input_sheet = "Sheet1", input_rows = c(1, 23:34), input_cols = c(2:37))),
  tar_target(name = raw_avgas, command = simple_read_xlsx(file_raw_avgas, input_sheet = "Sheet1", input_rows = c(4, 16), input_cols = c(3:38))),
  tar_target(name = raw_intra_jet, command = simple_read_xlsx(file_raw_avgas, input_sheet = "Sheet1", input_rows = c(4, 14), input_cols = c(3:38))),
  tar_target(name = raw_cec_jet, command = simple_read_xlsx(file_raw_cec_jet, input_sheet = "Jet Fuel Demand", input_rows = NULL, input_cols = c(1:4))),
  tar_target(name = raw_mil_jet, command = simple_read_xlsx(file_raw_mil_jet, input_sheet = "CA Fuel Consumption Data", input_rows = c(7, 9:26), input_cols = c(1, 16))),
  tar_target(name = raw_fpm_gasoline, command = read_raw_fpm_data(file_raw_fpm, input_sheet = "Gasoline Chart Data", start_row_input = 3)),
  tar_target(name = raw_fpm_diesel, command = read_raw_fpm_data(file_raw_fpm, input_sheet = "Diesel Chart Data", start_row_input = 4)),
  tar_target(name = raw_fpm_jet, command = read_raw_fpm_data(file_raw_fpm, input_sheet = "Jet Fuel Chart Data", start_row_input = 3)),
  tar_target(name = dt_refcap, command = read_refcap_data(file_refcap)),
  tar_target(name = dt_rediesel, command = read_rediesel_data(file_rediesel, "Fig10", input_rows = c(2, 15), input_cols = seq(1, 10))),
  tar_target(name = dt_renref, command = simple_read_xlsx(file_renref, "Sheet1")[, .(installation_year, refinery_name, installation_capacity_bpd, retired_capacity_bpd)]),
  tar_target(name = renewables_info, command = simple_read_xlsx(file_renref, "Sheet1")[, .(site_id, refinery_name, location, region, cluster)]),
  tar_target(name = dt_altair, command = read_altair_data(file_altair, "Sheet1", ei_crude, ei_gasoline)),
  tar_target(name = raw_ces, command = read_raw_ces_data(file_raw_ces)),
  tar_target(name = raw_dac, command = read_raw_dac_data(file_raw_dac, input_sheet = "SB535 tract list (2022)", input_cols = c(1, 2, 7, 11))),
  tar_target(name = raw_income_house, command = read_census_data(file_raw_income_house)),
  tar_target(name = raw_income_county, command = read_census_data(file_raw_income_county)),
  tar_target(name = dt_ef, command = fread_data(file_dt_ef)), #cluster-level emission factors
  tar_target(name = dt_ef_ref, command = fread_data(file_dt_ef_ref)), #refinery-level emission factors
  tar_target(name = dt_age_vsl, command = fread_data(file_dt_age_vsl)),
  tar_target(name = ct_inc_45, command = fread_data(file_dt_ct_inc_pop)),
  tar_target(name = growth_cap_rates, command = fread_data(file_dt_growth_cap_rate)),
  tar_target(name = health_income, command = fread_data(file_dt_health_income)),
  tar_target(name = raw_ct_2019, command = read_ct_2019_data(file_raw_ct_2019, ca_crs)),
  tar_target(name = raw_ct_2020, command = read_ct_2020_data(file_raw_ct_2020, ca_crs)),
  tar_target(name = raw_ct_2020_all, command = st_read(file_raw_ct_2020)),
  tar_target(name = raw_counties, command = st_read(file_ca_counties_sp)),
  tar_target(name = raw_pop_income_2020, command = read_nhgis_data(file_raw_census_2020)),
  tar_target(name = raw_pop_income_2021, command = read_nhgis_2021_data(file_raw_census_2021)),
  tar_target(name = raw_ct_race, command = read_census_race_data(file_raw_ct_race)),
  tar_target(name = raw_pop_poverty, command = read_poverty_data(file_raw_census_poverty)),
  tar_target(name = ca_regions, command = read_ca_regions(file_df_ca_regions)),
  # tar_target(name = proc_labor_df, command = read_labor_inputs(file_df_labor, input_sheet = "ica_total")),
  tar_target(name = proc_labor_fte_df, command = read_labor_fte_inputs(file_df_labor_fte, input_sheet = "2022")),
  tar_target(name = proc_labor_dest_df, command = read_labor_inputs(file_df_labor_dest, proc_labor_fte_df)),
  tar_target(name = proc_oil_px_df, command = read_oil_px(file_oil_px, input_sheet = "real", input_cols = c(1:4))),
  tar_target(name = refin_locs, command = read_refin_locs(
    file_refin_locs,
    file_refin_locs_orig,
    ca_crs
  )),
  tar_target(name = labor_2019, command = fread(file_labor_2019)),

  # create processed data
  tar_target(name = dt_its, command = get_its_forecast(raw_its_bau, raw_its_lc1, raw_avgas)),
  tar_target(name = dt_intra, command = get_intrastate_jet_forecast(raw_intra_jet)),
  tar_target(name = dt_jet, command = get_cec_interstate_jet_forecast(raw_cec_jet, raw_mil_jet, ei_gasoline, ei_jet)),
  tar_target(name = dt_fpm, command = get_finished_products_movements(raw_fpm_gasoline, raw_fpm_diesel, raw_fpm_jet)),
  tar_target(name = ces_county, command = get_ces_county(raw_ces)),
  tar_target(name = med_house_income, command = get_median_household_income(raw_income_house)),
  tar_target(name = county_income, command = get_median_county_income(raw_income_county)),
  tar_target(name = dt_inmap_re, command = rbindlist(lapply(buff_sites, read_inmap_data, inmap_path = file_inmap_re))),

  # set remaining file paths
  # tar_target(name = file_its, command = file.path(main_path, "outputs/fuel-demand/prelim-results/its_demand_bau_and_lc1_2020_2045.csv"), format = "file"),
  # tar_target(name = file_jet, command = file.path(main_path, "outputs/fuel-demand/prelim-results/cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv"), format = "file"),
  tar_target(name = file_fpm, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/processed/finished_product_movements_weekly_cec.csv"), format = "file"),
  tar_target(name = file_fw, command = file.path(main_path, "data-staged-for-deletion/stocks-flows/processed/fuel_watch_data.csv"), format = "file"),
  tar_target(name = file_ghgfac, command = file.path(main_path, "outputs-staged-for-deletion/stocks-flows/refinery_ghg_factor_x_indiv_refinery_revised.csv"), format = "file"),
  tar_target(name = file_processed_ces3, command = file.path(main_path, "data-staged-for-deletion/health/processed/ces3_data.csv"), format = "file"),
  tar_target(name = file_site_2019, command = file.path(main_path, "model-development/scenario-plot-staged-for-deletion/refinery-outputs/site_refining_outputs_2019.csv"), format = "file"),
  tar_target(name = file_county_2019, command = file.path(main_path, "model-development/scenario-plot-staged-for-deletion/refinery-outputs/county_refining_outputs_2019.csv"), format = "file"),
  tar_target(name = file_ghg_2019, command = file.path(main_path, "model-development/scenario-plot-staged-for-deletion/refinery-outputs/refining_emissions_state_2019_revised.csv"), format = "file"),

  # read in processed data files
  # tar_target(name = dt_its, command = simple_fread(file_its)),
  # tar_target(name = dt_jet, command = simple_fread(file_jet)),
  tar_target(name = dt_fw, command = simple_fread(file_fw)),
  tar_target(name = dt_ghgfac, command = read_ref_ghg_data(file_ghgfac, 2018)),
  tar_target(name = dt_ces, command = read_census_data(file_processed_ces3)),
  tar_target(name = dt_site_2019, command = simple_fread(file_site_2019)),
  tar_target(name = dt_county_2019, command = simple_fread(file_county_2019)),
  tar_target(name = dt_ghg_2019, command = read_ghg_2019_data(file_ghg_2019)),
  # tar_target(name = dt_fpm, command = simple_fread(file_fpm)),

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
  tar_target(name = product_px, command = create_prod_px_spread(proc_oil_px_df)),

  # refining module
  tar_target(name = refining_module_outputs, command = main_refining_module(
    dem_scens,
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
    ei_jet
  )),
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
  tar_target(name = ref_crude_gjd, command = divide_gjd_demand_crude_refineries(
    res_equiv_demand,
    res_crude_ref_reg_capacity,
    crude_refined_region,
    ei_crude,
    ei_gasoline,
    ei_diesel,
    ei_jet
  )),
  tar_target(name = ref_crude_res_regjd, command = divide_residual_gjd_crude_refineries(
    res_equiv_demand, res_crude_ref_reg_capacity, crude_refined_region,
    ei_crude, ei_gasoline, ei_diesel, ei_jet, dem_scens, ref_scens, ave_kern_rediesel
  )),
  tar_target(name = ref_renew_gjd, command = divide_residual_gjd_renewable_refineries(
    res_renew_demand, res_renew_ref_reg_capacity, renewables_info_altair, crude_refined_tot,
    ei_crude, ei_gasoline, ei_diesel, ei_jet
  )),
  tar_target(name = ref_cons_prod, command = combine_refinery_prod_cons(
    ref_crude_gjd,
    ref_crude_res_regjd,
    ref_renew_gjd,
    dt_ghgfac
  )),

  # individual refinery level production
  tar_target(name = indiv_prod, command = gather_refinery_production(ref_cons_prod, ei_crude, ei_gasoline, ei_diesel, ei_jet)),
  tar_target(name = indiv_prod_output, command = gather_refinery_production_output(indiv_prod)),
  tar_target(name = indiv_prod_output_bge, command = gather_refinery_production_output_bge(indiv_prod)),

  # individual refinery level crude consumption
  tar_target(name = indiv_cons, command = gather_refinery_crude_consumption(
    ref_cons_prod,
    ei_crude,
    ei_gasoline,
    ei_diesel,
    ei_jet
  )),
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
  tar_target(name = tot_fuel_demand_exports, command = combine_state_gjd_demand_and_exports(
    crude_refined_week, refined_movements_annual, dt_rediesel,
    res_equiv_demand, res_renew_demand,
    dem_scens, ref_scens, ei_crude, ei_gasoline, ei_diesel, ei_jet
  )),

  # paper figures
  tar_target(name = fig_demand_ghg, command = plot_combined_production(dt_its, dt_jet, dt_intra, tot_fuel_demand_exports, state_ghg_output)),
  tar_target(name = fig_refinery_capacity, plot_refinery_capacity(res_crude_ref_reg, res_renew_ref_reg, ei_crude, ei_gasoline)),
  tar_target(name = fig_refinery_count, plot_refinery_count(res_crude_ref_reg, res_renew_ref_reg, ei_crude, ei_gasoline)),

  # DAC / health: PM2.5 by county
  tar_target(name = county_dac, command = get_county_dac(dt_ces, ces_county)), ## matches county to DAC, but maybe circular
  # tar_target(name = site_ids, command = get_refinery_site_ids(dt_refcap)),
  tar_target(name = refining_site_consumption, command = get_site_level_refining_cons(indiv_cons_output)),
  tar_target(name = refining_site_ghg, command = get_site_level_refining_ghg(indiv_ghg_output)),
  tar_target(name = refining_site_output, command = combine_refining_site_cons_ghg(refining_site_consumption, refining_site_ghg)),
  tar_target(name = refining_sites_scenarios, command = create_all_sites_scenarios_df(refining_site_output)),
  tar_target(name = refining_sites_cons_ghg_2019_2045, command = organize_consumption_ghg_outputs(
    refining_sites_scenarios,
    dt_site_2019,
    refining_site_output,
    indiv_cons_output
  )),
  tar_target(name = srm_weighted_pm25, command = process_weighted_pm25(dt_inmap_re)),
  tar_target(name = ct_xwalk, command = create_ct_xwalk(
    raw_ct_2019,
    raw_ct_2020
  )),
  tar_target(name = refining_health_income, command = calculate_census_tract_emissions(
    refining_sites_cons_ghg_2019_2045,
    srm_weighted_pm25,
    county_dac,
    med_house_income,
    dt_ef, #cluster-level emission factors
    dt_ef_ref, #refinery-level emission factors
    dt_refcap,
    renewables_info_altair
  )),
  tar_target(name = health_weighted, command = calculate_weighted_census_tract_emissions(
    ct_xwalk,
    refining_health_income,
    raw_dac
  )),
  tar_target(name = refinery_pm25_srm, command = create_srm_xwalk(
    main_path,
    srm_weighted_pm25,
    ct_xwalk,
    raw_counties,
    raw_ct_2020_all
  )),
  tar_target(name = ct_pm25_srm, command = create_srm_ct(
    main_path,
    refinery_pm25_srm
  )),
  tar_target(name = pulse_fig, command = create_pulse_fig(
    main_path,
    refinery_pm25_srm,
    ct_pm25_srm,
    raw_counties,
    raw_ct_2020_all,
    refin_locs,
    ca_crs
  )),
  tar_target(name = refining_mortality, command = calculate_census_tract_mortality(
    beta,
    se,
    vsl_2015,
    vsl_2019,
    income_elasticity_mort,
    discount_rate,
    health_weighted,
    ct_inc_45,
    growth_cap_rates,
    dt_age_vsl
  )),
  tar_target(name = ref_mort_level, command = calculate_mort_level(refining_mortality)),
  tar_target(name = pop_ratios, command = calc_pop_ratios(
    raw_ct_race,
    raw_pop_poverty,
    refining_mortality
  )),
  tar_target(name = state_pop_ratios, command = calc_state_pop_ratios(
    raw_ct_race,
    raw_pop_poverty,
    refining_mortality
  )),
  tar_target(name = health_grp, command = calculate_race_disp(
    health_weighted,
    pop_ratios,
    refining_mortality
  )),

  # tar_target(name = health_pov, command = calculate_poverty_disp(raw_pop_poverty,
  #                                                                health_weighted)),
  #

  tar_target(name = cumul_av_mort, command = calc_cumul_av_mort(
    main_path,
    health_grp
  )),
  tar_target(name = county_pop_ratios, command = calc_pop_ratios_county(
    raw_ct_race,
    raw_pop_poverty,
    refining_mortality
  )),
  tar_target(name = county_grp_pop_ratios, command = calc_pop_ratios_county_grp(
    raw_ct_race,
    raw_pop_poverty,
    refining_mortality,
    ca_regions
  )),
  tar_target(name = ref_mortality_demog, command = calculate_mort_x_demg(
    refining_mortality,
    pop_ratios,
    main_path
  )),
  tar_target(name = annual_labor, command = calc_labor_outputs(
    main_path,
    proc_labor_dest_df,
    indiv_prod_output,
    dt_refcap,
    product_px,
    cpi2019,
    cpi2020,
    discount_rate,
    alpha_comp,
    alpha_emp
  )),
  tar_target(name = annual_labor_x_impact, command = calc_labor_outputs_x_impact(
    main_path,
    proc_labor_dest_df,
    indiv_prod_output,
    dt_refcap,
    product_px,
    cpi2019,
    cpi2020,
    discount_rate,
    alpha_comp,
    alpha_emp
  )),
  tar_target(name = ref_labor_demog_yr, command = calculate_labor_x_demg_annual(
    county_grp_pop_ratios,
    annual_labor,
    raw_pop_income_2021,
    refining_mortality,
    ca_regions
  )),
  tar_target(name = county_labor_outputs, command = calc_county_level_outputs(
    main_path,
    ref_labor_demog_yr,
    refining_mortality,
    ca_regions,
    raw_pop_income_2021
  )),
  tar_target(name = annual_labor_jobs_comp, command = calculate_annual_labor_x_demg_hl(
    main_path,
    ref_labor_demog_yr,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = ref_labor_demog, command = calculate_labor_x_demg(ref_labor_demog_yr)),
  tar_target(name = county_pm25_2019, command = calc_county_pm25(
    main_path,
    health_weighted,
    raw_counties,
    raw_ct_2020_all,
    refining_mortality
  )),
  tar_target(name = npv_plot, command = plot_npv_health_labor(
    main_path,
    refining_mortality,
    state_ghg_output,
    dt_ghg_2019,
    annual_labor
  )),
  tar_target(name = npv_labor_plot, command = plot_npv_labor_oilpx(
    main_path,
    state_ghg_output,
    dt_ghg_2019,
    annual_labor
  )),
  tar_target(name = health_levels_plot, command = plot_health_levels(
    main_path,
    health_grp
  )),
  tar_target(name = health_levels_pmil_plot, command = plot_health_levels_pc(
    main_path,
    health_grp,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = health_levels_plot_pm25, command = plot_health_levels_pm25(
    main_path,
    health_grp
  )),
  tar_target(name = labor_levels_plot, command = plot_labor_levels(
    main_path,
    ref_labor_demog_yr,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = labor_levels_plot_pmil, command = plot_labor_levels_pmil(
    main_path,
    ref_labor_demog_yr,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = health_gaps_plot, command = plot_health_levels_gaps(
    main_path,
    health_grp
  )),
  tar_target(name = health_gaps_pmil_plot, command = plot_health_levels_gaps_pmil(
    main_path,
    health_grp,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = health_gaps_plot_pm25, command = plot_health_levels_gaps_pm25(
    main_path,
    health_grp
  )),
  tar_target(name = labor_gaps_plot, command = plot_labor_levels_gaps(
    main_path,
    ref_labor_demog_yr,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = labor_gaps_plot_pmil, command = plot_labor_levels_gaps_pmil(
    main_path,
    ref_labor_demog_yr,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = demographic_npv_df, command = plot_hl_levels_df(
    main_path,
    ref_mortality_demog,
    ref_labor_demog,
    state_ghg_output,
    dt_ghg_2019
  )),

  # tar_target(name = county_health_labor, command = create_county_health_labor_df(main_path,
  #                                                                                refining_mortality,
  #                                                                                state_ghg_output,
  #                                                                                annual_labor,
  #                                                                                raw_ct_2020_all,
  #                                                                                raw_counties)),

  tar_target(name = health_county_df, command = calculate_county_health(
    # health_weighted,
    main_path,
    pop_ratios,
    refining_mortality,
    raw_ct_2020_all,
    raw_counties,
    discount_rate
  )),
  tar_target(name = demographic_npv_plot, command = plot_hl_levels(demographic_npv_df)),
  tar_target(name = demographic_npv_shares_plot, command = plot_hl_shares(
    main_path,
    demographic_npv_df,
    state_pop_ratios
  )),
  tar_target(name = demographic_npv_plot_pc, command = plot_hl_levels_pc(
    demographic_npv_df,
    refining_mortality,
    pop_ratios,
    main_path
  )),
  tar_target(name = health_labor_gaps_plot, command = fig4_hl(
    health_grp,
    ref_labor_demog_yr,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = health_labor_gaps_pmil_plot, command = fig4_hl_pmil(
    health_grp,
    ref_labor_demog_yr,
    refining_mortality,
    pop_ratios
  )),
  tar_target(name = state_level_results, command = create_health_labor_table(
    main_path,
    demographic_npv_df,
    ref_labor_demog,
    pop_ratios,
    refining_mortality
  )),

  # # save outputs
  # tar_target(name = save_ct_xwalk,
  #            command = simple_fwrite(ct_xwalk, main_path, "outputs/refining-2024/health-beta-adj", "ct_xwalk_2019_2020.csv"),
  #            format = "file"),
  # tar_target(name = save_health_income,
  #            command = simple_fwrite(refining_health_income, main_path, "outputs/refining-2024/health-beta-adj", "refining_health_income_2023.csv"),
  #            format = "file"),
  # tar_target(name = save_health_income_2000,
  #            command = simple_fwrite(health_weighted, main_path, "outputs/refining-2024/health-beta-adj", "refining_health_census_tract.csv"),
  #            format = "file"),
  # tar_target(name = save_mortality,
  #            command = simple_fwrite(refining_mortality, main_path, "outputs/refining-2024/health-beta-adj", "refining_mortality_2023.csv"),
  #            format = "file"),
  # tar_target(name = save_state_mort_levels,
  #            command = simple_fwrite(ref_mort_level, main_path, "outputs/refining-2024/health-beta-adj", "refining_state_mortality.csv"),
  #            format = "file"),
  #
  # # save figures
  # tar_target(
  #   name = save_fig_demand_ghg,
  #   command = simple_ggsave(fig_demand_ghg,
  #     main_path,
  #     "outputs/academic-out/refining/figures/2025-health-revisions",
  #     "combined_its_and_production",
  #     width = 25,
  #     height = 13,
  #     dpi = 600
  #   ),
  #   format = "file"
  # ),
  # tar_target(name = save_npv_fig,
  #            command = simple_ggsave(npv_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_npv_fig",
  #                                    width = 10,
  #                                    height = 5,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_levels_fig,
  #            command = simple_ggsave(health_levels_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_levels_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_levels_pmil_fig,
  #            command = simple_ggsave(health_levels_pmil_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_levels_pmil_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_levels_pm25_fig,
  #            command = simple_ggsave(health_levels_plot_pm25,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_levels_pm25_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_l_levels_fig,
  #            command = simple_ggsave(labor_levels_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_labor_levels_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_l_levels_pmil_fig,
  #            command = simple_ggsave(labor_levels_plot_pmil,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_labor_levels_pmil_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_gaps_fig,
  #            command = simple_ggsave(health_gaps_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_gaps_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_gaps_pmil_fig,
  #            command = simple_ggsave(health_gaps_pmil_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_gaps_pmil_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_gaps_pm25_fig,
  #            command = simple_ggsave(health_gaps_plot_pm25,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_gaps_pm25_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_labor_gaps_fig,
  #            command = simple_ggsave(labor_gaps_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_labor_gaps_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_labor_gaps_fig_pmil,
  #            command = simple_ggsave(labor_gaps_plot_pmil,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "state_labor_gaps_pmil_fig",
  #                                    width = 12,
  #                                    height = 8,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_demo_npv_fig,
  #            command = simple_ggsave(demographic_npv_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "demographic_npv_fig",
  #                                    width = 11,
  #                                    height = 12,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_demo_share_fig,
  #            command = simple_ggsave(demographic_npv_shares_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "demographic_npv_shares_fig",
  #                                    width = 11,
  #                                    height = 12,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_demo_npv_pc_fig,
  #            command = simple_ggsave(demographic_npv_plot_pc,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "demographic_npv_pc_fig",
  #                                    width = 11,
  #                                    height = 12,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_health_labor_gaps_plot,
  #            command = simple_ggsave(health_labor_gaps_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "health_labor_gaps_plot",
  #                                    width = 12,
  #                                    height = 6,
  #                                    dpi = 600),
  #            format = "file"),
  #
  # tar_target(name = save_health_labor_gaps_pmil_plot,
  #            command = simple_ggsave(health_labor_gaps_pmil_plot,
  #                                    main_path,
  #                                    "outputs/academic-out/refining/figures/2024-08-beta-adj",
  #                                    "health_labor_gaps_pmil_plot",
  #                                    width = 18,
  #                                    height = 6,
  #                                    dpi = 600),
  #            format = "file")


  # save outputs
  tar_target(
    name = save_ct_xwalk,
    command = simple_fwrite(ct_xwalk, main_path, "outputs/refining-2024/health", "ct_xwalk_2019_2020.csv"),
    format = "file"
  ),
  tar_target(
    name = save_health_income,
    command = simple_fwrite(refining_health_income, main_path, "outputs/refining-2024/health", "refining_health_income_2023.csv"),
    format = "file"
  ),
  tar_target(
    name = save_health_income_2000,
    command = simple_fwrite(health_weighted, main_path, "outputs/refining-2024/health", "refining_health_census_tract.csv"),
    format = "file"
  ),
  tar_target(
    name = save_mortality,
    command = simple_fwrite(refining_mortality, main_path, "outputs/refining-2024/health", "refining_mortality_2023.csv"),
    format = "file"
  ),
  tar_target(
    name = save_state_mort_levels,
    command = simple_fwrite(ref_mort_level, main_path, "outputs/refining-2024/health", "refining_state_mortality.csv"),
    format = "file"
  ),

  # save figures
  # tar_target(
  #   name = save_fig_demand,
  #   command = simple_ggsave(fig_demand,
  #     main_path,
  #     "outputs/academic-out/refining/figures/", folder_name,
  #     "its_demand_and_production_2023",
  #     width = 6.5,
  #     height = 8,
  #     dpi = 600
  #   ),
  #   format = "file"
  # ),
  # tar_target(
  #   name = save_fig_refined_production_ghg,
  #   command = simple_ggsave(fig_refined_production_ghg,
  #     main_path,
  #     "outputs/academic-out/refining/figures/", folder_name,
  #     "state_GJD_and_reGJD_production_and_ghg_emissions",
  #     width = 20,
  #     height = 12,
  #     dpi = 600
  #   ),
  #   format = "file"
  # ),
  tar_target(
    name = save_npv_fig,
    command = simple_ggsave(npv_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_npv_fig",
      width = 10,
      height = 5,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_npv_labor_fig,
    command = simple_ggsave(npv_labor_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_npv_labor_fig",
      width = 10,
      height = 5,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_levels_fig,
    command = simple_ggsave(health_levels_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_levels_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_levels_pmil_fig,
    command = simple_ggsave(health_levels_pmil_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_levels_pmil_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_levels_pm25_fig,
    command = simple_ggsave(health_levels_plot_pm25,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_levels_pm25_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_l_levels_fig,
    command = simple_ggsave(labor_levels_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_labor_levels_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_l_levels_pmil_fig,
    command = simple_ggsave(labor_levels_plot_pmil,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_labor_levels_pmil_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_gaps_fig,
    command = simple_ggsave(health_gaps_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_gaps_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_gaps_pmil_fig,
    command = simple_ggsave(health_gaps_pmil_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_gaps_pmil_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_gaps_pm25_fig,
    command = simple_ggsave(health_gaps_plot_pm25,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_gaps_pm25_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_labor_gaps_fig,
    command = simple_ggsave(labor_gaps_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_labor_gaps_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_labor_gaps_fig_pmil,
    command = simple_ggsave(labor_gaps_plot_pmil,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "state_labor_gaps_pmil_fig",
      width = 12,
      height = 8,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_demo_npv_fig,
    command = simple_ggsave(demographic_npv_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "demographic_npv_fig",
      width = 11,
      height = 12,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_demo_share_fig,
    command = simple_ggsave(demographic_npv_shares_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "demographic_npv_shares_fig",
      width = 12,
      height = 12,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_demo_npv_pc_fig,
    command = simple_ggsave(demographic_npv_plot_pc,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "demographic_npv_pc_fig",
      width = 11,
      height = 12,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_health_labor_gaps_plot,
    command = simple_ggsave(health_labor_gaps_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "health_labor_gaps_plot",
      width = 14,
      height = 6,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_health_labor_gaps_pmil_plot,
    command = simple_ggsave(health_labor_gaps_pmil_plot,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "health_labor_gaps_pmil_plot",
      width = 18,
      height = 6,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_fig_refinery_capacity,
    command = simple_ggsave(fig_refinery_capacity,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "refinery_capacity",
      width = 16,
      height = 12,
      dpi = 600
    ),
    format = "file"
  ),
  tar_target(
    name = save_fig_refinery_count,
    command = simple_ggsave(fig_refinery_count,
      main_path,
      "outputs/academic-out/refining/figures/2025-health-revisions",
      "refinery_count",
      width = 16,
      height = 12,
      dpi = 600
    ),
    format = "file"
  )
)
