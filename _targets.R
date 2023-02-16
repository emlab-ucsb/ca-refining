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
# library(unikn)
# library(tarchetypes) # Load other packages as needed. # nolint

# INPUTS
# main_path = '/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn'
# refin_scens = file.path(main_path, 'project-materials/scenario-inputs/refinery_scenario_inputs.csv')

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
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(name = main_path, command = "/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn"),
  tar_target(name = file_scen, command = file.path(main_path, "project-materials/scenario-inputs/refinery_scenario_inputs.csv"), format = "file"),
  tar_target(name = file_its, command = file.path(main_path, "outputs/fuel-demand/prelim-results/its_demand_bau_and_lc1_2020_2045.csv"), format = "file"),
  tar_target(name = file_jet, command = file.path(main_path, "outputs/fuel-demand/prelim-results/cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv"), format = "file"),
  tar_target(name = file_intra, command = file.path(main_path, "outputs/fuel-demand/prelim-results/its_demand_intrastate_jet_2020_2045.csv"), format = "file"),
  tar_target(name = file_fpm, command = file.path(main_path, "data/stocks-flows/processed/finished_product_movements_weekly_cec.csv"), format = "file"),
  tar_target(name = file_fw, command = file.path(main_path, "data/stocks-flows/processed/fuel_watch_data.csv"), format = "file"),
  tar_target(name = file_ei, command = file.path(main_path, "data/stocks-flows/processed/fuel-energy-intensities.csv"), format = "file"),
  tar_target(name = dt_scen, command = simple_fread(file_scen)),
  tar_target(name = dt_its, command = simple_fread(file_its)),
  tar_target(name = dt_jet, command = simple_fread(file_jet)),
  tar_target(name = dt_intra, command = simple_fread(file_intra)),
  tar_target(name = dt_fpm, command = simple_fread(file_fpm)),
  tar_target(name = dt_fw, command = simple_fread(file_fw)),
  tar_target(name = dt_ei, command = simple_fread(file_ei))
  # tar_target(name = output, command = track_files_basic(dt_scen), format = "file")
  )
