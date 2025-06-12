## compare health npv plot outputs

library(data.table)
library(tidyverse)

main_path <- "/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures/"

submission_df <- "2024-orig-submission/fig-csv-files/state_npv_fig_inputs_health.csv"
newer_df <- "2025-health-revisions/fig-csv-files/state_npv_fig_inputs_health.csv"

orig_df <- fread(paste0(main_path, submission_df))

new_df <- fread(paste0(main_path, newer_df)) |>
  rename(new_val = value) |>
  left_join(orig_df) |>
  mutate(diff = new_val - value)
