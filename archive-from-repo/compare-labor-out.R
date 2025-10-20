## compare outputs

orig <- fread(file.path(main_path, "outputs/academic-out/refining/figures/2025-update/fig-csv-files/labor_result_for_review.csv"),
              colClasses = c(census_tract = "character"))

compare <- merge(ct_out_refining_direct %>% filter(product_scenario == "changing prices"),
                 orig, 
                 by = c("demand_scenario",
                        "refining_scenario",
                        "oil_price_scenario",
                        "census_tract",
                        "year"), suffixes = c("_orig", "_new")) |>
                   ## compute absolute difference
                   mutate(diff_total_comp_usd19_h = total_comp_usd19_h_orig - total_comp_usd19_h_new,
                          diff_prev_comp_usd19h = prev_comp_usd19h_orig - prev_comp_usd19h_new,
                          diff_total_comp_usd19_l = total_comp_usd19_l_orig - total_comp_usd19_l_new,
                          diff_total_emp = total_emp_orig - total_emp_new,
                          diff_prev_emp = prev_emp_orig - prev_emp_new,
                          diff_r = total_emp_revised_orig - total_emp_revised_new)


orig2 <- fread(file.path(main_path, "outputs/academic-out/refining/figures/2025-update/fig-csv-files/state_out_labor_all_impacts.csv"))

orig2 <- orig2 |> pivot_longer(state_emp_h:comp_all_impacts_PV_l, names_to = "metric", values_to = "orig_value")

compare2 <- state_out_labor_all_impacts |> pivot_longer(state_emp_h:comp_all_impacts_PV_l, names_to = "metric", values_to = "value") |>
  filter(product_scenario == "changing prices") |>
  full_join(orig2) |>
  mutate(diff = orig_value - value)

orig3 <- fread(file.path(main_path, "outputs/academic-out/refining/figures/2025-update/fig-csv-files/state_labor_direct_impacts_demo_annual.csv")) |>
  pivot_longer(sum_demo_emp:sum_demo_comp_pv_l, names_to = "metric", values_to = "orig_value")

compare3 <- state_demo_labor_out |> pivot_longer(sum_demo_emp:sum_demo_comp_pv_l, names_to = "metric", values_to = "value") |>
  filter(product_scenario == "changing prices") |>
  full_join(orig3) |>
  mutate(diff = orig_value - value)

orig4 <- fread(file.path(main_path, "outputs/academic-out/refining/figures/2025-update/fig-csv-files/", "labor_high_low_annual_outputs.csv")) |>
  rename(orig_value = value)

compare4 <- labor_pct_long |>
  filter(product_scenario == "changing prices") |>
  full_join(orig4) |>
  mutate(diff = orig_value - value)

orig5 <- fread(file.path(main_path, "outputs/academic-out/refining/figures/2025-update/fig-csv-files/state_labor_direct_impacts_demo_annual.csv")) |>
  pivot_longer(sum_demo_emp:sum_demo_comp_pv_l, names_to = "metric", values_to = "orig_value")

compared5 <- state_demo_labor_out |>
  filter(product_scenario == "changing prices") |>
  pivot_longer(sum_demo_emp:sum_demo_comp_pv_l, names_to = "metric", values_to = "value") |>
  full_join(orig5) |>
  mutate(diff = orig_value - value)

npv_comp <- fread(file.path(main_path, "outputs/academic-out/refining/figures/2025-update/fig-csv-files/", "state_npv_fig_inputs_labor.csv"))

comp6 <- plot_df_labor |>
  filter(produc)

  
  
  