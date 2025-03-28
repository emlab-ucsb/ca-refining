## labor functions

create_prod_px_spread <- function(proc_oil_px_df) {
  crack_spread <- tibble(
    product = c("gasoline", "jet_fuel", "diesel"),
    spread = c(23, 20, 23)
  )

  crack_spread_ex <- expand.grid(
    year = c(2019:2045),
    product = unique(crack_spread$product)
  )

  crack_spread <- merge(crack_spread_ex, crack_spread,
    all.x = T
  )

  prod_price <- copy(proc_oil_px_df)
  # prod_price <- proc_oil_px_df[oil_price_scenario == "reference case"]

  prod_price <- merge(crack_spread, prod_price,
    by = c("year"),
    all = T,
    allow.cartesian = T
  )

  setDT(prod_price)

  prod_price[, product_price := oil_price_usd_per_bbl + spread]

  prod_price
}


calc_labor_outputs <- function(main_path,
                               proc_labor_dest_df,
                               indiv_prod_output,
                               dt_refcap,
                               product_px,
                               cpi2019,
                               cpi2020,
                               discount_rate,
                               alpha_comp,
                               alpha_emp) {
  ## add product for calculating price
  county_out_refining <- copy(indiv_prod_output)

  county_out_refining[, fuel := as.character(fuel)]

  county_out_refining[, product := fifelse(
    fuel %chin% c("gasoline", "drop-in gasoline"), "gasoline",
    fifelse(fuel %chin% c("diesel", "renewable diesel"), "diesel", "jet_fuel")
  )]

  ## merge with counties
  county_df <- dt_refcap[, .(site_id, county)]
  county_df[, site_id := as.character(site_id)]


  county_out_refining <- merge(county_out_refining, county_df,
    by = c("site_id"),
    all.x = T
  )

  # fill in missing counties
  county_out_refining[, county := fifelse(
    site_id == "342-2", "Contra Costa",
    fifelse(
      site_id == "99999", "Kern",
      fifelse(site_id == "t-800", "Los Angeles", county)
    )
  )]

  ## merge with prices
  product_df <- copy(product_px)
  product_df <- product_df[, .(year, oil_price_scenario, product, product_price)]

  county_out_refining <- merge(county_out_refining, product_df,
    by = c("year", "product"),
    all.x = T,
    allow.cartesian = T
  )

  ## calculate revenue
  county_out_refining[, revenue := value * product_price]

  ## summarize at the county level
  county_out_refining_summary <- county_out_refining[, .(
    production_bbl = sum(value),
    revenue = sum(revenue)
  ), by = .(
    demand_scenario, refining_scenario,
    oil_price_scenario, year, county
  )]


  ## calculate labor impacts
  county_out_refining_summary[, county := fifelse(county == "Solano County", "Solano", county)]


  ## merge with labor multipliers, calculate labor vals
  county_out_labor <- merge(county_out_refining_summary, proc_labor_dest_df,
    by = c("county"),
    all.x = T,
    allow.cartesian = T
  )

  # county_out_labor[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult,
  #                          c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult,
  #                          c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
  #                          c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult,
  #                          c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult,
  #                          c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]
  #
  # county_out_labor[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
  #                          total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]


  county_out_labor[, ":="(c.emp = (revenue / (10^6)) * employment,
    c.comp = (revenue / (10^6)) * emp_comp)]

  county_out_labor <- county_out_labor[, .(
    total_production_bbl = sum(production_bbl),
    total_revenue = sum(revenue),
    total_emp = sum(c.emp),
    total_comp = sum(c.comp)
  ), .(demand_scenario, refining_scenario, oil_price_scenario, year, destination)]

  ## convert to 2019 dollars
  county_out_labor[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]

  ## calc PV
  county_out_labor[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate)^(year - 2019))]

  ## rename columns
  setnames(county_out_labor, c("total_comp", "total_comp_usd19", "total_comp_PV"), c("total_comp_h", "total_comp_usd19_h", "total_comp_PV_h"))

  ## calculate the lower bound value
  county_out_labor <- county_out_labor %>%
    arrange(demand_scenario, refining_scenario, oil_price_scenario, destination, year) %>%
    group_by(demand_scenario, refining_scenario, oil_price_scenario, destination) %>%
    mutate(
      prev_emp = ifelse(year == 2020, NA, lag(total_emp)),
      total_emp_revised = ifelse(year == 2020, total_emp, total_emp - ((1 - alpha_emp) * prev_emp)),
      prev_comp_usd19h = ifelse(year == 2020, NA, lag(total_comp_usd19_h)),
      total_comp_usd19_l = ifelse(year == 2020, total_comp_usd19_h, total_comp_usd19_h - ((1 - alpha_comp) * prev_comp_usd19h))
    ) %>%
    ungroup() %>%
    as.data.table()

  review_df <- county_out_labor %>%
    select(demand_scenario, refining_scenario, oil_price_scenario, destination, year, total_production_bbl, total_revenue, total_comp_usd19_h, prev_comp_usd19h, total_comp_usd19_l, total_emp, total_emp_revised)

  ## save for review
  write_csv(review_df, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-update/fig-csv-files/labor_result_for_review.csv"))
  # write_csv(review_df, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/labor_result_for_review.csv"))

  ## calc discounted low
  county_out_labor[, total_comp_PV_l := total_comp_usd19_l / ((1 + discount_rate)^(year - 2019))]

  ## select columns
  county_out_labor <- county_out_labor[, .(
    demand_scenario, refining_scenario, oil_price_scenario, destination, year, total_emp, total_emp_revised, total_comp_h,
    total_comp_usd19_h, total_comp_usd19_l, total_comp_PV_h, total_comp_PV_l
  )]

  county_out_labor
}



calc_labor_outputs_x_impact <- function(main_path,
                                        proc_labor_dest_df,
                                        indiv_prod_output,
                                        dt_refcap,
                                        product_px,
                                        cpi2019,
                                        cpi2020,
                                        discount_rate,
                                        alpha_comp,
                                        alpha_emp) {
  ## add product for calculating price
  county_out_refining <- copy(indiv_prod_output)

  county_out_refining[, fuel := as.character(fuel)]

  county_out_refining[, product := fifelse(
    fuel %chin% c("gasoline", "drop-in gasoline"), "gasoline",
    fifelse(fuel %chin% c("diesel", "renewable diesel"), "diesel", "jet_fuel")
  )]

  ## merge with counties
  county_df <- dt_refcap[, .(site_id, county)]
  county_df[, site_id := as.character(site_id)]


  county_out_refining <- merge(county_out_refining, county_df,
    by = c("site_id"),
    all.x = T
  )

  # fill in missing counties
  county_out_refining[, county := fifelse(
    site_id == "342-2", "Contra Costa",
    fifelse(
      site_id == "99999", "Kern",
      fifelse(site_id == "t-800", "Los Angeles", county)
    )
  )]

  ## merge with prices
  product_df <- copy(product_px)
  product_df <- product_df[, .(year, oil_price_scenario, product, product_price)]

  county_out_refining <- merge(county_out_refining, product_df,
    by = c("year", "product"),
    all.x = T,
    allow.cartesian = T
  )

  ## calculate revenue
  county_out_refining[, revenue := value * product_price]

  ## summarize at the county level
  county_out_refining_summary <- county_out_refining[, .(
    production_bbl = sum(value),
    revenue = sum(revenue)
  ), by = .(
    demand_scenario, refining_scenario,
    oil_price_scenario, year, county
  )]


  ## calculate labor impacts
  county_out_refining_summary[, county := fifelse(county == "Solano County", "Solano", county)]


  ## merge with labor multipliers, calculate labor vals
  county_out_labor <- merge(county_out_refining_summary, proc_labor_dest_df,
    by = c("county"),
    all.x = T,
    allow.cartesian = T
  )

  # county_out_labor[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult,
  #                          c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult,
  #                          c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
  #                          c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult,
  #                          c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult,
  #                          c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]
  #
  # county_out_labor[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
  #                          total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]


  county_out_labor[, ":="(c.emp = (revenue / (10^6)) * employment,
    c.comp = (revenue / (10^6)) * emp_comp)]

  county_out_labor <- county_out_labor[, .(
    total_production_bbl = sum(production_bbl),
    total_revenue = sum(revenue),
    total_emp = sum(c.emp),
    total_comp = sum(c.comp)
  ), .(
    demand_scenario,
    refining_scenario,
    oil_price_scenario,
    impact_type,
    year,
    destination
  )]

  ## convert to 2019 dollars
  county_out_labor[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]

  ## calc PV
  county_out_labor[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate)^(year - 2019))]

  ## rename columns
  setnames(county_out_labor, c("total_comp", "total_comp_usd19", "total_comp_PV"), c("total_comp_h", "total_comp_usd19_h", "total_comp_PV_h"))

  ## calculate the lower bound value
  county_out_labor <- county_out_labor %>%
    arrange(demand_scenario, refining_scenario, oil_price_scenario, impact_type, destination, year) %>%
    group_by(demand_scenario, refining_scenario, oil_price_scenario, impact_type, destination) %>%
    mutate(
      prev_emp = ifelse(year == 2020, NA, lag(total_emp)),
      total_emp_revised = ifelse(year == 2020, total_emp, total_emp - ((1 - alpha_emp) * prev_emp)),
      prev_comp_usd19h = ifelse(year == 2020, NA, lag(total_comp_usd19_h)),
      total_comp_usd19_l = ifelse(year == 2020, total_comp_usd19_h, total_comp_usd19_h - ((1 - alpha_comp) * prev_comp_usd19h))
    ) %>%
    ungroup() %>%
    as.data.table()

  review_df <- county_out_labor %>%
    select(
      demand_scenario, refining_scenario, oil_price_scenario, impact_type,
      destination, year, total_production_bbl, total_revenue, total_comp_usd19_h,
      prev_comp_usd19h, total_comp_usd19_l, total_emp, total_emp_revised
    )

  ## save file
  write_csv(review_df, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-update/fig-csv-files/labor_result_x_impact_type_for_review.csv"))
  # write_csv(review_df, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/labor_result_x_impact_type_for_review.csv"))


  ## calc discounted low
  county_out_labor[, total_comp_PV_l := total_comp_usd19_l / ((1 + discount_rate)^(year - 2019))]

  ## select columns
  county_out_labor <- county_out_labor[, .(
    demand_scenario, refining_scenario, oil_price_scenario, impact_type, destination, year, total_emp, total_emp_revised, total_comp_h,
    total_comp_usd19_h, total_comp_usd19_l, total_comp_PV_h, total_comp_PV_l
  )]


  ## save file
  write_csv(county_out_labor, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-update/fig-csv-files/labor_result_x_impact_type.csv"))
  # write_csv(county_out_labor, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files/labor_result_x_impact_type.csv"))


  county_out_labor
}





## labor results grouped by demographic

calculate_labor_x_demg_annual <- function(county_grp_pop_ratios,
                                          annual_labor,
                                          raw_pop_income_2021,
                                          refining_mortality,
                                          ca_regions) {
  ## get county and census tracts
  c_ct_df <- raw_pop_income_2021[state == "California"]
  c_ct_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  c_ct_df <- c_ct_df[, .(county, census_tract)]
  c_ct_df[, county := str_remove(county, " County")]

  c_ct_df <- merge(c_ct_df, ca_regions,
    by = c("county"),
    allow.cartesian = T
  )

  ## get pop from refining_mortality
  pop_df <- refining_mortality %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(c_ct_df) %>%
    group_by(region, year) %>%
    summarise(pop = sum(pop)) %>%
    ungroup() %>%
    rename(destination = region)

  ## get
  ratio_df <- copy(county_grp_pop_ratios)
  setnames(ratio_df, "region", "destination")

  ## merge
  labor_pct_df <- merge(annual_labor, ratio_df,
    by = c("destination"),
    allow.cartesian = T
  )

  ## multiply by pct
  # labor_pct_df[, demo_emp := total_emp_revised * pct]
  labor_pct_df[, demo_emp := total_emp * pct]
  labor_pct_df[, demo_comp_pv_h := total_comp_PV_h * pct]
  labor_pct_df[, demo_comp_pv_l := total_comp_PV_l * pct]

  ## merge with population
  labor_pct_df <- merge(labor_pct_df, pop_df,
    by = c("destination", "year"),
    all.x = T
  )

  ## rename pop column
  setnames(labor_pct_df, "pop", "region_pop")

  return(labor_pct_df)
}


## function for summarizing labor data
calculate_labor_x_demg <- function(ref_labor_demog_yr) {
  labor_pct <- copy(ref_labor_demog_yr)

  ## county pop by demographic group
  labor_pct[, demo_pop := region_pop * pct]

  ## summarise over years
  labor_pct <- labor_pct[, .(
    sum_demo_emp = sum(demo_emp),
    sum_demo_comp_pv_h = sum(demo_comp_pv_h),
    sum_demo_comp_pv_l = sum(demo_comp_pv_l)
  ),
  by = .(demand_scenario, refining_scenario, oil_price_scenario, demo_cat, demo_group, title)
  ]

  return(labor_pct)
}

## function for creating labor output df
calculate_annual_labor_x_demg_hl <- function(main_path,
                                             ref_labor_demog_yr,
                                             refining_mortality,
                                             pop_ratios) {
  labor_pct <- copy(ref_labor_demog_yr)

  ## county pop by demographic group
  labor_pct[, demo_emp_revised := total_emp_revised * pct]

  ## summarise over years
  labor_pct <- labor_pct[, .(
    sum_demo_emp = sum(demo_emp),
    sum_demo_emp_revised = sum(demo_emp_revised),
    sum_demo_comp_pv_h = sum(demo_comp_pv_h),
    sum_demo_comp_pv_l = sum(demo_comp_pv_l)
  ),
  by = .(demand_scenario, refining_scenario, oil_price_scenario, demo_cat, demo_group, title, year)
  ]


  ## change scenario names, factor
  labor_pct[, scenario := paste0(demand_scenario, " demand - ", refining_scenario)]
  labor_pct[, scenario := gsub("LC1.", "Low ", scenario)]

  ## change historic to historical
  labor_pct[, refining_scenario := str_replace(refining_scenario, "historic", "historical")]
  labor_pct[, scenario := str_replace(scenario, "historic", "historical")]

  ## select columns
  labor_pct <- labor_pct[, .(
    scenario, demand_scenario, refining_scenario, oil_price_scenario, year, demo_cat, demo_group,
    title, sum_demo_emp, sum_demo_emp_revised, sum_demo_comp_pv_h, sum_demo_comp_pv_l
  )]

  ## compute per million stat
  ## ---------------------------------------------------------

  ## calc 2020 pop by demographic
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    left_join(pop_ratios) %>%
    as.data.table()

  pop_2020[, demo_pop := pop * pct]

  ## summarize by demographic group
  pop_2020 <- pop_2020[, .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]


  ## merge with 2020 pop
  labor_pct <- merge(labor_pct, pop_2020,
    by = c("demo_cat", "demo_group"),
    all.x = T
  )

  ## calculate per capita
  labor_pct[, demo_emp_pc_h := sum_demo_emp / pop_2020]
  labor_pct[, demo_emp_pmil_h := demo_emp_pc_h * 1e6]
  labor_pct[, demo_emp_pc_l := sum_demo_emp_revised / pop_2020]
  labor_pct[, demo_emp_pmil_l := demo_emp_pc_l * 1e6]
  labor_pct[, demo_comp_pc_h := sum_demo_comp_pv_h / pop_2020]
  labor_pct[, demo_comp_pc_pmil_h := demo_comp_pc_h * 1e6]
  labor_pct[, demo_comp_pc_l := sum_demo_comp_pv_l / pop_2020]
  labor_pct[, demo_comp_pc_pmil_l := demo_comp_pc_l * 1e6]
  ## remove pop
  labor_pct[, pop_2020 := NULL]
  labor_pct[, title := NULL]

  ## for renaming
  high_est_vec <- c("sum_demo_emp", "demo_emp_pc_h", "demo_emp_pmil_h", "sum_demo_comp_pv_h", "demo_comp_pc_h", "demo_comp_pc_pmil_h")
  low_est_vec <- c("sum_demo_emp_revised", "demo_emp_pc_l", "demo_emp_pmil_l", "sum_demo_comp_pv_l", "demo_comp_pc_l", "demo_comp_pc_pmil_l")

  labor_metric_df <- tibble(
    metric = c(high_est_vec, low_est_vec),
    metric_name = c(
      "employment",
      "employment_pc",
      "employment_pmil",
      "compensation_pv",
      "compensation_pv_pc",
      "compensation_pv_pmil",
      "employment",
      "employment_pc",
      "employment_pmil",
      "compensation_pv",
      "compensation_pv_pc",
      "compensation_pv_pmil"
    )
  )

  ## pivot longer
  labor_pct_long <- melt(labor_pct, id.vars = c("demo_cat", "demo_group", "scenario", "demand_scenario", "refining_scenario", "oil_price_scenario", "year"), variable.name = "metric", value.name = "value")
  labor_pct_long[, estimate := fifelse(metric %in% high_est_vec, "high", "low")]

  ## merge
  labor_pct_long <- merge(labor_pct_long, labor_metric_df,
    by = "metric",
    all.x = T
  )

  labor_pct_long <- labor_pct_long[, .(
    demo_cat, demo_group, scenario, demand_scenario,
    refining_scenario, oil_price_scenario, year, metric_name, estimate, value
  )]


  ## save df
  fwrite(labor_pct_long, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-update/fig-csv-files/", "labor_high_low_annual_outputs.csv"))
  # fwrite(labor_pct_long, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files", "labor_high_low_annual_outputs.csv"))


  return(labor_pct_long)
}

calc_county_level_outputs <- function(main_path,
                                      ref_labor_demog_yr,
                                      refining_mortality,
                                      ca_regions,
                                      raw_pop_income_2021) {
  ## compute county populations
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    as.data.table()

  ## census tract x county
  c_ct_df <- raw_pop_income_2021[state == "California"]
  c_ct_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  c_ct_df <- c_ct_df[, .(county, census_tract)]
  c_ct_df[, county := str_remove(county, " County")]

  ## merge with counties
  pop_2020 <- merge(pop_2020, c_ct_df,
    by = c("census_tract"),
    all.x = T
  )

  ## summarize by county
  pop_2020 <- pop_2020[, .(county_pop = sum(pop)), by = .(county)]

  ## compute county / region ratio
  county_region_ratio <- merge(pop_2020, ca_regions,
    by = "county"
  )

  ## sum region pop
  county_region_ratio[, region_pop := sum(county_pop), by = .(region)]

  ## calc ratio
  county_region_ratio[, county_ratio := county_pop / region_pop]

  county_region_ratio <- county_region_ratio[, .(county, region, county_pop, region_pop, county_ratio)]


  ## make labor outputs long and sum by county region
  labor_county_region_out <- ref_labor_demog_yr %>%
    mutate(demo_emp_revised = total_emp_revised * pct) %>%
    select(
      demand_scenario, refining_scenario, oil_price_scenario, destination, demo_cat, demo_group, title, year, demo_emp, demo_emp_revised, total_emp_revised,
      demo_comp_pv_h, demo_comp_pv_l
    ) %>%
    rename(region = destination)

  labor_county_region_out <- merge(labor_county_region_out, county_region_ratio,
    by = "region",
    allow.cartesian = T
  )


  labor_county_region_out <- labor_county_region_out %>%
    mutate(
      demo_emp_h_county = demo_emp * county_ratio,
      demo_emp_l_county = demo_emp_revised * county_ratio,
      demo_comp_PV_h_county = demo_comp_pv_h * county_ratio,
      demo_comp_PV_l_county = demo_comp_pv_l * county_ratio
    ) %>%
    select(
      county, demand_scenario, refining_scenario, oil_price_scenario, demo_cat, demo_group, title, year, demo_emp_h_county,
      demo_emp_l_county, demo_comp_PV_l_county, demo_comp_PV_h_county
    ) %>%
    pivot_longer(demo_emp_h_county:demo_comp_PV_h_county, names_to = "metric", values_to = "value") %>%
    group_by(county, demand_scenario, refining_scenario, oil_price_scenario, demo_cat, demo_group, title, metric) %>%
    summarise(value = sum(value)) %>%
    ungroup()

  # ## test
  # test_df <- labor_county_region_out %>%
  #   group_by(county, demand_scenario, refining_scenario, demo_cat, metric) %>%
  #   summarize(sum_value = sum(value)) %>%
  #   ungroup()
  #
  # ## total for county
  # total_county_test <- ref_labor_demog_yr %>%
  #   select(demand_scenario, refining_scenario, destination, year, total_emp, total_emp_revised,
  #          total_comp_PV_h, total_comp_PV_l) %>%
  #   unique() %>%
  #   rename(region = destination) %>%
  #   full_join(county_region_ratio) %>%
  #   mutate(total_emp_h_county = total_emp * county_ratio,
  #          total_emp_l_county = total_emp_revised * county_ratio,
  #          total_comp_PV_h_county = total_comp_PV_h * county_ratio,
  #          total_comp_PV_l_county = total_comp_PV_l * county_ratio) %>%
  #   select(county, demand_scenario, refining_scenario, year, total_emp_h_county,
  #          total_emp_l_county, total_comp_PV_l_county, total_comp_PV_h_county) %>%
  #   pivot_longer(total_emp_h_county:total_comp_PV_h_county, names_to = "metric", values_to = "value") %>%
  #   group_by(county, demand_scenario, refining_scenario, metric) %>%
  #   summarise(value = sum(value)) %>%
  #   ungroup()
  #


  ## for renaming
  high_est_vec <- c("demo_emp_h_county", "demo_comp_PV_h_county")
  low_est_vec <- c("demo_emp_l_county", "demo_comp_PV_l_county")

  labor_metric_df <- tibble(
    metric = c(high_est_vec, low_est_vec),
    metric_name = c(
      "employment",
      "compensation_pv",
      "employment",
      "compensation_pv"
    )
  )

  labor_county_out_df <- labor_county_region_out %>%
    left_join(labor_metric_df) %>%
    mutate(estimate = ifelse(metric %in% high_est_vec, "high", "low")) %>%
    select(county:title, metric_name, estimate, value) %>%
    as.data.table()


  ## save df
  fwrite(labor_county_out_df, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-update/fig-csv-files/", "labor_county_outputs.csv"))
  # fwrite(labor_county_out_df, file.path(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/fig-csv-files", "labor_county_outputs.csv"))


  return(labor_county_out_df)
}
