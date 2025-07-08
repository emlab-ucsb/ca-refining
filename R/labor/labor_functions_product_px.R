## labor functions multiple prices

## Required libraries and functions
library(data.table)
library(dplyr)
library(tidyr)
library(readr)

## Source save functions
source("R/save_functions.R")

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

  crack_spread <- merge(crack_spread_ex, crack_spread, all.x = T)

  prod_price <- copy(proc_oil_px_df)
  # prod_price <- proc_oil_px_df[oil_price_scenario == "reference case"]

  prod_price <- merge(
    crack_spread,
    prod_price,
    by = c("year"),
    all = T,
    allow.cartesian = T
  )

  setDT(prod_price)

  prod_price[, product_price := oil_price_usd_per_bbl + spread]

  prod_price
}


calc_labor_outputs <- function(
  main_path,
  save_path,
  indiv_prod_output,
  dt_refcap,
  product_px,
  cpi2019,
  cpi2020,
  discount_rate,
  alpha_comp,
  alpha_emp,
  refin_locs_ct,
  dt_direct_multipliers
) {
  ## add product for calculating price
  county_out_refining <- copy(indiv_prod_output)

  county_out_refining[, fuel := as.character(fuel)]

  county_out_refining[,
    product := fifelse(
      fuel %chin% c("gasoline", "drop-in gasoline"),
      "gasoline",
      fifelse(fuel %chin% c("diesel", "renewable diesel"), "diesel", "jet_fuel")
    )
  ]

  ## merge with counties
  county_df <- dt_refcap[, .(site_id, county)]
  county_df[, site_id := as.character(site_id)]

  county_out_refining <- merge(
    county_out_refining,
    county_df,
    by = c("site_id"),
    all.x = T
  )

  # fill in missing counties
  county_out_refining[,
    county := fifelse(
      site_id == "342-2",
      "Contra Costa",
      fifelse(
        site_id == "99999",
        "Kern",
        fifelse(site_id == "t-800", "Los Angeles", county)
      )
    )
  ]

  ## create product px df with changing prices and just 2020
  product_px_2020 <- product_px[year == 2020, ]
  product_px_2020[, product_scenario := "2020 prices"]
  product_px_2020 <- product_px_2020[, .(
    product,
    spread,
    oil_price_scenario,
    oil_price_usd_per_bbl,
    product_price,
    product_scenario
  )]

  product_px_2020_all <- product_px[, .(year, product, oil_price_scenario)]
  product_px_2020_all <- merge(
    product_px_2020_all,
    product_px_2020,
    by = c("product", "oil_price_scenario"),
    allow.cartesian = T
  )

  product_px_adj <- copy(product_px)
  product_px_adj[, product_scenario := "changing prices"]

  product_px_adj <- rbind(product_px_adj, product_px_2020_all)

  ## merge with prices
  # product_df <- copy(product_px)
  product_px_adj <- product_px_adj[, .(
    product_scenario,
    year,
    oil_price_scenario,
    product,
    product_price
  )]

  county_out_refining <- merge(
    county_out_refining,
    product_px_adj,
    by = c("year", "product"),
    all.x = T,
    allow.cartesian = T
  )

  ## calculate revenue
  county_out_refining[, revenue := value * product_price]

  ## merge with census tracts
  ct_out_refining <- merge(
    county_out_refining,
    refin_locs_ct[, .(GEOID, site_id)],
    by = c("site_id"),
    all.x = T
  )

  ## step 1: summarize at the census tract level
  ct_out_refining_summary <- ct_out_refining[,
    .(production_bbl = sum(value), revenue = sum(revenue)),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      year,
      county,
      GEOID
    )
  ]

  ## fix columns
  ct_out_refining_summary[,
    county := fifelse(county == "Solano County", "Solano", county)
  ]

  ct_out_refining_summary[, w_tract_geocode := substr(GEOID, 1, 11)]

  ## step 2: merge with direct multipliers on the "w_tract_geocode"
  ct_out_refining_direct <- merge(
    ct_out_refining_summary,
    dt_direct_multipliers,
    by = c("w_tract_geocode"),
    all.x = TRUE,
    allow.cartesian = TRUE
  )

  ## step 3: divide revenue by $1 million and multiply it separately
  ## by emp.rev (employment multiplier) and ec.rev (compensation multiplier).
  ct_out_refining_direct <- ct_out_refining_direct[, `:=`(
    empl_direct_impact = (revenue / 1e6) * emp.rev,
    comp_direct_impact = (revenue / 1e6) * ec.rev
  )]

  ## step 4: summarize by “h” census tract. This is what you will use for
  ## Figure 1 D, Figure 4 D-F, and Figure 5 D-F.

  ct_out_refining_direct <- ct_out_refining_direct[,
    .(
      total_emp = sum(empl_direct_impact),
      total_comp = sum(comp_direct_impact)
    ),
    .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      year,
      h_tract_geocode
    )
  ]

  ct_out_refining_direct <- ct_out_refining_direct[, .(
    h_tract_geocode,
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    year,
    total_emp,
    total_comp
  )]

  ## convert comp to 2019 dollars
  ct_out_refining_direct[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]

  ## calc PV
  ct_out_refining_direct[,
    total_comp_PV := total_comp_usd19 / ((1 + discount_rate)^(year - 2019))
  ]

  ## rename columns
  setnames(
    ct_out_refining_direct,
    c("total_comp", "total_comp_usd19", "total_comp_PV"),
    c("total_comp_h", "total_comp_usd19_h", "total_comp_PV_h")
  )

  ## revision: unclear if we need to do the following
  ## --------------------------------------------------------------------------

  ## calculate the lower bound value
  ct_out_refining_direct <- ct_out_refining_direct %>%
    rename(census_tract = h_tract_geocode) %>%
    arrange(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      census_tract,
      year
    ) %>%
    group_by(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      census_tract
    ) %>%
    mutate(
      prev_emp = ifelse(year == 2020, NA, lag(total_emp)),
      total_emp_revised = ifelse(
        year == 2020,
        total_emp,
        total_emp - ((1 - alpha_emp) * prev_emp)
      ),
      prev_comp_usd19h = ifelse(year == 2020, NA, lag(total_comp_usd19_h)),
      total_comp_usd19_l = ifelse(
        year == 2020,
        total_comp_usd19_h,
        total_comp_usd19_h - ((1 - alpha_comp) * prev_comp_usd19h)
      )
    ) %>%
    ungroup() %>%
    as.data.table()

  review_df <- ct_out_refining_direct %>%
    select(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      census_tract,
      year,
      total_comp_usd19_h,
      prev_comp_usd19h,
      total_comp_usd19_l,
      total_emp,
      prev_emp,
      total_emp_revised
    )

  # ## save for review
  # To save review files according to structure.md:
  # simple_fwrite_repo(
  #   data = review_df,
  #   folder_path = file.path(save_path, "tables", "labor"),
  #   filename = "labor_result_for_review.csv"
  # )

  ## calc discounted low
  ct_out_refining_direct[,
    total_comp_PV_l := total_comp_usd19_l / ((1 + discount_rate)^(year - 2019))
  ]

  ## select columns
  ct_out_refining_direct <- ct_out_refining_direct[, .(
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    census_tract,
    year,
    total_emp,
    total_emp_revised,
    total_comp_h,
    total_comp_usd19_h,
    total_comp_usd19_l,
    total_comp_PV_h,
    total_comp_PV_l
  )]

  return(ct_out_refining_direct)

  # ## summarize at the county level
  # county_out_refining_summary <- county_out_refining[, .(
  #   production_bbl = sum(value),
  #   revenue = sum(revenue)
  # ), by = .(
  #   demand_scenario, refining_scenario,
  #   oil_price_scenario, year, county
  # )]
  #
  #   ## calculate labor impacts
  #   county_out_refining_summary[, county := fifelse(county == "Solano County", "Solano", county)]
  #
  #
  #   ## merge with labor multipliers, calculate labor vals
  #   county_out_labor <- merge(county_out_refining_summary, proc_labor_dest_df,
  #     by = c("county"),
  #     all.x = T,
  #     allow.cartesian = T
  #   )

  # county_out_labor[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult,
  #                          c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult,
  #                          c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
  #                          c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult,
  #                          c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult,
  #                          c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]
  #
  # county_out_labor[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
  #                          total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]
  #
  #   county_out_labor[, ":="(c.emp = (revenue / (10^6)) * employment,
  #     c.comp = (revenue / (10^6)) * emp_comp)]
  #
  #   county_out_labor <- county_out_labor[, .(
  #     total_production_bbl = sum(production_bbl),
  #     total_revenue = sum(revenue),
  #     total_emp = sum(c.emp),
  #     total_comp = sum(c.comp)
  #   ), .(demand_scenario, refining_scenario, oil_price_scenario, year, destination)]

  # ## convert to 2019 dollars
  # county_out_labor[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]
  #
  # ## calc PV
  # county_out_labor[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate)^(year - 2019))]
  #
  # ## rename columns
  # setnames(county_out_labor, c("total_comp", "total_comp_usd19", "total_comp_PV"), c("total_comp_h", "total_comp_usd19_h", "total_comp_PV_h"))
  #
  # ## calculate the lower bound value
  # county_out_labor <- county_out_labor %>%
  #   arrange(demand_scenario, refining_scenario, oil_price_scenario, destination, year) %>%
  #   group_by(demand_scenario, refining_scenario, oil_price_scenario, destination) %>%
  #   mutate(
  #     prev_emp = ifelse(year == 2020, NA, lag(total_emp)),
  #     total_emp_revised = ifelse(year == 2020, total_emp, total_emp - ((1 - alpha_emp) * prev_emp)),
  #     prev_comp_usd19h = ifelse(year == 2020, NA, lag(total_comp_usd19_h)),
  #     total_comp_usd19_l = ifelse(year == 2020, total_comp_usd19_h, total_comp_usd19_h - ((1 - alpha_comp) * prev_comp_usd19h))
  #   ) %>%
  #   ungroup() %>%
  #   as.data.table()
  #
  # review_df <- county_out_labor %>%
  #   select(demand_scenario, refining_scenario, oil_price_scenario, destination, year, total_production_bbl, total_revenue, total_comp_usd19_h, prev_comp_usd19h, total_comp_usd19_l, total_emp, total_emp_revised)
  #
  # ## save for review
  # To save review files according to structure.md:
  # simple_fwrite_repo(
  #   data = review_df,
  #   folder_path = file.path(save_path, "tables", "labor"),
  #   filename = "labor_result_for_review.csv"
  # )
  #
  # ## calc discounted low
  # county_out_labor[, total_comp_PV_l := total_comp_usd19_l / ((1 + discount_rate)^(year - 2019))]
  #
  # ## select columns
  # county_out_labor <- county_out_labor[, .(
  #   demand_scenario, refining_scenario, oil_price_scenario, destination, year, total_emp, total_emp_revised, total_comp_h,
  #   total_comp_usd19_h, total_comp_usd19_l, total_comp_PV_h, total_comp_PV_l
  # )]
  #
  # county_out_labor
}


calc_state_direct_impacts <- function(annual_direct_labor) {
  dt <- annual_direct_labor

  dt_state <- dt[,
    .(
      state_emp_h = sum(total_emp),
      state_emp_l = sum(total_emp_revised),
      state_comp_h = sum(total_comp_h),
      state_comp_usd19_h = sum(total_comp_usd19_h),
      state_comp_PV_h = sum(total_comp_PV_h),
      state_comp_usd19_l = sum(total_comp_usd19_l),
      state_comp_PV_l = sum(total_comp_PV_l)
    ),
    .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      year
    )
  ]

  return(dt_state)
}


calc_labor_all_impacts_outputs <- function(
  main_path,
  save_path,
  state_annual_direct_impacts,
  indiv_prod_output,
  dt_refcap,
  product_px,
  cpi2019,
  cpi2020,
  discount_rate,
  alpha_comp,
  alpha_emp,
  dt_indirect_state_multipliers,
  indirect_induced_mult
) {
  ## add product for calculating price
  county_out_refining <- copy(indiv_prod_output)

  county_out_refining[, fuel := as.character(fuel)]

  county_out_refining[,
    product := fifelse(
      fuel %chin% c("gasoline", "drop-in gasoline"),
      "gasoline",
      fifelse(fuel %chin% c("diesel", "renewable diesel"), "diesel", "jet_fuel")
    )
  ]

  ## merge with counties
  county_df <- dt_refcap[, .(site_id, county)]
  county_df[, site_id := as.character(site_id)]

  county_out_refining <- merge(
    county_out_refining,
    county_df,
    by = c("site_id"),
    all.x = T
  )

  # fill in missing counties
  county_out_refining[,
    county := fifelse(
      site_id == "342-2",
      "Contra Costa",
      fifelse(
        site_id == "99999",
        "Kern",
        fifelse(site_id == "t-800", "Los Angeles", county)
      )
    )
  ]

  ## create product px df with changing prices and just 2020
  product_px_2020 <- product_px[year == 2020, ]
  product_px_2020[, product_scenario := "2020 prices"]
  product_px_2020 <- product_px_2020[, .(
    product,
    spread,
    oil_price_scenario,
    oil_price_usd_per_bbl,
    product_price,
    product_scenario
  )]

  product_px_2020_all <- product_px[, .(year, product, oil_price_scenario)]
  product_px_2020_all <- merge(
    product_px_2020_all,
    product_px_2020,
    by = c("product", "oil_price_scenario"),
    allow.cartesian = T
  )

  product_px_adj <- copy(product_px)
  product_px_adj[, product_scenario := "changing prices"]

  product_px_adj <- rbind(product_px_adj, product_px_2020_all)

  product_px_adj <- product_px_adj[, .(
    product_scenario,
    year,
    oil_price_scenario,
    product,
    product_price
  )]

  county_out_refining <- merge(
    county_out_refining,
    product_px_adj,
    by = c("year", "product"),
    all.x = T,
    allow.cartesian = T
  )

  ## calculate revenue
  county_out_refining[, revenue := value * product_price]

  ## step 1: summarize at the state level
  state_out_refining_summary <- county_out_refining[,
    .(
      production_bbl = sum(value),
      revenue = sum(revenue)
    ),
    by = .(
      product_scenario,
      demand_scenario,
      refining_scenario,
      oil_price_scenario,
      year
    )
  ]

  ## step 2: process induced df
  ## step 3: skip
  total_indir_induc_multipliers <- dt_indirect_state_multipliers[,
    .(
      emp.rev = sum(emp.rev),
      ec.rev = sum(ec.rev),
      emp.li = sum(emp.li, na.rm = T),
      ec.li = sum(ec.li, na.rm = T)
    ),
    by = .(DestinationRegion)
  ]

  ## step 4: divide revenue by $1 million.
  ## multiple the resulting number separately by emp.rev and ec.rev.
  ## You should now have the combined indirect and induced impacts by year and scenario.
  state_out_refining_summary[, `:=`(
    empl_indir_induc_impact = (revenue / 1e6) *
      total_indir_induc_multipliers$emp.rev[1],
    comp_indir_induc_impact = (revenue / 1e6) *
      total_indir_induc_multipliers$ec.rev[1]
  )]

  state_out_refining_summary[, indirect_induced_scenario := "baseline"]

  ## step 4.5 make bartik correction for indirect and induced impacts
  state_out_refining_summary_bartik <- copy(state_out_refining_summary)

  state_out_refining_summary_bartik[, `:=`(
    indirect_induced_scenario = "bartik-corrected",
    empl_indir_induc_impact = empl_indir_induc_impact * indirect_induced_mult,
    comp_indir_induc_impact = comp_indir_induc_impact * indirect_induced_mult
  )]

  ## bind
  state_out_refining_summary <- rbind(
    state_out_refining_summary,
    state_out_refining_summary_bartik
  )

  ## step 5: merge with state direct impacts
  state_out_refining_all_impacts <- merge(
    state_out_refining_summary,
    state_annual_direct_impacts,
    by = c(
      "product_scenario",
      "year",
      "demand_scenario",
      "refining_scenario",
      "oil_price_scenario"
    ),
    all = T
  )

  ## step 6: sum direct and indirct/induced impacts
  state_out_refining_all_impacts <- state_out_refining_all_impacts[,
    state_comp_all_impacts := comp_indir_induc_impact + state_comp_h
  ]

  state_out_refining_all_impacts <- state_out_refining_all_impacts[, .(
    product_scenario,
    demand_scenario,
    refining_scenario,
    oil_price_scenario,
    indirect_induced_scenario,
    year,
    production_bbl,
    revenue,
    state_comp_all_impacts
  )]

  ## merge with BAU to compute relative impact and induced and indirect impacts
  state_out_refining_all_impacts_bau <- filter(
    state_out_refining_all_impacts,
    demand_scenario == "BAU" &
      refining_scenario == "historic production" &
      oil_price_scenario == "reference case"
  )

  state_out_refining_all_impacts_bau <- state_out_refining_all_impacts_bau[, .(
    product_scenario,
    indirect_induced_scenario,
    year,
    state_comp_all_impacts
  )]

  setnames(
    state_out_refining_all_impacts_bau,
    c("state_comp_all_impacts"),
    c("state_comp_all_impacts_bau")
  )

  ## step 7: lag state_comp_all_impacts by one year.
  ## step 8: multiply lagged state_comp_all_impacts by 0.8 and
  ## then separately by emp.li and ec.li. This will leave you with
  ## induced employment and compensation effects from rehires’ labor
  ## income at their new jobs for each year and scenario.
  state_out_refining_all_impacts <- merge(
    state_out_refining_all_impacts,
    state_out_refining_all_impacts_bau,
    by = c("product_scenario", "indirect_induced_scenario", "year"),
    all.x = T
  ) %>%
    arrange(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      year
    ) %>%
    group_by(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario
    ) %>%
    mutate(
      prev_comp = ifelse(year == 2020, NA, lag(state_comp_all_impacts)),
      prev_comp_bau = ifelse(year == 2020, NA, lag(state_comp_all_impacts_bau)),
      state_comp_all_impacts_l = ifelse(
        year == 2020,
        state_comp_all_impacts,
        state_comp_all_impacts - ((1 - alpha_comp) * prev_comp)
      ),
      state_comp_all_impacts_l_bau = ifelse(
        year == 2020,
        state_comp_all_impacts_bau,
        state_comp_all_impacts_bau - ((1 - alpha_comp) * prev_comp_bau)
      ),
      state_comp_all_impacts_l_relative = state_comp_all_impacts_l -
        state_comp_all_impacts_l_bau,
      state_comp_all_impacts_l_relative_adj = ifelse(
        state_comp_all_impacts_l_relative > 0,
        NA,
        state_comp_all_impacts_l_relative
      ),
      prev_comp_l = ifelse(
        year == 2020,
        NA,
        lag(state_comp_all_impacts_l_relative_adj)
      ),
      state_comp_emp_li = ifelse(
        year == 2020,
        NA,
        (prev_comp_l / 1e6) * total_indir_induc_multipliers$emp.li[1]
      ),
      state_comp_ec_li = ifelse(
        year == 2020,
        NA,
        (prev_comp_l / 1e6) * total_indir_induc_multipliers$ec.li[1]
      )
    ) %>%
    ungroup() %>%
    as.data.table()

  ## save for review
  # To save step outputs according to structure.md:
  # simple_fwrite_repo(
  #   data = state_out_refining_all_impacts,
  #   folder_path = file.path(save_path, "tables", "labor"),
  #   filename = "step_8_output_for_review.csv"
  # )

  ## step 9: calc revised statewide indirect and induced impact that is equal to
  ## the indirect and induced impact from step 4 - direct and indirect impact from
  ## step 7.
  state_out_refining_summary <- merge(
    state_out_refining_summary,
    state_out_refining_all_impacts[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario,
      year,
      state_comp_all_impacts,
      state_comp_emp_li,
      state_comp_ec_li
    )],
    by = c(
      "product_scenario",
      "indirect_induced_scenario",
      "demand_scenario",
      "refining_scenario",
      "oil_price_scenario",
      "year"
    ),
    all = T
  )

  ## subtract indirect and induced values computed in step 8 (if year 2020, use original 2020 value)
  state_out_refining_summary <- arrange(
    state_out_refining_summary,
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    year
  ) %>%
    group_by(
      demand_scenario,
      refining_scenario,
      product_scenario,
      indirect_induced_scenario,
      oil_price_scenario
    ) %>%
    mutate(
      prev_comp = ifelse(year == 2020, NA, lag(comp_indir_induc_impact)),
      prev_empl = ifelse(year == 2020, NA, lag(empl_indir_induc_impact)),
      comp_indir_induc_impact_l = ifelse(
        year == 2020,
        comp_indir_induc_impact,
        comp_indir_induc_impact - ((1 - alpha_comp) * prev_comp)
      ),
      empl_indir_induc_impact_l = ifelse(
        year == 2020,
        empl_indir_induc_impact,
        empl_indir_induc_impact - ((1 - alpha_emp) * prev_empl)
      )
    ) %>%
    as.data.table()

  state_out_refining_summary[, `:=`(
    empl_indir_induc_impact_l = fifelse(
      year == 2020,
      empl_indir_induc_impact_l,
      empl_indir_induc_impact_l - state_comp_emp_li
    ),
    comp_indir_induc_impact_l = fifelse(
      year == 2020,
      comp_indir_induc_impact_l,
      comp_indir_induc_impact_l - state_comp_ec_li
    )
  )]

  ## create df with low and high induced and indirect impacts, convert to 2019, calc pv
  state_out_labor_induc_indir <- state_out_refining_summary[, .(
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    year,
    empl_indir_induc_impact,
    empl_indir_induc_impact_l,
    comp_indir_induc_impact,
    comp_indir_induc_impact_l
  )]

  setnames(
    state_out_labor_induc_indir,
    c("empl_indir_induc_impact", "comp_indir_induc_impact"),
    c("empl_indir_induc_impact_h", "comp_indir_induc_impact_h")
  )

  ## convert into to 2019 dollars
  state_out_labor_induc_indir[, `:=`(
    comp_indir_induc_impact_h_usd19 = comp_indir_induc_impact_h *
      cpi2019 /
      cpi2020,
    comp_indir_induc_impact_l_usd19 = comp_indir_induc_impact_l *
      cpi2019 /
      cpi2020
  )]

  ## calc PV
  state_out_labor_induc_indir[, `:=`(
    comp_indir_induc_impact_h_PV = comp_indir_induc_impact_h_usd19 /
      ((1 + discount_rate)^(year - 2019)),
    comp_indir_induc_impact_l_PV = comp_indir_induc_impact_l_usd19 /
      ((1 + discount_rate)^(year - 2019))
  )]

  ## merge with direct impact
  state_out_labor_induc_indir <- merge(
    state_out_labor_induc_indir,
    state_annual_direct_impacts[, .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      year,
      state_emp_h,
      state_emp_l,
      state_comp_h,
      state_comp_usd19_h,
      state_comp_PV_h,
      state_comp_usd19_l,
      state_comp_PV_l
    )]
  )

  ## compute total impact, high and low
  state_out_labor_induc_indir[, `:=`(
    empl_all_impacts_h = state_emp_h + empl_indir_induc_impact_h,
    emp_all_impacts_l = state_emp_l + empl_indir_induc_impact_l,
    comp_all_impacts_h = comp_indir_induc_impact_h + state_comp_h,
    comp_all_impacts_usd19_h = comp_indir_induc_impact_h_usd19 +
      state_comp_usd19_h,
    comp_all_impacts_PV_h = comp_indir_induc_impact_h_PV + state_comp_PV_h,
    comp_all_impacts_usd19_l = comp_indir_induc_impact_l_usd19 +
      state_comp_usd19_l,
    comp_all_impacts_PV_l = comp_indir_induc_impact_l_PV + state_comp_PV_l
  )]

  ## final df
  state_out_labor_all_impacts <- state_out_labor_induc_indir[, .(
    demand_scenario,
    refining_scenario,
    product_scenario,
    indirect_induced_scenario,
    oil_price_scenario,
    year,
    state_emp_h,
    empl_indir_induc_impact_h,
    empl_all_impacts_h,
    state_emp_l,
    empl_indir_induc_impact_l,
    emp_all_impacts_l,
    state_comp_h,
    comp_indir_induc_impact_h,
    comp_all_impacts_h,
    state_comp_usd19_h,
    comp_indir_induc_impact_h_usd19,
    comp_all_impacts_usd19_h,
    state_comp_PV_h,
    comp_indir_induc_impact_h_PV,
    comp_all_impacts_PV_h,
    state_comp_usd19_l,
    comp_indir_induc_impact_l_usd19,
    comp_all_impacts_usd19_l,
    state_comp_PV_l,
    comp_indir_induc_impact_l_PV,
    comp_all_impacts_PV_l
  )]

  # state_out_labor_all_impacts <- state_out_labor_all_impacts |>
  #   rename(comp_dir_impact_h = state_comp_h,
  #          comp_dir_usd19_h = state_comp_usd19_h,
  #          comp_dir_PV_h = state_comp_PV_h,
  #          comp_dir_usd19_l = state_comp_usd19_l,
  #          comp_dir_PV_l  = state_comp_PV_l) |>
  #   as.data.table()

  ## save for review
  # To save labor outputs according to structure.md:
  # simple_fwrite_repo(
  #   data = state_out_labor_all_impacts,
  #   folder_path = file.path(save_path, "tables", "labor"),
  #   filename = "state_out_labor_all_impacts.csv"
  # )

  return(state_out_labor_all_impacts)

  # ## summarize at the county level
  # county_out_refining_summary <- county_out_refining[, .(
  #   production_bbl = sum(value),
  #   revenue = sum(revenue)
  # ), by = .(
  #   demand_scenario, refining_scenario,
  #   oil_price_scenario, year, county
  # )]
  #
  #
  # ## calculate labor impacts
  # county_out_refining_summary[, county := fifelse(county == "Solano County", "Solano", county)]
  #
  #
  # ## merge with labor multipliers, calculate labor vals
  # county_out_labor <- merge(county_out_refining_summary, proc_labor_dest_df,
  #   by = c("county"),
  #   all.x = T,
  #   allow.cartesian = T
  # )
  #
  # # county_out_labor[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult,
  # #                          c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult,
  # #                          c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
  # #                          c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult,
  # #                          c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult,
  # #                          c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]
  # #
  # # county_out_labor[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
  # #                          total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]
  #
  #
  # county_out_labor[, ":="(c.emp = (revenue / (10^6)) * employment,
  #   c.comp = (revenue / (10^6)) * emp_comp)]
  #
  # county_out_labor <- county_out_labor[, .(
  #   total_production_bbl = sum(production_bbl),
  #   total_revenue = sum(revenue),
  #   total_emp = sum(c.emp),
  #   total_comp = sum(c.comp)
  # ), .(
  #   demand_scenario,
  #   refining_scenario,
  #   oil_price_scenario,
  #   impact_type,
  #   year,
  #   destination
  # )]
  #
  # ## convert to 2019 dollars
  # county_out_labor[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]
  #
  # ## calc PV
  # county_out_labor[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate)^(year - 2019))]
  #
  # ## rename columns
  # setnames(county_out_labor, c("total_comp", "total_comp_usd19", "total_comp_PV"), c("total_comp_h", "total_comp_usd19_h", "total_comp_PV_h"))
  #
  # ## calculate the lower bound value
  # county_out_labor <- county_out_labor %>%
  #   arrange(demand_scenario, refining_scenario, oil_price_scenario, impact_type, destination, year) %>%
  #   group_by(demand_scenario, refining_scenario, oil_price_scenario, impact_type, destination) %>%
  #   mutate(
  #     prev_emp = ifelse(year == 2020, NA, lag(total_emp)),
  #     total_emp_revised = ifelse(year == 2020, total_emp, total_emp - ((1 - alpha_emp) * prev_emp)),
  #     prev_comp_usd19h = ifelse(year == 2020, NA, lag(total_comp_usd19_h)),
  #     total_comp_usd19_l = ifelse(year == 2020, total_comp_usd19_h, total_comp_usd19_h - ((1 - alpha_comp) * prev_comp_usd19h))
  #   ) %>%
  #   ungroup() %>%
  #   as.data.table()
  #
  # review_df <- county_out_labor %>%
  #   select(
  #     demand_scenario, refining_scenario, oil_price_scenario, impact_type,
  #     destination, year, total_production_bbl, total_revenue, total_comp_usd19_h,
  #     prev_comp_usd19h, total_comp_usd19_l, total_emp, total_emp_revised
  #   )
  #
  # ## save file
  # To save review files according to structure.md:
  # simple_fwrite_repo(
  #   data = review_df,
  #   folder_path = file.path(save_path, "tables", "labor"),
  #   filename = "labor_result_x_impact_type_for_review.csv"
  # )
  #
  #
  # ## calc discounted low
  # county_out_labor[, total_comp_PV_l := total_comp_usd19_l / ((1 + discount_rate)^(year - 2019))]
  #
  # ## select columns
  # county_out_labor <- county_out_labor[, .(
  #   demand_scenario, refining_scenario, oil_price_scenario, impact_type, destination, year, total_emp, total_emp_revised, total_comp_h,
  #   total_comp_usd19_h, total_comp_usd19_l, total_comp_PV_h, total_comp_PV_l
  # )]
  #
  #
  # ## save file
  # To save county outputs according to structure.md:
  # simple_fwrite_repo(
  #   data = county_out_labor,
  #   folder_path = file.path(save_path, "tables", "labor"),
  #   filename = "labor_result_x_impact_type.csv"
  # )
  #
  #
  # county_out_labor
}


## labor results grouped by demographic

calculate_labor_x_demg_annual <- function(
  main_path,
  save_path,
  annual_direct_labor,
  pop_ratios
) {
  # ## census pop
  # census_pop <- refining_mortality %>%
  #   ungroup() %>%
  #   filter(year == 2020) %>%
  #   select(census_tract, pop) %>%
  #   unique() %>%
  #   rename(total_pop = pop) %>%
  #   as.data.table()

  ## merge with demographic info
  ct_out_demo <- merge(
    annual_direct_labor,
    pop_ratios,
    by = c("census_tract"),
    all.x = T,
    allow.cartesian = T
  )

  ct_out_demo <- ct_out_demo[, .(
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    census_tract,
    demo_cat,
    demo_group,
    title,
    pct,
    year,
    total_emp,
    total_emp_revised,
    total_comp_h,
    total_comp_usd19_h,
    total_comp_usd19_l,
    total_comp_PV_h,
    total_comp_PV_l
  )]

  #
  # ## merge with population
  # ct_out_demo <- merge(ct_out_demo, census_pop,
  #                      by = c("census_tract"),
  #                      all.x = T)

  ## multiply by pct
  ct_out_demo[, `:=`(
    demo_emp = total_emp * pct,
    demo_emp_revised = total_emp_revised * pct,
    demo_comp_h = total_comp_h * pct,
    demo_comp_usd19_h = total_comp_usd19_h * pct,
    demo_comp_usd19_l = total_comp_usd19_l * pct,
    demo_comp_PV_h = total_comp_PV_h * pct,
    demo_comp_PV_l = total_comp_PV_l * pct
  )]

  ## summarize at the state level
  state_demo_labor_out <- ct_out_demo[,
    .(
      sum_demo_emp = sum(demo_emp),
      sum_demo_emp_revised = sum(demo_emp_revised),
      sum_demo_usd19_h = sum(demo_comp_usd19_h),
      sum_demo_usd19_l = sum(demo_comp_usd19_l),
      sum_demo_comp_pv_h = sum(demo_comp_PV_h),
      sum_demo_comp_pv_l = sum(demo_comp_PV_l)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      year,
      demo_cat,
      demo_group,
      title
    )
  ]

  state_demo_labor_out <- state_demo_labor_out[, .(
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    demo_cat,
    demo_group,
    title,
    year,
    sum_demo_emp,
    sum_demo_emp_revised,
    sum_demo_usd19_h,
    sum_demo_usd19_l,
    sum_demo_comp_pv_h,
    sum_demo_comp_pv_l
  )]

  ## save for review
  simple_fwrite_repo(
    data = state_demo_labor_out,
    folder_path = file.path(save_path, "intermediate", "labor"),
    filename = "state_labor_direct_impacts_demo_annual.csv"
  )

  return(state_demo_labor_out)

  #
  # ## get county and census tracts
  # c_ct_df <- raw_pop_income_2021[state == "California"]
  # c_ct_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  # c_ct_df <- c_ct_df[, .(county, census_tract)]
  # c_ct_df[, county := str_remove(county, " County")]
  #
  # c_ct_df <- merge(c_ct_df, ca_regions,
  #   by = c("county"),
  #   allow.cartesian = T
  # )
  #
  #   ## get pop from refining_mortality
  #   pop_df <- refining_mortality %>%
  #     select(census_tract, year, pop)
  #
  #   # ## get
  # ratio_df <- copy(county_grp_pop_ratios)
  # setnames(ratio_df, "region", "destination")
  #
  # ## merge
  # labor_pct_df <- merge(annual_labor, ratio_df,
  #   by = c("destination"),
  #   allow.cartesian = T
  # )
  #
  # ## multiply by pct
  # # labor_pct_df[, demo_emp := total_emp_revised * pct]
  # labor_pct_df[, demo_emp := total_emp * pct]
  # labor_pct_df[, demo_comp_pv_h := total_comp_PV_h * pct]
  # labor_pct_df[, demo_comp_pv_l := total_comp_PV_l * pct]
  #
  #   ## merge with population
  #   labor_pct_df <- merge(labor_pct_df, pop_df,
  #     by = c("destination", "year"),
  #     all.x = T
  #   )
  #
  #   ## rename pop column
  #   setnames(labor_pct_df, "pop", "region_pop")
  #
  #   return(labor_pct_df)
}


## function for summarizing labor data
calculate_labor_x_demg <- function(ref_labor_demog_yr) {
  labor_pct <- copy(ref_labor_demog_yr)

  # ## county pop by demographic group
  # labor_pct[, demo_pop := region_pop * pct]

  ## summarise over years
  labor_pct <- labor_pct[,
    .(
      sum_demo_emp = sum(sum_demo_emp),
      sum_demo_comp_pv_h = sum(sum_demo_comp_pv_h),
      sum_demo_comp_pv_l = sum(sum_demo_comp_pv_l)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      demo_cat,
      demo_group,
      title
    )
  ]

  return(labor_pct)
}

## function for creating labor output df
calculate_annual_labor_x_demg_hl <- function(
  main_path,
  save_path,
  ref_labor_demog_yr,
  refining_mortality,
  pop_ratios
) {
  labor_pct <- copy(ref_labor_demog_yr)

  # ## county pop by demographic group
  # labor_pct[, demo_emp_revised := total_emp_revised * pct]
  #
  #   ## summarise over years
  #   labor_pct <- labor_pct[, .(
  #     sum_demo_emp = sum(demo_emp),
  #     sum_demo_emp_revised = sum(demo_emp_revised),
  #     sum_demo_comp_pv_h = sum(demo_comp_pv_h),
  #     sum_demo_comp_pv_l = sum(demo_comp_pv_l)
  #   ),
  #   by = .(demand_scenario, refining_scenario, oil_price_scenario, demo_cat, demo_group, title, year)
  #   ]

  ## change scenario names, factor
  labor_pct[,
    scenario := paste0(demand_scenario, " demand - ", refining_scenario)
  ]
  labor_pct[, scenario := gsub("LC1.", "Low ", scenario)]

  ## change historic to historical
  labor_pct[,
    refining_scenario := str_replace(
      refining_scenario,
      "historic",
      "historical"
    )
  ]
  labor_pct[, scenario := str_replace(scenario, "historic", "historical")]

  # ## select columns
  # labor_pct <- labor_pct[, .(
  #   scenario, demand_scenario, refining_scenario, oil_price_scenario, year, demo_cat, demo_group,
  #   title, sum_demo_emp, sum_demo_emp_revised, sum_demo_comp_pv_h, sum_demo_comp_pv_l
  # )]

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
  pop_2020 <- pop_2020[,
    .(pop_2020 = sum(demo_pop)),
    by = .(demo_group, demo_cat)
  ]

  ## merge with 2020 pop
  labor_pct <- merge(
    labor_pct,
    pop_2020,
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
  high_est_vec <- c(
    "sum_demo_emp",
    "demo_emp_pc_h",
    "demo_emp_pmil_h",
    "sum_demo_comp_pv_h",
    "demo_comp_pc_h",
    "demo_comp_pc_pmil_h"
  )
  low_est_vec <- c(
    "sum_demo_emp_revised",
    "demo_emp_pc_l",
    "demo_emp_pmil_l",
    "sum_demo_comp_pv_l",
    "demo_comp_pc_l",
    "demo_comp_pc_pmil_l"
  )

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
  labor_pct_long <- melt(
    labor_pct,
    id.vars = c(
      "product_scenario",
      "demo_cat",
      "demo_group",
      "scenario",
      "demand_scenario",
      "refining_scenario",
      "oil_price_scenario",
      "year"
    ),
    variable.name = "metric",
    value.name = "value"
  )
  labor_pct_long[, estimate := fifelse(metric %in% high_est_vec, "high", "low")]

  ## merge
  labor_pct_long <- merge(
    labor_pct_long,
    labor_metric_df,
    by = "metric",
    all.x = T
  )

  labor_pct_long <- labor_pct_long[, .(
    demo_cat,
    demo_group,
    scenario,
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    year,
    metric_name,
    estimate,
    value
  )]

  ## save df
  simple_fwrite_repo(
    data = labor_pct_long,
    folder_path = file.path(save_path, "tables", "labor"),
    filename = "labor_high_low_annual_outputs.csv"
  )
  # Legacy comment: Previously saved to "fig-csv-files"

  return(labor_pct_long)
}

calc_county_level_outputs <- function(
  main_path,
  save_path,
  annual_direct_labor,
  refining_mortality,
  raw_pop_income_2021,
  pop_ratios
) {
  ## merge with demographic info
  ct_out_demo <- merge(
    annual_direct_labor,
    pop_ratios,
    by = c("census_tract"),
    all.x = T,
    allow.cartesian = T
  )

  ct_out_demo <- ct_out_demo[, .(
    demand_scenario,
    refining_scenario,
    product_scenario,
    oil_price_scenario,
    census_tract,
    demo_cat,
    demo_group,
    title,
    pct,
    year,
    total_emp,
    total_emp_revised,
    total_comp_h,
    total_comp_usd19_h,
    total_comp_usd19_l,
    total_comp_PV_h,
    total_comp_PV_l
  )]

  ## multiply by pct
  ct_out_demo[, `:=`(
    demo_emp = total_emp * pct,
    demo_emp_revised = total_emp_revised * pct,
    demo_comp_h = total_comp_h * pct,
    demo_comp_usd19_h = total_comp_usd19_h * pct,
    demo_comp_usd19_l = total_comp_usd19_l * pct,
    demo_comp_PV_h = total_comp_PV_h * pct,
    demo_comp_PV_l = total_comp_PV_l * pct
  )]

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
  pop_2020 <- merge(pop_2020, c_ct_df, by = c("census_tract"), all.x = T)

  pop_2020 <- pop_2020[, .(census_tract, pop, county)]

  ## merge
  labor_county_out <- merge(
    ct_out_demo,
    pop_2020,
    by = c("census_tract"),
    all.x = T
  )

  ## direct impact
  ## make labor outputs long and sum by county region
  labor_county_out <- labor_county_out %>%
    select(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      demo_cat,
      demo_group,
      title,
      census_tract,
      county,
      pop,
      year,
      demo_emp,
      demo_emp_revised,
      demo_comp_PV_h,
      demo_comp_PV_l
    )

  ## summarize by county
  labor_county_out <- labor_county_out[,
    .(
      county_pop = sum(pop),
      demo_emp = sum(demo_emp),
      demo_emp_revised = sum(demo_emp_revised),
      demo_comp_PV_h = sum(demo_comp_PV_h),
      demo_comp_PV_l = sum(demo_comp_PV_l)
    ),
    by = .(
      demand_scenario,
      refining_scenario,
      product_scenario,
      oil_price_scenario,
      demo_cat,
      demo_group,
      title,
      county
    )
  ]

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
  high_est_vec <- c("demo_emp", "demo_comp_PV_h")
  low_est_vec <- c("demo_emp_revised", "demo_comp_PV_l")

  labor_metric_df <- tibble(
    metric = c(high_est_vec, low_est_vec),
    metric_name = c(
      "employment",
      "compensation_pv",
      "employment",
      "compensation_pv"
    )
  )

  labor_county_out_df <- labor_county_out %>%
    pivot_longer(
      demo_emp:demo_comp_PV_l,
      names_to = "metric",
      values_to = "value"
    ) %>%
    left_join(labor_metric_df) %>%
    mutate(estimate = ifelse(metric %in% high_est_vec, "high", "low")) %>%
    select(demand_scenario:county_pop, metric_name, estimate, value) %>%
    as.data.table()

  ## save df
  simple_fwrite_repo(
    data = labor_county_out_df,
    folder_path = file.path(save_path, "tables", "labor"),
    filename = "labor_county_outputs.csv"
  )
  # Legacy comment: Previously saved to "fig-csv-files"

  return(labor_county_out_df)
}
