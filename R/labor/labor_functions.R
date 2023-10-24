## labor functions

create_prod_px_spread <- function(proc_oil_px_df) {

  crack_spread <- tibble(product = c("gasoline", "jet_fuel", "diesel"),
                         spread = c(23, 20, 23))
  
  crack_spread_ex <- expand.grid(year = c(2019:2045),
                              product = unique(crack_spread$product))
  
  crack_spread <- merge(crack_spread_ex, crack_spread,
                        all.x = T)
  
  prod_price <- copy(proc_oil_px_df)
  prod_price <- proc_oil_px_df[oil_price_scenario == "reference case"]
  prod_price <- merge(prod_price, crack_spread,
                      all = T)
  prod_price[, product_price := oil_price_usd_per_bbl + spread]
 
  prod_price
  
}


calc_labor_outputs <- function(proc_labor_df,
                               indiv_prod_output,
                               dt_refcap,
                               product_px) {


  ## add product for calculating price
  county_out_refining <- copy(indiv_prod_output)
  
  county_out_refining[, fuel := as.character(fuel)]
  
  county_out_refining[, product := fifelse(fuel %chin% c("gasoline", "drop-in gasoline"), "gasoline",
                                           fifelse(fuel %chin% c("diesel", "renewable diesel"), "diesel", "jet_fuel"))]

  ## merge with counties
  county_df <- dt_refcap[, .(site_id, county)]
  county_df[, site_id := as.character(site_id)]
  
  
  county_out_refining <- merge(county_out_refining, county_df,
                               by = c("site_id"),
                               all.x = T)

  # fill in missing counties
  county_out_refining[, county := fifelse(site_id == "342-2", "Contra Costa",
                                          fifelse(site_id == "99999", "Kern",
                                                  fifelse(site_id == "t-800", "Los Angeles", county)))]

  ## merge with prices
  product_df <- copy(product_px)
  product_df <- product_df[, .(year, product, product_price)]
  
  county_out_refining <- merge(county_out_refining, product_df,
                               by = c("year", "product"),
                               all.x = T)

  ## calculate revenue
  county_out_refining[, revenue := value * product_price]

  ## summarize at the county level
  county_out_refining_summary <- county_out_refining[, .(revenue = sum(revenue)), by = .(demand_scenario, refining_scenario,
                                                                                         year, county)]

  ## calculate labor impacts
  county_out_refining_summary[, county := fifelse(county == "Solano County", "Solano", county)]
  
  ## merge with labor multipliers, calculate labor vals
  county_out_labor <- merge(county_out_refining_summary, proc_labor_df,
                            by = c("county"),
                            all.x = T)
  
  county_out_labor[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult,
                           c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult,
                           c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
                           c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult,
                           c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult,
                           c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]

  county_out_labor[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
                           total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]
  
  
  county_out_labor <- county_out_labor[, .(demand_scenario, refining_scenario, county, year, revenue, total_emp, total_comp)]

  county_out_labor

}