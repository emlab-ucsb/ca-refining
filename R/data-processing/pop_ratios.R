## demographic data processing

## calculate race, DAC, and poverty ratios with census data
calc_pop_ratios = function(raw_pop_income_2021,
                                 raw_pop_poverty,
                                 refining_mortality) {
  
  
  ## get proportions for race and merge
  ## -------------------------------------------------------------------------------
  pop_race_df <- copy(raw_pop_income_2021)
  
  ## extract census tract, process
  pop_race_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  pop_race_df[, year := NULL]
  pop_race_df <- pop_race_df[state == "California"]
  
  # ## add non-minority column, make longer, add percentages
  # pop_income_2020[, minority := hispanic + black + aialnative + asian]
  
  pop_race_df <- pop_race_df %>%
    pivot_longer(cols = c(hispanic, white, black, aialnative, asian, hawaiian_pacisl, nonh_other, nonh_two_or_more),
                 names_to = "demo_group",
                 values_to = "pop") %>%
    as.data.table()
  
  ## calculate share
  pop_race_df[, pct := pop / total_pop]
  
  ## if total pop == 0, change NA to zero
  pop_race_df[, pct := ifelse(total_pop == 0, 0, pct)]
  
  pop_race_df[, demo_cat := "Race"]
  
  ## get proportions for poverty 
  ## -------------------------------------------------------------------------------
  
  ## copy raw_pop_poverty
  pop_poverty <- copy(raw_pop_poverty)
  
  ## extract census tract, process
  pop_poverty[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  pop_poverty[, year := NULL]
  pop_poverty[, total_pop := NULL]
  pop_poverty <- pop_poverty[state == "California"]
  
  ## take total population from the previous file used to calculate race ratios
  total_pop_df <- pop_race_df[, .(census_tract, total_pop)]
  total_pop_df <- unique(total_pop_df)
  
  ## pivot longer, merge, calculate shares
  pop_poverty <- pop_poverty %>%
    full_join(total_pop_df) %>%
    ## re-estimate total above pov line by subtracting below rom total
    mutate(total_above_poverty = total_pop - total_below_poverty) %>%
    select(census_tract, total_pop, total_above_poverty, total_below_poverty) %>%
    pivot_longer(cols = c(total_above_poverty, total_below_poverty),
                 names_to = "demo_group",
                 values_to = "pop") %>%
    as.data.table() 
  
  ## add columns
  pop_poverty[, pct := pop / total_pop]
  
  ## if total_pop == 0, set pct to zero
  pop_poverty[, pct := ifelse(total_pop == 0, 0, pct)]
  
  pop_poverty[, demo_cat := "Poverty"]
  
  ## select columns
  pop_poverty <- pop_poverty[, .(census_tract, demo_cat, demo_group, pct)]
  
  pop_race_df <- pop_race_df[, .(census_tract, demo_cat, demo_group, pct)]
  
  
  ## get DAC proportions
  ## -------------------------------------------------------------------------------
  
  dac_df <- refining_mortality %>%
    ungroup() %>%
    select(census_tract, disadvantaged) %>%
    unique() %>%
    left_join(total_pop_df) %>%
    as.data.table() 
  
  dac_df[, pct := 1]
  dac_df[, pop := total_pop]
  dac_df[, demo_group := ifelse(disadvantaged == "No", "non_dac", "dac")]
  dac_df[, demo_cat := "DAC"]
  dac_df[, "disadvantaged" := NULL]
  
  dac_df <- dac_df[, .(census_tract, demo_cat, demo_group, pct)]

  
  
  ## rbind
  merged_ratio_df <- rbind(pop_race_df, pop_poverty, dac_df)
  
  ## legend names
  lnames_df <- tibble(demo_group = c("non_dac", "dac", "hispanic", "white", "asian", "aialnative", "black", 
                                     "hawaiian_pacisl", "nonh_other", "nonh_two_or_more", "total_above_poverty", "total_below_poverty"),
                      title = c("Non-DAC", "DAC", "Hispanic", "white", "Asian",  "American Indian or Alaska Native", "Black", 
                                "Hawaiian or Other Pacific Islander", "Other (Non-Hispanic)", "Two or more races (Non-Hispanic)", "Above poverty line", "Below poverty line"))
  
  merged_ratio_df <- merge(merged_ratio_df, lnames_df,
                              all.x = T)
  
  ## return
  return(merged_ratio_df)

  
}

calc_pop_ratios_county <- function(raw_pop_income_2021,
                                   raw_pop_poverty,
                                   refining_mortality) {
    
    ## get proportions for race and merge
    ## -------------------------------------------------------------------------------
    pop_race_c_df <- copy(raw_pop_income_2021)
    
    ## extract census tract, process
    pop_race_c_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
    pop_race_c_df[, year := NULL]
    pop_race_c_df <- pop_race_c_df[state == "California"]
    
    # ## add non-minority column, make longer, add percentages
    # pop_income_2020[, minority := hispanic + black + aialnative + asian]
    
    pop_race_c_df <- pop_race_c_df %>%
      pivot_longer(cols = c(hispanic, white, black, aialnative, asian, hawaiian_pacisl, nonh_other, nonh_two_or_more),
                   names_to = "demo_group",
                   values_to = "pop") %>%
      group_by(county, demo_group) %>%
      summarise(pop = sum(pop)) %>%
      ungroup() %>%
      group_by(county) %>%
      mutate(total_pop = sum(pop)) %>%
      ungroup() %>%
      as.data.table()
    
    ## calculate share
    pop_race_c_df[, pct := pop / total_pop]
    pop_race_c_df[, demo_cat := "Race"]
    

    ## get proportions for poverty 
    ## -------------------------------------------------------------------------------
    
    ## copy raw_pop_poverty
    pop_c_poverty <- copy(raw_pop_poverty)
    
    ## extract census tract, process
    pop_c_poverty[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
    pop_c_poverty[, year := NULL]
    pop_c_poverty <- pop_c_poverty[state == "California"]
    
    ## take total population from the previous file used to calculate race ratios
    total_pop_c_df <- copy(raw_pop_income_2021)
    total_pop_c_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
    total_pop_c_df[, year := NULL]
    total_pop_c_df <- total_pop_c_df[state == "California"]
    
    total_pop_c_df <- total_pop_c_df[, .(census_tract, county, total_pop)]
    
    
    ## pivot longer, summarize by county, merge, calculate shares
    pop_c_poverty <- pop_c_poverty %>%
      select(-total_pop) %>%
      full_join(total_pop_c_df, by = c("census_tract", "county")) %>%
      ## re-estimate total above pov line by subtracting below from total
      mutate(total_above_poverty = total_pop - total_below_poverty) %>%
      select(county, census_tract, total_pop, total_above_poverty, total_below_poverty) %>%
      pivot_longer(cols = c(total_above_poverty, total_below_poverty),
                   names_to = "demo_group",
                   values_to = "pop") %>%
      group_by(county, demo_group) %>%
      summarize(pop = sum(pop)) %>%
      ungroup() %>%
      group_by(county) %>%
      mutate(total_pop = sum(pop)) %>%
      as.data.table() 
    
    ## add columns
    pop_c_poverty[, pct := pop / total_pop]
    pop_c_poverty[, demo_cat := "Poverty"]
    
    ## select columns
    pop_c_poverty <- pop_c_poverty[, .(county, demo_cat, demo_group, pct)]
    
    pop_race_c_df <- pop_race_c_df[, .(county, demo_cat, demo_group, pct)]
    
    
    ## get DAC proportions
    ## -------------------------------------------------------------------------------
    
    ## county and census tracts
    c_ct_df <- raw_pop_income_2021[state == "California"]
    c_ct_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
    c_ct_df <- c_ct_df[, .(county, census_tract, total_pop)]
    
    dac_c_df <- refining_mortality %>%
      ungroup() %>%
      select(census_tract, disadvantaged) %>%
      unique() %>%
      left_join(c_ct_df) %>%
      mutate(pop = total_pop,
             demo_group = ifelse(disadvantaged == "No", "non_dac", "dac"),
             demo_cat = "DAC") %>%
      group_by(county, demo_cat, demo_group) %>%
      summarise(pop = sum(pop)) %>%
      ungroup() %>%
      group_by(county) %>%
      mutate(total_pop = sum(pop)) %>%
      ungroup() %>%
      mutate(pct = pop / total_pop) %>%
      select(county, demo_cat, demo_group, pct) %>%
      as.data.table() 
    
    ## rbind
    merged_c_ratio_df <- rbind(pop_race_c_df, pop_c_poverty, dac_c_df)
    
    ## legend names
    lnames_df <- tibble(demo_group = c("non_dac", "dac", "hispanic", "white", "asian", "aialnative", "black", 
                                       "hawaiian_pacisl", "nonh_other", "nonh_two_or_more", "total_above_poverty", "total_below_poverty"),
                        title = c("Non-DAC", "DAC", "Hispanic", "white", "Asian",  "American Indian or Alaska Native", "Black", 
                                  "Hawaiian or Other Pacific Islander", "Other (Non-Hispanic)", "Two or more races (Non-Hispanic)", "Above poverty line", "Below poverty line"))
    
    merged_c_ratio_df <- merge(merged_c_ratio_df, lnames_df,
                             all.x = T)
    
    ## return
    return(merged_c_ratio_df)
    
    
  }


## calculate race, DAC, and poverty ratios with census data
calc_state_pop_ratios = function(raw_pop_income_2021,
                                 raw_pop_poverty,
                                 refining_mortality) {
  
  
  ## get proportions for race and merge
  ## -------------------------------------------------------------------------------
  pop_race_df <- copy(raw_pop_income_2021)
  
  ## extract census tract, process
  pop_race_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  pop_race_df[, year := NULL]
  pop_race_df <- pop_race_df[state == "California"]
  
  # ## add non-minority column, make longer, add percentages
  # pop_income_2020[, minority := hispanic + black + aialnative + asian]
  
  pop_race_df <- pop_race_df %>%
    pivot_longer(cols = c(hispanic, white, black, aialnative, asian, hawaiian_pacisl, nonh_other, nonh_two_or_more),
                 names_to = "demo_group",
                 values_to = "pop") %>%
    as.data.table()
  
  ## sumamrize state pop
  state_pop_df <- pop_race_df[, .(state, county, census_tract, total_pop)]
  state_pop_df <- unique(state_pop_df)
  state_pop_df <- pop_race_df[, .(state_pop = sum(pop)), by = .(state)]

  ## summarize race pop
  state_race_df <- pop_race_df[, .(pop = sum(pop)), by = .(state, demo_group)]
  
  ## merge
  state_race_df<- merge(state_race_df, state_pop_df,
                        by = c("state"),
                        all.x = T)
  
  ## calculate share
  state_race_df[, pct := pop / state_pop]
  
  state_race_df[, demo_cat := "Race"]
  
  state_race_df <- state_race_df[, .(state, demo_group, demo_cat, pct)]
  
  
  ## get proportions for poverty 
  ## -------------------------------------------------------------------------------
  
  ## copy raw_pop_poverty
  pop_poverty <- copy(raw_pop_poverty)
  
  ## extract census tract, process
  pop_poverty[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  pop_poverty[, year := NULL]
  pop_poverty[, total_pop := NULL]
  pop_poverty <- pop_poverty[state == "California"]
  
  ## sumamrize state pop
  state_pop_pov_df <- pop_poverty[, .(pop_above = sum(total_above_poverty),
                                      pop_below = sum(total_below_poverty)), by = .(state)]
  
  state_pop_pov_df <- state_pop_pov_df[, total_pop := pop_above + pop_below]
  
  state_pop_pov_df[, total_above_poverty := pop_above / total_pop]
  state_pop_pov_df[, total_below_poverty := pop_below / total_pop]
  state_pop_pov_df <- state_pop_pov_df[, .(state, total_above_poverty, total_below_poverty)]
  
  ## pivot longer, merge, calculate shares
  state_pop_pov_df <- state_pop_pov_df %>%
    pivot_longer(cols = c(total_above_poverty, total_below_poverty),
                 names_to = "demo_group",
                 values_to = "pct") %>%
    as.data.table() 
  
  state_pop_pov_df[, demo_cat := "Poverty"]

  
  ## get DAC proportions
  ## -------------------------------------------------------------------------------
  
  ## ct populations
  ct_pop_df <- pop_race_df[, .(state, census_tract, total_pop)]
  ct_pop_df <- unique(ct_pop_df)
  
  dac_df <- refining_mortality %>%
    ungroup() %>%
    select(census_tract, disadvantaged) %>%
    unique() %>%
    left_join(ct_pop_df) %>%
    as.data.table() 
  
  dac_df[, demo_group := ifelse(disadvantaged == "No", "non_dac", "dac")]
  dac_df[, demo_cat := "DAC"]
  dac_df[, "disadvantaged" := NULL]
  
  ## summarize dac pop
  state_dac_df <- dac_df[, .(pop = sum(total_pop)), by = .(state, demo_group, demo_cat)]
  
  state_dac_df[, total_pop := sum(pop)]
  state_dac_df[, pct := pop / total_pop]
  
  state_dac_df <- state_dac_df[, .(state, demo_cat, demo_group, pct)]
  
  
  
  ## rbind
  merged_share_ratio_df <- rbind(state_race_df, state_pop_pov_df, state_dac_df)
  
  ## legend names
  lnames_df <- tibble(demo_group = c("non_dac", "dac", "hispanic", "white", "asian", "aialnative", "black", 
                                     "hawaiian_pacisl", "nonh_other", "nonh_two_or_more", "total_above_poverty", "total_below_poverty"),
                      title = c("Non-DAC", "DAC", "Hispanic", "white", "Asian",  "American Indian or Alaska Native", "Black", 
                                "Hawaiian or Other Pacific Islander", "Other (Non-Hispanic)", "Two or more races (Non-Hispanic)", "Above poverty line", "Below poverty line"))
  
  merged_share_ratio_df <- merge(merged_share_ratio_df, lnames_df,
                           all.x = T)
  
  ## return
  return(merged_share_ratio_df)
  
  
}



