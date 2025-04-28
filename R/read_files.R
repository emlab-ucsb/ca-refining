simple_fread <- function(file) {
  fread(file, header = T)
}

simple_read_xlsx <- function(file, input_sheet, input_rows = NULL, input_cols = NULL) {
  setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
}

read_refcap_data <- function(file) {
  dt <- simple_fread(file)
  dt <- dt[, c("site_id", "refinery_name", "barrels_per_day", "location", "county", "cluster")]
  setnames(dt, "cluster", "region")

  # remove asphalt and non-transportation refineries
  dt <- dt[!refinery_name %in% c("Greka Energy, Santa Maria Refinery", "Lunday Thagard, South Gate Refinery", "Valero Wilmington Asphalt Refinery")]

  dt
}

read_ref_ghg_data <- function(file, sel_year) {
  dt <- simple_fread(file)
  reg <- unique(dt[, .(year, region, region_barrels, region_co2e_kg, region_kgco2e_bbl)])
  year_ghg <- reg[year == sel_year]
  year_ghg
}

read_rediesel_data <- function(file, input_sheet, input_rows = NULL, input_cols = NULL) {
  dt <- setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
  dt <- melt(dt, id.vars = "X1", variable.name = "year", value.name = "consumption_gal")
  dt[, year := as.numeric(as.character(year))]
  dt[, fuel := "renewable diesel"]
  dt[, X1 := NULL]
  dt
}

read_altair_data <- function(file, input_sheet, ei_crude_input, ei_gasoline_input, input_rows = NULL, input_cols = NULL) {
  dt <- setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
  dt <- dt[, .(year, refinery_name, barrels_per_day, location, county, region)]
  dt[, bbl_per_year := barrels_per_day * 365]
  dt[, gal_per_year := bbl_per_year * 42]
  dt[, bge_per_year := bbl_per_year * (ei_crude_input / ei_gasoline_input)]
  dt[, gge_per_year := bge_per_year * 42]
  dt
}

read_raw_its_data <- function(file, input_sheet, input_rows = NULL, input_cols = NULL) {
  dt <- setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
  colnames(dt) <- c("fuel", "units", 2017:2050)
  dt[, fuel := tolower(fuel)]
  dt
}

read_raw_fpm_data <- function(file, input_sheet, start_row_input) {
  dt <- setDT(read.xlsx(file, sheet = input_sheet, startRow = start_row_input, detectDates = T))
  colnames(dt)[2] <- "code"
  dt
}

read_raw_ces_data <- function(file) {
  setDT(read_xlsx(file))
}

read_raw_dac_data <- function(file, input_sheet, input_rows = NULL, input_cols) {
  dt <- setDT(read.xlsx(file, sheet = input_sheet, cols = input_cols))
  dt <- janitor::clean_names(dt)
  setDT(dt)
  dt[, census_tract := as.character(paste0("0", census_tract))]

  setnames(
    dt,
    c("cal_enviro_screen_4_0_score", "total_population"),
    c("ces4_score", "population")
  )

  dt[, disadvantaged := "Yes"]

  dt
}

read_census_data <- function(file) {
  fread(file, header = T, stringsAsFactors = F)
}

fread_data <- function(file) {
  fread(file, stringsAsFactors = F)
}

read_ghg_2019_data <- function(file) {
  dt <- fread(file, header = T)
  dt_complete <- dt[boundary == "complete", .(year, value)]
  dt_complete[, mtco2e := value / 1e9]
  dt_complete[, value := NULL]

  dt_complete
}

read_inmap_data <- function(inmap_path, bsite) {
  nh3 <- fread(paste0(inmap_path, "/nh3/srm_nh3_site", bsite, ".csv"), header = TRUE, colClasses = c(GEOID = "character"))
  nox <- fread(paste0(inmap_path, "/nox/srm_nox_site", bsite, ".csv"), header = TRUE, colClasses = c(GEOID = "character"))
  pm25 <- fread(paste0(inmap_path, "/pm25/srm_pm25_site", bsite, ".csv"), header = TRUE, colClasses = c(GEOID = "character"))
  sox <- fread(paste0(inmap_path, "/sox/srm_sox_site", bsite, ".csv"), header = TRUE, colClasses = c(GEOID = "character"))
  voc <- fread(paste0(inmap_path, "/voc/srm_voc_site", bsite, ".csv"), header = TRUE, colClasses = c(GEOID = "character"))

  nh3[, pollutant := "nh3"]
  nox[, pollutant := "nox"]
  pm25[, pollutant := "pm25"]
  sox[, pollutant := "sox"]
  voc[, pollutant := "voc"]

  all_pollutants <- rbind(nh3, nox, pm25, sox, voc)
  all_pollutants[, site := bsite]

  as.data.table(all_pollutants)
}

read_ct_2019_data <- function(file, ca_crs) {
  dt <- read_sf(file) %>%
    select(GEOID, COUNTYFP) %>%
    st_transform(crs = ca_crs)
  dt
}

read_ct_2020_data <- function(file, ca_crs) {
  dt <- read_sf(file) %>%
    filter(STATEFP == "06") %>%
    select(GEOID) %>%
    st_transform(crs = ca_crs)
  dt <- st_make_valid(dt)
  dt
}

read_nhgis_data <- function(file) {
  dt <- fread(file)

  dt[, `:=`(
    gisjoin = GISJOIN,
    county = COUNTY,
    geoid = GEOID,
    year = YEAR,
    state = STATE,
    total_pop = AMP3E001,
    hispanic = AMP3E012,
    white = AMP3E003,
    black = AMP3E004,
    aialnative = AMP3E005,
    asian = AMP3E006,
    hawaiian_pacisl = AMP3E007,
    nonh_other = AMP3E008,
    nonh_two_or_more = AMP3E009,
    median_income = AMR8E001
  )]

  dt <- dt[, .(
    gisjoin, state, county, total_pop, hispanic, white, black, aialnative, asian, hawaiian_pacisl,
    nonh_other, nonh_two_or_more, median_income, geoid, year, state
  )]
}

read_nhgis_2021_data <- function(file) {
  dt <- fread(file)

  dt[, `:=`(
    gisjoin = GISJOIN,
    county = COUNTY,
    state = STATE,
    geoid = GEO_ID,
    year = YEAR,
    total_pop = AOOCE001,
    hispanic = AOOCE012,
    white = AOOCE003,
    black = AOOCE004,
    aialnative = AOOCE005,
    asian = AOOCE006,
    hawaiian_pacisl = AOOCE007,
    nonh_other = AOOCE008,
    nonh_two_or_more = AOOCE009,
    median_income = AOQIE001
  )]

  dt <- dt[, .(
    gisjoin, state, county, total_pop, hispanic, white, black, aialnative, asian, hawaiian_pacisl,
    nonh_other, nonh_two_or_more, median_income, geoid, year
  )]
}

read_census_race_data <- function(file) {
  dt <- fread(file)

  dt[, `:=`(
    gisjoin = GISJOIN,
    county = COUNTY,
    state = STATE,
    geoid = GEOID,
    year = YEAR,
    total_pop = U7R001,
    hispanic = U7R002,
    white = U7R005,
    black = U7R006,
    aialnative = U7R007,
    asian = U7R008,
    hawaiian_pacisl = U7R009,
    nonh_other = U7R010,
    nonh_two_or_more = U7R011
  )]

  dt <- dt[, .(
    gisjoin, state, county, total_pop, hispanic, white, black, aialnative, asian, hawaiian_pacisl,
    nonh_other, nonh_two_or_more, geoid, year
  )]
}



read_poverty_data <- function(file) {
  dt <- fread(file)

  dt[, `:=`(
    gisjoin = GISJOIN,
    geoid = GEO_ID,
    state = STATE,
    county = COUNTY,
    year = YEAR,
    total_pop = AOQGE001,
    total_below_poverty = AOQGE002,
    total_above_poverty = AOQGE003
  )]

  dt <- dt[, .(gisjoin, geoid, state, county, year, total_above_poverty, total_below_poverty, total_pop)]
}

# read_labor_inputs <- function(file, input_sheet, input_rows=NULL, input_cols=NULL) {
#
#   dt = setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
#
#   dt <- dt %>%
#     filter((county != "Statewide" & segment == "refining") | is.na(segment)==T) %>%
#     rename(dire_emp_mult = direct_emp,
#            indi_emp_mult = indirect_emp,
#            indu_emp_mult = induced_emp,
#            dire_comp_mult = direct_comp,
#            indi_comp_mult = indirect_comp,
#            indu_comp_mult = induced_comp,
#            ip.dire_comp_mult = ip.direct_comp,
#            ip.indi_comp_mult = ip.indirect_comp,
#            ip.indu_comp_mult = ip.induced_comp)
#
#   dt
# }


read_ca_regions <- function(file) {
  dt <- fread(file)

  ## change text to match implan outputs
  dt[, region := str_replace(region, "region", "census")]

  ## adjust region names for adjusted regions
  adj_regions <- c("census_3", "census_4", "census_5", "census_6")

  ## adjust names, make regions 8 and 9 Los Angeles and Orange, respectively
  dt[, region := fifelse(region %in% adj_regions, paste0(region, "_v2"), region)]
  dt[, region := fifelse(region == "census_8", "Los Angeles", region)]
  dt[, region := fifelse(region == "census_9", "Orange", region)]

  ## updated method uses regions and counties
  labor_counties <- c("Contra Costa", "Kern", "Los Angeles", "Orange", "San Luis Obispo", "Solano")

  ## flag if county is stand alone county, filter out, bind to county_pop_df
  dt[, remove := fifelse(county %in% labor_counties, 1, 0)]
  dt <- dt[remove == 0]
  dt[, remove := NULL]

  ## create df of regions and counties to for which to create population metrics
  county_l_df <- data.frame(
    region = labor_counties,
    county = labor_counties
  )

  dt <- rbind(county_l_df, dt)

  dt
}


read_labor_direct_mult_inputs <- function(file) {
  dt <- fread(file, colClasses = c(w_tract_geocode = "character",
                                   h_tract_geocode = "character"))
  dt
}


read_labor_indirect_mult_inputs <- function(file) {
  dt <- fread(file)
  
  dt
}

read_refin_locs_ct <- function(file,
                               refin_locs) {
  dt <- fread(file, colClasses = c(`GEOID (from Census Geocoder)` = "character"))
  
  dt <- janitor::clean_names(dt)
  dt[, geoid_from_census_geocoder := paste0("0", geoid_from_census_geocoder)]
  
  setnames(dt, c("geoid_from_census_geocoder"), c("GEOID"))
  
  dt <- unique(dt[, .(name, cluster, GEOID)])
  
  remove_refin <- c("Marathon Petroleum Corp., Golden  Eagle Martinez Refinery",
                    "AltAir Paramount (2020)")
  
  add_refinery <- c("Chevron U.S.A. Inc., El Segundo",
                    "Chevron U.S.A. Inc., Richmond",
                    "Marathon Petroleum Corp., Carson",
                    "Phillips 66, Rodeo San Francisco")
  
  dt <- dt[!name %in% remove_refin]
  dt <- dt[GEOID != "06.01336E+13"]
  
  dt[, adj_name := fifelse(name == "AltAir Paramount (2021)", "AltAir Paramount", 
                           fifelse(name %in% add_refinery, paste0(name, " Refinery"),
                                   fifelse(name == "PBF Energy, Martinez Refinery", "Shell Oil Products US, Martinez Refinery", name)))]
  
  setnames(dt, c("adj_name"), c("refinery_name"))
  setnames(dt, c("cluster"), c("region"))
  
  ## merge with refin_locs
  dt2 <- merge(dt, refin_locs,
              by = c("refinery_name", "region"),
              all.x = T
  )
  
  return(dt2)
  
}




read_labor_fte_inputs <- function(file, input_sheet) {
  dt <- setDT(read.xlsx(file, sheet = input_sheet, startRow = 2))

  dt <- janitor::clean_names(dt)

  setnames(dt, c("implan546index"), c("IndustryCode"))

  dt <- dt[, .(IndustryCode, ft_eper_total_emp)]

  dt
}


read_labor_inputs <- function(file, fte_file) {
  dt <- fread(file)

  ## merge with fte-job-years data
  dt <- merge(dt, fte_file,
    by = c("IndustryCode"),
    all.x = T
  )

  ## clean column names
  dt <- janitor::clean_names(dt)

  ## multiply employment multiplier by ft_eper_total_emp
  dt[, employment := employment * ft_eper_total_emp]

  ## summarize by origin region, destination region, and impcact type
  dt <- dt[, .(
    employment = sum(employment),
    emp_comp = sum(employee_compensation)
  ), .(origin_region, destination_region, impact_type)]

  ## lowercase
  dt[, impact_type := tolower(impact_type)]

  ## remove text
  dt[, county := str_remove(origin_region, " County, CA Group")]
  dt[, destination_region := str_remove(destination_region, " County, CA")]
  dt[, destination_region := str_trim(destination_region)]

  ## rename columns
  setnames(dt, c("origin_region", "destination_region"), c("origin", "destination"))

  ## select columns
  dt <- dt[, .(county, origin, destination, impact_type, employment, emp_comp)]

  dt
}

read_oil_px <- function(file, input_sheet, input_rows = NULL, input_cols = NULL) {
  ## oil prices
  oilpx_scens_real <- setDT(read.xlsx(file, sheet = "real", cols = input_cols))
  colnames(oilpx_scens_real) <- c("year", "reference_case", "high_oil_price", "low_oil_price")
  oilpx_scens_real <- melt(oilpx_scens_real,
    measure.vars = c("reference_case", "high_oil_price", "low_oil_price"),
    variable.name = "oil_price_scenario", value.name = "oil_price_usd_per_bbl"
  )
  oilpx_scens_real[, oil_price_scenario := gsub("_", " ", oil_price_scenario)]
  oilpx_scens_real[, oil_price_scenario := factor(oil_price_scenario, levels = c("reference case", "high oil price", "low oil price"))]
}

read_refin_locs <- function(file_refin_locs,
                            file_refin_locs_orig,
                            ca_crs) {
  refin_crs <- st_crs(st_read(file_refin_locs_orig))

  ## Refineries plus
  refin_new_locations <- fread(file_refin_locs) %>%
    mutate(coords = gsub("^c\\(|\\)$", "", geometry)) %>%
    separate(coords, c("lon", "lat"), sep = ",") %>%
    select(-geometry) %>%
    st_as_sf(
      coords = c("lon", "lat"),
      crs = refin_crs
    ) %>%
    st_transform(ca_crs)
}



# track_files_basic <- function(file) {
#   data = fread(file, header = T)
#   output = head(data)
#   orig_name = sub(".csv", "", basename(file))
#   output_name = paste0(orig_name, "_head.csv")
#   fwrite(output, output_name)
#   output_name
# }
