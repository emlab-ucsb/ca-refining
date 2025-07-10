# Census and demographic data reading functions
# Split from read_files.R

read_census_data <- function(file) {
    data.table::fread(file, header = T, stringsAsFactors = F)
}

read_raw_ces_data <- function(file) {
    data.table::setDT(readxl::read_xlsx(file))
}

read_raw_dac_data <- function(
    file,
    input_sheet,
    input_rows = NULL,
    input_cols
) {
    dt <- data.table::setDT(openxlsx::read.xlsx(
        file,
        sheet = input_sheet,
        cols = input_cols
    ))
    dt <- janitor::clean_names(dt)
    data.table::setDT(dt)
    dt[, census_tract := as.character(paste0("0", census_tract))]

    data.table::setnames(
        dt,
        c("cal_enviro_screen_4_0_score", "total_population"),
        c("ces4_score", "population")
    )

    dt[, disadvantaged := "Yes"]

    dt
}

read_ct_2019_data <- function(file, ca_crs) {
    dt <- sf::read_sf(file) %>%
        dplyr::select(GEOID, COUNTYFP) %>%
        sf::st_transform(crs = ca_crs)
    dt
}

read_ct_2020_data <- function(file, ca_crs) {
    dt <- sf::read_sf(file) %>%
        dplyr::filter(STATEFP == "06") %>%
        dplyr::select(GEOID) %>%
        sf::st_transform(crs = ca_crs)
    dt <- sf::st_make_valid(dt)
    dt
}

read_nhgis_data <- function(file) {
    dt <- data.table::fread(file)

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
        gisjoin,
        state,
        county,
        total_pop,
        hispanic,
        white,
        black,
        aialnative,
        asian,
        hawaiian_pacisl,
        nonh_other,
        nonh_two_or_more,
        median_income,
        geoid,
        year,
        state
    )]
}

read_nhgis_2021_data <- function(file) {
    dt <- data.table::fread(file)

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
        gisjoin,
        state,
        county,
        total_pop,
        hispanic,
        white,
        black,
        aialnative,
        asian,
        hawaiian_pacisl,
        nonh_other,
        nonh_two_or_more,
        median_income,
        geoid,
        year
    )]
}

read_census_race_data <- function(file) {
    dt <- data.table::fread(file)

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
        gisjoin,
        state,
        county,
        total_pop,
        hispanic,
        white,
        black,
        aialnative,
        asian,
        hawaiian_pacisl,
        nonh_other,
        nonh_two_or_more,
        geoid,
        year
    )]
}

read_poverty_data <- function(file) {
    dt <- data.table::fread(file)

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

    dt <- dt[, .(
        gisjoin,
        geoid,
        state,
        county,
        year,
        total_above_poverty,
        total_below_poverty,
        total_pop
    )]
}
