# Refinery-related data reading functions
# Split from read_files.R

read_refcap_data <- function(file) {
    dt <- simple_fread(file)
    dt <- dt[, c(
        "site_id",
        "refinery_name",
        "barrels_per_day",
        "location",
        "county",
        "cluster"
    )]
    data.table::setnames(dt, "cluster", "region")

    # remove asphalt and non-transportation refineries
    dt <- dt[
        !refinery_name %in%
            c(
                "Greka Energy, Santa Maria Refinery",
                "Lunday Thagard, South Gate Refinery",
                "Valero Wilmington Asphalt Refinery"
            )
    ]

    dt
}

read_ref_ghg_data <- function(file, sel_year) {
    dt <- simple_fread(file)
    reg <- unique(dt[, .(
        year,
        region,
        region_barrels,
        region_co2e_kg,
        region_kgco2e_bbl
    )])
    year_ghg <- reg[year == sel_year]
    year_ghg
}

read_rediesel_data <- function(
    file,
    input_sheet,
    input_rows = NULL,
    input_cols = NULL
) {
    dt <- data.table::setDT(openxlsx::read.xlsx(
        file,
        sheet = input_sheet,
        rows = input_rows,
        cols = input_cols
    ))
    dt <- data.table::melt(
        dt,
        id.vars = "X1",
        variable.name = "year",
        value.name = "consumption_gal"
    )
    dt[, year := as.numeric(as.character(year))]
    dt[, fuel := "renewable diesel"]
    dt[, X1 := NULL]
    dt
}

read_altair_data <- function(
    file,
    input_sheet,
    ei_crude_input,
    ei_gasoline_input,
    input_rows = NULL,
    input_cols = NULL
) {
    dt <- data.table::setDT(openxlsx::read.xlsx(
        file,
        sheet = input_sheet,
        rows = input_rows,
        cols = input_cols
    ))
    dt <- dt[, .(
        year,
        refinery_name,
        barrels_per_day,
        location,
        county,
        region
    )]
    dt[, bbl_per_year := barrels_per_day * 365]
    dt[, gal_per_year := bbl_per_year * 42]
    dt[, bge_per_year := bbl_per_year * (ei_crude_input / ei_gasoline_input)]
    dt[, gge_per_year := bge_per_year * 42]
    dt
}

read_oil_px <- function(
    file,
    input_sheet,
    input_rows = NULL,
    input_cols = NULL
) {
    ## oil prices
    oilpx_scens_real <- data.table::setDT(openxlsx::read.xlsx(
        file,
        sheet = "real",
        cols = input_cols
    ))
    colnames(oilpx_scens_real) <- c(
        "year",
        "reference_case",
        "high_oil_price",
        "low_oil_price"
    )
    oilpx_scens_real <- data.table::melt(
        oilpx_scens_real,
        measure.vars = c("reference_case", "high_oil_price", "low_oil_price"),
        variable.name = "oil_price_scenario",
        value.name = "oil_price_usd_per_bbl"
    )
    oilpx_scens_real[, oil_price_scenario := gsub("_", " ", oil_price_scenario)]
    oilpx_scens_real[,
        oil_price_scenario := factor(
            oil_price_scenario,
            levels = c("reference case", "high oil price", "low oil price")
        )
    ]
}

read_refin_locs <- function(file_refin_locs, file_refin_locs_orig, ca_crs) {
    refin_crs <- sf::st_crs(sf::st_read(file_refin_locs_orig))

    ## Refineries plus
    refin_new_locations <- data.table::fread(file_refin_locs) %>%
        dplyr::mutate(coords = gsub("^c\\(|\\)$", "", geometry)) %>%
        tidyr::separate(coords, c("lon", "lat"), sep = ",") %>%
        dplyr::select(-geometry) %>%
        sf::st_as_sf(
            coords = c("lon", "lat"),
            crs = refin_crs
        ) %>%
        sf::st_transform(ca_crs)
}

read_refin_locs_ct <- function(file, refin_locs) {
    dt <- data.table::fread(
        file,
        colClasses = c(`GEOID (from Census Geocoder)` = "character")
    )

    dt <- janitor::clean_names(dt)
    dt[, geoid_from_census_geocoder := paste0("0", geoid_from_census_geocoder)]

    data.table::setnames(dt, c("geoid_from_census_geocoder"), c("GEOID"))

    dt <- unique(dt[, .(name, cluster, GEOID)])

    remove_refin <- c(
        "Marathon Petroleum Corp., Golden  Eagle Martinez Refinery",
        "AltAir Paramount (2020)"
    )

    add_refinery <- c(
        "Chevron U.S.A. Inc., El Segundo",
        "Chevron U.S.A. Inc., Richmond",
        "Marathon Petroleum Corp., Carson",
        "Phillips 66, Rodeo San Francisco"
    )

    dt <- dt[!name %in% remove_refin]
    dt <- dt[GEOID != "06.01336E+13"]

    dt[,
        adj_name := data.table::fifelse(
            name == "AltAir Paramount (2021)",
            "AltAir Paramount",
            data.table::fifelse(
                name %in% add_refinery,
                paste0(name, " Refinery"),
                data.table::fifelse(
                    name == "PBF Energy, Martinez Refinery",
                    "Shell Oil Products US, Martinez Refinery",
                    name
                )
            )
        )
    ]

    data.table::setnames(dt, c("adj_name"), c("refinery_name"))
    data.table::setnames(dt, c("cluster"), c("region"))

    ## merge with refin_locs
    dt2 <- merge(dt, refin_locs, by = c("refinery_name", "region"), all.x = T)

    return(dt2)
}
