# Labor-related data reading functions
# Split from read_files.R

read_ca_regions <- function(file) {
    dt <- data.table::fread(file)

    ## change text to match implan outputs
    dt[, region := stringr::str_replace(region, "region", "census")]

    ## adjust region names for adjusted regions
    adj_regions <- c("census_3", "census_4", "census_5", "census_6")

    ## adjust names, make regions 8 and 9 Los Angeles and Orange, respectively
    dt[,
        region := data.table::fifelse(
            region %in% adj_regions,
            paste0(region, "_v2"),
            region
        )
    ]
    dt[,
        region := data.table::fifelse(
            region == "census_8",
            "Los Angeles",
            region
        )
    ]
    dt[, region := data.table::fifelse(region == "census_9", "Orange", region)]

    ## updated method uses regions and counties
    labor_counties <- c(
        "Contra Costa",
        "Kern",
        "Los Angeles",
        "Orange",
        "San Luis Obispo",
        "Solano"
    )

    ## flag if county is stand alone county, filter out, bind to county_pop_df
    dt[, remove := data.table::fifelse(county %in% labor_counties, 1, 0)]
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
    dt <- data.table::fread(
        file,
        colClasses = c(
            w_tract_geocode = "character",
            h_tract_geocode = "character"
        )
    )
    dt
}

read_labor_indirect_mult_inputs <- function(file) {
    dt <- data.table::fread(file)
    dt
}

read_labor_fte_inputs <- function(file, input_sheet) {
    dt <- data.table::setDT(openxlsx::read.xlsx(
        file,
        sheet = input_sheet,
        startRow = 2
    ))

    dt <- janitor::clean_names(dt)

    data.table::setnames(dt, c("implan546index"), c("IndustryCode"))

    dt <- dt[, .(IndustryCode, ft_eper_total_emp)]

    dt
}

read_labor_inputs <- function(file, fte_file) {
    dt <- data.table::fread(file)

    ## merge with fte-job-years data
    dt <- merge(dt, fte_file, by = c("IndustryCode"), all.x = T)

    ## clean column names
    dt <- janitor::clean_names(dt)

    ## multiply employment multiplier by ft_eper_total_emp
    dt[, employment := employment * ft_eper_total_emp]

    ## summarize by origin region, destination region, and impcact type
    dt <- dt[,
        .(
            employment = sum(employment),
            emp_comp = sum(employee_compensation)
        ),
        .(origin_region, destination_region, impact_type)
    ]

    ## lowercase
    dt[, impact_type := tolower(impact_type)]

    ## remove text
    dt[, county := stringr::str_remove(origin_region, " County, CA Group")]
    dt[,
        destination_region := stringr::str_remove(
            destination_region,
            " County, CA"
        )
    ]
    dt[, destination_region := stringr::str_trim(destination_region)]

    ## rename columns
    data.table::setnames(
        dt,
        c("origin_region", "destination_region"),
        c("origin", "destination")
    )

    ## select columns
    dt <- dt[, .(
        county,
        origin,
        destination,
        impact_type,
        employment,
        emp_comp
    )]

    dt
}
