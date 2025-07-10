# Miscellaneous data reading functions
# Split from read_files.R

read_raw_its_data <- function(
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
    colnames(dt) <- c("fuel", "units", 2017:2050)
    dt[, fuel := tolower(fuel)]
    dt
}

read_raw_fpm_data <- function(file, input_sheet, start_row_input) {
    dt <- data.table::setDT(openxlsx::read.xlsx(
        file,
        sheet = input_sheet,
        startRow = start_row_input,
        detectDates = T
    ))
    colnames(dt)[2] <- "code"
    dt
}

read_ghg_2019_data <- function(file) {
    dt <- data.table::fread(file, header = T)
    dt_complete <- dt[boundary == "complete", .(year, value)]
    dt_complete[, mtco2e := value / 1e9]
    dt_complete[, value := NULL]

    dt_complete
}

read_inmap_data <- function(inmap_path, bsite) {
    nh3 <- data.table::fread(
        paste0(inmap_path, "/nh3/srm_nh3_site", bsite, ".csv"),
        header = TRUE,
        colClasses = c(GEOID = "character")
    )
    nox <- data.table::fread(
        paste0(inmap_path, "/nox/srm_nox_site", bsite, ".csv"),
        header = TRUE,
        colClasses = c(GEOID = "character")
    )
    pm25 <- data.table::fread(
        paste0(inmap_path, "/pm25/srm_pm25_site", bsite, ".csv"),
        header = TRUE,
        colClasses = c(GEOID = "character")
    )
    sox <- data.table::fread(
        paste0(inmap_path, "/sox/srm_sox_site", bsite, ".csv"),
        header = TRUE,
        colClasses = c(GEOID = "character")
    )
    voc <- data.table::fread(
        paste0(inmap_path, "/voc/srm_voc_site", bsite, ".csv"),
        header = TRUE,
        colClasses = c(GEOID = "character")
    )

    nh3[, pollutant := "nh3"]
    nox[, pollutant := "nox"]
    pm25[, pollutant := "pm25"]
    sox[, pollutant := "sox"]
    voc[, pollutant := "voc"]

    all_pollutants <- rbind(nh3, nox, pm25, sox, voc)
    all_pollutants[, site := bsite]

    data.table::as.data.table(all_pollutants)
}
