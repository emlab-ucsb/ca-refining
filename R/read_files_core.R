# Core file reading helpers
# Split from read_files.R

simple_fread <- function(file) {
    fread(file, header = T)
}

simple_read_xlsx <- function(
    file,
    input_sheet,
    input_rows = NULL,
    input_cols = NULL
) {
    setDT(read.xlsx(
        file,
        sheet = input_sheet,
        rows = input_rows,
        cols = input_cols
    ))
}

fread_data <- function(file) {
    fread(file, stringsAsFactors = F)
}

read_and_bind_csv_files <- function(path, pattern) {
    library(data.table)
    file_list <- list.files(path, pattern = pattern, full.names = TRUE)
    data_list <- lapply(file_list, fread)
    combined_data <- rbindlist(data_list, fill = TRUE, use.names = TRUE)
    return(combined_data)
}
