# Core file reading functions remain in this file
# Specialized functions have been split into:
# - read_files_core.R (basic helpers)
# - read_files_refinery.R (refinery-specific functions)
# - read_files_census.R (census and demographic data)
# - read_files_labor.R (labor-related functions)
# - read_files_misc.R (miscellaneous functions)

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

# NOTE: Other functions have been moved to specialized files
# This file now serves as a compatibility layer and contains only the most commonly used functions
