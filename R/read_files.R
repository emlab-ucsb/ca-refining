simple_fread <- function(file) {
  fread(file, header = T)
}

simple_read_xlsx <- function(file, input_sheet, input_rows=NULL, input_cols=NULL) {
  setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
}

read_refcap_data <- function(file) {
  dt = simple_fread(file)
  dt = dt[, c('site_id', 'refinery_name', 'barrels_per_day', 'location', 'county', 'cluster')]
  setnames(dt, 'cluster', 'region')
  
  # remove asphalt and non-transportation refineries
  dt = dt[! refinery_name %in% c('Greka Energy, Santa Maria Refinery', 'Lunday Thagard, South Gate Refinery', 'Valero Wilmington Asphalt Refinery')]
  
  dt
}

read_ref_ghg_data <- function(file, sel_year) {
  dt = simple_fread(file)
  reg = unique(dt[, .(year, region, region_barrels, region_co2e_kg, region_kgco2e_bbl)])
  year_ghg = reg[year == sel_year]
  year_ghg
}

read_rediesel_data <- function(file, input_sheet, input_rows=NULL, input_cols=NULL) {
  dt = setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
  dt = melt(dt, id.vars = 'X1', variable.name = 'year', value.name = 'consumption_gal')
  dt[, year := as.numeric(as.character(year))]
  dt[, fuel := 'renewable diesel']
  dt[, X1 := NULL]
  dt
}

read_altair_data <- function(file, input_sheet, ei_crude_input, ei_gasoline_input, input_rows=NULL, input_cols=NULL) {
  dt = setDT(read.xlsx(file, sheet = input_sheet, rows = input_rows, cols = input_cols))
  dt = dt[, .(year, refinery_name, barrels_per_day, location, county, region)]
  dt[, bbl_per_year := barrels_per_day * 365]
  dt[, gal_per_year := bbl_per_year * 42]
  dt[, bge_per_year := bbl_per_year * (ei_crude_input/ei_gasoline_input)]
  dt[, gge_per_year := bge_per_year * 42]
  dt
}














# track_files_basic <- function(file) {
#   data = fread(file, header = T)
#   output = head(data)
#   orig_name = sub(".csv", "", basename(file))
#   output_name = paste0(orig_name, "_head.csv")
#   fwrite(output, output_name)
#   output_name
# }
