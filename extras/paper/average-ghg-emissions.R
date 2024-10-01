filename <- "/Users/meas/Library/CloudStorage/GoogleDrive-mmeng@ucsb.edu/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn/outputs-staged-for-deletion/stocks-flows/refinery_ghg_emissions.csv"

library(data.table)

dt <- fread(filename)

# aggregate adj_total_co2e and adj_total_Mt_co2e by year
agg_year <- dt[, .(adj_total_co2e = sum(adj_total_co2e), adj_total_Mt_co2e = sum(adj_total_Mt_co2e)), by = year]

# agg years 2014-2018 to get average
mean_year <- agg_year[year %in% 2014:2018, .(adj_total_co2e = mean(adj_total_co2e), adj_total_Mt_co2e = mean(adj_total_Mt_co2e))]
