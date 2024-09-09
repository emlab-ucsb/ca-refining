get_finished_products_movements <- function(raw_fpm_gasoline, raw_fpm_diesel, raw_fpm_jet) {
  fpm_gasoline <- copy(raw_fpm_gasoline)
  fpm_diesel <- copy(raw_fpm_diesel)
  fpm_jet <- copy(raw_fpm_jet)

  code_movement <- c(
    "DE" = "domestic_export",
    "DI" = "domestic_import",
    "FE" = "foreign_export",
    "FI" = "foreign_import",
    "NS" = "north_to_south",
    "PE" = "domestic_export",
    "SN" = "south_to_north"
  )
  code_movement_df <- data.table(
    code = names(code_movement),
    movement_type = code_movement
  )

  # label sources (marine, pipeline)
  fpm_gasoline[2:11, source := "marine"]
  fpm_gasoline[14:17, source := "pipeline"]
  fpm_gasoline[18:19, source := "marine_and_pipeline"]

  # label flow movement
  fpm_gasoline <- fpm_gasoline[code_movement_df, on = .(code)]
  fpm_gasoline[`Product:.Gasoline.&.Blendstocks` %like% "Total SC Pipeline", movement_type := "domestic_export"]
  fpm_gasoline[`Product:.Gasoline.&.Blendstocks` %like% "Net Imports", movement_type := "net_import"]


  # label location
  fpm_gasoline[`Product:.Gasoline.&.Blendstocks` %like% "NC", location := "north"]
  fpm_gasoline[`Product:.Gasoline.&.Blendstocks` %like% "SC", location := "south"]
  fpm_gasoline[movement_type %in% c("north_to_south", "south_to_north"), location := "both"]

  # remove NA rows
  fpm_gasoline2 <- fpm_gasoline[!is.na(source)]

  # melt data table from wide to long format
  fpm_gasoline2 <- melt(fpm_gasoline2,
    measure.vars = colnames(fpm_gasoline2)[3:158],
    variable.name = "date", value.name = "thous_bbl"
  )
  colnames(fpm_gasoline2)[1] <- "movement"

  # add fuel column
  fpm_gasoline2[, fuel := "gasoline"]

  # reorganize diesel data --------

  # label sources (marine, pipeline)
  fpm_diesel[1:10, source := "marine"]
  fpm_diesel[12:16, source := "pipeline"]

  fpm_diesel <- fpm_diesel[code_movement_df, on = .(code)]
  fpm_diesel[`Source:.Marine.(SLC-PIERS.Database)` %like% "Total SC Pipeline", movement_type := "domestic_export"]
  fpm_diesel[`Source:.Marine.(SLC-PIERS.Database)` %like% "Net Imports", movement_type := "net_import"]


  # label location
  fpm_diesel[`Source:.Marine.(SLC-PIERS.Database)` %like% "NC", location := "north"]
  fpm_diesel[`Source:.Marine.(SLC-PIERS.Database)` %like% "SC", location := "south"]
  fpm_diesel[movement_type %in% c("north_to_south", "south_to_north"), location := "both"]

  # remove NA rows
  fpm_diesel2 <- fpm_diesel[!is.na(source)]

  # melt data table from wide to long format
  fpm_diesel2 <- melt(fpm_diesel2,
    measure.vars = colnames(fpm_diesel2)[3:158],
    variable.name = "date", value.name = "thous_bbl"
  )

  fpm_diesel2 <- fpm_diesel2[!is.na(movement_type)]
  colnames(fpm_diesel2)[1] <- "movement"

  # add fuel column
  fpm_diesel2[, fuel := "diesel"]

  # reorganize jet data --------

  # label sources (marine, pipeline)
  fpm_jet[2:11, source := "marine"]
  fpm_jet[14:17, source := "pipeline"]

  # label flow movement
  fpm_jet <- fpm_jet[code_movement_df, on = .(code)]
  fpm_jet[`Product:.Jet.Fuel` %like% "Total SC Pipeline", movement_type := "domestic_export"]
  fpm_jet[`Product:.Jet.Fuel` %like% "Net Imports", movement_type := "net_import"]


  # label location
  fpm_jet[`Product:.Jet.Fuel` %like% "NC", location := "north"]
  fpm_jet[`Product:.Jet.Fuel` %like% "SC", location := "south"]
  fpm_jet[movement_type %in% c("north_to_south", "south_to_north"), location := "both"]

  # remove NA rows
  fpm_jet2 <- fpm_jet[!is.na(source)]

  # melt data table from wide to long format
  fpm_jet2 <- melt(fpm_jet2,
    measure.vars = colnames(fpm_jet2)[3:158],
    variable.name = "date", value.name = "thous_bbl"
  )

  fpm_jet2 <- fpm_jet2[!is.na(movement_type)]
  colnames(fpm_jet2)[1] <- "movement"

  # add fuel column
  fpm_jet2[, fuel := "jet"]

  # combine all fuel data
  fpm_all <- rbindlist(list(fpm_gasoline2, fpm_diesel2, fpm_jet2), use.names = T, fill = T)

  fpm_all
}


# # export csv ------
#
# fwrite(fpm_all, paste0(data.dir, 'processed/finished_product_movements_weekly_cec.csv'), row.names = F)
