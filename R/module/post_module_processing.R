# combine renewable refineries with altair -------

create_altair_ts <- function(dt_altair, dem_scens, ref_scens) {
  # create time series of Alt Air paramount capacity installed across all scenarios

  # for 2020, use smaller capacity

  altair_2020 <- CJ(
    installation_year = c(2020),
    refinery_name = dt_altair[year == 2020, refinery_name],
    installation_capacity_bpd = dt_altair[year == 2020, barrels_per_day],
    retired_capacity_bpd = 0,
    barrels_per_year = dt_altair[year == 2020, bbl_per_year],
    bge_per_year = dt_altair[year == 2020, bge_per_year],
    site_id = "t-800",
    location = dt_altair[year == 2020, location],
    region = dt_altair[year == 2020, region],
    cluster = "South",
    year = 2020,
    demand_scenario = dem_scens,
    refining_scenario = ref_scens
  )

  # for 2021 onwards, use larger capacity

  altair_2021 <- CJ(
    installation_year = c(2021),
    refinery_name = dt_altair[year == 2021, refinery_name],
    installation_capacity_bpd = dt_altair[year == 2021, barrels_per_day],
    retired_capacity_bpd = 0,
    barrels_per_year = dt_altair[year == 2021, bbl_per_year],
    bge_per_year = dt_altair[year == 2021, bge_per_year],
    site_id = "t-800",
    location = dt_altair[year == 2021, location],
    region = dt_altair[year == 2021, region],
    cluster = "South",
    year = 2021:2045,
    demand_scenario = dem_scens,
    refining_scenario = ref_scens
  )

  # combine all altair operating year time series
  altair_ref <- rbindlist(list(altair_2020, altair_2021), use.names = T)
  altair_ref
}


combine_renewables_outputs_with_altair <- function(
  altair_ref,
  res_renew_ref_reg
) {
  cp_res_renew_ref_reg <- copy(res_renew_ref_reg)
  # cp_res_renew_ref_reg[, capacity_ratio := NULL]
  res_renew_ref_reg_altair <- rbindlist(
    list(cp_res_renew_ref_reg, altair_ref),
    use.names = T
  )
  setorder(res_renew_ref_reg, demand_scenario, refining_scenario, year)
  res_renew_ref_reg_altair
}

combine_renewables_info_with_altair <- function(altair_ref, renewables_info) {
  renewables_info_altair <- rbindlist(
    list(
      unique(altair_ref[, .(
        site_id,
        refinery_name,
        location,
        region,
        cluster
      )]),
      renewables_info
    ),
    use.names = T
  )

  renewables_info_altair
}


# for each year, calculate the capacity ratio from operating refinery within its respective region -------

calculate_crude_capacity_ratio <- function(res_crude_ref_reg) {
  res_crude_ref_reg_capacity <- copy(res_crude_ref_reg)
  res_crude_ref_reg_capacity[, barrels_per_year := barrels_per_day * 365]
  res_crude_ref_reg_capacity[,
    capacity_ratio := barrels_per_year / sum(barrels_per_year),
    by = .(demand_scenario, refining_scenario, year, region)
  ]
  res_crude_ref_reg_capacity
}

calculate_renewable_capacity_ratio <- function(res_renew_ref_reg_altair) {
  res_renew_ref_reg_capacity <- copy(res_renew_ref_reg_altair)
  res_renew_ref_reg_capacity[,
    capacity_ratio := barrels_per_year / sum(barrels_per_year),
    by = .(demand_scenario, refining_scenario, year)
  ]
  res_renew_ref_reg_capacity
}


# divide GJD demand within crude refineries -------

divide_gjd_demand_crude_refineries <- function(
  res_equiv_demand,
  res_crude_ref_reg_capacity,
  crude_refined_region,
  ei_crude,
  ei_gasoline,
  ei_diesel,
  ei_jet
) {
  # get "traditional" GJD production (regular GJD demand and exports)
  ref_crude_gjd <- res_equiv_demand[, .(
    demand_scenario,
    refining_scenario,
    fuel_equiv,
    year,
    region,
    consumption_bge,
    consumption_bbl,
    export_bge,
    export_bbl
  )]
  ref_crude_gjd[,
    traditional_production_bge := consumption_bge + abs(export_bge)
  ]
  ref_crude_gjd[,
    traditional_production_bbl := consumption_bbl + abs(export_bbl)
  ]

  # merge demand with operating crude refineries
  ref_crude_gjd <- merge(
    ref_crude_gjd[, .(
      demand_scenario,
      refining_scenario,
      region,
      fuel_equiv,
      year,
      traditional_production_bbl
    )],
    res_crude_ref_reg_capacity[, .(
      demand_scenario,
      refining_scenario,
      region,
      year,
      site_id,
      refinery_name,
      location,
      capacity_ratio
    )],
    by = c("demand_scenario", "refining_scenario", "region", "year"),
    allow.cartesian = T
  )

  # split regional demand to indiv refineries
  ref_crude_gjd[,
    traditional_production_bbl := traditional_production_bbl * capacity_ratio
  ]

  # melt bbl data to wide format
  ref_crude_gjd <- dcast(
    ref_crude_gjd,
    demand_scenario +
      refining_scenario +
      region +
      site_id +
      refinery_name +
      location +
      year ~
      fuel_equiv,
    value.var = "traditional_production_bbl"
  )

  # use heat intensity equation to solve for refinery-level crude consumption
  ref_crude_gjd <- ref_crude_gjd[
    crude_refined_region[, .(region, coef)],
    on = .(region)
  ]
  ref_crude_gjd[,
    traditional_crude_consumption_bbl := ((gasoline * ei_gasoline) +
      (diesel * ei_diesel) +
      (jet * ei_jet)) /
      (ei_crude - coef)
  ]

  # reorganize data table
  setnames(ref_crude_gjd, "gasoline", "gasoline_production_bbl")
  setnames(ref_crude_gjd, "diesel", "diesel_production_bbl")
  setnames(ref_crude_gjd, "jet", "jet_production_bbl")

  ref_crude_gjd <- ref_crude_gjd[, .(
    demand_scenario,
    refining_scenario,
    site_id,
    refinery_name,
    location,
    region,
    year,
    gasoline_production_bbl,
    diesel_production_bbl,
    jet_production_bbl,
    traditional_crude_consumption_bbl
  )]

  ref_crude_gjd
}


# divide residual reGJD demand within crude refineries --------

divide_residual_gjd_crude_refineries <- function(
  res_equiv_demand,
  res_crude_ref_reg_capacity,
  crude_refined_region,
  ei_crude,
  ei_gasoline,
  ei_diesel,
  ei_jet,
  dem_scens,
  ref_scens,
  ave_kern_rediesel
) {
  # get residual reGJD demand
  ref_crude_res_regjd <- res_equiv_demand[, .(
    demand_scenario,
    refining_scenario,
    fuel_equiv,
    year,
    region,
    residual_consumption_bge,
    residual_consumption_bbl
  )]
  setnames(
    ref_crude_res_regjd,
    "residual_consumption_bge",
    "residual_production_bge"
  )
  setnames(
    ref_crude_res_regjd,
    "residual_consumption_bbl",
    "residual_production_bbl"
  )

  # create unique list of scenarios and years

  un_scens <- CJ(
    demand_scenario = dem_scens,
    refining_scenario = ref_scens,
    year = 2020:2045
  )

  # merge to decide if Kern Oil is operating in each unique scenario-year combo

  un_scens <- res_crude_ref_reg_capacity[refinery_name %like% "Kern Oil"][
    un_scens,
    on = .(demand_scenario, refining_scenario, year)
  ]
  un_scens[, kern_oil_operating := ifelse(is.na(refinery_name), "no", "yes")]
  un_scens <- un_scens[, .(
    demand_scenario,
    refining_scenario,
    year,
    kern_oil_operating
  )]

  # merge demand with operating crude refineries
  ref_crude_res_regjd <- merge(
    ref_crude_res_regjd[, .(
      demand_scenario,
      refining_scenario,
      region,
      fuel_equiv,
      year,
      residual_production_bbl
    )],
    res_crude_ref_reg_capacity[, .(
      demand_scenario,
      refining_scenario,
      region,
      year,
      site_id,
      refinery_name,
      location,
      capacity_ratio
    )],
    by = c("demand_scenario", "refining_scenario", "region", "year"),
    allow.cartesian = T
  )

  # merge with indicator of kern oil operating
  ref_crude_res_regjd <- ref_crude_res_regjd[
    un_scens,
    on = .(demand_scenario, refining_scenario, year)
  ]

  # for South region, split evenly amongst refineries using capacity ratio
  ref_crude_res_regjd[
    region == "South",
    residual_production_bbl_2 := residual_production_bbl * capacity_ratio
  ]

  # for renewable gasoline and renewable jet, split evenly using capacity ratios also
  ref_crude_res_regjd[
    region == "North" & fuel_equiv %in% c("gasoline", "jet"),
    residual_production_bbl_2 := residual_production_bbl * capacity_ratio
  ]

  # for the renewable diesel production in the North region:
  # - if Kern Oil is operating, then give Kern Oil its historic production values and split the rest evenly amongst the other refineries
  # - if Kern Oil is notoperating, then split total renewable diesel demand evenly

  ref_crude_res_regjd[
    region == "North" & fuel_equiv == "diesel",
    residual_production_bbl_2 := ifelse(
      kern_oil_operating == "yes",
      ifelse(
        residual_production_bbl > ave_kern_rediesel[, consumption_bbl],
        ifelse(
          refinery_name %like% "Kern Oil",
          ave_kern_rediesel[, consumption_bbl],
          (residual_production_bbl - ave_kern_rediesel[, consumption_bbl]) *
            capacity_ratio
        ),
        ifelse(refinery_name %like% "Kern Oil", residual_production_bbl, 0)
      ),
      residual_production_bbl * capacity_ratio
    )
  ]

  # rename
  ref_crude_res_regjd[, residual_production_bbl := NULL]
  setnames(
    ref_crude_res_regjd,
    "residual_production_bbl_2",
    "residual_production_bbl"
  )

  # melt bbl data to wide format
  ref_crude_res_regjd <- dcast(
    ref_crude_res_regjd,
    demand_scenario +
      refining_scenario +
      region +
      site_id +
      refinery_name +
      location +
      year ~
      fuel_equiv,
    value.var = "residual_production_bbl"
  )

  # use heat intensity equation to solve for refinery-level crude consumption
  ref_crude_res_regjd <- ref_crude_res_regjd[
    crude_refined_region[, .(region, coef)],
    on = .(region)
  ]
  ref_crude_res_regjd[,
    residual_renewable_crude_consumption_bbl := ((gasoline * ei_gasoline) +
      (diesel * ei_diesel) +
      (jet * ei_jet)) /
      (ei_crude - coef)
  ]

  # reorganize data table
  setnames(
    ref_crude_res_regjd,
    "gasoline",
    "residual_renewable_gasoline_production_bbl"
  )
  setnames(
    ref_crude_res_regjd,
    "diesel",
    "residual_renewable_diesel_production_bbl"
  )
  setnames(ref_crude_res_regjd, "jet", "residual_renewable_jet_production_bbl")

  ref_crude_res_regjd <- ref_crude_res_regjd[, .(
    demand_scenario,
    refining_scenario,
    site_id,
    refinery_name,
    location,
    region,
    year,
    residual_renewable_gasoline_production_bbl,
    residual_renewable_diesel_production_bbl,
    residual_renewable_jet_production_bbl,
    residual_renewable_crude_consumption_bbl
  )]

  ref_crude_res_regjd
}


# divide reGJD demand between renewable refineries ------

divide_residual_gjd_renewable_refineries <- function(
  res_renew_demand,
  res_renew_ref_reg_capacity,
  renewables_info_altair,
  crude_refined_tot,
  ei_crude,
  ei_gasoline,
  ei_diesel,
  ei_jet
) {
  # get residual reGJD demand
  ref_renew_gjd <- res_renew_demand[, .(
    demand_scenario,
    refining_scenario,
    fuel_equiv,
    year,
    renewable_refinery_consumption_bge,
    renewable_refinery_consumption_bbl
  )]
  setnames(
    ref_renew_gjd,
    "renewable_refinery_consumption_bge",
    "renewable_production_bge"
  )
  setnames(
    ref_renew_gjd,
    "renewable_refinery_consumption_bbl",
    "renewable_production_bbl"
  )

  # merge demand with operating renewable refineries
  ref_renew_gjd <- merge(
    ref_renew_gjd[, .(
      demand_scenario,
      refining_scenario,
      fuel_equiv,
      year,
      renewable_production_bbl
    )],
    res_renew_ref_reg_capacity[, .(
      demand_scenario,
      refining_scenario,
      year,
      refinery_name,
      capacity_ratio
    )],
    by = c("demand_scenario", "refining_scenario", "year"),
    allow.cartesian = T
  )

  # add locational info
  ref_renew_gjd <- ref_renew_gjd[renewables_info_altair, on = "refinery_name"]

  # split regional demand to indiv refineries
  ref_renew_gjd[,
    renewable_production_bbl := renewable_production_bbl * capacity_ratio
  ]

  # melt bbl data to wide format
  ref_renew_gjd <- dcast(
    ref_renew_gjd,
    demand_scenario +
      refining_scenario +
      region +
      site_id +
      refinery_name +
      location +
      year ~
      fuel_equiv,
    value.var = "renewable_production_bbl"
  )

  # use heat intensity equation to solve for refinery-level crude consumption
  ref_renew_gjd[, coef := crude_refined_tot[, coef]]
  ref_renew_gjd[,
    main_renewable_crude_consumption_bbl := ((gasoline * ei_gasoline) +
      (diesel * ei_diesel) +
      (jet * ei_jet)) /
      (ei_crude - coef)
  ]

  # reorganize data table
  setnames(ref_renew_gjd, "gasoline", "main_renewable_gasoline_production_bbl")
  setnames(ref_renew_gjd, "diesel", "main_renewable_diesel_production_bbl")
  setnames(ref_renew_gjd, "jet", "main_renewable_jet_production_bbl")

  ref_renew_gjd <- ref_renew_gjd[, .(
    demand_scenario,
    refining_scenario,
    site_id,
    refinery_name,
    location,
    region,
    year,
    main_renewable_gasoline_production_bbl,
    main_renewable_diesel_production_bbl,
    main_renewable_jet_production_bbl,
    main_renewable_crude_consumption_bbl
  )]

  ref_renew_gjd
}


# combine all refinery-level production and consumption -------

combine_refinery_prod_cons <- function(
  ref_crude_gjd,
  ref_crude_res_regjd,
  ref_renew_gjd,
  dt_ghgfac
) {
  ref_cons_prod <- merge(
    ref_crude_gjd,
    ref_crude_res_regjd,
    by = c(
      "demand_scenario",
      "refining_scenario",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "year"
    ),
    all = T
  )
  ref_cons_prod[, site_id := as.character(site_id)]

  ref_cons_prod <- merge(
    ref_cons_prod,
    ref_renew_gjd,
    by = c(
      "demand_scenario",
      "refining_scenario",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "year"
    ),
    all = T
  )

  fix_columns <- colnames(ref_cons_prod)[8:19]
  ref_cons_prod[,
    (fix_columns) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
    .SDcols = fix_columns
  ]

  ref_cons_prod[,
    total_crude_consumption_bbl := traditional_crude_consumption_bbl +
      residual_renewable_crude_consumption_bbl +
      main_renewable_crude_consumption_bbl
  ]

  # # split regional crude consumption to refinery level --------
  #
  #   cols = 'total_crude_demand_bbl'
  #   agg_crude_consumption_indiv[, (cols) := lapply(.SD, function(x) x*capacity_ratio), .SDcols = cols]
  #   setcolorder(agg_crude_consumption_indiv, c('demand_scenario', 'refining_scenario', 'region', 'year', 'site_id', 'refinery_name', 'location',
  #                                              'total_crude_demand_bbl', 'capacity_ratio'))
  #
  # add ghg emissions factor to refinery crude consumption ---------

  ref_cons_prod <- ref_cons_prod[
    dt_ghgfac[, .(region, region_kgco2e_bbl)],
    on = "region"
  ]
  ref_cons_prod[,
    total_co2e_kg := total_crude_consumption_bbl * region_kgco2e_bbl
  ]
  ref_cons_prod
}


# refinery-level: fuel production -----------

gather_refinery_production <- function(
  ref_cons_prod,
  ei_crude,
  ei_gasoline,
  ei_diesel,
  ei_jet
) {
  # get RJD production at the refinery level

  indiv_prod_1 <- ref_cons_prod[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    gasoline_production_bbl,
    diesel_production_bbl,
    jet_production_bbl
  )]

  indiv_prod_1 <- melt(
    indiv_prod_1,
    id.vars = c(
      "demand_scenario",
      "refining_scenario",
      "year",
      "site_id",
      "refinery_name",
      "location",
      "region"
    ),
    measure.vars = c(
      "gasoline_production_bbl",
      "diesel_production_bbl",
      "jet_production_bbl"
    ),
    variable.name = "fuel",
    value.name = "production_bbl"
  )
  indiv_prod_1[, fuel := gsub("_production_bbl", "", fuel)]

  # get reGJD production at the refinery level

  indiv_prod_2 <- ref_cons_prod[, .(
    demand_scenario,
    refining_scenario,
    site_id,
    refinery_name,
    location,
    region,
    year,
    residual_renewable_gasoline_production_bbl,
    residual_renewable_diesel_production_bbl,
    residual_renewable_jet_production_bbl,
    main_renewable_gasoline_production_bbl,
    main_renewable_diesel_production_bbl,
    main_renewable_jet_production_bbl
  )]
  indiv_prod_2 <- melt(
    indiv_prod_2,
    id.vars = c(
      "demand_scenario",
      "refining_scenario",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "year"
    ),
    measure.vars = c(
      "residual_renewable_gasoline_production_bbl",
      "residual_renewable_diesel_production_bbl",
      "residual_renewable_jet_production_bbl",
      "main_renewable_gasoline_production_bbl",
      "main_renewable_diesel_production_bbl",
      "main_renewable_jet_production_bbl"
    ),
    variable.name = "fuel",
    value.name = "production_bbl"
  )
  indiv_prod_2[fuel %like% "gasoline", fuel := "drop-in gasoline"]
  indiv_prod_2[fuel %like% "diesel", fuel := "renewable diesel"]
  indiv_prod_2[fuel %like% "jet", fuel := "sustainable aviation fuel"]

  # aggregate residual and main reGJD together

  indiv_prod_2 <- indiv_prod_2[,
    .(production_bbl = sum(production_bbl, na.rm = T)),
    by = .(
      demand_scenario,
      refining_scenario,
      year,
      site_id,
      refinery_name,
      location,
      region,
      fuel
    )
  ]

  # combine results and calculate gasoline equivalent

  indiv_prod <- rbindlist(
    list(indiv_prod_1, indiv_prod_2),
    use.names = T,
    fill = T
  )
  indiv_prod[fuel %like% "gasoline", production_bge := production_bbl]
  indiv_prod[
    fuel %like% "diesel",
    production_bge := production_bbl * (ei_diesel / ei_gasoline)
  ]
  indiv_prod[
    fuel %like% "jet" | fuel %like% "aviation",
    production_bge := production_bbl * (ei_jet / ei_gasoline)
  ]

  # assign clusters
  indiv_prod[region == "South", cluster := "South"]
  indiv_prod[
    region == "North" & location == "Bakersfield",
    cluster := "Bakersfield"
  ]
  indiv_prod[
    region == "North" & (!location == "Bakersfield"),
    cluster := "Bay Area"
  ]

  # set factor level order
  indiv_prod[,
    fuel := factor(
      fuel,
      levels = c(
        "gasoline",
        "drop-in gasoline",
        "diesel",
        "renewable diesel",
        "jet",
        "sustainable aviation fuel"
      )
    )
  ]
  indiv_prod[,
    cluster := factor(cluster, levels = c("Bay Area", "Bakersfield", "South"))
  ]

  setorder(indiv_prod, demand_scenario, refining_scenario, year, site_id, fuel)

  indiv_prod
}


# refinery-level: fuel production (outputs version) ------

gather_refinery_production_output <- function(indiv_prod) {
  indiv_prod_output <- indiv_prod[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    cluster,
    fuel,
    production_bbl
  )]
  indiv_prod_output[, type := "production"]
  indiv_prod_output[, units := "bbl"]
  indiv_prod_output[, source := "total"]
  indiv_prod_output[, boundary := "complete"]
  setnames(indiv_prod_output, "production_bbl", "value")
  setcolorder(
    indiv_prod_output,
    c(
      "demand_scenario",
      "refining_scenario",
      "year",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "cluster",
      "fuel",
      "source",
      "boundary",
      "type",
      "units",
      "value"
    )
  )

  indiv_prod_output
}

# refinery-level: fuel production (outputs bge version) ------

gather_refinery_production_output_bge <- function(indiv_prod) {
  indiv_prod_output_bge <- indiv_prod[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    cluster,
    fuel,
    production_bge
  )]
  indiv_prod_output_bge[, type := "production"]
  indiv_prod_output_bge[, units := "bge"]
  indiv_prod_output_bge[, source := "total"]
  indiv_prod_output_bge[, boundary := "complete"]
  setnames(indiv_prod_output_bge, "production_bge", "value")
  setcolorder(
    indiv_prod_output_bge,
    c(
      "demand_scenario",
      "refining_scenario",
      "year",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "cluster",
      "fuel",
      "source",
      "boundary",
      "type",
      "units",
      "value"
    )
  )

  indiv_prod_output_bge
}


# refinery-level: crude consumption -----------

gather_refinery_crude_consumption <- function(
  ref_cons_prod,
  ei_crude,
  ei_gasoline,
  ei_diesel,
  ei_jet
) {
  # get refinery-level consumption

  indiv_cons <- ref_cons_prod[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    traditional_crude_consumption_bbl,
    residual_renewable_crude_consumption_bbl,
    main_renewable_crude_consumption_bbl,
    total_crude_consumption_bbl
  )]

  indiv_cons <- melt(
    indiv_cons,
    id.vars = c(
      "demand_scenario",
      "refining_scenario",
      "year",
      "site_id",
      "refinery_name",
      "location",
      "region"
    ),
    measure.vars = c(
      "traditional_crude_consumption_bbl",
      "residual_renewable_crude_consumption_bbl",
      "main_renewable_crude_consumption_bbl",
      "total_crude_consumption_bbl"
    ),
    variable.name = "fuel",
    value.name = "consumption_bbl"
  )
  indiv_cons[, fuel := gsub("_crude_consumption_bbl", "", fuel)]
  indiv_cons[, source := gsub("_", " ", fuel)]
  indiv_cons[, fuel := "crude"]
  # indiv_cons[, fuel := paste0('crude (', fuel, ')')]

  # assign clusters

  indiv_cons[region == "South", cluster := "South"]
  indiv_cons[
    region == "North" & location == "Bakersfield",
    cluster := "Bakersfield"
  ]
  indiv_cons[
    region == "North" & (!location == "Bakersfield"),
    cluster := "Bay Area"
  ]

  # set factor level order

  indiv_cons[,
    source := factor(
      source,
      levels = c("traditional", "residual renewable", "main renewable", "total")
    )
  ]
  indiv_cons[,
    cluster := factor(cluster, levels = c("Bay Area", "Bakersfield", "South"))
  ]

  setorder(
    indiv_cons,
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    fuel,
    source
  )

  # calculate gasoline equivalent
  indiv_cons[
    fuel %like% "crude",
    consumption_bge := consumption_bbl * (ei_crude / ei_gasoline)
  ]

  indiv_cons
}

# refinery-level: crude consumption (outputs version) -----------

gather_refinery_crude_consumption_output <- function(indiv_cons) {
  indiv_cons_output <- indiv_cons[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    cluster,
    fuel,
    source,
    consumption_bbl
  )]
  indiv_cons_output[, type := "consumption"]
  indiv_cons_output[, units := "bbl"]
  indiv_cons_output[, boundary := "complete"]
  setnames(indiv_cons_output, "consumption_bbl", "value")
  setcolorder(
    indiv_cons_output,
    c(
      "demand_scenario",
      "refining_scenario",
      "year",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "cluster",
      "fuel",
      "source",
      "boundary",
      "type",
      "units",
      "value"
    )
  )

  indiv_cons_output
}

# refinery-level: crude consumption (outputs bge version) -----------

gather_refinery_crude_consumption_output_bge <- function(indiv_cons) {
  indiv_cons_output_bge <- indiv_cons[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    cluster,
    fuel,
    source,
    consumption_bge
  )]
  indiv_cons_output_bge[, type := "consumption"]
  indiv_cons_output_bge[, units := "bge"]
  indiv_cons_output_bge[, boundary := "complete"]
  setnames(indiv_cons_output_bge, "consumption_bge", "value")
  setcolorder(
    indiv_cons_output_bge,
    c(
      "demand_scenario",
      "refining_scenario",
      "year",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "cluster",
      "fuel",
      "source",
      "boundary",
      "type",
      "units",
      "value"
    )
  )

  indiv_cons_output_bge
}


# refinery-level: emissions -----------

gather_refinery_ghg <- function(ref_cons_prod, indiv_cons) {
  # get unique set of emissions factors (note: i think this is redundant now without innovation scenarios and ccs adoption)

  res <- ref_cons_prod[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    region_kgco2e_bbl
  )]

  # merge with consumption data to get refinery-emissions

  indiv_ghg <- indiv_cons[
    res,
    on = .(
      demand_scenario,
      refining_scenario,
      year,
      site_id,
      refinery_name,
      location,
      region
    )
  ]

  # calculate emissions

  indiv_ghg[, co2e_kg := consumption_bbl * region_kgco2e_bbl]

  # assign clusters

  indiv_ghg[region == "South", cluster := "South"]
  indiv_ghg[
    region == "North" & location == "Bakersfield",
    cluster := "Bakersfield"
  ]
  indiv_ghg[
    region == "North" & (!location == "Bakersfield"),
    cluster := "Bay Area"
  ]

  # set factor level order

  indiv_ghg[,
    source := factor(
      source,
      levels = c("traditional", "residual renewable", "main renewable", "total")
    )
  ]
  indiv_ghg[,
    cluster := factor(cluster, levels = c("Bay Area", "Bakersfield", "South"))
  ]

  setorder(indiv_ghg, demand_scenario, refining_scenario, year, site_id, source)

  indiv_ghg
}

# refinery-level: emissions (outputs version) -----------

gather_refinery_ghg_output <- function(indiv_ghg) {
  indiv_ghg_output <- indiv_ghg[, .(
    demand_scenario,
    refining_scenario,
    year,
    site_id,
    refinery_name,
    location,
    region,
    cluster,
    fuel,
    source,
    co2e_kg
  )]
  indiv_ghg_output[, type := "ghg"]
  indiv_ghg_output[, units := "kg"]
  indiv_ghg_output[, boundary := "complete"]
  setnames(indiv_ghg_output, "co2e_kg", "value")
  setcolorder(
    indiv_ghg_output,
    c(
      "demand_scenario",
      "refining_scenario",
      "year",
      "site_id",
      "refinery_name",
      "location",
      "region",
      "cluster",
      "fuel",
      "source",
      "boundary",
      "type",
      "units",
      "value"
    )
  )

  indiv_ghg_output
}


# cluster-level: fuel production, crude consumption, ghg emissions ----------

gather_cluster_prod_output <- function(indiv_prod_output) {
  clus_prod_output <- indiv_prod_output[,
    .(value = sum(value, na.rm = T)),
    by = .(
      demand_scenario,
      refining_scenario,
      cluster,
      year,
      fuel,
      source,
      boundary,
      type,
      units
    )
  ]

  clus_prod_output
}

gather_cluster_cons_output <- function(indiv_cons_output) {
  clus_cons_output <- indiv_cons_output[,
    .(value = sum(value, na.rm = T)),
    by = .(
      demand_scenario,
      refining_scenario,
      cluster,
      year,
      fuel,
      source,
      boundary,
      type,
      units
    )
  ]

  clus_cons_output
}

gather_cluster_cons_output <- function(indiv_ghg_output) {
  clus_ghg_output <- indiv_ghg_output[,
    .(value = sum(value, na.rm = T)),
    by = .(
      demand_scenario,
      refining_scenario,
      cluster,
      year,
      fuel,
      source,
      boundary,
      type,
      units
    )
  ]
  clus_ghg_output
}


# state-level: fuel production, crude consumption, ghg emissions ----------

gather_state_prod_output <- function(indiv_prod_output) {
  state_prod_output <- indiv_prod_output[,
    .(value = sum(value, na.rm = T)),
    by = .(
      demand_scenario,
      refining_scenario,
      year,
      fuel,
      source,
      boundary,
      type,
      units
    )
  ]
  state_prod_output
}

gather_state_cons_output <- function(indiv_cons_output) {
  state_cons_output <- indiv_cons_output[,
    .(value = sum(value, na.rm = T)),
    by = .(
      demand_scenario,
      refining_scenario,
      year,
      fuel,
      source,
      boundary,
      type,
      units
    )
  ]
  state_cons_output
}

# gather_state_ghg_output <- function(indiv_ghg_output) {
#
#   state_ghg_output = indiv_ghg_output[, .(value = sum(value, na.rm = T)),
#                                       by = .(demand_scenario, refining_scenario,
#                                              year, fuel, source, boundary, type, units)]
#   state_ghg_output
#
# }

# # get state-level fuel exports ------
#
# state_exp = res_equiv_demand[, .(demand_scenario, refining_scenario, region, year, fuel_equiv, export_bbl)]
# state_exp[, export_bbl := abs(export_bbl)]
# setnames(state_exp, 'fuel_equiv', 'fuel')
# state_exp = state_exp[, .(export_bbl = sum(export_bbl, na.rm = T)), by = .(demand_scenario, refining_scenario, year, fuel)]
#
# state_exp_wide = dcast(state_exp, demand_scenario + refining_scenario + year ~ fuel, value.var = 'export_bbl')
# colnames(state_exp_wide)[4:6] = c('diesel_exp_bbl', 'gasoline_exp_bbl', 'jet_exp_bbl')
#
# # get difference between intra-state and inter-state jet demand ------
#
# # select total state-wide forecasted jet fuel production
# jet_inter = state_prod_output[fuel == 'jet']
# setnames(jet_inter, 'value', 'complete_jet')
#
# # in intrastate jet fuel demand forecast, convert to bbl of jet
# dt_intra[, consumption_bbl := (consumption_gge / 42) * (ei_gasoline / ei_jet)]
#
# # combine total jet demand and intrastate jet demand
# jet_difference = jet_inter[dt_intra[year >= 2020 & year <= 2045, .(year, consumption_bbl)], on = .(year)]
# setnames(jet_difference, 'consumption_bbl', 'intrastate_jet_bbl')
#
# # combine jet demand (total and intrastate) with jet exports
# jet_difference = jet_difference[state_exp[fuel == 'jet'],
#                                 on = .(demand_scenario, refining_scenario, year, fuel)]
#
# # calculate interstate jet demand
# jet_difference[, interstate_jet_bbl := complete_jet - intrastate_jet_bbl - export_bbl]
#
# # select columns to keep
# jet_difference = jet_difference[, .(demand_scenario, refining_scenario, innovation_scenario,
#                                     carbon_price_scenario, ccs_scenario,
#                                     year, interstate_jet_bbl, intrastate_jet_bbl)]
#
# # separate boundaries for state-level ghg emissions (using traditional values) ------
#
# state_prod_wide = state_prod_output[fuel %in% c('gasoline', 'diesel', 'jet')]
# state_prod_wide = dcast(state_prod_wide, demand_scenario + refining_scenario + innovation_scenario + carbon_price_scenario + ccs_scenario + year ~ fuel,
#                         value.var = 'value')
# setnames(state_prod_wide, 'gasoline', 'complete_gasoline_bbl')
# setnames(state_prod_wide, 'diesel', 'complete_diesel_bbl')
# setnames(state_prod_wide, 'jet', 'complete_jet_bbl')
#
# boundary_trad = merge(state_prod_wide,
#                       state_ghg_output[source == 'traditional', .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
#                                                                   year, value)],
#                       by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
#                              'year'))
# setnames(boundary_trad, 'value', 'traditional_ghg_kg')
#
# boundary_trad = boundary_trad[jet_difference, on = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
#                                                      year)]
#
# boundary_trad = boundary_trad[state_exp_wide, on = .(demand_scenario, refining_scenario, year)]
#
# boundary_trad[, out_of_state_prop := ((interstate_jet_bbl*ei_jet) +
#                                         (gasoline_exp_bbl*ei_gasoline) +
#                                         (diesel_exp_bbl*ei_diesel) +
#                                         (jet_exp_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) + (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
#
# boundary_trad[, in_state_prop := ((complete_gasoline_bbl - gasoline_exp_bbl)*(ei_gasoline) + (complete_diesel_bbl - diesel_exp_bbl)*(ei_diesel) +
#                                     (intrastate_jet_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) +
#                                                                     (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
#
# boundary_trad[, out_of_state_ghg_kg := out_of_state_prop * traditional_ghg_kg]
# boundary_trad[, in_state_ghg_kg := in_state_prop * traditional_ghg_kg]
#
# # organize outputs for in-state emissions ------
#
# state_ghg_wide = dcast(state_ghg_output, demand_scenario + refining_scenario + innovation_scenario + carbon_price_scenario + ccs_scenario + year ~ source,
#                        value.var = 'value')
#
# in_state_ghg = boundary_trad[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, in_state_ghg_kg)]
# setnames(in_state_ghg, 'in_state_ghg_kg', 'traditional')
# in_state_ghg = merge(in_state_ghg,
#                      state_ghg_wide[, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario',
#                                         'ccs_scenario', 'year', 'residual renewable', 'main renewable')],
#                      by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'))
# in_state_ghg[, total := traditional + `residual renewable` + `main renewable` ]
#
# in_state_ghg = melt(in_state_ghg,
#                     id.vars = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'),
#                     measure.vars = c('traditional', 'residual renewable', 'main renewable', 'total'),
#                     variable.name = 'source',
#                     value.name = 'value')
#
# in_state_ghg[, fuel := 'crude']
# in_state_ghg[, boundary := 'in-state']
# in_state_ghg[, type := 'ghg']
# in_state_ghg[, units := 'kg']
#
# setcolorder(in_state_ghg, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year',
#                             'fuel', 'source', 'boundary', 'type', 'units', 'value'))
#
# # set factor level order
#
# in_state_ghg[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable',  'total'))]
# in_state_ghg[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
# in_state_ghg[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling',
#                                                                                  'carbon_setback_1000ft-no ccs', 'carbon_setback_2500ft-no ccs', 'carbon_setback_5280ft-no ccs', 'carbon_90_perc_reduction-no ccs',
#                                                                                  'carbon_setback_1000ft-medium CCS cost', 'carbon_setback_2500ft-medium CCS cost', 'carbon_setback_5280ft-medium CCS cost', 'carbon_90_perc_reduction-medium CCS cost'))]
# in_state_ghg[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost', "no ccs",
#                                                                'high CCS cost - 45Q', 'medium CCS cost - 45Q', 'low CCS cost - 45Q', "no ccs - 45Q",
#                                                                'high CCS cost - 45Q - LCFS', 'medium CCS cost - 45Q - LCFS', 'low CCS cost - 45Q - LCFS', "no ccs - 45Q - LCFS",
#                                                                'high CCS cost - 45Q - LCFS (constrained)', 'medium CCS cost - 45Q - LCFS (constrained)', 'low CCS cost - 45Q - LCFS (constrained)'))]
#
# setorder(in_state_ghg, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, source)
#
#
# # organize outputs for out-of-state emissions ------
#
# out_state_ghg = boundary_trad[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, out_of_state_ghg_kg)]
# setnames(out_state_ghg, 'out_of_state_ghg_kg', 'traditional')
# out_state_ghg = merge(out_state_ghg,
#                       state_ghg_wide[, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario',
#                                          'ccs_scenario', 'year', 'residual renewable', 'main renewable')],
#                       by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'))
# out_state_ghg[, `residual renewable` := 0]
# out_state_ghg[, `main renewable` := 0]
# out_state_ghg[, total := traditional + `residual renewable` + `main renewable` ]
#
# out_state_ghg = melt(out_state_ghg,
#                      id.vars = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'),
#                      measure.vars = c('traditional', 'residual renewable', 'main renewable', 'total'),
#                      variable.name = 'source',
#                      value.name = 'value')
#
# out_state_ghg[, fuel := 'crude']
# out_state_ghg[, boundary := 'out-of-state']
# out_state_ghg[, type := 'ghg']
# out_state_ghg[, units := 'kg']
#
# setcolorder(out_state_ghg, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year',
#                              'fuel', 'source', 'boundary', 'type', 'units', 'value'))
#
# # set factor level order
#
# out_state_ghg[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable',  'total'))]
# out_state_ghg[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
# out_state_ghg[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling',
#                                                                                   'carbon_setback_1000ft-no ccs', 'carbon_setback_2500ft-no ccs', 'carbon_setback_5280ft-no ccs', 'carbon_90_perc_reduction-no ccs',
#                                                                                   'carbon_setback_1000ft-medium CCS cost', 'carbon_setback_2500ft-medium CCS cost', 'carbon_setback_5280ft-medium CCS cost', 'carbon_90_perc_reduction-medium CCS cost'))]
# out_state_ghg[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost', "no ccs",
#                                                                 'high CCS cost - 45Q', 'medium CCS cost - 45Q', 'low CCS cost - 45Q', "no ccs - 45Q",
#                                                                 'high CCS cost - 45Q - LCFS', 'medium CCS cost - 45Q - LCFS', 'low CCS cost - 45Q - LCFS', "no ccs - 45Q - LCFS",
#                                                                 'high CCS cost - 45Q - LCFS (constrained)', 'medium CCS cost - 45Q - LCFS (constrained)', 'low CCS cost - 45Q - LCFS (constrained)'))]
#
# setorder(out_state_ghg, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, source)
#
# # ----- check if aggregated production matches with earlier demand plot ------
#
# # state_prod = indiv_prod[, .(production_bbl = sum(production_bbl, na.rm = T),
# #                             production_bge = sum(production_bge, na.rm = T)),
# #                            by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, fuel)]
# # state_prod[fuel %like% 'gasoline', production_bge := production_bbl]
# # state_prod[fuel %like% 'diesel', production_bge := production_bbl * (ei_diesel/ei_gasoline)]
# # state_prod[fuel %like% 'jet' | fuel %like% 'aviation', production_bge := production_bbl * (ei_jet/ei_gasoline)]
#
# # check_state_prod = unique(state_prod[, .(demand_scenario, refining_scenario, year, fuel, production_bge)])
#
#
#
# join outputs at each spatial resolution -------

# combine_refinery_outputs <- function(indiv_prod_output, indiv_prod_output_bge, indiv_cons_output, indiv_cons_output_bge, indiv_ghg_output) {
#
#   outputs_indiv = rbindlist(list(indiv_prod_output, indiv_prod_output_bge, indiv_cons_output, indiv_cons_output_bge, indiv_ghg_output), use.names = T, fill = T)
#   setcolorder(outputs_indiv, c('demand_scenario', 'refining_scenario', 'site_id', 'refinery_name', 'location', 'region', 'cluster', 'year', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
#
#   outputs_indiv
#
# }

# combine_cluster_outputs <- function(clus_prod_output, clus_cons_output, clus_ghg_output) {
#
#   outputs_clus = rbindlist(list(clus_prod_output, clus_cons_output, clus_ghg_output), use.names = T, fill = T)
#   setcolorder(outputs_clus, c('demand_scenario', 'refining_scenario', 'cluster', 'year', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
#
#   outputs_clus
#
# }

# combine_state_outputs <- function(state_prod_output, state_cons_output, state_ghg_output, in_state_ghg, out_state_ghg) {
#
#   outputs_state = rbindlist(list(state_prod_output, state_cons_output, state_ghg_output, in_state_ghg, out_state_ghg), use.names = T, fill = T)
#   setcolorder(outputs_state, c('demand_scenario', 'refining_scenario', 'year', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
#
#   outputs_state
#
# }

# GJD production at all refineries -- state level (area plot) --------

# get historic demand of GJD

combine_state_gjd_demand_and_exports <- function(
  crude_refined_week,
  refined_movements_annual,
  dt_rediesel,
  res_equiv_demand,
  res_renew_demand,
  dem_scens,
  ref_scens,
  ei_crude,
  ei_gasoline,
  ei_diesel,
  ei_jet
) {
  agg_hist_tot <- melt(
    crude_refined_week,
    id.vars = c("year"),
    measure.vars = c("crude_gge", "gasoline_gge", "diesel_gge", "jet_gge"),
    value.name = "consumption_gge",
    variable.name = "fuel"
  )
  agg_hist_tot[, fuel := gsub("_gge", "", fuel)]
  agg_hist_tot <- agg_hist_tot[
    year < 2020,
    .(consumption_gge = sum(consumption_gge, na.rm = T)),
    by = .(year, fuel)
  ]
  agg_hist_tot[, consumption_bge := consumption_gge / 42]

  agg_hist_exp <- refined_movements_annual[,
    .(consumption_bbl = sum(abs(net_export_bbl), na.rm = T)),
    by = .(year, fuel)
  ]
  agg_hist_exp[fuel == "gasoline", consumption_gge := consumption_bbl * 42]
  agg_hist_exp[
    fuel == "diesel",
    consumption_gge := consumption_bbl * 42 * (ei_diesel / ei_gasoline)
  ]
  agg_hist_exp[
    fuel == "jet",
    consumption_gge := consumption_bbl * 42 * (ei_jet / ei_gasoline)
  ]
  agg_hist_exp[, export_bge := consumption_gge / 42]

  agg_hist_tot <- merge(
    agg_hist_tot,
    agg_hist_exp[, .(year, fuel, export_bge)],
    by = c("year", "fuel")
  )
  agg_hist_tot[, consumption_bge_new := consumption_bge - export_bge]
  agg_hist_tot[, consumption_gge_new := consumption_bge_new * 42]

  agg_hist_tot <- agg_hist_tot[, .(
    year,
    fuel,
    consumption_gge_new,
    consumption_bge_new
  )]
  setnames(agg_hist_tot, "consumption_gge_new", "consumption_gge")
  setnames(agg_hist_tot, "consumption_bge_new", "consumption_bge")

  # aggregate exports
  agg_hist_exp <- agg_hist_exp[,
    .(consumption_gge = sum(consumption_gge, na.rm = T)),
    by = .(year)
  ]
  agg_hist_exp[, fuel := "exports"]
  agg_hist_exp[, consumption_bge := consumption_gge / 42]

  # get historic demand of renewable diesel
  agg_hist_rediesel <- dt_rediesel[,
    .(consumption_gal = sum(consumption_gal, na.rm = T)),
    by = .(year, fuel)
  ]
  agg_hist_rediesel[,
    consumption_gge := consumption_gal * (ei_diesel / ei_gasoline)
  ]
  agg_hist_rediesel[, consumption_bge := consumption_gge / 42]
  agg_hist_rediesel <- agg_hist_rediesel[, .(
    year,
    fuel,
    consumption_gge,
    consumption_bge
  )]

  # combine RE diesel with GJD
  agg_hist_tot <- rbindlist(
    list(agg_hist_tot, agg_hist_rediesel),
    use.names = T,
    fill = T
  )

  # create full data table of historic fuel demand for all scenarios
  agg_hist_tot_full <- CJ(
    demand_scenario = dem_scens,
    refining_scenario = ref_scens,
    year = 2014:2019,
    fuel = c(
      "gasoline",
      "drop-in gasoline",
      "diesel",
      "renewable diesel",
      "jet",
      "sustainable aviation fuel"
    )
  )
  agg_hist_tot_full <- agg_hist_tot[
    agg_hist_tot_full,
    on = .(year, fuel),
    allow.cartesian = T
  ]
  agg_hist_tot_full <- agg_hist_tot_full[
    !fuel == "crude",
    .(demand_scenario, refining_scenario, year, fuel, consumption_bge)
  ]
  agg_hist_tot_full[
    fuel %in% c("drop-in gasoline", "sustainable aviation fuel"),
    consumption_bge := 0
  ]
  setorder(agg_hist_tot_full, demand_scenario, refining_scenario, year, fuel)

  agg_hist_exp_full <- CJ(
    demand_scenario = dem_scens,
    refining_scenario = ref_scens,
    year = 2007:2019
  )
  agg_hist_exp_full <- agg_hist_exp_full[
    agg_hist_exp,
    on = "year",
    allow.cartesian = T
  ]
  agg_hist_exp_full <- agg_hist_exp_full[, .(
    demand_scenario,
    refining_scenario,
    year,
    fuel,
    consumption_bge
  )]
  setorder(agg_hist_exp_full, demand_scenario, refining_scenario, year, fuel)

  # get in-state GJD demand
  tot_petro_demand <- res_equiv_demand[,
    .(consumption_bge = sum(consumption_bge, na.rm = T)),
    by = .(demand_scenario, refining_scenario, year, fuel_equiv)
  ]
  setnames(tot_petro_demand, "fuel_equiv", "fuel")

  # get in-state reGJD demand at crude refineries
  tot_renew_demand_crude <- res_equiv_demand[,
    .(consumption_bge = sum(residual_consumption_bge, na.rm = T)),
    by = .(demand_scenario, refining_scenario, year, fuel_equiv)
  ]
  setnames(tot_renew_demand_crude, "fuel_equiv", "fuel")
  tot_renew_demand_crude[fuel == "gasoline", fuel := "drop-in gasoline"]
  tot_renew_demand_crude[fuel == "diesel", fuel := "renewable diesel"]
  tot_renew_demand_crude[fuel == "jet", fuel := "sustainable aviation fuel"]

  # get in-state reGJD demand at renewables refineries

  tot_renew_demand_ren <- res_renew_demand[,
    .(consumption_bge = sum(renewable_refinery_consumption_bge, na.rm = T)),
    by = .(demand_scenario, refining_scenario, year, fuel_equiv)
  ]
  setnames(tot_renew_demand_ren, "fuel_equiv", "fuel")
  tot_renew_demand_ren[fuel == "gasoline", fuel := "drop-in gasoline"]
  tot_renew_demand_ren[fuel == "diesel", fuel := "renewable diesel"]
  tot_renew_demand_ren[fuel == "jet", fuel := "sustainable aviation fuel"]

  # combine all reGJD demand

  tot_renew_demand <- rbindlist(list(
    tot_renew_demand_crude,
    tot_renew_demand_ren
  ))
  tot_renew_demand <- tot_renew_demand[,
    .(consumption_bge = sum(consumption_bge, na.rm = T)),
    by = .(demand_scenario, refining_scenario, year, fuel)
  ]

  # get exports
  tot_exports <- res_equiv_demand[, .(
    demand_scenario,
    refining_scenario,
    region,
    year,
    fuel_equiv,
    export_bge
  )]
  tot_exports[, export_bge := abs(export_bge)]
  tot_exports <- tot_exports[,
    .(consumption_bge = sum(abs(export_bge), na.rm = T)),
    by = .(demand_scenario, refining_scenario, fuel_equiv, year)
  ]
  setnames(tot_exports, "fuel_equiv", "fuel")
  tot_exports[, fuel := "exports"]
  tot_exports <- tot_exports[,
    .(consumption_bge = sum(consumption_bge, na.rm = T)),
    by = .(demand_scenario, refining_scenario, year, fuel)
  ]

  # combine together
  tot_fuel_demand_exports <- rbindlist(
    list(
      agg_hist_tot_full,
      agg_hist_exp_full,
      tot_petro_demand,
      tot_renew_demand,
      tot_exports
    ),
    use.names = T,
    fill = T
  )
  setorder(
    tot_fuel_demand_exports,
    demand_scenario,
    refining_scenario,
    year,
    fuel
  )
  tot_fuel_demand_exports[,
    fuel := factor(
      fuel,
      levels = rev(c(
        "gasoline",
        "drop-in gasoline",
        "diesel",
        "renewable diesel",
        "jet",
        "sustainable aviation fuel",
        "exports"
      ))
    )
  ]

  tot_fuel_demand_exports
}

# estimate_2019_ghg <- function(ei_crude,
#                               ei_gasoline,
#                               ei_diesel,
#                               ei_jet,
#                               dt_intra,
#                               dt_fw,
#                               dt_ghgfac,
#                               dt_fpm,
#                               gge_to_bbls) {
#
#   # convert gge intrastate jet fuel demand to bbls, filter for 2018-2020
#   intra_df <- copy(dt_intra)
#   intra_df[, consumption_bbl := (consumption_gge / gge_to_bbls) * (ei_gasoline / ei_jet)]
#   intra_df = intra_df[year >= 2018 & year < 2020]
#
#   # refinery and region emission factors
#   ghg_df <- copy(dt_ghgfac)
#   ghg_region = unique(ghg_df[, .(year, region, region_barrels, region_co2e_kg, region_kgco2e_bbl)])
#   ghg_region_2018 = ghg_region[year == 2018]
#
#   # filter for production and recategorize fuels ------
#   fw_df <- copy(dt_fw)
#   prod_fw_df = fw_df[stock_flow == 'Refinery Production']
#   prod_fw_df[category == 'Motor Gasoline', fuel := 'gasoline']
#   prod_fw_df[category == 'Distillates', fuel := 'diesel']
#   prod_fw_df[category == 'Jet Fuel: Kerosene-Naphtha', fuel := 'jet']
#   prod_fw_df[category == 'Residual', fuel := 'residual']
#
#   # adjust production values to remove ethanol from gasoline
#   prod_fw_df[, adj_thous_barrels := ifelse(category == 'Motor Gasoline' & sub_cat == 'Reformulated', 0.9 * thous_barrels, thous_barrels)]
#
#   # aggregate new adjusted production by region, week, and fuel ------
#   week_prod_fw_df = prod_fw_df[, .(fuel_prod_bbl = sum(adj_thous_barrels, na.rm = T) * 1e3),
#                                    by = .(region, date, year, fuel)]
#   week_prod_fw_df[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet', 'residual'))]
#   week_prod_fw_df = dcast(week_prod_fw_df, region + date + year ~ fuel, value.var = 'fuel_prod_bbl')
#
#   # aggregate crude production by region and week ------
#   crude_input_week = fw_df[stock_flow == 'Refinery Input', .(crude_bbl = sum(thous_barrels, na.rm = T) * 1e3),
#                              by = .(region, date, year)]
#
#   # merge weekly crude input and refined product production -------
#   crude_refined_week = crude_input_week[week_prod_fw_df, on = .(region, date, year)]
#
#   # aggregate crude and refined products by year -----
#   crude_refined_annual = crude_refined_week[year %in% 2014:2019, lapply(.SD, sum, na.rm = T),
#                                             by = c('region', 'year'),
#                                             .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
#
#   setnames(crude_refined_annual, 'crude_bbl', 'complete_crude_bbl')
#   setnames(crude_refined_annual, 'gasoline', 'complete_gasoline_bbl')
#   setnames(crude_refined_annual, 'diesel', 'complete_diesel_bbl')
#   setnames(crude_refined_annual, 'jet', 'complete_jet_bbl')
#
#   # calculate ghg emissions -------
#   crude_refined_ghg = crude_refined_annual[ghg_region_2018[, .(region, region_kgco2e_bbl)], on = .(region)]
#   crude_refined_ghg[, complete_ghg_kg := complete_crude_bbl * region_kgco2e_bbl]
#
#   crude_refined_ghg = crude_refined_ghg[, lapply(.SD, sum, na.rm = T),
#                                         by = c('year'),
#                                         .SDcols = c('complete_crude_bbl', 'complete_gasoline_bbl', 'complete_diesel_bbl', 'complete_jet_bbl', 'complete_ghg_kg')]
#
#
#   # get refined product exports ------
#   fpm_dt <- copy(dt_fpm)
#   refined_exports_month = fpm_dt[code %like% 'E$']
#   refined_exports_month[, year := year(ymd(date))]
#   refined_exports_month[location == 'north', region := 'North']
#   refined_exports_month[location == 'south', region := 'South']
#
#   # get annual exports ------
#   refined_exports_annual = refined_exports_month[, .(export_bbl = sum(thous_bbl*1e3, na.rm = T)), by = .(fuel, year)]
#   refined_exports_annual[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet'))]
#   refined_exports_annual[, export_bbl := abs(export_bbl)]
#   refined_exports_annual_wide = dcast(refined_exports_annual, year ~ fuel, value.var = 'export_bbl')
#   setnames(refined_exports_annual_wide, 'gasoline', 'export_gasoline_bbl')
#   setnames(refined_exports_annual_wide, 'diesel', 'export_diesel_bbl')
#   setnames(refined_exports_annual_wide, 'jet', 'export_jet_bbl')
#
#   # combine production with exports and intrastate jet ------
#   ghg_sep = crude_refined_ghg[refined_exports_annual_wide, on = .(year), nomatch = 0]
#   ghg_sep = ghg_sep[intra_df[, .(year, consumption_bbl)], on = .(year), nomatch = 0]
#   setnames(ghg_sep, 'consumption_bbl', 'intrastate_jet_bbl')
#
#   ghg_sep[, interstate_jet_bbl := complete_jet_bbl - intrastate_jet_bbl - export_jet_bbl]
#
#
#   ghg_sep[, out_of_state_prop := ((interstate_jet_bbl*ei_jet) +
#                                     (export_gasoline_bbl*ei_gasoline) +
#                                     (export_diesel_bbl*ei_diesel) +
#                                     (export_jet_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) + (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
#
#   ghg_sep[, in_state_prop := ((complete_gasoline_bbl - export_gasoline_bbl)*(ei_gasoline) + (complete_diesel_bbl - export_diesel_bbl)*(ei_diesel) +
#                                 (intrastate_jet_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) +
#                                                                 (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
#
#   ghg_sep[, out_of_state_ghg_kg := out_of_state_prop * complete_ghg_kg]
#   ghg_sep[, in_state_ghg_kg := in_state_prop * complete_ghg_kg]
#
#   # convert from wide to long -------
#   state_ghg = melt(ghg_sep, id.vars = 'year',
#                    measure.vars = c('complete_ghg_kg', 'in_state_ghg_kg', 'out_of_state_ghg_kg'),
#                    variable.name = 'boundary',
#                    value.name = 'value')
#   state_ghg[, boundary := gsub('_ghg_kg', '', boundary)]
#   state_ghg[boundary == 'in_state', boundary := 'in-state']
#   state_ghg[boundary == 'out_of_state', boundary := 'out-of-state']
#
#   # add columns ------
#
#   state_ghg[, fuel := 'crude']
#   state_ghg[, source := 'total']
#   state_ghg[, type := 'ghg']
#   state_ghg[, units := 'kg']
#
#   setcolorder(state_ghg, c('year',  'fuel', 'source', 'boundary', 'type', 'units', 'value'))
#
#   state_ghg
#
# }
#
