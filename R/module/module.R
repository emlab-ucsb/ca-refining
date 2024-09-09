main_refining_module <- function(dem_scens,
                                 ref_scens,
                                 clus,
                                 pred_years,
                                 ref_threshold,
                                 ren_threshold,
                                 drop_in_perc,
                                 dt_refcap,
                                 dt_renref,
                                 renewables_info,
                                 ave_crude_refined,
                                 ave_crude_refined_bge,
                                 ave_region_cdu,
                                 region_fuel_ratio,
                                 ave_refined_exports,
                                 crude_refined_region,
                                 ave_kern_rediesel,
                                 demand_state,
                                 ts_exports,
                                 ei_crude,
                                 ei_gasoline,
                                 ei_diesel,
                                 ei_jet) {
  # main loop  -------

  list_equiv_demand <- list()
  list_renew_demand <- list()
  list_init_cdu <- list()
  list_final_cdu <- list()
  list_crude_ref <- list()
  list_renewable_ref <- list()

  for (k in seq_along(ref_scens)) {
    print(ref_scens[k])

    temp_equiv_scen <- list()
    temp_renew_demand_scen <- list()
    temp_init_cdu_scen <- list()
    temp_final_cdu_scen <- list()
    temp_crude_cap_scen <- list()
    temp_renew_cap_scen <- list()

    for (j in seq_along(dem_scens)) {
      print(dem_scens[j])

      temp_equiv_year <- list()
      temp_renew_demand_year <- list()
      temp_init_cdu_year <- list()
      temp_final_cdu_year <- list()
      temp_crude_cap_year <- list()
      temp_renew_cap_year <- list()
      temp_delayed_cap <- list()

      for (i in seq_along(pred_years)) {
        t <- pred_years[i]
        print(t)

        # get operating refineries in year
        if (i == 1) {
          crude_cap <- copy(dt_refcap)
          ren_cap <- copy(dt_renref)[0, ] # empty data table with same columns
          # if year >= 2023, remove Santa Maria
        } else {
          crude_cap <- temp_crude_cap_year[[i - 1]][demand_scenario == dem_scens[j] & refining_scenario == ref_scens[k]]
          ren_cap <- temp_renew_cap_year[[i - 1]][demand_scenario == dem_scens[j] & refining_scenario == ref_scens[k]]
        }

        if (i >= 4) {
          crude_cap <- crude_cap[!refinery_name == "Phillips 66, Santa Maria Refinery"]
        }

        # get refineries planned to retire in year t
        planned_ren_ref <- dt_renref[installation_year == t]
        if (i > 1) {
          delayed_ren_ref <- temp_delayed_cap[[i - 1]]
          planned_ren_ref <- rbindlist(list(planned_ren_ref, delayed_ren_ref), use.names = T, fill = T)
        }

        # combine operating and planned installed renewable refineries to get full (planned) renewable refinery in year t
        ren_cap_t <- rbindlist(list(ren_cap, planned_ren_ref), use.names = T, fill = T)
        ren_cap_t[, barrels_per_year := installation_capacity_bpd * 365]
        ren_cap_t[, bge_per_year := barrels_per_year * (ei_crude / ei_gasoline)]

        # remove refineries that are converting from crude refineries
        crude_cap_conv <- merge(crude_cap, planned_ren_ref[, .(installation_year, refinery_name, installation_capacity_bpd, retired_capacity_bpd)],
          by = "refinery_name", all.x = T
        )
        crude_cap_conv[!is.na(retired_capacity_bpd), barrels_per_day := barrels_per_day - retired_capacity_bpd]
        crude_cap_conv <- crude_cap_conv[barrels_per_day > 0]
        crude_cap_conv[, c("installation_year", "installation_capacity_bpd", "retired_capacity_bpd") := NULL]

        # calculate crude refining capacity and renewable refining capacity under planned retirements
        crude_cap_reg <- crude_cap_conv[, .(barrels_per_day = sum(barrels_per_day)), by = region]
        crude_cap_reg[, barrels_per_year := barrels_per_day * 365]

        # get GJD demand
        # gjd_dem = product_demand_all[year == t & demand_scenario == dem_scens[j] & refining_scenario == ref_scens[k]]
        gjd_dem <- demand_state[fuel %in% c("gasoline", "diesel", "jet") & year == t & scenario == dem_scens[j]]
        setnames(gjd_dem, "scenario", "demand_scenario")

        # split GJD demand by region
        gjd_dem_2 <- gjd_dem[region_fuel_ratio[, .(region, fuel, ratio)],
          on = c("fuel_equiv" = "fuel"), nomatch = 0
        ]
        cols <- c("consumption_gge", "consumption_bge", "consumption_gal", "consumption_bbl")
        gjd_dem_2[, (cols) := lapply(.SD, function(x) x * ratio), .SDcols = cols]

        gjd_dem_reg <- gjd_dem_2[, .(
          demand_scenario, fuel, fuel_equiv, year, region,
          consumption_gge, consumption_bge, consumption_gal, consumption_bbl
        )]


        # get reGJD demand
        re_gjd_dem <- demand_state[fuel %in% c("drop-in gasoline", "renewable diesel", "sustainable aviation fuel") & year == t & scenario == dem_scens[j]]
        setnames(re_gjd_dem, "scenario", "demand_scenario")

        # limit drop-in gasoline demand to limit (if exceeded)

        re_gjd_dem[, tot_bge := sum(consumption_bge)]
        re_gjd_dem[, perc_bge := consumption_bge / sum(consumption_bge)]
        re_gjd_dem[, rem_bge := tot_bge - re_gjd_dem[fuel == "drop-in gasoline", consumption_bge]]

        re_gjd_dem[fuel == "drop-in gasoline", consumption_bge := ifelse(perc_bge > drop_in_perc,
          (drop_in_perc * rem_bge) / (1 - drop_in_perc),
          consumption_bge
        )]
        re_gjd_dem[fuel == "drop-in gasoline", consumption_gge := consumption_bge * 42]
        re_gjd_dem[fuel == "drop-in gasoline", consumption_gal := consumption_gge]
        re_gjd_dem[fuel == "drop-in gasoline", consumption_bbl := consumption_bge]

        re_gjd_dem[, c("tot_bge", "perc_bge", "rem_bge") := NULL]

        # compare total renewable gasoline, renewable jet, and renewable diesel (reGJD) demand to renewable refining capacity
        sum_re_gjd_dem <- sum(re_gjd_dem[, consumption_bge]) # in units of bge

        # calculate renewable refinery capacity
        if (nrow(ren_cap_t) > 0) {
          ren_cap_tot <- sum(ren_cap_t[, bge_per_year]) * ren_threshold
        } else {
          ren_cap_tot <- 0
        }

        # if kern oil refinery exists, adjust rediesel demand by subtraction kern oil's rediesel processing capacity

        if (nrow(crude_cap_conv[refinery_name %like% "Kern Oil"]) == 1) {
          # sum_re_gjd_dem_adj = sum_re_gjd_dem - ave_kern_rediesel[, consumption_bge]
          re_gjd_dem_adj <- copy(re_gjd_dem)
          re_gjd_dem_adj[fuel == "renewable diesel", consumption_gge := consumption_gge - ave_kern_rediesel[, consumption_gge]]
          re_gjd_dem_adj[fuel == "renewable diesel", consumption_bge := consumption_bge - ave_kern_rediesel[, consumption_bge]]
          re_gjd_dem_adj[fuel == "renewable diesel", consumption_gal := consumption_gal - ave_kern_rediesel[, consumption_gal]]
          re_gjd_dem_adj[fuel == "renewable diesel", consumption_bbl := consumption_bbl - ave_kern_rediesel[, consumption_bbl]]
          sum_re_gjd_dem_adj <- sum(re_gjd_dem_adj[, consumption_bge])
        } else {
          re_gjd_dem_adj <- copy(re_gjd_dem)
          sum_re_gjd_dem_adj <- copy(sum_re_gjd_dem)
        }

        # check if residual renewable demand is positive or negative

        residual_ren <- sum_re_gjd_dem_adj - ren_cap_tot

        if (residual_ren > 0) { #  if residual renewable demand is positive

          # calculate percentage of total renewable demand that is made up of residual renewable demand

          # residual_perc = ifelse(ren_cap_tot == 0, residual_ren/sum_re_gjd_dem_adj, residual_ren/sum_re_gjd_dem)

          # calculate demand that can be met by renewables refineries

          ren_ref_demand <- copy(re_gjd_dem_adj)
          ren_ref_demand[, demand_perc := ren_cap_tot / sum_re_gjd_dem_adj]
          ren_ref_demand[, renewable_refinery_consumption_bge := consumption_bge * demand_perc]
          setnames(ren_ref_demand, "consumption_gge", "adj_consumption_gge")
          setnames(ren_ref_demand, "consumption_bge", "adj_consumption_bge")
          setnames(ren_ref_demand, "consumption_gal", "adj_consumption_gal")
          setnames(ren_ref_demand, "consumption_bbl", "adj_consumption_bbl")

          ren_ref_demand <- ren_ref_demand[, .(demand_scenario, fuel, fuel_equiv, year, adj_consumption_bge, demand_perc, renewable_refinery_consumption_bge)]
          ren_ref_demand[fuel_equiv == "gasoline", renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline / ei_gasoline)]
          ren_ref_demand[fuel_equiv == "diesel", renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline / ei_diesel)]
          ren_ref_demand[fuel_equiv == "jet", renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline / ei_jet)]

          # recalculate how much reGJD needs to be processed at crude refineries

          re_gjd_dem_2 <- merge(re_gjd_dem,
            ren_ref_demand[, .(
              fuel,
              renewable_refinery_consumption_bge
            )],
            by = "fuel"
          )
          re_gjd_dem_2[, residual_consumption_bge := consumption_bge - renewable_refinery_consumption_bge]

          # split residual demand by ratio

          re_gjd_dem_2 <- re_gjd_dem_2[region_fuel_ratio[, .(region, fuel, ratio)],
            on = c("fuel_equiv" = "fuel"), nomatch = 0
          ]
          re_gjd_dem_2[, residual_consumption_bge := residual_consumption_bge * ratio]

          re_gjd_dem_reg <- re_gjd_dem_2[, .(demand_scenario, fuel, fuel_equiv, year, region, residual_consumption_bge)]
          re_gjd_dem_reg[fuel_equiv == "gasoline", residual_consumption_bbl := residual_consumption_bge * (ei_gasoline / ei_gasoline)]
          re_gjd_dem_reg[fuel_equiv == "diesel", residual_consumption_bbl := residual_consumption_bge * (ei_gasoline / ei_diesel)]
          re_gjd_dem_reg[fuel_equiv == "jet", residual_consumption_bbl := residual_consumption_bge * (ei_gasoline / ei_jet)]


          # calculate equivalent demand of gasoline, diesel, and jet
          equiv_demand <- merge(
            gjd_dem_reg[, .(
              demand_scenario, fuel_equiv, year, region,
              consumption_bge, consumption_bbl
            )],
            re_gjd_dem_reg[, .(
              demand_scenario, fuel_equiv, year, region,
              residual_consumption_bge, residual_consumption_bbl
            )],
            by = c("demand_scenario", "fuel_equiv", "region", "year")
          )
          equiv_demand <- equiv_demand[, lapply(.SD, sum, na.rm = T), by = c("demand_scenario", "fuel_equiv", "region", "year")]
          equiv_demand[, equiv_consumption_bge := consumption_bge + residual_consumption_bge]
          equiv_demand[, equiv_consumption_bbl := consumption_bbl + residual_consumption_bbl]

          # assign demand based on historic production or historic exports scenario

          if (ref_scens[k] == "historic production") {
            # use heat intensity equation method to solve for equiv crude demand
            crude_equiv <- dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = "equiv_consumption_bbl")
            crude_equiv <- crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
            crude_equiv[, equiv_crude_demand_bbl := ((gasoline * ei_gasoline) + (diesel * ei_diesel) + (jet * ei_jet)) / (ei_crude - coef)]

            # calculate CDU
            crude_cdu <- crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, equiv_crude_demand_bbl)], on = "region"]
            crude_cdu[, cdu := equiv_crude_demand_bbl / barrels_per_year]

            # set cdu at historic cdu / production
            crude_cdu <- crude_cdu[ave_region_cdu[, .(region, ave_hist_cdu)], on = .(region)]
            crude_cdu[, total_crude_demand_bbl := ave_hist_cdu * barrels_per_year]
            crude_cdu[, cdu := NULL]
            setnames(crude_cdu, "ave_hist_cdu", "cdu")

            # calculate exports
            crude_cdu[, export_crude_bbl := total_crude_demand_bbl - equiv_crude_demand_bbl]
            crude_cdu[, export_crude_bge := export_crude_bbl * (ei_crude / ei_gasoline)]
            temp_exports <- ave_crude_refined_bge[crude_cdu[, .(region, export_crude_bge)], on = .(region)]
            temp_exports[, export_bge := export_crude_bge * bge_perc]
            temp_exports[fuel == "gasoline", export_bbl := export_bge]
            temp_exports[fuel == "diesel", export_bbl := export_bge * (ei_gasoline / ei_diesel)]
            temp_exports[fuel == "jet", export_bbl := export_bge * (ei_jet / ei_diesel)]

            # add exports to demand
            equiv_demand <- equiv_demand[temp_exports[, .(region, fuel, export_bge, export_bbl)], on = c("fuel_equiv" = "fuel", "region")]
            equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
            equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]


            crude_cdu <- crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
          }

          if (ref_scens[k] == "historic exports") {
            # merge with historic exports
            equiv_demand <- equiv_demand[ave_refined_exports[, .(region, fuel, export_bge, export_bbl)], on = c("fuel_equiv" = "fuel", "region")]
            equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
            equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]

            # use heat intensity equation method to solve for equiv crude demand
            crude_equiv <- dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = "total_consumption_bbl")
            crude_equiv <- crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
            crude_equiv[, total_crude_demand_bbl := ((gasoline * ei_gasoline) + (diesel * ei_diesel) + (jet * ei_jet)) / (ei_crude - coef)]

            # calculate CDU
            crude_cdu <- crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = "region"]
            crude_cdu[, cdu := total_crude_demand_bbl / barrels_per_year]

            crude_cdu <- crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
          }

          if (ref_scens[k] == "low exports") {
            # merge with forecasted declining exports
            equiv_demand <- equiv_demand[ts_exports, on = c("fuel_equiv" = "fuel", "region", "year"), nomatch = 0]
            equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
            equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]

            # use heat intensity equation method to solve for equiv crude demand
            crude_equiv <- dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = "total_consumption_bbl")
            crude_equiv <- crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
            crude_equiv[, total_crude_demand_bbl := ((gasoline * ei_gasoline) + (diesel * ei_diesel) + (jet * ei_jet)) / (ei_crude - coef)]

            # calculate CDU
            crude_cdu <- crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = "region"]
            crude_cdu[, cdu := total_crude_demand_bbl / barrels_per_year]

            crude_cdu <- crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
          }
        } else { # if residual demand < 0

          # calculate demand that can be met by renewables refineries

          ren_ref_demand <- copy(re_gjd_dem_adj)
          ren_ref_demand[, demand_perc := 1]
          ren_ref_demand[, renewable_refinery_consumption_bge := consumption_bge * demand_perc]
          setnames(ren_ref_demand, "consumption_gge", "adj_consumption_gge")
          setnames(ren_ref_demand, "consumption_bge", "adj_consumption_bge")
          setnames(ren_ref_demand, "consumption_gal", "adj_consumption_gal")
          setnames(ren_ref_demand, "consumption_bbl", "adj_consumption_bbl")

          ren_ref_demand <- ren_ref_demand[, .(demand_scenario, fuel, fuel_equiv, year, adj_consumption_bge, demand_perc, renewable_refinery_consumption_bge)]
          ren_ref_demand[fuel_equiv == "gasoline", renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline / ei_gasoline)]
          ren_ref_demand[fuel_equiv == "diesel", renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline / ei_diesel)]
          ren_ref_demand[fuel_equiv == "jet", renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline / ei_jet)]

          # recalculate how much reGJD needs to be processed at crude refineries

          re_gjd_dem_2 <- merge(re_gjd_dem,
            ren_ref_demand[, .(fuel, renewable_refinery_consumption_bge)],
            by = "fuel"
          )
          re_gjd_dem_2[, residual_consumption_bge := consumption_bge - renewable_refinery_consumption_bge]

          # assign residual demand to North (Kern Oil)

          re_gjd_dem_2[, region := "North"]

          re_gjd_dem_reg <- re_gjd_dem_2[, .(demand_scenario, fuel, fuel_equiv, year, region, residual_consumption_bge)]
          re_gjd_dem_reg[fuel_equiv == "gasoline", residual_consumption_bbl := residual_consumption_bge * (ei_gasoline / ei_gasoline)]
          re_gjd_dem_reg[fuel_equiv == "diesel", residual_consumption_bbl := residual_consumption_bge * (ei_gasoline / ei_diesel)]
          re_gjd_dem_reg[fuel_equiv == "jet", residual_consumption_bbl := residual_consumption_bge * (ei_gasoline / ei_jet)]

          # if residual renewable demand is less than zero, then equivalent crude demand is just crude demand from GJD + residual reGJD at kern oil
          equiv_demand <- merge(
            gjd_dem_reg[, .(
              demand_scenario, fuel_equiv, year, region,
              consumption_bge, consumption_bbl
            )],
            re_gjd_dem_reg[, .(
              demand_scenario, fuel_equiv, year, region,
              residual_consumption_bge, residual_consumption_bbl
            )],
            by = c("demand_scenario", "fuel_equiv", "region", "year"),
            all = T
          )
          equiv_demand <- equiv_demand[, lapply(.SD, sum, na.rm = T), by = c("demand_scenario", "fuel_equiv", "region", "year")]
          equiv_demand[, equiv_consumption_bge := consumption_bge + residual_consumption_bge]
          equiv_demand[, equiv_consumption_bbl := consumption_bbl + residual_consumption_bbl]

          if (ref_scens[k] == "historic production") {
            # use heat intensity equation method to solve for equiv crude demand
            crude_equiv <- dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = "equiv_consumption_bbl")
            crude_equiv <- crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
            crude_equiv[, equiv_crude_demand_bbl := ((gasoline * ei_gasoline) + (diesel * ei_diesel) + (jet * ei_jet)) / (ei_crude - coef)]

            # calculate CDU
            crude_cdu <- crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, equiv_crude_demand_bbl)], on = "region"]
            crude_cdu[, cdu := equiv_crude_demand_bbl / barrels_per_year]

            # set cdu at historic cdu / production
            crude_cdu <- crude_cdu[ave_region_cdu[, .(region, ave_hist_cdu)], on = .(region)]
            crude_cdu[, total_crude_demand_bbl := ave_hist_cdu * barrels_per_year]
            crude_cdu[, cdu := NULL]
            setnames(crude_cdu, "ave_hist_cdu", "cdu")

            # calculate exports
            crude_cdu[, export_crude_bbl := total_crude_demand_bbl - equiv_crude_demand_bbl]
            crude_cdu[, export_crude_bge := export_crude_bbl * (ei_crude / ei_gasoline)]
            temp_exports <- ave_crude_refined_bge[crude_cdu[, .(region, export_crude_bge)], on = .(region)]
            temp_exports[, export_bge := export_crude_bge * bge_perc]
            temp_exports[fuel == "gasoline", export_bbl := export_bge]
            temp_exports[fuel == "diesel", export_bbl := export_bge * (ei_gasoline / ei_diesel)]
            temp_exports[fuel == "jet", export_bbl := export_bge * (ei_jet / ei_diesel)]

            # add exports to demand
            equiv_demand <- equiv_demand[temp_exports[, .(region, fuel, export_bge, export_bbl)], on = c("fuel_equiv" = "fuel", "region")]

            equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
            equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]

            crude_cdu <- crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
          }

          if (ref_scens[k] == "historic exports") {
            # merge with historic exports
            equiv_demand <- equiv_demand[ave_refined_exports[, .(region, fuel, export_bge, export_bbl)], on = c("fuel_equiv" = "fuel", "region")]
            equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
            equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]

            # use heat intensity equation method to solve for equiv crude demand
            crude_equiv <- dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = "total_consumption_bbl")
            crude_equiv <- crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
            crude_equiv[, total_crude_demand_bbl := ((gasoline * ei_gasoline) + (diesel * ei_diesel) + (jet * ei_jet)) / (ei_crude - coef)]

            # calculate CDU
            crude_cdu <- crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = "region"]
            crude_cdu[, cdu := total_crude_demand_bbl / barrels_per_year]

            crude_cdu <- crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
          }

          if (ref_scens[k] == "low exports") {
            # merge with forecasted declining exports
            equiv_demand <- equiv_demand[ts_exports, on = c("fuel_equiv" = "fuel", "region", "year"), nomatch = 0]
            equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
            equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]

            # use heat intensity equation method to solve for equiv crude demand
            crude_equiv <- dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = "total_consumption_bbl")
            crude_equiv <- crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
            crude_equiv[, total_crude_demand_bbl := ((gasoline * ei_gasoline) + (diesel * ei_diesel) + (jet * ei_jet)) / (ei_crude - coef)]

            # calculate CDU
            crude_cdu <- crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = "region"]
            crude_cdu[, cdu := total_crude_demand_bbl / barrels_per_year]

            crude_cdu <- crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
          }
        }

        # save equivalent demand at crude refineries
        equiv_demand[, year := t]
        equiv_demand[, demand_scenario := dem_scens[j]]
        equiv_demand[, refining_scenario := ref_scens[k]]
        temp_equiv_year[[i]] <- equiv_demand

        # save renewables demand at renewables refineries
        ren_ref_demand[, year := t]
        ren_ref_demand[, demand_scenario := dem_scens[j]]
        ren_ref_demand[, refining_scenario := ref_scens[k]]
        temp_renew_demand_year[[i]] <- ren_ref_demand

        # save initial cdu
        crude_cdu[, year := t]
        crude_cdu[, demand_scenario := dem_scens[j]]
        crude_cdu[, refining_scenario := ref_scens[k]]
        temp_init_cdu_year[[i]] <- crude_cdu

        # separate CDUs by region and either revert some capacities or retire additional capacity

        ren_cap_t <- ren_cap_t[, .(
          installation_year, refinery_name, installation_capacity_bpd,
          retired_capacity_bpd, barrels_per_year, bge_per_year
        )][renewables_info, on = .(refinery_name), nomatch = 0]
        planned_ren_ref <- planned_ren_ref[, .(
          installation_year, refinery_name, installation_capacity_bpd,
          retired_capacity_bpd
        )][renewables_info, on = .(refinery_name), nomatch = 0]

        temp_crude_reg_cap <- list()
        temp_renew_reg_cap <- list()
        temp_cdu_reg <- list()
        temp_del_ren_ref <- list()

        for (r in seq_along(clus)) {
          cur_reg <- clus[r]
          cur_cdu <- crude_cdu[region == cur_reg]
          temp_crude_ref <- crude_cap_conv[region == cur_reg]
          temp_ren_ref <- ren_cap_t[region == cur_reg]
          temp_planned <- planned_ren_ref[region == cur_reg]

          if (cur_cdu[, cdu] > 1) { # if CDU > 1, do not retire crude capacities planned for retirement
            temp_crude_reg_cap[[r]] <- copy(crude_cap[region == cur_reg])
            temp_planned[, installation_year := t + 1]
            temp_del_ren_ref[[r]] <- temp_planned
            temp_renew_reg_cap[[r]] <- temp_ren_ref[!temp_planned, on = "refinery_name"]
          } else {
            if (cur_cdu[, cdu] > ref_threshold) { # if CDU < 1 and CDU > threshhold, then don't do anything
              temp_crude_reg_cap[[r]] <- copy(crude_cap_conv[region == cur_reg])
              temp_renew_reg_cap[[r]] <- copy(temp_ren_ref)
              temp_del_ren_ref[[r]] <- planned_ren_ref[0, ]
            } else { # if CDU < 1 and CDU < threshhold, then retire smallest crude facilities until CDU > threshold
              while (cur_cdu[, cdu] < ref_threshold) {
                if (nrow(temp_crude_ref) == 1) {
                  temp_crude_reg_cap[[r]] <- temp_crude_ref
                  temp_renew_reg_cap[[r]] <- copy(ren_cap_t)
                  temp_del_ren_ref[[r]] <- planned_ren_ref[0, ]
                  break
                }

                minref <- temp_crude_ref[, .SD[which.min(barrels_per_day)], by = region]

                temp_crude_ref <- temp_crude_ref[!refinery_name == minref[, refinery_name]]
                temp_crude_cap_reg <- temp_crude_ref[, .(barrels_per_day = sum(barrels_per_day)), by = region]
                temp_crude_cap_reg[, barrels_per_year := barrels_per_day * 365]
                cur_cdu <- temp_crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, total_crude_demand_bbl)], on = "region", nomatch = 0]
                cur_cdu[, cdu := total_crude_demand_bbl / barrels_per_year]

                if (cur_cdu[, cdu] >= 1) {
                  temp_crude_ref <- rbindlist(list(temp_crude_ref, minref), use.names = T, fill = T)
                }

                # temp_crude_ref[, year := t]
                temp_crude_reg_cap[[r]] <- temp_crude_ref
                temp_renew_reg_cap[[r]] <- copy(ren_cap_t)
                temp_del_ren_ref[[r]] <- planned_ren_ref[0, ]
                # tempclus = tempref[, .(gallons_per_day = sum(barrels_per_day)*42) ]

                # temp = tempdat[i]
                # temp = temp[tempclus, on = c('region' = 'cluster'), nomatch = 0]
                # temp[, no_of_days := ifelse(leap_year(year) == TRUE, 366, 365)]
                # temp[, annual_capacity := gallons_per_day*no_of_days]
                # temp[, cdu := crude/annual_capacity]

                # temp_reg_cap[[r]] = tempref

                # temp_reg_cap[[r]] = copy(crude_cap_conv[region == cur_reg])
              }
            }

            temp_cdu_reg[[r]] <- cur_cdu
          }
        }

        # save final cdu

        dt_cdu_reg <- rbindlist(temp_cdu_reg, use.names = T, fill = T)
        dt_cdu_reg[, year := t]
        dt_cdu_reg[, demand_scenario := dem_scens[j]]
        dt_cdu_reg[, refining_scenario := ref_scens[k]]
        temp_final_cdu_year[[i]] <- dt_cdu_reg

        dt_temp_crude_cap <- rbindlist(temp_crude_reg_cap, use.names = T, fill = T)
        dt_temp_crude_cap[, year := t]
        dt_temp_crude_cap[, demand_scenario := dem_scens[j]]
        dt_temp_crude_cap[, refining_scenario := ref_scens[k]]

        dt_temp_renew_cap <- rbindlist(temp_renew_reg_cap, use.names = T, fill = T)
        dt_temp_renew_cap <- unique(dt_temp_renew_cap)
        dt_temp_renew_cap[, year := t]
        dt_temp_renew_cap[, demand_scenario := dem_scens[j]]
        dt_temp_renew_cap[, refining_scenario := ref_scens[k]]

        dt_temp_del_ren_ref <- rbindlist(temp_del_ren_ref, use.names = T, fill = T)
        dt_temp_del_ren_ref <- unique(dt_temp_del_ren_ref)
        dt_temp_del_ren_ref[, year := t]
        dt_temp_del_ren_ref[, demand_scenario := dem_scens[j]]
        dt_temp_del_ren_ref[, refining_scenario := ref_scens[k]]


        temp_crude_cap_year[[i]] <- dt_temp_crude_cap
        temp_renew_cap_year[[i]] <- dt_temp_renew_cap
        temp_delayed_cap[[i]] <- dt_temp_del_ren_ref
      }

      temp_equiv_scen[[j]] <- rbindlist(temp_equiv_year, use.names = T, fill = T)
      temp_renew_demand_scen[[j]] <- rbindlist(temp_renew_demand_year, use.names = T, fill = T)
      temp_init_cdu_scen[[j]] <- rbindlist(temp_init_cdu_year, use.names = T, fill = T)
      temp_final_cdu_scen[[j]] <- rbindlist(temp_final_cdu_year, use.names = T, fill = T)
      temp_crude_cap_scen[[j]] <- rbindlist(temp_crude_cap_year, use.names = T, fill = T)
      temp_renew_cap_scen[[j]] <- rbindlist(temp_renew_cap_year, use.names = T, fill = T)
    }

    list_equiv_demand[[k]] <- rbindlist(temp_equiv_scen, use.names = T, fill = T)
    list_renew_demand[[k]] <- rbindlist(temp_renew_demand_scen, use.names = T, fill = T)
    list_init_cdu[[k]] <- rbindlist(temp_init_cdu_scen, use.names = T, fill = T)
    list_final_cdu[[k]] <- rbindlist(temp_final_cdu_scen, use.names = T, fill = T)
    list_crude_ref[[k]] <- rbindlist(temp_crude_cap_scen, use.names = T, fill = T)
    list_renewable_ref[[k]] <- rbindlist(temp_renew_cap_scen, use.names = T, fill = T)
  }

  rm(
    crude_cap, ren_cap, planned_ren_ref, ren_cap_t, crude_cap_conv, crude_cap_reg, gjd_dem, re_gjd_dem, sum_re_gjd_dem, ren_cap_tot,
    residual_ren, crude_equiv, crude_cdu, temp_crude_reg_cap, temp_renew_reg_cap, cur_reg, cur_cdu, temp_crude_ref, minref,
    dt_temp_crude_cap, dt_temp_renew_cap, dt_cdu_reg
  )


  # create data tables of results --------

  res_equiv_demand <- rbindlist(list_equiv_demand, use.names = T, fill = T)
  res_renew_demand <- rbindlist(list_renew_demand, use.names = T, fill = T)
  res_final_cdu <- rbindlist(list_final_cdu, use.names = T, fill = T)
  res_crude_ref_reg <- rbindlist(list_crude_ref, use.names = T, fill = T)
  res_renew_ref_reg <- rbindlist(list_renewable_ref, use.names = T, fill = T)


  # join outputs into list
  module_outputs <- list(
    res_equiv_demand,
    res_renew_demand,
    res_final_cdu,
    res_crude_ref_reg,
    res_renew_ref_reg
  )

  module_outputs
}
