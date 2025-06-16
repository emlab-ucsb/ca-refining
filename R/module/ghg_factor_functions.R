#' Calculate region-level crude oil consumption
#' @param dt_fw Fuel watch data
#' @return Data table with region-level crude consumption
calculate_region_crude_consumption <- function(dt_fw) {
    cons_region <- dt_fw[
        stock_flow == "Refinery Input" & category == "Crude Oil"
    ]
    cons_region <- cons_region[,
        .(thous_barrels = sum(thous_barrels, na.rm = T)),
        by = .(year, region)
    ]
    cons_region[, region_barrels := thous_barrels * 1e3]
    return(cons_region)
}

#' Filter refinery capacity data to remove non-transportation refineries
#' @param dt_refcap Refinery capacity data
#' @return Filtered refinery capacity data
filter_refinery_capacity <- function(dt_refcap) {
    cap_dt <- copy(dt_refcap)

    # Only rename if "cluster" exists and "region" doesn't exist yet
    if ("cluster" %in% names(cap_dt) && !("region" %in% names(cap_dt))) {
        setnames(cap_dt, "cluster", "region")
    }
    # If "region" column doesn't exist at all, we have a problem
    if (!("region" %in% names(cap_dt))) {
        stop(
            "Neither 'cluster' nor 'region' column found in refinery capacity data"
        )
    }

    cap_dt <- cap_dt[
        !refinery_name %in%
            c(
                "Greka Energy, Santa Maria Refinery",
                "Lunday Thagard, South Gate Refinery",
                "Valero Wilmington Asphalt Refinery"
            )
    ]
    return(cap_dt)
}

#' Filter hydrogen facilities to only those with co-located refineries
#' @param dt_hydrogen_facilities Hydrogen facilities data
#' @return Filtered hydrogen facilities data
filter_hydrogen_facilities <- function(dt_hydrogen_facilities) {
    hyd_ref <- dt_hydrogen_facilities[!is.na(co_located_refinery_name)]
    return(hyd_ref)
}

#' Extract hydrogen GHG emissions from MRR data
#' @param dt_mrr MRR emissions data
#' @param hyd_ref Filtered hydrogen facilities
#' @return Hydrogen GHG emissions data
extract_hydrogen_ghg <- function(dt_mrr, hyd_ref) {
    hyd_ghg <- dt_mrr[
        hyd_ref[, .(ARB_ID, co_located_refinery_name)],
        on = .(ARB_ID),
        nomatch = 0
    ]
    return(hyd_ghg)
}

#' Match hydrogen facilities with refinery location information
#' @param hyd_ghg Hydrogen GHG emissions
#' @param cap_dt_filtered Filtered refinery capacity data
#' @return Hydrogen emissions with location data
match_hydrogen_with_refineries <- function(hyd_ghg, cap_dt_filtered) {
    hyd_ghg_loc <- hyd_ghg[
        cap_dt_filtered,
        on = .(co_located_refinery_name = refinery_name),
        nomatch = 0
    ]

    hyd_ghg_loc[,
        adj_total_co2e := fifelse(
            report_yr >= 2011,
            co2e_nb_ch4_n2o + co2_bf,
            total_co2e
        )
    ]
    hyd_ghg_loc[, adj_total_Mt_co2e := adj_total_co2e / 1e6]

    return(hyd_ghg_loc)
}

#' Match refinery emissions to region
#' @param cap_dt_filtered Filtered refinery capacity data
#' @param dt_ghg_emissions GHG emissions data (note: this needs to be created as a target)
#' @return Refinery emissions with location data
match_refinery_emissions_to_region <- function(
    cap_dt_filtered,
    dt_ghg_emissions
) {
    # Add site_id for Santa Maria refinery
    dt_ghg_copy <- copy(dt_ghg_emissions)
    dt_ghg_copy[
        refinery_name == "Phillips 66, Santa Maria Refinery",
        site_id := 3422
    ]

    ref_loc <- cap_dt_filtered[dt_ghg_copy, on = .(site_id), nomatch = 0]
    return(ref_loc)
}

#' Combine hydrogen and refinery emissions
#' @param hyd_ghg_loc Hydrogen emissions with location
#' @param ref_loc Refinery emissions with location
#' @return Combined emissions data
combine_hydrogen_and_refinery_emissions <- function(hyd_ghg_loc, ref_loc) {
    setnames(ref_loc, "facility_name_adj", "facility_name")
    setnames(hyd_ghg_loc, "report_yr", "year")

    combined_ghg <- rbind(
        hyd_ghg_loc[, .(
            facility_name,
            site_id,
            region,
            year,
            adj_total_co2e,
            adj_total_Mt_co2e
        )],
        ref_loc[, .(
            facility_name,
            site_id,
            region,
            year,
            adj_total_co2e,
            adj_total_Mt_co2e
        )]
    )

    return(combined_ghg)
}

#' Aggregate emissions by region
#' @param combined_ghg Combined emissions data
#' @return Region-level emissions
aggregate_emissions_by_region <- function(combined_ghg) {
    ghg_region <- combined_ghg[,
        .(
            region_co2e_tonnes = sum(adj_total_co2e, na.rm = T),
            region_co2e_megatonnes = sum(adj_total_Mt_co2e, na.rm = T)
        ),
        by = .(year, region)
    ]
    ghg_region[, region_co2e_kg := region_co2e_tonnes * 1e3]
    return(ghg_region)
}

#' Calculate region-level emission factors
#' @param cons_region Region-level consumption
#' @param ghg_region Region-level emissions
#' @return Region-level emission factors
calculate_region_emission_factors <- function(cons_region, ghg_region) {
    emfac_region <- cons_region[
        ghg_region,
        on = c("year", "region"),
        nomatch = 0
    ]
    emfac_region[, region_kgco2e_bbl := region_co2e_kg / region_barrels]
    emfac_region <- emfac_region[, .(
        year,
        region,
        region_barrels,
        region_co2e_kg,
        region_kgco2e_bbl
    )]
    setorder(emfac_region, year, region)
    return(emfac_region)
}

#' Calculate refinery capacity proportions
#' @param cap_dt_filtered Filtered refinery capacity data
#' @return Capacity proportions by refinery
calculate_capacity_proportions <- function(cap_dt_filtered) {
    cap_prop <- cap_dt_filtered[, .(
        site_id,
        refinery_name,
        region,
        barrels_per_day
    )]
    setnames(cap_prop, "barrels_per_day", "refinery_capacity_bpd")
    cap_prop[,
        region_capacity_bpd := sum(refinery_capacity_bpd),
        by = .(region)
    ]
    cap_prop[, proportion := refinery_capacity_bpd / region_capacity_bpd]
    return(cap_prop)
}

#' Calculate refinery-level consumption
#' @param cap_prop Capacity proportions
#' @param cons_region Region-level consumption
#' @return Refinery-level consumption
calculate_refinery_consumption <- function(cap_prop, cons_region) {
    cons_ref <- cap_prop[cons_region, on = "region", allow.cartesian = T]
    setnames(cons_ref, "thous_barrels", "region_thous_barrels")

    cons_ref[, refinery_barrels := region_barrels * proportion]
    cons_ref <- cons_ref[, .(
        year,
        site_id,
        refinery_name,
        region,
        region_barrels,
        proportion,
        refinery_barrels,
        refinery_capacity_bpd,
        region_capacity_bpd
    )]
    return(cons_ref)
}

#' Calculate refinery-level emission factors
#' @param cons_ref Refinery-level consumption
#' @param dt_ghg_emissions GHG emissions data
#' @param emfac_region Region-level emission factors
#' @return Refinery-level emission factors
calculate_refinery_emission_factors <- function(
    cons_ref,
    dt_ghg_emissions,
    emfac_region
) {
    # Add site_id for Santa Maria refinery
    dt_ghg_copy <- copy(dt_ghg_emissions)
    dt_ghg_copy[
        refinery_name == "Phillips 66, Santa Maria Refinery",
        site_id := 3422
    ]

    emfac_ref <- cons_ref[dt_ghg_copy, on = .(site_id, year), nomatch = 0]
    emfac_ref <- emfac_ref[, .(
        year,
        site_id,
        refinery_name,
        region,
        region_barrels,
        proportion,
        refinery_barrels,
        adj_total_co2e,
        adj_total_Mt_co2e,
        refinery_capacity_bpd,
        region_capacity_bpd
    )]
    setnames(emfac_ref, "adj_total_co2e", "refinery_co2e_tonnes")
    setnames(emfac_ref, "adj_total_Mt_co2e", "refinery_co2e_megatonnes")
    emfac_ref[, refinery_co2e_kg := refinery_co2e_tonnes * 1e3]
    emfac_ref[, refinery_kgco2e_bbl := refinery_co2e_kg / refinery_barrels]
    emfac_ref <- emfac_ref[, .(
        year,
        site_id,
        refinery_name,
        region,
        refinery_barrels,
        refinery_co2e_kg,
        refinery_kgco2e_bbl,
        proportion,
        refinery_capacity_bpd,
        region_capacity_bpd,
        region_barrels
    )]

    # Merge with region level emission factor
    emfac_ref <- merge(
        emfac_ref,
        emfac_region[, .(year, region, region_co2e_kg, region_kgco2e_bbl)],
        by = c("year", "region")
    )

    setorder(emfac_ref, year, region, -proportion)
    return(emfac_ref)
}
