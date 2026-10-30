# =============================================================================
# Refinery-level health and labor attribution
# -----------------------------------------------------------------------------
# PURPOSE
#   The published analysis reports health and labor outcomes for California's
#   refining sector as a whole: emissions from all refineries are summed to the
#   census tract before mortality is calculated, and labor impacts are summed
#   across refineries before being assigned to workers' home tracts. This script
#   reproduces those same calculations WITHOUT the collapse over `site_id`, so
#   that outcomes can be attributed to individual refineries.
#
#   Written for two refineries that closed in reality shortly after the analysis
#   was completed (see manuscript Discussion, ref [55]):
#       site_id 343 - Phillips 66, Wilmington Refinery (South cluster, Los Angeles)
#       site_id 164 - Valero Energy, Benicia Refinery  (North cluster, Solano)
#
# QUESTION ANSWERED
#   "Level" attribution, not scenario differences: in the Reference scenario
#   (BAU demand + historic production), what did the model project each of these
#   refineries would produce, emit, cause in premature mortality, and support in
#   employment and worker compensation, disaggregated by DAC status, poverty
#   level, and race?
#
#   NOTE this is deliberately NOT the paper's headline framing. The paper reports
#   differences relative to the Reference scenario. Here the Reference scenario is
#   the object of interest itself.
#
# STATEWIDE CONTEXT
#   Every output also carries the corresponding all-16-refinery total, so a
#   refinery's level can be read as a share of the California refining sector
#   rather than as a bare number. Columns added to each file:
#
#     refinery_level_health_labor.csv, ..._annual.csv
#       statewide_total           all 16 refineries, SAME basis as the row and
#                                 at the SAME demographic grain (so the DAC row
#                                 is divided by the statewide DAC total, not by
#                                 the statewide total across all groups)
#       statewide_total_pipeline  the joint sector total; see section 4f for why
#                                 health has two denominators and labor one
#       share_of_statewide        value / statewide_total
#
#     refinery_level_production_ghg.csv
#       statewide_{production_bge, crude_bbl, ghg_mtco2e} and the matching
#       share_of_statewide_* columns
#
#     refinery_level_production_by_fuel.csv
#       statewide_production_bbl, share_of_statewide
#
#   Section 6 checks (iv) and (v) verify these denominators against the
#   pipeline's own published state tables before anything is written.
#
# WHAT THIS SCRIPT DOES NOT CHANGE
#   Nothing in `_targets.R` or `R/` is modified. Inputs are read from the existing
#   targets store via tar_read(), plus two files read directly from disk. Output is
#   a single tidy CSV written next to this script.
#
# PROVENANCE
#   Each section notes the pipeline function it mirrors, with file:line references
#   so the logic can be checked against the published implementation.
#
# STYLE
#   dplyr where it reads clearly; data.table for the large joins (ct_inc_45 is
#   ~2.6M rows, the site x tract x year grid is larger still). No spatial work is
#   needed - the source-receptor matrix has already been joined to census tracts
#   upstream in the pipeline.
# =============================================================================

suppressPackageStartupMessages({
  library(targets)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

options(scipen = 999)

# -----------------------------------------------------------------------------
# 0. CONFIGURATION
# -----------------------------------------------------------------------------

SITES <- c("343", "164")

SITE_LABELS <- c(
  "343" = "Phillips 66, Wilmington Refinery",
  "164" = "Valero Energy, Benicia Refinery"
)

# The Reference scenario. Two spellings of this scenario exist across the repo's
# outputs ("historic" vs "historical"); objects read from the targets store use
# "historic", which is what we match on here.
REF_DEMAND   <- "BAU"
REF_REFINING <- "historic production"

# Labor scenario dimensions. The paper's headline labor numbers hold these fixed;
# leaving any of them unfixed would multiply-count rows.
#   product_scenario     "2020 prices"    - 2020 refined product prices held flat
#   oil_price_scenario   "reference case" - EIA AEO2020 reference oil price path
PRODUCT_SCENARIO   <- "2020 prices"
OIL_PRICE_SCENARIO <- "reference case"

YEARS <- 2020:2045

# Parameters, copied from _targets.R so this script is self-documenting.
# Values verified against outputs/rev-submission/.../targets_snapshot_summary.md
BETA          <- 0.00582        # Krewski et al. (2009) PM2.5 mortality coefficient
DISCOUNT_RATE <- 0.03           # _targets.R:151
CPI2019       <- 107.8645906    # _targets.R:177
CPI2020       <- 109.1951913    # _targets.R:176

STORE   <- "_targets"
OUT_DIR <- "extras/refinery-level-analysis"

# -----------------------------------------------------------------------------
# 1. LOAD INPUTS
# -----------------------------------------------------------------------------
# Most inputs come from the targets store. IMPORTANT EXCEPTION: the store was
# last built for the `test-no-conf-data` version, where `dt_direct_multipliers`
# is deliberately a blank all-NA stub (see _targets.R:714-721). The real IMPLAN x
# LODES worker-flow multipliers must therefore be read straight from disk, or
# every labor number below would come out NA.

message("Loading inputs from targets store...")

srm_weighted_pm25 <- as.data.table(tar_read_raw("srm_weighted_pm25", store = STORE))
site_cons_ghg     <- as.data.table(tar_read_raw("refining_sites_cons_ghg_2019_2045", store = STORE))
dt_ef             <- as.data.table(tar_read_raw("dt_ef", store = STORE))
ct_xwalk          <- as.data.table(tar_read_raw("ct_xwalk", store = STORE))
raw_dac           <- as.data.table(tar_read_raw("raw_dac", store = STORE))
pop_ratios        <- as.data.table(tar_read_raw("pop_ratios", store = STORE))
ct_inc_45         <- as.data.table(tar_read_raw("ct_inc_45", store = STORE))
dt_age_vsl        <- as.data.table(tar_read_raw("dt_age_vsl", store = STORE))
indiv_prod_output <- as.data.table(tar_read_raw("indiv_prod_output", store = STORE))
indiv_prod_output_bge <- as.data.table(tar_read_raw("indiv_prod_output_bge", store = STORE))
product_px        <- as.data.table(tar_read_raw("product_px", store = STORE))
refin_locs_ct     <- tar_read_raw("refin_locs_ct", store = STORE)

# refin_locs_ct is an sf object; we only need the attribute table.
refin_locs_ct <- as.data.table(sf::st_drop_geometry(refin_locs_ct))

# Confidential multipliers, read directly (see note above).
message("Loading confidential direct multipliers from disk...")
dt_direct_multipliers <- fread(
  "data/confidential-data/direct_multipliers_tract.csv",
  colClasses = list(character = c("w_tract_geocode", "h_tract_geocode"))
)
stopifnot(!all(is.na(dt_direct_multipliers$emp.rev)))

# Normalise site_id to character everywhere; the pipeline is inconsistent about
# this (numeric in some objects, character in others) and silent join failures
# are the usual symptom.
srm_weighted_pm25[, site_id := as.character(site_id)]
site_cons_ghg[,     site_id := as.character(site_id)]
indiv_prod_output[,     site_id := as.character(site_id)]
indiv_prod_output_bge[, site_id := as.character(site_id)]
refin_locs_ct[,     site_id := as.character(site_id)]

# =============================================================================
# 2. PRODUCTION  (Reference scenario)
# -----------------------------------------------------------------------------
# Straight from `indiv_prod_output`, which the pipeline already produces at
# refinery level and writes to tables/other/indiv_prod_output.csv. No derivation.
# =============================================================================

# NOTE ON UNITS: gasoline, diesel, jet and the renewable fuels have different
# energy contents, so adding their volumes in raw barrels does not give a
# meaningful physical total. The pipeline handles this with `indiv_prod_output_bge`,
# which converts each fuel to barrels of gasoline equivalent using the energy
# conversion factors in Methods 6.2. We therefore report:
#   production_bge - the energy-consistent total across fuels (use this)
#   crude_bbl      - crude throughput, a single commodity, so a plain sum is valid
# Per-fuel volumes in barrels are reported separately below, where summing is not
# involved and barrels are the natural unit.

# STATEWIDE DENOMINATOR: every quantity below is also computed for all 16
# modelled refineries, so each focus refinery can be reported as a share of the
# California refining sector rather than as a bare level. Production, crude and
# GHG are all plain sums over sites, so the statewide total is simply the
# unfiltered aggregate - no attribution question arises (unlike health, section
# 4f). Filtering to SITES is therefore deferred until after the sector total.

production_all <- indiv_prod_output_bge %>%
  filter(
    demand_scenario   == REF_DEMAND,
    refining_scenario == REF_REFINING,
    year %in% YEARS
  ) %>%
  group_by(site_id, year) %>%
  summarise(production_bge = sum(value, na.rm = TRUE), .groups = "drop")

stopifnot(n_distinct(production_all$site_id) == 16)

production <- production_all %>% filter(site_id %in% SITES)

production_statewide <- production_all %>%
  group_by(year) %>%
  summarise(statewide_production_bge = sum(production_bge, na.rm = TRUE),
            .groups = "drop")

# Per-fuel volumes in barrels, kept long so no cross-fuel addition occurs.
production_by_fuel_all <- indiv_prod_output %>%
  filter(
    demand_scenario   == REF_DEMAND,
    refining_scenario == REF_REFINING,
    year %in% YEARS
  ) %>%
  group_by(site_id, year, fuel) %>%
  summarise(production_bbl = sum(value, na.rm = TRUE), .groups = "drop")

production_by_fuel <- production_by_fuel_all %>% filter(site_id %in% SITES)

production_by_fuel_statewide <- production_by_fuel_all %>%
  group_by(year, fuel) %>%
  summarise(statewide_production_bbl = sum(production_bbl, na.rm = TRUE),
            .groups = "drop")

# =============================================================================
# 3. GHG EMISSIONS  (Reference scenario)
# -----------------------------------------------------------------------------
# `refining_sites_cons_ghg_2019_2045` already carries `ghg_kg` per site x
# scenario x year, built by organize_consumption_ghg_outputs()
# (R/health/census_pm.R:225). It is never written to CSV by the pipeline, which
# is why it has to be pulled from the store.
#
# Methods 6.4: GHG emission factors are CLUSTER-level, because crude throughput
# is only public at the cluster level. Within a cluster, modelled site GHG is
# therefore exactly proportional to site crude consumption (verified: implied EF
# is constant at 60.31312 kg/bbl South, 60.10967 kg/bbl North). Treat site-level
# GHG as an allocation of cluster emissions, not an independent estimate.
#
# `ghg_kg` is NA for year 2019 for every site (the historical base year, outside
# the 2020-2045 study window); filtering to YEARS removes those rows.
# =============================================================================

ghg_all <- site_cons_ghg %>%
  filter(
    demand_scenario   == REF_DEMAND,
    refining_scenario == REF_REFINING,
    year %in% YEARS
  ) %>%
  transmute(
    site_id,
    year,
    crude_bbl = bbls_consumed,
    ghg_mtco2e = ghg_kg / 1e9   # kg -> million tonnes
  )

# No site may carry an NA inside the study window, or the statewide sum below
# would silently drop a refinery from the denominator.
stopifnot(!anyNA(ghg_all$ghg_mtco2e), n_distinct(ghg_all$site_id) == 16)

ghg <- ghg_all %>% filter(site_id %in% SITES)

ghg_statewide <- ghg_all %>%
  group_by(year) %>%
  summarise(statewide_crude_bbl  = sum(crude_bbl, na.rm = TRUE),
            statewide_ghg_mtco2e = sum(ghg_mtco2e, na.rm = TRUE),
            .groups = "drop")

# =============================================================================
# 4. HEALTH
# =============================================================================

# -----------------------------------------------------------------------------
# 4a. Per-site criteria pollutant emissions, then per-site tract PM2.5
# -----------------------------------------------------------------------------
# Mirrors calculate_census_tract_emissions() (R/health/census_pm.R:372) up to but
# NOT including the aggregation at line 457, which is where the published
# pipeline sums over site_id:
#
#     ref_health_agg <- ref_health[, .(total_pm25 = sum(total_pm25, ...)),
#                                  by = .(GEOID, year, scen_id, ...)]
#
# Everything before that point is already refinery-resolved; we simply keep
# site_id in the grouping.
#
# Emissions (Methods 6.5, eq. 4):  e_ipt = V_it * f_pc   where f_pc is the
# cluster-level emission factor in kg/bbl and V is crude throughput.
# Concentrations (eq. 5): c_jt = sum_p omega_j(i,p) * e_ipt, with omega the
# InMAP source-receptor weights held in `srm_weighted_pm25`.

message("Building per-site census tract PM2.5...")

# Cluster lookup for our sites (North/South drives which emission factors apply).
site_cluster <- refin_locs_ct[, .(site_id, region)] %>% distinct()

# Emission factors wide, kg/bbl -> tonnes/bbl, as in census_pm.R:402-406.
ef_wide <- dt_ef %>%
  mutate(ton_bbl = kg_bbl / 1000) %>%
  select(-kg_bbl) %>%
  pivot_wider(names_from = pollutant_code, values_from = ton_bbl)

# Emissions are computed for ALL refineries, not just the focus sites, because
# the marginal attribution (section 4d) needs each tract's total concentration
# from every refinery, not only from the two of interest.
site_emissions <- site_cons_ghg %>%
  filter(
    demand_scenario   == REF_DEMAND,
    refining_scenario == REF_REFINING,
    year %in% YEARS
  ) %>%
  left_join(site_cluster, by = "site_id") %>%
  left_join(ef_wide, by = c("region" = "cluster")) %>%
  transmute(
    site_id, year,
    nh3  = bbls_consumed * NH3,
    nox  = bbls_consumed * NOX,
    pm25 = bbls_consumed * `PM25-PRI`,
    sox  = bbls_consumed * SO2,
    voc  = bbls_consumed * VOC
  ) %>%
  as.data.table()

# CRITICAL JOIN FIX: the emissions data and the source-receptor matrix use
# different site_id conventions for two facilities. The pipeline reconciles them
# before joining (census_pm.R:398-400):
#     refining[, site_id := ifelse(site_id == "t-800",  "800",   site_id)]
#     refining[, site_id := ifelse(site_id == "342-2", "34222", site_id)]
# Without this, AltAir Paramount ("t-800" vs "800") and the Phillips 66 Separate
# Unit ("342-2" vs "34222") fail to match any SRM row and drop out of the join
# silently - no warning, just two fewer refineries. In the Reference scenario
# they are 0.47% of statewide crude, which is exactly the size of the gap this
# produced against the pipeline's own statewide mortality.
site_emissions[, site_id := fifelse(site_id == "t-800", "800", site_id)]
site_emissions[, site_id := fifelse(site_id == "342-2", "34222", site_id)]

# Join emissions to the source-receptor weights and form tract concentrations.
# Cartesian by construction: 16 sites x 8,057 tracts x 26 years.
site_pm25 <- merge(
  site_emissions,
  srm_weighted_pm25,
  by = "site_id",
  allow.cartesian = TRUE
)

site_pm25[, total_pm25 :=
  weighted_totalpm25_nh3  * nh3 +
  weighted_totalpm25_nox  * nox +
  weighted_totalpm25_pm25 * pm25 +
  weighted_totalpm25_sox  * sox +
  weighted_totalpm25_voc  * voc
]

# Same GEOID patch the pipeline applies at census_pm.R:454 (a 2012 tract
# renumbering that InMAP and BenMAP disagree on).
site_pm25[, GEOID := fifelse(GEOID == "06037137000", "06037930401", GEOID)]

site_pm25 <- site_pm25[, .(total_pm25 = sum(total_pm25, na.rm = TRUE)),
                       by = .(site_id, GEOID, year)]

# -----------------------------------------------------------------------------
# 4b. Crosswalk 2019 -> 2020 census tracts
# -----------------------------------------------------------------------------
# Mirrors calculate_weighted_census_tract_emissions() (R/health/census_pm.R:693).
# The InMAP source-receptor matrix is on 2019 tract boundaries (8,057 tracts);
# population, demographics and DAC designations are on 2020 boundaries (9,129).
# `ct_xwalk` provides area-based intersection weights between the two vintages.
#
# Two quantities are crosswalked:
#   c_site - each focus refinery's own contribution
#   c_tot  - the all-refinery total, needed for the marginal attribution
#
# The crosswalk is a weighted.mean and therefore linear, so summing over
# refineries before or after it gives identical results (verified to 1e-15).
# We exploit that by summing to the all-refinery total FIRST, which keeps the
# crosswalk join small instead of expanding all 16 sites across both vintages.

setnames(site_pm25, "GEOID", "GEOID_2019")
xw_cols <- ct_xwalk[, .(GEOID_2020, GEOID_2019, rel_intersect)]

crosswalk_2020 <- function(dt, by_cols) {
  out <- merge(dt, xw_cols, by = "GEOID_2019", all = TRUE, allow.cartesian = TRUE)
  out <- out[, .(total_pm25 = weighted.mean(total_pm25, rel_intersect, na.rm = TRUE)),
             by = c(by_cols, "GEOID_2020")]
  out[!is.na(GEOID_2020)]
}

# all-refinery total concentration per tract-year
tot_pm25_2020 <- crosswalk_2020(
  site_pm25[, .(total_pm25 = sum(total_pm25, na.rm = TRUE)), by = .(GEOID_2019, year)],
  "year")
setnames(tot_pm25_2020, c("GEOID_2020", "total_pm25"), c("census_tract", "c_tot"))

# focus refineries' own contributions
site_pm25_2020 <- crosswalk_2020(site_pm25[site_id %in% SITES], c("site_id", "year"))
site_pm25_2020 <- site_pm25_2020[!is.na(site_id)]
setnames(site_pm25_2020, c("GEOID_2020", "total_pm25"), c("census_tract", "c_site"))

site_pm25_2020 <- merge(site_pm25_2020, tot_pm25_2020, by = c("census_tract", "year"))

# Attach DAC designation, as census_pm.R:735-748.
site_pm25_2020 <- merge(
  site_pm25_2020,
  raw_dac[, .(census_tract, ces4_score, disadvantaged)],
  by = "census_tract", all.x = TRUE
)
site_pm25_2020[, disadvantaged := fifelse(is.na(disadvantaged), "No", disadvantaged)]

# -----------------------------------------------------------------------------
# 4c. Baseline incidence and age-based VSL weights
# -----------------------------------------------------------------------------
# Mirrors the first half of calculate_census_tract_mortality()
# (R/health/census_pm.R:946-1070).
#
# We need only the 2019 age-based VSL variant, which is what the main text
# reports (`weighted_monetized_age_incidence_2019`). The growing-VSL and
# constant-VSL variants are SI sensitivities and are skipped here.
#
# The population base is restricted to start_age > 29, exactly as the pipeline
# does (census_pm.R:996, 1001, 1013). This is the paper's restriction, not one
# introduced here - Methods 6.7: "Because the beta term ... applies to only
# adults older than 29 years old, we restrict our sample to individuals above 29
# years" (beta from Krewski et al. 2009). Note ct_inc_45 itself spans all ages
# from 0; the filter is what narrows it to the 30+ population.

message("Building incidence and VSL weights...")

# Expand the age-VSL table so each single year of age has a value, then collapse
# onto the age bands used by ct_inc_45. Mirrors census_pm.R:978-1010.
age_vsl_sep <- dt_age_vsl %>%
  select(age_min, age_max, age_VSL_2019) %>%
  filter(age_min != age_max) %>%
  rowwise() %>%
  do(data.frame(
    age_min      = seq(.$age_min, .$age_max),
    age_max      = seq(.$age_min, .$age_max),
    age_VSL_2019 = .$age_VSL_2019
  )) %>%
  ungroup()

age_vsl_expanded <- dt_age_vsl %>%
  select(age_min, age_max, age_VSL_2019) %>%
  filter(age_min == age_max) %>%
  bind_rows(age_vsl_sep) %>%
  arrange(age_min)

# Interval join: match each single year of age to the age band it falls in.
# Kept verbatim from the pipeline (census_pm.R:995-1010) so this script matches
# the published implementation line for line. fuzzy_left_join() keeps pairs where
# age_min >= start_age AND age_max <= end_age.
vsl_cross_walk <- age_vsl_expanded %>%
  filter(age_min > 29) %>%
  fuzzyjoin::fuzzy_left_join(
    ct_inc_45 %>%
      select(start_age, end_age) %>%
      distinct() %>%
      filter(start_age > 29),
    by = c("age_min" = "start_age", "age_max" = "end_age"),
    match_fun = list(`>=`, `<=`)
  ) %>%
  group_by(start_age) %>%
  summarise(end_age = first(end_age), age_VSL_2019 = mean(age_VSL_2019)) %>%
  drop_na(start_age) %>%
  ungroup()

# -----------------------------------------------------------------------------
# ALTERNATIVE (commented, for reference): the same interval join without fuzzyjoin
# -----------------------------------------------------------------------------
# The pattern generalises: an interval / non-equi join can always be expressed as
# a Cartesian product followed by a filter on the join condition. fuzzy_left_join
# is essentially doing this internally, with the match_fun list supplying the
# comparison operators.
#
# WHY THIS IS EQUIVALENT, NOT MERELY SIMILAR
#   fuzzy_left_join is a LEFT join, so an age matching no band survives with NA in
#   start_age. The pipeline's very next call, drop_na(start_age), removes exactly
#   those rows. Filtering after a cross join never creates them in the first place,
#   so both routes land in the same place. Verified 2026-09-09: the two produce an
#   identical() 7-row crosswalk, and running this whole script each way yields
#   byte-identical output CSVs (max absolute and relative difference = 0).
#
# WHEN TO REACH FOR WHICH
#   Cross join + filter materialises every pair before discarding most of them, so
#   it is O(n*m) in memory. Fine here: 71 expanded ages x 7 bands = 497 rows. On
#   large inputs it is the wrong tool - prefer a data.table non-equi join, which
#   does the interval lookup without building the product:
#
#     bands[ages, on = .(start_age <= age_min, end_age >= age_max)]
#
#   cross_join() needs dplyr >= 1.1.0; before that the idiom was a mutate() of a
#   constant key followed by full_join(), or tidyr::expand_grid() on row indices.
#
# age_bands <- ct_inc_45 %>%
#   select(start_age, end_age) %>%
#   distinct() %>%
#   filter(start_age > 29)
#
# vsl_cross_walk <- age_vsl_expanded %>%
#   filter(age_min > 29) %>%
#   cross_join(age_bands) %>%
#   filter(age_min >= start_age, age_max <= end_age) %>%
#   group_by(start_age) %>%
#   summarise(end_age = first(end_age), age_VSL_2019 = mean(age_VSL_2019), .groups = "drop")

# Population-share-weighted baseline incidence and monetised incidence per tract
# and year. Mirrors census_pm.R:1035-1070.
ct_incidence <- ct_inc_45 %>%
  select(GEO_ID:end_age, year, pop, incidence_2015) %>%
  filter(start_age > 29) %>%
  left_join(vsl_cross_walk, by = c("start_age", "end_age")) %>%
  group_by(GEO_ID, year) %>%
  summarise(
    ct_pop = sum(pop, na.rm = TRUE),
    weighted_incidence = sum((pop / sum(pop, na.rm = TRUE)) * incidence_2015, na.rm = TRUE),
    weighted_monetized_age_incidence_2019 =
      sum((pop / sum(pop, na.rm = TRUE)) * incidence_2015 * age_VSL_2019, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(GEO_ID = str_remove(GEO_ID, "US")) %>%
  rename(census_tract = GEO_ID, pop = ct_pop) %>%
  as.data.table()

# -----------------------------------------------------------------------------
# 4d. Mortality attributable to each refinery - MARGINAL attribution
# -----------------------------------------------------------------------------
# Uses the same concentration-response expression as the pipeline
# (census_pm.R:1121-1147):
#
#     mortality(c) = (exp(beta * c) - 1) * weighted_incidence * pop
#
# but the question being asked determines which concentration goes in. Because
# the CRF is convex, three defensible attributions give different answers:
#
#   MARGINAL     mortality(c_tot) - mortality(c_tot - c_site)
#                "if this refinery shuts down and every other refinery keeps
#                 running, how much harm goes away?"  Its emissions sit on top
#                 of everyone else's baseline, where the CRF is steepest.
#                THIS IS WHAT WE REPORT - it matches the real-world event of a
#                single refinery closing while the rest of the sector operates.
#
#   STANDALONE   mortality(c_site)
#                "if this refinery were the only source, what would it cause?"
#                Its emissions sit in otherwise-clean air, where the CRF is
#                shallowest, so this is the smallest of the three. Retained
#                below as a secondary metric for comparison.
#
#   APPORTIONED  mortality(c_tot) * c_site / c_tot
#                "of the harm that occurs, what share traces to this refinery?"
#                The only definition whose shares sum to exactly 100%.
#
# Summing across all 16 refineries: marginal overshoots the joint total by about
# +0.14%, standalone falls short by about -0.14%, apportioned closes exactly.
# The three differ by well under 1% at these concentrations, so the choice is
# conceptual rather than numerical - but it must be stated, which is why the
# output carries an explicit `attribution` field.

message("Calculating attributable mortality (marginal)...")

site_mortality <- merge(
  site_pm25_2020,
  ct_incidence,
  by = c("census_tract", "year")
)

crf <- function(c) exp(BETA * c) - 1

site_mortality[, `:=`(
  # marginal: the increment this refinery adds on top of all others
  mortality_level = (crf(c_tot) - crf(c_tot - c_site)) * weighted_incidence * pop,
  benefit_level_2019 = (crf(c_tot) - crf(c_tot - c_site)) *
    weighted_monetized_age_incidence_2019 * pop,
  # standalone: retained for comparison
  mortality_standalone = crf(c_site) * weighted_incidence * pop,
  benefit_standalone_2019 = crf(c_site) * weighted_monetized_age_incidence_2019 * pop
)]

# Present value, base year 2019, as census_pm.R:1150-1155.
site_mortality[, `:=`(
  benefit_level_2019_pv      = benefit_level_2019 / ((1 + DISCOUNT_RATE)^(year - 2019)),
  benefit_standalone_2019_pv = benefit_standalone_2019 / ((1 + DISCOUNT_RATE)^(year - 2019))
)]

# -----------------------------------------------------------------------------
# 4e. Split by demographic group
# -----------------------------------------------------------------------------
# Mirrors calculate_race_disp() (census_pm.R:830-841) and calculate_mort_x_demg()
# (census_pm.R:1447-1450): tract-level outcomes are multiplied by each group's
# population share `pct` in that tract, then summed.
#
# `pop_ratios` covers all three demographic cuts used in the paper - DAC (2
# groups), Poverty (2), Race (8).

health_demo <- merge(
  site_mortality[, .(site_id, census_tract, year, mortality_level,
                     benefit_level_2019, benefit_level_2019_pv,
                     mortality_standalone, benefit_standalone_2019,
                     benefit_standalone_2019_pv)],
  pop_ratios,
  by = "census_tract",
  allow.cartesian = TRUE
)

# Annual version, retaining `year`, so downstream work can look at a single year
# (e.g. a 2021 snapshot) rather than only the 2020-2045 cumulative total.
health_demo_annual <- health_demo[
  , .(
    avoided_mortality = sum(mortality_level * pct, na.rm = TRUE),
    health_cost_2019  = sum(benefit_level_2019 * pct, na.rm = TRUE),
    health_cost_pv    = sum(benefit_level_2019_pv * pct, na.rm = TRUE),
    avoided_mortality_standalone = sum(mortality_standalone * pct, na.rm = TRUE),
    health_cost_2019_standalone  = sum(benefit_standalone_2019 * pct, na.rm = TRUE),
    health_cost_pv_standalone    = sum(benefit_standalone_2019_pv * pct, na.rm = TRUE)
  ),
  by = .(site_id, year, demo_cat, demo_group, title)
]

health_demo_summary <- health_demo[
  , .(
    # marginal (reported)
    avoided_mortality = sum(mortality_level * pct, na.rm = TRUE),
    health_cost_2019  = sum(benefit_level_2019 * pct, na.rm = TRUE),
    health_cost_pv    = sum(benefit_level_2019_pv * pct, na.rm = TRUE),
    # standalone (comparison)
    avoided_mortality_standalone = sum(mortality_standalone * pct, na.rm = TRUE),
    health_cost_2019_standalone  = sum(benefit_standalone_2019 * pct, na.rm = TRUE),
    health_cost_pv_standalone    = sum(benefit_standalone_2019_pv * pct, na.rm = TRUE)
  ),
  by = .(site_id, demo_cat, demo_group, title)
]

# -----------------------------------------------------------------------------
# 4f. Statewide health totals - the denominator for `share_of_statewide`
# -----------------------------------------------------------------------------
# Because the concentration-response function is exponential, "the statewide
# total" is not a single number, and the output carries BOTH definitions:
#
#   statewide_total           sum over all 16 refineries on the SAME attribution
#                             basis as the row (marginal rows get the sum of 16
#                             marginal contributions, standalone rows the sum of
#                             16 standalone ones). Shares defined against this
#                             sum to exactly 100% across refineries, which is
#                             what makes `share_of_statewide` interpretable.
#                             This is the convention refinery_2021_snapshot.csv
#                             already uses.
#
#   statewide_total_pipeline  the joint all-refinery total, mortality(c_tot),
#                             i.e. the harm actually occurring with every
#                             refinery running. This is the published pipeline
#                             number (tables/health/refining_state_mortality.csv)
#                             and is reproduced to within 0.001% in section 6.
#
# The two differ because marginal attribution double-counts the convexity of the
# CRF: summing the 16 marginal increments overshoots the joint total by ~0.14%,
# and summing the 16 standalone effects undershoots it by ~0.14%. Reporting both
# puts that gap in the data rather than leaving it in a console message.
#
# Computing the per-site marginal effect for all 16 refineries needs each site's
# own concentration on 2020 tract boundaries, which section 4b deliberately
# avoided building (it crosswalked the all-refinery SUM instead, to keep the join
# small). The marginal expression is not linear in c_site, so there is no way
# around it - but it costs ~10s and ~0.3 GB, so we simply build it here and
# collapse over site_id immediately, before the demographic split multiplies the
# row count twelvefold.

message("Building statewide health totals (all 16 refineries)...")

all_pm25_2020 <- crosswalk_2020(site_pm25, c("site_id", "year"))
all_pm25_2020 <- all_pm25_2020[!is.na(site_id)]
setnames(all_pm25_2020, c("GEOID_2020", "total_pm25"), c("census_tract", "c_site"))
all_pm25_2020 <- merge(all_pm25_2020, tot_pm25_2020, by = c("census_tract", "year"))
all_pm25_2020 <- merge(all_pm25_2020, ct_incidence, by = c("census_tract", "year"))

stopifnot(uniqueN(all_pm25_2020$site_id) == 16)

all_pm25_2020[, `:=`(
  mortality_level         = (crf(c_tot) - crf(c_tot - c_site)) * weighted_incidence * pop,
  benefit_level_2019      = (crf(c_tot) - crf(c_tot - c_site)) *
    weighted_monetized_age_incidence_2019 * pop,
  mortality_standalone    = crf(c_site) * weighted_incidence * pop,
  benefit_standalone_2019 = crf(c_site) * weighted_monetized_age_incidence_2019 * pop
)]

# Collapse over refineries first: the tract-year grid is 9,109 x 26 instead of
# 16 x that, and everything downstream is linear from here.
state_mort_tract <- all_pm25_2020[
  , .(mortality_level         = sum(mortality_level),
      benefit_level_2019      = sum(benefit_level_2019),
      mortality_standalone    = sum(mortality_standalone),
      benefit_standalone_2019 = sum(benefit_standalone_2019)),
  by = .(census_tract, year)
]

# The joint basis: one row per tract-year, evaluated at the combined
# concentration rather than summed over sites.
state_mort_tract_joint <- unique(all_pm25_2020[
  , .(census_tract, year, c_tot, weighted_incidence,
      weighted_monetized_age_incidence_2019, pop)
])
state_mort_tract_joint[, `:=`(
  mortality_joint    = crf(c_tot) * weighted_incidence * pop,
  benefit_joint_2019 = crf(c_tot) * weighted_monetized_age_incidence_2019 * pop
)]

state_mort_tract <- merge(
  state_mort_tract,
  state_mort_tract_joint[, .(census_tract, year, mortality_joint, benefit_joint_2019)],
  by = c("census_tract", "year")
)

# Present value, base year 2019, as in section 4d.
state_mort_tract[, `:=`(
  benefit_level_2019_pv      = benefit_level_2019      / ((1 + DISCOUNT_RATE)^(year - 2019)),
  benefit_standalone_2019_pv = benefit_standalone_2019 / ((1 + DISCOUNT_RATE)^(year - 2019)),
  benefit_joint_2019_pv      = benefit_joint_2019      / ((1 + DISCOUNT_RATE)^(year - 2019))
)]

# Same demographic split as section 4e, applied to the sector total.
state_health_demo <- merge(state_mort_tract, pop_ratios, by = "census_tract",
                           allow.cartesian = TRUE)

state_health_metrics <- function(dt, by_cols) {
  dt[, .(
    avoided_mortality = sum(mortality_level * pct, na.rm = TRUE),
    health_cost_2019  = sum(benefit_level_2019 * pct, na.rm = TRUE),
    health_cost_pv    = sum(benefit_level_2019_pv * pct, na.rm = TRUE),
    avoided_mortality_standalone = sum(mortality_standalone * pct, na.rm = TRUE),
    health_cost_2019_standalone  = sum(benefit_standalone_2019 * pct, na.rm = TRUE),
    health_cost_pv_standalone    = sum(benefit_standalone_2019_pv * pct, na.rm = TRUE),
    avoided_mortality_joint = sum(mortality_joint * pct, na.rm = TRUE),
    health_cost_2019_joint  = sum(benefit_joint_2019 * pct, na.rm = TRUE),
    health_cost_pv_joint    = sum(benefit_joint_2019_pv * pct, na.rm = TRUE)
  ), by = by_cols]
}

state_health_demo_annual  <- state_health_metrics(state_health_demo,
                                                  c("year", "demo_cat", "demo_group", "title"))
state_health_demo_summary <- state_health_metrics(state_health_demo,
                                                  c("demo_cat", "demo_group", "title"))

# =============================================================================
# 5. LABOR
# =============================================================================
# Mirrors calc_labor_outputs() (R/labor/labor_functions_product_px.R:60-315),
# keeping `site_id` through "step 1" (line 151) and "step 4" (line 191), which
# are the two points where the published pipeline drops refinery identity: first
# by aggregating to the WORK tract, then by summing to the HOME tract.
#
# Each of our two refineries occupies its own work tract, so the site is cleanly
# separable:
#     343 Phillips 66 Wilmington -> 06037980015
#     164 Valero Benicia         -> 06095252102
# (This is NOT true for every refinery: Phillips 66 Rodeo and the Phillips 66
# "Separate Unit" share tract 06013358000 and could not be split this way.)
#
# We report LEVELS - the employment and compensation each refinery supports in
# the Reference scenario. The pipeline's `_l` / `total_emp_revised` variants are
# year-over-year differences used to measure LOSSES under reemployment
# assumptions; they are not meaningful as a level and so are not computed here.

message("Calculating labor impacts...")

# Map fuels to the three priced products, as labor_functions_product_px.R:63-71.
# Computed for ALL 16 refineries; the focus subset is taken afterwards. The
# statewide branch (section 5b) needs the full set, and unlike health there is no
# attribution subtlety here - equation 11 is linear in revenue, so the sector
# total is exactly the sum of its parts.
site_revenue_all <- indiv_prod_output %>%
  filter(
    demand_scenario   == REF_DEMAND,
    refining_scenario == REF_REFINING,
    year %in% YEARS
  ) %>%
  mutate(
    fuel = as.character(fuel),
    product = case_when(
      fuel %in% c("gasoline", "drop-in gasoline")  ~ "gasoline",
      fuel %in% c("diesel", "renewable diesel")    ~ "diesel",
      TRUE                                         ~ "jet_fuel"
    )
  )

# Product prices. Under the "2020 prices" scenario the pipeline holds 2020 prices
# flat across all years (labor_functions_product_px.R:96-113); we replicate that
# by joining the 2020 price onto every year.
px_2020 <- product_px %>%
  filter(year == 2020, oil_price_scenario == OIL_PRICE_SCENARIO) %>%
  select(product, product_price)

site_revenue_all <- site_revenue_all %>%
  left_join(px_2020, by = "product") %>%
  mutate(revenue = value * product_price) %>%
  group_by(site_id, year) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
  as.data.table()

# Attach each refinery's work tract (11-digit tract from the 15-digit block
# geocode), as labor_functions_product_px.R:146-158.
all_work_tract <- unique(
  refin_locs_ct[, .(site_id, w_tract_geocode = substr(GEOID, 1, 11))]
)

site_revenue_all <- merge(site_revenue_all, all_work_tract, by = "site_id")
stopifnot(!anyNA(site_revenue_all$w_tract_geocode))

site_revenue <- site_revenue_all[site_id %in% SITES]

# Each FOCUS refinery occupies its own work tract, which is what makes the
# per-site split valid. Not true sector-wide: 99999/271 share 06029000507 and
# 342/342-2 share 06013358000, which is why the statewide branch below goes
# through the work tract rather than through site_id.
stopifnot(uniqueN(site_revenue$w_tract_geocode) == length(SITES))

# Apply IMPLAN direct multipliers per $1M of output value, distributed to worker
# home tracts by LODES commute shares (Methods eq. 11).
# labor_functions_product_px.R:169-186.
labor_direct <- merge(
  site_revenue,
  dt_direct_multipliers[, .(w_tract_geocode, h_tract_geocode, emp.rev, ec.rev)],
  by = "w_tract_geocode",
  allow.cartesian = TRUE
)

labor_direct[, `:=`(
  total_emp  = (revenue / 1e6) * emp.rev,
  total_comp = (revenue / 1e6) * ec.rev
)]

labor_direct <- labor_direct[
  , .(total_emp = sum(total_emp), total_comp = sum(total_comp)),
  by = .(site_id, census_tract = h_tract_geocode, year)
]

# Convert to 2019 dollars and discount, as labor_functions_product_px.R:214-221.
labor_direct[, total_comp_usd19 := total_comp * CPI2019 / CPI2020]
labor_direct[, total_comp_pv := total_comp_usd19 / ((1 + DISCOUNT_RATE)^(year - 2019))]

# Demographic split, mirroring calculate_labor_x_demg_annual()
# (labor_functions_product_px.R:1068-1099).
labor_demo <- merge(labor_direct, pop_ratios, by = "census_tract",
                    allow.cartesian = TRUE)

labor_demo[, `:=`(
  demo_emp        = total_emp * pct,
  demo_comp_usd19 = total_comp_usd19 * pct,
  demo_comp_pv    = total_comp_pv * pct
)]

labor_demo_annual <- labor_demo[
  , .(
    fte_job_years   = sum(demo_emp, na.rm = TRUE),
    compensation    = sum(demo_comp_usd19, na.rm = TRUE),
    compensation_pv = sum(demo_comp_pv, na.rm = TRUE)
  ),
  by = .(site_id, year, demo_cat, demo_group, title)
]

labor_demo_summary <- labor_demo[
  , .(
    fte_job_years   = sum(demo_emp, na.rm = TRUE),
    compensation    = sum(demo_comp_usd19, na.rm = TRUE),
    compensation_pv = sum(demo_comp_pv, na.rm = TRUE)
  ),
  by = .(site_id, demo_cat, demo_group, title)
]

# -----------------------------------------------------------------------------
# 5b. Statewide labor totals - the denominator for `share_of_statewide`
# -----------------------------------------------------------------------------
# The same four steps, run over all 16 refineries. Revenue is aggregated to the
# WORK tract before the multiplier join, exactly as the pipeline does at
# labor_functions_product_px.R:149-176, which is what makes shared work tracts
# (99999/271, 342/342-2) harmless here: the join is by tract, not by site.
#
# Labor is linear throughout, so there is no marginal-vs-joint distinction as
# there is for health - the sector total IS the sum of the refinery parts. The
# `statewide_total_pipeline` column therefore carries the same value as
# `statewide_total` for labor rows, and section 6 check (iv) confirms both
# reproduce the pipeline's own published state table.

message("Calculating statewide labor impacts (all 16 refineries)...")

state_revenue_wt <- site_revenue_all[
  , .(revenue = sum(revenue, na.rm = TRUE)), by = .(w_tract_geocode, year)
]

state_labor_direct <- merge(
  state_revenue_wt,
  dt_direct_multipliers[, .(w_tract_geocode, h_tract_geocode, emp.rev, ec.rev)],
  by = "w_tract_geocode",
  allow.cartesian = TRUE
)

state_labor_direct[, `:=`(
  total_emp  = (revenue / 1e6) * emp.rev,
  total_comp = (revenue / 1e6) * ec.rev
)]

state_labor_direct <- state_labor_direct[
  , .(total_emp = sum(total_emp), total_comp = sum(total_comp)),
  by = .(census_tract = h_tract_geocode, year)
]

state_labor_direct[, total_comp_usd19 := total_comp * CPI2019 / CPI2020]
state_labor_direct[, total_comp_pv := total_comp_usd19 / ((1 + DISCOUNT_RATE)^(year - 2019))]

state_labor_demo <- merge(state_labor_direct, pop_ratios, by = "census_tract",
                          allow.cartesian = TRUE)

state_labor_demo[, `:=`(
  demo_emp        = total_emp * pct,
  demo_comp_usd19 = total_comp_usd19 * pct,
  demo_comp_pv    = total_comp_pv * pct
)]

state_labor_metrics <- function(dt, by_cols) {
  dt[, .(
    fte_job_years   = sum(demo_emp, na.rm = TRUE),
    compensation    = sum(demo_comp_usd19, na.rm = TRUE),
    compensation_pv = sum(demo_comp_pv, na.rm = TRUE)
  ), by = by_cols]
}

state_labor_demo_annual  <- state_labor_metrics(state_labor_demo,
                                                c("year", "demo_cat", "demo_group", "title"))
state_labor_demo_summary <- state_labor_metrics(state_labor_demo,
                                                c("demo_cat", "demo_group", "title"))

# =============================================================================
# 6. VALIDATION
# -----------------------------------------------------------------------------
# Three checks, printed to the console. These are the checks worth failing on:
# a silently wrong join here would produce plausible-looking numbers.
# =============================================================================

message("\n--- validation ---")

# (i) Demographic shares should reconcile: DAC and Poverty are 2-group partitions
#     and Race is an 8-group partition of the same tract population, so all three
#     cuts must total to the same site-level figure.
recon <- health_demo_summary[
  , .(total_mortality = sum(avoided_mortality)), by = .(site_id, demo_cat)
]
cat("\n(i) mortality total by demographic cut (should agree within a cut):\n")
print(dcast(recon, site_id ~ demo_cat, value.var = "total_mortality"))

# (ii) Attributed mortality against the pipeline's all-refinery total, on both
#      bases, so the effect of the attribution choice is visible rather than
#      implicit.
cat("\n(ii) attributed mortality vs pipeline all-refinery total (Reference, 2020-2045):\n")
pipeline_mort <- fread(
  "outputs/rev-submission/cuf=0.6_beta-scenario=main/tables/health/refining_state_mortality.csv"
)[scen_id == paste(REF_DEMAND, REF_REFINING)]
marg <- sum(site_mortality$mortality_level)
alone <- sum(site_mortality$mortality_standalone)
cat(sprintf("    all 16 refineries (pipeline) : %9.1f deaths\n", pipeline_mort$cumul_mort_level))
cat(sprintf("    343 + 164, MARGINAL (reported): %8.1f deaths  (%.2f%% of total)\n",
            marg, 100 * marg / pipeline_mort$cumul_mort_level))
cat(sprintf("    343 + 164, standalone         : %8.1f deaths  (%.2f%% of total)\n",
            alone, 100 * alone / pipeline_mort$cumul_mort_level))
cat(sprintf("    marginal exceeds standalone by: %8.2f%%\n", 100 * (marg / alone - 1)))

# (ii-b) The all-refinery concentration must reconcile with the pipeline's own,
#        or the marginal attribution is built on the wrong baseline.
cat("\n(ii-b) all-refinery mortality rebuilt from c_tot (should be near pipeline):\n")
tot_check <- unique(site_mortality[, .(census_tract, year, c_tot, weighted_incidence, pop)])
tot_check <- tot_check[, sum((exp(BETA * c_tot) - 1) * weighted_incidence * pop)]
cat(sprintf("    rebuilt from c_tot           : %9.1f deaths  (%+.2f%% vs pipeline)\n",
            tot_check, 100 * (tot_check / pipeline_mort$cumul_mort_level - 1)))

# (iii) Labor is linear in revenue, so site contributions should sum exactly.
#       Compare against the pipeline's own tract-level labor output.
cat("\n(iii) labor levels, Reference scenario, cumulative 2020-2045:\n")
print(labor_demo_summary[demo_cat == "DAC",
  .(fte_job_years = round(sum(fte_job_years)),
    compensation_pv_musd = round(sum(compensation_pv) / 1e6)), by = site_id])

# (iv) The statewide labor denominator must reproduce the pipeline's own
#      published state table, or every `share_of_statewide` on a labor row is
#      wrong. This is an exact check, not an approximate one: both sides run the
#      same linear multiplier chain, so agreement should be to floating point.
cat("\n(iv) statewide labor vs pipeline's published state table:\n")
pipeline_labor <- fread(
  "outputs/rev-submission/cuf=0.6_beta-scenario=main/intermediate/labor/state_labor_direct_impacts_demo_annual.csv"
)[demand_scenario == REF_DEMAND & refining_scenario == REF_REFINING &
  product_scenario == PRODUCT_SCENARIO & oil_price_scenario == OIL_PRICE_SCENARIO]

labor_check <- merge(
  state_labor_demo_annual,
  pipeline_labor[, .(year, demo_cat, demo_group, title,
                     p_emp = sum_demo_emp, p_comp = sum_demo_usd19_h,
                     p_pv = sum_demo_comp_pv_h)],
  by = c("year", "demo_cat", "demo_group", "title")
)
stopifnot(nrow(labor_check) == nrow(pipeline_labor))
cat(sprintf("    rows matched: %d\n", nrow(labor_check)))
cat(sprintf("    max relative difference - employment  : %.2e\n",
            max(abs(labor_check$fte_job_years / labor_check$p_emp - 1))))
cat(sprintf("    max relative difference - compensation: %.2e\n",
            max(abs(labor_check$compensation / labor_check$p_comp - 1))))
cat(sprintf("    max relative difference - comp (PV)   : %.2e\n",
            max(abs(labor_check$compensation_pv / labor_check$p_pv - 1))))
stopifnot(max(abs(labor_check$fte_job_years / labor_check$p_emp - 1)) < 1e-8)

# (v) The two statewide health bases, side by side, so the size of the
#     attribution gap carried in the output columns is explicit.
cat("\n(v) statewide health denominators, cumulative 2020-2045:\n")
state_dac <- state_health_demo_summary[demo_cat == "DAC"]
cat(sprintf("    joint, mortality(c_tot)  [statewide_total_pipeline]: %9.1f deaths\n",
            sum(state_dac$avoided_mortality_joint)))
cat(sprintf("    sum of 16, marginal      [statewide_total]         : %9.1f deaths (%+.3f%%)\n",
            sum(state_dac$avoided_mortality),
            100 * (sum(state_dac$avoided_mortality) /
                     sum(state_dac$avoided_mortality_joint) - 1)))
cat(sprintf("    sum of 16, standalone    [statewide_total]         : %9.1f deaths (%+.3f%%)\n",
            sum(state_dac$avoided_mortality_standalone),
            100 * (sum(state_dac$avoided_mortality_standalone) /
                     sum(state_dac$avoided_mortality_joint) - 1)))
cat(sprintf("    pipeline CSV                                       : %9.1f deaths\n",
            pipeline_mort$cumul_mort_level))

# =============================================================================
# 7. ASSEMBLE TIDY LONG OUTPUT
# -----------------------------------------------------------------------------
# Schema deliberately matches tables/health-and-labor/state_health_labor_ouputs.csv
# so these rows can sit alongside the published state-level table.
#
# THREE STATEWIDE COLUMNS are attached to every row:
#   statewide_total           all 16 refineries on the same basis as the row
#   statewide_total_pipeline  the joint sector total (= statewide_total for
#                             labor, which is linear; smaller than the marginal
#                             sum and larger than the standalone sum for health)
#   share_of_statewide        value / statewide_total, so shares are additive
#                             across refineries
# =============================================================================

# Statewide health, reshaped to the same long key. The `_joint` columns become
# statewide_total_pipeline; the marginal and standalone columns become
# statewide_total on their respective attribution bases.
state_long_health <- state_health_demo_summary %>%
  pivot_longer(
    -c(demo_cat, demo_group, title),
    names_to = "metric", values_to = "statewide_total"
  ) %>%
  mutate(
    basis  = case_when(str_detect(metric, "_standalone$") ~ "standalone",
                       str_detect(metric, "_joint$")      ~ "joint",
                       TRUE                               ~ "marginal"),
    metric = str_remove(metric, "_(standalone|joint)$")
  )

state_health_cols <- state_long_health %>%
  filter(basis != "joint") %>%
  rename(health_attribution = basis) %>%
  left_join(
    state_long_health %>%
      filter(basis == "joint") %>%
      select(demo_cat, demo_group, title, metric,
             statewide_total_pipeline = statewide_total),
    by = c("demo_cat", "demo_group", "title", "metric")
  )

# Statewide labor. No attribution basis applies, and the linear multiplier chain
# makes the joint total identical to the sum of refineries - verified against the
# pipeline's own state table in check (iv).
state_labor_cols <- state_labor_demo_summary %>%
  pivot_longer(
    c(fte_job_years, compensation, compensation_pv),
    names_to = "metric", values_to = "statewide_total"
  ) %>%
  mutate(statewide_total_pipeline = statewide_total)

long_health <- health_demo_summary %>%
  pivot_longer(
    c(avoided_mortality, health_cost_2019, health_cost_pv,
      avoided_mortality_standalone, health_cost_2019_standalone,
      health_cost_pv_standalone),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(
    segment = "health",
    # marginal is the reported basis; standalone rows are carried for comparison
    health_attribution = if_else(str_detect(metric, "_standalone$"),
                                 "standalone", "marginal"),
    metric = str_remove(metric, "_standalone$"),
    metric_desc = case_when(
      metric == "avoided_mortality" ~ "attributable_mortality",
      TRUE                          ~ "attributable_health_cost"
    ),
    unit_desc = case_when(
      metric == "avoided_mortality" ~ "persons",
      metric == "health_cost_2019"  ~ "USD (2019 age-based VSL)",
      metric == "health_cost_pv"    ~ "USD PV 2019 (2019 age-based VSL)"
    )
  ) %>%
  inner_join(state_health_cols,
             by = c("demo_cat", "demo_group", "title", "metric",
                    "health_attribution"))

long_labor <- labor_demo_summary %>%
  pivot_longer(
    c(fte_job_years, compensation, compensation_pv),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(
    segment = "labor",
    # Labor is linear in refinery revenue, so marginal, standalone and
    # apportioned attributions coincide exactly. The distinction is a property
    # of the exponential health CRF only.
    health_attribution = NA_character_,
    metric_desc = if_else(metric == "fte_job_years", "employment_supported",
                          "compensation_supported"),
    unit_desc = case_when(
      metric == "fte_job_years"   ~ "fte-job-years (supported; no-reemployment basis)",
      metric == "compensation"    ~ "USD 2019 (supported; no-reemployment basis)",
      metric == "compensation_pv" ~ "USD PV 2019 (supported; no-reemployment basis)"
    )
  ) %>%
  inner_join(state_labor_cols, by = c("demo_cat", "demo_group", "title", "metric"))

refinery_level_results <- bind_rows(long_health, long_labor) %>%
  mutate(
    refinery_name     = SITE_LABELS[site_id],
    demand_scenario   = REF_DEMAND,
    refining_scenario = REF_REFINING,
    scenario          = "Reference",
    product_scenario  = PRODUCT_SCENARIO,
    oil_price_scenario = OIL_PRICE_SCENARIO,
    attribution       = "level",    # not a difference vs Reference
    share_of_statewide = value / statewide_total
  ) %>%
  select(
    site_id, refinery_name, scenario, demand_scenario, refining_scenario,
    product_scenario, oil_price_scenario, attribution, health_attribution,
    demo_cat, demo_group, title, segment, metric, metric_desc, unit_desc, value,
    statewide_total, statewide_total_pipeline, share_of_statewide
  ) %>%
  arrange(site_id, segment, health_attribution, demo_cat, demo_group, metric)

# Every row must have found a statewide denominator; a silent join failure here
# would show up as a missing share rather than an error.
stopifnot(!anyNA(refinery_level_results$statewide_total),
          !anyNA(refinery_level_results$share_of_statewide))

# Annual physical outputs kept as a separate, simpler table (no demographic
# dimension applies to production or GHG).
refinery_annual_physical <- production %>%
  full_join(ghg, by = c("site_id", "year")) %>%
  inner_join(production_statewide, by = "year") %>%
  inner_join(ghg_statewide, by = "year") %>%
  mutate(
    refinery_name = SITE_LABELS[site_id],
    share_of_statewide_production_bge = production_bge / statewide_production_bge,
    share_of_statewide_crude_bbl      = crude_bbl / statewide_crude_bbl,
    share_of_statewide_ghg_mtco2e     = ghg_mtco2e / statewide_ghg_mtco2e
  ) %>%
  select(site_id, refinery_name, year, production_bge, crude_bbl, ghg_mtco2e,
         statewide_production_bge, statewide_crude_bbl, statewide_ghg_mtco2e,
         share_of_statewide_production_bge, share_of_statewide_crude_bbl,
         share_of_statewide_ghg_mtco2e) %>%
  arrange(site_id, year)

refinery_annual_production_by_fuel <- production_by_fuel %>%
  inner_join(production_by_fuel_statewide, by = c("year", "fuel")) %>%
  mutate(refinery_name = SITE_LABELS[site_id],
         share_of_statewide = production_bbl / statewide_production_bbl) %>%
  select(site_id, refinery_name, year, fuel, production_bbl,
         statewide_production_bbl, share_of_statewide) %>%
  arrange(site_id, year, fuel)

# -----------------------------------------------------------------------------
# 8. WRITE
# -----------------------------------------------------------------------------

fwrite(refinery_level_results,
       file.path(OUT_DIR, "refinery_level_health_labor.csv"))
fwrite(refinery_annual_physical,
       file.path(OUT_DIR, "refinery_level_production_ghg.csv"))
fwrite(refinery_annual_production_by_fuel,
       file.path(OUT_DIR, "refinery_level_production_by_fuel.csv"))

# Annual health + labor by demographic group, long form.
# The same three statewide columns as the cumulative table, but keyed on year as
# well, so a single-year share (e.g. 2021) can be read straight off a row.
ANN_ID <- c("year", "demo_cat", "demo_group", "title")

state_health_annual <- melt(
  state_health_demo_annual, id.vars = ANN_ID,
  variable.name = "metric", value.name = "statewide_total")
state_health_annual[, basis := fifelse(grepl("_standalone$", metric), "standalone",
                                fifelse(grepl("_joint$", metric), "joint", "marginal"))]
state_health_annual[, metric := sub("_(standalone|joint)$", "", as.character(metric))]

state_health_annual <- merge(
  state_health_annual[basis != "joint"][, .(year, demo_cat, demo_group, title, metric,
                                            health_attribution = basis, statewide_total)],
  state_health_annual[basis == "joint"][, .(year, demo_cat, demo_group, title, metric,
                                            statewide_total_pipeline = statewide_total)],
  by = c(ANN_ID, "metric"))
state_health_annual[, segment := "health"]

state_labor_annual <- melt(
  state_labor_demo_annual, id.vars = ANN_ID,
  variable.name = "metric", value.name = "statewide_total")
state_labor_annual[, `:=`(metric = as.character(metric),
                          statewide_total_pipeline = statewide_total,
                          health_attribution = NA_character_,
                          segment = "labor")]

annual_long <- rbind(
  merge(
    melt(health_demo_annual,
         id.vars = c("site_id", "year", "demo_cat", "demo_group", "title"),
         variable.name = "metric", value.name = "value")[
      , `:=`(segment = "health",
             health_attribution = fifelse(grepl("_standalone$", metric),
                                          "standalone", "marginal"),
             metric = sub("_standalone$", "", metric))],
    state_health_annual,
    by = c(ANN_ID, "metric", "segment", "health_attribution")),
  merge(
    melt(labor_demo_annual,
         id.vars = c("site_id", "year", "demo_cat", "demo_group", "title"),
         variable.name = "metric", value.name = "value")[
      , `:=`(segment = "labor", health_attribution = NA_character_,
             metric = as.character(metric))],
    state_labor_annual[, -"health_attribution"],
    by = c(ANN_ID, "metric", "segment")),
  use.names = TRUE)

annual_long[, refinery_name := SITE_LABELS[site_id]]
annual_long[, share_of_statewide := value / statewide_total]
setcolorder(annual_long, c("site_id", "refinery_name", "year", "segment",
                           "health_attribution", "demo_cat", "demo_group",
                           "title", "metric", "value",
                           "statewide_total", "statewide_total_pipeline",
                           "share_of_statewide"))
setorder(annual_long, site_id, segment, year, demo_cat, demo_group, metric)

stopifnot(nrow(annual_long) ==
            nrow(health_demo_annual) * 6 + nrow(labor_demo_annual) * 3,
          !anyNA(annual_long$statewide_total))

fwrite(annual_long, file.path(OUT_DIR, "refinery_level_health_labor_annual.csv"))

message("\nWrote:")
message("  ", file.path(OUT_DIR, "refinery_level_health_labor.csv"),
        "  (", nrow(refinery_level_results), " rows)")
message("  ", file.path(OUT_DIR, "refinery_level_production_ghg.csv"),
        "  (", nrow(refinery_annual_physical), " rows)")
message("  ", file.path(OUT_DIR, "refinery_level_production_by_fuel.csv"),
        "  (", nrow(refinery_annual_production_by_fuel), " rows)")
message("  ", file.path(OUT_DIR, "refinery_level_health_labor_annual.csv"),
        "  (", nrow(annual_long), " rows)")
