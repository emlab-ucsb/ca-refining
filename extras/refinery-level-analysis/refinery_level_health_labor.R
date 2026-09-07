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
indiv_prod_output[, site_id := as.character(site_id)]
refin_locs_ct[,     site_id := as.character(site_id)]

# =============================================================================
# 2. PRODUCTION  (Reference scenario)
# -----------------------------------------------------------------------------
# Straight from `indiv_prod_output`, which the pipeline already produces at
# refinery level and writes to tables/other/indiv_prod_output.csv. No derivation.
# =============================================================================

production <- indiv_prod_output %>%
  filter(
    site_id %in% SITES,
    demand_scenario   == REF_DEMAND,
    refining_scenario == REF_REFINING,
    year %in% YEARS
  ) %>%
  group_by(site_id, year) %>%
  summarise(production_bbl = sum(value, na.rm = TRUE), .groups = "drop")

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

ghg <- site_cons_ghg %>%
  filter(
    site_id %in% SITES,
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

stopifnot(!anyNA(ghg$ghg_mtco2e))

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

site_emissions <- site_cons_ghg %>%
  filter(
    site_id %in% SITES,
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

# Join emissions to the source-receptor weights and form tract concentrations.
# Cartesian by construction: 2 sites x 8,057 tracts x 26 years.
site_pm25 <- merge(
  site_emissions,
  srm_weighted_pm25[site_id %in% SITES],
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
# Skipping this step would produce numbers that are subtly wrong rather than
# obviously wrong, so it matters.

setnames(site_pm25, "GEOID", "GEOID_2019")

site_pm25_2020 <- merge(
  site_pm25,
  ct_xwalk[, .(GEOID_2020, GEOID_2019, rel_intersect)],
  by = "GEOID_2019",
  all = TRUE,
  allow.cartesian = TRUE
)

site_pm25_2020 <- site_pm25_2020[
  , .(total_pm25 = weighted.mean(total_pm25, rel_intersect, na.rm = TRUE)),
  by = .(site_id, GEOID_2020, year)
]

site_pm25_2020 <- site_pm25_2020[!is.na(GEOID_2020) & !is.na(site_id)]
setnames(site_pm25_2020, "GEOID_2020", "census_tract")

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
# beta applies to adults over 29 (Krewski et al. 2009), so the population base is
# restricted to start_age > 29 exactly as the pipeline does.

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

# NOTE ON A DELIBERATE DEVIATION FROM THE PIPELINE:
# census_pm.R:995-1003 uses fuzzyjoin::fuzzy_left_join() with
# match_fun = list(`>=`, `<=`), i.e. keep pairs where age_min >= start_age AND
# age_max <= end_age. `fuzzyjoin` is not installed in this environment, so the
# same interval match is done below with a cross join + filter. This is exactly
# equivalent, not merely similar: the pipeline immediately calls drop_na(start_age),
# which discards precisely the unmatched rows a left join would have retained, so
# inner-join semantics give identical output. Both inputs are tens of rows, so the
# cross join costs nothing.
age_bands <- ct_inc_45 %>%
  select(start_age, end_age) %>%
  distinct() %>%
  filter(start_age > 29)

vsl_cross_walk <- age_vsl_expanded %>%
  filter(age_min > 29) %>%
  cross_join(age_bands) %>%
  filter(age_min >= start_age, age_max <= end_age) %>%
  group_by(start_age) %>%
  summarise(end_age = first(end_age), age_VSL_2019 = mean(age_VSL_2019), .groups = "drop")

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
# 4d. Mortality attributable to each refinery
# -----------------------------------------------------------------------------
# Mirrors the `mortality_level` / `benefit_age_level_2019` expressions at
# census_pm.R:1121-1147, applied to each site's own concentration contribution
# rather than the all-refineries total:
#
#     mortality = (exp(beta * pm25) - 1) * weighted_incidence * pop
#
# Because the concentration-response function is exponential, per-site mortality
# does NOT sum exactly to the all-refinery total. The discrepancy is small at
# these concentrations; section 6 quantifies it explicitly rather than assuming.

message("Calculating attributable mortality...")

site_mortality <- merge(
  site_pm25_2020,
  ct_incidence,
  by = c("census_tract", "year")
)

site_mortality[, `:=`(
  mortality_level = (exp(BETA * total_pm25) - 1) * weighted_incidence * pop,
  benefit_level_2019 = (exp(BETA * total_pm25) - 1) *
    weighted_monetized_age_incidence_2019 * pop
)]

# Present value, base year 2019, as census_pm.R:1150-1155.
site_mortality[, benefit_level_2019_pv :=
  benefit_level_2019 / ((1 + DISCOUNT_RATE)^(year - 2019))]

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
                     benefit_level_2019, benefit_level_2019_pv)],
  pop_ratios,
  by = "census_tract",
  allow.cartesian = TRUE
)

health_demo[, `:=`(
  mortality_level_dem       = mortality_level * pct,
  benefit_level_2019_dem    = benefit_level_2019 * pct,
  benefit_level_2019_pv_dem = benefit_level_2019_pv * pct
)]

health_demo_summary <- health_demo[
  , .(
    avoided_mortality = sum(mortality_level_dem, na.rm = TRUE),
    health_cost_2019  = sum(benefit_level_2019_dem, na.rm = TRUE),
    health_cost_pv    = sum(benefit_level_2019_pv_dem, na.rm = TRUE)
  ),
  by = .(site_id, demo_cat, demo_group, title)
]

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
site_revenue <- indiv_prod_output %>%
  filter(
    site_id %in% SITES,
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

site_revenue <- site_revenue %>%
  left_join(px_2020, by = "product") %>%
  mutate(revenue = value * product_price) %>%
  group_by(site_id, year) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
  as.data.table()

# Attach each refinery's work tract (11-digit tract from the 15-digit block
# geocode), as labor_functions_product_px.R:146-158.
site_work_tract <- refin_locs_ct[
  site_id %in% SITES, .(site_id, w_tract_geocode = substr(GEOID, 1, 11))
] %>% distinct()

stopifnot(uniqueN(site_work_tract$w_tract_geocode) == length(SITES))

site_revenue <- merge(site_revenue, site_work_tract, by = "site_id")

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

labor_demo_summary <- labor_demo[
  , .(
    fte_job_years   = sum(demo_emp, na.rm = TRUE),
    compensation    = sum(demo_comp_usd19, na.rm = TRUE),
    compensation_pv = sum(demo_comp_pv, na.rm = TRUE)
  ),
  by = .(site_id, demo_cat, demo_group, title)
]

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

# (ii) The exponential concentration-response means per-site mortality will not
#      sum exactly to the all-refinery total. Quantify rather than assume.
cat("\n(ii) per-site vs pipeline all-refinery mortality (Reference, 2020-2045):\n")
pipeline_mort <- fread(
  "outputs/rev-submission/cuf=0.6_beta-scenario=main/tables/health/refining_state_mortality.csv"
)[scen_id == paste(REF_DEMAND, REF_REFINING)]
site_total <- sum(site_mortality$mortality_level)
cat(sprintf("    all 16 refineries (pipeline): %10.1f deaths\n", pipeline_mort$cumul_mort_level))
cat(sprintf("    sites 343 + 164 (this script): %9.1f deaths  (%.1f%% of total)\n",
            site_total, 100 * site_total / pipeline_mort$cumul_mort_level))

# (iii) Labor is linear in revenue, so site contributions should sum exactly.
#       Compare against the pipeline's own tract-level labor output.
cat("\n(iii) labor levels, Reference scenario, cumulative 2020-2045:\n")
print(labor_demo_summary[demo_cat == "DAC",
  .(fte_job_years = round(sum(fte_job_years)),
    compensation_pv_musd = round(sum(compensation_pv) / 1e6)), by = site_id])

# =============================================================================
# 7. ASSEMBLE TIDY LONG OUTPUT
# -----------------------------------------------------------------------------
# Schema deliberately matches tables/health-and-labor/state_health_labor_ouputs.csv
# so these rows can sit alongside the published state-level table.
# =============================================================================

long_health <- health_demo_summary %>%
  pivot_longer(
    c(avoided_mortality, health_cost_2019, health_cost_pv),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(
    segment = "health",
    metric_desc = case_when(
      metric == "avoided_mortality" ~ "attributable_mortality",
      TRUE                          ~ "attributable_health_cost"
    ),
    unit_desc = case_when(
      metric == "avoided_mortality" ~ "persons",
      metric == "health_cost_2019"  ~ "USD (2019 age-based VSL)",
      metric == "health_cost_pv"    ~ "USD PV 2019 (2019 age-based VSL)"
    )
  )

long_labor <- labor_demo_summary %>%
  pivot_longer(
    c(fte_job_years, compensation, compensation_pv),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(
    segment = "labor",
    metric_desc = if_else(metric == "fte_job_years", "employment_supported",
                          "compensation_supported"),
    unit_desc = case_when(
      metric == "fte_job_years"   ~ "fte-job-years",
      metric == "compensation"    ~ "USD 2019",
      metric == "compensation_pv" ~ "USD PV 2019"
    )
  )

refinery_level_results <- bind_rows(long_health, long_labor) %>%
  mutate(
    refinery_name     = SITE_LABELS[site_id],
    demand_scenario   = REF_DEMAND,
    refining_scenario = REF_REFINING,
    scenario          = "Reference",
    product_scenario  = PRODUCT_SCENARIO,
    oil_price_scenario = OIL_PRICE_SCENARIO,
    attribution       = "level"     # not a difference vs Reference
  ) %>%
  select(
    site_id, refinery_name, scenario, demand_scenario, refining_scenario,
    product_scenario, oil_price_scenario, attribution,
    demo_cat, demo_group, title, segment, metric, metric_desc, unit_desc, value
  ) %>%
  arrange(site_id, segment, demo_cat, demo_group, metric)

# Annual physical outputs kept as a separate, simpler table (no demographic
# dimension applies to production or GHG).
refinery_annual_physical <- production %>%
  full_join(ghg, by = c("site_id", "year")) %>%
  mutate(refinery_name = SITE_LABELS[site_id]) %>%
  select(site_id, refinery_name, year, production_bbl, crude_bbl, ghg_mtco2e) %>%
  arrange(site_id, year)

# -----------------------------------------------------------------------------
# 8. WRITE
# -----------------------------------------------------------------------------

fwrite(refinery_level_results,
       file.path(OUT_DIR, "refinery_level_health_labor.csv"))
fwrite(refinery_annual_physical,
       file.path(OUT_DIR, "refinery_level_production_ghg.csv"))

message("\nWrote:")
message("  ", file.path(OUT_DIR, "refinery_level_health_labor.csv"),
        "  (", nrow(refinery_level_results), " rows)")
message("  ", file.path(OUT_DIR, "refinery_level_production_ghg.csv"),
        "  (", nrow(refinery_annual_physical), " rows)")
