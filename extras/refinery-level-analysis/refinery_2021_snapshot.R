# =============================================================================
# 2021 snapshot: jobs, compensation and health for two refineries,
# with each refinery's share of the statewide refining total
# -----------------------------------------------------------------------------
# PURPOSE
#   A single-year picture of what the model attributes to two refineries that
#   have since closed, and how large those refineries were relative to the whole
#   California refining sector:
#       site_id 343 - Phillips 66, Wilmington Refinery (South cluster)
#       site_id 164 - Valero Energy, Benicia Refinery  (North cluster)
#
#   Reports, for each refinery separately and as a share of the 16-refinery total:
#     - employment      : direct / indirect / induced / total  (FTE job-years)
#     - compensation    : direct / indirect / induced / total  (2019 USD)
#     - health          : premature mortality (deaths) and monetized mortality
#
# WHY 2021
#   The model's prediction window is 2020-2045 (`pred_years`), so 2019 is not a
#   modelled year - it exists only as a historical crude-throughput anchor. In the
#   Reference scenario production is pinned to historical levels, so modelled 2021
#   is NOT a COVID-depressed year: statewide modelled crude is within 3% of 2019
#   actuals (595.3 vs 615.9 million bbl), and 2021 is within 0.1% of 2020.
#   2021 therefore represents normal-year conditions while giving full coverage of
#   health and labor, which 2019 cannot.
#
# SCENARIO
#   Reference = BAU demand + historic production, the case where refineries keep
#   operating at historical levels. Levels, not differences.
#
# IMPORTANT SCOPE NOTE ON INDIRECT AND INDUCED
#   The manuscript states indirect and induced impacts cannot be assigned to
#   census tracts, because that would require knowing the location of every
#   establishment in the supply chain (Methods 6.8.2). That is a limitation on
#   LOCATION, not on MAGNITUDE: equation 12 is linear in output value,
#       Impact_t = Multiplier x sum_r OutputValue_rt
#   so the share of indirect/induced activity attributable to one refinery is
#   simply its share of output value. Magnitudes below are therefore well defined.
#   What is NOT available is any demographic or geographic split of the indirect
#   and induced effects - only direct impacts can be broken down that way.
#
# STYLE / DEPENDENCIES
#   Mirrors the published pipeline (file:line references given per section).
#   Inputs come from the targets store, except the two confidential multiplier
#   files, whose copies in the store are blank no-conf-data stubs.
# =============================================================================

suppressPackageStartupMessages({
  library(targets); library(data.table); library(dplyr); library(tidyr)
  library(stringr); library(ggplot2); library(scales)
})
options(scipen = 999)

YEAR         <- 2021
REF_DEMAND   <- "BAU"
REF_REFINING <- "historic production"
OIL_PRICE_SCENARIO <- "reference case"
FOCUS <- c("343", "164")
FOCUS_LABEL <- c("343" = "P66 Wilmington", "164" = "Valero Benicia")

BETA          <- 0.00582        # Krewski et al. (2009), _targets.R:68-77
CPI2019       <- 107.8645906    # _targets.R:177
CPI2020       <- 109.1951913    # _targets.R:176
BARTIK        <- 0.741          # indirect_induced_mult, _targets.R:143

STORE   <- "_targets"
OUT_DIR <- "extras/refinery-level-analysis"

# -----------------------------------------------------------------------------
# 1. INPUTS
# -----------------------------------------------------------------------------
message("Loading inputs...")

srm               <- as.data.table(tar_read_raw("srm_weighted_pm25", store = STORE))
site_cons_ghg     <- as.data.table(tar_read_raw("refining_sites_cons_ghg_2019_2045", store = STORE))
dt_ef             <- as.data.table(tar_read_raw("dt_ef", store = STORE))
ct_xwalk          <- as.data.table(tar_read_raw("ct_xwalk", store = STORE))
ct_inc_45         <- as.data.table(tar_read_raw("ct_inc_45", store = STORE))
dt_age_vsl        <- as.data.table(tar_read_raw("dt_age_vsl", store = STORE))
indiv_prod_output <- as.data.table(tar_read_raw("indiv_prod_output", store = STORE))
product_px        <- as.data.table(tar_read_raw("product_px", store = STORE))
refin_locs_ct     <- as.data.table(sf::st_drop_geometry(tar_read_raw("refin_locs_ct", store = STORE)))

# Confidential multipliers (store copies are blank stubs - see _targets.R:714-721)
mult_direct <- fread("data/confidential-data/direct_multipliers_tract.csv",
                     colClasses = list(character = c("w_tract_geocode", "h_tract_geocode")))
mult_ii     <- fread("data/confidential-data/indirect_induced_multipliers_state.csv")
stopifnot(!all(is.na(mult_direct$emp.rev)), nrow(mult_ii) > 0)

for (d in list(srm, site_cons_ghg, indiv_prod_output, refin_locs_ct))
  d[, site_id := as.character(site_id)]

site_names <- unique(indiv_prod_output[, .(site_id, refinery_name)])

# =============================================================================
# 2. LABOR
# =============================================================================
# Mirrors calc_labor_outputs() (labor_functions_product_px.R:60-230) for the
# direct effect and calc_state_all_impacts() (same file, 535-575) for indirect
# and induced, keeping site_id throughout instead of collapsing to the state.
#
# REEMPLOYMENT BASIS: these are jobs and compensation SUPPORTED at a point in
# time, matching the pipeline's no-reemployment "_h" columns. Verified: summed
# across all 16 refineries this reproduces the pipeline's Reference 2021
# total_emp = 10,640.6 FTE and total_comp_usd19_h = $1,265,606,907 exactly.
#
# The "_l" / total_emp_revised variants are NOT the same quantity under a
# different assumption - they are year-over-year differences,
# total_emp - (1 - alpha) * prev_emp, and with flat Reference production they
# come to just 46.9 FTE statewide. The reemployment assumption concerns what
# happens to DISPLACED workers; no refinery declines in the Reference scenario,
# so nothing is displaced and the assumption is not engaged.
#
# Consequently these levels are not comparable to the paper's headline labor
# figures, which are reemployment-adjusted LOSSES measured against Reference.

message("Calculating labor...")

# --- 2a. Output value (revenue) per refinery ---------------------------------
# Fuels map to three priced products (labor_functions_product_px.R:63-71). Under
# the "2020 prices" scenario the pipeline holds 2020 prices flat across all years
# (same file, 96-113), which we replicate by joining the 2020 price to every year.
px_2020 <- product_px[year == 2020 & oil_price_scenario == OIL_PRICE_SCENARIO,
                      .(product, product_price)]

site_revenue <- indiv_prod_output %>%
  filter(demand_scenario == REF_DEMAND, refining_scenario == REF_REFINING,
         year == YEAR) %>%
  mutate(
    fuel = as.character(fuel),
    product = case_when(
      fuel %in% c("gasoline", "drop-in gasoline") ~ "gasoline",
      fuel %in% c("diesel", "renewable diesel")   ~ "diesel",
      TRUE                                        ~ "jet_fuel")
  ) %>%
  left_join(px_2020, by = "product") %>%
  mutate(revenue = value * product_price) %>%
  group_by(site_id) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
  as.data.table()

# --- 2b. Direct effect --------------------------------------------------------
# The tract-level direct multipliers are LODES commute shares within each work
# tract, and sum to the same statewide total for every work tract
# (0.2121287 jobs and $25,542.16 compensation per $1M of output value). The tract
# detail therefore only distributes the direct effect geographically; for a
# refinery total we can use the summed multiplier directly. Cross-checked against
# Methods 6.8: "$1 million in refining output value directly supports more than
# $25,000 in worker compensation".
direct_mult <- mult_direct[, .(emp_per_musd = sum(emp.rev), ec_per_musd = sum(ec.rev)),
                           by = w_tract_geocode]

site_work_tract <- unique(refin_locs_ct[, .(site_id, w_tract_geocode = substr(GEOID, 1, 11))])

labor <- site_revenue %>%
  left_join(site_work_tract, by = "site_id") %>%
  left_join(direct_mult, by = "w_tract_geocode") %>%
  mutate(
    emp_direct  = (revenue / 1e6) * emp_per_musd,
    comp_direct = (revenue / 1e6) * ec_per_musd
  ) %>%
  as.data.table()

# --- 2c. Indirect and induced -------------------------------------------------
# labor_functions_product_px.R:543-572. The pipeline SUMS the Indirect and
# Induced rows before applying them; we keep them separate, which is possible
# because the multiplier table stores them as distinct rows. The Bartik
# correction (x0.741) is applied to both, matching the paper's headline numbers.
ii <- mult_ii[, .(ImpactType, emp.rev, ec.rev)]
get_mult <- function(type, col) ii[ImpactType == type][[col]][1]

labor[, `:=`(
  emp_indirect  = (revenue / 1e6) * get_mult("Indirect", "emp.rev") * BARTIK,
  emp_induced   = (revenue / 1e6) * get_mult("Induced",  "emp.rev") * BARTIK,
  comp_indirect = (revenue / 1e6) * get_mult("Indirect", "ec.rev")  * BARTIK,
  comp_induced  = (revenue / 1e6) * get_mult("Induced",  "ec.rev")  * BARTIK
)]

# Convert compensation to 2019 USD (labor_functions_product_px.R:214).
labor <- copy(labor)
for (v in c("comp_direct", "comp_indirect", "comp_induced"))
  set(labor, j = v, value = labor[[v]] * CPI2019 / CPI2020)

labor[, `:=`(
  emp_total  = emp_direct + emp_indirect + emp_induced,
  comp_total = comp_direct + comp_indirect + comp_induced
)]

# =============================================================================
# 3. HEALTH
# =============================================================================
# Same construction as refinery_level_health_labor.R, restricted to one year but
# run for ALL 16 refineries so that each focus refinery's SHARE of the statewide
# total can be computed.

message("Calculating health for all 16 refineries...")

site_cluster <- unique(refin_locs_ct[, .(site_id, region)])

ef_wide <- dt_ef %>%
  mutate(ton_bbl = kg_bbl / 1000) %>% select(-kg_bbl) %>%
  pivot_wider(names_from = pollutant_code, values_from = ton_bbl)

# Emissions e_ipt = V_it * f_pc  (Methods eq. 4)
site_emissions <- site_cons_ghg %>%
  filter(demand_scenario == REF_DEMAND, refining_scenario == REF_REFINING,
         year == YEAR) %>%
  left_join(site_cluster, by = "site_id") %>%
  left_join(ef_wide, by = c("region" = "cluster")) %>%
  transmute(site_id,
            nh3 = bbls_consumed * NH3, nox = bbls_consumed * NOX,
            pm25 = bbls_consumed * `PM25-PRI`, sox = bbls_consumed * SO2,
            voc = bbls_consumed * VOC) %>%
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

# Concentrations c_jt = sum_p omega_j(i,p) * e_ipt  (Methods eq. 5)
site_pm <- merge(site_emissions, srm, by = "site_id", allow.cartesian = TRUE)
site_pm[, total_pm25 :=
  weighted_totalpm25_nh3 * nh3 + weighted_totalpm25_nox * nox +
  weighted_totalpm25_pm25 * pm25 + weighted_totalpm25_sox * sox +
  weighted_totalpm25_voc * voc]
site_pm[, GEOID := fifelse(GEOID == "06037137000", "06037930401", GEOID)]  # census_pm.R:454
site_pm <- site_pm[, .(total_pm25 = sum(total_pm25, na.rm = TRUE)), by = .(site_id, GEOID)]

# 2019 -> 2020 tract vintage crosswalk (census_pm.R:693)
setnames(site_pm, "GEOID", "GEOID_2019")
site_pm <- merge(site_pm, ct_xwalk[, .(GEOID_2020, GEOID_2019, rel_intersect)],
                 by = "GEOID_2019", all = TRUE, allow.cartesian = TRUE)
site_pm <- site_pm[, .(total_pm25 = weighted.mean(total_pm25, rel_intersect, na.rm = TRUE)),
                   by = .(site_id, GEOID_2020)]
site_pm <- site_pm[!is.na(GEOID_2020) & !is.na(site_id)]
setnames(site_pm, "GEOID_2020", "census_tract")

# Baseline incidence and age-based VSL (census_pm.R:978-1070), 2019 age-based VSL
# only - the variant reported in the main text.
age_vsl_sep <- dt_age_vsl %>%
  select(age_min, age_max, age_VSL_2019) %>% filter(age_min != age_max) %>%
  rowwise() %>%
  do(data.frame(age_min = seq(.$age_min, .$age_max),
                age_max = seq(.$age_min, .$age_max),
                age_VSL_2019 = .$age_VSL_2019)) %>% ungroup()

age_vsl_expanded <- dt_age_vsl %>%
  select(age_min, age_max, age_VSL_2019) %>% filter(age_min == age_max) %>%
  bind_rows(age_vsl_sep) %>% arrange(age_min)

vsl_cross_walk <- age_vsl_expanded %>%
  filter(age_min > 29) %>%
  fuzzyjoin::fuzzy_left_join(
    ct_inc_45 %>% select(start_age, end_age) %>% distinct() %>% filter(start_age > 29),
    by = c("age_min" = "start_age", "age_max" = "end_age"),
    match_fun = list(`>=`, `<=`)) %>%
  group_by(start_age) %>%
  summarise(end_age = first(end_age), age_VSL_2019 = mean(age_VSL_2019)) %>%
  drop_na(start_age) %>% ungroup()

ct_incidence <- ct_inc_45 %>%
  filter(year == YEAR, start_age > 29) %>%
  select(GEO_ID, start_age, end_age, year, pop, incidence_2015) %>%
  left_join(vsl_cross_walk, by = c("start_age", "end_age")) %>%
  group_by(GEO_ID) %>%
  # NOTE: the population total MUST NOT be named `pop` here. summarise() evaluates
  # expressions in order and later ones see the earlier results, so naming it `pop`
  # would shadow the original column and turn (pop / sum(pop)) into 1 for every age
  # group - silently converting the population-weighted mean into a raw sum across
  # age bands. The pipeline sidesteps this by computing shares in a mutate() first
  # (census_pm.R:1035-1049); here we simply use a non-colliding name.
  summarise(
    ct_pop = sum(pop, na.rm = TRUE),
    weighted_incidence = sum((pop / sum(pop, na.rm = TRUE)) * incidence_2015, na.rm = TRUE),
    weighted_monetized_age_incidence_2019 =
      sum((pop / sum(pop, na.rm = TRUE)) * incidence_2015 * age_VSL_2019, na.rm = TRUE),
    .groups = "drop") %>%
  rename(pop = ct_pop) %>%
  mutate(census_tract = str_remove(GEO_ID, "US")) %>%
  select(-GEO_ID) %>% as.data.table()

# Concentration-response as in the pipeline (census_pm.R:1121-1147):
#     mortality(c) = (exp(beta * c) - 1) * baseline incidence * population
#
# MARGINAL ATTRIBUTION (what we report):
#     mortality(c_tot) - mortality(c_tot - c_site)
# i.e. "if this refinery shuts down and the rest of the sector keeps running,
# how much harm goes away?" - which is the real-world event these two refineries
# went through. Because the CRF is convex its slope is steepest at high
# concentrations, so a refinery's emissions do more damage stacked on top of
# other pollution than they would in clean air.
#
# The standalone alternative, mortality(c_site), asks instead what the refinery
# would cause as the only source. It is retained below for comparison. The two
# differ by well under 1% here, but the definition must be stated.
health <- merge(site_pm, ct_incidence, by = "census_tract")
health[, c_tot := sum(total_pm25, na.rm = TRUE), by = census_tract]  # all 16 refineries

crf <- function(c) exp(BETA * c) - 1

health[, `:=`(
  mortality     = (crf(c_tot) - crf(c_tot - total_pm25)) * weighted_incidence * pop,
  mortality_usd = (crf(c_tot) - crf(c_tot - total_pm25)) * weighted_monetized_age_incidence_2019 * pop,
  mortality_standalone     = crf(total_pm25) * weighted_incidence * pop,
  mortality_usd_standalone = crf(total_pm25) * weighted_monetized_age_incidence_2019 * pop
)]

health_site <- health[, .(mortality = sum(mortality),
                          mortality_usd = sum(mortality_usd),
                          mortality_standalone = sum(mortality_standalone),
                          mortality_usd_standalone = sum(mortality_usd_standalone)),
                      by = site_id]

# =============================================================================
# 4. ASSEMBLE: levels and shares
# =============================================================================

metrics <- merge(
  labor[, .(site_id, revenue,
            emp_direct, emp_indirect, emp_induced, emp_total,
            comp_direct, comp_indirect, comp_induced, comp_total)],
  health_site, by = "site_id", all = TRUE)

# Statewide denominator = all 16 modelled refineries.
statewide <- metrics[, lapply(.SD, sum, na.rm = TRUE), .SDcols = setdiff(names(metrics), "site_id")]

shares <- copy(metrics)
for (v in setdiff(names(metrics), "site_id"))
  shares[[v]] <- metrics[[v]] / statewide[[v]]

# =============================================================================
# 5. VALIDATION
# =============================================================================
message("\n--- validation ---")

# Because the concentration-response function is exponential, per-site mortality
# does not sum exactly to the all-refinery total. Quantify it against the
# pipeline's own output rather than assuming it is negligible.
rm_all <- as.data.table(tar_read_raw("refining_mortality", store = STORE))
pipeline_2021 <- rm_all[scen_id == paste(REF_DEMAND, REF_REFINING) & year == YEAR,
                        sum(mortality_level, na.rm = TRUE)]
baseline <- unique(health[, .(census_tract, c_tot, weighted_incidence, pop)])[
  , sum(crf(c_tot) * weighted_incidence * pop)]
cat(sprintf("\nmortality %d, Reference:\n", YEAR))
cat(sprintf("  pipeline (all refineries at once) : %8.2f deaths\n", pipeline_2021))
cat(sprintf("  rebuilt here from c_tot           : %8.2f deaths  (%+.2f%% vs pipeline)\n",
            baseline, 100 * (baseline / pipeline_2021 - 1)))
cat(sprintf("  sum of 16 sites, MARGINAL         : %8.2f deaths  (%+.2f%% vs pipeline)\n",
            sum(health_site$mortality), 100 * (sum(health_site$mortality) / pipeline_2021 - 1)))
cat(sprintf("  sum of 16 sites, standalone       : %8.2f deaths  (%+.2f%% vs pipeline)\n",
            sum(health_site$mortality_standalone),
            100 * (sum(health_site$mortality_standalone) / pipeline_2021 - 1)))

cat("\nrevenue share of the two focus refineries:",
    sprintf("%.1f%%\n", 100 * sum(metrics[site_id %in% FOCUS, revenue]) / statewide$revenue))

# =============================================================================
# 6. TIDY OUTPUT
# =============================================================================

long <- metrics %>%
  filter(site_id %in% FOCUS) %>%
  pivot_longer(-site_id, names_to = "metric", values_to = "value") %>%
  left_join(
    shares %>% filter(site_id %in% FOCUS) %>%
      pivot_longer(-site_id, names_to = "metric", values_to = "share_of_statewide"),
    by = c("site_id", "metric")) %>%
  left_join(statewide %>% pivot_longer(everything(), names_to = "metric",
                                       values_to = "statewide_total"), by = "metric") %>%
  mutate(
    refinery = FOCUS_LABEL[site_id],
    segment = case_when(str_starts(metric, "emp_")  ~ "labor: employment",
                        str_starts(metric, "comp_") ~ "labor: compensation",
                        str_starts(metric, "mortality") ~ "health",
                        TRUE ~ "output"),
    effect = case_when(str_detect(metric, "_direct")   ~ "direct",
                       str_detect(metric, "_indirect") ~ "indirect",
                       str_detect(metric, "_induced")  ~ "induced",
                       str_detect(metric, "_total")    ~ "total",
                       TRUE ~ NA_character_),
    unit = case_when(str_starts(metric, "emp_") ~ "FTE job-years (supported; no-reemployment basis)",
                     str_starts(metric, "comp_") ~ "USD 2019 (supported; no-reemployment basis)",
                     str_starts(metric, "mortality_usd") ~ "USD 2019 (age-based VSL)",
                     str_starts(metric, "mortality") ~ "deaths",
                     metric == "revenue" ~ "USD 2020"),
    # Health uses the marginal attribution; standalone rows are carried for
    # comparison. Labor is linear in revenue, so the distinction does not apply.
    health_attribution = case_when(
      !str_starts(metric, "mortality")     ~ NA_character_,
      str_detect(metric, "_standalone$")   ~ "standalone",
      TRUE                                 ~ "marginal"),
    metric = str_remove(metric, "_standalone$"),
    year = YEAR, scenario = "Reference") %>%
  select(year, scenario, site_id, refinery, segment, effect, metric, unit,
         health_attribution, value, statewide_total, share_of_statewide) %>%
  arrange(site_id, segment, health_attribution, effect)

fwrite(long, file.path(OUT_DIR, "refinery_2021_snapshot.csv"))

# =============================================================================
# 7. FIGURES
# =============================================================================

theme_ref <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

eff_cols <- c(direct = "#1b4965", indirect = "#5fa8d3", induced = "#bee9e8")

# Figure 1 - employment and compensation by effect type
fig1_dat <- long %>%
  filter(effect != "total", segment %in% c("labor: employment", "labor: compensation")) %>%
  mutate(effect = factor(effect, levels = c("induced", "indirect", "direct")),
         plot_val = if_else(segment == "labor: compensation", value / 1e6, value),
         panel = if_else(segment == "labor: compensation",
                         "Compensation (2019 USD, millions)", "Employment (FTE job-years)"))

fig1 <- ggplot(fig1_dat, aes(refinery, plot_val, fill = effect)) +
  geom_col(width = 0.6) +
  facet_wrap(~panel, scales = "free_y") +
  scale_fill_manual(values = eff_cols, name = NULL) +
  scale_y_continuous(labels = comma) +
  labs(title = paste0("Employment and compensation supported, ", YEAR),
       subtitle = "Reference scenario (BAU demand, historic production). Indirect and induced include the Bartik correction (x0.741).",
       x = NULL, y = NULL) +
  theme_ref

ggsave(file.path(OUT_DIR, "fig_2021_labor.png"), fig1, width = 9, height = 5, dpi = 300)

# Figure 2 - share of statewide refining total
# Labor is deliberately shown as a single row per measure rather than split by
# effect type. Direct, indirect and induced are all linear in the same refinery
# revenue and use statewide multipliers, so each refinery's SHARE is identical
# across the three - plotting them separately would imply a distinction in the
# shares that does not exist. The effect split is informative for magnitudes
# (Figure 1) but not for shares.
fig2_dat <- long %>%
  filter(effect == "total" |
         (metric %in% c("mortality", "mortality_usd") & health_attribution == "marginal")) %>%
  mutate(lab = case_when(
    segment == "labor: employment"   ~ "Employment (FTE job-years)",
    segment == "labor: compensation" ~ "Compensation (2019 USD)",
    metric  == "mortality"           ~ "Premature mortality (deaths)",
    metric  == "mortality_usd"       ~ "Monetized mortality (2019 USD)"),
    lab = factor(lab, levels = c("Monetized mortality (2019 USD)",
                                 "Premature mortality (deaths)",
                                 "Compensation (2019 USD)",
                                 "Employment (FTE job-years)")))

fig2 <- ggplot(fig2_dat, aes(lab, share_of_statewide, fill = refinery)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = percent(share_of_statewide, accuracy = 0.1)),
            position = position_dodge(width = 0.7), hjust = -0.15, size = 3.1) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.12, 0.02),
                     limits = c(0, 0.115), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c("P66 Wilmington" = "#c1666b", "Valero Benicia" = "#4281a4"),
                    name = NULL) +
  labs(title = paste0("Share of statewide refining total, ", YEAR),
       subtitle = paste("Each refinery as a share of all 16 modelled California refineries.",
                        "\nLabor shares are identical across direct, indirect and induced effects by construction."),
       x = NULL, y = "Share of statewide total") +
  theme_ref

ggsave(file.path(OUT_DIR, "fig_2021_shares.png"), fig2, width = 9, height = 4.5, dpi = 300)

# Figure 3 - health levels
fig3_dat <- long %>%
  filter(metric %in% c("mortality", "mortality_usd"), health_attribution == "marginal") %>%
  mutate(plot_val = if_else(metric == "mortality_usd", value / 1e6, value),
         panel = if_else(metric == "mortality_usd",
                         "Monetized mortality (2019 USD, millions)", "Premature mortality (deaths)"))

fig3 <- ggplot(fig3_dat, aes(refinery, plot_val, fill = refinery)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = comma(plot_val, accuracy = 0.1)), vjust = -0.4, size = 3.2) +
  facet_wrap(~panel, scales = "free_y") +
  scale_fill_manual(values = c("P66 Wilmington" = "#c1666b", "Valero Benicia" = "#4281a4")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = paste0("Health impacts attributable to each refinery, ", YEAR),
       subtitle = paste("Premature mortality from PM2.5 and its monetized value (2019 age-based VSL).",
                        "\nMarginal attribution: harm avoided if the refinery closes while the rest of the sector operates."),
       x = NULL, y = NULL) +
  theme_ref

ggsave(file.path(OUT_DIR, "fig_2021_health.png"), fig3, width = 8, height = 4.5, dpi = 300)

message("\nWrote:")
message("  ", file.path(OUT_DIR, "refinery_2021_snapshot.csv"), " (", nrow(long), " rows)")
message("  fig_2021_labor.png / fig_2021_shares.png / fig_2021_health.png")
