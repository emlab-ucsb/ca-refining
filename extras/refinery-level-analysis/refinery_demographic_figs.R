# =============================================================================
# Demographic breakdown of refinery-level health and labor impacts
# -----------------------------------------------------------------------------
# PURPOSE
#   Splits the cumulative (2020-2045) health and labor impacts attributed to
#   Phillips 66 Wilmington (343) and Valero Benicia (164) across the three
#   demographic cuts used in the paper: disadvantaged community (DAC) status,
#   poverty level, and race.
#
#   Produces two complementary views, following the paper's own treatment:
#     TOTALS      - how much of each impact falls on each group. Dominated by
#                   group size, so large groups appear largest almost by
#                   definition. This is the Supplementary Figure 8 framing.
#     PER CAPITA  - impacts per million people in each group, which removes the
#                   group-size effect and is the meaningful equity comparison.
#                   This is the Figure 5 framing.
#   Both are shown because either alone is misleading: totals overstate large
#   groups, per capita says nothing about how many people are affected.
#
# INPUTS
#   extras/refinery-level-analysis/refinery_level_health_labor.csv
#     (written by refinery_level_health_labor.R - run that first)
#   Group populations are rebuilt from the targets store, mirroring
#   process_npv_pc_data() at R/figures/health_labor_revised_figs.R:163-177.
#
# SCOPE NOTES
#   - Health uses the MARGINAL attribution (harm avoided if the refinery closes
#     while the rest of the sector runs). Standalone rows in the input are
#     filtered out here.
#   - Labor is DIRECT IMPACTS ONLY. Indirect and induced effects cannot be
#     assigned to demographic groups, because that would require knowing where
#     supply-chain and consumer-spending workers live (Methods 6.8.2). The
#     2021 snapshot reports those at refinery level, but they cannot be split
#     this way.
#   - Reference scenario (BAU demand, historic production); levels, not
#     differences against Reference.
# =============================================================================

suppressPackageStartupMessages({
  library(targets); library(data.table); library(dplyr); library(tidyr)
  library(stringr); library(ggplot2); library(scales)
})
options(scipen = 999)

# PERIOD selects what the figures show:
#   a single year (e.g. 2021) - a snapshot of one year's impacts
#   "cumulative"              - the 2020-2045 total
# 2021 is the default, matching the snapshot period used elsewhere in this
# folder. In the Reference scenario production is pinned to historical levels,
# so modelled 2021 is not COVID-depressed and represents normal-year conditions.
# Override from the command line, e.g.
#   Rscript refinery_demographic_figs.R cumulative
#   Rscript refinery_demographic_figs.R 2030
PERIOD <- 2021
.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) >= 1 && nzchar(.args[1])) {
  PERIOD <- if (identical(.args[1], "cumulative")) "cumulative" else as.integer(.args[1])
}

STORE   <- "_targets"
OUT_DIR <- "extras/refinery-level-analysis"
IN_CSV  <- file.path(OUT_DIR, "refinery_level_health_labor_annual.csv")

REF_LAB <- c("343" = "P66 Wilmington", "164" = "Valero Benicia")
REF_COL <- c("P66 Wilmington" = "#c1666b", "Valero Benicia" = "#4281a4")

# -----------------------------------------------------------------------------
# 1. Impacts
# -----------------------------------------------------------------------------
stopifnot(file.exists(IN_CSV))
res <- fread(IN_CSV)[, site_id := as.character(site_id)]

if (!identical(PERIOD, "cumulative")) {
  stopifnot(PERIOD %in% unique(res$year))
  res <- res[year == PERIOD]
  period_lab  <- as.character(PERIOD)
  period_axis <- paste0("Total in ", PERIOD)
} else {
  period_lab  <- "cumulative 2020-2045"
  period_axis <- "Cumulative total, 2020-2045"
}

impacts <- res[
  (segment == "health" & metric == "avoided_mortality" & health_attribution == "marginal") |
  (segment == "labor"  & metric == "fte_job_years"),
  .(value = sum(value)), by = .(site_id, demo_cat, demo_group, title, segment)
]
impacts[, refinery := REF_LAB[site_id]]

# -----------------------------------------------------------------------------
# 2. Group populations (2020)
# -----------------------------------------------------------------------------
# Mirrors process_npv_pc_data() (health_labor_revised_figs.R:163-177): tract
# population in 2020 multiplied by each group's population share in that tract,
# summed statewide. `refining_mortality` supplies the tract population the
# pipeline uses; it is the 30+ population from ct_inc_45, the same base the
# mortality calculation is built on, so numerator and denominator are consistent.
message("Building 2020 group populations...")

pop_ratios <- as.data.table(tar_read_raw("pop_ratios", store = STORE))
# Population is taken in the same year the impacts are measured, so numerator
# and denominator refer to the same population. For the cumulative view we use
# 2020, matching the pipeline's own convention in process_npv_pc_data().
pop_year <- if (identical(PERIOD, "cumulative")) 2020 else PERIOD
rm_2020 <- as.data.table(tar_read_raw("refining_mortality", store = STORE))
rm_2020 <- unique(rm_2020[year == pop_year, .(census_tract, pop)])

pop_2020 <- merge(rm_2020, pop_ratios, by = "census_tract", allow.cartesian = TRUE)
pop_2020[, demo_pop := pop * pct]
pop_2020 <- pop_2020[, .(pop_2020 = sum(demo_pop, na.rm = TRUE)), by = .(demo_cat, demo_group)]

# NOTE ON THE PER-CAPITA DENOMINATOR: this is the population aged 30 and over
# (~24.1 million), not California's total population (~39 million). The Krewski
# concentration-response function applies to adults over 29, so ct_inc_45 - and
# therefore refining_mortality$pop - carries only that age range. Using it keeps
# the numerator and denominator on the same base, and matches what the pipeline
# does in process_npv_pc_data(), so these per-capita values are comparable to the
# paper's Figure 5. Read them as "per million people aged 30+", not per million
# residents.

plot_dat <- merge(impacts, pop_2020, by = c("demo_cat", "demo_group"))
plot_dat[, per_million := value / (pop_2020 / 1e6)]

# Order groups within each cut by population, largest at the top of the bars.
plot_dat[, grp := factor(title, levels = unique(title[order(pop_2020)]))]

# Panel labels. The labor label states the DIRECT-ONLY scope explicitly, and the
# share of the full impact it represents, because that is the single easiest
# thing to misread here: the 2021 snapshot's headline labor figure is direct +
# indirect + induced, roughly 8x larger than what can be shown by demographic
# group. Comparing this panel against that total will look like a mismatch.
# Strip labels are kept short because they sit in a nested facet header
# (refinery + segment) and long text truncates. The full definitions - marginal
# health attribution, direct-only labor - are in the subtitle and caption.
plot_dat[, seg_lab := fifelse(segment == "health",
                              "Health (deaths)",
                              "Labor (direct FTE)")]
plot_dat[, demo_cat := factor(demo_cat, levels = c("DAC", "Poverty", "Race"))]

# -----------------------------------------------------------------------------
# 3. Figures
# -----------------------------------------------------------------------------
theme_ref <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9),
        plot.caption = element_text(colour = "grey40", size = 8, hjust = 0),
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "bottom")

# Colour carries the demographic CUT, so the three blocks of groups stay visually
# separate within each panel. Faceting is by refinery, which puts each refinery's
# full demographic profile in one column and makes the within-refinery comparison
# the primary read - rather than the across-refinery comparison the earlier
# grouped-bar version emphasised.
CAT_COL <- c("DAC" = "#2a6f97", "Poverty" = "#e09f3e", "Race" = "#7a4988")

# Order groups by cut, then by population within cut (largest at the top).
grp_order <- unique(plot_dat[order(demo_cat, pop_2020), .(demo_cat, title)])
plot_dat[, grp := factor(title, levels = grp_order$title)]

make_fig <- function(dat, yvar, title, subtitle, xlab, caption) {
  ggplot(dat, aes(grp, .data[[yvar]], fill = demo_cat)) +
    geom_col(width = 0.72) +
    coord_flip() +
    facet_grid(demo_cat ~ refinery + seg_lab, scales = "free", space = "free_y") +
    scale_fill_manual(values = CAT_COL, name = NULL) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = xlab, caption = caption) +
    theme_ref +
    theme(strip.text.x = element_text(size = 8.5))
}

cap <- paste(
  paste0("Reference scenario (BAU demand, historic production), ", period_lab, "."),
  "Health is the marginal attribution: harm avoided if the refinery closes while the rest of the sector operates.",
  paste0("Labor is DIRECT impacts only (~12% of the full labor effect). Indirect and induced jobs cannot be assigned to ",
         "demographic groups,\nso these bars are ~8x smaller than the direct+indirect+induced totals in the 2021 snapshot. ",
         "They are not directly comparable."),
  sep = "\n")

fig_tot <- make_fig(plot_dat, "value",
  paste0("Health and labor impacts by demographic group, ", period_lab),
  paste0("Totals. Larger groups show larger totals partly because they are larger; see the per-capita figure.",
         "\nNote: each panel has its own x-axis, so compare bars WITHIN a panel, not across refineries."),
  period_axis, cap)

fig_pc <- make_fig(plot_dat, "per_million",
  paste0("Health and labor impacts per million people, by demographic group, ", period_lab),
  paste0("Normalized by ", pop_year, " group population aged 30+, which removes the group-size effect and isolates disparity.",
         "\nNote: each panel has its own x-axis, so compare bars WITHIN a panel, not across refineries."),
  paste0("Per million people aged 30+ in group, ", tolower(period_axis)), cap)

ggsave(file.path(OUT_DIR, paste0("fig_demographics_totals_", gsub("[^0-9a-z]+","_",period_lab), ".png")),     fig_tot, width = 10, height = 8, dpi = 300)
ggsave(file.path(OUT_DIR, paste0("fig_demographics_per_capita_", gsub("[^0-9a-z]+","_",period_lab), ".png")), fig_pc,  width = 10, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# 4. Underlying data
# -----------------------------------------------------------------------------
plot_dat[, overall_pc := sum(value) / sum(pop_2020) * 1e6, by = .(refinery, segment, demo_cat)]
plot_dat[, disparity := per_million / overall_pc]

out <- plot_dat[, .(site_id, refinery, period = period_lab, segment, demo_cat,
                    demo_group, title, value, pop_2020, per_million,
                    overall_per_million = overall_pc, disparity_index = disparity)]
setorder(out, site_id, segment, demo_cat, -value)
fwrite(out, file.path(OUT_DIR, paste0("refinery_demographics_fig_inputs_", gsub("[^0-9a-z]+","_",period_lab), ".csv")))

message("\n--- validation ---")
cat("\ngroup populations (millions), should be internally consistent per cut:\n")
print(pop_2020[, .(demo_cat, demo_group, pop_millions = round(pop_2020 / 1e6, 3))][order(demo_cat, -pop_millions)])
cat("\ncut totals (should agree across DAC / Poverty / Race):\n")
print(pop_2020[, .(total_pop_millions = round(sum(pop_2020) / 1e6, 3)), by = demo_cat])
cat("\nimpact totals by cut (should also agree):\n")
print(dcast(plot_dat[, .(v = sum(value)), by = .(refinery, segment, demo_cat)],
            refinery + segment ~ demo_cat, value.var = "v")[
            , lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])

cat("\nwithin-refinery spread of the disparity index (max/min across groups):\n")
print(dcast(plot_dat[, .(spread = round(max(disparity) / min(disparity), 2)),
                     by = .(refinery, segment, demo_cat)],
            segment + demo_cat ~ refinery, value.var = "spread"))

message("\nWrote:")
message("  fig_demographics_{totals,per_capita,disparity}_", gsub("[^0-9a-z]+","_",period_lab), ".png")
message("  refinery_demographics_fig_inputs_", gsub("[^0-9a-z]+","_",period_lab), ".csv (", nrow(out), " rows)")
