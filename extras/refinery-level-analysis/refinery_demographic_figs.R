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

STORE   <- "_targets"
OUT_DIR <- "extras/refinery-level-analysis"
IN_CSV  <- file.path(OUT_DIR, "refinery_level_health_labor.csv")

REF_LAB <- c("343" = "P66 Wilmington", "164" = "Valero Benicia")
REF_COL <- c("P66 Wilmington" = "#c1666b", "Valero Benicia" = "#4281a4")

# -----------------------------------------------------------------------------
# 1. Impacts
# -----------------------------------------------------------------------------
stopifnot(file.exists(IN_CSV))
res <- fread(IN_CSV)[, site_id := as.character(site_id)]

impacts <- res[
  (segment == "health" & metric == "avoided_mortality" & health_attribution == "marginal") |
  (segment == "labor"  & metric == "fte_job_years"),
  .(site_id, demo_cat, demo_group, title, segment, value)
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
rm_2020 <- as.data.table(tar_read_raw("refining_mortality", store = STORE))
rm_2020 <- unique(rm_2020[year == 2020, .(census_tract, pop)])

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

plot_dat[, seg_lab := fifelse(segment == "health",
                              "Health: premature mortality (deaths)",
                              "Labor: direct employment (FTE job-years)")]
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

make_fig <- function(dat, yvar, title, subtitle, xlab, caption) {
  ggplot(dat, aes(grp, .data[[yvar]], fill = refinery)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    coord_flip() +
    facet_grid(demo_cat ~ seg_lab, scales = "free", space = "free_y") +
    scale_fill_manual(values = REF_COL, name = NULL) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = xlab, caption = caption) +
    theme_ref
}

cap <- paste(
  "Reference scenario (BAU demand, historic production), cumulative 2020-2045.",
  "Health is the marginal attribution: harm avoided if the refinery closes while the rest of the sector operates.",
  "Labor is direct impacts only - indirect and induced effects cannot be assigned to demographic groups.",
  sep = "\n")

fig_tot <- make_fig(plot_dat, "value",
  "Health and labor impacts by demographic group",
  "Totals. Larger groups show larger totals partly because they are larger; see the per-capita figure.",
  "Cumulative total, 2020-2045", cap)

fig_pc <- make_fig(plot_dat, "per_million",
  "Health and labor impacts per million people, by demographic group",
  "Normalized by 2020 group population aged 30+, which removes the group-size effect and isolates disparity.",
  "Per million people aged 30+ in group, cumulative 2020-2045", cap)

ggsave(file.path(OUT_DIR, "fig_demographics_totals.png"),     fig_tot, width = 10, height = 8, dpi = 300)
ggsave(file.path(OUT_DIR, "fig_demographics_per_capita.png"), fig_pc,  width = 10, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# 4. Underlying data
# -----------------------------------------------------------------------------
out <- plot_dat[, .(site_id, refinery, segment, demo_cat, demo_group, title,
                    value, pop_2020, per_million)]
setorder(out, site_id, segment, demo_cat, -value)
fwrite(out, file.path(OUT_DIR, "refinery_demographics_fig_inputs.csv"))

message("\n--- validation ---")
cat("\ngroup populations (millions), should be internally consistent per cut:\n")
print(pop_2020[, .(demo_cat, demo_group, pop_millions = round(pop_2020 / 1e6, 3))][order(demo_cat, -pop_millions)])
cat("\ncut totals (should agree across DAC / Poverty / Race):\n")
print(pop_2020[, .(total_pop_millions = round(sum(pop_2020) / 1e6, 3)), by = demo_cat])
cat("\nimpact totals by cut (should also agree):\n")
print(dcast(plot_dat[, .(v = sum(value)), by = .(refinery, segment, demo_cat)],
            refinery + segment ~ demo_cat, value.var = "v")[
            , lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])

message("\nWrote:")
message("  fig_demographics_totals.png / fig_demographics_per_capita.png")
message("  ", file.path(OUT_DIR, "refinery_demographics_fig_inputs.csv"), " (", nrow(out), " rows)")
