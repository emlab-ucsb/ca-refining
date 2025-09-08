# CUF Sensitivity Analysis - CORRECTED VERSION
# Properly using ALL join keys to prevent data aggregation

library(data.table)

# Define file paths
file_05 <- "outputs/rev-submission/cuf=0.5_beta-scenario=main/results/figures/figure-5/state_disaggregated_npv_pc_fig_inputs.csv"
file_06 <- "outputs/rev-submission/cuf=0.6_beta-scenario=main/results/figures/figure-5/state_disaggregated_npv_pc_fig_inputs.csv"
file_07 <- "outputs/rev-submission/cuf=0.7_beta-scenario=main/results/figures/figure-5/state_disaggregated_npv_pc_fig_inputs.csv"

cat("=== CORRECTED CUF SENSITIVITY ANALYSIS ===\n")
cat("Loading data with proper join key handling...\n")

# Load data
data_05 <- fread(file_05)
data_06 <- fread(file_06)
data_07 <- fread(file_07)

cat("Data dimensions:\n")
cat("  CUF 0.5:", nrow(data_05), "rows\n")
cat("  CUF 0.6:", nrow(data_06), "rows\n")
cat("  CUF 0.7:", nrow(data_07), "rows\n")

# Add CUF identifier
data_05[, cuf := 0.5]
data_06[, cuf := 0.6]
data_07[, cuf := 0.7]

# Combine all data
all_data <- rbindlist(list(data_05, data_06, data_07))

# Filter for key metrics
key_metrics <- c(
  "sum_cost_pv",
  "sum_cost_2019_pv",
  "forgone_wages_h",
  "forgone_wages_l"
)
analysis_data <- all_data[metric %in% key_metrics]

cat("After filtering for key metrics:", nrow(analysis_data), "rows\n")

# Manual spot check to verify we can see differences
cat("\n=== MANUAL SPOT CHECK ===\n")
test_filter <- analysis_data[
  demo_group == "aialnative" &
    scen_id == "BAU low exports" &
    metric == "sum_cost_pv" &
    segment == "health"
]
cat("Test case: aialnative, BAU low exports, sum_cost_pv, health\n")
for (cuf_val in c(0.5, 0.6, 0.7)) {
  test_val <- test_filter[cuf == cuf_val, value]
  cat("  CUF", cuf_val, ":", test_val, "\n")
}

# Create wide format using ALL join keys (as specified by user)
cat("\n=== CREATING WIDE FORMAT WITH ALL JOIN KEYS ===\n")
wide_data <- dcast(
  analysis_data,
  demo_group +
    demo_cat +
    scen_id +
    demand_scenario +
    refining_scenario +
    product_scenario +
    title +
    ghg_perc_diff +
    metric +
    segment +
    unit_desc +
    metric_desc +
    seg_title +
    scenario +
    demand_title +
    scen_title +
    demo_grp_metric ~
    cuf,
  value.var = "value"
)

# Rename columns for clarity
setnames(wide_data, c("0.5", "0.6", "0.7"), c("cuf_05", "cuf_06", "cuf_07"))

cat("Wide format created with", nrow(wide_data), "rows\n")

# Calculate differences
wide_data[, `:=`(
  diff_06_minus_05 = cuf_06 - cuf_05,
  diff_07_minus_06 = cuf_07 - cuf_06,

  pct_diff_06_minus_05 = ifelse(
    abs(cuf_05) > 1e-6,
    ((cuf_06 - cuf_05) / abs(cuf_05)) * 100,
    NA
  ),
  pct_diff_07_minus_06 = ifelse(
    abs(cuf_06) > 1e-6,
    ((cuf_07 - cuf_06) / abs(cuf_06)) * 100,
    NA
  )
)]

# Check if we now detect differences
nonzero_diffs <- wide_data[
  abs(diff_06_minus_05) > 1e-6 | abs(diff_07_minus_06) > 1e-6
]

cat("\n=== DIFFERENCE ANALYSIS ===\n")
if (nrow(nonzero_diffs) > 0) {
  cat("SUCCESS! Found", nrow(nonzero_diffs), "rows with non-zero differences\n")

  # Show examples
  cat("\n=== EXAMPLES OF DETECTED DIFFERENCES ===\n")
  sample_diffs <- head(nonzero_diffs, 5)
  print(sample_diffs[, .(
    demo_group,
    demo_cat,
    scen_id,
    segment,
    metric,
    cuf_05,
    cuf_06,
    cuf_07,
    diff_06_minus_05,
    diff_07_minus_06,
    pct_diff_06_minus_05,
    pct_diff_07_minus_06
  )])

  # Summary by sector (health vs labor)
  summary_by_sector <- nonzero_diffs[,
    .(
      n_observations = .N,
      min_abs_diff_06_05 = round(min(abs(diff_06_minus_05), na.rm = TRUE), 6),
      max_abs_diff_06_05 = round(max(abs(diff_06_minus_05), na.rm = TRUE), 6),
      min_abs_diff_07_06 = round(min(abs(diff_07_minus_06), na.rm = TRUE), 6),
      max_abs_diff_07_06 = round(max(abs(diff_07_minus_06), na.rm = TRUE), 6),
      mean_abs_diff_06_05 = round(mean(abs(diff_06_minus_05), na.rm = TRUE), 6),
      mean_abs_diff_07_06 = round(mean(abs(diff_07_minus_06), na.rm = TRUE), 6),
      min_pct_diff_06_05 = round(
        min(pct_diff_06_minus_05, na.rm = TRUE),
        4
      ),
      max_pct_diff_06_05 = round(
        max(pct_diff_06_minus_05, na.rm = TRUE),
        4
      ),
      min_pct_diff_07_06 = round(
        min(pct_diff_07_minus_06, na.rm = TRUE),
        4
      ),
      max_pct_diff_07_06 = round(
        max(pct_diff_07_minus_06, na.rm = TRUE),
        4
      )
    ),
    by = segment
  ]

  cat("\n", rep("=", 80), "\n")
  cat("SUMMARY BY SECTOR (HEALTH vs LABOR)\n")
  cat(rep("=", 80), "\n")
  print(summary_by_sector)

  # Summary by demographic category
  demo_summary <- nonzero_diffs[,
    .(
      n_observations = .N,
      min_abs_diff_06_05 = round(min(abs(diff_06_minus_05), na.rm = TRUE), 6),
      max_abs_diff_06_05 = round(max(abs(diff_06_minus_05), na.rm = TRUE), 6),
      min_abs_diff_07_06 = round(min(abs(diff_07_minus_06), na.rm = TRUE), 6),
      max_abs_diff_07_06 = round(max(abs(diff_07_minus_06), na.rm = TRUE), 6),
      mean_abs_diff_06_05 = round(mean(abs(diff_06_minus_05), na.rm = TRUE), 6),
      mean_abs_diff_07_06 = round(mean(abs(diff_07_minus_06), na.rm = TRUE), 6)
    ),
    by = .(demo_cat, segment)
  ][order(-max_abs_diff_06_05)]

  cat("\n", rep("=", 80), "\n")
  cat("SUMMARY BY DEMOGRAPHIC CATEGORY (Most sensitive first)\n")
  cat(rep("=", 80), "\n")
  print(demo_summary)

  # Find extreme cases
  cat("\n", rep("=", 80), "\n")
  cat("EXTREME CASES\n")
  cat(rep("=", 80), "\n")

  # Largest changes
  max_increase_06_05 <- nonzero_diffs[
    diff_06_minus_05 == max(diff_06_minus_05, na.rm = TRUE)
  ][1]
  max_decrease_06_05 <- nonzero_diffs[
    diff_06_minus_05 == min(diff_06_minus_05, na.rm = TRUE)
  ][1]
  max_increase_07_06 <- nonzero_diffs[
    diff_07_minus_06 == max(diff_07_minus_06, na.rm = TRUE)
  ][1]
  max_decrease_07_06 <- nonzero_diffs[
    diff_07_minus_06 == min(diff_07_minus_06, na.rm = TRUE)
  ][1]

  cat("Largest increase (CUF 0.5→0.6):\n")
  print(max_increase_06_05[, .(
    demo_group,
    demo_cat,
    scen_id,
    segment,
    metric,
    cuf_05,
    cuf_06,
    diff_06_minus_05,
    pct_diff_06_minus_05
  )])

  cat("\nLargest decrease (CUF 0.5→0.6):\n")
  print(max_decrease_06_05[, .(
    demo_group,
    demo_cat,
    scen_id,
    segment,
    metric,
    cuf_05,
    cuf_06,
    diff_06_minus_05,
    pct_diff_06_minus_05
  )])

  cat("\nLargest increase (CUF 0.6→0.7):\n")
  print(max_increase_07_06[, .(
    demo_group,
    demo_cat,
    scen_id,
    segment,
    metric,
    cuf_06,
    cuf_07,
    diff_07_minus_06,
    pct_diff_07_minus_06
  )])

  cat("\nLargest decrease (CUF 0.6→0.7):\n")
  print(max_decrease_07_06[, .(
    demo_group,
    demo_cat,
    scen_id,
    segment,
    metric,
    cuf_06,
    cuf_07,
    diff_07_minus_06,
    pct_diff_07_minus_06
  )])

  # Add flag column to identify zero vs non-zero difference rows
  wide_data[, has_differences := ifelse(
    abs(diff_06_minus_05) > 1e-6 | abs(diff_07_minus_06) > 1e-6,
    "non_zero",
    "zero"
  )]
  
  # Save comprehensive results (ALL rows including zero differences)
  fwrite(wide_data, "outputs/analysis/cuf_sensitivity.csv")
  cat("\nComplete results saved to: outputs/analysis/cuf_sensitivity.csv\n")
  cat("Total rows saved:", nrow(wide_data), "(including", nrow(wide_data) - nrow(nonzero_diffs), "zero-difference rows)\n")

  # Final summary
  cat("\n", rep("=", 100), "\n")
  cat("FINAL SUMMARY: CUF SENSITIVITY RANGES\n")
  cat(rep("=", 100), "\n")

  health_sum <- summary_by_sector[segment == "health"]
  labor_sum <- summary_by_sector[segment == "labor"]

  if (nrow(health_sum) > 0) {
    cat("HEALTH SECTOR:\n")
    cat(
      "  Absolute differences (CUF 0.5→0.6): MIN =",
      health_sum$min_abs_diff_06_05,
      ", MAX =",
      health_sum$max_abs_diff_06_05,
      "\n"
    )
    cat(
      "  Absolute differences (CUF 0.6→0.7): MIN =",
      health_sum$min_abs_diff_07_06,
      ", MAX =",
      health_sum$max_abs_diff_07_06,
      "\n"
    )
    cat(
      "  Percent differences (CUF 0.5→0.6): MIN =",
      health_sum$min_pct_diff_06_05,
      "%, MAX =",
      health_sum$max_pct_diff_06_05,
      "%\n"
    )
    cat(
      "  Percent differences (CUF 0.6→0.7): MIN =",
      health_sum$min_pct_diff_07_06,
      "%, MAX =",
      health_sum$max_pct_diff_07_06,
      "%\n\n"
    )
  }

  if (nrow(labor_sum) > 0) {
    cat("LABOR SECTOR:\n")
    cat(
      "  Absolute differences (CUF 0.5→0.6): MIN =",
      labor_sum$min_abs_diff_06_05,
      ", MAX =",
      labor_sum$max_abs_diff_06_05,
      "\n"
    )
    cat(
      "  Absolute differences (CUF 0.6→0.7): MIN =",
      labor_sum$min_abs_diff_07_06,
      ", MAX =",
      labor_sum$max_abs_diff_07_06,
      "\n"
    )
    cat(
      "  Percent differences (CUF 0.5→0.6): MIN =",
      labor_sum$min_pct_diff_06_05,
      "%, MAX =",
      labor_sum$max_pct_diff_06_05,
      "%\n"
    )
    cat(
      "  Percent differences (CUF 0.6→0.7): MIN =",
      labor_sum$min_pct_diff_07_06,
      "%, MAX =",
      labor_sum$max_pct_diff_07_06,
      "%\n"
    )
  }
} else {
  cat("ERROR: Still no differences detected. Check data loading logic.\n")
}

cat("\n=== ANALYSIS COMPLETED ===\n")
