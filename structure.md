# CA Refining Project Output Structure

This document outlines the file structure for the CA Refining project outputs.

```plaintext
outputs/
├── version/
│   ├── iteration1 (e.g., CUF=0.6)/
│   │   ├── intermediate/
│   │   │   ├── health/
│   │   │   │   ├── ct_xwalk_2019_2020.csv
│   │   │   │   ├── refining_health_census_tract_ref.csv
│   │   │   │   ├── refining_health_census_tract.csv
│   │   │   │   ├── refining_health_income_2023.csv*
│   │   │   │   └── refining_health_income_ref_2023.csv
│   │   │   └── labor/
│   │   │       └── state_labor_direct_impacts_demo_annual.csv*
│   │   ├── results/
│   │   │   └── figures/
│   │   │       ├── figure-1*/
│   │   │       │   ├── figures/
│   │   │       │   │   └── [files currently in academic-out/refining/figures/figures/scenario/fig1/]
│   │   │       │   └── tables/
│   │   │       ├── figure-2*/
│   │   │       ├── figure-3*/
│   │   │       │   ├── state_npv_fig_2020_ppx_bartik.pdf
│   │   │       │   ├── state_npv_fig_2020_ppx_bartik.png
│   │   │       │   ├── state_npv_fig_inputs_labor.csv
│   │   │       │   ├── fig3_high_legend.pdf
│   │   │       │   └── fig3_high_legend.png
│   │   │       ├── figure-4*/
│   │   │       │   ├── health_labor_gaps_pmil_plot_2020ppx_no_reemp.pdf
│   │   │       │   ├── health_labor_gaps_pmil_plot_2020ppx_no_reemp.png
│   │   │       │   ├── health_dac_legend.pdf
│   │   │       │   ├── health_poverty_legend.pdf
│   │   │       │   ├── health_race_legend.pdf
│   │   │       │   ├── labor_dac_legend.pdf
│   │   │       │   ├── labor_poverty_legend.pdf
│   │   │       │   ├── labor_race_legend.pdf
│   │   │       │   ├── state_labor_levels_fig_gaps_pmil_inputs.csv
│   │   │       │   └── state_levels_fig_gaps_pmil_inputs.csv
│   │   │       ├── figure-5*/
│   │   │       │   ├── demographic_npv_pc_fig_2020ppx.pdf
│   │   │       │   ├── demographic_npv_pc_fig_2020ppx.png
│   │   │       │   ├── state_disaggregated_npv_fig_inputs.csv
│   │   │       │   └── state_disaggregated_npv_pc_fig_inputs.csv
│   │   │       ├── figures-si/
│   │   │       │   ├── demographic_npv_shares_fig_2020ppx.pdf
│   │   │       │   ├── demographic_npv_shares_fig_2020ppx.png
│   │   │       │   ├── refinery_capacity.pdf
│   │   │       │   ├── refinery_capacity.png
│   │   │       │   ├── refinery_count.pdf
│   │   │       │   └── refinery_count.png
│   │   │       └── extra/
│   │   │           ├── pulse-figs/
│   │   │           │   ├── [files currently in academic-out/refining/figures/figures/scenario/pulse-figs]
│   │   │           │   ├── srm_pm25_ct.csv
│   │   │           │   └── srm_pm25_refinery_level.csv
│   │   │           ├── state-levels-fig/
│   │   │           │   ├── state_levels_fig_inputs.csv
│   │   │           │   ├── state_levels_fig.pdf
│   │   │           │   ├── state_levels_fig.png
│   │   │           │   ├── state_levels_pm25_fig.pdf
│   │   │           │   ├── state_levels_pm25_fig.png
│   │   │           │   ├── state_levels_pmil_fig.pdf
│   │   │           │   └── state_levels_pmil_fig.png
│   │   │           ├── extra-figure-3/
│   │   │           │   ├── state_npv_fig_2020_ppx_annual_vsl.pdf
│   │   │           │   ├── state_npv_fig_2020_ppx_annual_vsl.png
│   │   │           │   ├── state_npv_fig_2020_ppx_bartik_annual_vsl.pdf
│   │   │           │   ├── state_npv_fig_2020_ppx_bartik_annual_vsl.png
│   │   │           │   ├── state_npv_fig_2020_ppx_bartik_non_age_vsl.pdf
│   │   │           │   ├── state_npv_fig_2020_ppx_bartik_non_age_vsl.png
│   │   │           │   ├── state_npv_fig_2020_ppx_bartik_ref.pdf
│   │   │           │   ├── state_npv_fig_2020_ppx_bartik_ref.png
│   │   │           │   ├── state_npv_fig_2020_ppx_non_age_vsl.pdf
│   │   │           │   ├── state_npv_fig_2020_ppx_non_age_vsl.png
│   │   │           │   ├── state_npv_fig_2020_ppx_ref.pdf
│   │   │           │   ├── state_npv_fig_2020_ppx_ref.png
│   │   │           │   ├── state_npv_fig_2020_ppx.pdf
│   │   │           │   ├── state_npv_fig_2020_ppx.png
│   │   │           │   ├── state_npv_fig_annual_vsl.pdf
│   │   │           │   ├── state_npv_fig_annual_vsl.png
│   │   │           │   ├── state_npv_fig_ref.pdf
│   │   │           │   ├── state_npv_fig_ref.png
│   │   │           │   ├── state_npv_fig.pdf
│   │   │           │   └── state_npv_fig.png
│   │   │           ├── extra-figure-4/
│   │   │           │   ├── health_labor_gaps_pmil_plot_2020ppx.pdf
│   │   │           │   ├── health_labor_gaps_pmil_plot_2020ppx.png
│   │   │           │   ├── health_labor_gaps_pmil_plot.pdf
│   │   │           │   └── health_labor_gaps_pmil_plot.png
│   │   │           ├── extra-figure-5/
│   │   │           │   ├── demographic_npv_fig.pdf
│   │   │           │   ├── demographic_npv_fig.png
│   │   │           │   ├── demographic_npv_pc_fig.pdf
│   │   │           │   └── demographic_npv_pc_fig.png
│   │   │           ├── health-labor-figures/
│   │   │           │   ├── health_labor_gaps_plot_2020ppx_no_reemp.pdf
│   │   │           │   ├── health_labor_gaps_plot_2020ppx_no_reemp.png
│   │   │           │   ├── health_labor_gaps_plot_2020ppx.pdf
│   │   │           │   ├── health_labor_gaps_plot_2020ppx.png
│   │   │           │   ├── health_labor_gaps_plot.pdf
│   │   │           │   ├── health_labor_gaps_plot.png
│   │   │           │   ├── demographic_npv_fig_2020ppx.pdf
│   │   │           │   ├── demographic_npv_fig_2020ppx.png
│   │   │           │   ├── demographic_npv_shares_fig.pdf
│   │   │           │   └── demographic_npv_shares_fig.png
│   │   │           ├── health-figures/
│   │   │           │   ├── state_gaps_fig.pdf
│   │   │           │   ├── state_gaps_fig.png
│   │   │           │   ├── state_gaps_pm25_fig.pdf
│   │   │           │   ├── state_gaps_pm25_fig.png
│   │   │           │   ├── state_gaps_pmil_fig.pdf
│   │   │           │   └── state_gaps_pmil_fig.png
│   │   │           └── labor-figures/
│   │   │               ├── state_labor_gaps_fig_2020ppx.pdf
│   │   │               ├── state_labor_gaps_fig_2020ppx.png
│   │   │               ├── state_labor_gaps_fig.pdf
│   │   │               ├── state_labor_gaps_fig.png
│   │   │               ├── state_labor_gaps_pmil_fig_2020ppx.pdf
│   │   │               ├── state_labor_gaps_pmil_fig_2020ppx.png
│   │   │               ├── state_labor_levels_fig_2020ppx.pdf
│   │   │               ├── state_labor_levels_fig_2020ppx.png
│   │   │               ├── state_labor_levels_fig.pdf
│   │   │               ├── state_labor_levels_fig.png
│   │   │               ├── state_labor_levels_pmil_fig_2020ppx.pdf
│   │   │               ├── state_labor_levels_pmil_fig.png
│   │   │               ├── state_labor_levels_pmil_fig.pdf
│   │   │               ├── state_labor_levels_pmil_fig_2020ppx.png
│   │   │               ├── state_npv_labor_fig_2020ppx_bartik.pdf
│   │   │               ├── state_npv_labor_fig_2020ppx_bartik.png
│   │   │               ├── state_npv_labor_fig_2020ppx.pdf
│   │   │               ├── state_npv_labor_fig_2020ppx.png
│   │   │               ├── state_npv_labor_fig.pdf
│   │   │               ├── state_npv_labor_fig.png
│   │   │               └── state_disaggreated_npv_share_fig_inputs.csv
│   │   └── tables/
│   │       ├── other/
│   │       │   └── ct_missing_pop.csv
│   │       ├── health/
│   │       │   ├── refining_mortality_2023_constant_vsl.csv
│   │       │   ├── refining_mortality_2023_ref.csv
│   │       │   ├── refining_mortality_2023.csv
│   │       │   ├── refining_state_mortality.csv
│   │       │   ├── avg_pm25_county_2019.csv
│   │       │   ├── cumulative_avoided_mortality.csv*
│   │       │   ├── cumulative_health_x_county.csv*
│   │       │   ├── state_levels_fig_gaps_inputs.csv*
│   │       │   ├── state_levels_fig_gaps_pm25_inputs.csv
│   │       │   ├── state_levels_pm25_inputs.csv
│   │       │   ├── state_levels_pmil_fig_inputs.csv
│   │       │   ├── state_npv_fig_inputs_health_annual_vsl.csv
│   │       │   ├── state_npv_fig_inputs_health_non_age_vsl.csv
│   │       │   ├── state_npv_fig_inputs_health_ref.csv
│   │       │   └── state_npv_fig_inputs_health.csv
│   │       ├── labor/
│   │       │   ├── labor_county_outputs.csv*
│   │       │   ├── labor_high_low_annual_outputs.csv*
│   │       │   ├── state_labor_levels_fig_gaps_inputs.csv
│   │       │   ├── state_levels_labor_fig_inputs.csv
│   │       │   ├── state_levels_labor_pmil_fig_inputs.csv
│   │       │   └── state_npv_fig_inputs_labor_all_oilpx.csv
│   │       └── health-and-labor/
│   │           └── state_health_labor_ouputs.csv*
│   └── iteration2 (e.g., CUF=0.2)/
│       └── [same structure as iteration1]
```

## Structure Overview

1. **Version-Iteration Organization**
   - Top level: Version identifiers
   - Second level: Iteration names (e.g., "iteration1 (CUF=0.6)", "iteration2 (CUF=0.2)")

2. **Main Directory Categories**
   - `intermediate/`: Intermediate data files
     - `health/`: Health-related intermediate outputs
     - `labor/`: Labor-related intermediate outputs
   - `results/`: Final results
     - `figures/`: Organized by figure number and supplementary materials
   - `tables/`: Output tables organized by category
     - `other/`: Miscellaneous tables
     - `health/`: Health-related tables
     - `labor/`: Labor-related tables
     - `health-and-labor/`: Combined health and labor tables
