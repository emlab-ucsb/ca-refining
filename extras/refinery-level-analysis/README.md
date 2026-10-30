# Refinery-level analysis: Phillips 66 Wilmington and Valero Benicia

This folder supports an analysis adjacent to the paper. It breaks the paper's
sector-wide health and labor results down to two individual refineries that have
since closed. Their retirements were announced after the paper's analysis was
completed, so the paper does not model them; in the paper's scenarios both keep
operating (SI Table 2, notes).

| site_id | Refinery | Cluster | Capacity (bbl/day) |
|---|---|---|---|
| 343 | Phillips 66, Wilmington Refinery | South | 139,000 |
| 164 | Valero Energy, Benicia Refinery | North | 145,000 |

*Cluster and capacity from SI Table 1.*

**In brief.** In 2021, each refinery accounted for a similar share of California's
refining output value and refinery-supported jobs:

| | P66 Wilmington | Valero Benicia |
|---|---|---|
| Share of state output value and jobs | 7.7% | 7.2% |
| Jobs, including supply-chain and spending effects | about 6,700 | about 6,300 |
| Premature deaths from refinery PM2.5 | 27.6 (9.6% of state) | 6.6 (2.3% of state) |

Their health impacts are very different. The two process similar volumes of
crude, and Benicia's per-barrel emission factors are, if anything, higher. The
gap therefore comes from location: how each refinery's emissions disperse over
the surrounding population.

**Sources.** Everything below is drawn from the code in this repo and from the
paper draft, *Health, labor, and equity in the low-carbon transition of
California's petroleum refineries* (Environmental Research Letters), and its
Supplementary Information. "Section X" refers to the main text; "SI Note X",
"SI Table X" and "SI Fig. X" refer to the SI.

---

## Contents

- [Shared files at a glance](#shared-files-at-a-glance)
- [Key concepts](#key-concepts) (each says which files it applies to)
- [Column definitions](#column-definitions)
- [Caveats](#caveats)
- [Other files in this folder](#other-files-in-this-folder)
- [Reproducing the outputs](#reproducing-the-outputs)
- [Questions and answers](#questions-and-answers)

---

## Shared files at a glance

Four files are shared. They are referred to below by short names.

| Short name | File | Period | Contents |
|---|---|---|---|
| **snapshot** | `refinery_2021_snapshot.csv` | 2021 | Refinery totals: jobs and compensation (direct, indirect, induced, total), output value, and health. One row per refinery × metric. |
| **annual** | `refinery_level_health_labor_annual.csv` | Each year 2020–2045. **Filter to `year == 2021`** (216 rows) | Health and **direct** labor, split by demographic group. One row per refinery × year × demographic group × metric. |
| **demographics** | `refinery_demographics_fig_inputs_2021.csv` | 2021 | Marginal deaths and direct FTE jobs by demographic group, with **per-capita rates and a disparity index**. Data behind the demographic figures. |
| **production_ghg** | `refinery_level_production_ghg.csv` | Each year 2020–2045 | Refined product output, crude throughput and GHG emissions. One row per refinery × year. |

**How the 2021 files fit together.** They are consistent with each other:

- **annual vs. snapshot.** Summed within one demographic cut, annual's 2021 rows
  exactly reproduce the snapshot's `mortality` (marginal), `emp_direct` and
  `comp_direct` for each refinery.
- **demographics vs. annual.** The `value` and `statewide_value` in demographics
  are identical to annual's 2021 marginal deaths and direct FTE jobs (and their
  `statewide_total`).

| You need… | Use |
|---|---|
| Refinery totals, including indirect and induced labor, or output value | snapshot |
| Health (deaths or dollars, marginal or standalone) or direct labor (jobs or compensation) by demographic group | annual, filtered to 2021 |
| Per-capita rates or disparity by group | demographics |
| Production, crude or GHG | production_ghg |

Which concepts apply to which file:

| Concept | snapshot | annual | demographics | production_ghg |
|---|---|---|---|---|
| [Levels in the Reference scenario](#levels-in-the-reference-scenario-not-differences) | ✓ | ✓ | ✓ | ✓ |
| [Statewide totals and shares](#statewide-totals-and-shares) | ✓ | ✓ (by group) | ✓ (by group) | ✓ |
| [Two statewide totals for health](#two-statewide-totals-for-health) | ✓ | ✓ | marginal sum only | — |
| [Marginal vs. standalone health](#health-attribution-marginal-vs-standalone) | both | both | **marginal only** | — |
| [Direct, indirect and induced labor](#direct-indirect-and-induced-labor-effects) | all three | **direct only** | **direct only** | — |
| ["Supported; no-reemployment basis"](#supported-no-reemployment-basis) | ✓ | ✓ | ✓ | — |
| [Undiscounted vs. present value](#undiscounted-vs-present-value) | undiscounted | both (**use undiscounted for 2021**) | no dollars | — |
| [Demographic groups](#demographic-groups) | — | ✓ | ✓ | — |
| [Per capita and disparity](#per-capita-rates-and-disparity-index) | — | — | ✓ | — |
| [Shares change over time](#shares-change-over-time) | — | ✓ (years ≠ 2021) | — | ✓ |
| [Cluster-level allocation](#refinery-level-quantities-are-allocations-of-cluster-level-totals) | ✓ | ✓ | ✓ | ✓ |

---

## Key concepts

### Levels in the Reference scenario, not differences

*Applies to: all four files.*

The paper reports health and labor outcomes in each low-carbon scenario *relative
to* the Reference scenario (BAU demand, historical production). Here, the
Reference scenario itself is the object of interest. Every number answers the
question: *in the Reference scenario, what does the model attribute to this
refinery?*

As a result, none of these numbers is a "benefit" or a "loss". They are the
production, emissions, deaths, jobs and compensation associated with each
refinery operating as projected.

### Statewide totals and shares

*Applies to: all four files.*

Every row carries the matching total for **all 16 modelled California
refineries**, plus the refinery's share of that total. The rule is the same in
every file:

```
share = refinery value / statewide total, computed on the same basis
```

Here, "the same basis" means:
- the same year;
- the same metric and unit;
- where relevant, the same demographic group and the same health attribution
  basis.

Shares across all 16 refineries therefore add up to exactly 100%, so they are
additive and comparable between refineries.

How the columns are named in each file:

| File | Statewide total | Share |
|---|---|---|
| snapshot, annual | `statewide_total` (plus `statewide_total_pipeline` for health; see next section) | `share_of_statewide` |
| demographics | `statewide_value` | `share_of_statewide` |
| production_ghg | `statewide_production_bge`, `statewide_crude_bbl`, `statewide_ghg_mtco2e` | `share_of_statewide_production_bge`, `share_of_statewide_crude_bbl`, `share_of_statewide_ghg_mtco2e` |

**In annual and demographics, the statewide total is at the same demographic
grain as the row.** For example, the DAC row is divided by the statewide total
*for DAC communities*, not by the statewide total across all groups. So
`share_of_statewide` on a DAC row means "this refinery's share of all refinery
impacts falling on DAC communities." It is not a population share.

### Two statewide totals for health

*Applies to: health rows in snapshot and annual. demographics carries only the
first.*

For labor, revenue, production, crude and GHG, the sector total is simply the sum
of the 16 refineries, because those calculations are linear. For health it is
not, because mortality is a convex function of PM2.5 concentration (see the next
section). Health rows therefore carry two totals:

| Column | Definition |
|---|---|
| `statewide_total` (demographics: `statewide_value`) | The sum of all 16 refineries' values, each calculated **on the same basis as the row**. Marginal rows get the sum of 16 marginal values, and standalone rows get the sum of 16 standalone values. **This is the denominator for `share_of_statewide`.** |
| `statewide_total_pipeline` (snapshot and annual only) | The sector total calculated **jointly**, with all refineries' pollution combined in a single mortality calculation. This is the harm actually occurring with every refinery running, and it is the number the published pipeline reports. Marginal and standalone rows carry the same value. |

On labor rows the two columns are identical.

Statewide deaths on each basis:

| Basis | 2021 | 2020–2045 (annual, summed over years) | vs. joint |
|---|---|---|---|
| Sum of 16 marginal contributions | 287.77 | 10,354.8 | +0.14% |
| Joint, all refineries at once (`statewide_total_pipeline`) | 287.36 | 10,340.4 | — |
| Sum of 16 standalone contributions | 286.95 | 10,325.9 | −0.14% |

*Values are summed within one demographic cut (for example DAC + Non-DAC). The
joint 2020–2045 total, 10,340 deaths, matches the Reference-scenario figure in
the paper (Section 3.2).*

If you divided marginal values by `statewide_total_pipeline` instead, shares
across the 16 refineries would add up to about 100.14% rather than 100%.

### Health attribution: marginal vs. standalone

*Applies to: health rows in snapshot and annual (`health_attribution` column;
blank on all other rows). demographics contains marginal only.*

Mortality from PM2.5 is computed with an exponential concentration-response
function (SI Eq. 9; `R/health/census_pm.R`):

```
mortality(c) = population × baseline mortality rate × (exp(β × c) − 1),   β = 0.0058
```

Because this curve is convex, a given amount of pollution causes more deaths
where the air is already dirtier. As a result, "deaths caused by one refinery"
depends on what you assume about the other refineries.

- **Marginal (the reported basis):** deaths with all refineries running, minus
  deaths with every refinery running except this one:
  `mortality(c_total) − mortality(c_total − c_refinery)`. It answers the
  question: *if this refinery closes and the rest of the sector keeps operating,
  how much harm goes away?* That matches what happened to these two refineries.
- **Standalone (kept for comparison):** deaths this refinery would cause if it
  were the only source: `mortality(c_refinery)`. Its emissions are added to
  otherwise clean air, so this number is slightly smaller.

The two differ by well under 1%. In 2021:

| | Marginal | Standalone |
|---|---|---|
| P66 Wilmington | 27.56 | 27.43 |
| Valero Benicia | 6.59 | 6.58 |

**Use rows with `health_attribution == "marginal"` for headline numbers.**

**Relation to the paper's convention.** The paper computes a scenario's health
change by applying the function to the *change* in concentration,
`exp(β × Δc) − 1` (SI Eq. 9; `mortality_delta` in `R/health/census_pm.R`).
Applied to a single refinery closing, that would give a value slightly below the
standalone basis. It is not computed in these outputs.

### Direct, indirect and induced labor effects

*Applies to: snapshot (all three effects plus total); annual and demographics
(**direct only**).*

The paper estimates employment and compensation by applying fixed IMPLAN
input-output multipliers for California's refining sector to refinery output
value (Section 2.6). The multipliers measure labor-market effects in three places:

- **Direct:** the refined petroleum products sector itself.
- **Indirect:** sectors that provide inputs to refinery production.
- **Induced:** sectors where these workers spend their income.

**Why annual and demographics have direct effects only.** Direct effects are
assigned to the census tracts where workers *live*, which is what allows them to
be split by demographic group (SI Note 3.3, Eq. 12). Each refinery's output value
is multiplied by IMPLAN direct multipliers for industry 154, Petroleum
refineries, and distributed by commuting shares from the 2020 Census LEHD Origin
Destination Employment Statistics (LODES). Indirect and induced effects cannot be
located. Placing them would require knowing the census tract of every
establishment in California, so they are calculated at the state level only (SI
Notes 3.2 and 3.4).

**Size of the gap.** In 2021, direct effects are about 12% of total employment
and about 20% of total compensation. The paper reports a similar split for its
losses: about 11% of job-years and 20% of NPV compensation (Section 3.2). Labor
numbers in annual and demographics are therefore much smaller than the
snapshot's `total` rows. **Compare them with the snapshot's `direct` rows only.**

**Indirect and induced scaling.** Both are scaled by 0.741, because "realized
indirect and induced impacts are about one-quarter lower than would be predicted
by input-output models such as IMPLAN" (Bartik & Sotherland, 2019; SI Note 3.4).
The values here include that scaling, as the paper's do.

**Why every labor share in the snapshot is the same.** Indirect and induced
effects are a multiplier times output value (SI Eq. 13). A refinery's share of
the statewide total is therefore identical across direct, indirect, induced,
total and revenue. This is true by construction, not a coincidence. In annual and
demographics, direct-labor shares *do* vary across demographic groups, because
each refinery's workers live in different places.

**Units (SI Note 3.1).** IMPLAN defines a full-time job as 2,080 hours in a year.
Employee compensation is the total payroll cost of a worker, including wages,
salaries, benefits and payroll taxes.

### "Supported; no-reemployment basis"

*Applies to: labor rows in snapshot, annual and demographics.*

- **Supported:** labor figures are levels. They count the jobs and compensation
  that the refinery's output sustains in the Reference scenario. They are not job
  losses. "FTE job-years" means one full-time job for one year, so for a single
  year (2021) the value is simply the number of FTE jobs.
- **No-reemployment basis:** the paper models two assumptions about workers
  displaced when production falls (Section 2.6; SI Note 3.5):
  - **with reemployment (main estimate):** displaced workers are unemployed for
    one year, then reemployed at 20% lower compensation;
  - **without reemployment (upper bound):** no reemployment through 2045.

  Both come from SI Eq. 15:

  ```
  Impact_t = X_t − (1 − β)·X_{t−1} − (1 − α)·LI_t
  ```

  where `X_t` is the employment or compensation impact in year t and `LI_t` is the
  labor income regained through reemployment.
  - **Without reemployment** (α = 1, β = 1), this reduces to `Impact_t = X_t`. The
    figure *is* the level in that year.
  - **With reemployment** (α = 0.2, β = 0), it becomes
    `X_t − X_{t−1} − 0.8·LI_t`. That is a year-over-year change, which has no
    meaning as a level.

  The figures here are levels, so they are on the no-reemployment basis (the
  pipeline's `_h` columns). The scripts check that summing all 16 refineries
  reproduces the pipeline's own state-level `_h` totals.

The label exists to prevent one comparison: **these figures are not comparable to
the paper's headline labor numbers.** Those are losses relative to Reference,
with the main estimate assuming reemployment. That is a different quantity.

### Undiscounted vs. present value

*Applies to: dollar values in snapshot and annual. demographics has no dollar
values.*

| File | Dollar metrics | Discounting |
|---|---|---|
| snapshot | `comp_*`, `mortality_usd` (2019 USD); `revenue` (2020 USD) | Undiscounted |
| annual | `health_cost_2019`, `compensation` (2019 USD) | Undiscounted |
| annual | `health_cost_pv`, `compensation_pv` (2019 USD) | Discounted at 3% per year back to 2019, as in the paper (Section 2.5; SI Note 3.4) |

**For 2021, use the undiscounted metrics.** A present value is meaningful when
summing a stream of future years. For a single year, the `_pv` columns are just
the 2021 value divided by 1.03². They also won't match the snapshot, which is
undiscounted. The undiscounted annual values match the snapshot exactly. Deaths
and job-years are never discounted.

### Demographic groups

*Applies to: annual and demographics.*

Each tract's outcome is divided among groups by that group's share of the tract
population (SI Note 4).

| `demo_cat` | Groups | Source (SI Note 4) |
|---|---|---|
| `DAC` | DAC, Non-DAC | Disadvantaged-community designation under CalEnviroScreen 4.0 |
| `Poverty` | Above / below poverty line | Share of households above or below the poverty line, 2017–2021 5-year ACS (NHGIS) |
| `Race` | Black, Hispanic, Asian, white, American Indian or Alaska Native, Hawaiian or Other Pacific Islander, Other (non-Hispanic), Two or more races (non-Hispanic) | Population share by race (NHGIS) |

Each `demo_cat` is a complete partition of the population. **To get a
refinery's total, sum within one `demo_cat`; never sum across them**, or the
total will be counted three times. For Race, include all 8 groups. In
demographics, that includes the rows with `in_figure = FALSE`.

### Per-capita rates and disparity index

*Applies to: demographics.*

- **Per million people aged 30+, not per million residents.** `per_million` is
  `value` divided by the group's population aged 30+, in millions. This is the
  denominator the paper's code uses for its per-capita results: the 30+
  population from the health calculation (`process_npv_pc_data()` in
  `R/figures/health_labor_revised_figs.R`). The paper's health effects are
  restricted to adults over 29 (Section 2.5; SI Note 2). SI Note 4 (Eq. 17)
  currently describes the denominator as each group's 2020 Census population;
  that text is being updated to match.
- **The population column is 2021, despite its name.** It is called `pop_2020`,
  but in this 2021 file it holds 2021 population.
- **Disparity index.** `overall_per_million` is the refinery's per-million rate
  across all groups in the cut. `disparity_index` = `per_million` /
  `overall_per_million`. A value above 1 means the group bears more than its
  population share of this refinery's impact.
- **Statewide comparison.** The `statewide_*` columns give the same quantities
  for the 16-refinery sector, so each refinery's pattern can be compared with
  the sector-wide pattern.
- **Race groups in figures.** The figures show the four race groups the paper
  reports (Black, Hispanic, Asian, white). The other four groups are each under
  0.9% of the population. They are kept in the CSV with `in_figure = FALSE`.

### Shares change over time

*Applies to: production_ghg, and annual if you look at years other than 2021.*

The Reference scenario is not entirely static. Statewide modelled crude
throughput falls from 613 to 565 million barrels between 2020 and 2045 (−8%),
most of it by 2025. The sources show where this comes from:

- The paper attributes it to refinery retirements announced at the time of the
  study (Section 3.1).
- Those announced changes are in the North cluster (SI Table 2): the Golden
  Eagle Martinez conversion (2022), the Santa Maria retirement (2023) and the
  Rodeo conversion (2024).
- In the "BAU Demand, Historical Production" panel of SI Fig. 3, North cluster
  capacity falls through 2024 while South cluster capacity stays flat.
- The paper reports 7,100 job-years of cumulative employment losses in the
  Reference scenario (Section 3.2).

The two focus refineries stay close to flat: 45.7–45.8 million bbl/yr for
Wilmington and 46.3–49.5 million bbl/yr for Benicia. Because the rest of the
sector shrinks around them, most of their shares grow over time:

| Share of statewide | Wilmington 2021 | Wilmington 2045 | Benicia 2021 | Benicia 2045 |
|---|---|---|---|---|
| Direct jobs (annual) | 7.7% | 8.3% | 7.2% | 7.9% |
| Deaths, marginal (annual) | 9.6% | 9.8% | 2.3% | 2.2% |
| GHG (production_ghg) | 7.5% | 8.3% | 7.6% | 8.4% |

---

## Column definitions

### Columns shared across the files

| Column | snapshot | annual | demographics | production_ghg | Meaning |
|---|---|---|---|---|---|
| `site_id` | ✓ | ✓ | ✓ | ✓ | `343` = P66 Wilmington, `164` = Valero Benicia |
| `refinery` / `refinery_name` | `refinery` | `refinery_name` | `refinery` | `refinery_name` | Refinery label |
| `year` / `period` | `year` = 2021 | `year` (filter to 2021) | `period` = 2021 | `year` | |
| `segment` | ✓ | ✓ | ✓ | — | Which block of results the row belongs to (values differ by file; see below) |
| `metric` | ✓ | ✓ | — | — | What is measured (see below) |
| `health_attribution` | ✓ | ✓ | — | — | `marginal` or `standalone` on health rows; blank otherwise |
| `demo_cat`, `demo_group`, `title` | — | ✓ | ✓ | — | Demographic cut, group code and display label. See [Demographic groups](#demographic-groups) |
| `value` | ✓ | ✓ | ✓ | — | The refinery's own value |
| `share_of_statewide` | ✓ | ✓ | ✓ | per measure | See [Statewide totals and shares](#statewide-totals-and-shares) |

All files are for the Reference scenario only.

### snapshot: `refinery_2021_snapshot.csv`

| Column | Meaning |
|---|---|
| `scenario` | Always `Reference` |
| `segment` | `labor: employment`, `labor: compensation`, `health`, or `output` |
| `effect` | `direct`, `indirect`, `induced`, or `total` (= the sum of the three) on labor rows; blank for health and output |
| `unit` | Unit of `value` (see below) |
| `statewide_total`, `statewide_total_pipeline` | See [Two statewide totals for health](#two-statewide-totals-for-health) |

| `metric` | Unit |
|---|---|
| `emp_direct`, `emp_indirect`, `emp_induced`, `emp_total` | FTE jobs in 2021 |
| `comp_direct`, `comp_indirect`, `comp_induced`, `comp_total` | Compensation, 2019 USD |
| `mortality` | Premature deaths |
| `mortality_usd` | Monetized mortality, 2019 USD, age-based VSL |
| `revenue` | Refinery output value, 2020 USD |

**Output value (`revenue`).** Each refinery's production is valued at 2020 product
prices, held flat across years (Section 3.2). The 2020 price of each product is
the projected 2020 Brent crude price plus that product's average 2005–2014 spread
over Brent (SI Note 3.2, Eq. 11; SI Table 4). This is the quantity that all labor
effects are proportional to.

### annual: `refinery_level_health_labor_annual.csv`

**Filter to `year == 2021`.** Labor in this file is **direct only**. The file has
no unit column; units are given below.

| Column | Meaning |
|---|---|
| `segment` | `health` or `labor` |
| `statewide_total`, `statewide_total_pipeline` | See [Two statewide totals for health](#two-statewide-totals-for-health) |

| `metric` | Unit | Notes |
|---|---|---|
| `avoided_mortality` | deaths | Despite the pipeline-inherited name, this is **attributable** mortality (a level): the deaths that would be avoided if the refinery closed |
| `health_cost_2019` | 2019 USD | Monetized with age-based VSL, undiscounted. **Use this for 2021** |
| `health_cost_pv` | 2019 USD, present value | Discounted at 3% to 2019; not recommended for a single year |
| `fte_job_years` | FTE job-years | Direct employment supported (for one year, = FTE jobs) |
| `compensation` | 2019 USD | Direct compensation supported, undiscounted. **Use this for 2021** |
| `compensation_pv` | 2019 USD, present value | Discounted at 3% to 2019; not recommended for a single year |

**Monetization (SI Note 2).** The age-based VSL comes from Aldy & Viscusi (2008).
Their 2001 cohort-adjusted values are adjusted to 2019 income levels with an
income elasticity of 0.4. The VSL peaks at about $12.7 million around age 46 and
declines to $8.3 million at age 62. Ages 62 and older are assigned the age-62
value.

### demographics: `refinery_demographics_fig_inputs_2021.csv`

| Column | Meaning |
|---|---|
| `segment` | `health` (value = marginal premature deaths) or `labor` (value = direct FTE jobs) |
| `value` | The refinery's 2021 deaths or direct FTE jobs for this group |
| `pop_2020` | Group population aged 30+ (2021 population, despite the name) |
| `per_million` | `value` per million people aged 30+ in the group |
| `overall_per_million` | The refinery's per-million rate across all groups in the `demo_cat` |
| `disparity_index` | `per_million / overall_per_million` |
| `statewide_value`, `statewide_per_million`, `statewide_overall_per_million`, `statewide_disparity_index` | The same quantities for the 16-refinery sector |
| `share_of_statewide` | `value / statewide_value`: the refinery's share of the sector impact **on this group** |
| `in_figure` | `FALSE` for the four small race groups omitted from the figures |

See [Per-capita rates and disparity index](#per-capita-rates-and-disparity-index).

### production_ghg: `refinery_level_production_ghg.csv`

| Column | Meaning |
|---|---|
| `production_bge` | Total refined product output in **barrels of gasoline equivalent**: each fuel's volume converted by energy content (Section 3.1; SI Note 1). Use this for a cross-fuel total; raw barrels of different fuels should not be added together |
| `crude_bbl` | Equivalent crude oil throughput, barrels. The model uses equivalent crude as a proxy for both crude oil and renewable feedstock (Section 2.2; SI Note 1, step 5) |
| `ghg_mtco2e` | GHG emissions, million tonnes CO2e (an allocation of cluster emissions; see [Caveats](#caveats)) |
| `statewide_production_bge`, `statewide_crude_bbl`, `statewide_ghg_mtco2e` | 16-refinery totals for the same year |
| `share_of_statewide_production_bge`, `share_of_statewide_crude_bbl`, `share_of_statewide_ghg_mtco2e` | The refinery's share of each |

---

## Caveats

### Refinery-level quantities are allocations of cluster-level totals

*Applies to: all four files.*

Historical refined fuel production is public only for the two refinery clusters
(North and South), so the model works at the cluster level (Section 2.2; SI
Note 1, step 3). Cluster-level demand is then apportioned to each operating
refinery in proportion to its capacity (Section 2.2; SI Note 3.2).

Emission factors are also set per cluster and applied to every refinery in the
cluster:
- **GHG** factors come from 2018 CARB MRR emissions and 2018 cluster crude
  consumption (SI Note 1, step 10, Eq. 6).
- **Air-pollutant** factors (primary PM2.5, NOx, SOx, NH3, VOC) come from the
  2017 EPA NEI and 2017 cluster crude consumption (SI Note 2, Eq. 7; values in
  SI Table 5).

As a result:
- A refinery's production, revenue, labor, GHG and air-pollutant emissions
  reflect its capacity and cluster, not refinery-specific operations or emission
  rates. Within a cluster, modelled GHG is exactly proportional to crude
  throughput.
- The labor multipliers are the same per dollar of output for every refinery.
  IMPLAN reflects industry-wide relationships, not those of specific refineries
  (SI Note 3.6).
- What *is* refinery-specific on the health side is dispersion. The InMAP
  source-receptor weights are built per refinery, using refinery stack
  parameters from CARB (SI Note 2). The difference in health impacts between the
  two refineries therefore comes from **where each one is located** and from
  their different cluster emission factors.
  - Per barrel of crude, the North cluster factors (Benicia) are *higher* than
    the South's (Wilmington) for primary PM2.5, SOx, NH3 and VOCs (SI Table 5).
  - Even so, Wilmington's attributed mortality is about four times Benicia's.

The paper tests refinery-level air-pollutant emission factors as a sensitivity.
Statewide cumulative health benefits came out at 105–109% of the cluster-level
results (SI Note 2; SI Fig. 4). That variant is not used here.

### Health

*Applies to: health rows in snapshot, annual and demographics.*

- **PM2.5 premature mortality only.** This matches the paper, which excludes
  morbidity and other pollutants (Section 3.2).
- **Adults 30 and over.** The mortality coefficient (β = 0.0058, Krewski et al.
  2009) applies only to adults over 29 (Section 2.5; SI Note 2).
- **Inputs.** Baseline mortality rates are 2015 county-level rates by age group
  from CDC WONDER, assigned to tracts within each county. Tract populations come
  from the 2020 Census, projected forward with California Department of Finance
  county growth rates by age group (SI Note 2).
- **Main specification only.** The VSL sensitivities (constant VSL; age-based VSL
  growing with income) and the 5th/95th-percentile dose-response variants are not
  computed here (Section 2.5; SI Note 2).

### Labor

*Applies to: labor rows in snapshot, annual and demographics.*

- **Commuting shares are not refinery-specific** (direct effects only). They
  cover all workers in the refinery's work tract in NAICS sectors 11, 21, 23 and
  31–33, not only refinery employees. The SI notes that refineries account for
  25–85% of output value in their zip codes (SI Note 3.3).
- **2020 commuting data** (direct effects only). 2020 LODES may reflect COVID-19
  effects. The paper shows its distributional results are similar with 2019
  commuting data (SI Note 3.3; SI Figs. 12–13); that check is not repeated here.
- **Workers' home tracts are held fixed** (SI Note 3.6).

### Why 2021

*Applies to: snapshot, demographics, and the 2021 rows of annual.*

The 2021 files are meant to show the two refineries, which have since closed, in
a year close to the start of the model's 2020–2045 projection period. 2021 is the
second projected year.

For context on how representative that year is:
- In the Reference ("historical production") scenario, crude demand in each
  cluster is set to meet that cluster's historical average capacity utilization,
  and anything beyond in-state demand is exported (Section 2.2; SI Note 1,
  step 7).
- Modelled 2021 statewide production is 635 million bge, in line with the
  paper's 2014–2019 historical average of about 630 million bge per year
  (Section 3.1).
- Both focus refineries' 2021 throughput is the same as in 2020.

---

## Other files in this folder

These are not part of the shared set but are produced by the same scripts.

| File | Contents |
|---|---|
| `refinery_level_health_labor.csv` | The annual file summed over 2020–2045, with added scenario descriptors and `metric_desc` / `unit_desc` columns |
| `refinery_level_production_by_fuel.csv` | Annual production by fuel, in barrels of each fuel, with statewide totals and shares |
| `refinery_demographics_fig_inputs_cumulative_2020_2045.csv` | demographics for the cumulative 2020–2045 period (population base 2020) |
| `fig_2021_labor.png`, `fig_2021_health.png`, `fig_2021_shares.png` | Figures from the snapshot |
| `fig_demographics_{totals,per_capita}_{2021,cumulative_2020_2045}.png` | Health and direct labor by DAC status, poverty and race |
| `fig_refinery_map.png` | Location of the two refineries among all 16, with points sized by capacity |

---

## Reproducing the outputs

The scripts mirror the published pipeline in `R/` (each section cites the
`file:line` it reproduces). They do not modify the pipeline.

**Requirements**
- A built `targets` store (`_targets/`) in the repo root.
- Access to confidential data in `data/confidential-data/`. The store's copies of
  the labor multipliers are blank stubs, so the scripts read the real ones from
  disk.
- The published outputs in `outputs/rev-submission/` (used for validation
  checks).

Run from the **repo root**, in this order:

```sh
Rscript extras/refinery-level-analysis/refinery_level_health_labor.R      # annual, production_ghg, cumulative + by-fuel files
Rscript extras/refinery-level-analysis/refinery_2021_snapshot.R           # snapshot + its figures
Rscript extras/refinery-level-analysis/refinery_demographic_figs.R        # demographics (2021, the default) + figures
Rscript extras/refinery-level-analysis/refinery_demographic_figs.R cumulative
Rscript extras/refinery-level-analysis/refinery_map.R
```

`refinery_demographic_figs.R` reads the annual CSV, so it must run after
`refinery_level_health_labor.R`. Each script prints validation checks to the
console, including reconciliation against the pipeline's published state totals.

---

## Questions and answers

*Answers to questions raised while reviewing these outputs.*
