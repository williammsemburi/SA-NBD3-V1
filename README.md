# Third South African National Burden of Disease Study

**Version 1 — Reproducible mortality estimates for South Africa, 1997–2019**

This repository contains the R implementation of the Third South African National Burden of Disease Study (NBD3). It produces annual, internally coherent mortality estimates by cause, province, population group, sex, age, and year. It also calculates years of life lost, propagates uncertainty through the main analytical corrections, and builds an interactive results report.

The repository is organised for three uses:

- running the complete analysis from raw inputs to the final report;
- reviewing and teaching the statistical methods step by step; and
- maintaining a transparent, testable Git codebase for collaboration.

## Study scope

The analysis covers **1997–2019**. It estimates:

- deaths by detailed and aggregate South African NBD causes;
- crude mortality rates;
- age-standardised mortality rates;
- years of life lost under the supplied YLL schedules;
- national and provincial estimates;
- national population-group estimates; and
- 95% uncertainty intervals for supported deaths and crude-rate outputs.

Years 2020–2022 and the explicit estimation of COVID-19 mortality are outside the scope of this repository.

## Analytical workflow

The core mortality analysis follows six linked operations.

### 1. Clean and aggregate registered cause-of-death data

The pipeline reads the record-level mortality data, verifies ICD-10 information, applies the South African NBD cause classification and plausibility rules, implements documented exclusions and corrections, and aggregates the records to the analytical grid. Unknown sex, age, and population group are redistributed while preserving the represented number of deaths.

### 2. Construct the completed mortality envelope

African natural deaths are adjusted to the specified all-cause envelope using the established S1, S2, and National Population Register inputs. The completed envelope is the mortality boundary used by all downstream stages.

### 3. Calibrate total injury mortality

IMS 2009 and FAMHIS 2017 are estimated directly from the supplied survey records and weights.

- IMS injury-level eligibility: `Cause_of_death == 1`.
- FAMHIS injury-level eligibility: a nonblank injury mechanism and no explicit exclusion through `non_nat_undert == 0`.

These rules reproduce weighted national totals of approximately 52,493 injuries in 2009 and 53,288 injuries in 2017 from the supplied analytical files. Published estimates are retained as external audit references rather than substituted into the point calculation.

For each survey year, the weighted survey injury total and province–sex–age profile are compared with the completed routine injury envelope. The IMS correction is applied through 2009, the IMS and FAMHIS corrections are interpolated for 2010–2016, and the FAMHIS correction is applied for 2017–2019. The calibration changes the division between natural and injury deaths while preserving each completed all-cause demographic cell.

### 4. Estimate HIV/AIDS mortality

The HIV/AIDS model is applied after completeness and injury calibration because both operations affect the remaining natural-cause envelope. The fitted model estimates misclassified HIV/AIDS deaths and reallocates them from the approved donor causes while preserving the demographic-cell total.

### 5. Estimate specific injury causes

The calibrated injury envelope is divided among 15 injury causes using NIMS 2000, IMS 2009, and FAMHIS 2017.

All eligible injuries contribute to the total injury envelope. Cause fractions use only records that map directly to a common, well-specified injury cause. Generic and unresolved mechanisms therefore contribute to injury completeness but do not determine a specific cause fraction.

NIMS supplies the national 2000 composition and IMS supplies the spatial distribution used to expand that composition. IMS supplies the 2009 composition and FAMHIS supplies the 2017 composition. Causes 132, 136, and 138 are harmonised before interpolation to reduce artefacts caused by differences in survey classification. Annual fractions are constructed in hierarchical additive-log-ratio space and transformed back to positive fractions that sum to one.

### 6. Redistribute ill-defined and garbage causes

The deterministic analysis applies the complete ordered set of expert-approved redistribution rules. Each source is allocated proportionally across its approved targets using the current target distribution, with biological fallback distributions where the target denominator is zero. Redistribution preserves the natural, injury, and all-cause envelopes.

After these six operations, the pipeline maps analysis causes to the final South African NBD hierarchy, calculates YLLs and rates, constructs aggregate ages and person estimates, and writes the final database.

## Joint uncertainty model

The uncertainty analysis reruns the evidence-supported stochastic operations in analytical order. Aggregation occurs within each draw, so covariance among causes, provinces, and population groups is preserved.

The four main uncertainty drivers are:

1. **African natural-cause reporting completeness.** Each province receives a mean-one factor. Its log standard deviation is estimated from death-weighted annual aggregate S2 variation across time after age and sex are collapsed.
2. **Injury level, demographic profile, and cause composition.** IMS mortuaries and FAMHIS forensic pathology service units are resampled within their survey strata. The same survey replicate jointly determines the national injury total, provincial and sex-age profile, and well-specified cause counts. NIMS contributes count-based cause-composition uncertainty.
3. **HIV/AIDS estimation.** Fitted Stage 05 coefficient vectors are sampled jointly from their estimated variance-covariance matrices, after completeness and injury calibration.
4. **Redistribution.** Every approved target remains eligible. Multi-target rules receive continuous positive random target multipliers, and the weighted proportional redistribution is rerun. Single-target rules remain deterministic.

The 95% uncertainty intervals are the empirical 2.5th and 97.5th percentiles of the final draw-specific estimates.

## Repository structure

```text
R/          Numbered analytical modules
config/     Point-estimate and uncertainty settings
data/       Raw inputs, bundled lookups, and derived checkpoints
dev/        Extended diagnostics and full validation tools
docs/       Methods, inputs, uncertainty, and workshop documentation
output/     Databases, uncertainty outputs, figures, report data, and tables
report/     Interactive HTML report and report-specific helpers
tests/      Focused automated tests
_targets.R  Deterministic dependency graph
run_nbd.R   Main project entry point
```

The R modules follow the analytical narrative:

| Module | Purpose |
|---|---|
| `00_core.R` | Configuration, paths, assertions, constants, and shared utilities |
| `01_population_cod.R` | Population preparation, ICD verification, COD aggregation, and unknown-demographic redistribution |
| `02_injuries.R` | Injury survey preparation, injury-level calibration, cause fractions, harmonisation, and interpolation |
| `03_completeness_hiv.R` | Completeness, NPR integration, prevalence preparation, and HIV/AIDS estimation |
| `04_redistribution.R` | The `redist()` helper and ordered redistribution rules |
| `05_yll_database.R` | Cause hierarchy, YLLs, rates, aggregate ages, and final database construction |
| `06_uncertainty.R` | Joint uncertainty propagation |
| `07_reporting.R` | Conversion of complete draws into reportable uncertainty intervals |
| `08_pipeline.R` | Stage wrappers and checkpoint writing |

## Required inputs

Place the restricted analytical files in `data/raw/`. Lookup tables required by the analysis are already included in `data/lookups/`.

| Input | Default filename |
|---|---|
| Cause-of-death microdata | `COD1997-2019_F1.dta` |
| Population | `NewPropPopulation.dta` |
| Population fallback workbook | `New AltMYR.xls` |
| NIMS 2000 | `NIMS2.xlsx` |
| IMS 2009 | `IMS raw data for completeness.dta` |
| FAMHIS 2017 | `FAMHIS_FinalLabelled_Updated&Weighted_04January2023_NBD.dta` |
| Child completeness | `Completeness25April2014.csv` |
| Provincial completeness | `Completeness and Province Deaths 27012014.xlsx` |
| National Population Register | `NPR15_22.dta` |
| HIV prevalence | `ProvPrevs.csv` |
| YLL schedules | `NBD YLL.xlsx` |
| ASR factors | `ASRFactor.dta` |

For the model-comparison sections of the report, place:

```text
viz.input.Rda
```

at:

```text
report/data/legacy/viz.input.Rda
```

The exact input contract and supported aliases are documented in `docs/INPUTS.md`. Filenames can be changed in `config/config.yml` without editing the analytical functions.

## Installation

From the repository root:

```bat
Rscript install_packages.R
```

The installation script checks and installs the packages required by the pipeline and report.

## Running the complete project

Run:

```bat
Rscript run_nbd.R
```

The controls at the top of `run_nbd.R` determine which phases execute:

```r
RUN_POINT_ESTIMATES <- TRUE
RUN_UNCERTAINTY     <- TRUE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE
```

A normal complete run performs:

```text
Input check
    -> point-estimate analysis
    -> joint uncertainty
    -> report-data construction
    -> interactive HTML report
```

The `{targets}` cache allows completed deterministic stages to be reused when their inputs and source functions have not changed.

### Run point estimates only

```r
RUN_POINT_ESTIMATES <- TRUE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- FALSE
OPEN_REPORT         <- FALSE
```

### Run uncertainty and rebuild the report from completed point estimates

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- TRUE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
```

### Rebuild the report only

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
```

The number of uncertainty draws and the output directory are configured in `config/uncertainty_joint.yml`. A small draw count is useful for checking a new statistical specification before a full run.

## Main outputs

### Deterministic checkpoints

```text
data/derived/
```

The numbered files in this directory correspond to the main analytical stages and allow inspection of intermediate estimates.

### Final database

```text
output/database/NBD_database_1997_2019.parquet
```

### Uncertainty results

```text
output/uncertainty/<configured output name>/
```

This directory contains completed draws, draw diagnostics, uncertainty summaries, province outputs, and population-group outputs.

### Tables and diagnostics

```text
output/tables/
```

Key audit tables include the input status, injury survey reference comparison, completeness diagnostics, point-reconstruction checks, and report-input coverage.

### Interactive report

The report is generated from:

```text
report/nbd3_results_report.Rmd
```

It presents:

- the development of the South African NBD studies;
- HIV/AIDS methods and model comparisons;
- injury data sources, methods, and model comparisons;
- final provincial and population-group estimates;
- propagated uncertainty; and
- the assumptions and current boundaries of the analysis.

NBD3-Stata is retained as a working unpublished comparison series. NBD3-R is the reproducible implementation used to generate the final estimates in this repository. NBD2, THEMBISA, and GBD2023 are included where equivalent comparison series are available.

## Validation and testing

The normal collaborator workflow uses the safeguards embedded in each analytical stage. Extended validation is available when required:

```r
RUN_FULL_VALIDATION <- TRUE
```

or:

```r
source("dev/full_validation.R")
```

Focused automated tests are stored in `tests/testthat/`.

## Reproducibility and Git

Source code, configuration, lookup tables, tests, and documentation should be committed to Git. Restricted raw data, derived checkpoints, uncertainty draws, report outputs, and local R session files are excluded through `.gitignore`.

Analytical changes should be made in the relevant numbered R module and accompanied by:

- a clear description of the statistical change;
- an update to the relevant methods documentation;
- focused tests where practical; and
- review of point-estimate and uncertainty diagnostics before results are shared.

The repository does not use alternate BAT launchers or multiple competing entry scripts. `run_nbd.R` is the project entry point.

## Documentation

| File | Content |
|---|---|
| `docs/ANALYTICAL_SEQUENCE.md` | Concise analytical sequence |
| `docs/METHODS.md` | Point-estimate methods |
| `docs/UNCERTAINTY.md` | Joint uncertainty methods |
| `docs/INJURY_SURVEY_AUDIT.md` | Survey eligibility, weighted totals, and injury survey design checks |
| `docs/INPUTS.md` | Input files and data contracts |
| `docs/REPORT.md` | Report structure and runtime-data contract |
| `docs/WORKSHOP_GUIDE.md` | Suggested sequence for explaining the analysis to collaborators |

## Data governance

The raw mortality and survey data are restricted and are not distributed through this repository. Collaborators are responsible for using the data under the applicable approvals and data-sharing agreements. Do not commit raw inputs, individual-level data, derived record-level extracts, or any file containing identifiable information to Git.
