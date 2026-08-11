# Third South African National Burden of Disease Study — Version 1

This repository produces reproducible mortality estimates for South Africa for 1997–2019. It is the first R implementation of the Third South African National Burden of Disease Study (NBD3) and provides the analytical foundation for extending the study through 2022, including COVID-19 mortality.

Version 1 combines registered cause-of-death data with population estimates, three national injury data sources, completeness adjustments, antenatal HIV prevalence, expert redistribution rules, years-of-life-lost schedules, and standard-population weights. It produces annual cause-specific deaths, rates, YLLs, joint uncertainty intervals, and an interactive collaborator report.

## Quick start

1. Install R 4.5 or a compatible recent version.
2. Place the required raw inputs in `data/raw/`.
3. Place `viz.input.Rda` in `report/data/legacy/`.
4. Run:

```r
source("install_packages.R")
source("run_nbd.R")
```

From Command Prompt:

```bat
Rscript install_packages.R
Rscript run_nbd.R
```

## Normal run controls

The five controls at the top of `run_nbd.R` are the only settings normally changed:

```r
RUN_POINT_ESTIMATES <- TRUE
RUN_UNCERTAINTY     <- TRUE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE
```

Examples:

- Full run: leave all settings as shown.
- Rebuild uncertainty and the report: set `RUN_POINT_ESTIMATES <- FALSE`.
- Rebuild the report only: set both `RUN_POINT_ESTIMATES <- FALSE` and `RUN_UNCERTAINTY <- FALSE`.
- Run the extended development checks: set `RUN_FULL_VALIDATION <- TRUE`.

## Analytical sequence

The point-estimate analysis follows eight stages:

1. **Population denominators** — prepare the population panel for 1997–2019.
2. **Registered cause-of-death data** — verify ICD-10 coding, aggregate records, and redistribute unknown sex, age, and population group.
3. **Injury causes** — prepare NIMS 2000, IMS 2009, and FAMHIS 2017; interpolate injury compositions in additive-log-ratio space; apply a five-year triangular smoother; and allocate the injury envelope across 15 causes.
4. **Completeness and NPR adjustment** — adjust registered deaths for under-reporting and apply the National Population Register transition.
5. **HIV/AIDS** — estimate and reallocate misclassified HIV/AIDS deaths using the fitted statistical models.
6. **Ill-defined and garbage causes** — apply the ordered expert redistribution rules while preserving demographic-cell totals and natural/injury envelopes.
7. **Years of life lost** — map analysis causes to the South African NBD hierarchy and calculate YLLs.
8. **Final database** — add aggregate ages, person estimates, crude rates, age-standardised rates, and national totals.

The `_targets.R` file shows this sequence directly and provides restartable caching. Statistical functions are grouped into nine numbered teaching modules under `R/`.

## Repository map

```text
SA-NBD3-V1/
├── run_nbd.R                  # One top-to-bottom runner
├── _targets.R                 # Readable Stage 01-08 dependency graph
├── install_packages.R
├── R/
│   ├── 00_core.R
│   ├── 01_population_cod.R
│   ├── 02_injuries.R
│   ├── 03_completeness_hiv.R
│   ├── 04_redistribution.R
│   ├── 05_yll_database.R
│   ├── 06_uncertainty.R
│   ├── 07_reporting.R
│   └── 08_pipeline.R
├── config/                    # Point-estimate and uncertainty settings
├── data/
│   ├── raw/                   # Restricted inputs; not tracked by Git
│   ├── lookups/               # Version-controlled cause mappings
│   └── derived/               # Generated analytical checkpoints
├── report/                    # Interactive HTML report and comparison mappings
├── output/                    # Database, uncertainty, tables, figures and report data
├── tests/                     # Automated unit tests
├── dev/                       # Optional extended validation
└── docs/                      # Methods, inputs, uncertainty and workshop guide
```

## Main outputs

```text
output/database/NBD_database_1997_2019.parquet
output/uncertainty/nbd3_v1_joint/
output/report-data/
```

The interactive report compares NBD3-R with NBD3-Stata, NBD2, THEMBISA and GBD2023 where those comparison series are available. The final detailed cause estimates are NBD3-R estimates with joint uncertainty intervals where the draw output supports the selected result.

## Joint uncertainty

Each draw reruns the linked analytical operations in sequence:

- a shared completeness factor derived from the province-level S2 evidence;
- injury survey-composition draws propagated through the fixed interpolation and smoother;
- HIV/AIDS coefficient draws from the fitted variance-covariance matrices; and
- alternative non-empty subsets of the same expert-approved redistribution targets.

The complete cause vector is retained within each draw, so uncertainty in HIV/AIDS donors, redistribution recipients, provinces, South Africa, and population groups is aggregated with its covariance intact. Quantities without a defensible variance source remain fixed.

The default configuration uses 100 draws for collaborator review. Increase `n_draws` in `config/uncertainty_joint.yml` when more stable interval tails are required.

## Documentation

- [Inputs](docs/INPUTS.md)
- [Methods and pipeline](docs/METHODS.md)
- [Uncertainty](docs/UNCERTAINTY.md)
- [Results report](docs/REPORT.md)
- [Collaborator workshop guide](docs/WORKSHOP_GUIDE.md)
- [Development and testing](docs/DEVELOPMENT.md)

## Data governance

Raw mortality microdata, survey data, local comparison files, derived checkpoints, and outputs are excluded from Git. Only code, configuration, tests, documentation, and approved lookup tables should be committed.
