# Third South African National Burden of Disease Study

**Version 1 — Reproducible cause-of-death estimates for South Africa, 1997–2019**

This repository contains the analytical code, configuration, tests, documentation, and interactive research report supporting publications from the **Third South African National Burden of Disease Study (NBD3)**. The analysis produces internally coherent annual mortality estimates by cause, geography, population group, sex, age, and year, while propagating uncertainty through the principal corrections applied to the registered cause-of-death data.

The repository is intended to support scientific transparency and reproducibility. It documents how the published estimates are generated, provides the exact computational workflow used for the study, and separates restricted source data from code and publication-ready outputs.

## Study scope

The current analysis covers:

- **Years:** 1997–2019
- **Geography:** South Africa, the nine provinces, and four national population groups
- **Sex:** male, female, and person
- **Age:** neonatal, post-neonatal, childhood, adult five-year age groups, and reporting aggregates
- **Cause hierarchy:** all causes, broad cause groups, disease categories, detailed South African NBD causes, and supported reporting aggregates
- **Primary mortality measures:** deaths, crude mortality rates, and age-standardised mortality rates
- **Additional analytical measure:** years of life lost (YLLs), retained in the database but not exposed in the interactive results tool

The public-facing results report focuses on mortality through 2019. Estimates for the COVID-19 period, comparative risk assessment, and health-district burden are outside the scope of this repository release.

## Scientific objectives

NBD3 is designed to estimate the level, composition, and distribution of mortality in South Africa after addressing known limitations in routine mortality data. The analytical workflow addresses six linked problems:

1. classification and aggregation of registered causes of death;
2. incomplete registration of deaths;
3. incomplete or distorted reporting of injury mortality;
4. misclassification of HIV/AIDS deaths to indicator causes;
5. incomplete information on specific injury causes; and
6. ill-defined, intermediate, and otherwise unsuitable underlying causes of death.

The resulting estimates are constructed to remain internally coherent: demographic-cell totals are conserved through cause reallocation, injury and natural-cause envelopes reconcile to completed all-cause mortality, and final cause aggregates are formed within each uncertainty draw.

## Analytical workflow

### 1. Prepare population and registered mortality data

The pipeline reads Statistics South Africa cause-of-death microdata, verifies and standardises certificate fields, applies the project ICD-10-to-NBD mapping, and implements age-, sex-, perinatal-, cancer-, and multiple-cause rules. It then redistributes records with unknown sex, age, or population group and collapses the data to the analytical mortality grid.

### 2. Complete the mortality envelope

Registered deaths are reconciled to specified all-cause mortality envelopes using completeness inputs developed for the South African NBD programme. The correction distinguishes African natural deaths from other mortality components. African natural mortality is treated as the residual component required to reconcile registered mortality to the completed envelope. National Population Register information is used to inform changes in reporting completeness after the period covered by the original NBD2 completeness inputs.

For uncertainty, each province receives a mean-one reporting factor. Its log-scale dispersion is estimated from death-weighted variation in annual aggregate completeness values over time after age and sex have been collapsed.

### 3. Calibrate total injury mortality

The injury envelope is estimated from the supplied nationally representative injury mortality surveys rather than from hard-coded totals.

- **IMS 2009:** the supplied records and weights reproduce the published-compatible national injury total.
- **FAMHIS 2017:** the later-cleaned published eligibility definition and supplied weights reproduce the corresponding national injury total.

Survey-eligible injury records determine the national and provincial injury level and the province–sex–age profile. Stratified primary-sampling-unit bootstrap replicates preserve the joint uncertainty in the national total, provincial totals, demographic profile, and well-specified injury causes.

The survey-to-vital-registration calibration factor observed in 2009 is applied through 2009. The 2009 and 2017 factors are interpolated on the log scale for 2010–2016, and the 2017 factor is applied through 2019. Any change in the injury envelope is offset against natural-cause mortality so that the completed all-cause total remains unchanged.

### 4. Correct misclassified HIV/AIDS mortality

The HIV/AIDS model estimates deaths assigned to causes that act as indicators of underlying HIV infection. It combines antenatal HIV prevalence, counterfactual background mortality, province- and sex-specific temporal trends, and age patterns to identify excess deaths among the indicator causes. Those deaths are transferred to HIV/AIDS while conserving deaths within each demographic cell.

The deterministic estimate uses the fitted model coefficients. Joint uncertainty draws sample the fitted coefficient vectors from their estimated variance-covariance matrices after completeness and injury calibration have been applied.

### 5. Estimate specific injury causes

The completed injury envelope is distributed across 15 injury causes using evidence from:

- NIMS 2000;
- IMS 2009; and
- FAMHIS 2017.

All survey-eligible injuries contribute to injury completeness. Cause fractions are based on records that can be assigned to a well-specified common injury cause. Generic and unresolved injuries remain part of the total injury envelope but do not provide direct evidence for a specific cause fraction.

Cause fractions are represented using a hierarchical additive-log-ratio formulation, interpolated between survey anchors, and transformed back to positive compositions that sum to one. Causes 132, 136, and 138 are harmonised before interpolation to reduce artefacts caused by differences in classification across the surveys. NIMS supplies the national 2000 composition, with IMS information used to support geographic allocation.

### 6. Redistribute ill-defined and garbage-coded causes

The deterministic analysis applies the ordered expert-approved redistribution rules. Each source cause is allocated proportionally across its approved targets, with biological fallback distributions where the observed target denominator is zero.

Redistribution uncertainty is represented using continuous positive multipliers on the same approved target vector. For a multi-target rule, each draw changes the relative weights of the eligible destinations while retaining every approved target and conserving all source deaths. Single-target rules remain deterministic.

## Joint uncertainty

The uncertainty engine reruns the linked stochastic operations in analytical order. A complete draw contains:

1. province-specific African natural-cause completeness factors;
2. a stratified IMS survey replicate;
3. a stratified FAMHIS survey replicate;
4. NIMS count-based cause-fraction uncertainty;
5. a multivariate HIV/AIDS coefficient draw; and
6. continuous redistribution target-weight draws.

The final cause hierarchy, aggregate ages, person estimates, crude rates, and age-standardised rates are constructed inside each draw. This preserves covariance among causes, ages, sexes, provinces, and population groups.

The interactive tool reports empirical 95% uncertainty intervals for all supported combinations of:

```text
cause or aggregate × geography × sex × age × year × metric
```

The supported metrics are:

- deaths;
- crude mortality rate per 100,000; and
- all-age age-standardised mortality rate per 100,000.

Population denominators and standard-population weights are treated as fixed. ASRs are recalculated inside each draw rather than being derived from interval endpoints. The neonatal view is deaths-only because the analytical population file does not contain a separate neonatal denominator.

## Data sources

The main analytical inputs are:

| Domain | Source |
|---|---|
| Registered mortality | Statistics South Africa cause-of-death microdata, 1997–2019 |
| Population denominators | South African mid-year population estimates used by the study |
| Mortality completeness | Child and adult completeness inputs developed for the South African NBD programme |
| Recent registration change | National Population Register deaths |
| Injury composition, 2000 | National Injury Mortality Surveillance System (NIMS) |
| Injury level and causes, 2009 | Injury Mortality Survey (IMS) |
| Injury level and causes, 2017 | Fatal Injury Mortality Survey (FAMHIS) |
| HIV model | Provincial antenatal HIV prevalence and registered indicator-cause mortality |
| Cause hierarchy | South African NBD cause list and analysis-to-report mappings |
| Standardisation | WHO standard-population factors used by the study |

The raw mortality and survey files are restricted and are not distributed in the public repository. See `docs/INPUTS.md` for the expected filenames, schemas, and supported aliases.

## Repository structure

```text
R/                  Numbered analytical modules
config/             Point-estimate and uncertainty settings
data/lookups/        Version-controlled mappings and analytical lookups
data/raw/            Restricted source data; excluded from Git
data/derived/        Cached analytical checkpoints; excluded from Git
dev/                 Extended diagnostics and release validation
docs/                Methods, inputs, uncertainty, and report documentation
output/              Databases, uncertainty draws, tables, figures, and report data
report/              Shiny-enabled R Markdown research report
report/R/            Report-data and Highcharter helpers
tests/               Focused automated tests
_targets.R           Deterministic dependency graph
run_nbd.R            Main project entry point
install_packages.R   Package installation and dependency check
```

The R modules follow the analytical sequence:

| Module | Purpose |
|---|---|
| `00_core.R` | Configuration, paths, assertions, constants, and shared utilities |
| `01_population_cod.R` | Population preparation, ICD verification, mortality aggregation, and unknown-demographic redistribution |
| `02_injuries.R` | Injury survey preparation, injury-level calibration, cause fractions, harmonisation, and interpolation |
| `03_completeness_hiv.R` | Mortality completeness, NPR integration, prevalence preparation, and HIV/AIDS estimation |
| `04_redistribution.R` | Redistribution helper and ordered expert rules |
| `05_yll_database.R` | Cause hierarchy, mortality measures, aggregate ages, YLLs, and final database construction |
| `06_uncertainty.R` | Joint uncertainty propagation |
| `07_reporting.R` | Conversion of complete draws into reportable intervals |
| `08_pipeline.R` | Stage wrappers and checkpoint writing |

## Reproducibility model

This repository supports three levels of reproducibility:

| Level | What can be reproduced | Requirements |
|---|---|---|
| Public code review | Methods, functions, configuration, tests, and report structure | Public repository only |
| Authorised analytical reproduction | Point estimates, joint uncertainty, database, and report | Restricted source data plus the repository |
| Publication-output reproduction | Figures and tables from released aggregate outputs | Publication-specific aggregate data release, where approved |

The deterministic pipeline uses `{targets}` to cache intermediate stages. Uncertainty draws use controlled random seeds and a run signature that prevents incompatible outputs from being mixed. The exact Git commit and tagged release used for each publication should be recorded in the paper, supplementary material, or archived software citation.

## Installation

A current R installation is required. From the repository root:

```bat
Rscript install_packages.R
```

The uncertainty finalisation step uses `matrixStats` for exact row-wise type-8 quantiles without loading the complete 1,000-draw table into memory; it is installed by `install_packages.R`.

Place the restricted inputs in `data/raw/` and the legacy comparison object at:

```text
report/data/legacy/viz.input.Rda
```

The default filenames are listed in `docs/INPUTS.md` and can be changed in `config/config.yml` without editing the analytical functions.

## Running the analysis

Run the project from the repository root:

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

### Point estimates only

```r
RUN_POINT_ESTIMATES <- TRUE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- FALSE
OPEN_REPORT         <- FALSE
```

### Uncertainty and report from completed point estimates

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- TRUE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
```

### Report only

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
```

The publication configuration in `config/uncertainty_joint.yml` uses 1,000 joint draws and writes to `output/uncertainty/nbd3_v1_joint_full_ui_1000/`. A smoke test should use a different `output_name`; smoke-test and publication draws must never share one output directory.

## Main outputs

### Final analytical database

```text
output/database/NBD_database_1997_2019.parquet
```

### Joint uncertainty

```text
output/uncertainty/<configured-output-name>/
```

The complete visualization grid retains cause-specific, base-age, sex-specific, provincial, national, and population-group deaths in one Parquet file per draw. Those per-draw files are the canonical uncertainty data. The pipeline deliberately does not create one monolithic draw table because a 1,000-draw table exceeds common Arrow/Parquet Thrift limits. Compact analytical summaries are calculated in bounded blocks. For the publication application, `dev/build_shiny_ui_cache.R` converts the complete draws into partitioned interval summaries so the live report never scans the raw draw archive.

### Tables, diagnostics, and audit outputs

```text
output/tables/
output/figures/
```

These directories contain input audits, injury survey reconciliation, completeness diagnostics, point-reconstruction checks, uncertainty diagnostics, and publication-supporting figures and tables.

### Interactive research report

```text
report/nbd3_results_report.Rmd
```

The report is organised as a research report with background, data, methods, results, discussion, and a technical appendix. It includes:

- completeness diagnostics by province and year;
- HIV/AIDS method and model-comparison panels;
- injury survey, calibration, composition, and model-comparison panels;
- an explanation of redistribution and uncertainty;
- one-cause comparisons across South Africa, provinces, and population groups; and
- multi-cause comparisons within a selected geography or population group.

Interactive charts use animated Highcharts transitions by default. Animation can be disabled for testing, performance, or reduced-motion use with:

```r
options(nbd3.highcharts.animation = FALSE)
```

## Validation and quality assurance

The pipeline contains checks for:

- required inputs and schemas;
- unique analytical keys;
- finite and non-negative estimates;
- preservation of deaths during demographic redistribution;
- closure of injury cause fractions;
- reproduction of empirical injury survey totals;
- conservation of all-cause, natural-cause, and injury envelopes;
- HIV/AIDS reallocation identities;
- redistribution source depletion and envelope preservation;
- consistency of national, provincial, sex, and age aggregations; and
- point reconstruction by the uncertainty engine.

Focused automated tests are stored in `tests/testthat/`. Extended release checks are available through:

```r
RUN_FULL_VALIDATION <- TRUE
```

or:

```r
source("dev/full_validation.R")
```

## Data governance

The raw mortality and survey data are confidential or access controlled. They must not be committed to Git, attached to public issues, or included in public release archives. The `.gitignore` excludes raw inputs, record-level derived data, analytical caches, uncertainty draws, report outputs, and local R session files.

Users with authorised access remain responsible for complying with the relevant ethics approvals, data-sharing agreements, institutional policies, and disclosure controls. Public releases should contain only code, documentation, non-restricted lookups, and approved aggregate outputs.

## Publication and release practice

For each publication supported by this repository:

1. freeze the analytical configuration and uncertainty draw count;
2. run the complete validation suite;
3. record the Git commit SHA in the publication materials;
4. create a tagged GitHub release corresponding to the submitted or accepted analysis;
5. archive the tagged release in an approved long-term repository where possible;
6. publish only disclosure-reviewed aggregate outputs; and
7. add the final article citation and software archive identifier to this README.

The repository `VERSION` file identifies the analytical release. Only tagged scientific releases form the public version history.

## Citation

When using this repository, cite both:

1. the relevant NBD3 publication; and
2. the tagged software release used to produce the reported results.

The definitive NBD3 article citation and archival software identifier should be inserted here when the study is published.

### Related South African NBD publications

- Bradshaw D, Groenewald P, Laubscher R, et al. *Initial burden of disease estimates for South Africa, 2000.* South African Medical Journal. 2003;93(9):682–688.
- Pillay-van Wyk V, Msemburi W, Laubscher R, et al. *Mortality trends and differentials in South Africa from 1997 to 2012: second National Burden of Disease Study.* The Lancet Global Health. 2016;4(9):e642–e653. DOI: 10.1016/S2214-109X(16)30113-9.
- Matzopoulos R, Prinsloo M, Pillay-van Wyk V, et al. *Injury-related mortality in South Africa: a retrospective descriptive study of postmortem investigations.* Bulletin of the World Health Organization. 2015;93:303–313. DOI: 10.2471/BLT.14.145771.
- Prinsloo M, Mhlongo S, Roomaney RA, et al. *Injury mortality in South Africa: a 2009 and 2017 comparison to track progress to meeting sustainable development goal targets.* Global Health Action. 2024;17(1):2377828. DOI: 10.1080/16549716.2024.2377828.

## Contributing

Changes should be scientifically motivated, reviewable, and traceable. Modify the relevant numbered module rather than adding fragmented helper scripts. Analytical changes should include updated documentation, focused tests, and review of point-estimate and uncertainty diagnostics. Cause-list changes and redistribution-target changes require substantive expert review.

See `CONTRIBUTING.md` for the repository workflow.

## Licence

The repository currently carries an all-rights-reserved notice while the public software licence is being determined. **An approved software licence must replace the current notice before public release.** Data access and output reuse may remain subject to separate restrictions even after a software licence is selected.

## Questions and issue reporting

Once the repository is public, use GitHub Issues for reproducible code problems, documentation errors, and feature requests. Do not attach restricted data. Scientific interpretation and data-access requests should follow the contact information in the associated NBD3 publication.

## Fast publication application

The public Shiny report is deployed from a query-optimised summary cache, not
from the 1,000 raw uncertainty draw files. This keeps the publication app small,
low-memory, and responsive while preserving the complete draws in the
analytical archive.

After the full-grid uncertainty analysis is complete, run:

```bat
Rscript dev\build_shiny_ui_cache.R
```

Then test the report and prepare a minimal shinyapps.io bundle:

```bat
Rscript dev\prepare_shinyapps_bundle.R
```

The deployment bundle is written to:

```text
deployment/shinyapps/
```

Deploy it with:

```bat
Rscript dev\deploy_shinyapps.R
```

The live report uses partitioned Parquet interval summaries, app-level Shiny
caching, debounced controls, and short Highcharts transitions. Raw-draw queries
are disabled by default and are available only as an explicit local debugging
fallback.

See [`docs/SHINY_DEPLOYMENT.md`](docs/SHINY_DEPLOYMENT.md) for cache-building,
deployment, storage, and performance guidance.
