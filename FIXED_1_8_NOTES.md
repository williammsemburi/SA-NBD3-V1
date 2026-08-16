# SA-NBD3 Version 1 — fixed full-UI uncertainty update

This update is a clean publication-scale implementation of the full uncertainty grid. It is intended for a new run and does not recover, convert, or reuse the earlier compact v1.7 draw files.

## What is retained in every draw

The uncertainty engine writes four canonical per-draw stores:

```text
joint/draws/                    compact province/national Person results
joint/population_draws/         compact population-group Person results
joint/full_draws/               province/South Africa base-age Male/Female/Person results
joint/population_full_draws/    population-group base-age Male/Female/Person results
```

The full stores contain all 214 Stage 06 analysis causes, all causes, all injuries, all 20 base ages, all years, and all requested geographies. The report derives aggregate-age deaths, crude mortality rates, and all-age ASRs from these complete joint draws.

## Scalable finalisation

The finaliser no longer attempts to create `uncertainty_draws.parquet`. The individual draw files are the canonical uncertainty data.

After all draws finish, the engine:

1. verifies every expected draw and diagnostic file;
2. streams the compact death vectors to a temporary binary matrix;
3. calculates exact type-8 quantiles, means and standard deviations in bounded row blocks;
4. calculates convergence and HIV-cause covariance diagnostics without combining the full draw tables;
5. writes `uncertainty_draw_storage.csv` and `UNCERTAINTY_DRAW_STORAGE.txt`; and
6. removes the temporary binary matrix after successful completion.

The report validates the complete full-draw directories and calculates selected final-cause intervals dynamically. It does not materialise a single final-cause uncertainty table covering every possible tool selection.

## Visualization coverage

The interactive tool supports joint uncertainty for:

- all supported causes and aggregates;
- South Africa and the nine provinces;
- South Africa and the four national population groups;
- Male, Female and Person;
- all supported age groups;
- deaths;
- crude mortality rates; and
- all-age age-standardised mortality rates calculated inside each draw.

YLLs remain in the analytical database but are not available as a visualization metric.

## Configuration

The publication configuration is:

```yaml
run:
  output_name: nbd3_v1_joint_full_ui_1000
  n_draws: 1000
```

This new output directory prevents any compact v1.7 files from being reused.

## Required package

`matrixStats` has been added to `install_packages.R` for efficient exact row-wise type-8 quantiles during bounded finalisation.

## Run from an existing validated point-estimate repository

Set the controls at the top of `run_nbd.R` to:

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- TRUE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE
OVERWRITE_UNCERTAINTY <- FALSE
```

Then run:

```bat
Rscript install_packages.R
Rscript run_nbd.R
```

Because the output name is new, overwrite should remain `FALSE` unless this exact new directory already contains incomplete files created by the same code version.

## Storage planning

The full grid is much larger than the compact reporting grid. Ensure that the drive has ample free space before beginning the 1,000-draw run. The exact per-draw sizes depend on Parquet compression and the generated data; the engine writes a storage manifest after completion.
