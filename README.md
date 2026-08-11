# SA-NBD3 Version 1 comparison-interval fix

## What this fixes

The 100 joint uncertainty draws completed successfully. The report then stopped after processing all 100 comparison draw files with:

```
The uncertainty interval table contains 1 invalid row(s).
```

The comparison table used the fast row-wise type-8 quantile summariser but discarded the original draw matrix before applying the exact interval-repair routine already used by the final-cause and population-group tables.

This update retains the comparison draw matrix until interval validation is complete. Any non-finite or reversed comparison interval is recomputed from its original 100 draw values using:

```r
stats::quantile(values, probs = c(0.025, 0.5, 0.975), type = 8)
```

No draw, point estimate, uncertainty assumption, cause mapping, or external comparison series is changed.

## Install

Close any running report, then extract the contents of this ZIP directly into the root of the existing `SA-NBD3-V1` repository. Allow Windows to replace:

```
R/07_reporting.R
tests/testthat/test-integrated-reporting.R
```

## Run only the report

At the top of `run_nbd.R`, use:

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE
```

Then run:

```bat
cd /d "C:\Users\williamms\SA-NBD3-V1"
Rscript run_nbd.R
```

Do not rerun the deterministic analysis or the 100 uncertainty draws.

## Diagnostic output

When the affected row is repaired, the console will report it and write:

```
output/report-data/comparison_interval_repair.csv
```

The file records the original summary, the minimum and maximum of the 100 draws, and the exact recomputed type-8 interval.
