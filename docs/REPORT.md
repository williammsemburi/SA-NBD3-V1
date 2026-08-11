# Interactive results report

The report is an interactive HTML document built from the final NBD3 Version 1 database, the completed joint draws, and `viz.input.Rda`.

## Sections

1. Study overview and the development of the South African NBD studies.
2. HIV/AIDS methods and comparison of NBD2, NBD3-Stata, NBD3-R, THEMBISA and GBD2023.
3. Injury methods, survey inputs and comparison of NBD2, NBD3-Stata, NBD3-R and GBD2023.
4. Final NBD3-R cause estimates for provinces and population groups.
5. Methods used to derive uncertainty.

The final detailed cause panels show NBD3-R only. Model comparisons are confined to the dedicated HIV/AIDS and injury sections.

## Tables

All displayed tables use a common `knitr::kable()` HTML style with captions, sticky headers, alternating row shading, numeric alignment and responsive scrolling. Downloads remain available for the underlying selected data.

## Uncertainty

The report reads the individual per-draw files rather than one oversized consolidated Parquet file. Final cause and population-group results are mapped within each draw so covariance is retained. Intervals appear only where the draw output provides an exact match for the selected geography, sex, age, year and cause.

## Running the report

The normal route is:

```r
source("run_nbd.R")
```

For a report-only rebuild, set:

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
```
