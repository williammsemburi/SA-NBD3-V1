# Interactive results report

`nbd3_results_report.Rmd` is the interactive HTML report for the Third South African National Burden of Disease Study, covering 1997–2019.

Build and open the report through the project entry point:

```r
source("run_nbd.R")
```

or from a command prompt:

```bat
Rscript run_nbd.R
```

The report combines:

- NBD3-R point estimates and draw-supported uncertainty intervals;
- the working unpublished NBD3-Stata comparison series;
- NBD2, THEMBISA, and GBD2023 comparisons where equivalent series are available;
- HIV/AIDS and injury methods;
- provincial and population-group results; and
- the assumptions and current boundaries of the uncertainty analysis.

The comparison input belongs at:

```text
report/data/legacy/viz.input.Rda
```

The report uses the runtime data built in `output/report-data/`. Its data contract and supported output dimensions are documented in `docs/REPORT.md`.
