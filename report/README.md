# NBD3 interactive report

`nbd3_results_report.Rmd` is the interactive HTML report for the Third South African National Burden of Disease Study, Version 1 (1997–2019).

Run the report through the project entry point:

```r
source("run_nbd.R")
```

The report combines the historical and external comparison series with the final NBD3-R estimates and exact draw-supported uncertainty intervals. Its injury section also compares the three survey inputs with the final smoothed injury interpolation, without treating survey-year values as exact constraints. See `docs/REPORT.md` for the report structure and runtime-data contract.
