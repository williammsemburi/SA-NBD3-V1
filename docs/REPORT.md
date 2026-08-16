# Interactive results report

The report is an interactive HTML document built from the final NBD3 Version 1 database, the completed joint draws, and `viz.input.Rda`.

## Sections

1. Study overview and the development of the South African NBD studies.
2. HIV/AIDS methods and comparison of NBD2, NBD3-Stata, NBD3-R, THEMBISA and GBD2023.
3. Injury methods, including survey-derived IMS/FAMHIS injury-level and relative-profile calibration, well-specified cause inputs, and comparison of NBD2, NBD3-Stata, NBD3-R and GBD2023.
4. Final NBD3-R cause estimates for provinces and population groups.
5. Methods used to derive uncertainty.

The final detailed cause panels show NBD3-R only. Model comparisons are confined to the dedicated HIV/AIDS and injury sections.

## Tables

All displayed tables use a common `knitr::kable()` HTML style with captions, sticky headers, alternating row shading, numeric alignment and responsive scrolling. Downloads remain available for the underlying selected data.

## Uncertainty

The publication-scale uncertainty engine treats the individual per-draw files as the canonical draw store and writes a manifest rather than attempting to serialize a monolithic table. An offline cache builder maps those draws to compact, partitioned interval summaries before deployment. The live report reads the summaries and does not scan the raw draw files. The full analytical archive continues to retain Male, Female and Person deaths for South Africa, all provinces, and all four national population groups.

For every supported cause and aggregation, the interactive explorers derive exact draw-based intervals for:

- deaths at every supported report age;
- crude mortality rates at ages with a valid population denominator; and
- all-age age-standardised death rates calculated within each draw.

Final cause and population-group results are mapped within each draw so covariance from survey-design injury level/profile/cause draws, completeness, HIV/AIDS and continuous redistribution weights is retained. YLLs remain in the analytical database but are not offered as a visualization metric.

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

## Production performance

The publication app reads `output/report-data/ui_uncertainty_cache/` rather
than scanning the raw uncertainty draw files. The cache is partitioned by sex
and age, with separate province/national and population-group stores. Model
comparison intervals are precomputed in one compact table.

The report also uses application-level `bindCache()` memoisation and debounces
linked inputs before querying. The analytical draw files remain the canonical
scientific archive, while the deployment cache is a deterministic reporting
product that can be rebuilt from those draws.

Build and deploy using:

```bat
Rscript dev\build_shiny_ui_cache.R
Rscript dev\prepare_shinyapps_bundle.R
Rscript dev\deploy_shinyapps.R
```

See `docs/SHINY_DEPLOYMENT.md` for full details.
