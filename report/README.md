# NBD3 interactive analytical report

The report source is:

```text
report/nbd3_results_report.Rmd
```

It is a Shiny-enabled R Markdown report organised as a conventional research report:

1. study overview and report guide;
2. background and objectives;
3. data and analytical inputs;
4. methods;
5. results;
6. discussion and next analytical phases; and
7. a technical appendix.

The report reads completed point-estimate report data and a query-optimised interval cache generated from the completed joint uncertainty draws. The per-draw files remain the canonical analytical store, but they are not queried by the live publication app. The report does not re-estimate the mortality model. The offline cache builder uses the full-grid base-age draws to support exact intervals for all tool combinations of cause, geography, sex and age.

## Main report features

The methods section documents and visualises:

- record-level vital-registration preparation and cause assignment;
- redistribution of unknown sex, age, and population group;
- the residual completeness adjustment to African natural deaths;
- the NPR update to the post-2016 mortality trajectory;
- correction of deaths misclassified from HIV/AIDS;
- survey-derived injury levels, demographic profiles, and cause fractions;
- redistribution of ill-defined and garbage-coded causes; and
- joint propagation of completeness, injury, HIV, and redistribution uncertainty.

The results section contains two general interactive explorers:

- **Compare one cause across populations:** select a cause or aggregation and compare South Africa, provinces, or population groups.
- **Compare causes within one population:** select a geography or population group and display several causes or aggregations together.

Both explorers support the full report hierarchy: all causes, broad cause groups, disease categories, detailed causes, and reporting/custom aggregates.


## Measures in the visualization tool

The interactive explorers expose:

- deaths;
- crude mortality rates per 100,000; and
- all-age age-standardised death rates per 100,000.

Deaths and crude rates are available for the supported report ages. ASRs are calculated inside each joint draw from the age-specific death vector and the fixed standard-population weights. YLLs remain in the final analytical database for other analyses but are deliberately excluded from this visualization tool.

## Rebuild the report only

At the top of `run_nbd.R`, use:

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE
```

Then run from the repository root:

```bat
Rscript run_nbd.R
```

The report uses:

```text
output/database/
output/report-data/
output/uncertainty/
data/derived/
report/data/legacy/viz.input.Rda
```

## Report-specific code

The report sources:

```text
report/R/report_data.R
report/R/report_charts.R
report/config/labels.yml
report/config/cause_comparison_map.csv
report/www/report.css
```

Analytical code belongs under `R/`; report-only transformations and chart helpers belong under `report/R/` or in the report setup chunk.

## Fast deployment mode

For publication deployment, the report uses the cache produced by:

```bat
Rscript dev\build_shiny_ui_cache.R
```

The cache is stored under:

```text
output/report-data/ui_uncertainty_cache/
```

It contains exact draw-based summaries for all supported tool combinations and
is partitioned by sex and age so the live app reads only a small slice. The
report does not scan the 1,000 full-grid draw files on shinyapps.io.

The report sources:

```text
report/R/report_data.R
report/R/report_cache.R
report/R/report_charts.R
```

See `docs/SHINY_DEPLOYMENT.md` for the minimal deployment-bundle workflow.


The production runtime prewarms the default all-age Person and ASR partitions. Set `NBD3_PREWARM_UI_CACHE=false` to disable this diagnostic optimisation.
