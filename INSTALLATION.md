# Installation

This update changes collaborator-facing documentation and the interactive report only. It does not change any point-estimate or uncertainty calculation.

Extract the contents directly into the repository root:

```text
C:\Users\williamms\SA-NBD3-V1
```

Allow Windows to replace the matching files.

The updated files are:

```text
README.md
report/README.md
report/config/labels.yml
report/nbd3_results_report.Rmd
docs/UNCERTAINTY.md
```

The report source incorporates the supplied current report and preserves its edits to the overview and study-label explanations.

No point estimates or uncertainty draws need to be regenerated. To rebuild and open the report, use:

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- FALSE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE
```

Then run:

```bat
Rscript run_nbd.R
```
