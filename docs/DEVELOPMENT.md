# Development, testing and release checks

## Unit tests

Run:

```r
source("tests/testthat.R")
```

The tests cover ICD rules, population preparation, injury data and interpolation, completeness, HIV/AIDS, redistribution, the final database, uncertainty, report data and report runtime behaviour.

## Full validation

The extended stage-by-stage validation used during model development is retained as:

```r
source("dev/full_validation.R")
```

It writes `output/tables/pipeline_validation_summary.csv`. The normal collaborator run leaves this suite off because analytical functions and `{targets}` already fail immediately on violated invariants.

## Git workflow

- Never commit files under `data/raw/`, `data/derived/`, `_targets/` or `output/`.
- Create a branch for each analytical change.
- Add or update a focused unit test when changing a statistical or redistribution rule.
- Run the affected stage, the unit tests and the full validation before merging analytical changes.
- Keep lookup-table changes separate from code changes and document their source.
- Do not change uncertainty distributions solely to obtain narrower or externally similar intervals.

## Release checklist

1. Confirm all required inputs and lookup tables.
2. Run the point-estimate pipeline from a clean target store.
3. Run the full validation suite.
4. Run the configured joint draws and inspect convergence and completeness-support outputs.
5. Build and review the collaborator report.
6. Record the Git commit and configuration files used for the release.
