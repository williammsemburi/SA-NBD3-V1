# Fast publication deployment on shinyapps.io

The analytical uncertainty archive and the public Shiny application have
different storage requirements.

The analytical repository retains one complete file per uncertainty draw. That
layout is appropriate for validation, re-analysis, and scientific archiving. It
is not an efficient live-query layout because a first user selection would
otherwise require the application to inspect every draw file.

The publication application therefore uses a two-stage workflow:

```text
Complete joint uncertainty draws
        ↓
Offline deployment-cache builder
        ↓
Partitioned interval summaries
        ↓
Minimal shinyapps.io bundle
        ↓
Fast interactive report
```

## 1. Build the deployment uncertainty cache

After the full-grid uncertainty run and report inputs are complete, run:

```bat
Rscript dev\build_shiny_ui_cache.R
```

The builder reads the complete joint draws offline and writes:

```text
output/report-data/ui_uncertainty_cache/
├── province/
│   └── sex_code=<1|2|3>/age_id=<age>/part-0.parquet
├── population_group/
│   └── sex_code=<1|2|3>/age_id=<age>/part-0.parquet
├── model_comparison_uncertainty.parquet
├── methods/
├── build_summary.csv
└── manifest.yml
```

The cache contains deterministic point estimates and exact type-8 uncertainty
summaries for every supported report cause, geography, sex, age, year, and
metric. Crude-rate limits are derived from death limits and the fixed population
denominator. ASR limits are calculated inside every draw before summarisation.

The build is restartable. Temporary binary files are written under:

```text
output/report-data/.ui_uncertainty_work/
```

The builder processes one geography scope and sex at a time, so it does not
load the full uncertainty archive into memory. Approximately 12 GB of temporary
disk space should be available for a 1,000-draw build. Temporary files are
removed after successful completion unless:

```text
NBD3_UI_CACHE_KEEP_WORK=true
```

Useful controls are:

```text
NBD3_UI_CACHE_OVERWRITE=true
NBD3_UI_CACHE_KEEP_WORK=true
NBD3_UI_CACHE_BLOCK_ROWS=5000
```

## 2. Test the cached report locally

Launch the completed cached report directly, without rebuilding report inputs:

```bat
Rscript dev\run_cached_report.R
```

A conventional report-only `run_nbd.R` execution remains available when the
report-data inputs themselves need to be rebuilt.

The report uses the cache automatically when `manifest.yml` is present. Raw
uncertainty querying is disabled by default. It can be enabled only for local
debugging with:

```text
NBD3_ALLOW_RAW_UI_DRAWS=true
```

Do not use that fallback on shinyapps.io.

## Optional local benchmark

After building the cache, measure cold and warm query latency with:

```bat
Rscript dev\benchmark_shiny_cache.R
```

The benchmark writes:

```text
output/tables/shiny_cache_benchmark.csv
```

It exercises national, provincial, population-group, crude-rate, and ASR
queries. The first iteration measures a cold partition read; later iterations
measure the in-process partition cache.

## 3. Prepare the minimal deployment bundle

Run:

```bat
Rscript dev\prepare_shinyapps_bundle.R
```

The resulting bundle is:

```text
deployment/shinyapps/
```

It includes only:

- report code and web assets;
- project configuration and lookups required by the report;
- point-estimate report data;
- the partitioned uncertainty summary cache; and
- small method/audit tables displayed by the report.

It excludes:

- restricted raw data;
- deterministic analytical checkpoints;
- the `{targets}` store;
- the final analytical database;
- raw uncertainty draw files;
- diagnostics not used by the public report; and
- development and test directories.

The script writes a deployment manifest and stops when the uncompressed bundle
exceeds `NBD3_DEPLOY_MAX_GB`, which defaults to 0.95 GB. The threshold can be
changed to match the shinyapps.io plan:

```text
NBD3_DEPLOY_MAX_GB=4.8
```

## 4. Deploy

Configure shinyapps.io authentication once using `rsconnect::setAccountInfo()`.
Then set, where applicable:

```text
NBD3_SHINY_APP_NAME=sa-nbd3-results
NBD3_SHINY_ACCOUNT=<account-name>
```

Deploy with:

```bat
Rscript dev\deploy_shinyapps.R
```

The primary document is:

```text
report/nbd3_results_report.Rmd
```

## Runtime performance features

The live report is designed to avoid analytical computation:

1. **Partition pruning.** Sex and age are encoded in the cache directory path,
   so Arrow opens only the relevant Parquet partition.
2. **Small result slices.** One selection normally returns only a few dozen or
   a few hundred annual rows.
3. **App-level memoisation.** `shiny::bindCache()` lets sessions on the same R
   worker reuse previous selections.
4. **Input debouncing.** Expensive queries wait briefly for linked controls and
   year sliders to settle.
5. **Precomputed comparison intervals.** HIV/AIDS and injury comparison panels
   do not inspect the full uncertainty archive.
6. **Progressive visual feedback.** Charts fade smoothly while a new selection
   is being retrieved.
7. **Short chart animations.** Initial series animations are retained but kept
   short enough not to obscure responsiveness.

The application cache is intentionally small and in memory. It is an
acceleration layer, not a data store. The query-optimised Parquet cache is the
persistent deployment data included in the application bundle.

## Memory and worker settings

The live application should not load the 1,000 full-grid draws into RAM. Each
worker reads only selected cache partitions. Start with one worker per instance
when assessing memory use. Increase concurrency only after observing real
memory and latency behaviour.

The report cache size can be controlled with:

```text
NBD3_REPORT_CACHE_MB=96
```

The default is 96 MB per R worker. The separate partition cache defaults to 64 MB and can be changed with `NBD3_PARTITION_CACHE_MB`. The default Person/all-age and Person/ASR partitions are prewarmed at application startup; set `NBD3_PREWARM_UI_CACHE=false` only for diagnostics.

## Rebuilding after an analytical update

When point estimates, uncertainty draws, cause mappings, age definitions, or
report metrics change:

1. rebuild the analytical report inputs;
2. rebuild `ui_uncertainty_cache`;
3. test the report locally;
4. rebuild `deployment/shinyapps`; and
5. redeploy.

Never mix a deployment cache with a different uncertainty run or Git release.
The cache manifest records the uncertainty output name, draw count, and build
time for audit purposes.
