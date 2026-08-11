# Collaborator workshop guide

This guide is designed for a code walk-through of NBD3 Version 1. The repository is organised so the workshop can follow the analytical sequence rather than individual helper files.

## Suggested agenda

### 1. Study purpose and lineage

Begin with the national burden-of-disease objective: to derive consistent and coherent estimates of deaths and premature mortality by cause despite incompleteness, misclassification and invalid underlying causes. Explain the progression from the initial 2000 study, through NBD2 for 1997–2012, to the reproducible NBD3 Version 1 series for 1997–2019.

### 2. The one-command workflow

Open `run_nbd.R`. Review the five controls and the three main phases:

- point-estimate analysis;
- joint uncertainty; and
- results report.

### 3. The deterministic sequence

Open `_targets.R`. Walk through Stages 01–08 in order. Emphasise that `{targets}` provides caching and restartability but does not change the statistical sequence.

### 4. Module walk-through

| Module | Workshop topic |
|---|---|
| `R/00_core.R` | Configuration, paths, assertions, shared labels and efficient aggregation |
| `R/01_population_cod.R` | Population denominators, ICD verification, aggregation and unknown demographic redistribution |
| `R/02_injuries.R` | NIMS, IMS, FAMHIS, compositional interpolation and smoothing |
| `R/03_completeness_hiv.R` | Under-reporting, NPR and HIV/AIDS reallocation |
| `R/04_redistribution.R` | The `redist()` helper and ordered expert rules |
| `R/05_yll_database.R` | Cause hierarchy, YLLs, rates and final database |
| `R/06_uncertainty.R` | Joint stochastic operations and draw outputs |
| `R/07_reporting.R` | Mapping joint draws to report intervals |
| `R/08_pipeline.R` | Thin stage wrappers used by `_targets.R` |

Each module has section dividers that identify the major analytical components and function groups.

### 5. Uncertainty

Use `docs/UNCERTAINTY.md` and `config/uncertainty_joint.yml`. Focus on what is stochastic, what remains fixed, and why cause reallocations preserve totals while producing covariance between causes.

### 6. Results

Launch the HTML report and demonstrate:

- HIV/AIDS model comparison;
- injury model comparison;
- final provincial estimates;
- final population-group estimates; and
- the uncertainty methods section.

## Practical exercises

1. Rebuild the report without rerunning analysis or uncertainty.
2. Trace one injury cause from its survey inputs to the annual smoothed fraction.
3. Trace one HIV-indicator cause through the Stage 05 donor-to-HIV accounting identity.
4. Select a multi-target garbage rule and follow its ordered proportional redistribution.
5. Inspect one final cause across the point database and the corresponding uncertainty draws.

## Extended validation

The full development validation suite is retained under `dev/full_validation.R`. It is intentionally outside the normal workshop path. Run it only when reviewing a release, modifying analytical logic, or diagnosing a failed invariant.
