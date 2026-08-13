# NIMS draw and injury-harmonisation order correction

## Failure addressed

The joint uncertainty run stopped after draw 1 with:

```text
Borrowed injury survey compositions are inconsistent within a shared donor group.
```

The Stage 03 point estimates and the IMS/FAMHIS injury-level calibration had already completed correctly.

## Cause

The uncertainty code was applying the cross-survey corrections for causes 132, 136 and 138 before sampling the NIMS 2000 composition. Those corrections use the sampled IMS 2009 and FAMHIS 2017 fractions in each fine province-population-group-sex-age cell. Consequently, cells that shared the same raw borrowed NIMS donor could legitimately have different final harmonised NIMS anchors. The subsequent donor-level sampler incorrectly required those already-harmonised compositions to be identical.

## Correction

The corrected order is:

1. reconstruct the national NIMS sex-age cause counts from the saved expanded NIMS panel;
2. draw the national NIMS composition from its count information;
3. expand the sampled NIMS composition geographically using the IMS profile from the same PSU-bootstrap replicate;
4. combine it with the sampled IMS and FAMHIS cause counts;
5. apply the documented harmonisation for causes 132, 136 and 138;
6. run the deterministic hierarchical ALR interpolation and triangular smoother.

This preserves one common raw NIMS draw while allowing the harmonised fine-cell anchors to differ legitimately.

## Statistical effects

The correction does not change:

- the deterministic 1997-2019 point estimates;
- the empirical IMS 2009 or FAMHIS 2017 injury totals;
- the survey eligibility rules or weights;
- the injury completeness trajectory;
- the harmonisation formulas for causes 132, 136 and 138;
- completeness uncertainty;
- HIV coefficient-covariance uncertainty; or
- continuous weighted redistribution uncertainty.

It improves the uncertainty dependence structure because the same sampled IMS profile now also determines the spatial expansion of the sampled national NIMS composition.

## Files replaced

```text
R/02_injuries.R
R/06_uncertainty.R
tests/testthat/test-injury-model.R
tests/testthat/test-structured-uncertainty.R
docs/UNCERTAINTY.md
```

## Installation

Extract the contents of the ZIP directly into:

```text
C:\Users\williamms\SA-NBD3-V1
```

Allow Windows to replace the matching files. Keep all raw data, derived checkpoints, the `_targets` directory, the deterministic database and report inputs.

## Rerun

The deterministic point estimates completed and do not need to run again. Use:

```r
RUN_POINT_ESTIMATES <- FALSE
RUN_UNCERTAINTY     <- TRUE
RUN_REPORT          <- TRUE
OPEN_REPORT         <- TRUE
RUN_FULL_VALIDATION <- FALSE

OVERWRITE_UNCERTAINTY <- TRUE
```

Then run:

```bat
cd /d "C:\Users\williamms\SA-NBD3-V1"
Rscript run_nbd.R
```

Use `OVERWRITE_UNCERTAINTY <- TRUE` once because the uncertainty code signature has changed. Return it to `FALSE` after a successful run.

For a short smoke test, first use a unique output name and `n_draws: 2` in `config/uncertainty_joint.yml`. The point-estimate pipeline still remains off.
