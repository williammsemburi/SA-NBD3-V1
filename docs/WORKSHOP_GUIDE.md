# Collaborator workshop guide

## 1. Study objective

Introduce the NBD objective: derive coherent mortality estimates despite under-registration, misclassification, incomplete injury information, and invalid underlying causes. Locate Version 1 within the progression from the initial 2000 study and NBD2 to the reproducible 1997–2019 R pipeline.

## 2. Walk through the six scientific operations

1. Clean and aggregate registered deaths.
2. Complete African natural mortality.
3. Calibrate total injuries from IMS 2009 and FAMHIS 2017.
4. Re-estimate HIV/AIDS from the revised natural envelope.
5. Divide injuries among 15 causes using NIMS, IMS, and FAMHIS.
6. Redistribute ill-defined and garbage causes.

## 3. Explain the numbered modules

| Module | Workshop focus |
|---|---|
| `00_core.R` | Configuration, constants, assertions, and common helpers |
| `01_population_cod.R` | Population, ICD verification, aggregation, and unknown demographics |
| `02_injuries.R` | Survey eligibility, PSU bootstrap, injury calibration, harmonised cause fractions |
| `03_completeness_hiv.R` | Completeness, NPR, prevalence, and HIV/AIDS |
| `04_redistribution.R` | Proportional redistribution and continuous target-weight uncertainty |
| `05_yll_database.R` | YLLs and final database |
| `06_uncertainty.R` | Joint draw sequence and diagnostics |
| `07_reporting.R` | Within-draw mapping and interval construction |
| `08_pipeline.R` | Stage wrappers and checkpoint outputs |

## 4. Injury exercise

Show that all published-compatible IMS/FAMHIS injury records determine the envelope, while only well-specified causes determine fractions. Review the external-reference audit and the survey bootstrap tables. Trace causes 132, 136, and 138 through the harmonisation formulas and annual interpolation.

## 5. Redistribution exercise

For a three-target rule, begin with the point vector $(1,1,1)$. Draw several continuous positive vectors, normalise them to mean one, and apply them to the same target counts. Demonstrate that all targets remain eligible, source deaths are conserved, and the allocation changes smoothly rather than by exact on/off switching.

## 6. Uncertainty exercise

Trace one complete draw through:

- province completeness;
- IMS/FAMHIS PSU replicates;
- injury level, profile, and causes;
- HIV coefficient covariance;
- redistribution target weights; and
- final cause aggregation.

Use the draw diagnostics to distinguish statistical uncertainty from Monte Carlo error and numerical repairs.
