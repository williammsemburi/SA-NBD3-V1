# Methods and analytical pipeline

## Purpose

The Third South African National Burden of Disease Study develops consistent and coherent estimates of mortality by cause, year, age, sex, province, and population group. Version 1 covers 1997–2019 and focuses on mortality and premature mortality measured through years of life lost.

The analysis addresses four principal limitations of the source mortality data: under-registration of deaths, misclassification of HIV/AIDS deaths, incomplete information on specific injury causes, and causes coded to ill-defined or invalid underlying causes.

## Stage 01 — Population denominators

The population module standardises the supplied population estimates to a complete panel by population group, sex, year, age group, and province. These denominators support crude and age-standardised rates and the population-group outputs.

Code: `R/01_population_cod.R`

## Stage 02 — Registered cause-of-death data

The cause-of-death module:

1. reads the required microdata fields;
2. standardises certificate and demographic variables;
3. applies record-level ICD-10 verification and the final NBD analysis classification;
4. applies age, sex, perinatal, cancer, and multiple-cause rules;
5. aggregates to restartable checkpoints; and
6. redistributes unknown sex, age, and population group while preserving totals.

The record-level verification checkpoint is cached separately because it is the most expensive deterministic operation.

Code: `R/01_population_cod.R`

## Stage 03 — Injury cause estimation

NIMS 2000, IMS 2009, and FAMHIS 2017 provide the injury composition inputs. The 15 final injury causes are represented as a closed hierarchical composition:

- transport injuries;
- other unintentional injuries;
- self-harm; and
- interpersonal violence.

Broad and within-group additive log ratios are linearly interpolated between the three surveys. A centred five-year triangular moving average smooths changes in slope around the survey years. The survey-year values are not inserted again after smoothing. The inverse hierarchical softmax transformation guarantees positive fractions that sum to one. These fractions divide the existing injury envelope without changing total injury deaths.

Code: `R/02_injuries.R`

## Stage 04 — Completeness and NPR adjustment

Child and adult completeness inputs are converted to the S1 and S2 scalars used by the established analysis. The scalars are applied to the appropriate demographic and cause envelopes. The post-freeze period reproduces the configured freeze-year completeness values, and the National Population Register transition is applied from its configured start year.

Code: `R/03_completeness_hiv.R`

## Stage 05 — HIV/AIDS reallocation

Antenatal prevalence is combined with the population panel. The HIV/AIDS analysis estimates counterfactual background mortality, zero-prevalence intercepts, age patterns, and the number of deaths misclassified to HIV-indicator causes. Misclassified deaths are transferred to HIV/AIDS, with residual non-HIV deaths returned to their designated destination causes. Every demographic-cell total is preserved.

The fitted coefficient vectors and variance-covariance matrices are retained for joint uncertainty propagation.

Code: `R/03_completeness_hiv.R`

## Stage 06 — Ill-defined and garbage-code redistribution

The redistribution module applies the ordered expert rules used to move deaths from invalid underlying causes to approved targets. The `redist()` helper provides proportional redistribution, while the stage-specific rule sequence handles exclusions, zero-denominator biological reference allocations, and envelope preservation.

Code: `R/04_redistribution.R`

## Stage 07 — Cause hierarchy and YLLs

Analysis causes are mapped to the South African NBD cause hierarchy using the approved many-to-many lookup. Deaths are combined with the supplied remaining-life-expectancy schedules to calculate undiscounted and discounted YLL measures.

Code: `R/05_yll_database.R`

## Stage 08 — Final database

The final database contains detailed and aggregate age groups, male, female and person estimates, provincial and national estimates, population-group estimates, deaths, YLLs, crude rates and age-standardised rates.

Code: `R/05_yll_database.R`

## Reproducible execution

`_targets.R` expresses Stages 01–08 as a dependency graph. Each stage reads canonical inputs and writes canonical outputs. Cached targets allow interrupted runs to resume without repeating completed upstream work. `run_nbd.R` is the collaborator-facing entry point and adds joint uncertainty and the interactive report after the deterministic database has been created.

## Study references

- Bradshaw D, Groenewald P, Laubscher R, et al. *Initial burden of disease estimates for South Africa, 2000.* South African Medical Journal. 2003;93:682–688.
- Msemburi W, Pillay-van Wyk V, Dorrington RE, et al. *Second national burden of disease study for South Africa: Cause-of-death profile for South Africa, 1997–2012.* South African Medical Research Council; 2016.
- Awotiwon OF, Pillay-van Wyk V, Groenewald P, et al. *SA NBD–GBD and SA NBD–WHO cause-list mappings for the second South African National Burden of Disease.* South African Medical Research Council; 2017.
