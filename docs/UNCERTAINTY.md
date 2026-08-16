# Joint uncertainty methods

## Overview

Every draw reruns the evidence-supported stochastic operations in analytical order. Intervals are empirical 2.5th and 97.5th percentiles of the final within-draw estimates. Aggregation occurs inside each draw, preserving covariance among causes, provinces, and population groups.

## 1. African natural-cause reporting completeness

For province $p$, sex and age are first collapsed within each eligible year using implied pre-adjustment African natural deaths as weights. Let $S2_{pt}$ be the resulting annual aggregate factor. The weighted standard deviation of $\log S2_{pt}$ across time defines $\sigma_p$.

Each draw uses:

$$
F_p^{(b)}=\exp\left(-\frac{1}{2}\sigma_p^2+\sigma_pZ_p^{(b)}\right),
\qquad Z_p^{(b)}\sim N(0,1).
$$

The factor is common to all African natural causes in the province. Province factors are independent because no cross-province covariance input is supplied.

## 2. Injury level, profile, and cause composition

### Survey eligibility

IMS and FAMHIS point estimates are calculated from the raw analytic files using their published-compatible criteria and supplied weights. Published national estimates and intervals are retained only as validation benchmarks.

### Stratified PSU bootstrap

IMS mortuaries and FAMHIS forensic pathology facilities are resampled with replacement within their survey strata. Singleton sampled strata are held fixed. One IMS replicate and one FAMHIS replicate are selected in each NBD draw.

Within a survey replicate, the same PSU multipliers generate:

- the national injury total;
- provincial and province–sex–age totals; and
- counts for all well-specified injury causes.

This preserves their joint survey covariance. National totals equal the sum of provincial totals within every replicate.

### Time propagation

Each replicate is compared with the current completed routine injury envelope. The IMS level/profile correction is held through 2009, log corrections are interpolated through 2017, and the FAMHIS correction is held thereafter. The bounded calibration preserves completed all-cause totals.

### Cause composition

Generic and unresolved survey injuries contribute to completeness but are excluded from cause fractions. IMS and FAMHIS cause counts come from the same PSU replicate used for level and profile. NIMS uses count-based composition uncertainty. The national NIMS sex-age composition is sampled first and is then expanded geographically using the IMS profile from that same uncertainty draw.

Causes 132, 136, and 138 are harmonised only after the NIMS count draw and IMS-based spatial expansion. This order preserves the shared raw donor draw while allowing the documented cross-survey correction to differ legitimately across fine cells. The harmonised anchors then enter hierarchical ALR interpolation and the triangular moving average. No additional inter-survey path variance is added.

## 3. HIV/AIDS model uncertainty

Each fitted Stage 05 coefficient vector is sampled jointly:

$$
\boldsymbol\beta^{(b)}\sim N\left(\widehat{\boldsymbol\beta},
\widehat{\operatorname{Var}}(\widehat{\boldsymbol\beta})\right).
$$

The complete HIV/AIDS reallocation is rerun after the completeness and injury draws. Antenatal prevalence remains fixed because no approved variance-covariance input is supplied.

## 4. Redistribution uncertainty

The point estimate uses the full approved target vector with equal multipliers. For a rule with $J>1$ targets, the uncertainty draw generates independent Gamma variables:

$$
G_j\sim\operatorname{Gamma}(\alpha_J,\alpha_J),
\qquad W_j=\frac{G_j}{\overline G}.
$$

All $W_j$ are strictly positive and have mean one after normalisation. The same vector is applied to every demographic cell governed by the rule.

The shape $\alpha_J$ is derived from the former uniform non-empty-subset model. Let $K$ be the size of a uniformly sampled non-empty subset of $J$ targets:

$$
P(K=k)=\frac{\binom{J}{k}}{2^J-1}.
$$

For equal baseline target counts, the old marginal allocation-share variance is:

$$
V_J=\frac{1}{J}E\left(\frac{1}{K}\right)-\frac{1}{J^2}.
$$

The symmetric Dirichlet/Gamma shape is chosen so that:

$$
\frac{J-1}{J^2(J\alpha_J+1)}=V_J.
$$

Thus the continuous model retains the former structural variance under a neutral equal-target reference, while removing exact target switching. Single-target rules remain deterministic.

## 5. Draw order

```text
Completed Stage 04 point boundary
    -> province-specific African natural completeness draw
    -> one joint IMS survey-design replicate
    -> one joint FAMHIS survey-design replicate
    -> injury level/profile calibration
    -> NIMS/IMS/FAMHIS cause-composition trajectory
    -> HIV/AIDS coefficient draw and Stage 05 rerun
    -> continuous redistribution target-weight draw and Stage 06 rerun
    -> final cause aggregation and reporting
```

## 6. Reporting grid and derived measures

Each completed draw retains base-age deaths for Male, Female and Person across South Africa, the nine provinces, and the four national population groups. The report maps the complete analysis-cause vector to every supported cause and aggregate within the draw.

The visualization tool derives three measures:

1. deaths for every supported report age group;
2. crude mortality rates using the fixed population denominator for the selected geography, sex, cause and age; and
3. all-age age-standardised death rates calculated inside each draw using the same standard-population weights and age-group convention as the deterministic database.

Calculating the ASR inside each draw preserves covariance among age-specific deaths. The report does not combine independently calculated age-specific interval endpoints.

YLLs remain available in the analytical database, but they are not exposed in the visualization tool and are not stored in the full-grid uncertainty reporting files.

## 7. Scalable draw storage and finalisation

The canonical uncertainty data are the individual Parquet files written for each completed draw:

```text
output/uncertainty/<output-name>/joint/draws/
output/uncertainty/<output-name>/joint/population_draws/
output/uncertainty/<output-name>/joint/full_draws/
output/uncertainty/<output-name>/joint/population_full_draws/
```

The compact `draws/` and `population_draws/` files support headline comparisons and convergence diagnostics. The wide `full_draws/` and `population_full_draws/` files retain all 20 base-age death counts for Male, Female and Person and are the source for the interactive deaths, crude-rate and ASR intervals.

The pipeline does not combine the draws into one `uncertainty_draws.parquet`. At publication-scale draw counts, that table can exceed Arrow/Parquet serialization limits and is unnecessary for either inference or reporting. After the last draw, the engine:

1. validates that every expected per-draw file exists;
2. streams the compact death vectors to temporary binary storage;
3. calculates exact type-8 quantiles, means and standard deviations in bounded row blocks;
4. calculates convergence and HIV-cause covariance diagnostics without loading all draw rows into memory;
5. writes an explicit draw-storage manifest; and
6. removes the temporary binary file after successful finalisation.

The interactive report opens the complete full-grid draw directories as Arrow datasets and retrieves only the selected causes, geographies, sexes, years and age columns. Aggregate-age deaths, crude rates and all-age ASRs are calculated from the selected joint draws, not from separately summarised marginal intervals.

## 8. Current boundaries

S1, an independent NPR variance term, population denominators, and antenatal HIV prevalence remain fixed because the supplied inputs do not define approved stochastic models for them. The stratified PSU bootstrap uses the supplied PSU and stratum variables; finite-population corrections and the exact variance settings of the original publication software are not imposed. Crude-rate and ASR intervals therefore represent mortality-estimation uncertainty conditional on the supplied population denominators.
