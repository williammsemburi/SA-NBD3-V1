# Methods

## Purpose

NBD3 Version 1 derives internally coherent mortality estimates for South Africa from 1997 to 2019. The analysis addresses under-registration, HIV/AIDS misclassification, incomplete injury information, and invalid or ill-defined underlying causes.

## Population and registered mortality

Population denominators are harmonised to the analytical province, population-group, sex, age, and year grid. Registered death records undergo ICD verification, NBD classification, plausibility rules, targeted corrections, exclusions, and aggregation. Unknown demographic categories are redistributed conservatively.

## African natural-cause completeness

The deterministic point estimate applies the established all-cause completeness and NPR adjustments. The uncertainty scale is estimated separately within each province from annual aggregate S2 values across time after sex and age are collapsed using implied pre-adjustment African natural deaths as weights.

## Total injury mortality

IMS 2009 and FAMHIS 2017 are used directly from their supplied analytic records and weights.

- IMS level eligibility: `Cause_of_death == 1`.
- FAMHIS level eligibility: nonblank injury mechanism and not explicitly excluded by `non_nat_undert == 0`.

All eligible records contribute to the injury level and profile. The published estimates are audit references only.

The national survey-to-routine level ratio and the normalised survey-to-routine province–sex–age profile are estimated at 2009 and 2017. The IMS correction is held before and through 2009, log components are interpolated between 2009 and 2017, and the FAMHIS correction is held thereafter. A bounded calibration preserves every completed all-cause cell.

## Specific injury causes

Cause fractions use only directly mapped, well-specified injury mechanisms. Generic and unresolved survey injuries are excluded from the cause-fraction denominator but retained in the total injury envelope.

The 15 causes form a hierarchical composition: transport, other unintentional injury, self-harm, and interpersonal violence, followed by detailed causes within the first two groups. Causes 132, 136, and 138 are harmonised using the documented cross-survey formulas before interpolation. Annual fractions are generated in additive-log-ratio space and smoothed with a fixed five-year triangular moving average.

## HIV/AIDS

Stage 05 estimates background mortality and misclassified HIV/AIDS deaths using the established regression formulation. The model is rerun after completeness and injury calibration. Fitted coefficient vectors and variance-covariance matrices are retained for joint uncertainty propagation.

## Ill-defined and garbage causes

Stage 06 applies the ordered expert rules through the `redist()` helper. The deterministic point estimate allocates each source proportionally across all approved targets. Biological fallback distributions are used where current target denominators are zero. Natural, injury, and all-cause envelopes are preserved.

## YLLs and reporting

Deaths are mapped to the final South African NBD hierarchy and combined with standard YLL weights and population denominators. The report compares NBD3-R with NBD2, NBD3-Stata, THEMBISA, and GBD where equivalent series are available, and presents NBD3-R detailed cause estimates with propagated uncertainty.
