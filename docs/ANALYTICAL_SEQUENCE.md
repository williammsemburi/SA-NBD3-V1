# Analytical sequence

## 1. Clean and aggregate registered deaths

The pipeline verifies ICD-10 coding, applies the NBD classification and plausibility rules, performs documented corrections and exclusions, and collapses the record-level data to the analytical grid. Unknown sex, age, and population group are redistributed while preserving represented deaths.

## 2. Construct the completed mortality envelope

African natural deaths are adjusted using the established S1, S2, and NPR inputs. The deterministic point envelope is used by all downstream stages. In uncertainty draws, the reporting scale is estimated separately for each province from the death-weighted variation in annual aggregate log S2 after sex and age are collapsed.

## 3. Calibrate total injury mortality

IMS 2009 and FAMHIS 2017 supply empirical national injury totals and province–sex–age profiles.

For survey year $y$:

$$
K_y=\frac{S_y}{V_y},
$$

where $S_y$ is the weighted survey injury total and $V_y$ is the completed routine injury total. The IMS ratio is held for 1997–2009, log ratios are interpolated for 2010–2016, and the FAMHIS ratio is held for 2017–2019.

The survey and routine province–sex–age totals are also normalised and compared:

$$
R_{psay}=\frac{q^S_{psay}}{q^V_{psay}}.
$$

One bounded annual intercept makes the adjusted demographic cells sum to the national survey-calibrated target. Injury and natural deaths remain complementary parts of every completed province × sex × population-group × age × year total.

## 4. Estimate HIV/AIDS mortality

The HIV/AIDS model is fitted and applied after injury calibration because the injury adjustment changes the residual natural-cause envelope. Misclassified HIV/AIDS deaths are identified from the statistical model and transferred from the approved donor causes while preserving each demographic-cell total.

## 5. Estimate specific injury causes

Total injury mortality is divided among 15 causes. NIMS 2000 supplies the national 2000 cause composition; IMS 2009 supplies the spatial distribution used to expand NIMS and its own 2009 composition; FAMHIS supplies the 2017 composition.

Only well-specified survey mechanisms enter the cause-fraction denominator. Generic and unresolved injuries remain in the total injury envelope.

Causes 132 and 138 use the documented NIMS backward harmonisation:

$$
p_{2000}^{*}=\max\{2p_{2009}-p_{2017},10^{-5}\}.
$$

Cause 136 uses the documented FAMHIS forward harmonisation:

$$
p_{2017}^{*}=\max\{2p_{2009}-p_{2000},10^{-5}\}.
$$

The corrected composition is renormalised. Broad and within-group additive log ratios are interpolated between 2000, 2009, and 2017, held flat outside the survey range, smoothed with the five-year triangular moving average, and transformed back to positive fractions summing to one.

## 6. Redistribute ill-defined and garbage causes

The deterministic point analysis applies the full ordered expert rules. For a rule with source $s$, approved targets $a_1,\ldots,a_J$, current target counts $D_j$, and multipliers $W_j$, the source allocation is:

$$
A_j=s\frac{D_jW_j}{\sum_kD_kW_k}.
$$

The point estimate uses $W_j=1$. In uncertainty draws, all approved targets remain active and a continuous positive mean-one multiplier vector is drawn once per rule and shared across demographic cells.

## 7. YLLs and final database

After redistribution, analysis causes are mapped to the South African NBD hierarchy. The pipeline constructs deaths, YLLs, crude rates, age-standardised rates, aggregate ages, person estimates, provincial totals, national totals, and population-group estimates.
