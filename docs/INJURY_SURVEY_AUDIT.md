# Injury survey anchor audit

This audit documents the raw-file rules used by Version 1 before the injury
model is allowed to calibrate the mortality envelope.

## Injury-level eligibility

### IMS 2009

The national injury level uses all records satisfying:

```r
Cause_of_death == 1
```

with the supplied `weight`. In the supplied analytic file this gives:

- 24,197 unweighted injury records;
- 52,493.413 weighted injury deaths; and
- 63 sampled mortuaries contributing eligible records.

The external published reference is 52,493 deaths (95% interval
46,930–58,057). The external value is retained for validation and is not
substituted into the NBD3-R point estimate.

### FAMHIS 2017

The later-cleaned injury level uses all records satisfying:

```r
trimws(nbd_cod_mech) != "" &
  (is.na(non_nat_undert) | non_nat_undert != 0)
```

with the supplied `wht`. In the supplied updated analytic file this gives:

- 29,944 unweighted injury records;
- 53,288.410 weighted injury deaths; and
- 81 sampled forensic pathology service units contributing eligible records.

The later external published reference is 53,288 deaths (95% interval
49,964–56,613). The original 2021 report estimate of 54,734
(51,376–58,093) is retained as a second audit reference.

## Cause-fraction eligibility

All records meeting the level criterion contribute to total injury
completeness. Cause fractions use only records mapping directly to one of the
15 common Version 1 injury causes. Generic, unresolved, and otherwise
non-comparable injury mechanisms are excluded from the cause-fraction
denominator, while remaining part of the injury-level estimate.

In the supplied files, well-specified causes represent:

- 50,418.356 weighted IMS deaths, or 96.047% of the IMS injury envelope; and
- 46,951.540 weighted FAMHIS deaths, or 88.108% of the FAMHIS injury envelope.

The retained well-specified fractions are renormalised to a closed
15-cause composition before harmonisation and interpolation.

## Survey uncertainty

Version 1 resamples sampled PSUs with replacement inside survey strata:

- IMS: mortuary within province × size × type;
- FAMHIS: forensic pathology service within `stratprov`.

Singleton sampled strata are held fixed. The same PSU multipliers jointly
generate the national level, provincial and sex-age profile, and
well-specified cause counts.

An independent 10,000-replicate audit using the same rules produced:

| Survey | Weighted point | Bootstrap 2.5% | Bootstrap median | Bootstrap 97.5% |
|---|---:|---:|---:|---:|
| IMS 2009 | 52,493.413 | 47,018.130 | 52,583.354 | 57,938.384 |
| FAMHIS 2017 | 53,288.410 | 49,959.992 | 53,273.745 | 56,631.955 |

These intervals are diagnostic rather than calibration targets. Exact
agreement with intervals produced by other software is not required; the point
estimates and survey-design logic are the key reproducibility checks.

## Cross-survey cause harmonisation

Before interpolation, causes 132, 136, and 138 are harmonised using the
established cross-survey formulas. The complete composition is then
renormalised. The correction changes cause composition but never changes the
survey-derived total injury envelope.
