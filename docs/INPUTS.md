# Input files

All restricted source files belong in `data/raw/`. The supplied model-comparison file belongs in `report/data/legacy/`. Lookup tables required by the analysis are already bundled in `data/lookups/`.

Filenames can be changed in `config/config.yml`, but no R function needs to be edited when only a filename changes.

## Raw inputs

| Configuration key | Default filename | Requirement | Used in |
|---|---|---|---|
| `cod_raw` | `COD1997-2019_F1.dta` | Required | Cause-of-death cleaning |
| `population_current` | `NewPropPopulation.dta` | One population source is required | Population denominators |
| `population_workbook` | `New AltMYR.xls` | Fallback when the current population file is absent | Population denominators |
| `nims_2000` | `NIMS2.xlsx` | Required | Injury estimation |
| `ims_2009` | `IMS raw data for completeness.dta` | Required | Injury estimation |
| `famhis_2017` | `FAMHIS_FinalLabelled_Updated&Weighted_04January2023_NBD.dta` | Required | Injury estimation |
| `completeness_child` | `Completeness25April2014.csv` | Required | Completeness adjustment |
| `completeness_province` | `Completeness and Province Deaths 27012014.xlsx` | Required | Completeness adjustment |
| `npr` | `NPR15_22.dta` | Required | Completeness adjustment |
| `investigation_parameters` | `Investigation Parameters.dta` | Optional | Additional national investigation fields |
| `prevalence_raw` | `ProvPrevs.csv` | Required | HIV/AIDS estimation |
| `yll` | `NBD YLL.xlsx` | Required | Years of life lost |
| `asr_factors` | `ASRFactor.dta` | Required | Age-standardised rates |

The input audit accepts either `NewPropPopulation.dta` or `New AltMYR.xls`. When `NewPropPopulation.dta` is present, it is used in preference to the workbook.

## Population contract

The preferred population file must be identifiable as the following canonical fields, using the aliases supported by `R/01_population_cod.R`:

```text
Popgroup   population group, codes 1-4
Sex        sex, codes 1-2
DeathYear  calendar year
age5       denominator age code, 2-20
Death_Prov province code, 1-9
Pop        finite, non-negative population
```

The file may contain years outside 1997–2019. Stage 01 retains only the configured analysis years. The final database creates the structural-zero neonatal denominator separately.

The workbook fallback uses the established male and female population blocks on annual sheets and is suitable only when it covers the full configured analysis period.

## Cause-of-death source

`COD1997-2019_F1.dta` must contain the death year and month, province, sex, population group, age and age unit, underlying cause, death type, and the certificate fields required by the ICD verification functions. The reader imports the configured source aliases and stops when required content is absent.

## Injury inputs

### NIMS 2000

The original wide workbook contains one injury cause code and male/female age columns. A supported long equivalent can instead contain:

```text
nbdcode
Sex
source_age
Inj2000
```

NIMS is national. It is allocated to province and population group using IMS 2009 cause-specific shares within sex, age, and cause. The expanded cells must sum exactly to the national NIMS totals. An equal 1/36 split is used only when the corresponding IMS distribution is zero.

### IMS 2009

The source must contain fields identifiable as survey weight, province, cause type, age or NBD age, injury group, population group, and sex. The reader retains non-natural injury records, maps the 15 final injury causes, resolves unknown demographic categories, and completes the fine-stratum grid.

### FAMHIS 2017

The source must contain fields identifiable as province, sex, population group, injury mechanism, age, age unit, and weight. Generic transport, self-harm, interpersonal, and unintentional indicators are used when present. Positive weighted mass in an unmapped mechanism is a strict error.

## Completeness inputs

`Completeness25April2014.csv` supplies child completeness by province, age, and year.

`Completeness and Province Deaths 27012014.xlsx` must contain the province worksheets used by the analysis and adjusted deaths by sex, age, and year.

`NPR15_22.dta` supplies recent National Population Register deaths by province, sex, year, age, and natural/injury type.

`Investigation Parameters.dta` is optional. When present, its supported fields are appended to the national investigation dataset.

## HIV prevalence input

`ProvPrevs.csv` must provide province antenatal prevalence by source year and age in a form accepted by `R/03_completeness_hiv.R`. The analysis reproduces the configured lag construction, combines prevalence with Stage 01 population, creates national and both-sex cells, and applies the configured post-2009 freeze.

A pre-generated prevalence-population file is not required.

## YLL and ASR inputs

`NBD YLL.xlsx` must contain remaining life expectancy by age and sex for the undiscounted, 3% discounted, and 1.5% discounted schedules.

`ASRFactor.dta` must contain the age codes and standard-population weights required by the final database builder.

## Bundled lookup tables

The following source-derived lookup tables are included:

```text
data/lookups/icd10_to_nbd.csv
data/lookups/nbd_rule_manifest.csv
data/lookups/analysis_codes.csv
data/lookups/analysis_to_za.csv
data/lookups/za_codes.csv
data/lookups/lookup_build_diagnostics.csv
```

The pipeline stops on a missing or invalid required mapping. These files should not be edited casually because they define the ICD-to-analysis mapping and the many-to-many analysis-to-ZA reporting hierarchy.

## Comparison input

Place:

```text
viz.input.Rda
```

at:

```text
report/data/legacy/viz.input.Rda
```

The report-data reader expects these objects:

```text
compare.hiv
compare.injury
provincial_deaths_long
popgroup_deaths_long
```

The comparison objects supply NBD2, NBD3-Stata, THEMBISA, and GBD2023 series where available. They are loaded into an isolated environment; embedded functions are not executed.

## Input audit

The first phase of `run_nbd.R` calls the production input contract and writes:

```text
output/tables/input_status.csv
```

The audit checks file presence and lookup schemas. Stage-specific readers then enforce source columns, code ranges, keys, coverage, and conservation rules.
