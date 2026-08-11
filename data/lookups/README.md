# Bundled lookup tables

These files define the production mappings used by the analysis:

- `icd10_to_nbd.csv` — static ICD-10 to analysis-cause assignments;
- `nbd_rule_manifest.csv` — ordered NBD assignment rules and provenance;
- `analysis_codes.csv` — analysis-cause labels;
- `analysis_to_za.csv` — detailed and aggregate analysis-to-ZA hierarchy;
- `za_codes.csv` — ZA cause labels; and
- `lookup_build_diagnostics.csv` — lookup construction diagnostics.

Age-dependent ICD assignments and neonatal overrides are implemented in `R/01_population_cod.R` and covered by unit tests.

The analysis-to-ZA table is many-to-many: detailed causes partition retained analysis causes, while aggregate layers intentionally repeat detailed deaths. The pipeline validates mapping coverage and conservation before accepting final results.
