# Contributing

Analytical changes should be small, reviewable and traceable.

1. Create a focused Git branch.
2. Modify the relevant numbered module rather than adding a new helper file unless a genuinely new analytical domain is introduced.
3. Preserve the Stage 01–08 interface and canonical checkpoint names.
4. Add or update unit tests for statistical rules, mappings, redistribution logic and output invariants.
5. Run the affected target, the unit tests and `dev/full_validation.R` before requesting review.
6. Keep raw data and generated outputs out of Git.
7. Document any new input, lookup or uncertainty assumption in the corresponding file under `docs/`.

The cause hierarchy and expert redistribution targets require substantive review; they should not be changed as routine refactoring.
