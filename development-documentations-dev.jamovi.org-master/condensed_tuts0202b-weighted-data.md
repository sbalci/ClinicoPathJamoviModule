<!-- CONDENSED SUMMARY, not a verbatim copy. Source: https://dev.jamovi.org/tutorial/tuts0202b-weighted-data (fetched 2026-10-08 via a summarising fetch tool).
     Replace with the verbatim upstream file (src/content/docs/... in jamovi/dev.jamovi.org) when convenient. -->

# Weighted Data (condensed)

`.a.yaml` header key `weightsSupport`: `auto` (default; rows replicated by integer weights before `.run()`, non-integers rounded, nrow = sum of weights), `full` (fractional weights passed through), `integerOnly` (no expansion, rounded with warning), `none` (unweighted + strong warning).
For full/integerOnly: `attr(self$data, 'jmv-weights')` (numeric or NULL), `attr(self$data, 'jmv-weights-name')`.
jamovi prepends a weights Notice; define a result named `.weights` to replace/suppress it. No minimum version stated.
