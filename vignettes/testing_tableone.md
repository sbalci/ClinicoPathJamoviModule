# Testing Table One

## Focused automated tests

From a development environment with the module's dependencies available:

```r
devtools::test(filter = "^tableone")
```

`testthat::test_file()` alone does not load the module. For an installed package,
attach `ClinicoPath` first; internal classes are resolved from the public function's
namespace. Integration tests require an installed package.

| Scenario | Required assertion |
| --- | --- |
| All four styles | Numeric values and denominators agree with base R/engine output |
| Date/duration plus a complete measurement | Unsupported column named; no case loss or report entry caused by it |
| All-missing variable | Omitted before exclusion and named |
| t4 with Age and Sex | Numeric Age skipped; exclusion uses Sex only |
| Empty dataset / selection | Onboarding explanation; no crash or old table/report |
| Restored t1 → t4 with numeric variables only | No table and no stale supplementary output |
| Restored results followed by engine error | Prior summary and report cleared |
| Threshold boundary and just above it | Compare counts, not rounded percentages |
| HTML-like labels and unusual names | Escaped once; generated syntax parses and runs |
| Arsenal generated HTML/text export | No renderer entities or cell tags leaking into text; user markup remains safe |
| Nonnormal t1 | Correct median and quartiles |
| Engine-specific precision | t2 type-2 quartiles and rounding; t3 sample SD, range and recorded-value denominator |
| Repeated records | Counts described as cases/rows, not verified unique patients |
| Actual NA factor levels, ordered/unordered | All engines and reports agree with canonical missing values, with/without exclusion |
| Literal NA/Unknown categories | Remain recorded categories; HTML-like levels remain escaped |
| Total/NA/Unknown/N-Miss category names | Unique summary-row labels; no janitor factor-level error; counts unchanged |
| File-backed framework save/load | All four styles restore results; changed/filtered cohorts replace saved counts |
| Matrix/array/list columns | Reject before flattening; unselected columns do not affect valid analyses |
| Many unused factor levels | Janitor displays observed levels only; recorded-category limit still applies |
| Dichotomous variables | gtsummary explicitly labels the counted TRUE/1/yes level |
| Compiled Turkish catalog | All backend markers translated; placeholders, counts and user labels preserved |

Detailed regressions are in `test-tableone-audit-fixes.R` and
`test-tableone-review-fixes.R` and `test-tableone-label-collisions.R`. Existing argument,
basic, edge, release-review and integration files cover surrounding behavior.
Protobuf tests require `RProtoBuf` and exercise actual result restoration.
Report skips rather than counting them as passes.

## Release checks requiring the full application/module

- Compile analysis/results schemas and UI; regenerate derived help.
- Run installed-package integration tests against intended release dependencies.
- Run full-module `R CMD check` in the release environment.
- In jamovi, switch styles with summary/report enabled, including no-table paths.
- Save/reopen `.omv` files and repeat style, exclusion and variable changes.
- Inspect light/dark themes, clipboard/plain-text/HTML export, and table layout.
- Verify supported locale catalogs; hooks are not completed translations.

This is a checklist, not a blanket QA sign-off. Dated audit/remediation reports
in `quality-reports/` record checks actually executed and their limits.
