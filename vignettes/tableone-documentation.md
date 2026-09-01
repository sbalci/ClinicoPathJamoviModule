# Table One — developer documentation

Updated 2026-08-31 against the options, result schema and backend.

## Scope

`ClinicoPath::tableone()` describes one overall cohort. It does not stratify,
compare groups, fit models, impute values, produce plots, or calculate p-values,
confidence intervals or standardized mean differences. Each input row is one
case; repeated records are not deduplicated. The jamovi menu is
ExplorationT → ClinicoPath Descriptives → Table One.

## Options

| Option | Default | Meaning |
| --- | --- | --- |
| `data` | Required in R | Data frame; supplied by the dataset in jamovi |
| `vars` | `NULL` | Selected column names; an empty selection shows instructions |
| `sty` | `"t1"` | One of the four engines below |
| `excl` | `FALSE` | Listwise deletion over eligible variables only |
| `showSummary` | `FALSE` | Source missingness, analyzed N and exclusions |
| `showAbout` | `FALSE` | Scope, supported types and denominator guidance |
| `showReportSentence` | `FALSE` | Editable report text with case/row counts |
| `nonnormal` | `FALSE` | Median (Q1, Q3) in t1 only; disabled for other styles in the UI |

## Engines and denominators

| Style | Numeric summaries | Categorical summaries | Missingness |
| --- | --- | --- | --- |
| t1: tableone | Mean (SD), or median (Q1, Q3) when requested | N (% recorded); second level only for binary factors | Missing percentage column |
| t2: gtsummary | Median (Q1, Q3); fewer than 10 distinct numeric values may be categorical | N (% recorded); dichotomous variables may use one row | Unknown count row |
| t3: arsenal | Mean (SD) and range | N (% recorded) | N-Miss count row |
| t4: janitor | Not tabulated | Factors, ordered factors, text and logicals with at most 20 recorded categories | NA row; Percent over all rows, Valid Percent over recorded rows |

The styles retain their engines' quantile definitions and display precision;
they are not guaranteed to print identical quartiles. With the verified
tableone 0.13.2 and gtsummary 2.5.1 versions, t1 uses R's type-7 quartiles
and t2 uses type 2. For `c(1:19, 100)`, t1's median display is
`10.50 [5.75, 15.25]`; t2's underlying quartiles are 5.5 and 15.5 and its
default integer display is `11 (6, 16)`. Both summarize the same observations.
These definitions are described in the [R quantile reference](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html)
and [gtsummary statistic reference](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).
Package upgrades require rerunning the numeric regression cases.

Supported storage classes are numeric/integer, factor/ordered, character and
logical. All-missing variables and unsupported R classes (including Date,
date-time and difftime) are named in an omission notice **before** exclusion.
Convert dates to explicitly defined measurements or categories yourself; the
analysis does not silently interpret dates as a number of days. Numeric category
codes must be made nominal/ordinal in jamovi or converted with `factor()` in R.

Selected matrix, array and list columns are rejected before framework selection
can flatten them. Actual NA factor levels (for example from `addNA()`) become
missing values before eligibility or exclusion; literal text levels `"NA"` and
`"Unknown"` remain categories. Janitor renders only observed levels, so unused
levels cannot bypass its 20-recorded-category guard. Single-row gtsummary results
name the counted level (`flag = TRUE`, `status = yes`, or `code = 1`).

Summary labels cannot hide a real category: if needed, t2's Unknown or t3's
N-Miss row becomes `Missing (NA)`, and janitor distinguishes a literal `NA`
category from its missing row. If a real category is `Total`, janitor uses
`Total (all cases)` for the grand total. A numeric suffix is added if the
fallback label itself is already a category. This works with factors,
ordered factors and text; only presentation labels change, not input values,
counts, ordering or percentage denominators. About explains these exceptions.

With `excl = FALSE`, all eligible source rows remain and each variable can have
a different recorded denominator. With `excl = TRUE`, rows missing any eligible
variable are excluded and every retained variable uses the same complete-case
cohort. Omitted variables cannot remove rows or enter the summary/report.

## Execution and lifecycle

1. Public `init()` rejects non-scalar selected R columns and works around jmvcore's zero-column selection failure only for
   the no-selection R path, preserving the original frame for onboarding.
2. `.run()` clears all prior content without changing declarative visibility.
3. `.prepareVariables()` resolves eligibility; `.prepareCohort()` captures source
   completeness and applies optional listwise deletion. No-data/no-variable/
   no-case paths return an explanation without leftover tables or reports.
4. `.renderTable()` invokes the engine with checkpoints. Reports are populated
   only after rendering succeeds. Partial janitor failures remain visible, but
   supplementary summaries and report text are withheld.
5. `.populateReports()` passes source missingness and final N to the summary,
   report and data-quality helpers.

`todo` and `assumptions` precede the tables. Strong warnings and recommendations
use different theme-safe styling. Thresholds (N < 10 / < 30, incomplete rows >
50% / > 20%, excluded rows > 30% / > 10%) are descriptive screening heuristics,
not validated clinical cutoffs. Decisions use unrounded counts.

`reportSentence`, `summary` and `assumptions` invalidate on `vars`, `excl` and
`sty`; `assumptions` also invalidates on `nonnormal`. Explicit clearing remains
necessary because restored Html bodies survive framework invalidation.

## Rendering and maintenance

- jmvcore 2.7.38's `Html$asString()` does not strip attributes from HTML tags,
  so t2/t4 plain-text R exports retain markup. Use the HTML result or t1 for a
  clean plain-text table. This framework limitation does not establish that
  jamovi's separate clipboard/Word/PDF exporters fail; those need desktop checks.
- Authoritative files: `R/tableone.b.R` and `jamovi/tableone.{a,r,u}.yaml`.
  Regenerate `R/tableone.h.R` with the jamovi compiler and `man/tableone.Rd`
  through roxygen; do not hand-edit generated files.
- Native Notice/Notification elements are not used on the tested compiler/runtime.
  Theme-safe Html messages and actionable rejection messages provide feedback.
- Arsenal labels/levels are escaped once. `.normalizeArsenalHtml()` replaces
  renderer-generated nonbreaking-space entities and normalizes cell tags for
  text export without decoding user markup. gtsummary/janitor escape at rendering.
- Formulas use `jmvcore::constructFormula()` and `asFormula()`. Syntax export uses
  `encodeString()` for names, preserving quotes, braces and backticks.
- Backend prose, including educational panels, errors and summaries, has
  translation hooks. Named placeholders are substituted without rescanning
  braces/backslashes in user values. See the focused TR translation plan for
  catalog coverage and remaining desktop-language validation.
- See `testing_tableone.md` for checks. A focused source test or isolated
  test-package install is not full-module or actual jamovi GUI release sign-off.
