# Testing Checklist: Treatment Response: Patient-Level Burden (`waterfall`)

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Data Conditions | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Default options execution | Standard valid dataset | Clean table and plot outputs populated |
| **TC-02** | Missing values handling | Dataset with 5-15% NAs | Proper omission/imputation notice and valid calculations |
| **TC-03** | Single-level factor / edge case | Factor with 1 observed level | Graceful advisory notice, no fatal crash |
| **TC-04** | Empty dataset / zero rows | Dataset with 0 rows | Error notice shown, results hidden gracefully |
| **TC-05** | Special characters in variable names | Column names with spaces, hyphens, parentheses | Correctly escaped, executed without parsing errors |
| **TC-06** | Full option permutations | All non-default options enabled | All child tables and visual layers rendered accurately |

## 2. Automated Test Execution

Seventeen files under `tests/testthat/` cover `waterfall`:

```r
# Run all seventeen waterfall test suites
testthat::test_dir("tests/testthat", filter = "^waterfall($|-)")
```

| File | What it covers |
| :--- | :--- |
| `test-waterfall.R` | Core waterfall and spider plot integration tests, baseline-only patient reconciliation, and category overrides. |
| `test-waterfall-basic.R` | Smoke coverage, result object creation, default option behavior, and onboarding instructions. |
| `test-waterfall-arguments.R` | Full option permutation matrix (`inputType`, `measurementUnit`, sorting, annotations, thresholds, median/CI, color schemes, spider plot, duration table, and narrative panels). |
| `test-waterfall-edge-cases.R` | Missing values, negative raw measurements, zero baselines, shrinkage below -100% capping, single-patient cohorts, and special characters in column names. |
| `test-waterfall-enhancements.R` | Baseline reference line, confirmation/ongoing markers, and `responseCategoryVar` overrides. |
| `test-waterfall-large-patient-ids.R` | Exact grouping of numeric patient IDs beyond 15 digits (avoiding `as.character()` scientific-notation collisions). |
| `test-waterfall-recist-validation.R` | Adapted RECIST v1.1 percentage cut-points (`-100%`, `-30%`, `+20%`), floating-point boundary tolerances, and the >= 5 mm absolute increase rule. |
| `test-waterfall-validation.R` | Input data validation, duplicate `time = 0` rejection, negative time warnings, and percentage scale diagnostics. |
| `test-waterfall-audit-fixes.R` | Audit regressions for plot state payloads, theme-safe HTML notices, and fixed-row `.init()` table skeletons. |
| `test-waterfall-check-fixes.R` | Schema wiring and option validation checks. |
| `test-waterfall-i18n-checks.R` | Internationalization catalog and `.()` wrapper checks. |
| `test-waterfall-jamovify.R` | `jmvcore` helper migration and `toNumeric` safety checks. |
| `test-waterfall-release-review.R` | Primary-source mathematical oracle checks: nadir-referenced progression, Kaplan-Meier median duration of response (`survival::survfit`), Clopper-Pearson exact binomial CIs (`stats::binom.test`), Fisher / Mann-Whitney / Kruskal-Wallis group tests, and `addResponseCategory` row alignment. |
| `test-waterfall-release-fixes.R` | Release pass regressions for time-to-response, person-time metrics, untimed assessments, repeated visit times, and group conflict resolution. |
| `test-waterfall-release-20261019.R` | Pre-release critical review regressions (2026-10-19). |
| `test-waterfall-release-profile-fixes.R` | Release-profile regressions for group palette alignment between waterfall and spider plots, copy-ready report text, and unconfirmed response disclosures. |
| `test-waterfall-presubmission-20261107.R` | Pre-submission review regressions (2026-11-07): percent-of-baseline scale guard with and without `timeVar`, per-visit `responseCategoryVar` derivation up to first PD (including `first_pd` bar cutoff and TTR/DoR propagation), `responseCategoryVar` / `groupVar` named `"response"`, unrecognized labels mixed with per-visit categories, and supplied `"NE"` handling. |

## 3. QA Sign-Off Criteria

Status recorded 2026-11-07:

- [x] **testthat, all seventeen `waterfall` files**: 100% passing (0 failures, 0 errors).
- [x] **Release gate (`tools/release_gate.py --analysis waterfall`)**: 0 blocking findings.
- [x] **UI labels and schemas reviewed**: all 40 options in `jamovi/waterfall.a.yaml` have matching controls in `jamovi/waterfall.u.yaml` and match `jamovi/waterfall.r.yaml`.

