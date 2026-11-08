# Testing Checklist: Swimmer Plot (`swimmerplot`)

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

Fifteen files under `tests/testthat/` cover `swimmerplot`:

```r
# Run all fifteen swimmerplot test suites
testthat::test_dir("tests/testthat", filter = "swimmerplot")
```

| File | What it covers |
| :--- | :--- |
| `test-swimmerplot.R` | Core swimmerplot integration tests. |
| `test-swimmerplot-basic.R` | Smoke coverage, result object creation, default option behavior, and onboarding instructions. |
| `test-swimmerplot-arguments.R` | Full option permutation matrix (`timeType`, `dateFormat`, `timeUnit`, `timeDisplay`, milestones, event markers, themes, palettes, reference lines, sorting, exports, and narrative panels). |
| `test-swimmerplot-edge-cases.R` | Missing values, invalid start/end times (`end < start`), date parsing errors, single-patient cohorts, and special characters in column names. |
| `test-swimmerplot-integration.R` | End-to-end clinical trial timeline workflows, export tables, and plot layer verification. |
| `test-swimmerplot-fixes.R` | Early numerical and plotting regression fixes. |
| `test-swimmerplot-correctness.R` | Statistical verification of duration summaries, person-time incidence rates, and response proportions. |
| `test-swimmerplot-audit-20261019.R` | Audit regressions for theme-safe HTML notices and table structures. |
| `test-swimmerplot-check-fixes.R` | Schema wiring and option validation checks. |
| `test-swimmerplot-i18n-checks.R` | Internationalization catalog and `.()` wrapper checks. |
| `test-swimmerplot-jamovify.R` | `jmvcore` helper migration and `toNumeric` safety checks. |
| `test-swimmerplot-release-review.R` | Primary-source mathematical oracle checks: Schemper & Smith (1996) reverse Kaplan-Meier median follow-up, Clopper-Pearson exact binomial CIs, log-rank and Wilcoxon duration tests, and Fisher / Chi-square response comparisons. |
| `test-swimmerplot-release-20261019.R` | Pre-release critical review regressions (2026-10-19). |
| `test-swimmerplot-release-profile-fixes.R` | Release-profile regressions for RECIST category validation, explicit `censorCoding` handling, unconfirmed multi-row response disclosures, and observed-median follow-up fallbacks. |
| `test-swimmerplot-presubmission-20261107.R` | Pre-submission review regressions (2026-11-07): patient-level BOR repeated across episodes vs per-episode response derivation, row-exclusion isolation for milestones/events/sorting, relative-mode `end < start` classification when first row has `start = NA`, all-missing `responseVar` with `groupVar`, `"Non-CR/Non-PD"` handling, and `sortOrder = "response"` ordering. |

## 3. QA Sign-Off Criteria

Status recorded 2026-11-07:

- [x] **testthat, all fifteen `swimmerplot` files**: 100% passing (0 failures, 0 errors).
- [x] **Release gate (`tools/release_gate.py --analysis swimmerplot`)**: 0 blocking findings.
- [x] **UI labels and schemas reviewed**: all 44 options in `jamovi/swimmerplot.a.yaml` have matching controls in `jamovi/swimmerplot.u.yaml` and match `jamovi/swimmerplot.r.yaml`.

