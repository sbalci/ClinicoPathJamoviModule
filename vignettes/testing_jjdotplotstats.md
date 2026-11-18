# Testing Checklist: Horizontal Box-Violin Comparison (`jjdotplotstats`)

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Data Conditions | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Default options execution (`all Bool = FALSE`) | Standard 2- or 3-group dataset (`jjdotplotstats_test`) | Horizontal box-violin `plot`, `todo` summary, and `INFO` notice populated |
| **TC-02** | Selective missing & non-finite handling | Dataset with `NA`, `Inf`/`-Inf`, `addNA()` `<NA>` level, and missing `grvar` | Selective `(dep, group)` omission preserves unsplit `N`; `<NA>` level stripped; `Inf` excluded and disclosed separately |
| **TC-03** | Single-level factor, constant `dep`, or non-numeric `dep` | Factor with 1 observed level, constant `y`, or non-numeric `dep` | Severity-sorted `ERROR` notice shown; `plot$state$valid = FALSE`; stale `explanation` cleared |
| **TC-04** | Empty dataset / zero rows / incomplete selection | Dataset with 0 rows or unset `dep`/`group` | `ERROR` notice on 0 rows; dark-theme-safe welcome block in `todo` when variables unset |
| **TC-05** | Special characters, spaces, Unicode & `haven_labelled` | Column names with spaces, parentheses, UTF-8 accents; `haven_labelled` & `values`-attributed factors | Clean execution, preserved value-label order, valid `asSource()` syntax in `Rscript --vanilla` |
| **TC-06** | Statistical families, post-hoc modes & `.omv` state reopen | `parametric` (Welch / Games-Howell / BH-Welch), `nonparametric` (MW / KW BCa CI / Dunn), `robust` (Yuen / Wilcox `t1way` clamped `xi`), `bayes` (2- & 3+-group JZS `log_e(BF_01)`) | Exact numerical agreement with first-principles derivations; serialized `image$state` renders on reopen without `.run()` |

## 2. Automated Test Execution

Run the complete test and validation suites for `jjdotplotstats` from the repository root:

```r
# All 8 dedicated testthat suites
testthat::test_file("tests/testthat/test-jjdotplotstats.R")
testthat::test_file("tests/testthat/test-jjdotplotstats-arguments.R")
testthat::test_file("tests/testthat/test-jjdotplotstats-basic.R")
testthat::test_file("tests/testthat/test-jjdotplotstats-correctness.R")
testthat::test_file("tests/testthat/test-jjdotplotstats-edge-cases.R")
testthat::test_file("tests/testthat/test-jjdotplotstats-integration.R")
testthat::test_file("tests/testthat/test-jjdotplotstats-release-review.R")
testthat::test_file("tests/testthat/test-jjdotplotstats-validation.R")
```

```sh
# Deterministic first-principles validation & independent audit scripts
LANG=en_US.UTF-8 Rscript development-scripts/validate_jjdotplotstats.R
LANG=en_US.UTF-8 Rscript development-scripts/audit_jjdotplotstats.R
```

## 3. QA Sign-Off Criteria

- [x] `0` Failures, `0` Warnings, `0` Skips across all 8 `jjdotplotstats` `testthat` files (`498+` assertions) and `test-zzz-results-rendering-contract.R`.
- [x] `0` Blocking issues in `tools/release_gate.py`, `tools/check_state_guards.py`, and `tools/theme_safe_html.py`; `wired = 1.00, debt = 0` in `tools/promotion_screen.py`.
- [x] `jmvtools::prepare(".")` and `devtools::document(".")` complete cleanly with zero errors or roxygen warnings.
