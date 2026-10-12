# Testing Checklist: Medical Decision (`decision`)

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Data Conditions | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Default options execution | Standard valid dataset | Clean table and plot outputs populated |
| **TC-02** | Missing values handling | Dataset with 5-15% NAs | Proper omission/imputation notice and valid calculations |
| **TC-03** | Single-level factor / edge case | Factor with 1 observed level | Graceful advisory notice, no fatal crash |
| **TC-04** | Empty dataset / zero rows | Dataset with 0 rows | Error notice shown, results hidden gracefully |
| **TC-05** | Special characters in variable names | Column names with spaces, hyphens, parentheses | Correctly escaped, executed without parsing errors |
| **TC-06** | Full option permutations | All non-default options enabled | All child tables and visual layers rendered accurately |
| **TC-07** | Margin emptied by an excluded level | Excluding one variable's unselected level removes every case of the other's positive or negative level | ERROR notice (no 2x2 table), shown first; no blank or `NaN` statistic |
| **TC-08** | Notice order | Worse-than-chance test plus missing rows | ERROR above every warning and note |
| **TC-09** | Coin-flip test | Youden's index near 0 on either side (e.g. -0.005 and +0.005, n = 201) | The same "No evidence that this test discriminates" warning with its 95% CI; the inverted-levels ERROR only when the whole interval is below 0 |
| **TC-10** | Excluded cases in the copy-ready paragraph | Missing results and/or an unselected level | The paragraph states how many of the total were excluded and why (STARD 2015) |

## 2. Automated Test Execution

Run the dedicated test suite for `decision`:

```r
testthat::test_file("tests/testthat/test-decision.R")
```

## 3. QA Sign-Off Criteria

- [x] 0 Failures, 0 Warnings on R CMD check / testthat.
- [x] UI labels match clinical guidance standards.
- [x] Turkish catalog re-synced (measured 2026-09-27): `jmvtools::i18nUpdate()` run; all 316 `.()` strings of `R/decision.b.R` are in `catalog.pot`, and all 424 `decision` entries of `tr.po` have a Turkish msgstr; `msgfmt -c` passes.
