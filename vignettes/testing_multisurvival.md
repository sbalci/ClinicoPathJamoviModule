# Testing Checklist: Multivariable Survival Analysis (`multisurvival`)

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Data Conditions | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Default options execution | Standard valid dataset | Clean table and plot outputs populated |
| **TC-02** | Missing values handling | Dataset with 5-15% NAs | Proper omission/imputation notice and valid calculations |
| **TC-03** | Single-level factor / edge case | Factor with 1 observed level | Graceful advisory notice, no fatal crash |
| **TC-04** | Empty dataset / zero rows | Dataset with 0 rows | Error notice shown, results hidden gracefully |
| **TC-05** | Special characters in variable names | Column names with spaces, hyphens, parentheses | Correctly escaped, executed without parsing errors |
| **TC-06** | Full option permutations | All non-default options enabled | All child tables and visual layers rendered accurately |
| **TC-07** | Temporary-name collisions | Predictors named `mytime`, `myoutcome`, `row_names`, or Fine-Gray temporary names | Same coefficients as renamed reference data; spreadsheet IDs preserved |
| **TC-08** | Fine-Gray nomogram disclosure | Competing-event data expanded into pseudo-rows | Original patient count; Fine-Gray label; no ordinary C-index claim |
| **TC-09** | Disease-free endpoint without deaths | AWD/AWOD mapping with qualifying recurrence times | Cox model fits the correctly coded recurrence endpoint |
| **TC-10** | Infinite follow-up | An infinite elapsed time | Actionable validation before model fitting |
| **TC-11** | Date-unit changes | Dates plus landmark, followed by output-unit change | Saved outcome and risk columns invalidate and match a fresh run |
| **TC-12** | Adjusted plot layout | Normal and narrow image sizes | Wrapped titles/subtitles with no clipped estimand text |
| **TC-13** | Regenerated R syntax | Crafted saved option values/names, quotes, backslashes and newlines | Values remain literals; undeclared names and invalid lists are omitted; payloads are never evaluated in tests |
| **TC-14** | Interaction syntax round-trip | Variable names containing quotes or backslashes; crossed terms | Export preserves the data argument and model terms; refitted coefficients and covariance agree |

## 2. Automated Test Execution

Run the dedicated test suite for `multisurvival`:

```r
devtools::load_all(quiet = TRUE)
testthat::test_dir("tests/testthat", filter = "^multisurvival")
```

## 3. QA Sign-Off Criteria

- [ ] Generated wrappers and documentation regenerated from the reviewed YAML.
- [ ] Rebuilt test module passes native save/reopen, resize and PNG/PDF export checks.
- [ ] Intended clinical use and endpoint timing reviewed for the study population.

See `quality-reports/MULTISURVIVAL_RELEASE_REVIEW_2026-10-22.md` for the dated evidence,
test outcomes and remaining release actions. Passing numerical and software checks does not
constitute clinical validation. Sparse-event fixtures intentionally exercise convergence warnings.

