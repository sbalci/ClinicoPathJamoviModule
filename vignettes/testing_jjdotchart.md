# Testing Checklist: Dot Chart (Summary vs Reference Value) (`jjdotchart`)

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Data Conditions | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Default and 4-mode execution | Multi-group numeric data across `parametric`, `nonparametric`, `robust`, `bayes` | Synchronized table summaries/CIs, dynamic column header, and matching plot error bars |
| **TC-02** | Missing and infinite values handling | Dataset with `NA`, `NaN`, `+/-Inf`, and missing optional `Split By` values | Separate `NA`/`NaN` vs `Inf` counts (no imputation); `Split By` `NA`s excluded only from `plot2` |
| **TC-03** | Single-level factor & `Split By` edge cases | Overall $k = 1$ or per-panel $k_s < 2$ / $k_s < 4$ | Actionable `ERROR`/`WARNING` notices, 0 stale rows on overall $k = 1$, no uncaught `print()` crash |
| **TC-04** | Empty dataset / zero complete rows | Dataset with 0 rows or 100% `NA` outcome | `ERROR` notice shown, `summary` cleared to 0 rows |
| **TC-05** | Special characters & explicit `<NA>` factor levels | Spaces, `%`, `/`, `#`, UTF-8 symbols, unused levels, `addNA()` `<NA>` level | Special names preserved without mangling; ghost and `<NA>` levels excluded |
| **TC-06** | Zero-variance, exact-null & small-$k$ robust boundaries | Constant group summaries, $k = 5$ trimmed bootstrap, 0-tail bootstrap $p$ | Explicit `ERROR` refusal on zero variance; analytic Winsorized-$t$ fallback + `WARNING` on `[-Inf, Inf]`; $p \ge 1/(B+1)$ |
| **TC-07** | Caseload imbalance & precision heterogeneity | $\ge 3\times$ group-size ratio, opposite unweighted vs weighted sign, $\ge 5\times$ SE ratio | Prominent `WARNING` notices alerting to ecological sign reversal and studentization distortion |
| **TC-08** | Stateful R6 lifecycle & `.omv` reopen | Live `Valid -> Invalid -> Valid` row-filter transitions and `image$state` reopen | Stale `summary` rows cleared before early return; plot re-renders from `image$state` |

## 2. Automated Test Execution

Run the dedicated unit, validation, and independent audit suites for `jjdotchart`:

```r
testthat::test_file("tests/testthat/test-jjdotchart.R")
testthat::test_file("tests/testthat/test-jjdotchart-validation.R")
```

```sh
LANG=en_US.UTF-8 Rscript --vanilla development-scripts/audit_jjdotchart.R
```

## 3. QA Sign-Off Criteria

- [x] 0 Failures on `test-jjdotchart.R`, `test-jjdotchart-validation.R`, and `audit_jjdotchart.R`.
- [x] UI labels, single-paragraph `.a.yaml` descriptions, and American English spelling pass `tools/release_gate.py`.
- [x] HTML notices use theme-safe `rgba(...)` backgrounds with `color: inherit` (`tools/theme_safe_html.py`).
