# Testing Checklist: Diagnostic Test Meta-Analysis for Pathology (`diagnosticmeta`)

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Data Conditions | Expected Outcome | Covered By |
| :--- | :--- | :--- | :--- | :--- |
| **TC-01** | Default options execution | Standard valid dataset | Clean table and plot outputs populated | `-basic.R`, `-integration.R` |
| **TC-02** | Missing values handling | Dataset with 5-15% NAs | Affected studies excluded and the exclusion disclosed in the `notices` panel; the remaining studies are analysed | `-edge-cases.R`, `-release-fixes.R` |
| **TC-03** | Covariate with a single observed level | Constant / zero-variance covariate | Meta-regression refused with an explanatory notice, not a warning and not a crash | `-edge-cases.R`, `-release-review.R` |
| **TC-04** | Empty dataset / zero rows | Dataset with 0 rows | `.run()` returns early, the Getting Started panel is shown and nothing else is computed - note this is *not* an error notice | **not covered by an automated test** |
| **TC-05** | Special characters in variable names | Column names with spaces, hyphens, parentheses | Names used verbatim as `self$data[[...]]` keys; analysis runs | `-edge-cases.R` |
| **TC-06** | Full option permutations | All non-default options enabled | All child tables and visual layers rendered accurately | `-arguments.R`, `-integration.R` |

## 2. Automated Test Execution

Seventeen files under `tests/testthat/` cover `diagnosticmeta`:

```r
# all seventeen at once
testthat::test_dir("tests/testthat", filter = "diagnosticmeta")
```

| File | What it covers |
| :--- | :--- |
| `test-diagnosticmeta.R` | Cross-checks pooled sensitivity/specificity against `mada` using the `data-raw/non-rda/diagnostic_meta_test.csv` fixture, and checks that the optional analysis flags leave their tables unpopulated when switched off. |
| `test-diagnosticmeta-basic.R` | Smoke coverage: the function exists, runs on the minimal argument set, returns a `diagnosticmetaResults` object, produces default outputs, honours `bivariate_analysis` / `confidence_level` / `method`, runs on a small dataset, produces plots on request, and shows guidance rather than throwing when a required variable is omitted. |
| `test-diagnosticmeta-arguments.R` | Argument-combination matrix: meta-regression with continuous and categorical covariates, all four zero-cell corrections, all analysis options together, all plot options together, all colour palettes, all display options, HSROC alone, heterogeneity alone, publication bias, a large dataset, method x correction combinations, and a quality score as covariate. |
| `test-diagnosticmeta-edge-cases.R` | Missing, negative, non-integer, all-zero and duplicated rows are excluded and disclosed rather than raised. Very small samples, zero cells, perfect sensitivity, perfect specificity, extreme heterogeneity, extreme confidence levels, special characters in variable names; a single study and a constant covariate are refused with an explanation. |
| `test-diagnosticmeta-integration.R` | End-to-end workflows and package integration: agreement with `mada` and `metafor`, run-to-run consistency, basic meta-analysis into meta-regression, the publication-bias workflow, model comparison, CSV- and Excel-shaped imports, reproducible plots, a comprehensive all-options run, consistency across data structures, and sensitivity analysis across corrections. |
| `test-diagnosticmeta-critical-fixes.R` | Numerical regressions: SROC pooled point not passed through `plogis()` twice, Deeks' test uses effective sample size rather than an arithmetic total, I-squared is dimension-specific, ESS arithmetic, minimum study count enforcement, zero-cell handling, pooled estimates inside the valid probability range, meta-regression with a continuous covariate, individual-study table population, colour palettes, and `confidence_level` widening the CI. |
| `test-diagnosticmeta-notices-wilsonci.R` | The `notices` channel and Wilson score intervals: table CIs reproduced against an independent Wilson (1927) implementation, honouring `confidence_level` and staying inside [0, 100] at extreme proportions; all four correction keys run, are disclosed, and genuinely differ; fewer than 3 studies is a hard reject; meta-regression without a covariate raises an INFO notice; Deeks' below 10 studies raises the power caution; excluded studies are counted in a WARNING; notices do not accumulate across run cycles. |
| `test-diagnosticmeta-release-review.R` | Release-review regressions checked against `mada`, `metafor` or hand-coded primary-source formulas: Deeks' specification and zero-cell behaviour, pooled estimates vs `mada::reitsma`, SROC prediction region, `confidence_level` reaching that region, prediction interval built on t with k-2 df, exclusion and zero-cell disclosure, visible pooled interval on the forest plot, per-study and pooled accuracy on one scale, meta-regression refusals and Knapp-Hartung adjustment, PHM p-value testing H0: theta = 1, PHM continuity correction, PHM AUC = 1/(1 + theta), fixed effects with SROC plot, bivariate variance components and sens/spec correlation, I-squared matching metafor's REML fit, and SROC/forest plots drawing the same corrected data. |
| `test-diagnosticmeta-release-fixes.R` | Nominal-integer counts read as counts and not level codes, non-integer counts excluded and impossible ones rejected, duplicate or missing study identifiers made unique and disclosed, covariate-driven exclusions disclosed, zero-cell handling states which models keep those studies, Deeks' test corrects every study and withholds a verdict when zeros dominate, fixed-effect method disclosures, HSROC refit when mada hits its iteration limit, and SROC curve stopping at the observed false-positive range. |
| `test-diagnosticmeta-audit-fixes.R` | Switching every option off explains itself instead of showing a blank pane, Notices panel hidden when empty, 3-4 studies pooled with a caution, hidden explanation panels not built, zero-cell table note survives with summary switched off, failed meta-regression explained, `bivariate_analysis` in `clearWith`, stable ASCII note keys, out-of-range `method` refused, and `.run()` yields to jamovi between expensive fits. |
| `test-diagnosticmeta-audit-20261019.R` | Audit regressions for HTML table conversion (`likelihoodRatioGuide`, `predictiveValues`), theme-safe styling, and notice hygiene. |
| `test-diagnosticmeta-check-fixes.R` | Validation and schema wiring checks across option combinations. |
| `test-diagnosticmeta-i18n-checks.R` | Internationalization catalog and `.()` wrapper checks. |
| `test-diagnosticmeta-jamovify.R` | `jmvcore` helper migration and `toNumeric` safety checks. |
| `test-diagnosticmeta-release-20261019.R` | Pre-release critical review regressions (2026-10-19). |
| `test-diagnosticmeta-release-profile-fixes.R` | Release-profile regressions for zero-cell handling across models, factor meta-regression omnibus F-tests, and predictive values by prevalence. |
| `test-diagnosticmeta-presubmission-20261107.R` | Pre-submission review regressions (2026-11-07): near-100% accuracy formatting (`.fmtPct`, `.fmtRatio`), IEEE-754 90.0% boundary classification, LR threshold-crossing digits, SROC collapse guard, and `contr.poly` / `contr.sum` meta-regression contrast labels. |

To run one file on its own, note that testthat's `filter` strips the `test-` prefix:

```r
testthat::test_dir("tests/testthat", filter = "diagnosticmeta-presubmission-20261107")
```

## 3. QA Sign-Off Criteria

Status recorded 2026-11-07:

- [x] **testthat, all seventeen `diagnosticmeta` files**: 100% passing (0 failures, 0 errors). Recorded 2026-11-07 after the pre-submission review pass.
- [x] **Release gate (`tools/release_gate.py --analysis diagnosticmeta`)**: 0 blocking findings.
- [x] **UI labels and schemas reviewed**: every option title and description in `vignettes/diagnosticmeta-documentation.md` matches `jamovi/diagnosticmeta.a.yaml`, all 23 user options have a control in `jamovi/diagnosticmeta.u.yaml`, and all 19 result items match `jamovi/diagnosticmeta.r.yaml`.
