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

There is no single "dedicated" suite. Nine files under `tests/testthat/` cover `diagnosticmeta`:

```r
# all ten at once
testthat::test_dir("tests/testthat", filter = "diagnosticmeta")
```

| File | Size | What it covers |
| :--- | :--- | :--- |
| `test-diagnosticmeta.R` | 183 lines, 2 tests | Cross-checks pooled sensitivity/specificity against `mada` using the `data-raw/non-rda/diagnostic_meta_test.csv` fixture, and checks that the optional analysis flags leave their tables unpopulated when switched off. Skips if `jmvReadWrite`, `mada`, `metafor` or the fixture are unavailable. |
| `test-diagnosticmeta-basic.R` | 190 lines, 9 tests | Smoke coverage: the function exists, runs on the minimal argument set, returns a `diagnosticmetaResults` object, produces default outputs, honours `bivariate_analysis` / `confidence_level` / `method`, runs on a small dataset, produces plots on request, and shows guidance rather than throwing when a required variable is omitted. |
| `test-diagnosticmeta-arguments.R` | 263 lines, 13 tests | Argument-combination matrix: meta-regression with continuous and categorical covariates, all four zero-cell corrections, all analysis options together, all plot options together, all five colour palettes, all display options, HSROC alone, heterogeneity alone, publication bias, a large dataset, method x correction combinations, and a quality score as covariate. |
| `test-diagnosticmeta-edge-cases.R` | 337 lines, 16 tests | The "a jamovi analysis must never throw on a data problem" contract: missing, negative, non-integer, all-zero and duplicated rows are excluded and disclosed rather than raised. Also very small samples, zero cells, perfect sensitivity, perfect specificity, extreme heterogeneity, extreme confidence levels, special characters in variable names; a single study and a constant covariate are refused with an explanation. |
| `test-diagnosticmeta-integration.R` | 350 lines, 12 tests | End-to-end workflows and package integration: agreement with `mada` and `metafor`, run-to-run consistency, basic meta-analysis into meta-regression, the publication-bias workflow, model comparison, CSV- and Excel-shaped imports, reproducible plots, a comprehensive all-options run, consistency across data structures, and sensitivity analysis across corrections. |
| `test-diagnosticmeta-critical-fixes.R` | 381 lines, 11 tests | Numerical regressions from an earlier fix round: the SROC pooled point is not passed through `plogis()` twice, Deeks' test uses effective sample size rather than an arithmetic total, I-squared is dimension-specific, ESS arithmetic, minimum study count enforcement, zero-cell handling, pooled estimates inside the valid probability range, meta-regression with a continuous covariate, individual-study table population, colour palettes, and `confidence_level` widening the CI. |
| `test-diagnosticmeta-notices-wilsonci.R` | 221 lines, 11 tests | The `notices` channel and the Wilson score intervals: table CIs reproduced against an independent Wilson (1927) implementation, honouring `confidence_level` and staying inside [0, 100] at extreme proportions; all four correction keys run, are disclosed, and genuinely differ; fewer than 3 studies is a hard reject; meta-regression without a covariate raises an INFO notice; Deeks' below 10 studies raises the power caution; excluded studies are counted in a WARNING; notices do not accumulate across run cycles; the instructions panel stays pure onboarding. The file header records that it previously asserted intended behaviour through vacuous `expect_s3_class()` checks. |
| `test-diagnosticmeta-release-review.R` | 411 lines, 23 tests | Release-review regressions, each checked against `mada`, `metafor` or a hand-coded primary-source formula rather than the module's own arithmetic: Deeks' own specification and its behaviour with a zero cell, pooled estimates vs `mada::reitsma`, the SROC prediction region, `confidence_level` reaching that region, a prediction interval wider than the CI and built on t with k-2 df, exclusion and zero-cell disclosure, a visible pooled interval on the forest plot, per-study and pooled accuracy on one scale, meta-regression refusals (no residual df, constant covariate) and success, the Knapp-Hartung adjustment, the PHM p-value testing H0: theta = 1, PHM using the same continuity correction as the bivariate model, PHM AUC = 1/(1 + theta), fixed effects with the SROC plot not aborting, bivariate variance components and sens/spec correlation, I-squared matching metafor's REML fit, and the SROC and forest plots drawing the same corrected data. |
| `test-diagnosticmeta-release-fixes.R` | 228 lines, 11 tests | The 2026-09-20 release pass; every test here fails on the backend as it stood before that pass. Nominal-integer counts are read as counts and not level codes, non-integer counts are excluded and impossible ones rejected, duplicate or missing study identifiers are made unique and disclosed, covariate-driven exclusions are disclosed, zero-cell handling states which models keep those studies and which drop them, Deeks' test corrects every study and withholds a verdict when zeros dominate, the fixed-effect method discloses what it removes, no heterogeneity is claimed when none was estimated while genuine heterogeneity still is, the HSROC model is refitted when mada hits its iteration limit, and the SROC curve stops at the observed false-positive range. |
| `test-diagnosticmeta-audit-fixes.R` | 11 tests | The 2026-09-20 `/check-function-full` audit pass; 6 of its 11 blocks fail on the backend as it stood before that pass, and the rest are no-regression guards. Covers: switching every option off explains itself instead of showing a blank pane, the Notices panel is hidden when empty, three or four studies are not pooled without a caution, hidden explanation panels are not built at all, the zero-cell table note survives with the summary switched off, a failed meta-regression would be explained rather than echoed, `bivariate_analysis` is in the bivariate table's `clearWith`, note keys are stable ASCII rather than translated labels, an out-of-range `method` is refused before the backend sees it, `.run()` yields to jamovi between the expensive fits, and removing the dead scaffolding changed no number. |

To run one file on its own, note that testthat's `filter` strips the `test-` prefix:

```r
testthat::test_dir("tests/testthat", filter = "diagnosticmeta-release-fixes")
```

## 3. QA Sign-Off Criteria

Status recorded 2026-09-20. A ticked box below means someone ran the check and it passed on that date; an unticked box means the check has not been done or has not passed. Nothing here is ticked on the strength of intent.

- [x] **testthat, all ten `diagnosticmeta` files**: 345 assertions, 0 failures, 0 skips. Recorded 2026-09-20 after the `/check-function-full` audit pass.
- [ ] **R CMD check**: not run as part of this pass. No clean-check evidence exists for `diagnosticmeta` at this date.
- [ ] **UI labels reviewed against clinical guidance standards**: no such review is on record. What *was* verified on 2026-09-20 is narrower: every option title and description in `vignettes/diagnosticmeta-documentation.md` now matches `jamovi/diagnosticmeta.a.yaml`, and all 23 user options have a control in `jamovi/diagnosticmeta.u.yaml`.
- [ ] **Internationalization**: **incomplete.** The strings added during the 2026-09-20 release pass were never extracted into the catalog: 57 of the roughly 237 single-line `.()` msgids in `R/diagnosticmeta.b.R` are absent from both `jamovi/i18n/catalog.pot` and `jamovi/i18n/tr.po`. Turkish output falls back to English for all of them. Run the extraction and a translation pass (`/prepare-translation diagnosticmeta`) before claiming translation coverage. Counted by a static scan that does not see multi-line `.()` literals, so both figures are lower bounds.
- [ ] **TC-04 (zero-row dataset)**: behaviour is implemented in `.run()` but has no automated test. Add one, or accept the gap knowingly.
- [ ] **Untranslated HTML panels**: `.populateInstructions()`, `.populateAboutPanel()`, `.populateInterpretation()` and the three plot-explanation panels are English string literals, not `.()` calls. Tracked in the backend as a deferred library-audit INFO finding.
