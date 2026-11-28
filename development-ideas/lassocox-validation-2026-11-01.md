# LASSO Cox targeted validation — 2026-11-01

Verdict: VALIDATED WITH CONDITIONS, restricted to the executed software assertions below.
This is not clinical validation, a full release review, or validation of the entire ClinicoPath package.
Production R and jamovi sources remain unchanged from `70d5853f75accad5ee66638879b7d5e9ea24e376`.

## Execution and scope

The generated R wrapper and real backend were sourced with their two shared utility files.
No replacement statistical implementation was substituted for the backend. The full umbrella
namespace was not loaded, so this route does not establish installation or NAMESPACE integrity.

Environment: Ubuntu 24.04, R 4.3.3, testthat 3.2.1, jmvcore 28.3, survival 3.5-8,
Matrix 1.6-5, ncvreg 3.16.0. Both glmnet 4.1-8 and the current CRAN 5.2 were exercised.

| Executed check | Result |
|---|---|
| Unmodified October 28 regression file, glmnet 4.1-8, before ncvreg installation | 38 assertions passed; optional ncvreg branch was not entered |
| Original nine blocks with ncvreg installed, during expanded execution | 39 assertions passed |
| Final regression file, glmnet 4.1-8 | 74 passed; 0 failed/errors/skips/warnings |
| Final regression file, glmnet 5.2 | 74 passed; 0 failed/errors/skips/warnings |
| Fresh-process repeat with glmnet 5.2 | 74 passed; 0 failed/errors/skips/warnings; identical results.csv |
| YAML/R parsing and git diff whitespace check | Passed locally |

The CSV matrix records the 12 test blocks and assertion counts. It is an execution inventory,
not a claim that 74 independent statistical estimands were validated. Several assertions share
one fixture and several are plumbing or consistency checks.

## Review of the October 28 tests

- **Independent numerical evidence:** base-R Breslow log-likelihood/gradient/Hessian and
  Newton solver check unpenalized comparison AIC/log-likelihood; pairwise counting checks
  apparent concordance. Optional `ncvreg::ncvsurv` supplies a separate LASSO optimizer.
- **Same-library evidence:** the separate `glmnet::cv.glmnet` fit and exported-code round trip
  establish option/data/fit consistency. They do not independently establish glmnet mathematics.
- The broad `C01-C22` description is larger than its actual set of assertions. A passing name
  alone does not prove every named output or branch. The original KKT calculation used a
  separately fitted glmnet coefficient vector; it now uses the module's returned table vector,
  verifies it against that reference, requires an active coefficient, and checks the inactive
  inequality as well as active stationarity.
- The original explicit-NA factor test only required no error. Added assertions verify exactly
  which rows are excluded, the event vector, indicator values, reference levels and ordered
  treatment coding. Reordering outcome levels and changing to numeric event=1/censor=2 preserve
  the selected event mapping.
- Added backend fold tests require identical folds, lambda and coefficients on repeated fits,
  events/censors in every fold, balanced stratum counts, changed folds for a changed seed, and
  restoration of both an existing RNG state and absence of an initial RNG state.
- Added hand-counted performance cases cover perfect/reversed ordering, tied scores (5/6),
  tied failures (1/2), same-time event/censor observations, constant scores (1/2), and missing
  concordance when no comparable pairs exist. Reversing scores must complement concordance.

## Precision finding and adjudication

An exploratory active-KKT threshold of 1e-4 on the added 90-row encoding fixture failed under
older glmnet 4.1-8: maximum residuals were about 0.000290 and 0.000137 for standardized and
unstandardized fitting. That fixture deliberately repeats outcome and factor patterns, so one
category has no events; it is a precision stress case, not an inferential validation dataset.

`development-scripts/diagnose_lassocox_precision.R` compares the actual backend with a tighter
upstream glmnet path and, for standardized fitting, ncvreg with a longer iteration budget.
An initial full-path ncvreg diagnostic did not converge at some penalties; a single-penalty
coefficient interpolation was invalid. The retained diagnostic uses three penalties bracketing
the selected value and reports the same selected penalty without changing data or estimand.

| glmnet | Standardize | Max coefficient difference: default vs tighter path | Penalized objective gap |
|---|---|---:|---:|
| 4.1-8 | TRUE | 0.1526956 | 1.019800e-5 |
| 4.1-8 | FALSE | 0.1520476 | 1.015012e-5 |
| 5.2 | TRUE | 0.00004807448 | 5.647145e-10 |
| 5.2 | FALSE | 0.00002632248 | 1.582250e-10 |

For standardized fitting, ncvreg differed from the default 4.1-8 fit by 0.1532217 and from the
tighter fit by 0.0005261406. With glmnet 5.2 those differences were 0.00004870896 and
0.000002158532. This localizes the discrepancy to upstream optimizer precision near a flat
objective. It does not establish a new backend regression. The arbitrary exploratory residual
threshold is not retained as a guarantee of glmnet's default stopping criterion; the original
independent KKT fixture keeps its original 1e-4 tolerance, now testing actual module estimates.
No production fitting parameters were changed to obtain a pass. Recheck such sparse-event
categories with current dependencies and tighter optimization before interpreting coefficient size.

## CI correction

The cited runs 40726290040 and 40726289905 were inspected: only Check version completed;
release/pkgdown jobs were skipped. Neither run executed this suite.

The proposed separate workflow runs only on relevant master pushes, relevant pull requests,
or manual dispatch. It installs an explicit small dependency set and executes only this focused
regression file, with required ncvreg rather than silently omitting its comparison. It fails if
any test fails, errors, skips, or no tests execute, and uploads block results plus sessionInfo.
The existing full-package and PR workflows are unchanged.

## Not verified

- Full-package loading/install/R CMD check, other LASSO test files, jamovi desktop rendering,
  serialization, localization, and a packaged release were not validated by this targeted route.
- C-index SE/IJ variance, approximate-time ties, large-p numerical robustness, external prediction,
  calibration, optimism correction, selection stability and clinical usefulness were not established.
- No exhaustive citation, narrative or clinical-threshold audit was performed. Existing descriptions
  distinguish apparent development performance and selection-biased Cox refits from external validation.
- The longstanding AppVeyor failure was not rerun or attributed to this commit.

## References and rerun

Primary method contracts inspected: glmnet Cox vignette, sections Introduction, Cross-validation,
and Handling of ties: https://glmnet.stanford.edu/articles/Coxnet.html ; survival concordance
reference, Arguments and Details: https://stat.ethz.ch/R-manual/R-devel/library/survival/html/concordance.html .
The former explicitly defines the risk set and Cox partial likelihood; the latter documents
risk-score orientation, comparable pairs and default Harrell weighting.

From the repository root:

```sh
Rscript --vanilla development-scripts/validate_lassocox.R
Rscript --vanilla development-scripts/diagnose_lassocox_precision.R
```

Install testthat, jmvcore, R6, glmnet, survival, htmltools, ncvreg and withr first. Set
`LASSOCOX_REPORT_DIR` to write results.csv and sessionInfo.txt. The precision diagnostic is
exploratory and prints a deprecation notice on glmnet 5.2 for the legacy `thresh` argument;
this does not occur in the regression suite.
