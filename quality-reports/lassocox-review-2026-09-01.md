# CODE REVIEW: `lassocox`

Date: 2026-09-01. Reviewed analysis version: **0.0.5**. Review mode: report-only.
The source and generated files match the verified 2026-08-31 isolated build.

> **Resolution update (analysis 0.0.6):** The six implementation action items from
> this review were repaired on 2026-09-01. Non-clean Cox refits now yield unavailable
> comparison cells and cannot produce a PH verdict; correlation HTML and translation,
> encoded-width handling, neutral sample-size status, vectorized masking, catalog
> cleanup, and regression coverage were implemented. The validation enhancements
> remain future model-development features, as described below.

**Overall Quality**: ★★★★☆  
**Maintainability**: HIGH  
**Performance**: GOOD  
**User Experience**: GOOD  
**Mathematical/Statistical Correctness**: MINOR_ISSUES  
**Clinical & Release Readiness**: NEEDS_VALIDATION  
**CRAN Compliance (scoped hygiene greps)**: CLEAN  
**Static Analysis (lintr)**: CLEAN

The penalized Cox fit itself is statistically coherent and agrees with upstream
`glmnet`. One optional unpenalized comparison path can nevertheless present
statistics from a model that explicitly failed to converge. That output should be
guarded before public release. The analysis is appropriately labeled as model
development and is not ready to support clinical decisions without full-process
internal validation and independent external validation.

## Strengths

1. The estimator uses `glmnet::cv.glmnet(..., family = "cox", alpha = 1,
   cox.ties = "breslow")` with explicit event/censor-stratified folds and a
   user-controlled seed ([backend](../R/lassocox.b.R#L594)). RNG state is restored.
2. The requested `lambda.min` or `lambda.1se` rule is respected exactly. A valid
   empty model remains empty rather than being replaced after inspection
   ([backend](../R/lassocox.b.R#L724)). Coefficients, linear predictors, tables and
   paths come from the same full-data fitted path.
3. Numeric agreement is strong: selected lambda, coefficients and scores match an
   independent upstream call exactly; the independently computed C-index differs by
   only `1.9984e-15` ([reference evidence](lassocox-fixes-2026-08-31/reference.json)).
4. Input handling is unusually careful. It rejects overlapping roles, nonpositive
   time, nonfinite values, nonbinary outcomes, entirely missing predictors and
   insufficient event/censor counts. It records complete-case exclusions and both
   kinds of constant-column removal ([backend](../R/lassocox.b.R#L269)).
5. Factor coding is deterministic treatment coding, including ordered factors.
   Encoding/provenance outputs disclose reference levels and column-wise selection.
6. Apparent performance, development-sample median splits, post-selection refits and
   PH diagnostics are labeled honestly. No ordinary post-selection p-values or
   confidence intervals are manufactured. The clinical guidance explicitly forbids
   treatment or surveillance decisions from these outputs.
7. Lifecycle and serialization coverage is strong: invalid reruns clear tables,
   notes, plots and scores; file-backed jmvcore save/load preserves appropriate
   states and regenerates row-aligned scores.
8. All current LASSO user strings extracted from the isolated package have Turkish
   translations; placeholders match, GNU `msgfmt` passes, and compiled Turkish
   outputs and plots have runtime coverage.

## Critical issues

### 1. P1 — non-converged Cox comparison is reported as a valid numeric result

The optional all-variable comparison calls `.coxRefit()` and immediately calculates
concordance, AIC and log-likelihood without treating a `coxph` convergence warning as
failure ([backend](../R/lassocox.b.R#L1937)). `coxph()` can return an object and finite
statistics after warning that it ran out of iterations. The outer warning handler puts
that message in the general **Analysis Notes** panel, while the comparison row still
looks complete.

A deterministic review probe with 70 observations, 47 events and 60 encoded columns
produced:

| Row | C-index | AIC | Log-likelihood |
|---|---:|---:|---:|
| Standard Cox (all variables) | 0.9569 | 298.31 | -89.15 |

The fit simultaneously warned: **“Ran out of iterations and did not converge.”**
Those impressive values are not a valid comparison. The table note discusses
selection bias but does not say that this particular row is non-converged.

Capture warnings inside `.coxRefit()` or the comparison block and mark the entire row
unavailable when convergence/rank/infinite-coefficient warnings occur. Prefer skipping
the all-variable Cox refit before fitting when the available event information cannot
support it. Apply the same validity check before using a selected-column refit for
`cox.zph()`, so a diagnostic cannot appear green after an invalid refit.

### 2. P3 — special predictor names are double-escaped in correlation details

The correlation builder escapes each column name, then escapes the combined pair text
a second time ([backend](../R/lassocox.b.R#L1675)). A probe with a predictor named
`marker & {x}` produced `&amp;amp;` in the HTML source, so the user sees the entity text
instead of the original ampersand. Escape names exactly once at the final HTML boundary.
This same area builds a translated sentence by appending a separately translated
`Top pairs:` fragment ([backend](../R/lassocox.b.R#L1682)); use one complete translated
sentence with a `{pairs}` placeholder.

### 3. P3 — the single-predictor branch emits a warning before an inevitable rejection

The cleaner warns that LASSO selection is limited when one predictor remains
([backend](../R/lassocox.b.R#L420)), but the encoded design is then rejected whenever it
has fewer than two columns ([backend](../R/lassocox.b.R#L526)). The warning cannot lead
to a result and is cleared by the error path. Replace the warning with one direct,
actionable engine-limitation rejection, preferably before matrix construction.

## Code-hygiene findings

### Real issues

None in the scoped CRAN hygiene checks.

### False positives

1. `set.seed(seed_value)` at lines 616 and 702 uses the user-facing `random_seed`
   option. The other two matches are inside generated copy-ready R code.
2. Every `<<-` match occurs inside an error/warning handler and intentionally updates
   the enclosing method frame. None occurs in a `tryCatch` body or writes a standalone
   object into `.GlobalEnv`.
3. `warning()` is used as an in-frame condition transport and is immediately caught,
   categorized and rendered into the HTML notice panel. The messages are not invisible
   jamovi console warnings.
4. There is no package-source `library()`/`require()`, unrestored `par()`/`options()`,
   hardcoded seed or missing `\value` section. All namespace-qualified packages are
   declared in `Imports`.
5. All eight top-level and item-level reference keys resolve exactly in
   `jamovi/00refs.yaml` and contain author/year metadata.

Package-wide `checktor` categories are outside this function-scoped review.

## Lintr findings

The prescribed review linter set found no `seq_linter`, `equals_na_linter`,
`sprintf_linter`, `unreachable_code_linter`, `duplicate_argument_linter`,
`missing_argument_linter` or `T_and_F_symbol_linter` defects. Its 20 findings were
suppressed/nonblocking style classes: 15 quote-style, three brace-style, one comma
spacing and one infix spacing finding.

Manual review found no undefined local read. Scalar `if`/`while` conditions use
`&&`/`||`; vectorized `&` is used appropriately for matrix masking. Two nonblocking
unused values remain: `data` in `.calculatePerformanceMetrics()` and `lambda_val` in
`.populateSummary()`; `cindex_se` is computed but deliberately not displayed because
its model-conditional uncertainty omits preprocessing and selection.

## Improvement opportunities

1. Split the ~2,000-line R6 private list into focused helpers for preprocessing,
   fitting, diagnostic HTML and plot state. Keep the public R6 lifecycle in this file.
2. Replace the nested `500 × 500` within-factor correlation loops with a vectorized
   origin-comparison mask. The current cap bounds the cost, so this is an optimization,
   not a blocker.
3. The suitability sample-size check is always yellow once the hard minimum passes
   ([backend](../R/lassocox.b.R#L1614)); therefore the nominal all-green overall branch
   is unreachable in practice. Use a neutral “not determined” status instead of
   encoding unavoidable uncertainty as a warning color.
4. Remove or obsolete stale catalog entries that still cite earlier LASSO text such as
   `Protective`, `Time Elapsed`, and action-style labels. They are not part of the
   current isolated extraction and do not affect runtime, but they make source-based
   translation audits noisy.
5. Add a regression fixture in which `coxph` warns without throwing and assert that no
   numeric comparison or PH verdict is published.

## Enhancement suggestions

1. Implement full-process bootstrap optimism correction or nested cross-validation,
   repeating missing-data handling, encoding, lambda selection and fitting within
   each resample. Keep the current apparent C-index only as a clearly labeled
   development statistic.
2. Add a frozen-model external prediction path with stored encoding, coefficients,
   baseline survival and prespecified horizons. Then provide time-specific calibration,
   discrimination and clinical-utility evaluation.
3. Consider grouped penalties for categorical predictors and elastic net for correlated
   biomarkers; the current column-wise LASSO behavior is correctly disclosed.
4. Add a clinician-oriented example report block that demonstrates safe wording for a
   development model. Do not supply patient-level advice or attach a clinical meaning
   to arbitrary C-index/EPV thresholds.
5. For accessibility, offer line types or interactive highlighting for coefficient
   paths; 30 traces cannot be reliably distinguished by color alone.

## Clinician-friendly checklist

| Area | Status | Review |
|---|---:|---|
| Plain-language labels/tooltips | Good | Clear noun-based labels; technical options have R help |
| Micro-explanations | Good | Optional LASSO, CV, path and score explanations |
| Glossary | Partial | HR/C-index explained in context; no compact glossary panel |
| Guided flow | Missing | Variable supplier is well ordered, but no wizard |
| Misuse guards | Good | Strong role/time/outcome/event-count and instability guards |
| Example interpretations | Partial | Natural summary exists; no worked report example |
| Copy-ready summary | Good | Optional result summary, appropriately caveated |
| Clinical presets | Not applicable | No safe universal clinical preset for this development model |
| Accessibility | Partial | Readable sizes; dense paths still rely on color |
| Turkish/English | Good | Complete current LASSO extraction and runtime checks |
| About/how-to | Partial | Welcome and explanations; canonical external guide exists |
| Caveats/assumptions | Good | PH, missingness, instability and validation limitations surfaced |

## Validation performed

- **423/423** assertions pass in **86** blocks across eight LASSO test files; no
  failures, errors or skips.
- Module-wide result-rendering contract: **6/6** assertions pass.
- Real jamovi client UI harness: no placeholder and no option-panel errors.
- Backend parses; all 25 options and 22 results are wired; `menuGroup: SurvivalT`
  remains correctly routed for development testing.
- State guards and dark-theme HTML scanners report zero violations.
- Reference integrity, dependency declarations, documentation `\value`, scoped
  hygiene and current Turkish placeholder integrity pass.
- Existing verified build hashes match every current LASSO source/schema/test file.
- Direct probes reproduced both residual findings above.

## Action items

- [x] Block numeric Cox comparison/PH outputs from non-converged or non-finite refits.
- [x] Add warning-without-error Cox fixtures and assert unavailable rows/diagnostics.
- [x] Remove double HTML escaping and replace the `Top pairs:` fragment with a complete translated sentence.
- [x] Consolidate the one-predictor warning and rejection.
- [x] Treat sample-size adequacy as undetermined rather than permanently yellow.
- [x] Clean obsolete LASSO catalog entries during the scoped extraction pass.
- [ ] Add full-process internal validation before describing performance as validated.
- [ ] Validate a frozen model externally before any clinical-facing use.

## Clinical and release recommendation

The **penalized development analysis is mathematically sound**, reproducible and much
safer than the pre-repair implementation. Analysis 0.0.6 repairs and regression-tests
the optional Cox-comparison blocker, so it can be released as a clearly labeled
**development tool**. It remains **NEEDS_VALIDATION** for clinical-facing prediction:
software correctness, cross-validation for lambda selection and apparent C-index
agreement do not establish calibration, transportability or clinical utility.
