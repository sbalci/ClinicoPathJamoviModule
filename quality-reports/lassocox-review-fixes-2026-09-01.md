# LASSO Cox review repairs

Date: 2026-09-01. Analysis version: **0.0.6**. Module: **1.0.8.04**.

All concrete defects and maintenance recommendations in the 2026-09-01
[function review](lassocox-review-2026-09-01.md) were implemented. This repair
does not claim model validation and does not change the penalized `glmnet` fit.

## Implemented repairs

1. `.coxRefit()` now captures and suppresses `coxph` warnings, rejects every
   non-clean refit, and verifies finite coefficients and log-likelihood before a
   comparison metric can be calculated. Failed rows remain present with unavailable
   numeric cells and an explanatory table note.
2. The proportional-hazards diagnostic uses the same validated refit and treats a
   `cox.zph` warning as an unavailable diagnostic. It cannot show a green verdict
   after an invalid refit.
3. Correlated encoded-column names are escaped once at the final HTML boundary.
   The former `Top pairs:` fragment is one complete translatable sentence with a
   `{pairs}` placeholder.
4. Predictor sufficiency is decided from the usable encoded design. A single numeric
   column receives the direct engine-limit error; one categorical candidate may run
   when it produces at least two non-constant encoded columns.
5. Sample-size adequacy is gray/undetermined above the computational hard minimum.
   Event-count, imbalance and instability checks retain their warning/error colors.
6. The within-factor correlation mask uses a vectorized origin comparison rather
   than nested loops.
7. The scoped translation refresh removed 35 obsolete LASSO-only entries. All 322
   current LASSO-referenced messages have Turkish translations with matching format
   placeholders.

## Verification

- Eight LASSO test files: **442 assertions in 91 blocks**, with zero failures,
  errors, or skips.
- New regressions cover warning-without-error Cox non-convergence, invalid PH refits,
  one-time HTML escaping, encoded-width handling, and neutral sample-size status.
- Deterministic 70-row/60-column probe: the non-converged all-variable Cox row now
  reports `NA` for C-index, AIC and log-likelihood and records the convergence reason.
- Module result-rendering contract: **6 assertions in 4 blocks**, all passing.
- Jamovi client UI harness: `placeholder present = false`; `errors = undefined`.
- Isolated compiler/install succeeded. Backend, generated header and all three schemas
  match the installed staging source byte-for-byte.
- GNU `msgfmt --check --check-format` passes for English, Turkish and POT catalogs;
  compiled Turkish runtime output contains the new messages.
- Scoped lintr: zero findings from the seven bug-class linters. Remaining findings
  are suppressed/nonblocking quote, brace, comma and infix-spacing style classes.
- State-guard and dark-theme HTML scans report zero findings. All module Output
  result items remain wired to options of the same name.

## Remaining model-development scope

The current C-index, risk groups and unpenalized refits remain apparent development
outputs and are labeled accordingly. Full-process resampling and frozen-model external
prediction remain separate feature work because they require new analysis contracts,
outputs and validation datasets rather than repairs to the current estimator.
