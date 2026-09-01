# LASSO Cox: encoding and reproducibility

These changes apply to analysis version 0.0.6, on the `SurvivalT` development route.

## Input requirements

Follow-up time must be strictly positive. The outcome must have exactly two
observed values, with event and censored levels selected explicitly. Time and
outcome cannot also be predictors. Infinite values and entirely missing predictor
columns are rejected. Constant predictors are removed with a notice; ordinary
missing rows are excluded, and saved scores remain aligned with the original rows.

Predictors must be available at the intended prediction time. The software can
reject overlapping input roles but cannot identify every post-outcome variable.

## Factor coding changed

Both nominal and ordered factors now use treatment indicators. Their reference is
the first observed level in the supplied factor order; unused levels are dropped
after complete-case filtering. Character levels are sorted. Custom contrasts and
global R contrast settings are overridden.

An ordered grade is therefore categorical, not an automatically fitted polynomial
trend. This can change fits made with earlier versions. Inspect the **Predictor
encoding** table before comparing coefficients across analyses. It identifies each
design column, original predictor, comparison and reference category, and whether
the column was selected. LASSO selects columns separately, not whole factors.

The summary distinguishes original candidate predictors, encoded columns, selected
columns, and selected original predictors. Numeric hazard ratios describe a
one-unit increase; indicator hazard ratios compare the displayed category with its
reference. Hazard ratios are not ratios of event probabilities.

## Optional outputs

Under **Plots**, **Coefficient paths** displays the coefficient trajectories across
log(lambda), marks lambda.min and lambda.1se, and shows full-model nonzero counts
on the upper axis. At most 30 traces are displayed, ranked by summed absolute
coefficients over the path. The caption discloses this limit; the limit does not
change fitting or selection. Trace rank is not clinical importance. The existing
coefficient bar plot remains available independently.

Under **Output options**:

- **Predictor encoding** explains the fitted design columns.
- **Reproducibility details** gives lambda values at 17-digit precision, the selected
  rule, seed, event/censor counts for each fold, removed constants, and software
  versions.
- **Reproducible R code** gives executable upstream `glmnet` code. Supply the same
  development data frame named `data`, preserving its factor levels and row order.
  The code rebuilds the recorded design columns, stratified folds, coefficients,
  row-aligned linear predictors, and apparent C-index. It preserves the R session's
  random-number state and does not export patient values.

All new outputs are off by default. Tables, scores, and paths now use the same
full-data fit already computed by `cv.glmnet`, rather than a separate single-lambda
refit. A valid empty 1-SE model remains empty, with constant zero scores.

Pairwise-correlation diagnostics are skipped above 500 encoded columns to bound
memory and computation. This does not remove predictors from the fitted model.

## What this does not validate

Lambda cross-validation is for tuning. The displayed C-index and risk-group
curves remain apparent development results. Neither these outputs nor their R
export provides nested validation, development optimism correction, external
validation, horizon-specific event probabilities, or clinical surveillance advice.

A full validation procedure must repeat preprocessing, tuning, and fitting inside
the appropriate resampling loop. Frozen external prediction and shared validation
remain subsequent implementation work.

## Audit repairs in 0.0.5

Optional unpenalized Cox comparisons now tolerate predictor names such as `y`,
`.time`, and `.status`, including names containing punctuation. These comparisons
remain descriptive and selection-biased. The penalized fit is unchanged.

Coefficient paths use a compact legend with abbreviated long labels; the
coefficient chart labels lower and higher fitted hazard. The path plot, not the CV
plot, shows nonzero counts. Removed constants are reported consistently in the
notices, suitability report, and reproducibility table, including columns that
become constant after complete-case filtering.

Fixed summary rows are initialized before fitting. Invalid inputs clear previous
tables, notes, plots, and saved scores. Runtime caveats survive framework save/load,
and score output preserves the original row order with missing values for excluded
rows. Very few events or censored observations and too many candidate columns
receive prominent stability warnings.

Educational explanations and current LASSO messages have Turkish translations.
If `glmnet` or `survival` is unavailable, an actionable error stops fitting. If only
`survminer` is unavailable, survival curves use a base-R fallback without a risk
table.

See the [audit repair and validation report](../quality-reports/lassocox-fixes-2026-08-31.md)
for tested behavior and remaining validation limitations.

## Review repairs in 0.0.6

Optional unpenalized Cox comparison rows are now left unavailable when `coxph`
warns, fails to converge cleanly, or returns non-finite estimates. The same guard
prevents an invalid selected-variable refit from producing a proportional-hazards
verdict. These checks affect only descriptive refits; the penalized `glmnet` fit is
unchanged.

Correlation details escape special predictor names once and use a complete
translatable sentence. Within-factor correlation masking is vectorized. A single
candidate predictor is accepted when its categorical encoding supplies at least two
usable columns, while a one-column numeric design receives the existing direct
engine-limitation error.

The suitability panel now shows sample-size adequacy in gray because total sample
size and a universal events-per-variable threshold cannot establish adequacy for a
prediction model. This neutral state does not weaken the hard minimum checks or the
separate event-count and instability warnings.
