# Observed risk and cure-model implementation

Implementation following the [Putter–Andersen article review](putter-andersen-2026-cure-models-citation-review.md), 4 October 2026.

The original review describes the code **before** these changes. Its linked source line numbers are historical. The changes below implement the supported estimation paths and remove fabricated results from the affected drafts. They do not turn all previously advertised draft options into validated features.

## Changes by function

| Function | Implemented behavior |
|---|---|
| `curemodels` | Explicit event mapping and endpoint selection; correctly complemented `smcure` susceptibility predictions; cohort-standardized or reference-profile cure probabilities; genuine bootstrap uncertainty; matched background-hazard requirements; observed-risk and follow-up-support tables; actual administrative-censoring sensitivity refits. |
| `singlearm` | All-state Aalen–Johansen table with event-free probability, target-event CIF, competing-event CIF, pointwise intervals, and follow-up counts. The competing-risk plot can display a computed common risk table. |
| `survival` | The same table by group, including outcome-organizer handoffs. Requested times beyond a group's follow-up are omitted. |
| `intervalsurvival` | Actual `icenReg` Turnbull, PH, and parametric AFT likelihoods; correct exact/left/right/interval censoring; infinite right bounds retained; optional subject bootstrap. |
| `illnessdeath` | Validated complete subject histories; full outgoing transition risk sets; transition-specific Cox models; unadjusted Aalen–Johansen state occupation; restricted mean time in state with optional whole-subject bootstrap. |
| `flexmultistate` | Actual clock-forward transition models, integrated transition matrices, state probabilities, restricted expected time in state, fitted-parameter simulation intervals, and optional trajectory simulation. |
| `flexparametric` | The requested spline family, scale, knots, and time-varying effects are fitted directly. Failed spline fits no longer silently become Weibull fits. Coefficients retain their correct model-scale interpretation. |
| `flexrstpm2` | Real PH/PO/probit spline fits and predictions, fitted uncertainty, manual knots and time-varying effects, externally matched background hazards, and bootstrap prediction uncertainty from actual refits. |

## Observed risk before cure interpretation

In `singlearm` or `survival`, select competing-risk coding and enable **Report all event types**. The internal status convention is 0 = censored, 1 = target event, 2 = competing event(s). The existing outcome mappings determine which original labels enter those categories. A pooled competing category is not an estimate for each individual cause within it.

At each supported time, the three probabilities sum to one. Risk counts describe subjects still observed immediately before that time. Event and censoring counts are cumulative through the time, including tied events. The event-free row's event count includes both terminal event types. Boundary confidence intervals are left unavailable rather than presented as certainty. The confidence intervals are pointwise, not simultaneous bands.

For calculated dates, the existing time conversion still applies. For an already numeric duration column, the selected output unit declares its unit. No table extends a group's observed curve to an unsupported time.

## Cure-model workflow

Set `event_value`, `censor_value`, and, when present, `competing_value` to the original values or factor labels. Factor ordering does not determine event status. Choose the endpoint explicitly:

- `recurrence`: a nonfatal event. If competing deaths are present, the analysis reports observed cumulative incidence and refuses to fit a single-event cure model by censoring those deaths.
- `all_cause_death`: reports observed risk and explains why a positive lifetime survival asymptote is not the requested cure estimand.
- `excess_mortality`: requires `model_type = "cure"`, `use_background_mortality = TRUE`, and an externally matched background-hazard column in the same time units.

The default `prediction_profile = "standardized"` averages individual predicted cure probabilities over the original complete-case cohort. It does not transform an average coefficient. The `reference` choice uses zero for numeric predictors and reference factor levels; `npcure` uses its documented median covariate reference. These profiles are named in the results.

`smcure` incidence coefficients describe susceptibility. Reported cure probabilities are `1 - plogis(Z %*% b)`. Its latency model uses the supported PH or AFT specification and the susceptible-tail convention documented in the results. Only the verified logit incidence link is accepted for this engine. An intercept-only request is rejected with an explanation; use a supported alternative engine where appropriate. Separate incidence and latency predictors are available; an empty latency selection reuses the incidence predictors. The `cuRe` adapter does not accept separate latency predictors.

Enable `bootstrap_ci` for cohort-profile uncertainty. Resampling has a fixed attempt limit, restores the caller's random-number state, and reports successful and attempted fits. Percentile intervals require at least 20 successful fits and 80% of attempts. An intercept-only `flexsurvcure` fit may report its native transformed Wald interval without bootstrap. Missing coefficient intervals and latent susceptible medians mean those quantities are unavailable.

`cure_threshold` is now a **follow-up assessment time**. It counts late support; it does not classify any subject as cured. Reverse-Kaplan–Meier follow-up is left blank when its median is not estimable.

For sensitivity analysis, set `analysis_horizons`, enable `sensitivity_analysis`, and optionally enable alternative latency assumptions. Each supported horizon retains the original cohort, replaces follow-up by `pmin(time, horizon)`, censors later events, and refits the model. The original prediction profiles remain fixed. Failed fits remain visible. AIC is not compared across horizons, and stability is not evidence of cure identification.

## Interval and multistate input

For `intervalsurvival`, provide numeric lower and upper bounds. Equal positive bounds mean an exact event; `(0, upper)` means left censoring; an infinite upper bound means right censoring. Optional status codes are 0 = right, 1 = interval, 2 = left, 3 = exact. With status 0, a missing upper bound is replaced by infinity before completeness checks. No midpoint event imputation is used. A nonparametric model is unadjusted; select PH/AFT for covariates. Survival between Turnbull support endpoints uses the package's interpolation and is not uniquely identified by the NPMLE alone.

The two multistate functions require subject ID, entry and exit times, and states before and after each interval. Use 0 = event-free, 1 = ill, 2 = dead. An unchanged state on the final interval denotes censoring. For example:

| ID | Entry | Exit | From | To |
|---|---:|---:|---:|---:|
| A | 0 | 1 | 0 | 1 |
| A | 1 | 3 | 1 | 2 |
| B | 0 | 2 | 0 | 2 |
| C | 0 | 4 | 0 | 0 |

Histories must be contiguous, non-overlapping, and consistent with the selected transition structure. Missing history rows are not silently deleted. Death is absorbing. All times use a common clock; these are not clock-reset models. Delayed entry is represented by the entry time itself. State occupation in `illnessdeath` is unadjusted for the observed cohort, distinct from its transition regression results. Its restricted mean integrates occupation probabilities only to a supported finite horizon. `flexmultistate` reports model-based matrices and predictions for the stated reference profile and flags their possible extrapolation in the results.

## Flexible and relative survival

For `flexparametric`, choose spline-based modeling to fit spline time-varying effects. Traditional and automatic distribution paths reject that request. Manual knot values are input times and are transformed to log time internally. Curves use numeric predictor means and the most frequent factor levels. Plot line types distinguish observed follow-up from extrapolation; their intervals use the requested confidence level. Parameter-simulation intervals use seed 2026 locally and restore the caller's random-number state.

`flexrstpm2` uses numeric-zero/reference-factor predictions. Its PH, PO, and probit coefficients are not interchangeable hazard ratios. Bootstrap output estimates prediction uncertainty, not external validation or optimism correction. Fixed manual knots disable the automatic degrees-of-freedom comparison. Relative-survival output separates the estimated relative survival/excess hazard from unavailable expected and all-cause survival: a hazard measured at each subject's event time does not supply a future population survival curve.

## Remaining limits and release status

Joint interval-censored recurrence with competing death, automatic life-table matching and future population survival integration, semi-Markov/clock-reset models, shared transition effects, frailty, and predictive cross-validation require separate methods and validation. Unsupported legacy controls are hidden using the compiler's `hidden` option; incompatible modeling requests return an explicit explanation. Unimplemented plots are hidden, and unsupported inferential quantities are unavailable.

The fix workflow leaves affected menu groups in their repository-required `T` testing routes. Draft analyses retain their draft routing. These changes are not a declaration of release readiness: interactive jamovi/OMV persistence and packaged submodule release checks remain separate.

## Validation

The new tests source the changed analyses and helpers directly, avoiding accidental validation of an older installed module:

- `tests/testthat/test-curemodels-observed-risk.R`: susceptibility complement and profiles, real cure engines, genuine horizon refits, bounded seeded bootstrap, endpoint guards, and hand-calculated CIF examples.
- `tests/testthat/test-survival-cure-gap-extensions.R`: independently optimized interval likelihood, four-subject Aalen–Johansen and constant-rate multistate matrix examples, trajectory bootstrap, public wrappers, supported spline scales, time-varying effects, and direct prediction comparisons.

The new suites and final targeted checks passed **263 expectations across 34 new tests** (70 core, 193 extension), with no failures, errors, warnings, or skips. The CIF renderer was checked with and without its risk panel, including the actual displayed counts. The legacy cure file passed 30 expectations; its two tests needing installed-package data were then checked against the repository fixture, adding five successful assertions. Altogether these focused checks passed 298 assertions across 51 tests. All seven offered interval model choices also produced finite fitted survival probabilities in a mixed-censoring smoke check.

Seven existing survival regression files passed **334 expectations**, with no failures, errors, test warnings, or skips. A descriptive benchmark with 50,000 observations, two groups, and six times produced 36 rows in 15.7 seconds on the current machine; probabilities summed to one at every group/time. This is a local benchmark, not a platform guarantee.

The official jamovi compiler regenerated the affected headers and module menu metadata. `devtools::document()` completed; its package-loading warnings concerned the existing TMB/glmmTMB version mismatch and unavailable XQuartz display. Numerical tests do not replace interactive release verification.
