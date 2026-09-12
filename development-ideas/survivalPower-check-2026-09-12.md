# survivalPower standard check — 2026-09-12

Standard profile; fixes applied. All 37 options have backend references and UI controls.
There are 24 result items, each with a population path. This analysis takes design
parameters, not dataset variables: variable-name escaping and labelled-factor parity are
not applicable. Empty-data runs are covered. All seven checkbox defaults remain false.

## Corrections

- Applied the selected multiplicity adjustment to non-inferiority sample size, power,
  detectable HR, duration, interim boundaries, and the non-inferiority table. For a
  three-arm trial at familywise one-sided alpha 0.025, true HR 1, margin 1.25 and 80%
  power (other defaults), required enrollment changes from **1,352 to 1,635**. The
  per-comparison alpha is 0.0125.
- Used the non-inferiority margin in multi-arm comparison power. The same example's
  comparison table changes from **0% to approximately 80%**. Its power curve now uses
  the calculated enrollment instead of the unrelated 200-subject input.
- Corrected the shared-control correlation under unequal allocation from
  `ratio / (1 + ratio)` to `1 / (1 + ratio)` in the normal approximation.
- Applied sequential information inflation once in plot calculations and preserved it
  in Cox fallback calculations. Sensitivity scenarios now retain design factors and
  use the scenario's alpha for sequential calculations.
- Included both rejection tails in two-sided normal-approximation power. At HR = 1,
  power is alpha rather than zero. Checked against the noncentral chi-square tail.
- Failed calculations now produce an error notice and stop before derived narratives,
  plots, simulations or an “Analysis Complete” message are generated.
- Expanded result invalidation to every design/validation dependency. Explicitly reset
  requested HTML narratives before validation because jamovi can restore old HTML
  content even when `clearWith` marks the result unfilled. Regression coverage checks
  unchanged save/restore and transitions to unsupported accrual/design settings.
- Restricted simulation validation to the fixed two-arm superiority designs it actually
  simulates; unsupported combinations receive an explanatory notice. Simulation rows
  are created only in `.init()` and populated with `setRow()`.
- Presets reset the analysis type, distribution, accrual pattern, interim count and
  spending function, so unsupported settings from a previous design cannot carry over.
- Routed the analysis to `PowerT` under the repository's JamoviTest rule, including its
  generated menu entry. Regenerated `R/survivalPower.h.R` and `man/survivalPower.Rd`.

## Argument effects

| Inputs | Default → checked alternative | Effect |
|---|---|---|
| Analysis type | Sample size → power, detectable HR, duration | Appropriate primary result and detail table |
| Test type | Log-rank → Cox, non-inferiority | Calculation and report reflect the selected test |
| Effect specification | HR 0.75 → median ratio 1.5; HR 1 | Conversion is respected; null power equals alpha |
| Multiplicity/design | Two-arm → three-arm, Bonferroni; cluster design | Enrollment, power, tables and scenarios retain design adjustments |
| Interim looks | 0 → 2 with O'Brien–Fleming/Pocock | Boundaries, information inflation and curves respond |
| Alpha scenario | Current alpha → 0.02 | Scenario agrees with a separate calculation at that alpha |
| Clinical presets | Custom → each worked example | JavaScript handler resets supported assumptions |
| Simulation | Off → on | Runs for supported fixed two-arm sample-size/power designs; other designs are refused |
| Narrative toggles | False → true | Requested content is populated and cleared after invalid changes |
| Distribution/accrual | Exponential/uniform → unsupported choices | Error notice, no stale numeric or narrative output |

`weibull_shape` is referenced only by distribution helpers; non-exponential analysis
remains explicitly blocked. The standard profile does not implement unavailable methods
or change the existing panel organization.

## Output population

| Result group | Count | Population |
|---|---:|---|
| Notices and instructions | 2 | `setContent()` |
| Primary summary and four analysis-specific tables | 5 | `setRow()` |
| Simulation, assumptions, specialized, sensitivity and regulatory tables | 7 | Option-determined rows in `.init()`; `setRow()` in `.run()` |
| Plot outputs | 5 | `setState()` plus guarded renderers |
| Optional narrative outputs | 5 | Toggle-controlled `setContent()` |

No result item lacks a setter/renderer. Five populated plots and five NULL-state renderer
calls were exercised successfully. The module-wide results-rendering contract passed.

## Validation and build limitations

- Final targeted test result: **716 assertions passed; 0 failures, 0 errors, 0 warnings, 0 skips** across all nine survivalPower test files with the package namespace loaded.
- Runtime output-population sweep: **24/24 outputs populated** across supported configurations. The simulation events row intentionally has no confidence interval; both analytical and simulated values were verified as finite.
- Targeted whitespace, state-guard, HTML-entity and theme checks passed; runtime `::`
  packages are declared in Imports. No runtime `warning()` or failure-driven hiding.
- `jmvtools::prepare()` was attempted and retried with access to the installed app. It
  exits with `jamovi could not be accessed` because the app's `--version` invocation
  returns no parsable version. The installed app's Info.plist reports 28.2.0.0.
- The same installed compiler completed the full source-copy prepare successfully with
  `--assume-app-version 28.2.0`. This validates the schemas, UI and generated headers;
  it does not establish a successful desktop launch.
- `devtools::document()` completed in the full temporary source copy. Environment
  warnings concern glmmTMB/TMB version mismatch and unavailable XQuartz; neither comes
  from survivalPower. Only this analysis's generated artifacts/menu entry were copied
  back, preserving pre-existing work elsewhere in the repository.

This is a standard check, not a release certification. Exponential survival and uniform
accrual remain required; competing-risks, RMST, SNP and weighted log-rank tests remain
unavailable. Holm and Dunnett choices still use the disclosed conservative Bonferroni
approximation, and advanced designs retain the module's analytical approximations.

The multiplicity and one-sided-alpha checks were cross-checked with primary documentation:
[R's multiplicity adjustments](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html)
and [gsDesign's time-to-event design arguments](https://keaven.github.io/gsDesign/reference/nSurv.html).
The broad external audit/release profile was not run.
