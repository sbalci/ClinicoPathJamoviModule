# Conditional Survival Estimation Documentation

## Overview

Conditional survival estimation calculates the probability of surviving beyond a specific time point **given that the patient has already survived** to a conditioning time: P(T > t | T > s). This is a fundamental tool in clinical oncology for updating prognosis as patients accumulate disease-free follow-up. For example, a colon cancer patient who has already survived 2 years post-surgery has a different (typically better) 5-year survival probability than what was predicted at diagnosis.

The `conditionalsurvival` function in ClinicoPath (module: jsurvival, menu: SurvivalD > ClinicoPath Survival) implements this analysis with support for multiple estimation methods: Kaplan-Meier weights, landmark subsetting, inverse probability weighting, and presmoothed Kaplan-Meier. Results are presented as a probability table, a step-function survival plot comparing overall and conditional curves, method explanations, a copy-ready report sentence, and an assumptions panel.

The analysis accepts both numeric (0/1) and factor (2-level) outcome variables, automatically validates event counts and group sizes, and provides sensible defaults (median follow-up as conditioning time, auto-generated time points). Stratified analysis by any grouping variable (e.g., treatment arm, disease stage) is supported through the optional conditioning variable.

## Feature Summary

**Input Variables**: Time, event/status, and an optional grouping variable for stratified conditional survival curves.

**Analysis Options**: Four estimation methods, configurable conditioning time (defaults to median follow-up), custom time points, adjustable confidence level, and a bandwidth parameter reserved for future kernel smoothing.

**Core Results**: A table of conditional survival probabilities with standard errors and confidence intervals, plus a survival plot overlaying overall and conditional curves.

**Explanatory Output**: Method-specific interpretation, a copy-ready report sentence for manuscripts, and an assumptions/caveats panel.

**Display Options**: Independent toggles for the table, plot, and explanatory text panels.

## Feature Details

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Survival time | `timeVar` (Variable, suggested: continuous, permitted: numeric) | Time Variable | Used in all results | `.run()` -- `data[[timeVar]]` |
| Event indicator | `outcomeVar` (Variable, suggested: ordinal/nominal, permitted: numeric/factor) | Event/Status Variable | Used in all results | `.run()` -- handles factor (2-level) and numeric (0/1) |
| Grouping variable | `conditionVar` (Variable, suggested: ordinal/nominal/continuous, permitted: numeric/factor) | Conditioning Variable (optional) | `condsurvTable` group column visible when set | `.run()` -- stratified loop over `levels(as.factor(...))` |
| **Analysis Options** | | | | |
| Conditioning time | `conditionTime` (Number, default: 0) | Conditioning Time Point | `condsurvTable` condtime column | `.run()` -- 0/NULL/NA uses `median(time)` |
| Estimation method | `method` (List: km/landmark/ipw/pkm, default: km) | Estimation Method | `methodExplanation` | `.computeCondSurv()` dispatches to `.runCondSurv()` or `.runManualCondSurv()` |
| Smoothing bandwidth | `bandwidth` (Number, default: 0) | Bandwidth (for smoothing methods) | N/A (read but unused) | `.run()` -- reserved for future PKM smoothing |
| Confidence level | `confInt` (Number, default: 0.95, min: 0.01, max: 0.99) | Confidence Level | `condsurvTable` lower/upper columns | `.runManualCondSurv()` -- `qnorm(1 - (1-confInt)/2)` |
| Custom time points | `timePoints` (String, default: '') | Specific Time Points | `condsurvTable` time column | `.parseTimePoints()` -- comma-separated, falls back to `.getDefaultTimePoints()` |
| Plot style | `plotType` (List: curves/probability/both, default: curves) | Plot Type | `survplot` clearWith | `.plot()` -- TODO: always draws curves style |
| **Core Results** | | | | |
| Getting started guide | N/A | N/A | `todo` (Html, always visible) | `.initTodo()` |
| Conditional survival table | N/A | N/A | `condsurvTable` (Table, 7 columns: group/time/condtime/condprob/se/lower/upper) | `.populateCondSurvTable()` |
| Survival plot | N/A | N/A | `survplot` (Image, 600x450, renderFun: `.plot`) | `.createCondSurvPlot()` -- ggplot2 step plot |
| **Display Options** | | | | |
| Show results table | `showTable` (Bool, default: true) | Show Results Table | `condsurvTable` visible: `(showTable)` | N/A (visibility only) |
| Show survival plot | `showPlot` (Bool, default: true) | Show Survival Plot | `survplot` visible: `(showPlot)` | N/A (visibility only) |
| Show explanations | `showExplanations` (Bool, default: true) | Show Explanations | `methodExplanation` visible: `(showExplanations)` | `.init()` calls `.initMethodExplanation()` when true |
| **Explanatory Output** | | | | |
| Method explanation | N/A | N/A | `methodExplanation` (Html) | `.updateMethodExplanation()` -- method-specific description + summary stats |
| Report sentence | N/A | N/A | `reportSentence` (Html, always visible) | `.generateReportSentence()` -- copy-ready text with CI |
| Assumptions panel | N/A | N/A | `assumptions` (Html, always visible) | `.populateAssumptions()` -- 5 bullet points |

## Method Dispatch Logic

| `method` value | `condSURV` available | Actual path |
|----------------|---------------------|-------------|
| `km` | Yes | `.runCondSurv()` -- `condSURV::KMW()` then `.calculateCondSurvFromWeights()` |
| `km` | No | `.runManualCondSurv()` -- S(t)/S(s) ratio from `survival::survfit()` |
| `landmark` | Yes | `.runCondSurv()` -- subsets data to T >= condTime, refits KM on adjusted times |
| `landmark` | No | `.runManualCondSurv()` -- fallback |
| `ipw` | Yes | `.runCondSurv()` -- `.calculateIPWCondSurv()` (stub, falls back to manual) |
| `ipw` | No | `.runManualCondSurv()` -- fallback |
| `pkm` | Yes | `.runCondSurv()` -- falls through to `.runManualCondSurv()` (not yet implemented) |
| `pkm` | No | `.runManualCondSurv()` -- fallback |

## Validation Rules

| Check | Threshold | Error behavior |
|-------|-----------|---------------|
| Factor outcome levels | Must be exactly 2 | `jmvcore::reject()` |
| Numeric outcome values | Must be only 0 and 1 | `jmvcore::reject()` |
| Minimum event count | >= 5 events overall | `jmvcore::reject()` |
| Conditioning variable levels | >= 2 levels when provided | `jmvcore::reject()` |
| Group event count | >= 3 events per group | Group silently skipped |
| Conditioning time vs max follow-up | condTime < max(time) | `jmvcore::reject()` |
| Landmark subjects | >= 10 subjects surviving to condTime | `stop()` in `.runCondSurv()` |

## Dependencies

| Package | Role | Required |
|---------|------|----------|
| `survival` | `Surv()`, `survfit()` for KM estimation | Yes |
| `ggplot2` | Conditional survival plots | Yes |
| `scales` | `percent` formatter for y-axis | Yes |
| `condSURV` | `KMW()` for KM weights method | Optional (fallback to manual) |

## Known Limitations

1. **`bandwidth`**: Read from options but never used in any computation path. Reserved for future PKM kernel smoothing.
2. **`plotType`**: Read from options but `.plot()` always renders the curves style. The probability and both options have no effect.
3. **IPW method**: `calculateIPWCondSurv()` is a stub that immediately delegates to `.runManualCondSurv()`.
4. **PKM method**: When `condSURV` is not available (common), falls through to manual KM ratio. Even with `condSURV`, the pkm branch falls to the else clause in `.runCondSurv()`.
5. **SE calculation**: Uses delta method approximation for the ratio S(t)/S(s), which can be imprecise with small risk sets.
