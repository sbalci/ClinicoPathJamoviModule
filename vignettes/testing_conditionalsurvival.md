# Testing Checklist: Conditional Survival Estimation (`conditionalsurvival`)

This document provides a comprehensive testing plan for the `conditionalsurvival` jamovi function. Each scenario specifies the dataset, variable assignments, option values, and expected behavior.

## Datasets

| Dataset | Source | Key Variables |
|---------|--------|---------------|
| `conditionalsurvival_test` | `data("conditionalsurvival_test")` (150 patients, colorectal cancer, Weibull survival) | `OverallTime`, `Event` (0/1), `EventFactor` (Alive/Dead), `Treatment` (Surgery / Surgery+Chemo), `Stage` (I/II/III/IV), `Grade` (Well/Moderate/Poor), `Age`, `Sex`, `CEA` |
| `histopathology` | Built-in ClinicoPath dataset | Varies; check available time/event columns |
| Inline simple data | `make_simple_data()` in test file (80 patients, exponential survival) | `time`, `event` (0/1), `group` (A/B), `stage` (I/II/III) |

---

## 1. Standard Clinical Analysis

### 1.1 Unstratified conditional survival (overall cohort)

| # | Scenario | Dataset | timeVar | outcomeVar | conditionVar | conditionTime | method | Expected |
|---|----------|---------|---------|------------|--------------|---------------|--------|----------|
| 1.1.1 | Basic unstratified, default condTime | `conditionalsurvival_test` | `OverallTime` | `Event` | (empty) | 0 | km | Table with ~5 rows, condtime = median follow-up, probabilities in [0,1], plot shows overall + conditional curves |
| 1.1.2 | Unstratified, fixed condTime = 12 months | `conditionalsurvival_test` | `OverallTime` | `Event` | (empty) | 12 | km | condtime column = 12 in all rows, conditional probs higher than unconditional |
| 1.1.3 | Unstratified, condTime = 24 months | `conditionalsurvival_test` | `OverallTime` | `Event` | (empty) | 24 | km | Fewer time points beyond 24, conditional probs reflect updated prognosis |

### 1.2 Stratified by Treatment

| # | Scenario | Dataset | timeVar | outcomeVar | conditionVar | conditionTime | method | Expected |
|---|----------|---------|---------|------------|--------------|---------------|--------|----------|
| 1.2.1 | By Treatment, condTime = 12 | `conditionalsurvival_test` | `OverallTime` | `Event` | `Treatment` | 12 | km | Group column visible with "Surgery" and "Surgery+Chemo" rows, separate conditional probs per group |
| 1.2.2 | By Treatment, condTime = 0 (median) | `conditionalsurvival_test` | `OverallTime` | `Event` | `Treatment` | 0 | km | condTime auto-calculated, both groups present |

### 1.3 Stratified by Stage

| # | Scenario | Dataset | timeVar | outcomeVar | conditionVar | conditionTime | method | Expected |
|---|----------|---------|---------|------------|--------------|---------------|--------|----------|
| 1.3.1 | By Stage, condTime = 12 | `conditionalsurvival_test` | `OverallTime` | `Event` | `Stage` | 12 | km | Four groups (I, II, III, IV), groups with <3 events silently skipped |
| 1.3.2 | By Stage, condTime = 6 | `conditionalsurvival_test` | `OverallTime` | `Event` | `Stage` | 6 | landmark | Landmark method subsets to T >= 6, Stage I group may have few subjects |

---

## 2. Estimation Methods

### 2.1 Kaplan-Meier Weights (default)

| # | Scenario | method | Expected |
|---|----------|--------|----------|
| 2.1.1 | KM with condSURV available | km | Uses `condSURV::KMW()` weights, method explanation says "Kaplan-Meier Weights" |
| 2.1.2 | KM without condSURV | km | Falls back to manual S(t)/S(s) ratio, same table structure |

### 2.2 Landmark Approach

| # | Scenario | method | conditionTime | Expected |
|---|----------|--------|---------------|----------|
| 2.2.1 | Landmark, condTime = 12 | landmark | 12 | Subsets to patients with OverallTime >= 12, adjusts times by -12, refits KM |
| 2.2.2 | Landmark, condTime = 6 | landmark | 6 | More subjects available, wider time range |
| 2.2.3 | Landmark, condTime very late | landmark | 60 | May have <10 subjects, expect error "Insufficient subjects surviving to landmark time" |

### 2.3 Inverse Probability Weighting

| # | Scenario | method | Expected |
|---|----------|--------|----------|
| 2.3.1 | IPW, condTime = 12 | ipw | Stub: falls back to manual KM ratio. Results identical to manual KM method. Method explanation says "Inverse Probability Weighting" |

### 2.4 Presmoothed Kaplan-Meier

| # | Scenario | method | Expected |
|---|----------|--------|----------|
| 2.4.1 | PKM, condTime = 12 | pkm | Falls through to manual KM ratio. Results identical to manual KM. Method explanation says "Presmoothed Kaplan-Meier" |

---

## 3. Custom Configuration

### 3.1 Custom Time Points

| # | Scenario | timePoints | conditionTime | Expected |
|---|----------|------------|---------------|----------|
| 3.1.1 | Specific time points | `"12,24,36,48,60"` | 6 | Table has exactly 5 rows at times 12, 24, 36, 48, 60 |
| 3.1.2 | Sparse time points | `"24,60"` | 12 | Table has exactly 2 rows |
| 3.1.3 | Time points before condTime | `"6,12,24,36"` | 18 | Times 6 and 12 have condprob = 1.0, se = 0, CI = [1,1] |
| 3.1.4 | Single time point | `"36"` | 12 | Table has 1 row |
| 3.1.5 | Empty string (default) | `""` | 12 | Auto-generated via `.getDefaultTimePoints()`, ~5 evenly spaced points from condTime to max(time) |

### 3.2 Confidence Level Variations

| # | Scenario | confInt | Expected |
|---|----------|--------|----------|
| 3.2.1 | 95% CI (default) | 0.95 | Standard width intervals, z = 1.96 |
| 3.2.2 | 90% CI | 0.90 | Narrower intervals than 95%, z = 1.645 |
| 3.2.3 | 99% CI | 0.99 | Wider intervals, z = 2.576 |
| 3.2.4 | 80% CI | 0.80 | Noticeably narrow intervals |
| 3.2.5 | Minimum CI | 0.01 | Very narrow, nearly point estimates |

### 3.3 Conditioning Time Variations

| # | Scenario | conditionTime | Expected |
|---|----------|---------------|----------|
| 3.3.1 | Zero (auto = median) | 0 | Uses `median(OverallTime)` as conditioning time |
| 3.3.2 | Early conditioning | 3 | Many subjects at risk, stable estimates |
| 3.3.3 | Mid-range conditioning | 24 | Moderate risk set |
| 3.3.4 | Late conditioning | 60 | Small risk set, wider CIs, may have few time points |
| 3.3.5 | Very late conditioning | 100 | May trigger reject if >= max(time) |

---

## 4. Outcome Variable Handling

| # | Scenario | outcomeVar | Type | Expected |
|---|----------|------------|------|----------|
| 4.1 | Numeric 0/1 | `Event` | integer | Direct use, 0 = censored, 1 = event |
| 4.2 | Factor 2-level (Alive/Dead) | `EventFactor` | factor | Converted via `as.numeric(status) - 1`, first level = 0 (censored) |
| 4.3 | Factor 2-level (custom labels) | Create: `factor(Event, labels=c("No","Yes"))` | factor | Same conversion, first level = censored |
| 4.4 | Numeric with only 0s and 1s present | Subset where both values exist | numeric | Passes validation |

---

## 5. Edge Cases

### 5.1 Event Count Validation

| # | Scenario | Setup | Expected |
|---|----------|-------|----------|
| 5.1.1 | Too few events overall (<5) | Data with only 3 events out of 50 | `jmvcore::reject()`: "Too few events (3). Conditional survival requires at least 5 events..." |
| 5.1.2 | Exactly 5 events | Data with exactly 5 events | Should pass validation and produce results |
| 5.1.3 | Group with <3 events | Stratified by variable where one group has 2 events | That group silently skipped, other groups shown |
| 5.1.4 | All groups with <3 events | Extreme case | Error: "No groups had enough events (>=3)..." |

### 5.2 Conditioning Variable Edge Cases

| # | Scenario | conditionVar | Expected |
|---|----------|-------------|----------|
| 5.2.1 | Single-level factor | Variable where all values are the same (e.g., `"OnlyGroup"`) | `jmvcore::reject()`: "fewer than 2 levels" |
| 5.2.2 | 3-level factor | `Stage` with levels I/II/III | Works, one row per group per time point |
| 5.2.3 | 4-level factor | `Stage` with levels I/II/III/IV | Works, all four groups |
| 5.2.4 | Numeric grouping variable | `Age` (continuous) | Treated as factor with many levels; may produce many groups or fail if too many |
| 5.2.5 | No conditioning variable | (empty) | Unstratified analysis, group = "Overall" |

### 5.3 Conditioning Time Edge Cases

| # | Scenario | conditionTime | Expected |
|---|----------|---------------|----------|
| 5.3.1 | condTime = max follow-up | Set to max(OverallTime) | `jmvcore::reject()`: "Conditioning time ... is at or beyond maximum follow-up" |
| 5.3.2 | condTime > max follow-up | Set to max(OverallTime) + 10 | Same reject as above |
| 5.3.3 | condTime = 0 | 0 | Uses median(time) as default |
| 5.3.4 | Negative condTime | -5 | Treated as <= 0, uses median(time) |

### 5.4 Outcome Variable Edge Cases

| # | Scenario | outcomeVar | Expected |
|---|----------|------------|----------|
| 5.4.1 | 3-level factor | `factor(sample(c("A","B","C"), n, replace=TRUE))` | `jmvcore::reject()`: "must have exactly 2 levels (got 3)" |
| 5.4.2 | Non-binary numeric | Values include 0, 1, 2, 3 | `jmvcore::reject()`: "must contain only 0 (censored) and 1 (event)" |
| 5.4.3 | All events (no censoring) | All values = 1 | Should run but unusual; all subjects are events |
| 5.4.4 | All censored (no events) | All values = 0 | Rejected: "Too few events (0)" |

### 5.5 Time Points Edge Cases

| # | Scenario | timePoints | Expected |
|---|----------|------------|----------|
| 5.5.1 | Empty string | `""` | Auto-generated default time points |
| 5.5.2 | Non-numeric values | `"a,b,c"` | Parsed to NA, filtered out, falls back to default |
| 5.5.3 | Mixed valid/invalid | `"12,abc,36"` | Keeps 12 and 36, drops invalid |
| 5.5.4 | All time points <= condTime | `"3,6,9"` with conditionTime=12 | All rows have condprob = 1.0 |
| 5.5.5 | Very large time points | `"200,300"` with max follow-up ~120 | condprob near 0 or exactly 0 |

---

## 6. Display Options

| # | Scenario | showTable | showPlot | showExplanations | Expected |
|---|----------|-----------|----------|------------------|----------|
| 6.1 | All on (defaults) | TRUE | TRUE | TRUE | Table visible, plot rendered, method explanation + assumptions shown |
| 6.2 | All off | FALSE | FALSE | FALSE | All optional panels hidden; todo, reportSentence, assumptions still visible (always visible) |
| 6.3 | Table only | TRUE | FALSE | FALSE | Only table shown, plot not rendered |
| 6.4 | Plot only | FALSE | TRUE | FALSE | Only plot rendered, table hidden |
| 6.5 | Explanations only | FALSE | FALSE | TRUE | Method explanation shown, table and plot hidden |
| 6.6 | Table + explanations | TRUE | FALSE | TRUE | Table and explanation panels, no plot |

---

## Complete Option Coverage Checklist

Every `.a.yaml` option must be exercised in at least one test scenario above.

| Option | Type | Default | Tested In |
|--------|------|---------|-----------|
| `timeVar` | Variable | (required) | All scenarios (1.1.1--6.6) |
| `outcomeVar` | Variable | (required) | All scenarios; factor handling in 4.1--4.4; edge cases in 5.4.1--5.4.4 |
| `conditionVar` | Variable | (optional) | Stratified: 1.2.x, 1.3.x; edge cases: 5.2.1--5.2.5; unstratified: 1.1.x |
| `conditionTime` | Number | 0 | Auto (0): 1.1.1, 3.3.1; fixed: 1.1.2, 1.1.3; edge cases: 5.3.1--5.3.4 |
| `method` | List | km | KM: 2.1.x; landmark: 2.2.x; ipw: 2.3.1; pkm: 2.4.1 |
| `bandwidth` | Number | 0 | Read but unused; implicitly tested whenever method=pkm (2.4.1) |
| `confInt` | Number | 0.95 | Default: most scenarios; variations: 3.2.1--3.2.5 |
| `timePoints` | String | '' | Custom: 3.1.1--3.1.5; edge cases: 5.5.1--5.5.5 |
| `plotType` | List | curves | Implicitly tested in all showPlot=TRUE scenarios; TODO: no visual difference for probability/both |
| `showTable` | Bool | TRUE | Display: 6.1--6.6 |
| `showPlot` | Bool | TRUE | Display: 6.1--6.6 |
| `showExplanations` | Bool | TRUE | Display: 6.1--6.6 |

---

## Notes

- The `bandwidth` option is read but never used in any code path. Testing confirms it does not cause errors when set to various values (0, 0.5, 1.0).
- The `plotType` option is read but `.plot()` always renders the curves style. Setting it to "probability" or "both" produces the same plot.
- IPW and PKM methods both fall back to the manual KM ratio calculation, so their numerical results are identical to the manual fallback.
- The `condSURV` package is optional. When not installed, all methods use the manual S(t)/S(s) ratio approach.
- The `reportSentence` and `assumptions` results are always visible regardless of display option settings.
