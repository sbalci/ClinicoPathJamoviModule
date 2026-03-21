# Testing Checklist: survival

**Function:** `survival()`
**Module:** ClinicoPath Survival (SurvivalT submenu)
**Last updated:** 2026-03-22

---

## Available Test Datasets

| Dataset | File | n | Key Variables | Purpose |
|---------|------|---|---------------|---------|
| `survival_test` | `data/survival_test.rda` | ~200 | `elapsedtime`, `outcome`, `treatment` (3 levels), `stage` (4 levels), `sex` | General-purpose baseline test data with binary outcome |
| `survival_comprehensive` | `data/survival_comprehensive.rda` | 200 | `FollowUpMonths`, `Status` (4-level), `TumorStage`, `Age`, `Sex`, `TumorGrade`, `Ki67` (~5% NA), `TumorSize` (~5% NA), `LymphovascularInvasion`, `MarginStatus`, `DiagnosisDate`, `LastFollowUpDate`, `HER2Status`, `ERStatus` | Realistic Weibull-simulated clinicopathological dataset; covers competing risks, date-based time, age adjustment, RCS, calibration, and all plot types |
| `survival_dates` | `data/survival_dates.rda` | ~200 | `dxdate`, `fudate`, `outcome`, `treatment` | Date-based time calculation testing (YMD format) |
| `survival_competing` | `data/survival_competing.rda` | ~200 | `elapsedtime`, `outcome` (Dead of Disease / Dead of Other / Alive w Disease / Alive w/o Disease), `treatment` | Competing risks and cause-specific analysis |
| `survival_landmark` | `data/survival_landmark.rda` | ~200 | `elapsedtime`, `outcome`, `treatment`, `response_6mo` | Landmark analysis with 6-month response variable |
| `survival_stratified` | `data/survival_stratified.rda` | ~200 | `elapsedtime`, `outcome`, `treatment`, `sex` | Stratified Cox regression testing |
| `survival_person_time` | `data/survival_person_time.rda` | ~200 | `elapsedtime`, `outcome`, `risk_category` | Person-time metrics and incidence rate calculation |
| `survival_rmst` | `data/survival_rmst.rda` | ~200 | `elapsedtime`, `outcome`, `treatment` | Restricted Mean Survival Time analysis |
| `survival_rmst_test` | `data/survival_rmst_test.rda` | 80 | `FollowUpMonths`, `Status`, `Treatment` | Short follow-up RMST for tau default behavior testing |
| `survival_small` | `data/survival_small.rda` | ~20-30 | `elapsedtime`, `outcome`, `treatment` | Small sample edge case testing |
| `survival_low_epv` | `data/survival_low_epv.rda` | 60 | `FollowUpMonths`, `Status`, `TumorSubtype` (6 levels), `Age` | Low events-per-variable scenario (EPV ~2); triggers EPV warnings |
| `survival_extreme_hr` | `data/survival_extreme_hr.rda` | 100 | `FollowUpMonths`, `Status`, `RiskGroup` (Common/Rare), `Age` | Extreme hazard ratio detection (HR > 10); rare subgroup convergence |
| `survival_analysis_data` | `data/survival_analysis_data.rda` | varies | varies | Additional general-purpose survival analysis data |

---

## Existing Test Files

| Test File | Focus | Key Tests |
|-----------|-------|-----------|
| `tests/testthat/test-survival.R` | Original mixed tests | Basic KM, date-based, person-time, RMST, stratified Cox, landmark, pairwise, residual diagnostics, PH test, plots, multi-event, comprehensive combo, performance (n=1000) |
| `tests/testthat/test-survival-basic.R` | Basic functionality | Function existence, minimal args, explanatory variable types (binary/ordinal/multi-level), output structure, small dataset, no-grouping scenario |
| `tests/testthat/test-survival-arguments.R` | Option combinations | Date-based time, competing risks, landmark, stratified Cox, person-time, RMST, all plot types (sc/kmunicate/ce/ch/loglog), plot options (ci95/risktable/censored/pplot/medianline), PH test, pairwise with different adjustment methods, residual diagnostics, explanations, summaries, cutpoints, cause-specific, time output types |
| `tests/testthat/test-survival-edge-cases.R` | Boundary conditions | NA in time/outcome, all censored, all events, zero/negative time, very small/large times, single observation, small sample, single group, unbalanced groups, group with no events, landmark beyond max, invalid dates, duplicates, tied times, invalid outcome levels, missing strata variable, extreme y-axis, short plot time, constant strata, >4 outcome levels, RMST tau=0, person-time with zero-event interval |
| `tests/testthat/test-survival-integration.R` | Workflows and consistency | Run reproducibility, progressive workflows (basic to plots to diagnostics), date-based workflow, competing risks workflow, landmark workflow, stratified Cox workflow, person-time workflow, RMST workflow, CSV/Excel import, tibble vs data.frame, publication-ready combo, subgroup analysis, sensitivity cutpoints, analysis type comparison (overall/cause/compete) |
| `tests/testthat/test-survival-comprehensive.R` | Full feature coverage | All 25 feature areas using `survival_comprehensive` dataset; data integrity checks (dimensions, variable names, factor levels, value ranges, missingness) |
| `tests/testthat/test-survival-safety.R` | Clinical safety checks | Negative time validation, <10 events blocking, RMST default tau, variable names with spaces, weighted log-rank, bootstrap validation, calibration curves, PH assumption, pairwise, plot options, person-time, landmark |

---

## Test Coverage Matrix

| # | Feature | Option(s) | Recommended Dataset | Test File(s) | Status |
|---|---------|-----------|---------------------|--------------|--------|
| 1 | **Basic KM analysis** | `elapsedtime`, `outcome`, `explanatory` | `survival_test`, `survival_comprehensive` | `test-survival-basic.R`, `test-survival-comprehensive.R` | Covered |
| 2 | **Cox regression** | (automatic with KM when explanatory provided) | `survival_test`, `survival_comprehensive` | `test-survival-basic.R`, `test-survival-safety.R` | Covered |
| 3a | **Survival plot (KM curve)** | `sc = TRUE` | `survival_test` | `test-survival-arguments.R` | Covered |
| 3b | **Cumulative events plot** | `ce = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 3c | **Cumulative hazard plot** | `ch = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 3d | **KMunicate-style plot** | `kmunicate = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 3e | **Log-log plot** | `loglog = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 4a | **95% CI on plot** | `sc = TRUE`, `ci95 = TRUE` | `survival_test` | `test-survival-arguments.R` | Covered |
| 4b | **Risk table** | `sc = TRUE`, `risktable = TRUE` | `survival_test` | `test-survival-arguments.R` | Covered |
| 4c | **Censored marks** | `sc = TRUE`, `censored = TRUE` | `survival_test` | `test-survival-arguments.R` | Covered |
| 4d | **P-value on plot** | `sc = TRUE`, `pplot = TRUE` | `survival_test` | `test-survival-arguments.R` | Covered |
| 4e | **Median survival line** | `sc = TRUE`, `medianline = "hv"` | `survival_test` | `test-survival-arguments.R` | Covered |
| 5a | **Plot end time** | `endplot = <int>` | `survival_test` | `test-survival-arguments.R`, `test-survival-edge-cases.R` | Covered |
| 5b | **Plot time interval** | `byplot = <int>` | `survival_test` | `test-survival-arguments.R` | Covered |
| 5c | **Y-axis range** | `ybegin_plot`, `yend_plot` | `survival_test` | `test-survival-edge-cases.R` | Covered |
| 6 | **Survival probability tables** | `cutp = "12, 36, 60"` | `survival_test` | `test-survival-arguments.R`, `test-survival-integration.R` | Covered |
| 7a | **Pairwise comparisons** | `pw = TRUE`, `padjustmethod` | `survival_test` | `test-survival-arguments.R`, `test-survival-safety.R` | Covered |
| 7b | **All p-value adjustment methods** | `padjustmethod = "holm"/"bonferroni"/"BH"/"fdr"/"none"/...` | `survival_test` | `test-survival-arguments.R` | Partial -- holm, bonferroni, fdr tested; hochberg, hommel, BY not individually tested |
| 8 | **PH assumption test** | `ph_cox = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R`, `test-survival-safety.R` | Covered |
| 9 | **Stratified Cox** | `stratified_cox = TRUE`, `strata_variable` | `survival_stratified`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 10a | **Person-time analysis** | `person_time = TRUE` | `survival_person_time`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R`, `test-survival-safety.R` | Covered |
| 10b | **Custom time intervals** | `time_intervals = "6, 12, 24, 48"` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 10c | **Rate multiplier** | `rate_multiplier = 100/1000` | `survival_person_time`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 11a | **RMST analysis** | `rmst_analysis = TRUE` | `survival_rmst`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 11b | **RMST custom tau** | `rmst_tau = <number>` | `survival_rmst` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 11c | **RMST default tau (tau=0)** | `rmst_tau = 0` | `survival_test`, `survival_comprehensive` | `test-survival-edge-cases.R`, `test-survival-safety.R` | Covered |
| 12 | **Residual diagnostics** | `residual_diagnostics = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 13a | **Weighted log-rank** | `weightedLogRank = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-safety.R`, `test-survival-comprehensive.R` | Covered |
| 13b | **All survival test types** | `survivalTestType = "logrank"/"gehan_breslow"/"tarone_ware"/"peto_peto"/"fleming_harrington"` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered (loop over all 5) |
| 14a | **Bootstrap validation** | `bootstrapValidation = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-safety.R`, `test-survival-comprehensive.R` | Covered |
| 14b | **Bootstrap N** | `bootstrapValN = 50/200` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 15a | **Calibration curves** | `calibration_curves = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-safety.R`, `test-survival-comprehensive.R` | Covered |
| 15b | **Calibration timepoint** | `calibration_timepoint = 0/36` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 15c | **Calibration groups** | `calibration_ngroups = 4/5` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 16a | **RCS non-linearity** | `rcs_analysis = TRUE`, `rcs_variable` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 16b | **RCS with different knots** | `rcs_knots = 3/4` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 16c | **RCS with missing data** | `rcs_variable = "Ki67"` (has NAs) | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 17a | **Age adjustment** | `age_adjustment = TRUE`, `age_variable` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 17b | **Age interaction** | `age_interaction = TRUE` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 17c | **Age-stratified Cox** | `age_stratified_cox = TRUE`, `age_group_cutpoints` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 17d | **Age as time scale** | `age_time_scale = TRUE` | `survival_comprehensive` | -- | **NOT TESTED** |
| 17e | **Age standardization (SMR)** | `age_standardization = TRUE`, `age_standardization_method` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered (indirect only) |
| 17f | **Direct age standardization** | `age_standardization_method = "direct"` | `survival_comprehensive` | -- | **NOT TESTED** |
| 17g | **Age-stratified KM plots** | `age_stratified_km = TRUE` | `survival_comprehensive` | -- | **NOT TESTED** |
| 17h | **Adjusted survival curves** | `adjusted_curves = TRUE` | `survival_comprehensive` | -- | **NOT TESTED** |
| 18a | **Date-based time** | `tint = TRUE`, `dxdate`, `fudate` | `survival_dates`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 18b | **Time type data formats** | `timetypedata = "ymd"/"mdy"/"dmy"/...` | `survival_dates` | `test-survival-arguments.R` | Partial -- ymd tested; other 6 formats not individually tested |
| 18c | **Time type output** | `timetypeoutput = "days"/"weeks"/"months"/"years"` | `survival_dates` | `test-survival-arguments.R` | Partial -- days, months, years tested; weeks not tested |
| 19a | **Multi-event competing risks** | `multievent = TRUE`, `dod`, `dooc`, `awd`, `awod`, `analysistype = "compete"` | `survival_competing`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R`, `test-survival-integration.R` | Covered |
| 19b | **Cause-specific survival** | `analysistype = "cause"` | `survival_competing`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 19c | **Overall survival (multi-event)** | `analysistype = "overall"` | `survival_competing` | `test-survival-integration.R` | Covered |
| 20 | **Landmark analysis** | `uselandmark = TRUE`, `landmark = <int>` | `survival_landmark`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R`, `test-survival-safety.R` | Covered |
| 21 | **Parametric models** | `use_parametric = TRUE`, `parametric_distribution`, `compare_distributions`, `parametric_diagnostics`, `spline_knots`, `spline_scale`, `parametric_extrapolation`, `hazard_plots` | `survival_comprehensive` | `test-survival-comprehensive.R` | Partial -- Weibull + compare tested; splines, extrapolation, hazard plots not individually tested |
| 22a | **Show explanations** | `showExplanations = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 22b | **Show summaries** | `showSummaries = TRUE` | `survival_test`, `survival_comprehensive` | `test-survival-arguments.R`, `test-survival-comprehensive.R` | Covered |
| 23 | **REMARK checklist** | `remark_checklist = TRUE` | `survival_comprehensive` | `test-survival-comprehensive.R` | Covered |
| 24a | **Export survival data** | `export_survival_data` (Output type) | `survival_test` | `test-survival.R` | Minimal -- no explicit assertion on output content |
| 24b | **Calculated time output** | `calculatedtime` (Output type) | `survival_dates` | -- | **NOT TESTED** |
| 24c | **Redefined outcome output** | `outcomeredefined` (Output type) | `survival_competing` | -- | **NOT TESTED** |
| 25a | **Negative time error** | Negative values in `elapsedtime` | `survival_test` (modified) | `test-survival-edge-cases.R`, `test-survival-safety.R` | Covered |
| 25b | **<10 events blocking** | Very few events in outcome | `survival_test` (modified) | `test-survival-safety.R` | Covered |
| 25c | **EPV warning** | Low events-per-variable | `survival_low_epv` | -- | **NOT TESTED** (dataset exists but no dedicated assertion) |
| 25d | **Extreme HR detection** | HR > 10 or HR < 0.1 | `survival_extreme_hr` | -- | **NOT TESTED** (dataset exists but no dedicated assertion) |
| 25e | **Convergence issues** | Near-separation or rare categories | `survival_extreme_hr` | -- | **NOT TESTED** |

---

## Gaps: Features Without Dedicated Tests

The following features have `.a.yaml` options defined but lack explicit test coverage:

1. **`age_time_scale = TRUE`** -- Age as the time axis in Cox model (`Surv(age_entry, age_event, event)`)
2. **`age_standardization_method = "direct"`** -- Direct standardization (only indirect is tested)
3. **`age_stratified_km = TRUE`** -- KM curves stratified by age groups
4. **`adjusted_curves = TRUE`** -- Age-adjusted survival curves from Cox model
5. **`timetypeoutput = "weeks"`** -- Output in weeks
6. **`timetypedata`** formats other than `"ymd"` -- e.g., `"dmy"`, `"mdy"`, `"ydm"`, `"myd"`, `"dym"`, `"ymdhms"`
7. **`calculatedtime`** (Output) -- Verify calculated time is added to data
8. **`outcomeredefined`** (Output) -- Verify redefined outcome is added to data
9. **Parametric model options** -- `parametric_distribution` (all 8 distributions), `parametric_extrapolation`, `extrapolation_time`, `parametric_survival_plots`, `hazard_plots`, `spline_knots`, `spline_scale`
10. **EPV warning** -- `survival_low_epv` dataset exists but no test asserts the warning
11. **Extreme HR detection** -- `survival_extreme_hr` dataset exists but no test asserts the warning
12. **`padjustmethod`** -- `"hochberg"`, `"hommel"`, `"BY"` not explicitly tested
13. **`medianline`** -- Only `"hv"` tested; `"h"`, `"v"`, `"none"` not individually tested
14. **Empty dataset (0 rows)** -- Boundary condition not currently tested

---

## Edge Case Scenarios

| Scenario | Data Setup | Expected Behavior | Test File | Status |
|----------|-----------|-------------------|-----------|--------|
| Empty dataset (0 rows) | `survival_test[0, ]` | Error: insufficient data | -- | **NOT TESTED** |
| All censored (0 events) | Set all `outcome = 0` | Error: no events / all censored | `test-survival-edge-cases.R` | Covered |
| All events (no censoring) | Set all `outcome = 1` | Completes (may warn) | `test-survival-edge-cases.R` | Covered |
| Single group (no explanatory) | Omit `explanatory` | Overall KM, no log-rank test | `test-survival-basic.R`, `test-survival-comprehensive.R` | Covered |
| Single-level explanatory | All same group value | Warning: single level | `test-survival-edge-cases.R` | Covered |
| Variable names with spaces | Rename columns to have spaces | Completes via jmvcore::composeTerm | `test-survival-safety.R` | Covered |
| Very small sample (n=5) | `survival_test[1:5, ]` | Completes (may warn about small sample) | `test-survival-edge-cases.R` | Covered |
| Single observation (n=1) | `survival_test[1, ]` | Error: insufficient data | `test-survival-edge-cases.R` | Covered |
| Very large sample (n=1000) | Generated in test | Completes within 30s | `test-survival.R` | Covered |
| Missing values in time | Set some `elapsedtime = NA` | Warning: rows removed | `test-survival-edge-cases.R` | Covered |
| Missing values in outcome | Set some `outcome = NA` | Warning: rows removed | `test-survival-edge-cases.R` | Covered |
| Missing values in continuous covariate | `Ki67` in `survival_comprehensive` (~5% NA) | Completes, handles NAs gracefully | `test-survival-comprehensive.R` | Covered |
| Zero time values | Set some `elapsedtime = 0` | Condition (warning or error) | `test-survival-edge-cases.R` | Covered |
| Negative time values | Set some `elapsedtime = -5` | Error: negative/invalid time | `test-survival-edge-cases.R`, `test-survival-safety.R` | Covered |
| Very small time values | Multiply time by 0.01 | Completes | `test-survival-edge-cases.R` | Covered |
| Very large time values | Multiply time by 100 | Completes | `test-survival-edge-cases.R` | Covered |
| Tied event times | Round times to nearest 10 | Completes | `test-survival-edge-cases.R` | Covered |
| Duplicate observations | `rbind(data, data[1:10, ])` | Completes | `test-survival-edge-cases.R` | Covered |
| Unbalanced groups (195 vs 5) | Overwrite treatment levels | Completes (may warn) | `test-survival-edge-cases.R` | Covered |
| Group with no events | Set outcome=0 for one group | Completes (may warn) | `test-survival-edge-cases.R` | Covered |
| Landmark beyond max follow-up | `landmark = 100` with max ~70 | Error: landmark exceeds max | `test-survival-edge-cases.R` | Covered |
| Invalid outcome level (competing risks) | `outcomeLevel = "Invalid Level"` | Error: level not found | `test-survival-edge-cases.R` | Covered |
| Missing strata variable | `stratified_cox = TRUE` without `strata_variable` | Error: strata required | `test-survival-edge-cases.R` | Covered |
| Constant strata variable | All same stratum value | Warning: constant/single level | `test-survival-edge-cases.R` | Covered |
| Outcome with >4 levels | 5-level outcome variable | Completes (converts/warns) | `test-survival-edge-cases.R` | Covered |
| Wrong date format | Use `"mdy"` for YMD data | Error | `test-survival-edge-cases.R` | Covered |
| Narrow y-axis range (0.5--0.6) | `ybegin_plot = 0.5`, `yend_plot = 0.6` | Completes | `test-survival-edge-cases.R` | Covered |
| Very short plot time (endplot=1) | `endplot = 1` | Completes | `test-survival-edge-cases.R` | Covered |
| Person-time with zero-event interval | No events before month 12 | Completes | `test-survival-edge-cases.R` | Covered |
| Data from CSV import | Write/read CSV round-trip | Completes | `test-survival-integration.R` | Covered |
| Data from Excel import | Write/read XLSX round-trip | Completes | `test-survival-integration.R` | Covered |
| Tibble input | `as_tibble(data)` | Completes | `test-survival-integration.R` | Covered |

---

## Smoke Test Template

Minimal copy-paste R code to verify the `survival` function loads and runs. Uses the bundled test datasets with the fewest possible arguments.

```r
# ── Smoke Test: survival function ──────────────────────────
# Prerequisites: ClinicoPath package installed or loaded via devtools::load_all()

library(ClinicoPath)
# OR for development:
# devtools::load_all(".")

# Load test data
data(survival_test, package = "ClinicoPath")

# ── 1. Minimal run (no explanatory variable) ──
result_minimal <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome"
)
cat("1. Minimal run: OK\n")

# ── 2. Basic KM + Cox (with explanatory) ──
result_basic <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment"
)
cat("2. Basic KM + Cox: OK\n")

# ── 3. With survival plot ──
result_plot <- survival(
  data = survival_test,
  elapsedtime = "elapsedtime",
  outcome = "outcome",
  explanatory = "treatment",
  sc = TRUE,
  ci95 = TRUE,
  risktable = TRUE
)
cat("3. Survival plot: OK\n")

# ── 4. Comprehensive feature check ──
data(survival_comprehensive, package = "ClinicoPath")

result_full <- survival(
  data = survival_comprehensive,
  elapsedtime = "FollowUpMonths",
  outcome = "Status",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",
  dooc = "Dead of Other Causes",
  awd = "Alive with Disease",
  awod = "Alive without Disease",
  explanatory = "TumorStage",
  sc = TRUE,
  ce = TRUE,
  ch = TRUE,
  loglog = TRUE,
  ci95 = TRUE,
  risktable = TRUE,
  censored = TRUE,
  pplot = TRUE,
  medianline = "hv",
  endplot = 80,
  byplot = 12,
  cutp = "12, 36, 60",
  pw = TRUE,
  padjustmethod = "holm",
  ph_cox = TRUE,
  person_time = TRUE,
  time_intervals = "12, 36, 60",
  rate_multiplier = 100,
  rmst_analysis = TRUE,
  rmst_tau = 60,
  residual_diagnostics = TRUE,
  showExplanations = TRUE,
  showSummaries = TRUE,
  remark_checklist = TRUE
)
cat("4. Comprehensive run: OK\n")

# ── 5. Competing risks ──
result_compete <- survival(
  data = survival_comprehensive,
  elapsedtime = "FollowUpMonths",
  outcome = "Status",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",
  dooc = "Dead of Other Causes",
  awd = "Alive with Disease",
  awod = "Alive without Disease",
  analysistype = "compete",
  multievent = TRUE,
  explanatory = "TumorStage"
)
cat("5. Competing risks: OK\n")

# ── 6. Age adjustment ──
result_age <- survival(
  data = survival_comprehensive,
  elapsedtime = "FollowUpMonths",
  outcome = "Status",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",
  dooc = "Dead of Other Causes",
  awd = "Alive with Disease",
  awod = "Alive without Disease",
  explanatory = "TumorStage",
  age_adjustment = TRUE,
  age_variable = "Age",
  age_interaction = TRUE,
  age_stratified_cox = TRUE,
  age_group_cutpoints = "50, 65, 75"
)
cat("6. Age adjustment: OK\n")

# ── 7. RCS non-linearity ──
result_rcs <- survival(
  data = survival_comprehensive,
  elapsedtime = "FollowUpMonths",
  outcome = "Status",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",
  dooc = "Dead of Other Causes",
  awd = "Alive with Disease",
  awod = "Alive without Disease",
  explanatory = "TumorStage",
  rcs_analysis = TRUE,
  rcs_variable = "Age",
  rcs_knots = 4
)
cat("7. RCS non-linearity: OK\n")

# ── 8. Calibration curves ──
result_cal <- survival(
  data = survival_comprehensive,
  elapsedtime = "FollowUpMonths",
  outcome = "Status",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",
  dooc = "Dead of Other Causes",
  awd = "Alive with Disease",
  awod = "Alive without Disease",
  explanatory = "TumorStage",
  calibration_curves = TRUE,
  calibration_timepoint = 36,
  calibration_ngroups = 5
)
cat("8. Calibration curves: OK\n")

# ── 9. Bootstrap validation ──
result_boot <- survival(
  data = survival_comprehensive,
  elapsedtime = "FollowUpMonths",
  outcome = "Status",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",
  dooc = "Dead of Other Causes",
  awd = "Alive with Disease",
  awod = "Alive without Disease",
  explanatory = "TumorStage",
  bootstrapValidation = TRUE,
  bootstrapValN = 50
)
cat("9. Bootstrap validation: OK\n")

# ── 10. Weighted log-rank tests ──
result_wlr <- survival(
  data = survival_comprehensive,
  elapsedtime = "FollowUpMonths",
  outcome = "Status",
  outcomeLevel = "Dead of Disease",
  dod = "Dead of Disease",
  dooc = "Dead of Other Causes",
  awd = "Alive with Disease",
  awod = "Alive without Disease",
  explanatory = "TumorStage",
  weightedLogRank = TRUE,
  survivalTestType = "fleming_harrington"
)
cat("10. Weighted log-rank: OK\n")

cat("\n=== All smoke tests passed ===\n")
```

---

## Recommended Test Priority for Missing Coverage

If writing new tests, address these gaps in order of clinical impact:

| Priority | Feature | Why it matters |
|----------|---------|----------------|
| 1 | **EPV warning** (`survival_low_epv`) | Clinicians must see warnings when events-per-variable is dangerously low |
| 2 | **Extreme HR detection** (`survival_extreme_hr`) | HR > 10 or < 0.1 should be flagged as potentially unreliable |
| 3 | **Age as time scale** (`age_time_scale`) | Important for cancer epidemiology; `Surv(age_entry, age_event, event)` formulation |
| 4 | **Adjusted survival curves** (`adjusted_curves`) | Commonly requested for age-adjusted KM curves |
| 5 | **Direct age standardization** (`age_standardization_method = "direct"`) | Complementary to the tested indirect method |
| 6 | **Age-stratified KM plots** (`age_stratified_km`) | Visual output for stratified analysis |
| 7 | **Parametric distributions** (all 8) | Only Weibull is tested; exp, lnorm, llogis, gamma, gengamma, gompertz, survspline untested |
| 8 | **Parametric extrapolation** | Health economic modeling use case |
| 9 | **Output columns** (`calculatedtime`, `outcomeredefined`) | Verify data export features work |
| 10 | **Empty dataset** (0 rows) | Boundary condition not tested |

---

## Quick Reference: Required Arguments

The auto-generated wrapper function requires these arguments. Level-type args must be provided even when unused (pass `NULL` or empty string `""`).

```r
survival(
  data,                    # data.frame (required)
  elapsedtime,             # Variable name: time column (required unless tint=TRUE)
  outcome,                 # Variable name: event/status column (required)
  outcomeLevel = NULL,     # Level: which level = event
  dod = NULL,              # Level: dead of disease
  dooc = NULL,             # Level: dead of other causes
  awd = NULL,              # Level: alive with disease
  awod = NULL,             # Level: alive without disease
  explanatory = NULL       # Variable name: grouping factor (optional)
)
```

---

## Regression Test Checklist

These are known pitfalls from development history. Each should be validated when making changes to `survival.b.R`.

| ID | Regression | What to check |
|----|-----------|---------------|
| R01 | **Serialization safety** | No `insert(999, notice)` calls; all notices use table notes or Html outputs |
| R02 | **Competing risk skip pattern** | When `multievent=TRUE && analysistype="compete"`, Cox, age-adjusted Cox, pairwise, person-time, calibration, RCS, bootstrap, and weighted log-rank must all be skipped without error |
| R03 | **Event count safety** | n_events < 10: error; 10-19: caution note; 20-49: moderate note |
| R04 | **Data caching** | `.cachedGetData` is reset at the start of `.run()` to prevent stale data |
| R05 | **Landmark filtering** | After landmark, all survival times >= 0 and shifted by landmark amount |
| R06 | **Bootstrap C-index direction** | `survival::concordance()` must use `reverse = TRUE` for Cox LP (higher LP = worse prognosis) |
| R07 | **Calibration with categorical predictors** | Binary factor produces only 2 unique predicted values; calibration should reduce groups or warn |
| R08 | **RCS variable from self$data** | RCS variable must be pulled from `self$data` and aligned by rownames (not from `.cleandata()`) |

---

## Performance Benchmarks

| Test | Expected Time (n=200) |
|------|----------------------|
| Basic KM + Cox | < 5 seconds |
| All plots (5 types) | < 10 seconds |
| Bootstrap validation (100 resamples) | < 30 seconds |
| Full feature combination | < 2 minutes |
| RCS analysis | < 5 seconds |
| Calibration | < 5 seconds |
| Person-time | < 3 seconds |
| RMST | < 3 seconds |

---

## Notes for Test Authors

1. **Level-type arguments**: When calling the wrapper function directly in R (outside jamovi), `outcomeLevel`, `dod`, `dooc`, `awd`, `awod` have no defaults. You must supply them -- use `NULL` or `""` for unused ones.

2. **Bootstrap tests are slow**: Use `bootstrapValN = 50` (the minimum) in tests to keep runtime under control.

3. **Parametric models**: The `use_parametric` option is currently disabled in the UI but the backend code may still run. Tests should verify graceful handling.

4. **Plot tests**: Survival plots are rendered via jamovi's image mechanism. In R wrapper mode, `expect_no_error()` confirms the plot state is set but does not render the image. For visual verification, use jamovi or export to file.

5. **Data integrity**: The `test-survival-comprehensive.R` file includes data integrity checks (dimensions, variable names, factor levels, value ranges, missingness). These serve as a canary for data corruption.

6. **Reproducibility**: The `survival_comprehensive` dataset is generated with `set.seed(2026)` in `data-raw/create_survival_test_data.R`. If regenerated, all hardcoded expectations (exact n, median values) may change.

7. **Known pitfall -- cleanData**: The `.cleandata()` method only includes columns specified as time/outcome/explanatory. Extra variables (e.g., `rcs_variable`, `age_variable`, `strata_variable`) must be pulled from `self$data` separately and aligned by rownames.

8. **Known pitfall -- concordance direction**: `survival::concordance(Surv ~ x)` treats higher x as better prognosis. For Cox linear predictor (higher = worse), use `reverse = TRUE`.
