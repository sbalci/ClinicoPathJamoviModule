# Survival Analysis - Testing Checklist

## Test Dataset

**Primary:** `survival_comprehensive` (200 patients, 15 variables)
- Load: `data("survival_comprehensive", package = "ClinicoPath")`
- Status levels: "Alive without Disease" (80), "Alive with Disease" (25), "Dead of Disease" (73), "Dead of Other Causes" (22)
- Continuous vars with NAs: Ki67, TumorSize
- Date columns: DiagnosisDate, LastFollowUpDate (YYYY-MM-DD strings)

**Additional datasets for edge cases:**
- `survival_small` -- small sample size
- `survival_competing` -- competing risks focus
- `survival_dates` -- date-based time calculation
- `survival_landmark` -- landmark analysis
- `survival_person_time` -- person-time focus
- `survival_rmst` -- RMST focus
- `survival_stratified` -- stratified Cox
- `survival_test` -- general testing

---

## Test Scenarios

### T01: Basic Kaplan-Meier Analysis (Minimal Required Inputs)

**Inputs:**
- `elapsedtime` = FollowUpMonths
- `outcome` = Status
- `outcomeLevel` = "Dead of Disease"
- `explanatory` = TumorStage

**Expected Outputs:**
- [x] `medianTable` populated with 4 rows (Stage I-IV), columns: factor, records, events, rmean, se_rmean, median, x0_95lcl, x0_95ucl
- [x] `coxTable` populated with HR_univariable for each stage
- [x] `tCoxtext2` (forest plot HTML) rendered
- [x] `survTable` populated with survival at months 12, 36, 60

**Verify:**
- Total records sums to approximately 200 (minus NAs)
- Events should be < records for each group
- Median survival is NA for groups where KM curve does not cross 0.5
- Advanced stages should have lower median survival

---

### T02: Survival Plots (All Types)

**Inputs:** Same as T01, plus:
- `sc` = true, `ce` = true, `ch` = true, `kmunicate` = true, `loglog` = true
- `ci95` = true, `risktable` = true, `censored` = true, `pplot` = true
- `medianline` = "hv"
- `endplot` = 80, `byplot` = 12, `ybegin_plot` = 0.0, `yend_plot` = 1.0

**Expected Outputs:**
- [x] `plot` (Survival Curve) -- 4 colored curves with CI bands, risk table below, censoring marks, p-value, median lines
- [x] `plot2` (Cumulative Events) -- increasing curves
- [x] `plot3` (Cumulative Hazard) -- increasing curves
- [x] `plot6` (KMunicate) -- KMunicate-style with extended risk table
- [x] `plot7` (Log-Log) -- log(-log(S)) vs log(time), parallel lines = PH holds

**Verify:**
- All 5 plots render without error
- Y-axis range matches ybegin_plot to yend_plot for survival plot
- X-axis ends at endplot (80)
- Time intervals on x-axis match byplot (12)
- Risk table shows decreasing numbers at risk

---

### T03: Cox Regression with PH Assumption Test

**Inputs:** Same as T01, plus:
- `ph_cox` = true

**Expected Outputs:**
- [x] `cox_ph` (Preformatted) -- Schoenfeld residuals test output
- [x] `phInterpretation` (Html) -- interpretation of PH test
- [x] `plot8` (PH Schoenfeld plot) -- Schoenfeld residuals over time

**Verify:**
- PH test p-value reported for each covariate and global test
- If p < 0.05, interpretation should flag violation
- Schoenfeld residual plot shows smooth curve with CI band

---

### T04: Residual Diagnostics

**Inputs:** Same as T01, plus:
- `residual_diagnostics` = true

**Expected Outputs:**
- [x] `residualsTable` -- observation, martingale, deviance, score, schoenfeld columns
- [x] `residualsPlot` -- residual diagnostic plots

**Verify:**
- Table has rows for each observation
- Martingale residuals between -inf and 1
- Deviance residuals approximately symmetric around 0

---

### T05: Stratified Cox Regression

**Inputs:** Same as T01, plus:
- `stratified_cox` = true
- `strata_variable` = MarginStatus

**Expected Outputs:**
- [x] `coxTable` -- HR from stratified Cox model
- [x] Formula should include strata(MarginStatus)

**Verify:**
- HR values may differ from unstratified model
- No error even if strata variable has 3 levels

---

### T06: Pairwise Comparisons

**Inputs:** Same as T01, plus:
- `pw` = true
- `padjustmethod` = "bonferroni"

**Expected Outputs:**
- [x] `pairwiseComparisonHeading` visible
- [x] `pairwiseTable` -- 6 pairwise comparisons for 4 stages (4 choose 2 = 6)
- [x] `pairwiseSummary` -- interpretation

**Verify:**
- Adjusted p-values >= unadjusted p-values
- All pairs compared (Stage I vs II, I vs III, I vs IV, II vs III, II vs IV, III vs IV)

**Test all adjustment methods:**
- holm, hochberg, hommel, bonferroni, BH, BY, fdr, none

---

### T07: Weighted Log-Rank Tests

**Inputs:** Same as T01, plus:
- `weightedLogRank` = true
- `survivalTestType` = "fleming_harrington"

**Expected Outputs:**
- [x] `weightedLogRankTable` -- 4 rows (all test types), columns: test, rho, chisq, df, pvalue, weighting

**Verify:**
- Standard log-rank (rho=0) result matches base KM p-value
- Gehan-Breslow (rho=1) emphasizes early differences
- All 4 test types reported

**Test all survivalTestType values:**
- logrank, gehan_breslow, tarone_ware, peto_peto, fleming_harrington

---

### T08: Restricted Mean Survival Time (RMST)

**Inputs:** Same as T01, plus:
- `rmst_analysis` = true
- `rmst_tau` = 60

**Expected Outputs:**
- [x] `rmstHeading` visible
- [x] `rmstTable` -- one row per TumorStage level, columns: group, rmst, se, ci_lower, ci_upper, tau

**Verify:**
- All tau values = 60
- RMST values are positive and less than tau
- Higher-stage tumors should have lower RMST
- SE > 0

**Additional test:**
- `rmst_tau` = 0 (should auto-select 75th percentile)

---

### T09: Person-Time Analysis

**Inputs:** Same as T01, plus:
- `person_time` = true
- `time_intervals` = '12, 36, 60'
- `rate_multiplier` = 1000

**Expected Outputs:**
- [x] `personTimeHeading` visible
- [x] `personTimeTable` -- rows for intervals 0-12, 12-36, 36-60, 60+
- [x] `personTimeSummary` (Html)

**Verify:**
- Events sum approximately to total events across intervals
- Rates = (events / person_time) * rate_multiplier
- CI bounds make sense (lower < rate < upper)

---

### T10: Multiple Outcome / Competing Risks

**Inputs:**
- `elapsedtime` = FollowUpMonths
- `outcome` = Status
- `multievent` = true
- `dod` = "Dead of Disease"
- `dooc` = "Dead of Other Causes"
- `awd` = "Alive with Disease"
- `awod` = "Alive without Disease"
- `analysistype` = "compete"
- `explanatory` = TumorStage

**Expected Outputs:**
- [x] `medianTable` -- cumulative incidence-based medians
- [x] Cox regression is SKIPPED (no coxTable populated differently)
- [x] Pairwise comparisons are SKIPPED
- [x] Person-time, RMST, calibration, RCS, bootstrap are SKIPPED

**Verify:**
- No errors despite skipping multiple features
- Median table uses CIF-based estimation
- outcomeredefined output works if enabled

**Also test:**
- `analysistype` = "overall" with multievent=true
- `analysistype` = "cause" with multievent=true

---

### T11: Cause-Specific Survival

**Inputs:** Same as T10 but:
- `analysistype` = "cause"

**Expected Outputs:**
- [x] Outcome recoded: "Dead of Disease" = event, "Dead of Other" = censored
- [x] Standard KM/Cox analysis proceeds (not competing risk mode)

**Verify:**
- Fewer events than overall survival (only disease deaths counted)
- Cox model runs normally

---

### T12: Date-Based Time Calculation

**Inputs:**
- `tint` = true
- `dxdate` = DiagnosisDate
- `fudate` = LastFollowUpDate
- `timetypedata` = "ymd"
- `timetypeoutput` = "months"
- `outcome` = Status
- `outcomeLevel` = "Dead of Disease"
- `explanatory` = TumorStage

**Expected Outputs:**
- [x] Time calculated from dates
- [x] All core analyses run with calculated time
- [x] `calculatedtime` output available (if enabled)

**Verify:**
- Calculated time values are positive
- Results consistent with FollowUpMonths-based analysis (should be similar)

**Also test:**
- `timetypeoutput` = "days", "weeks", "years"
- `calculatedtime` = true (export to dataset)

---

### T13: Landmark Analysis

**Inputs:** Same as T01, plus:
- `uselandmark` = true
- `landmark` = 12

**Expected Outputs:**
- [x] Patients with events/censoring before 12 months are excluded
- [x] `medianTable` note about landmark exclusion
- [x] Survival times shifted (time = original time - 12)

**Verify:**
- Fewer rows in analysis than original dataset
- No negative survival times
- Median survival values may differ from standard analysis

---

### T14: Age-Adjusted Cox Regression

**Inputs:** Same as T01, plus:
- `age_adjustment` = true
- `age_variable` = Age

**Expected Outputs:**
- [x] `ageAdjustedCoxHeading` visible
- [x] `ageAdjustedCoxTable` -- unadjusted vs age-adjusted HR
- [x] `ageAdjustedInterpretation` (Html)

**Verify:**
- Age-adjusted HR differs from unadjusted HR
- Interpretation mentions confounding assessment

---

### T15: Age x Group Interaction

**Inputs:** Same as T14, plus:
- `age_interaction` = true

**Expected Outputs:**
- [x] `ageInteractionTable` -- interaction term with coef, HR, SE, z, pvalue

**Verify:**
- Interaction p-value reported
- If significant, age modifies the group effect

---

### T16: Age as Time Scale

**Inputs:** Same as T14, plus:
- `age_time_scale` = true

**Expected Outputs:**
- [x] `ageTimeScaleTable` -- Cox model using age as time axis
- [x] `ageTimeScaleInterpretation`

**Verify:**
- HRs may differ substantially from standard model
- Time axis is biological age, not follow-up time

---

### T17: Age Standardization (SMR)

**Inputs:** Same as T14, plus:
- `age_standardization` = true
- `age_standardization_method` = "indirect"
- `age_group_cutpoints` = "50, 65, 75"

**Expected Outputs:**
- [x] `ageStandardizationTable` -- group, observed, expected, SMR, CI, pvalue

**Verify:**
- SMR = observed / expected
- CI bounds around SMR
- SMR > 1 means excess mortality

**Also test:**
- `age_standardization_method` = "direct"

---

### T18: Age-Stratified KM Plots

**Inputs:** Same as T14, plus:
- `age_stratified_km` = true
- `age_group_cutpoints` = "50, 65, 75"

**Expected Outputs:**
- [x] `ageStratifiedKMPlot` (Image 700x500)

**Verify:**
- Separate KM curves for age groups
- Curves are distinguishable and labeled

---

### T19: Adjusted Survival Curves

**Inputs:** Same as T14, plus:
- `adjusted_curves` = true

**Expected Outputs:**
- [x] `adjustedCurvesPlot` (Image 700x500)

**Verify:**
- Curves adjusted for age effect
- Different from unadjusted KM curves

---

### T20: Calibration Assessment

**Inputs:** Same as T01, plus:
- `calibration_curves` = true
- `calibration_timepoint` = 60
- `calibration_ngroups` = 5

**Expected Outputs:**
- [x] `calibrationTable` -- slope, intercept, etc.
- [x] `calibrationGroupTable` -- 5 risk groups with predicted vs observed
- [x] `calibrationPlot` (Image 600x500)

**Verify:**
- Calibration slope near 1 = good calibration
- Points on calibration plot cluster around diagonal
- Each risk group has reasonable N

**Also test:**
- `calibration_timepoint` = 0 (auto-select median)
- `calibration_ngroups` = 3, 10

---

### T21: Bootstrap Internal Validation

**Inputs:** Same as T01, plus:
- `bootstrapValidation` = true
- `bootstrapValN` = 100

**Expected Outputs:**
- [x] `bootstrapValidationTable` -- metric (C-index, Dxy, Cal. slope), apparent, optimism, corrected

**Verify:**
- Corrected < apparent (optimism-correction reduces performance)
- Optimism is positive
- C-index between 0.5 and 1.0
- n_bootstrap matches bootstrapValN

**Performance note:** With 200 patients and 100 bootstraps, should complete in < 30 seconds.

---

### T22: Non-Linearity Assessment (RCS)

**Inputs:** Same as T01, plus:
- `rcs_analysis` = true
- `rcs_variable` = Age
- `rcs_knots` = 4

**Expected Outputs:**
- [x] `rcsTestTable` -- linear vs spline model comparison (AIC, LR test, p-value, conclusion)
- [x] `rcsPlot` (Image 600x450) -- hazard ratio curve

**Verify:**
- If p < 0.05, non-linearity detected
- HR curve shows non-linear relationship with reference point
- Knot locations at Harrell-recommended percentiles

**Also test:**
- `rcs_variable` = Ki67 (has NAs -- should handle gracefully)
- `rcs_variable` = TumorSize (has NAs)
- `rcs_knots` = 3, 5, 7

---

### T23: REMARK Reporting Checklist

**Inputs:** Same as T01, plus:
- `remark_checklist` = true

**Expected Outputs:**
- [x] `remarkChecklist` (Html) -- checklist items with status

**Verify:**
- Lists REMARK items
- Shows which are addressed by current analysis
- Items related to enabled features are checked

---

### T24: Explanations and Summaries

**Inputs:** Same as T01, plus:
- `showExplanations` = true
- `showSummaries` = true

**Expected Outputs:**
- [x] `medianSurvivalExplanation`, `coxRegressionExplanation`, `survivalTablesExplanation` all visible
- [x] `medianSummary`, `coxSummary`, `survTableSummary` all visible
- [x] `clinicalGlossaryExplanation` visible
- [x] `clinicalInterpretationExplanation` visible
- [x] `copyReadySentencesExplanation` visible

**Verify:**
- Explanations contain methodological content
- Summaries contain natural language interpretation with actual values
- Copy-ready sentences are formatted for clinical reports

---

### T25: Export Survival Data

**Inputs:** Same as T01, plus:
- `export_survival_data` = true (Output type)

**Expected Outputs:**
- [x] `survivalExport` (Output) -- data frame with survival estimates
- [x] `survivalExportSummary` (Html) -- summary of export

**Verify:**
- Exported data contains time points, survival probabilities, CIs

---

### T26: Full Feature Combination

**Inputs:** ALL features enabled simultaneously:
- Basic: elapsedtime, outcome, outcomeLevel, explanatory
- Plots: sc, ce, ch, kmunicate, loglog (all true)
- Plot options: ci95, risktable, censored, pplot, medianline="hv"
- Cox: ph_cox, stratified_cox (with strata_variable), residual_diagnostics
- Age: age_adjustment, age_variable, age_interaction, age_stratified_cox, age_time_scale, age_standardization, age_stratified_km, adjusted_curves
- Validation: calibration_curves, bootstrapValidation (N=50), rcs_analysis (rcs_variable=Age)
- Tests: pw, weightedLogRank
- Other: rmst_analysis, person_time, remark_checklist
- Explanations: showExplanations, showSummaries

**Expected Outputs:**
- [x] All result items populated without errors
- [x] No serialization errors
- [x] All plots render

**Verify:**
- Analysis completes without crash
- Performance: should complete in < 2 minutes

---

## Edge Case Tests

### E01: Single Group (No Explanatory)

This test is not directly possible since `explanatory` is required. However, test with a binary variable where one group has very few patients.

**Inputs:**
- `explanatory` = LymphovascularInvasion (2 groups: Absent ~120, Present ~80)

**Verify:**
- Analysis runs normally
- Only 2 groups in all tables

---

### E02: Binary Explanatory Variable

**Inputs:**
- `explanatory` = Sex

**Verify:**
- 2-group comparison
- Only 1 pairwise comparison (if pw=true)
- Weighted log-rank shows all test types

---

### E03: Many Groups

**Inputs:**
- `explanatory` = TumorStage (4 groups)

**Verify:**
- All 4 groups in median table
- 6 pairwise comparisons (4C2)
- Plots show 4 curves

---

### E04: Few Events Per Group

If a specific stage has very few events (< 5), verify:
- Analysis still runs
- Warning notes appear on medianTable
- Median survival is NA for groups without enough events

---

### E05: All Censored (Simulated)

Create a scenario where no events occur:
- This should hit the "< 10 events" check and produce an ERROR stop

---

### E06: Special Characters in Variable Names

If column names contain spaces or special characters:
- `.escapeVariableNames()` should backtick-escape them
- Formulas should work with escaped names

---

### E07: Missing Values in Continuous Variables

**Inputs:**
- `rcs_variable` = Ki67 (has NAs)

**Verify:**
- RCS analysis handles NAs gracefully (naOmit)
- Reduced sample size noted

---

### E08: Very Short Follow-up

If all follow-up times are < 1 month:
- `cutp` = "12, 36, 60" would produce empty survival table rows
- Should handle gracefully with NAs

---

### E09: Very Long Follow-up

If follow-up extends beyond `endplot`:
- Plots should truncate at endplot
- Tables should still report full data

---

### E10: Large Dataset Performance

Test with `nrow > 5000`:
- `.getData()` triggers garbage collection
- Bootstrap validation should still complete
- Checkpoint calls prevent timeout

---

## Complete Option Coverage Checklist

Every option should be exercised in at least one test.

| # | Option | Default | Test(s) |
|---|--------|---------|---------|
| 1 | `elapsedtime` | null | T01-T26 |
| 2 | `tint` | false | T12 |
| 3 | `dxdate` | null | T12 |
| 4 | `fudate` | null | T12 |
| 5 | `calculatedtime` | Output | T12 |
| 6 | `explanatory` | null | T01-T26 |
| 7 | `outcome` | null | T01-T26 |
| 8 | `outcomeLevel` | (none) | T01-T26 |
| 9 | `dod` | (none) | T10, T11 |
| 10 | `dooc` | (none) | T10, T11 |
| 11 | `awd` | (none) | T10, T11 |
| 12 | `awod` | (none) | T10, T11 |
| 13 | `analysistype` | overall | T10 (compete), T11 (cause) |
| 14 | `outcomeredefined` | Output | T10 |
| 15 | `cutp` | '12, 36, 60' | T01, custom in T26 |
| 16 | `timetypedata` | ymd | T12 |
| 17 | `timetypeoutput` | months | T12 (days, weeks, years) |
| 18 | `uselandmark` | false | T13 |
| 19 | `landmark` | 3 | T13 (12) |
| 20 | `pw` | false | T06 |
| 21 | `padjustmethod` | holm | T06 (all methods) |
| 22 | `weightedLogRank` | false | T07 |
| 23 | `survivalTestType` | logrank | T07 (all types) |
| 24 | `ph_cox` | false | T03 |
| 25 | `sc` | false | T02 |
| 26 | `kmunicate` | false | T02 |
| 27 | `ce` | false | T02 |
| 28 | `ch` | false | T02 |
| 29 | `endplot` | 60 | T02 (80) |
| 30 | `ybegin_plot` | 0.00 | T02 |
| 31 | `yend_plot` | 1.00 | T02 |
| 32 | `byplot` | 12 | T02 |
| 33 | `multievent` | false | T10, T11 |
| 34 | `ci95` | false | T02 |
| 35 | `risktable` | false | T02 |
| 36 | `censored` | false | T02 |
| 37 | `pplot` | false | T02 |
| 38 | `medianline` | none | T02 (hv) |
| 39 | `person_time` | false | T09 |
| 40 | `time_intervals` | '12, 36, 60' | T09 |
| 41 | `rate_multiplier` | 100 | T09 (1000) |
| 42 | `rmst_analysis` | false | T08 |
| 43 | `rmst_tau` | 0 | T08 (60, 0) |
| 44 | `stratified_cox` | false | T05 |
| 45 | `strata_variable` | null | T05 (MarginStatus) |
| 46 | `age_adjustment` | false | T14-T19 |
| 47 | `age_variable` | null | T14-T19 (Age) |
| 48 | `age_interaction` | false | T15 |
| 49 | `age_stratified_cox` | false | T14 |
| 50 | `age_group_cutpoints` | '50, 65, 75' | T17, T18 |
| 51 | `age_time_scale` | false | T16 |
| 52 | `age_standardization` | false | T17 |
| 53 | `age_standardization_method` | indirect | T17 (indirect, direct) |
| 54 | `age_stratified_km` | false | T18 |
| 55 | `adjusted_curves` | false | T19 |
| 56 | `remark_checklist` | false | T23 |
| 57 | `residual_diagnostics` | false | T04 |
| 58 | `export_survival_data` | Output | T25 |
| 59 | `loglog` | false | T02 |
| 60 | `showExplanations` | false | T24 |
| 61 | `showSummaries` | false | T24 |
| 62 | `use_parametric` | false | (DISABLED -- skip) |
| 63 | `parametric_distribution` | weibull | (DISABLED) |
| 64 | `parametric_covariates` | true | (DISABLED) |
| 65 | `spline_knots` | 3 | (DISABLED) |
| 66 | `spline_scale` | hazard | (DISABLED) |
| 67 | `parametric_extrapolation` | false | (DISABLED) |
| 68 | `extrapolation_time` | 0 | (DISABLED) |
| 69 | `parametric_diagnostics` | true | (DISABLED) |
| 70 | `compare_distributions` | false | (DISABLED) |
| 71 | `parametric_survival_plots` | false | (DISABLED) |
| 72 | `hazard_plots` | false | (DISABLED) |
| 73 | `calibration_curves` | false | T20 |
| 74 | `calibration_timepoint` | 0 | T20 (60, 0) |
| 75 | `calibration_ngroups` | 5 | T20 (3, 5, 10) |
| 76 | `rcs_analysis` | false | T22 |
| 77 | `rcs_variable` | null | T22 (Age, Ki67) |
| 78 | `rcs_knots` | 4 | T22 (3, 4, 5, 7) |
| 79 | `bootstrapValidation` | false | T21 |
| 80 | `bootstrapValN` | 200 | T21 (100) |

---

## Regression Tests

### R01: Serialization Safety
No `insert(999, notice)` calls should exist. All notices use table notes or Html outputs.

### R02: Competing Risk Skip Pattern
When `multievent=true && analysistype="compete"`, the following must be skipped without error:
- `.cox()`, `.ageAdjustedCox()`, `.ageTimeScaleCox()`, `.ageStandardization()`
- `.pairwise()`, `.personTimeAnalysis()`, `.calculateCalibration()`, `.calculateRCS()`
- `.calculateWeightedLogRank()`, `.calculateBootstrapValidation()`

### R03: Event Count Safety
- n_events < 10: analysis stops with informative error
- n_events 10-19: caution note on medianTable
- n_events 20-49: moderate note on medianTable

### R04: Data Caching
`.cachedGetData` is reset at the start of `.run()` to prevent stale data.

### R05: Landmark Filtering
After landmark filtering, all survival times should be >= 0 and shifted by landmark amount.

### R06: Bootstrap C-index Direction
`survival::concordance()` must use `reverse = TRUE` for Cox linear predictor (higher LP = worse prognosis).

### R07: Calibration with Categorical Predictors
Binary factor predictors produce only 2 unique predicted values. Calibration should reduce effective groups or warn.

### R08: RCS Variable from self$data
RCS variable must be pulled from `self$data` and aligned by rownames (not from `.cleandata()` output which only has time/outcome/explanatory).

---

## Performance Benchmarks

| Test | Expected Time (200 patients) |
|---|---|
| Basic KM + Cox | < 5 seconds |
| All plots (5 types) | < 10 seconds |
| Bootstrap validation (100 resamples) | < 30 seconds |
| Full feature combination (T26) | < 2 minutes |
| RCS analysis | < 5 seconds |
| Calibration | < 5 seconds |
| Person-time | < 3 seconds |
| RMST | < 3 seconds |

---

## Quick Smoke Test Command

```r
# Quick parse check
parse(file = "R/survival.b.R")

# Quick instantiation test (without full jamovi)
devtools::load_all(".")
data("survival_comprehensive")

result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    explanatory = "TumorStage",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    sc = TRUE,
    pw = TRUE,
    rmst_analysis = TRUE,
    person_time = TRUE,
    showSummaries = TRUE
)
```
