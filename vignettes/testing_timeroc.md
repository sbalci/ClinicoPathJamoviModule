# Enhanced ROC Analysis (timeroc) - Testing Checklist

## Test Datasets

**Primary:** `timeroc_test` (200 breast cancer patients, 9 variables)
- Load: `data("timeroc_test", package = "ClinicoPath")`
- Variables: PatientID, FollowUpMonths, Recurrence (0/1), RecurrenceFactor (No Recurrence/Recurrence), Age, Stage (I/II/III), Ki67, GeneScore, NoiseMarker
- CSV copy: `data-raw/non-rda/timeroc_test.csv`
- OMV copy: `data-raw/non-rda/timeroc_test.omv`

**Additional datasets for edge cases:**
- `timeroc_cancer_biomarker` -- general cancer biomarker scenarios
- `timeroc_cardiovascular_risk` -- cardiovascular risk markers
- `timeroc_multi_biomarker` -- multiple biomarkers for comparison testing
- `timeroc_edge_cases` -- designed edge cases (missing data, extremes)
- `timeroc_competing_risks` -- competing risks scenarios
- `timeroc_landmark_biomarker` -- landmark biomarker scenarios

---

## Test Scenarios

### Section 1: Time-Dependent ROC

#### T01: Basic Time-Dependent ROC (Minimal Required Inputs)

**Inputs:**
- `elapsedtime` = FollowUpMonths
- `outcome` = Recurrence
- `outcomeLevel` = 1
- `marker` = Ki67
- `analysisType` = timedep (default)
- All other options at defaults

**Expected Outputs:**
- [ ] `aucTable` populated with 3 rows (timepoints 12, 36, 60), columns: timepoint, auc, se, ci_lower, ci_upper
- [ ] `rocPlot` rendered with 3 colored ROC curves + diagonal reference line
- [ ] `aucPlot` rendered with 3 points connected by line, reference lines at 0.5, 0.7, 0.8
- [ ] `markerStats` populated with 9 rows (N, Mean, Median, SD, IQR, Min, Max, Events, Event Rate)
- [ ] `cutoffTable` populated with 3 rows (optimal cutoff, sensitivity, specificity, Youden for each timepoint)
- [ ] `clinicalInterpretation` HTML rendered with overall performance, time trend, method description
- [ ] `text` HTML rendered with per-timepoint AUC interpretation
- [ ] `notices` HTML rendered with INFO notice (Analysis Complete)

**Verify:**
- AUC values are between 0 and 1
- SE columns show NaN (bootstrapCI is false by default)
- CI columns show NaN (bootstrapCI is false by default)
- Ki67 mean/median are plausible for breast cancer data
- Event rate matches actual proportion of Recurrence=1

---

#### T02: Factor Outcome with Level Selection (Time-Dependent)

**Inputs:**
- `elapsedtime` = FollowUpMonths
- `outcome` = RecurrenceFactor
- `outcomeLevel` = "Recurrence"
- `marker` = GeneScore
- `analysisType` = timedep

**Expected Outputs:**
- [ ] Analysis completes without error
- [ ] AUC values should differ from T01 (different marker)
- [ ] Outcome correctly converted to 0/1 using factor level

**Verify:**
- Number of events matches count of RecurrenceFactor == "Recurrence"
- AUC table has same structure as T01

---

#### T03: Different IPCW Weighting Methods

**Inputs (3 runs):**
- Same as T01, but vary `method`:
  - Run A: `method` = marginal (Kaplan-Meier)
  - Run B: `method` = cox (Cox model)
  - Run C: `method` = aalen (Aalen additive)

**Expected Outputs:**
- [ ] All 3 runs complete without error
- [ ] AUC values differ slightly between methods
- [ ] `clinicalInterpretation` text correctly identifies the weighting method used
- [ ] `text` HTML correctly labels the analysis method

**Verify:**
- Method label in interpretation matches selection
- Cox and Aalen may produce slightly different AUCs vs marginal

---

#### T04: Confidence Intervals Enabled

**Inputs:**
- Same as T01, plus: `bootstrapCI` = true

**Expected Outputs:**
- [ ] `aucTable` SE column populated with numeric values (not NaN)
- [ ] `aucTable` CI columns populated (ci_lower, ci_upper)
- [ ] `aucPlot` shows confidence ribbon (blue shaded area)
- [ ] `text` HTML includes significance p-values for each timepoint
- [ ] `text` HTML includes "Confidence Intervals: Asymptotic (influence function-based)" line

**Verify:**
- CI lower <= AUC <= CI upper for each timepoint
- CI is narrower when more events are available at a timepoint
- SE values are positive and reasonable (typically 0.02-0.15)

---

### Section 2: Binary ROC

#### T05: Basic Binary ROC (Numeric Outcome)

**Inputs:**
- `outcome` = Recurrence
- `outcomeLevel` = 1
- `marker` = Ki67
- `analysisType` = binary
- (elapsedtime not needed)

**Expected Outputs:**
- [ ] `binaryROCTable` populated with 1 row: marker, auc, se, ci_lower, ci_upper, sensitivity, specificity, optimal_cutoff
- [ ] `binaryROCPlot` rendered with single ROC curve + diagonal reference
- [ ] `diagnosticPerformance` HTML rendered with AUC interpretation and clinical summary
- [ ] `notices` HTML with INFO (Analysis Complete) showing observation count, event count, event rate

**Verify:**
- AUC value is between 0 and 1
- SE derived from CI: (ci_upper - ci_lower) / (2 * 1.96)
- Direction auto-detected by pROC

---

#### T06: Binary ROC with Factor Outcome and Level

**Inputs:**
- `outcome` = RecurrenceFactor
- `outcomeLevel` = "Recurrence"
- `marker` = GeneScore
- `analysisType` = binary

**Expected Outputs:**
- [ ] Analysis completes; factor correctly converted to 0/1
- [ ] `binaryROCTable` shows AUC, optimal cutoff for GeneScore
- [ ] `diagnosticPerformance` provides correct discrimination label

**Verify:**
- Same number of events as T02

---

#### T07: Auto Direction Detection

**Inputs:**
- `outcome` = Recurrence
- `outcomeLevel` = 1
- `marker` = NoiseMarker (random noise, AUC should be near 0.5)
- `analysisType` = binary

**Expected Outputs:**
- [ ] AUC close to 0.5 (within 0.4-0.6 range)
- [ ] Notice: WARNING "Limited Clinical Utility" if AUC < 0.70
- [ ] `diagnosticPerformance` labels as "Poor discrimination" or "No discrimination"

**Verify:**
- pROC `direction = "auto"` correctly handles the marker

---

### Section 3: ROC Comparison

#### T08: DeLong Test (Multi-Marker Comparison)

**Inputs:**
- `outcome` = Recurrence
- `outcomeLevel` = 1
- `marker` = Ki67
- `markers` = [GeneScore, NoiseMarker]
- `analysisType` = binary
- `compareROCs` = true
- `rocComparison` = delong

**Expected Outputs:**
- [ ] `rocComparisonTable` populated with 2 rows (Ki67 vs GeneScore, Ki67 vs NoiseMarker)
- [ ] Each row: comparison name, method="DeLong", test_statistic, p_value, interpretation
- [ ] `binaryROCPlot` shows 3 ROC curves in different colors with legend
- [ ] Ki67 vs NoiseMarker comparison should be significant (or near-significant)

**Verify:**
- p-values are between 0 and 1
- Interpretation text matches significance threshold (p < 0.05)
- Primary marker (Ki67) listed first in comparison label

---

#### T09: Bootstrap Comparison Method

**Inputs:**
- Same as T08, but: `rocComparison` = bootstrap

**Expected Outputs:**
- [ ] `rocComparisonTable` method column shows "Bootstrap"
- [ ] Test statistic and p-value present
- [ ] Takes longer to compute (n=1000 bootstrap samples)

---

#### T10: Venkatraman Test

**Inputs:**
- Same as T08, but: `rocComparison` = venkatraman

**Expected Outputs:**
- [ ] `rocComparisonTable` method column shows "Venkatraman"
- [ ] Test statistic and p-value present

---

### Section 4: Configuration Options

#### T11: Custom Timepoints

**Inputs:**
- Same as T01, but: `timepoints` = "6, 12, 24, 48"

**Expected Outputs:**
- [ ] `aucTable` has 4 rows (timepoints 6, 12, 24, 48)
- [ ] `rocPlot` shows 4 ROC curves
- [ ] `cutoffTable` has up to 4 rows

**Verify:**
- Timepoints are sorted ascending
- If any timepoint exceeds max follow-up, it is dropped with a WARNING notice

---

#### T12: Time Units Display

**Inputs (4 runs):**
- Same as T01, but vary `timetypeoutput`: days, weeks, months, years

**Expected Outputs:**
- [ ] `rocPlot` title/legend uses correct time unit label
- [ ] `aucPlot` x-axis label changes to match
- [ ] `text` HTML uses correct unit in interpretation
- [ ] `clinicalInterpretation` uses correct unit

**Verify:**
- Numeric timepoint values remain unchanged (only labels change)

---

#### T13: Smooth AUC Curve

**Inputs:**
- Same as T01, plus: `smoothAUC` = true, `timepoints` = "6, 12, 18, 24, 36, 48, 60"

**Expected Outputs:**
- [ ] `aucPlot` rendered with loess smooth curve (requires >2 timepoints)
- [ ] Smooth line is darker blue, thicker than point-to-point line

**Verify:**
- With only 3 default timepoints, smooth may fall back to regular line
- With 7 timepoints, loess should work

---

### Section 5: Display Toggles

#### T14: showOptimalCutoff = false

**Inputs:**
- Same as T01, but: `showOptimalCutoff` = false

**Expected Outputs:**
- [ ] `cutoffTable` is hidden (not rendered)
- [ ] All other outputs still present

---

#### T15: showMarkerStats = false

**Inputs:**
- Same as T01, but: `showMarkerStats` = false

**Expected Outputs:**
- [ ] `markerStats` table is hidden
- [ ] All other outputs still present

---

#### T16: compareBaseline = true (with CI)

**Inputs:**
- Same as T01, plus: `compareBaseline` = true, `bootstrapCI` = true

**Expected Outputs:**
- [ ] `modelComparison` HTML rendered with per-timepoint comparison to AUC=0.5
- [ ] Each timepoint shows improvement percentage, AUC vs 0.50, p-value, significance label

**Verify:**
- p-values are calculated from z-score = (AUC - 0.5) / SE
- Significant results have p < 0.05

---

#### T17: compareBaseline = true (without CI)

**Inputs:**
- Same as T01, plus: `compareBaseline` = true, `bootstrapCI` = false

**Expected Outputs:**
- [ ] `modelComparison` HTML shows message: "Enable 'Confidence Intervals (Asymptotic)' to compute model comparison"

---

#### T18: youdenIndex = false (Binary Mode)

**Inputs:**
- `outcome` = Recurrence, `outcomeLevel` = 1, `marker` = Ki67, `analysisType` = binary
- `youdenIndex` = false

**Expected Outputs:**
- [ ] `binaryROCTable` sensitivity, specificity, optimal_cutoff columns show NA
- [ ] AUC and CI still calculated normally

---

#### T19: plotROC = false

**Inputs:**
- Same as T01, but: `plotROC` = false

**Expected Outputs:**
- [ ] `rocPlot` (timedep) or `binaryROCPlot` (binary) is hidden
- [ ] All tables and HTML outputs still render

---

#### T20: plotAUC = false

**Inputs:**
- Same as T01, but: `plotAUC` = false

**Expected Outputs:**
- [ ] `aucPlot` is hidden
- [ ] `smoothAUC` option has no effect (UI disabled when plotAUC is false)

---

### Section 6: Edge Cases

#### T21: All Censored (No Events)

**Inputs:**
- Custom data where all Recurrence = 0
- `marker` = Ki67, `analysisType` = timedep

**Expected Outputs:**
- [ ] ERROR notice: "No Events Found"
- [ ] Analysis stops with error

---

#### T22: Missing Data Handling

**Inputs:**
- Data with NA values in Ki67, FollowUpMonths, and/or Recurrence
- Same as T01

**Expected Outputs:**
- [ ] Complete cases used (rows with NA removed)
- [ ] INFO notice reports actual sample size after NA removal
- [ ] Results are valid for complete cases only

---

#### T23: Timepoints Beyond Maximum Follow-Up

**Inputs:**
- Same as T01, but: `timepoints` = "120, 240, 360"

**Expected Outputs:**
- [ ] WARNING notice: "Timepoints Adjusted" -- all timepoints exceed max follow-up
- [ ] Timepoints replaced with quartiles of actual follow-up time
- [ ] Analysis completes with adjusted timepoints

---

#### T24: Few Unique Marker Values

**Inputs:**
- Custom data where marker has only 3 unique values (e.g., 1, 2, 3)
- `analysisType` = timedep

**Expected Outputs:**
- [ ] WARNING notice: "Few Unique Marker Values" (< 5 unique values)
- [ ] Analysis still attempts to run
- [ ] ROC curve may appear step-like

---

#### T25: Reversed Marker (AUC < 0.5)

**Inputs:**
- Marker where higher values = better outcome (e.g., albumin predicting death)
- `analysisType` = timedep

**Expected Outputs:**
- [ ] STRONG_WARNING notice: "AUC Below 0.5 (Reversed Marker?)"
- [ ] Suggestion to negate marker values

---

#### T26: Invalid Timepoints String

**Inputs:**
- Same as T01, but: `timepoints` = "abc, xyz"

**Expected Outputs:**
- [ ] WARNING notice: "Invalid Timepoints"
- [ ] Falls back to defaults: 12, 36, 60

---

#### T27: Single Timepoint

**Inputs:**
- Same as T01, but: `timepoints` = "24"

**Expected Outputs:**
- [ ] `aucTable` has 1 row
- [ ] `rocPlot` shows 1 curve
- [ ] `aucPlot` shows 1 point (no smoothing possible)
- [ ] `cutoffTable` has 1 row

---

#### T28: Few Events at a Timepoint

**Inputs:**
- Same as T01, but: `timepoints` = "3" (very early timepoint, few events)

**Expected Outputs:**
- [ ] WARNING notice: "Few Events at Timepoint" if < 5 events by that time
- [ ] Analysis still runs but with less reliable estimates

---

#### T29: Comparison Marker with Insufficient Data

**Inputs:**
- `analysisType` = binary, `compareROCs` = true
- `markers` includes a variable with >90% missing values

**Expected Outputs:**
- [ ] WARNING notice: "Insufficient Data" for that marker (< 10 complete cases)
- [ ] That marker is skipped in comparison
- [ ] Other markers compared normally

---

### Section 7: Notice Validation

#### T30: Notice Severity Presence

**Across all tests, verify:**
- [ ] ERROR notices have red styling (color #dc2626, bgcolor #fef2f2)
- [ ] STRONG_WARNING notices have orange styling (color #ea580c, bgcolor #fff7ed)
- [ ] WARNING notices have yellow styling (color #ca8a04, bgcolor #fefce8)
- [ ] INFO notices have blue styling (color #2563eb, bgcolor #eff6ff)
- [ ] Each notice has title (bold) and content (regular text)
- [ ] Notices cleared at start of each `.run()` call (private$.noticeList reset)

---

## COMPLETE OPTION COVERAGE CHECKLIST

Every option must appear in at least one test scenario:

| # | Option | Type | Covered In |
|---|---|---|---|
| 1 | `data` | Data | All tests (implicit) |
| 2 | `elapsedtime` | Variable | T01, T02, T03, T04, T11-T17, T19-T28 |
| 3 | `outcome` | Variable | All tests |
| 4 | `outcomeLevel` | Level | All tests |
| 5 | `marker` | Variable | All tests |
| 6 | `timepoints` | String | T01 (default), T11, T23, T26, T27, T28 |
| 7 | `method` | List | T01 (default), T03 (all 3 values) |
| 8 | `bootstrapCI` | Bool | T01 (false), T04 (true), T16 (true), T17 (false) |
| 9 | `nboot` | Integer | (deprecated/hidden -- no active test needed) |
| 10 | `plotROC` | Bool | T01 (default true), T19 (false) |
| 11 | `plotAUC` | Bool | T01 (default true), T20 (false) |
| 12 | `timetypeoutput` | List | T01 (default months), T12 (all 4 values) |
| 13 | `showOptimalCutoff` | Bool | T01 (default true), T14 (false) |
| 14 | `showMarkerStats` | Bool | T01 (default true), T15 (false) |
| 15 | `compareBaseline` | Bool | T01 (default false), T16 (true+CI), T17 (true-CI) |
| 16 | `smoothAUC` | Bool | T01 (default false), T13 (true) |
| 17 | `analysisType` | List | T01-T04 (timedep), T05-T10 (binary) |
| 18 | `compareROCs` | Bool | T05 (default false), T08-T10 (true) |
| 19 | `markers` | Variables | T08-T10, T29 |
| 20 | `rocComparison` | List | T08 (delong), T09 (bootstrap), T10 (venkatraman) |
| 21 | `youdenIndex` | Bool | T05 (default true), T18 (false) |

**Coverage: 21/21 options (100%)**
