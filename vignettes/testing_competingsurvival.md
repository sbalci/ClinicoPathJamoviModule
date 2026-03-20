# Competing Survival Analysis - Testing Checklist

## Test Datasets

**Primary:** `competing_survival_data` (300 patients, 10 variables)
- Load: `data("competing_survival_data", package = "ClinicoPath")`
- Outcome levels: "Dead_of_Disease" (115), "Dead_of_Other" (41), "Alive_w_Disease" (57), "Alive_wo_Disease" (87)
- Group variables: Treatment (Experimental_Therapy / Standard_Care), Tumor_Stage (I/II/III/IV), Sex (Female/Male)
- Time variable: Overall_Time (continuous, months)
- Additional factors: Grade (1/2/3), EGFR_Status (Negative/Positive), ECOG_PS (0/1/2)

**Secondary:** `survival_competing` (180 patients, 8 variables)
- Load: `data("survival_competing", package = "ClinicoPath")`
- Outcome levels: "Alive w/o Disease" (55), "Alive w Disease" (35), "Dead of Disease" (53), "Dead of Other" (37)
- Group variables: treatment (Experimental / Standard), stage (I/II/III/IV), molecular_subtype (Type A/B/C)
- Time variable: elapsedtime (continuous, months)

---

## 1. Basic Functionality

### T01: Overall Survival Analysis

**Inputs:**
- `overalltime` = Overall_Time
- `outcome` = Outcome
- `explanatory` = Treatment
- `analysistype` = "overall"
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"
- `awd` = "Alive_w_Disease"
- `awod` = "Alive_wo_Disease"

**Expected Outputs:**
- [ ] `survivalTable` populated with HR for Treatment (Experimental vs Standard)
- [ ] `summary` HTML shows "Overall Survival Analysis Results"
- [ ] `interpretation` HTML describes overall survival interpretation
- [ ] `assumptions` HTML populated with assumptions/caveats

**Verify:**
- All deaths (Dead_of_Disease + Dead_of_Other = 156) counted as events
- Alive patients (Alive_w_Disease + Alive_wo_Disease = 144) counted as censored
- HR compares Experimental_Therapy vs Standard_Care

---

### T02: Cause-Specific Survival Analysis

**Inputs:**
- Same as T01 but `analysistype` = "cause"

**Expected Outputs:**
- [ ] `survivalTable` populated with cause-specific HR
- [ ] `summary` HTML shows "Cause-Specific Survival Analysis Results"
- [ ] `interpretation` describes cause-specific interpretation (disease death only)

**Verify:**
- Only Dead_of_Disease (115) counted as events
- Dead_of_Other + Alive patients all counted as censored
- Different HR than overall survival

---

### T03: Competing Risks Analysis

**Inputs:**
- Same as T01 but `analysistype` = "compete"

**Expected Outputs:**
- [ ] `survivalTable` populated with competing risks HR
- [ ] `cuminc` table populated with CIF estimates at default timepoints (12, 24, 36, 60 months)
- [ ] `comprisksPlot` rendered (CIF plot)
- [ ] `summary` shows "Competing Risks Analysis Results"
- [ ] `interpretation` describes competing risks interpretation
- [ ] `assumptions` describes competing risks assumptions including independent censoring, event classification, Fine-Gray model, Gray's test

**Verify:**
- Disease death (status_crr=1): 115 events
- Competing death (status_crr=2): 41 events
- CIF estimates are between 0 and 1 and monotonically non-decreasing
- CIF1 + CIF2 never exceeds 1.0

---

## 2. Outcome Level Configuration

### T04: Different Outcome Levels (survival_competing dataset)

**Inputs:**
- Dataset: `survival_competing`
- `overalltime` = elapsedtime
- `outcome` = outcome
- `explanatory` = treatment
- `analysistype` = "overall"
- `dod` = "Dead of Disease"
- `dooc` = "Dead of Other"
- `awd` = "Alive w Disease"
- `awod` = "Alive w/o Disease"

**Expected Outputs:**
- [ ] `survivalTable` populated with HR
- [ ] Analysis runs without error using different level naming conventions

**Verify:**
- Deaths: Dead of Disease (53) + Dead of Other (37) = 90 events for overall
- Censored: 55 + 35 = 90 patients

---

### T05: Overall Survival with Only DOD Defined

**Inputs:**
- `analysistype` = "overall"
- `dod` = "Dead_of_Disease"
- `dooc` = NULL (not set)
- `awd` = NULL
- `awod` = NULL

**Expected Outputs:**
- [ ] Analysis runs; only Dead_of_Disease treated as death event
- [ ] No error about missing dooc

**Verify:**
- Only 115 disease deaths treated as events
- All other outcomes treated as censored

---

### T06: Competing Risks Requires Both Death Types

**Inputs:**
- `analysistype` = "compete"
- `dod` = "Dead_of_Disease"
- `dooc` = NULL (not set)

**Expected Outputs:**
- [ ] Error message: "For competing risks analysis, please specify 'Dead of Other' level"

---

## 3. Gray's Test

### T07: Gray's Test Enabled with Group Variable

**Inputs:**
- `analysistype` = "compete"
- `explanatory` = Treatment
- `graystest` = TRUE
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] `summary` HTML includes Gray's Test Results section
- [ ] Test statistic (chi-squared), degrees of freedom, and p-value reported for disease death
- [ ] If two event types, separate test results for each

**Verify:**
- Gray's test chi-squared is non-negative
- Degrees of freedom = number of groups - 1 (for Treatment: df=1)
- p-value is between 0 and 1

---

### T08: Gray's Test Without Group Variable

**Inputs:**
- `analysistype` = "compete"
- `explanatory` = NULL (not set)
- `graystest` = TRUE
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] Analysis runs; Gray's test result is NULL (no groups to compare)
- [ ] Summary does not include Gray's test section

---

## 4. Fine-Gray Model

### T09: Subdistribution Hazard Model Enabled

**Inputs:**
- `analysistype` = "compete"
- `explanatory` = Treatment
- `subdistribution` = TRUE
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"
- `awd` = "Alive_w_Disease"
- `awod` = "Alive_wo_Disease"

**Expected Outputs:**
- [ ] `survivalTable` includes row for "Subdistribution HR (Fine-Gray)"
- [ ] `fineGrayTable` populated with coefficient, HR, CI, SE, z, p columns
- [ ] fineGrayTable has note about Fine-Gray model interpretation

**Verify:**
- Subdistribution HR is positive (exp(coef) > 0)
- CI contains the HR point estimate
- p-value is between 0 and 1
- Fine-Gray HR may differ from cause-specific HR

---

### T10: Fine-Gray Coefficient Table

**Inputs:**
- Same as T09

**Expected Outputs:**
- [ ] `fineGrayTable` columns: term, coef, hr, hr_lower, hr_upper, se, z, p
- [ ] Term label includes comparison description (e.g., "Standard_Care vs Experimental_Therapy (ref)")
- [ ] Note text: "Fine-Gray subdistribution hazard model. HR > 1 indicates higher cumulative incidence. 95% CI shown."

**Verify:**
- exp(coef) equals HR (within rounding)
- hr_lower < hr < hr_upper
- z = coef / se (approximately)

---

## 5. CIF Timepoints

### T11: Default Timepoints (12, 24, 36, 60)

**Inputs:**
- `analysistype` = "compete"
- `timepoints` = "12,24,36,60" (default)
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] `cuminc` table has 4 rows (one per timepoint)
- [ ] Columns: time, est_1 (CIF Disease Death), est_2 (CIF Other Death), var_1, var_2
- [ ] Summary HTML includes "Cumulative Incidence at Key Time Points" table

**Verify:**
- CIF estimates increase over time (or stay same)
- Variance values are non-negative
- est_1 + est_2 <= 1.0 at each timepoint

---

### T12: Custom Timepoints

**Inputs:**
- `analysistype` = "compete"
- `timepoints` = "6,18,30,48"
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] `cuminc` table has 4 rows at times 6, 18, 30, 48
- [ ] Summary HTML table shows estimates at custom timepoints

**Verify:**
- Timepoints match requested values in the time column
- CIF estimates are consistent (earlier timepoints have lower or equal CIF)

---

## 6. Confidence Level

### T13: Default Confidence Level (0.95)

**Inputs:**
- `analysistype` = "compete"
- `subdistribution` = TRUE
- `explanatory` = Treatment
- `confidencelevel` = 0.95
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] Fine-Gray table note says "95% CI shown"
- [ ] Summary timepoint CI labels show "95% CI"
- [ ] CI width reflects 95% confidence

---

### T14: Alternative Confidence Level (0.90)

**Inputs:**
- Same as T13 but `confidencelevel` = 0.90

**Expected Outputs:**
- [ ] Fine-Gray table note says "90% CI shown"
- [ ] CI intervals are narrower than 95% CI
- [ ] Summary timepoint CI labels show "90% CI"

**Verify:**
- HR CI at 90% is strictly contained within HR CI at 95% for the same data

---

## 7. Plots

### T15: CIF Plot (Competing Risks Plot)

**Inputs:**
- `analysistype` = "compete"
- `explanatory` = Treatment
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"
- `cifColors` = "default"

**Expected Outputs:**
- [ ] `comprisksPlot` renders with step function curves
- [ ] Title: "Cumulative Incidence Function"
- [ ] X-axis: "Time (months)", Y-axis: "Cumulative Incidence" (percentage scale)
- [ ] Legend shows event type groups
- [ ] Curves use Red/Blue default color scheme

**Verify:**
- CIF curves are monotonically non-decreasing
- Y-axis ranges from 0% to 100%
- Multiple curves visible (one per group-event combination)

---

### T16: Stacked Probability Plot

**Inputs:**
- `analysistype` = "compete"
- `showStackedPlot` = TRUE
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"
- `cifColors` = "colorblind"

**Expected Outputs:**
- [ ] `stackedPlot` renders with stacked area chart
- [ ] Title: "Stacked Probability Plot: CIF1 + CIF2 + Survival"
- [ ] Three stacked areas: CIF Disease Death, CIF Competing Death, Survival
- [ ] Total area fills to 100% at each time point
- [ ] Colorblind-safe color palette used

**Verify:**
- At time 0: Survival approx 100%, both CIFs approx 0%
- CIF Disease Death + CIF Competing Death + Survival = 100% at every time
- Colors match colorblind palette (#56B4E9, #E69F00, #D55E00)

---

### T17: KM vs CIF Comparison Plot

**Inputs:**
- `analysistype` = "compete"
- `showKMvsCIF` = TRUE
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"
- `cifColors` = "default"

**Expected Outputs:**
- [ ] `kmvscifPlot` renders with two curves
- [ ] Title: "Comparison: Naive 1-KM vs. Proper CIF"
- [ ] Subtitle explains bias direction
- [ ] Legend: "Naive 1-KM (Biased)" (dashed) and "Proper CIF (Unbiased)" (solid)
- [ ] Legend at bottom of plot

**Verify:**
- 1-KM curve is always >= CIF curve (naive overestimates)
- Difference increases over time as competing events accumulate
- Both curves start at 0 (time 0)

---

## 8. Edge Cases

### T18: Small Sample Size

**Inputs:**
- Create subset of competing_survival_data with 20 rows
- `analysistype` = "compete"
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] If >= 5 events of each type: analysis completes with wider CIs
- [ ] If < 5 events of either type: error about too few events

**Verify:**
- Event count validation catches inadequate sample

---

### T19: All Censored (No Events)

**Inputs:**
- Create dataset where all patients are Alive_wo_Disease
- `analysistype` = "overall"
- `dod` = "Dead_of_Disease"

**Expected Outputs:**
- [ ] survivalTable shows no hazard ratios computed or median survival is NA
- [ ] No crash / graceful handling

---

### T20: Single Group (No Explanatory Variable)

**Inputs:**
- `explanatory` = NULL (not set)
- `analysistype` = "overall"
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] `survivalTable` shows single row with "All patients" and median survival
- [ ] `summary` indicates no group comparison
- [ ] No errors from missing explanatory variable

**Verify:**
- Total n matches dataset size (after NA removal)
- Events count is correct

---

### T21: Missing Data Handling

**Inputs:**
- Introduce NAs in Overall_Time and Outcome for 10% of rows
- `analysistype` = "compete"
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] Analysis runs on complete cases only
- [ ] No crash from NAs in key variables
- [ ] Sample size in results reflects rows after NA removal

---

## 9. Event Count Validation

### T22: Fewer Than 5 Disease Events

**Inputs:**
- Create subset with only 3 disease deaths
- `analysistype` = "compete"
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] Error: "Too few disease events (3). Competing risks analysis requires at least 5 events of each type for reliable estimates."

---

### T23: Adequate Events in Both Categories

**Inputs:**
- Full competing_survival_data (115 disease deaths, 41 other deaths)
- `analysistype` = "compete"
- `dod` = "Dead_of_Disease"
- `dooc` = "Dead_of_Other"

**Expected Outputs:**
- [ ] Analysis completes without event count warnings
- [ ] CIF estimates are stable (small variances)

**Verify:**
- 115 disease events and 41 competing events both exceed the 5-event minimum
- Variance columns in cuminc table have small values relative to estimates

---

## Complete Option Coverage Checklist

| Option | Type | Default | Tests Covering It |
|--------|------|---------|-------------------|
| `data` | Data | (required) | All tests |
| `explanatory` | Variable | NULL | T01-T03, T07, T09-T10, T13-T17, T20 |
| `overalltime` | Variable | (required) | All tests |
| `outcome` | Variable | (required) | All tests |
| `dod` | Level | NULL (allowNone) | All tests except T05 dooc-only |
| `dooc` | Level | NULL (allowNone) | T01-T04, T06-T17, T21-T23 |
| `awd` | Level | NULL (allowNone) | T01, T04, T05, T09 |
| `awod` | Level | NULL (allowNone) | T01, T04, T05, T09 |
| `analysistype` | List (overall/cause/compete) | "overall" | T01(overall), T02(cause), T03-T17(compete), T19-T20(overall) |
| `graystest` | Bool | FALSE | T07(TRUE), T08(TRUE, no group) |
| `subdistribution` | Bool | FALSE | T09-T10(TRUE), T13-T14(TRUE) |
| `timepoints` | String | "12,24,36,60" | T11(default), T12(custom) |
| `confidencelevel` | Number | 0.95 | T13(0.95), T14(0.90) |
| `showrisksets` | Bool | FALSE | Not explicitly tested (risk table is summary-only placeholder) |
| `showStackedPlot` | Bool | FALSE | T16(TRUE) |
| `showKMvsCIF` | Bool | FALSE | T17(TRUE) |
| `cifColors` | List (default/colorblind/grayscale) | "default" | T15(default), T16(colorblind), T17(default) |

### Uncovered / Low-Priority Options

- `showrisksets`: Currently a summary-only placeholder (risk table beneath CIF plot is noted as TODO in source). Test by enabling and verifying the summary text appends risk set counts.
- `cifColors` = "grayscale": Not explicitly tested above. Add a quick visual check using `cifColors = "grayscale"` on any competing risks plot.

---

## Quick Smoke Test (Minimum Viable Verification)

```r
data("competing_survival_data", package = "ClinicoPath")

# Overall survival
competingsurvival(
  data = competing_survival_data,
  overalltime = "Overall_Time",
  outcome = "Outcome",
  explanatory = "Treatment",
  analysistype = "overall",
  dod = "Dead_of_Disease",
  dooc = "Dead_of_Other",
  awd = "Alive_w_Disease",
  awod = "Alive_wo_Disease"
)

# Competing risks with all features
competingsurvival(
  data = competing_survival_data,
  overalltime = "Overall_Time",
  outcome = "Outcome",
  explanatory = "Treatment",
  analysistype = "compete",
  dod = "Dead_of_Disease",
  dooc = "Dead_of_Other",
  awd = "Alive_w_Disease",
  awod = "Alive_wo_Disease",
  graystest = TRUE,
  subdistribution = TRUE,
  timepoints = "12,24,36,60",
  confidencelevel = 0.95,
  showStackedPlot = TRUE,
  showKMvsCIF = TRUE,
  cifColors = "default"
)
```
