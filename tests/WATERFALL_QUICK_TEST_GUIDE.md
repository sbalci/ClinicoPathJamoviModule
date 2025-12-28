# Waterfall Function - Quick Testing Reference Card

## 🚀 Quick Start

### Load Data in jamovi
```
File → Open → data/waterfall_percentage_basic.omv
```

### Run Analysis
```
Analyses → OncoPathT → Patient Follow-Up Plots → Treatment Response Analysis
```

---

## 📊 5-Minute Test Checklist

### ✅ Test 1: Basic Waterfall (2 min)
- **Data**: `waterfall_percentage_basic.omv`
- **Settings**:
  - Patient ID: `PatientID`
  - Response Value: `Response`
  - Input Type: `Percentage Changes`
  - Show Waterfall Plot: ✅
  - Show RECIST Thresholds: ✅
- **Expected**: Waterfall plot with bars sorted by response, threshold lines at -30% and +20%

---

### ✅ Test 2: Spider Plot (1 min)
- **Data**: `waterfall_raw_longitudinal.omv`
- **Settings**:
  - Patient ID: `PatientID`
  - Response Value: `Measurement`
  - Time Variable: `Time`
  - Input Type: `Raw Measurements`
  - Show Spider Plot: ✅
- **Expected**: Spaghetti plot showing response trajectories over time

---

### ✅ Test 3: Group Comparison (1 min)
- **Data**: `waterfall_oncology_trial.csv`
- **Settings**:
  - Group Variable: `Treatment`
  - Color By: `Patient Groups`
  - Color Scheme: `Colorful`
- **Expected**: Bars colored by treatment group, comparison table showing ORR/DCR by group

---

### ✅ Test 4: Clinical Report (1 min)
- **Data**: `waterfall_oncology_trial.csv`
- **Settings**:
  - Show Confidence Intervals: ✅
  - Generate Copy-Ready Report: ✅
  - Show Clinical Significance: ✅
- **Expected**: Tables with CIs, copy-ready text for manuscripts

---

### ✅ Test 5: Error Handling (<1 min)
- **Data**: `waterfall_missing_baseline.csv`
- **Settings**:
  - Input Type: `Raw Measurements`
  - Time Variable: `Time`
- **Expected**: Clear error message about missing baseline measurements

---

## 🎯 Key Features to Verify

| Feature | Test File | Expected Result |
|---------|-----------|----------------|
| RECIST Categories | `waterfall_percentage_basic` | 4 categories: CR, PR, SD, PD |
| ORR Calculation | `waterfall_oncology_trial` | ~40% (CR + PR) |
| DCR Calculation | `waterfall_oncology_trial` | ~70% (CR + PR + SD) |
| Threshold Lines | Any percentage data | Dashed lines at -30% and +20% |
| Spider Plot | `waterfall_raw_longitudinal` | Lines showing trajectories |
| Group Colors | `waterfall_oncology_trial` | Different colors by treatment |
| Edge Case Handling | `waterfall_edge_cases` | Warnings for invalid values |
| Single Patient | `waterfall_single_patient` | Warning but still produces plot |

---

## 🧪 R Console Quick Tests

### Test 1: Basic Usage
```r
library(ClinicoPath)
data("waterfall_percentage_basic")

result <- waterfall(
  data = waterfall_percentage_basic,
  patientID = "PatientID",
  responseVar = "Response",
  inputType = "percentage"
)

print(result$summaryTable$asDF)
```

### Test 2: With Groups
```r
data("waterfall_oncology_trial")

result <- waterfall(
  data = waterfall_oncology_trial,
  patientID = "PatientID",
  responseVar = "Response",
  groupVar = "Treatment",
  colorBy = "group"
)

print(result$groupComparisonTable$asDF)
```

### Test 3: Spider Plot
```r
data("waterfall_raw_longitudinal")

result <- waterfall(
  data = waterfall_raw_longitudinal,
  patientID = "PatientID",
  responseVar = "Measurement",
  timeVar = "Time",
  inputType = "raw",
  showSpiderPlot = TRUE
)

print(result$personTimeTable$asDF)
```

---

## 🔍 What to Look For

### Waterfall Plot
- ✅ Bars sorted by response (default) or patient ID
- ✅ Correct RECIST colors: Blue (CR/PR) → Green (SD) → Red (PD)
- ✅ Threshold lines at -30% and +20% (if enabled)
- ✅ Y-axis range appropriate for data
- ✅ Patient IDs visible (if few patients) or suppressed (if many)

### Spider Plot
- ✅ One line per patient
- ✅ All lines start at time = 0
- ✅ Lines colored by responder status or group
- ✅ X-axis labeled with correct time unit
- ✅ Y-axis shows percentage change from baseline
- ✅ Threshold zones shaded (if enabled)

### Summary Tables
- ✅ RECIST categories sum to 100%
- ✅ ORR = (CR + PR) / Total × 100
- ✅ DCR = (CR + PR + SD) / Total × 100
- ✅ Sample sizes match (n in each category)
- ✅ Confidence intervals present (if enabled)

### Clinical Metrics
- ✅ ORR with 95% CI
- ✅ DCR with 95% CI
- ✅ Median response with IQR
- ✅ Time to response (if longitudinal data)
- ✅ Duration of response (if longitudinal data)

### Notices/Warnings
- ✅ Clear error messages for missing required variables
- ✅ Warnings for edge cases (invalid shrinkage, large growth)
- ✅ Informative guidance for data issues
- ✅ No spurious warnings on valid data

---

## 🐛 Common Issues & Fixes

| Issue | Cause | Fix |
|-------|-------|-----|
| "Time Variable Required" error | Using raw data without time variable | Set Input Type to "Percentage Changes" OR add Time Variable |
| "Missing Baseline" error | No time=0 measurements | Add baseline measurements OR use percentage data |
| Empty spider plot | No time variable specified | Select time variable in settings |
| All bars same color | Color By set to RECIST but no variation | Check data has multiple RECIST categories |
| "Only 1 patient" warning | Dataset has n=1 | Expected behavior; warning is informative |
| Invalid shrinkage warning | Values <-100% | Expected behavior; values capped at -100% |

---

## 📁 Test Data Files Reference

| File | Patients | Purpose | Key Features |
|------|----------|---------|--------------|
| `waterfall_percentage_basic` | 20 | Basic waterfall | All RECIST categories, 2 groups |
| `waterfall_raw_longitudinal` | 15 | Spider plot | 4 time points, various patterns |
| `waterfall_oncology_trial` | 50 | Realistic trial | Demographics, balanced arms |
| `waterfall_edge_cases` | 10 | Edge cases | Boundaries, invalid values |
| `waterfall_single_patient` | 1 | Minimum data | Warning message test |
| `waterfall_missing_baseline` | 3 | Error test | Missing baseline validation |
| `waterfall_time_to_event` | 30 | Person-time | Event times, censoring |

---

## 🎨 Color Schemes Quick Reference

| Scheme | Best For | Colors |
|--------|----------|--------|
| `jamovi` | Default, clean | jamovi theme colors |
| `RECIST` | Clinical standard | Red (PD), Yellow (SD), Blue (PR), Green (CR) |
| `Simple` | Publications | Red (bad) vs Green (good) |
| `Colorful` | Group comparisons | Distinct colors for each group |
| `Colorblind` | Accessibility | Okabe-Ito palette |

---

## ⚡ Running Automated Tests

### All Waterfall Tests
```r
testthat::test_dir("tests/testthat", filter = "waterfall")
```

### Specific Test File
```r
testthat::test_file("tests/testthat/test-waterfall.R")
```

### Verification Script
```bash
Rscript tests/verify_waterfall.R
```

### Expected Output
```
✅ Testing Basic Percentage Analysis...
   Success
✅ Testing Raw Data Analysis...
   Success
✅ Testing Grouped Analysis...
   Success
⚠️  Testing Missing Time Variable (Expected Failure/Warning)...
   Success (failed gracefully as expected)
✅ Testing Clinical Reporting...
   Success
```

---

## 📝 Manual Test Log Template

```
Date: ___________
Tester: ___________
Version: ___________

[ ] Test 1: Basic Waterfall (waterfall_percentage_basic)
    Result: PASS / FAIL
    Notes: ________________________________

[ ] Test 2: Spider Plot (waterfall_raw_longitudinal)
    Result: PASS / FAIL
    Notes: ________________________________

[ ] Test 3: Group Comparison (waterfall_oncology_trial)
    Result: PASS / FAIL
    Notes: ________________________________

[ ] Test 4: Clinical Report (waterfall_oncology_trial)
    Result: PASS / FAIL
    Notes: ________________________________

[ ] Test 5: Error Handling (waterfall_missing_baseline)
    Result: PASS / FAIL
    Notes: ________________________________

Overall Status: PASS / FAIL
Issues Found: _________________________
```

---

## 🔗 Resources

- **Full Test Guide**: `tests/WATERFALL_TEST_DATA_GUIDE.md`
- **Test Data Generator**: `tests/generate_waterfall_test_data.R`
- **Automated Tests**: `tests/testthat/test-waterfall*.R`
- **Verification Script**: `tests/verify_waterfall.R`
- **Function Documentation**: `?ClinicoPath::waterfall`

---

**Last Updated**: 2025-12-28
**Quick Test Time**: ~5 minutes for all 5 tests
