# psychopdaROC Comprehensive Argument Testing Results

**Date:** 2026-01-01
**Test File:** `tests/test_psychopdaROC_comprehensive_arguments.R`
**Total Tests:** 54
**Passed:** 43 (79.6%)
**Failed:** 11 (20.4%)

## Summary of Errors Found

### 1. Missing Default Arguments ✅ FIXED
**Tests:** All tests (initially)
**Error:** `argument "subGroup" is missing, with no default`
**Error:** `argument "refVar" is missing, with no default`

**Root Cause:** The `.a.yaml` file didn't specify `default: null` for optional Variable and Level types.

**Fix Applied:**
- Added `default: null` to `subGroup` in `jamovi/psychopdaROC.a.yaml` (line 83)
- Added `default: null` to `refVar` in `jamovi/psychopdaROC.a.yaml` (line 603)
- Manually updated `R/psychopdaROC.h.R` (lines 2158, 2201) - will be auto-generated on next compile

**Status:** ✅ RESOLVED

---

### 2. Invalid Cutpoint Methods - Spline Methods
**Tests:** test_2_5, test_2_6
**Error:** `'maximize_spline_metric' is not an exported object from 'namespace:cutpointr'`
**Error:** `'minimize_spline_metric' is not an exported object from 'namespace:cutpointr'`

**Root Cause:** These methods don't exist in the cutpointr package. They were likely based on outdated documentation or planned features that were never implemented.

**Fix Required:**
- Remove `maximize_spline_metric` and `minimize_spline_metric` from method options in `jamovi/psychopdaROC.a.yaml`

**Impact:** Users won't be able to select these non-existent methods

**Status:** ⏳ TO FIX

---

### 3. Invalid Optimization Metrics
**Tests:** test_3_9, test_3_11, test_3_12
**Error:** Metrics `ij_cutpoint`, `cost_loss_accuracy`, and `brier_score` not in cutpointr's allowed metrics

**Valid Metrics (from cutpointr):**
- `youden`, `sum_sens_spec`, `accuracy`, `sum_ppv_npv`
- `prod_sens_spec`, `prod_ppv_npv`, `cohens_kappa`
- `abs_d_sens_spec`, `abs_d_ppv_npv`, `F1_score`
- `odds_ratio`, `risk_ratio`, `misclassification_cost`
- `total_utility`, `roc01`, `p_chisquared`

**Fix Required:**
- Remove `ij_cutpoint`, `cost_loss_accuracy`, and `brier_score` from metric options in `jamovi/psychopdaROC.a.yaml`

**Status:** ⏳ TO FIX

---

### 4. DeLong Test setNote Error with NA Data
**Test:** test_4_2
**Error:** `'setNote' does not exist in this results element`

**Root Cause:** In the recent NA handling fix (lines 2545-2548 of `R/psychopdaROC.b.R`), I attempted to add a note to `self$results$delongTest`, but this element doesn't support `setNote()`.

**Fix Required:**
- Change to use `self$results$delongComparisonTable$setNote()` instead
- OR create an HTML output element for NA warnings

**Status:** ⏳ TO FIX

---

### 5. Bootstrap Validation - Minimum Values Too Restrictive
**Tests:** test_4_3, test_4_4 (idiNriBootRuns), test_6_2 (bootstrapReps)
**Error:** `idiNriBootRuns must be between 100 and 10000 (is 50)`
**Error:** `bootstrapReps must be between 100 and 10000 (is 50)`

**Root Cause:** Validation rules set minimum to 100, but this prevents quick testing and may be unnecessarily restrictive for exploratory analysis.

**Options:**
1. Keep validation as-is (forces good statistical practice)
2. Lower minimum to 50 or 25 for faster exploratory analysis
3. Add warning instead of error for values below 100

**Recommendation:** Lower minimum to 50 with a warning message for values below 100.

**Fix Required:**
- Update validation in `jamovi/psychopdaROC.a.yaml` for both parameters
- Add conditional warning in `R/psychopdaROC.b.R` when values < 100

**Status:** ⏳ TO FIX (DESIGN DECISION NEEDED)

---

### 6. Missing Result Elements - Effect Size Analysis
**Test:** test_6_3
**Error:** `No such key or name`

**Root Cause:** The `effectSizeAnalysis` option is defined but the corresponding result output elements may not be properly defined in `.r.yaml` or are not being populated correctly.

**Investigation Needed:**
- Check `jamovi/psychopdaROC.r.yaml` for effect size result elements
- Check `R/psychopdaROC.b.R` for effect size result population code
- Verify that `effectSizeMethod` option is being used

**Status:** ⏳ TO INVESTIGATE & FIX

---

### 7. Missing Result Elements - Power Analysis
**Test:** test_6_4
**Error:** `No such key or name`

**Root Cause:** Similar to effect size - the `powerAnalysis` option exists but result elements are missing or not populated.

**Investigation Needed:**
- Check `jamovi/psychopdaROC.r.yaml` for power analysis result elements
- Check `R/psychopdaROC.b.R` for power analysis implementation
- Verify all related options: `powerAnalysisType`, `expectedAUCDifference`, `targetPower`, `significanceLevel`, `correlationROCs`

**Status:** ⏳ TO INVESTIGATE & FIX

---

## Additional Issues Found

### 8. Warning Messages - max() with No Non-Missing Arguments
**Warnings:** Multiple instances during testing
**Message:** `no non-missing arguments to max; returning -Inf`

**Likely Cause:** Code attempting to find maximum value in an empty or all-NA vector, possibly in ROC curve calculation code.

**Impact:** Non-fatal but indicates potential edge case handling issues

**Status:** ⏳ TO INVESTIGATE

---

## Tests That Passed ✅

### Basic Functionality (4/4)
- ✅ Single dependent variable
- ✅ Multiple dependent variables
- ✅ Data with NA values
- ✅ Subgroup analysis

### Cutpoint Methods (12/14)
- ✅ maximize_metric, minimize_metric
- ✅ maximize_loess_metric, minimize_loess_metric
- ❌ maximize_spline_metric, minimize_spline_metric (don't exist)
- ✅ maximize_boot_metric, minimize_boot_metric
- ✅ oc_youden_kernel, oc_youden_normal
- ✅ oc_manual, oc_cost_ratio
- ✅ oc_equal_sens_spec, oc_closest_01

### Optimization Metrics (9/12)
- ✅ youden, accuracy, F1_score
- ✅ cohens_kappa, sum_sens_spec, prod_sens_spec
- ✅ abs_d_sens_spec, roc01, misclassification_cost
- ❌ ij_cutpoint, cost_loss_accuracy, brier_score (invalid)

### Statistical Comparisons (2/4)
- ✅ DeLong test (clean data)
- ❌ DeLong test with NA data (setNote error)
- ❌ IDI calculation (bootstrap minimum)
- ❌ NRI calculation (bootstrap minimum)

### Plotting (4/4)
- ✅ Basic ROC plot
- ✅ Combined plots
- ✅ Criterion plot
- ✅ Precision-Recall curve

### Advanced Features (1/4)
- ✅ Partial AUC
- ❌ Bootstrap CI (minimum value)
- ❌ Effect size analysis (missing results)
- ❌ Power analysis (missing results)

### Fixed Sens/Spec Analysis (5/5)
- ✅ Fixed sensitivity
- ✅ Fixed specificity
- ✅ Linear interpolation
- ✅ Nearest interpolation
- ✅ Stepwise interpolation

### Clinical Modes & Presets (7/7)
- ✅ Basic, Advanced, Comprehensive modes
- ✅ Screening, Confirmation, Balanced, Research presets

---

## Recommended Fixes Priority

### High Priority (Breaks Functionality)
1. ✅ Fix missing defaults (subGroup, refVar) - COMPLETED
2. Remove invalid cutpoint methods (spline)
3. Remove invalid metrics (ij_cutpoint, cost_loss_accuracy, brier_score)
4. Fix setNote error for DeLong with NA data

### Medium Priority (Usability Issues)
5. Investigate and fix effect size analysis results
6. Investigate and fix power analysis results
7. Review bootstrap minimum values (design decision)

### Low Priority (Warnings/Edge Cases)
8. Investigate max() warning messages
9. Add more comprehensive edge case handling

---

## Next Steps

1. Remove invalid options from `.a.yaml`
2. Fix DeLong NA handling to use correct result element
3. Investigate missing result implementations for effect size and power analysis
4. Re-run comprehensive tests to verify all fixes
5. Update user documentation to reflect valid options only

---

## Files Modified

- `jamovi/psychopdaROC.a.yaml` - Added defaults for optional parameters
- `R/psychopdaROC.h.R` - Manually updated (will be regenerated on compile)
- `R/psychopdaROC.b.R` - Added NA filtering for DeLong test
- `tests/test_psychopdaROC_comprehensive_arguments.R` - New comprehensive test suite

---

**Test Author:** Claude Code
**Review Status:** Pending human review
**Next Compile:** Required to regenerate .h.R file from updated .a.yaml
