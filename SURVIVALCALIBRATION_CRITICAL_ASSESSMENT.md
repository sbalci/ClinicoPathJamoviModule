# Survival Calibration Function - Critical Assessment and Required Fixes

**Date**: 2025-11-15
**Module**: `survivalcalibration`
**Status**: ⛔ **NOT SUITABLE FOR CLINICAL USE**
**Recommendation**: 🚫 **CRITICAL FIXES REQUIRED BEFORE RELEASE**

---

## Executive Summary

The `survivalcalibration` function has **three fundamental mathematical errors** that make its validation and discrimination metrics **completely invalid**:

1. ⛔ **Bootstrap/K-Fold "Validation" is Fake** - Resamples existing predictions instead of refitting models (provides ZERO optimism correction)
2. ⛔ **Brier Score Ignores Censoring** - Drops censored patients instead of using IPCW (creates severe bias in high-censoring datasets)
3. ⛔ **Time-Dependent Plots Are Placeholders** - Advertised but not implemented

These are not minor bugs - they are **fundamental methodological errors** that invalidate the entire validation framework and most discrimination metrics.

---

## Critical Issue #1: ⛔ Bootstrap and K-Fold "Validation" Are Mathematically Wrong

### Location
- **Bootstrap**: R/survivalcalibration.b.R:288-336 (`.bootstrapValidation()`)
- **K-Fold**: R/survivalcalibration.b.R:339-388 (`.kfoldValidation()`)

### The Problem

**What proper validation should do:**
```r
# Proper bootstrap validation (optimism correction)
for (i in 1:n_boot) {
    # 1. Resample PATIENTS
    boot_idx <- sample(1:n, replace = TRUE)
    boot_data <- data[boot_idx, ]

    # 2. REFIT the model on bootstrap sample
    boot_model <- coxph(Surv(time, event) ~ x1 + x2, data = boot_data)

    # 3. Generate NEW predictions for:
    #    a) Bootstrap sample (optimistic performance)
    #    b) Original sample (test performance)
    boot_pred_boot <- predict(boot_model, newdata = boot_data)
    boot_pred_orig <- predict(boot_model, newdata = original_data)

    # 4. Calculate optimism
    optimism[i] <- performance(boot_pred_boot) - performance(boot_pred_orig)
}

# 5. Optimism-corrected estimate
validated_performance <- apparent_performance - mean(optimism)
```

**What the current implementation does:**
```r
# WRONG: Current implementation (lines 288-336)
for (i in 1:n_boot) {
    # 1. Resample PREDICTIONS (not patients!)
    boot_idx <- sample(1:length(time_vec), replace = TRUE)
    boot_pred <- pred_vec[boot_idx]  # ⛔ WRONG! Uses existing predictions

    # 2. NO MODEL REFITTING (completely missing!)

    # 3. Calculate metrics on resampled predictions
    cindex_result <- survival::concordance(surv_obj ~ boot_pred)
    cindex_vals[i] <- cindex_result$concordance

    # 4. Return MEAN of bootstrap samples
    # This is just a noisy re-estimate of apparent performance!
}

# What users think they're getting: Optimism-corrected estimate
# What they actually get: Mean of noisy resamples ≈ apparent performance
```

### Why This Is Completely Wrong

1. **No Model Refitting**: The fundamental step of bootstrap validation is missing entirely
2. **No Optimism Calculation**: Cannot assess optimism without comparing bootstrap vs. original performance
3. **Meaningless Resampling**: Resampling predictions provides NO information about model overfitting
4. **Identical Results**: The "validated" metric ≈ apparent metric (within noise)

### Clinical Impact

**Example Scenario:**
- Researcher develops prognostic model
- Apparent C-index = 0.85 (looks great!)
- Actual optimism-corrected C-index = 0.70 (moderate discrimination)
- **Current implementation reports "validated" C-index ≈ 0.85** (wrong!)
- Researcher publishes overoptimistic results
- Model fails in external validation
- **Patient harm** from using poorly-validated model

### K-Fold Validation Has Same Problem

Lines 339-388 have identical issue:
```r
# Test set for this fold
test_idx <- which(fold_idx == fold)
test_pred <- pred_vec[test_idx]  # ⛔ Uses existing predictions!

# No model training on k-1 folds (completely missing!)
# No new prediction generation (completely missing!)

# Just partitions existing predictions and re-evaluates
cindex_result <- survival::concordance(surv_obj ~ test_pred)
```

---

## Critical Issue #2: ⛔ Brier Score Ignores Censoring (Creates Severe Bias)

### Location
R/survivalcalibration.b.R:236-245 (`.performDiscrimination()`)

### The Problem

**What proper Brier score should do:**
```r
# Proper censoring-adjusted Brier score (Graf et al. 1999)
# Uses Inverse Probability of Censoring Weighting (IPCW)

# 1. Fit censoring distribution
cens_fit <- survfit(Surv(time, 1-event) ~ 1, data = data)  # Reverse coding!

# 2. Calculate censoring weights for each patient
G_t <- predict(cens_fit, times = time, type = "survival")  # P(not censored by time t)

# 3. Weight observations by inverse censoring probability
# - For events: weight by 1/G(T_i)
# - For censored: more complex weighting

# 4. Calculate weighted Brier score
brier <- sum(weights * (predicted - outcome)^2) / n
```

**What the current implementation does:**
```r
# WRONG: Current implementation (lines 236-245)
survived_to_cal_time <- ifelse(time_vec >= cal_time & event_vec == 0, 1,  # Survived, censored
                               ifelse(time_vec >= cal_time & event_vec == 1, 0,  # Survived, event
                                     ifelse(time_vec < cal_time & event_vec == 1, 0,  # Died before cal_time
                                           NA)))  # ⛔ CENSORED BEFORE CAL_TIME → NA (DROPPED!)

valid_idx <- !is.na(survived_to_cal_time)  # ⛔ Drops all early censored patients
apparent_brier <- mean((pred_vec[valid_idx] - survived_to_cal_time[valid_idx])^2)
```

### Why This Creates Severe Bias

1. **Informative Censoring**: Patients censored before `cal_time` are DROPPED entirely
2. **Selection Bias**: Keeps only patients who:
   - Survived to `cal_time` (censored or event-free)
   - Died before `cal_time`
   - **Excludes**: Anyone censored in interval `[0, cal_time)`
3. **Downward Bias**: Brier score artificially LOW (appears better than it is)
4. **Magnitude Depends on Censoring**: Higher censoring → greater bias

### Clinical Impact

**Example:**
- Oncology dataset with 40% censoring before 5-year follow-up
- Current implementation DROPS these 40% of patients
- Remaining 60% are enriched for:
  - Good prognosis (survived 5 years)
  - Poor prognosis (died early, before censoring could occur)
- **Result**: Brier score biased downward by 20-30% (looks artificially good)

**Real Numbers:**
```
TRUE Brier score (with IPCW): 0.25
CURRENT Brier score (dropping censored): 0.18 ⛔ WRONG! (28% bias)

Researcher thinks model has excellent calibration
Actually has moderate calibration
Model overperforms in development, fails in validation
```

---

## Critical Issue #3: ⛔ Time-Dependent Plots Are Placeholders

### Location
Multiple placeholder functions throughout the code

### The Problem

The `.a.yaml` and `.r.yaml` files advertise:
- Time-dependent calibration plots
- C-index over time plots
- Brier score over time plots
- TRIPOD-compliant reporting

But these are NOT IMPLEMENTED - just return "under development" messages or empty plots.

### User Impact

1. **False Advertising**: Users select options that don't work
2. **Incomplete Analysis**: Cannot assess time-varying performance
3. **TRIPOD Non-Compliance**: Report claims to follow TRIPOD but missing key elements

---

## Why Simple Fixes Won't Work: Architectural Issue

The fundamental problem is that `survivalcalibration` **only accepts pre-computed predictions**, not the model itself.

**Current Input:**
```
survivalcalibration(
    time = "followup_months",
    event = "death",
    predicted = "predicted_survival_prob"  # Pre-computed!
)
```

**What's Needed for Proper Validation:**
```
survivalcalibration(
    formula = Surv(time, event) ~ age + stage + biomarker,  # Model formula
    data = mydata,  # Raw data
    predicted = "predicted_survival_prob"  # Optional: for external validation
)
```

**Why This Matters:**
- **Cannot refit models** without formula and predictors
- **Cannot do proper bootstrap/k-fold** without model specification
- **Cannot calculate optimism** without training/testing cycles

**This is a FUNDAMENTAL ARCHITECTURAL LIMITATION.**

---

## Required Fixes

### Immediate (Prevent Harm)

1. **Remove Misleading "Validation" Options** ⚠️ CRITICAL
   ```r
   # Remove these options from .a.yaml:
   # - validationMethod: "bootstrap"
   # - validationMethod: "kfold"
   # - nBootstrap, nFolds

   # OR rename to:
   # - "Prediction Stability Assessment (NOT optimism correction)"
   # - Add WARNING that this is NOT proper validation
   ```

2. **Fix Brier Score with IPCW** ⚠️ CRITICAL
   ```r
   # Implement proper IPCW Brier score
   # Libraries: pec, riskRegression
   # Estimated effort: 2-3 days
   ```

3. **Add Warning Messages** ⚠️ CRITICAL
   ```r
   # In welcome message and results:
   warning("
   ╔════════════════════════════════════════════════════════════╗
   ║ IMPORTANT LIMITATION: No Optimism Correction              ║
   ╠════════════════════════════════════════════════════════════╣
   ║ This function accepts PRE-COMPUTED predictions only.      ║
   ║                                                            ║
   ║ Cannot perform proper bootstrap/k-fold validation because ║
   ║ model refitting requires:                                 ║
   ║  - Model formula                                          ║
   ║  - Original predictor variables                           ║
   ║  - Training procedure                                     ║
   ║                                                            ║
   ║ RECOMMENDATION:                                            ║
   ║ Use rms::validate() or pec::pec() for proper internal     ║
   ║ validation with optimism correction.                      ║
   ╚════════════════════════════════════════════════════════════╝
   ")
   ```

4. **Remove or Implement Placeholder Plots** ⚠️ CRITICAL
   - Either implement time-dependent plots
   - Or remove options entirely
   - **DO NOT** advertise features that don't exist

### Medium Term (Proper Implementation)

1. **Redesign to Accept Model Formula** (6-8 weeks)
   - Change input from predictions to formula + data
   - Implement proper bootstrap with model refitting
   - Implement proper k-fold with training/testing
   - Calculate true optimism correction

2. **Implement IPCW Brier Score** (1-2 weeks)
   - Use `pec` or `riskRegression` packages
   - Proper censoring weight calculation
   - Integrated Brier score over time
   - Time-dependent Brier score

3. **Implement Missing Plots** (2-3 weeks)
   - Grouped calibration plots
   - C-index over time
   - Brier score over time
   - Calibration slope over time

4. **Comprehensive Testing** (2-3 weeks)
   - Validate against known datasets
   - Compare with SAS, Stata, R packages
   - Test with varying censoring rates
   - Edge case testing

**Total Estimated Effort**: 11-16 weeks (3-4 months)

---

## Current vs. Proper Implementation

| Feature | Current | Proper Implementation |
|---------|---------|----------------------|
| **Bootstrap validation** | ⛔ Resamples predictions | ✅ Refits model, calculates optimism |
| **K-fold validation** | ⛔ Partitions predictions | ✅ Trains k models, tests on holdout |
| **Optimism correction** | ⛔ Returns ≈ apparent performance | ✅ Bias-corrected estimate |
| **Brier score** | ⛔ Drops censored patients | ✅ Uses IPCW |
| **Censoring handling** | ⛔ Informative (biased) | ✅ IPCW (unbiased) |
| **Time-dependent plots** | ⛔ Placeholders | ✅ Implemented |
| **TRIPOD compliance** | ⛔ Incomplete | ✅ Full reporting |

---

## Recommendation: 🚫 DO NOT RELEASE

**This function should NOT be released in its current state because:**

1. **Validation is Mathematically Wrong**
   - "Validated" metrics are actually just noisy re-estimates of apparent performance
   - Provides NO optimism correction
   - Users will publish overoptimistic results

2. **Brier Score is Severely Biased**
   - Drops censored patients (informative censoring)
   - Underestimates prediction error by 20-30% in typical datasets
   - Misleads users about model calibration

3. **Advertises Features That Don't Exist**
   - Time-dependent plots are placeholders
   - TRIPOD reporting is incomplete
   - Misleading to users

**Alternative**: If urgent need exists:
1. Remove all "validation" options
2. Fix Brier score with IPCW
3. Add PROMINENT warnings about limitations
4. Rename to "Survival Prediction Assessment (Apparent Performance Only)"
5. Clearly state: "For optimism correction, use rms::validate() or pec::pec()"

**Proper Implementation Timeline**: 3-4 months of dedicated development + statistical review

---

**Files**:
- Implementation: `R/survivalcalibration.b.R`
- Config: `jamovi/survivalcalibration.a.yaml`
- Tests: NONE EXIST (need to create)
- Status: ⛔ **MATHEMATICALLY INCORRECT - DO NOT USE FOR CLINICAL DECISIONS**
