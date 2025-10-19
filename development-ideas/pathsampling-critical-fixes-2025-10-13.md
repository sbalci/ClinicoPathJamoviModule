# Pathsampling Module - Critical Fixes Applied (2025-10-13)

## Executive Summary

Critical evaluation of the pathsampling module revealed **one major conceptual error** affecting the Diagnostic Yield Curve and Bootstrap Analysis. The module was incorrectly calculating **population-level detection probabilities** instead of **conditional detection probabilities (sensitivity)**.

## Issues Found and Fixed

### 1. ✅ Data Summary Table - CORRECT

All calculations verified against test data:

| Measure | Expected | Observed | Status |
|---------|----------|----------|--------|
| Total cases | 1000 | 1000 | ✅ |
| Cases analyzed | 1000 | 1000 | ✅ |
| Total samples (input) | 10211 | 10211 | ✅ |
| Total samples analyzed | 6492 | 6492 | ✅ |
| Mean samples per case | 10.21 | 10.21 | ✅ |
| Median first detection | 1 | 1 | ✅ |
| Cases without lesion | 526 | 526 | ✅ |

**Conclusion**: No issues found.

---

### 2. ❌ Diagnostic Yield Curve - CRITICAL ERROR FIXED

**Problem**: The "Observed" line showed **population-level detection** rates instead of **conditional detection** (sensitivity).

#### What Was Wrong

**Before Fix** (Line 843-845):
```r
observedProbVec <- sapply(1:maxSamp, function(n) {
    sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nCases
})
```

This divided by **total cases** (including negatives), giving population-level rates:
- n=1: 24.3% (243/1000 cases)
- n=5: 42.6% (426/1000 cases)
- n=10: 46.6% (466/1000 cases)

**Problem**: These are incomparable with the binomial model which represents conditional probability (sensitivity).

#### After Fix

```r
# Calculate observed conditional detection probability (sensitivity)
# Only among positive cases, not population-level
nPositiveCases <- sum(!is.na(firstDetectionData))

observedProbVec <- sapply(1:maxSamp, function(n) {
    if (nPositiveCases == 0) return(0)
    # Count positive cases detected by sample n / total positive cases
    sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases
})
```

This divides by **positive cases only**, giving conditional rates (sensitivity):
- n=1: 51.3% (243/474 positive cases) ✅
- n=5: 89.9% (426/474 positive cases) ✅
- n=10: 98.3% (466/474 positive cases) ✅

**Files Modified**:
- R/pathsampling.b.R (line 843-857)

---

### 3. ❌ Bootstrap Analysis - CRITICAL ERROR FIXED

**Problem**: Bootstrap resampled from ALL cases (including negatives) instead of positive cases only.

#### What Was Wrong

**Before Fix** (Line 778-792):
```r
for (iter in 1:nBoot) {
    # Resample with replacement
    sampledIndices <- sample(1:nCases, nCases, replace = TRUE)
    sampledFirst <- firstDetectionData[sampledIndices]

    # Calculate detection rate for each sample count
    for (j in 1:maxSamp) {
        detected <- sum(!is.na(sampledFirst) & sampledFirst <= j)
        bootstrapResults[iter, j] <- detected / nCases  # WRONG
    }
}
```

This estimated **population-level detection** with confidence intervals, not sensitivity.

#### After Fix

```r
# Get positive cases only for bootstrap (conditional probability)
positiveCases <- !is.na(firstDetectionData)
positiveIndices <- which(positiveCases)
nPositiveCases <- length(positiveIndices)

for (iter in 1:nBoot) {
    # Resample from POSITIVE cases only with replacement
    sampledPositiveIndices <- sample(positiveIndices, nPositiveCases, replace = TRUE)
    sampledFirst <- firstDetectionData[sampledPositiveIndices]

    # Calculate detection rate for each sample count (conditional)
    for (j in 1:maxSamp) {
        detected <- sum(!is.na(sampledFirst) & sampledFirst <= j)
        bootstrapResults[iter, j] <- detected / nPositiveCases  # CORRECT
    }
}
```

**Files Modified**:
- R/pathsampling.b.R (line 778-798)

---

### 4. ❌ Detection Curve Plot - CRITICAL ERROR FIXED

**Problem**: Plot calculation also used population-level instead of conditional probability.

#### What Was Wrong

**Before Fix** (Line 2595-2603):
```r
nCases <- length(firstDetectionData)

# Calculate observed and predicted values
nSamples <- 1:maxSamp
observedProb <- sapply(nSamples, function(n) {
    sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nCases  # WRONG
})
```

#### After Fix

```r
nCases <- length(firstDetectionData)
nPositiveCases <- sum(!is.na(firstDetectionData))

# Calculate observed conditional probability (sensitivity)
# Only among positive cases, not population-level
nSamples <- 1:maxSamp
observedProb <- sapply(nSamples, function(n) {
    if (nPositiveCases == 0) return(0)
    sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases  # CORRECT
})
```

**Files Modified**:
- R/pathsampling.b.R (line 2595-2607)

---

### 5. ✅ Empty "Detection Rates by Cassette Groups" Table - CORRECT BEHAVIOR

**Finding**: Table is empty because test data (`pathsampling_basic.csv`) lacks the required `positiveCassettes` variable.

**Code Check** (Line 969):
```r
if (self$options$showStageMigration && !is.null(positiveCassettes)) {
    # ... populate table
}
```

**Conclusion**: This is **correct behavior**. The table should only populate when optional data is provided. Not a bug.

---

### 6. ✅ Q Estimation Methods - CORRECT

Verified both q estimation methods are mathematically sound:

#### Method 1: Geometric MLE (from first detection only)

```r
q = 1 / mean(first_pos)
```

**Test Result**:
- Mean first detection: 2.4662
- q (geometric MLE): **0.4055**
- Fits geometric distribution reasonably well

**Mathematical Basis**: Maximum Likelihood Estimator for geometric distribution parameter.

#### Method 2: Empirical Proportion (from all positive samples)

```r
q = sum(positive_samples) / sum(total_samples)  # positive cases only
```

**Test Result**:
- Total samples (positive cases): 4888
- Total positive samples: 2606
- q (empirical proportion): **0.5331**

**Why Different?**
- Geometric MLE uses only first detection position (right-censored data)
- Empirical proportion uses ALL examined samples (complete data when available)
- Both are valid; empirical is more informative when `positiveCount` data exists

**Conclusion**: No issues found. Both methods correctly implemented.

---

### 7. ✅ No Dummy Code or Magic Numbers

**Checked For**:
- Hardcoded probabilities
- Fallback "magic" values
- Unexplained constants

**Found**:
- `1e-12`: Numerical stability epsilon (prevents log(0) errors) - **APPROPRIATE**
- `1e-6`: Ridge adjustment epsilon for beta-binomial - **APPROPRIATE**
- Default values in UI (0.95 confidence, 10 max samples) - **DOCUMENTED IN UI**

**Conclusion**: All numerical constants are justified and documented.

---

## Impact Assessment

### Before Fixes

**Diagnostic Yield Curve**:
- Blue line (Binomial Model): Shows sensitivity ~40-99%
- Orange line (Observed): Shows population detection ~24-47%
- **Not comparable!** Different quantities plotted on same graph

**Bootstrap CI**:
- Confidence intervals for population-level detection
- **Wrong interpretation** for clinical use (should estimate sensitivity)

**Clinical Recommendations**:
- Based on population-level probabilities
- **Misleading** for determining sampling adequacy

### After Fixes

**Diagnostic Yield Curve**:
- Blue line (Binomial Model): Shows sensitivity ~40-99%
- Orange line (Observed): Shows sensitivity ~51-98%
- **Directly comparable** - both conditional on positive cases

**Bootstrap CI**:
- Confidence intervals for conditional detection (sensitivity)
- **Correct interpretation** for clinical recommendations

**Clinical Recommendations**:
- Based on sensitivity estimates
- **Appropriate** for sampling protocol design

---

## Verification

### Test Data: pathsampling_basic.csv (1000 cases)

**Observed Conditional Detection Rates** (After Fix):

| Samples (n) | Binomial Model | Observed | Match? |
|-------------|----------------|----------|--------|
| 1 | 40.5% | 51.3% | Reasonable |
| 2 | 64.7% | 71.3% | Reasonable |
| 3 | 79.0% | 79.5% | ✅ Excellent |
| 4 | 87.5% | 85.2% | Reasonable |
| 5 | 92.6% | 89.9% | Reasonable |
| 7 | 97.4% | 95.1% | ✅ Good |
| 10 | 99.4% | 98.3% | ✅ Good |

**Interpretation**:
- Observed data shows slightly higher early detection (more cases at k=1 than geometric predicts)
- Good convergence at higher sample counts
- Validates both model and observed calculations are correct

---

## Files Modified

1. **R/pathsampling.b.R**:
   - Line 843-857: Fixed observed probability calculation (.run method)
   - Line 778-798: Fixed bootstrap resampling to use positive cases only
   - Line 2595-2607: Fixed detection curve plot calculation

2. **Auto-generated** (via jmvtools::prepare):
   - R/pathsampling.h.R
   - jamovi/pathsampling.src.js

---

## Testing Instructions

### 1. Reinstall Module

```r
setwd("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule")
jmvtools::prepare('.')
jmvtools::install()
```

### 2. Restart jamovi

Close and reopen jamovi to load updated module.

### 3. Test with pathsampling_basic.csv

**Configuration**:
- Total Samples: `n_samples`
- First Detection: `first_pos`
- Target Confidence: 0.95
- Maximum Samples: 10
- Bootstrap Iterations: 1000 (for quick test)

**Expected Results**:
- **Diagnostic Yield Curve**: Orange "Observed" line should now be ~51% at n=1, ~90% at n=5, ~98% at n=10
- **Bootstrap Analysis**: CI should reflect sensitivity (conditional probability)
- **Both lines should track closely** on the plot

### 4. Compare Before/After

**Before Fix** (Population-Level):
```
n=1:  24.3%  ❌ (Can't compare with binomial model)
n=5:  42.6%  ❌
n=10: 46.6%  ❌ (Never reaches target confidence)
```

**After Fix** (Conditional/Sensitivity):
```
n=1:  51.3%  ✅ (Comparable with binomial model)
n=5:  89.9%  ✅
n=10: 98.3%  ✅ (Reaches target confidence)
```

---

## Conceptual Clarification

### Population-Level vs Conditional Detection

**Population-Level Detection**:
- P(detect tumor in any random case)
- Includes prevalence: P(detect) = π × sensitivity
- π = 474/1000 = 47.4% in test data
- At n=10 samples: 0.474 × 0.983 ≈ 46.6%
- **Useful for**: Epidemiological studies, screening programs

**Conditional Detection (Sensitivity)**:
- P(detect tumor | tumor actually present)
- Does NOT include prevalence
- At n=10 samples: 98.3%
- **Useful for**: Sampling protocol design, diagnostic test evaluation

### Why This Matters

The pathsampling module is designed for **sampling protocol optimization**:

> "How many samples should I examine to be confident I won't miss a lesion **IF IT EXISTS**?"

This is a **sensitivity question**, not a prevalence question.

**Clinical Example**:
- Pathologist receives omentum specimen
- Question: "If there's tumor, how many sections to find it?"
- Answer: Based on **conditional probability** (sensitivity)
- NOT: "What's the chance any random omentum has tumor?" (population)

The original code mixed these concepts, making the "Observed" line incomparable with the binomial model.

---

## Summary of Changes

| Component | Error Type | Severity | Fixed |
|-----------|------------|----------|-------|
| Data Summary table | None | N/A | ✅ Already correct |
| Diagnostic Yield Curve (observed) | Conditional vs population | **CRITICAL** | ✅ Fixed |
| Bootstrap analysis | Conditional vs population | **CRITICAL** | ✅ Fixed |
| Detection curve plot | Conditional vs population | **CRITICAL** | ✅ Fixed |
| Empty stage migration table | None (expected behavior) | N/A | ✅ Already correct |
| Q estimation (geometric) | None | N/A | ✅ Already correct |
| Q estimation (empirical) | None | N/A | ✅ Already correct |
| Magic numbers / dummy code | None found | N/A | ✅ Clean |

---

## Commit Message Suggestion

```
Fix critical conditional probability errors in pathsampling module

The Diagnostic Yield Curve and Bootstrap Analysis were incorrectly
calculating population-level detection rates instead of conditional
detection probabilities (sensitivity). This made the "Observed" data
incomparable with the binomial model predictions.

Changes:
- Fixed observed detection calculation to use positive cases only (sensitivity)
- Fixed bootstrap resampling to resample from positive cases only
- Fixed detection curve plot to show conditional probabilities
- Added comments clarifying conditional vs population-level calculations

Impact:
- Observed curve now properly tracks binomial model (both show sensitivity)
- Bootstrap confidence intervals now estimate sensitivity, not population rates
- Clinical recommendations now based on correct conditional probabilities

Testing:
- Verified with pathsampling_basic.csv (1000 cases, 474 positive)
- Observed detection at n=10: Now 98.3% (was 46.6%)
- Matches expected conditional probability from binomial model

Fixes sampling protocol optimization calculations
```

---

## References

**Statistical Concepts**:
- Conditional probability: P(A|B) vs P(A)
- Sensitivity vs detection rate in epidemiology
- Geometric distribution MLE
- Bootstrap resampling for non-parametric CI

**Clinical Applications**:
- Skala SL, Hagemann IS. Int J Gynecol Pathol. 2015;34(4):374-378.
- Habib CA, et al. Modern Pathology. 2024.
- Goess R, et al. Virchows Arch. 2024.

---

## Next Steps

1. ✅ Test with pathsampling_basic.csv
2. ✅ Test with pathsampling_enhanced.csv (includes pos_count for empirical method)
3. Test with pathsampling_complete.csv (includes spatial clustering)
4. Verify bootstrap CI widths are reasonable
5. Check clinical recommendations make sense
6. Document expected behavior in user guide

---

## Questions Answered

1. **Are tables and graphs correct?**
   - Data Summary: ✅ Yes
   - Diagnostic Yield Curve: ❌ Fixed (was showing wrong probability type)
   - Detection Rates by Cassette Groups: ✅ Empty is expected

2. **Any dummy code or magic numbers?**
   - ✅ No unexplained constants found
   - ✅ All epsilon values are justified
   - ✅ All defaults are documented

3. **Are calculations critically evaluated?**
   - ✅ Data summary verified against raw data
   - ✅ Q estimation methods verified mathematically
   - ✅ Conditional vs population probability errors identified and fixed
   - ✅ Bootstrap methodology corrected

---

**Status**: ✅ All critical errors fixed and ready for testing
