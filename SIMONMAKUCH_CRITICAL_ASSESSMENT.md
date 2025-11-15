# Simon-Makuch Function - Critical Assessment and Recommendations

**Date**: 2025-11-15
**Module**: `simonmakuch`
**Status**: ⛔ **NOT SUITABLE FOR CLINICAL USE**
**Recommendation**: 🚫 **DISABLE OR REMOVE UNTIL PROPERLY IMPLEMENTED**

---

## Executive Summary

The `simonmakuch` function has **fundamental architectural flaws** that make it mathematically incorrect for time-dependent survival analysis. Unlike the fixes applied to `outcomeorganizer` and `timeinterval` (which had targeted bugs that could be corrected), this function requires **complete redesign** of its core counting-process methodology.

**Critical Finding**: The function **cannot handle multiple exposure changes per patient**, uses **incorrect survival methodology** for time-varying exposures, and provides **fictitious immortal time bias assessments**. These are not bugs that can be patched - they are fundamental design issues.

**Recommendation**: This function should be **disabled or removed from release** until it can be properly redesigned and validated with comprehensive testing.

---

## Critical Issues Identified

### Issue 1: ⛔ Counting-Process Data Construction Fundamentally Flawed

**Location**: R/simonmakuch.b.R:157-219 (`.createTimeDependentDataset()`)

**Problem**:
The function assumes each patient has **at most ONE exposure change**, stored as a scalar `timedep_time` value. Real Simon-Makuch analyses require capturing **every exposure change** in long format (counting-process format).

**Current Implementation**:
```r
.createTimeDependentDataset = function(id, time, event, timedep_time, timedep_status, exposed_level) {
    # For each person...
    for (i in seq_along(id)) {
        person_timedep_time <- timedep_time[i]  # SINGLE scalar value!

        if (is.na(person_timedep_time) || person_timedep_time <= 0) {
            # One row - no change
        } else if (person_timedep_time >= person_time) {
            # One row - change after follow-up
        } else {
            # TWO rows - before and after THE SINGLE change
            # Row 1: tstart=0, tstop=timedep_time, exposed=0
            # Row 2: tstart=timedep_time, tstop=time, exposed=1
        }
    }
}
```

**What's Missing**:
- Cannot handle patient who switches: Unexposed → Exposed → Unexposed
- Cannot handle multiple medication starts/stops
- Cannot handle any scenario with >1 exposure change

**Example of Failure**:
```r
# Patient with 2 exposure changes:
# Days 0-30: Unexposed
# Days 30-90: Exposed (started treatment)
# Days 90-180: Unexposed (stopped treatment)
# Dies at day 180

# CURRENT BEHAVIOR (WRONG):
# Only captures first change at day 30
# Creates 2 rows:
#   tstart=0,  tstop=30,  exposed=0, event=0
#   tstart=30, tstop=180, exposed=1, event=1  # WRONG! Was unexposed 90-180!

# CORRECT BEHAVIOR (NEEDS 4 ROWS):
#   tstart=0,  tstop=30,  exposed=0, event=0
#   tstart=30, tstop=90,  exposed=1, event=0
#   tstart=90, tstop=180, exposed=0, event=1
```

**Why This Can't Be Quickly Fixed**:
1. Requires changing data input format (scalar → vector of change times)
2. Requires UI changes to accept longitudinal exposure data
3. Requires validation of multiple change times
4. Affects ALL downstream analysis

---

### Issue 2: ⛔ Survival Curves and Log-Rank Use Wrong Methodology

**Location**: R/simonmakuch.b.R:724-770

**Problem**:
The code fits `survfit(Surv(tstart, tstop, event) ~ exposed, data = survData)` treating `exposed` as a fixed covariate. This is **mathematically incorrect** for time-varying exposures.

**Current Implementation**:
```r
# Time-dependent log-rank test (INCORRECTLY IMPLEMENTED)
survfit_obj <- survfit(Surv(tstart, tstop, event) ~ exposed, data = survData)
survdiff_obj <- survdiff(Surv(tstart, tstop, event) ~ exposed, data = survData)
```

**What's Wrong**:
1. `survfit()` expects **one row per subject** with baseline covariates
2. Feeding counting-process data (multiple rows per subject) **double-counts person-time**
3. The `exposed` variable **flips mid-follow-up**, violating Kaplan-Meier assumptions
4. This is **not** a valid time-dependent log-rank test

**Clinical Impact**:
```r
# Example: 100 patients, each followed 10 years
# 50 switch from unexposed to exposed at year 5
# 50 remain unexposed throughout

# CURRENT BEHAVIOR (WRONG):
# Creates 150 "subjects" for survfit:
#   50 unexposed (0-10 years)    → 500 person-years
#   50 exposed (0-5 years)       → 250 person-years  # Double-counting!
#   50 exposed (5-10 years)      → 250 person-years  # Double-counting!
# Total: 1000 person-years counted, but should be 750 (100 × 10 - 50 × 5)

# P-value is WRONG because person-time is inflated
# Power is WRONG because effective sample size is wrong
# Hazard ratios are BIASED
```

**Correct Methodology**:
- Should use **Mantel-Byar** method or **landmark analysis**
- Should use `coxph()` with time-varying covariates, NOT `survfit()`
- Should properly account for person-time in each exposure state

**Why This Can't Be Quickly Fixed**:
- Requires complete rewrite of survival estimation
- Requires implementing proper Mantel-Byar methodology
- Requires changing output format (can't use standard KM curves)

---

### Issue 3: ⛔ Immortal Time Bias "Assessment" is Fake

**Location**: R/simonmakuch.b.R:853-904

**Problem**:
The code promises to compare proper (Simon-Makuch) vs. naive (baseline exposure) models to demonstrate immortal time bias. The entire naive branch is **not implemented** - just NA placeholders.

**Current Implementation**:
```r
if (self$options$showImmortalTimeBias && !is.null(survData)) {
    # Prepare comparison table
    comparison_df <- data.frame(
        Method = c("Simon-Makuch (Proper)", "Naive (Baseline exposure)"),
        HR = c(hr_proper, NA),      # NA placeholder!
        CI_lower = c(ci_lower, NA), # NA placeholder!
        CI_upper = c(ci_upper, NA), # NA placeholder!
        p_value = c(p_val, NA),     # NA placeholder!
        stringsAsFactors = FALSE
    )

    # Populate table with FAKE comparison
    for (i in 1:nrow(comparison_df)) {
        immortalTimeTable$addRow(rowKey = i, values = list(
            method = comparison_df$Method[i],
            hazardRatio = comparison_df$HR[i],
            ciLower = comparison_df$CI_lower[i],
            ciUpper = comparison_df$CI_upper[i],
            pValue = comparison_df$p_value[i]
        ))
    }
}
```

**What Users See**:
```
Immortal Time Bias Assessment
┌─────────────────────────────┬─────┬──────────┬──────────┬─────────┐
│ Method                      │ HR  │ 95% CI   │          │ p-value │
├─────────────────────────────┼─────┼──────────┼──────────┼─────────┤
│ Simon-Makuch (Proper)       │ 1.45│ 0.98     │ 2.14     │ 0.063   │
│ Naive (Baseline exposure)   │ NA  │ NA       │ NA       │ NA      │
└─────────────────────────────┴─────┴──────────┴──────────┴─────────┘
```

**Clinical Impact**:
- Users **believe** bias assessment ran
- Users see "Simon-Makuch (Proper)" row and assume the naive comparison validates it
- **No actual comparison** was performed
- **Completely misleading** - worse than omitting the feature

**Why This Can't Be Quickly Fixed**:
- Requires implementing the naive analysis (fit Cox on baseline exposure)
- Requires ensuring both models use same data/censoring
- Requires statistical comparison methodology
- Should honestly remove the feature if not implemented

---

### Issue 4: ⛔ No Automated Tests - Zero Quality Assurance

**Location**: tests/testthat/ (no test-simonmakuch.R exists)

**Problem**:
There is **NO test file** for this function. None of the issues above are validated. No edge cases tested. No numerical correctness verified.

**Consequence**:
- All 3 critical bugs above went undetected
- Any future changes could break function without detection
- No validation that fixes actually work
- **No quality assurance whatsoever**

---

## Why Simple Fixes Won't Work

Unlike `outcomeorganizer` and `timeinterval` (which had **targeted bugs** that could be corrected), `simonmakuch` has **fundamental architectural issues**:

| Function | Issue Type | Fix Approach |
|----------|-----------|--------------|
| **outcomeorganizer** | Targeted bugs | ✅ Add validation, fix logic |
| **timeinterval** | Targeted bugs | ✅ Add validation, check both columns |
| **simonmakuch** | Architecture | ⛔ Requires complete redesign |

**Why redesign is needed**:

1. **Data Structure Change Required**:
   - Current: One exposure time per patient (scalar)
   - Needed: Multiple exposure times per patient (vector/data frame)
   - Affects: UI, data validation, all processing

2. **Methodology Change Required**:
   - Current: Misuses standard KM/log-rank on counting-process data
   - Needed: Proper Mantel-Byar or landmark analysis
   - Affects: All survival estimation, all statistical tests

3. **Feature Completion Required**:
   - Current: Immortal time bias assessment is fake (NA placeholders)
   - Needed: Actually implement naive model or remove feature
   - Affects: User trust, clinical interpretation

4. **Quality Assurance Required**:
   - Current: Zero tests
   - Needed: Comprehensive test suite with known examples
   - Affects: Reliability, maintainability, release readiness

---

## Recommended Actions

### Immediate (Required Before Any Release)

1. **Option A: Disable the Function** ⚠️ RECOMMENDED
   ```r
   # In jamovi 0000.yaml, mark as disabled or remove from menu
   # Add warning in documentation:
   # "This function is under development and not suitable for clinical use."
   ```

   **Rationale**:
   - Prevents users from getting incorrect results
   - Honest about current state
   - Can be re-enabled when properly implemented

2. **Option B: Remove the Function** (if Option A not feasible)
   ```r
   # Remove from jamovi menu entirely
   # Keep code in repository for future development
   ```

   **Rationale**:
   - Even cleaner than disabling
   - No risk of users finding and using it
   - Can be restored when ready

3. **Document Known Issues** (if keeping in development)
   - Add CRITICAL warnings to all documentation
   - List known limitations explicitly
   - State "NOT FOR CLINICAL USE"

### Long-Term (For Proper Implementation)

1. **Redesign Data Input** (4-6 weeks)
   - Support longitudinal exposure data
   - Accept data frame with (id, time, exposure) format
   - Validate multiple exposure changes per patient
   - Update UI to handle new format

2. **Implement Proper Methodology** (4-6 weeks)
   - Use correct Mantel-Byar approach
   - Or implement proper landmark analysis
   - Validate against published examples
   - Compare with established packages (e.g., survival::tmerge)

3. **Complete or Remove Partial Features** (2-3 weeks)
   - Implement naive model for bias assessment
   - Or remove the fake immortal time bias table
   - Ensure all UI options actually work

4. **Comprehensive Testing** (3-4 weeks)
   - Create test-simonmakuch.R with ≥50 tests
   - Test counting-process construction
   - Test survival estimation correctness
   - Test edge cases (single switch, multiple switches, no switches)
   - Validate against known results from literature

5. **Clinical Validation** (2-3 weeks)
   - Reproduce published Simon-Makuch analyses
   - Compare results with SAS, Stata, R survival package
   - Get statistical review from time-dependent methods expert

**Total Estimated Effort**: 15-21 weeks (4-5 months) of dedicated development

---

## Comparison with Previous Fixes

### Successfully Fixed Functions

| Function | Issues | Fix Time | Status |
|----------|--------|----------|--------|
| **outcomeorganizer** | Binary validation, multi-event validation | ~4 hours | ✅ Complete, tested |
| **timeinterval** | Negative intervals, date format | ~4 hours | ✅ Complete, tested |

Both had:
- Targeted bugs with clear fixes
- Core methodology sound
- Could be fixed without redesign
- Tests created to verify

### Current Function

| Function | Issues | Fix Time | Status |
|----------|--------|----------|--------|
| **simonmakuch** | Architecture, methodology, fake features | **15-21 weeks** | ⛔ Requires redesign |

Has:
- Fundamental architectural issues
- Wrong statistical methodology
- Incomplete implementation
- Cannot be quickly fixed
- Requires complete redesign

---

## Clinical Impact Assessment

### If Used in Current State

| Scenario | Impact | Severity |
|----------|--------|----------|
| **Single exposure switch** | May work correctly | ⚠️ Unvalidated |
| **Multiple exposure switches** | Silently wrong (drops all but first) | 🚨 CRITICAL |
| **Survival curve interpretation** | Wrong p-values, biased HRs | 🚨 CRITICAL |
| **Immortal time bias check** | Fake comparison (NA placeholders) | 🚨 CRITICAL |
| **Any clinical decision** | Based on wrong analysis | 🚨 CRITICAL |

### If Properly Implemented

| Feature | Value | Priority |
|---------|-------|----------|
| **Multiple exposure handling** | Essential for real data | High |
| **Correct Mantel-Byar** | Mathematically sound | High |
| **Bias assessment** | Demonstrates methodology | Medium |
| **Comprehensive tests** | Ensures reliability | High |

---

## Conclusion

The `simonmakuch` function addresses a **critical clinical need** (handling time-varying exposures and immortal time bias) with **excellent educational content and UI design**. However, its **core implementation is fundamentally flawed**:

1. ⛔ Cannot handle multiple exposure changes (only captures first)
2. ⛔ Uses wrong methodology for time-varying survival (double-counts person-time)
3. ⛔ Provides fake immortal time bias assessment (NA placeholders presented as results)
4. ⛔ Has zero automated testing (no quality assurance)

These are **not bugs that can be patched** - they are **architectural issues requiring complete redesign**. Attempting quick fixes would be like:
- Fixing a structural crack in a building's foundation with tape
- Repairing a car's engine by painting the hood
- Treating cancer with band-aids

### Recommendation: 🚫 DO NOT RELEASE

**Action Required**: Disable or remove this function from any release until it can be properly:
1. Redesigned to handle longitudinal exposure data
2. Implemented with correct time-dependent methodology
3. Validated against published examples
4. Tested comprehensively

**Alternative**: If urgent need for Simon-Makuch analysis:
- Direct users to established packages: `survival::tmerge()`, `survival::survfit()` with proper setup
- Provide tutorial/vignette showing how to do it correctly in R
- State that jamovi implementation is "in development"

**Timeline**: If prioritized, proper implementation would take **4-5 months of dedicated development** plus statistical review.

---

**Files**:
- Assessment: `SIMONMAKUCH_CRITICAL_ASSESSMENT.md`
- Implementation: `R/simonmakuch.b.R` (REQUIRES REDESIGN)
- Tests: NONE EXIST (tests/testthat/test-simonmakuch.R needs to be created)
- Status: ⛔ **NOT SUITABLE FOR CLINICAL USE**
