# Survival Continuous - Critical Fixes Implementation Plan

**Date**: 2025-11-15
**Module**: `survivalcont`
**File Size**: 3,708 lines
**Status**: 🔄 PLANNING PHASE

---

## Issues Identified (Same as singlearm)

### 1. ❌ CRITICAL: Competing/Cause-Specific Claims Inaccurate
**Location**: R/survivalcont.b.R:777-816, 1427-1570
**Problem**: Multievent options recode to 0/1/2, but downstream uses `survival::Surv()` which treats any >0 as event
**Impact**: "Competing risk" and "cause-specific" modes collapse to overall-death analysis
**Severity**: CRITICAL - clinically misleading

### 2. ❌ CRITICAL: Person-Time Event Miscounting
**Location**: R/survivalcont.b.R:1803-1820, 1850-1892
**Problem**: Uses `sum(mydata[[myoutcome]])` instead of counting events
**Impact**: When dooc=2, inflates overall rate or gets dropped in intervals
**Severity**: CRITICAL - numerically wrong incidence rates

### 3. ❌ CRITICAL: Invalid RMST Variance Estimator
**Location**: R/survivalcont.b.R:2948-2984
**Problem**: Ad-hoc SE = `sqrt(sum(km_fit$std.err^2)) * tau / max(time)`
**Impact**: RMST confidence intervals have no statistical backing
**Severity**: CRITICAL - invalid inference

### 4. ⚠️ MAJOR: Exploratory Cut-Points Presented as Definitive
**Locations**:
- Quantile splits: R/survivalcont.b.R:2367-2378
- Recursive: R/survivalcont.b.R:2382-2415
- Min p-value: R/survivalcont.b.R:2474-2514

**Problems**:
- Hard-coded percentiles regardless of sample size
- Recursive deletes data with arbitrary 0.1·SD buffer
- Min p-value has no multiple-testing correction or seeding
- No resampling/adjustments

**Impact**: False discoveries, overfitting, unreproducible results
**Severity**: MAJOR - methodologically questionable

### 5. ⚠️ MAJOR: Time Unit Handling Inconsistent
**Location**: R/survivalcont.b.R:1762-1769, 1907-1919
**Problem**: Hardcoded "months" in warnings/narratives even when user sets days/years
**Impact**: Wrong units in clinical narratives
**Severity**: MAJOR - user confusion

### 6. ⚠️ MAJOR: No Statistical Validation in Tests
**Location**: tests/testthat/test-survivalcont.R:36-140+
**Problem**: Only `expect_no_error()`, no numerical verification
**Impact**: No regression protection for statistical outputs
**Severity**: MAJOR - no quality assurance

---

## Fix Strategy (Following singlearm Pattern)

### Phase 1: Core Fixes (CRITICAL)

#### 1.1 Implement Proper Competing Risk Analysis
- Add `.isCompetingRisk()` helper function
- Add `.competingRiskCumInc()` using `cmprsk::cuminc()`
- Branch survival calculations based on analysis type
- Update all downstream functions (cutoff, cox, RMST)

#### 1.2 Fix Event Counting Everywhere
Replace all instances of:
```r
# WRONG:
total_events <- sum(mydata[[myoutcome]])

# CORRECT:
total_events <- sum(mydata[[myoutcome]] >= 1, na.rm = TRUE)
```

Locations to fix:
- Person-time overall count (line ~1807)
- Person-time interval counts (lines ~1850-1892)
- Any data quality assessments
- Any summary statistics

#### 1.3 Fix or Replace RMST Implementation

**Option A** (Preferred): Use `survRM2::rmst2()`
```r
# Use validated package
library(survRM2)
rmst_result <- rmst2(
  time = mydata[[mytime]],
  status = mydata[[myoutcome]],
  arm = mydata[[group]],  # or NULL for single-arm
  tau = tau
)
```

**Option B**: Fix variance calculation
- Implement proper Greenwood formula
- Add citations to methodology
- Include methodological warning

#### 1.4 Add Time-Unit Awareness
- Create `.getDefaultCutpoints()` helper (like singlearm)
- Replace all hardcoded "months" with `self$options$timetypeoutput`
- Make follow-up warnings unit-aware

### Phase 2: Methodological Warnings (MAJOR)

#### 2.1 Cut-Point Method Warnings
Add prominent warnings for each exploratory method:

**Quantile Method**:
```
⚠️ Methodological Note: Quantile-based cut-points use fixed percentiles
and may not be optimal for your data. Consider clinical thresholds or
data-driven methods with cross-validation.
```

**Recursive Method**:
```
⚠️ Warning: Recursive cut-point selection removes observations near
previous splits (0.1·SD buffer) which can reduce sample size and may
not be appropriate for all datasets. Validate results with independent data.
```

**Min P-Value Method**:
```
⚠️ CRITICAL WARNING: Minimum p-value selection has NO multiple-testing
correction and is highly prone to overfitting. Results are exploratory only
and MUST be validated in independent cohorts before clinical use.

This method:
- Tests multiple cut-points without adjustment
- Maximizes Type I error (false positives)
- Should NOT be used for confirmatory analysis
- Requires external validation

Consider instead: Use clinically meaningful thresholds or pre-specified
percentiles with proper statistical adjustment.
```

#### 2.2 Competing Risk Information Message
Replace error-prone implementation with proper method + informative message:
```
✅ This analysis uses proper competing risk methods (cumulative incidence functions)
via the cmprsk package. Results account for competing events and provide correct
cause-specific probabilities.
```

### Phase 3: Comprehensive Testing

#### 3.1 Create test-survivalcont-critical-fixes.R

Test categories:
1. **Event Counting** (10 tests)
   - Binary events
   - Competing risk events (0/1/2)
   - Person-time overall
   - Person-time intervals
   - Consistency checks

2. **Competing Risk Analysis** (8 tests)
   - CIF calculation
   - Median from CIF
   - Event-specific counts
   - Comparison with `cmprsk` direct usage

3. **RMST Calculation** (6 tests)
   - Point estimates
   - Confidence intervals
   - Comparison with `survRM2` (if using that package)
   - Edge cases (all events, no events)

4. **Cut-Point Methods** (6 tests)
   - Quantile method reproducibility
   - Recursive method behavior
   - Min p-value stability
   - Warning generation

5. **Time Units** (4 tests)
   - Days/weeks/months/years handling
   - Narrative correctness
   - Cut-point adjustment

6. **Integration Tests** (4 tests)
   - End-to-end workflows
   - Multi-feature combinations

**Target**: 35-40 tests covering all critical fixes

---

## Implementation Order

### Step 1: Apply singlearm fixes (3 hours)
- Copy helper functions from singlearm.b.R
- `.isCompetingRisk()`
- `.getDefaultCutpoints()`
- `.competingRiskCumInc()`

### Step 2: Fix event counting (2 hours)
- Search and replace `sum(mydata[[myoutcome]])` patterns
- Update person-time module
- Update data quality module
- Test consistency

### Step 3: Address RMST (2 hours)
**Decision needed**: Use survRM2 or fix variance?
- If survRM2: Add dependency, implement wrapper
- If fix: Implement Greenwood formula, add tests

### Step 4: Add cut-point warnings (1 hour)
- Add warning HTML to each method
- Update UI descriptions if needed
- Document limitations

### Step 5: Fix time units (1 hour)
- Replace hardcoded "months"
- Make cut-points unit-aware
- Update narratives

### Step 6: Comprehensive tests (3 hours)
- Create test file
- Write 35-40 tests
- Validate all fixes

### Step 7: Validation (1 hour)
- jmvtools::prepare()
- jmvtools::check()
- Run all tests
- Document results

**Total Estimated Time**: 13 hours

---

## Questions for User

### 1. RMST Implementation
**Question**: Should we:
- **A)** Add `survRM2` dependency and use their validated implementation? (Recommended)
- **B)** Fix the current variance calculation with proper Greenwood formula?
- **C)** Remove RMST feature entirely and recommend external tools?

**Recommendation**: Option A - use survRM2 for statistical rigor

### 2. Cut-Point Methods
**Question**: Should we:
- **A)** Keep all methods but add strong warnings?
- **B)** Keep only quantile method, remove recursive/min-p?
- **C)** Add cross-validation/bootstrap corrections?

**Recommendation**: Option A initially, then consider B for v2.0

### 3. Priority
**Question**: Which fixes are most urgent?
- **Priority 1**: Competing risk + event counting (clinical safety)
- **Priority 2**: RMST + time units (statistical validity)
- **Priority 3**: Cut-point warnings + tests (transparency)

---

## Files to Modify

### Primary
- `R/survivalcont.b.R` - Main implementation
- `tests/testthat/test-survivalcont.R` - Existing tests (update)
- `tests/testthat/test-survivalcont-critical-fixes.R` - New comprehensive tests

### Secondary
- `DESCRIPTION` - Add survRM2 if needed
- `jamovi/survivalcont.a.yaml` - Update descriptions if needed
- `jamovi/survivalcont.r.yaml` - Check result structure

### Documentation
- `SURVIVALCONT_FIXES_SUMMARY.md` - Comprehensive fix documentation
- `SURVIVALCONT_COMPETING_RISK.md` - Competing risk specific docs
- Update `NEWS.md` with version notes

---

## Success Criteria

### Must Have (Before Release)
- [ ] ✅ Competing risk uses `cmprsk::cuminc()`
- [ ] ✅ Event counting uses `>= 1` everywhere
- [ ] ✅ RMST has valid variance (survRM2 or proper Greenwood)
- [ ] ✅ Time units dynamically handled
- [ ] ✅ 35+ tests passing
- [ ] ✅ jmvtools::prepare() succeeds
- [ ] ✅ jmvtools::check() succeeds

### Should Have
- [ ] ⚠️ Cut-point methods have clear warnings
- [ ] ⚠️ Exploratory analyses clearly marked
- [ ] ⚠️ Documentation explains limitations

### Nice to Have
- [ ] 📖 Vignettes explaining methods
- [ ] 📖 Comparison with R packages
- [ ] 📖 Clinical use case examples

---

## Risks and Mitigation

### Risk 1: survRM2 Dependency
**Impact**: Adds external dependency
**Mitigation**: survRM2 is well-maintained, on CRAN, widely used
**Fallback**: Implement proper Greenwood variance ourselves

### Risk 2: Breaking Changes
**Impact**: Existing analyses may produce different results
**Mitigation**:
- Increment version number
- Document changes in NEWS.md
- Provide migration guide

### Risk 3: Time Constraints
**Impact**: 13 hours is substantial
**Mitigation**:
- Phase implementation (Priority 1 first)
- Parallel work possible (tests while coding)
- Can split across sessions

---

## Next Steps

**Awaiting User Decision**:
1. Approve overall approach?
2. RMST strategy preference?
3. Cut-point methods - keep/remove/warn?
4. Priority order confirmation?

**Once approved, I will**:
1. Start with Priority 1 fixes (competing risk + event counting)
2. Create backup
3. Implement fixes systematically
4. Test incrementally
5. Document thoroughly

---

## Comparison with singlearm

| Aspect | singlearm | survivalcont |
|--------|-----------|--------------|
| File size | 2,476 lines | 3,708 lines (+50%) |
| Competing risk issue | ✅ FIXED | 🔄 TO FIX |
| Event counting | ✅ FIXED | 🔄 TO FIX |
| Time units | ✅ FIXED | 🔄 TO FIX |
| Tests created | 39 tests | 35-40 planned |
| Additional issues | Hazard warnings | RMST, cut-points |
| Complexity | Medium | High |
| Est. time | 6 hours | 13 hours |

---

**Status**: 📋 PLAN COMPLETE - AWAITING USER APPROVAL TO PROCEED

