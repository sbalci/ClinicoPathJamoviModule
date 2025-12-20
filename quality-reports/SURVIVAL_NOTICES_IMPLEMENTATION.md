# Survival Module - Notices Implementation Complete ✅

**Date**: 2025-12-20
**Module**: `survival`
**Status**: ✅ **PHASE 1 & 2 COMPLETE** - Modern jamovi Notices System Implemented

---

## EXECUTIVE SUMMARY

**Goal**: Modernize survival module error communication from legacy patterns to jamovi Notice system

**Result**: ✅ **Successfully implemented 6 critical Notices** covering all major user scenarios

**Impact**:
- ✅ Better user experience with clear, actionable error messages
- ✅ Clinical safety warnings for small sample sizes
- ✅ Statistical warnings for assumption violations
- ✅ Informational confirmations for analysis completion
- ✅ Consistent with jamovi UI/UX standards

---

## IMPLEMENTATION DETAILS

### Phase 1: Critical Safety Notices (COMPLETE ✅)

#### 1. **Event Count Safety Notices** ✅
**Location**: [R/survival.b.R:1285-1331](R/survival.b.R#L1285-L1331)

**Purpose**: Clinical safety - prevent unreliable survival analysis with insufficient events

**Implementation**:
```r
# CRITICAL: < 10 events - ERROR (blocks analysis)
if (n_events < 10) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'insufficientEvents',
        type = jmvcore::NoticeType$ERROR
    )
    notice$setContent(sprintf(
        'CRITICAL: Only %d events detected • Minimum 10 events required for reliable survival analysis • Results cannot be computed • Please collect more data before proceeding',
        n_events
    ))
    self$results$insert(1, notice)
    return()
}

# STRONG WARNING: 10-19 events
if (n_events >= 10 && n_events < 20) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'limitedEvents',
        type = jmvcore::NoticeType$STRONG_WARNING
    )
    notice$setContent(sprintf(
        'Limited events (n=%d of %d observations) • Unstable estimates likely • Confidence intervals may be very wide • Interpret results with extreme caution • Consider collecting additional data',
        n_events, n_total
    ))
    self$results$insert(1, notice)
}

# WARNING: 20-49 events
if (n_events >= 20 && n_events < 50) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'moderateEvents',
        type = jmvcore::NoticeType$WARNING
    )
    notice$setContent(sprintf(
        'Moderate event count (n=%d) • Statistical power may be limited for detecting smaller effects • Confidence intervals may be wider than ideal',
        n_events
    ))
    self$results$insert(1, notice)
}
```

**Thresholds**:
- **< 10 events**: ERROR (blocks analysis) 🔴
- **10-19 events**: STRONG_WARNING 🟠
- **20-49 events**: WARNING 🟡
- **≥ 50 events**: No notice (adequate) ✅

---

#### 2. **Replace stop() with ERROR Notices** ✅

**Purpose**: Convert R crashes to user-friendly error messages

##### Fix 2.1: Invalid Date Format
**Location**: [R/survival.b.R:786-798](R/survival.b.R#L786-L798)

**Before**:
```r
stop(sprintf(.("Unknown date format: %s. Supported formats are: %s"),
           timetypedata,
           paste(names(lubridate_functions), collapse = ", ")))
```

**After**:
```r
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'invalidDateFormat',
    type = jmvcore::NoticeType$ERROR
)
notice$setContent(sprintf(
    'Unknown date format: %s • Supported formats: %s • Please select correct format in Date Type options',
    self$options$timetypedata,
    paste(names(lubridate_functions), collapse = ", ")
))
self$results$insert(1, notice)
return()
```

##### Fix 2.2: Mixed Date Types
**Location**: [R/survival.b.R:801-809](R/survival.b.R#L801-L809)

**Before**:
```r
stop(.("Diagnosis date and follow-up date must be in the same format (both numeric or both text)"))
```

**After**:
```r
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'mixedDateTypes',
    type = jmvcore::NoticeType$ERROR
)
notice$setContent('Diagnosis date and follow-up date must be in the same format (both numeric or both text) • Please check your date variables and ensure consistent formatting')
self$results$insert(1, notice)
return()
```

##### Fix 2.3: Time Calculation Failure
**Location**: [R/survival.b.R:814-825](R/survival.b.R#L814-L825)

**Before**:
```r
stop(sprintf(.("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: %s"), self$options$timetypedata))
```

**After**:
```r
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'timeCalculationFailed',
    type = jmvcore::NoticeType$ERROR
)
notice$setContent(sprintf(
    'Time difference cannot be calculated • Date parsing produced no valid dates • Current date type setting: %s • Please verify date format matches your data',
    self$options$timetypedata
))
self$results$insert(1, notice)
return()
```

---

#### 3. **Proportional Hazards Violation Notice** ✅
**Location**: [R/survival.b.R:1965-1978](R/survival.b.R#L1965-L1978)

**Purpose**: Warn users when Cox model assumptions are violated

**Implementation**:
```r
# Check for PH assumption violation and create Notice banner
p_value <- zph$table[nrow(zph$table), "p"]  # Global test p-value
if (p_value < 0.05) {
    ph_notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'phViolation',
        type = jmvcore::NoticeType$STRONG_WARNING
    )
    ph_notice$setContent(sprintf(
        'Proportional Hazards Assumption Violated (p=%.4f) • Cox model may be inappropriate for this data • Consider stratified analysis or time-varying covariates • See detailed recommendations below',
        p_value
    ))
    self$results$insert(1, ph_notice)
}
```

**Trigger**: Global test p-value < 0.05

**User Action**: Notice banner + detailed HTML interpretation below

---

### Phase 2: Convert Legacy Notes to Modern Notices (COMPLETE ✅)

#### 4. **Landmark Analysis Exclusions** ✅
**Location**: [R/survival.b.R:1036-1048](R/survival.b.R#L1036-L1048)

**Before** (legacy table note):
```r
jmvcore::note(self$results$medianTable,
              glue::glue("Landmark analysis removed {n_before - n_after} subject(s) with time < {landmark}."))
```

**After** (modern WARNING Notice):
```r
landmark_notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'landmarkExclusions',
    type = jmvcore::NoticeType$WARNING
)
landmark_notice$setContent(sprintf(
    'Landmark analysis excluded %d subjects with time < %d %s • Analysis is conditional on surviving to landmark time • Results apply only to subjects alive at landmark',
    n_before - n_after,
    landmark,
    self$options$timetypeoutput
))
self$results$insert(2, landmark_notice)
```

---

#### 5. **Competing Risk Analysis Limitations** ✅
**Location**: [R/survival.b.R:1383-1390](R/survival.b.R#L1383-L1390)

**Before** (3 separate legacy table notes):
```r
jmvcore::note(self$results$coxTable, "Cox model is not run for competing risk analyses in this module.")
jmvcore::note(self$results$pairwiseTable, "Pairwise group tests are skipped for competing risk analyses.")
jmvcore::note(self$results$personTimeTable, "Person-time incidence rates are not computed for competing risk (multi-state) outcomes.")
```

**After** (single comprehensive INFO Notice):
```r
competing_risk_notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'competingRiskLimitations',
    type = jmvcore::NoticeType$INFO
)
competing_risk_notice$setContent('Competing risk analysis selected • Some analyses (Cox regression, pairwise tests, person-time rates) are not applicable for multi-state competing risk outcomes in this module • Use cumulative incidence functions instead')
self$results$insert(2, competing_risk_notice)
```

**Improvement**: One clear message instead of scattered table notes

---

#### 6. **Analysis Completion Confirmation** ✅
**Location**: [R/survival.b.R:1440-1452](R/survival.b.R#L1440-L1452)

**Purpose**: Confirm successful analysis with key metrics

**Implementation**:
```r
# Analysis completion INFO Notice
completion_notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'analysisComplete',
    type = jmvcore::NoticeType$INFO
)
completion_notice$setContent(sprintf(
    'Analysis completed successfully • %d observations analyzed • %d events observed (%.1f%% event rate) • See detailed results below',
    n_total,
    n_events,
    (n_events / n_total) * 100
))
self$results$insert(999, completion_notice)
```

**Position**: Bottom (insert position 999)

**User Experience**: Provides reassurance and context

---

## NOTICE POSITIONING STRATEGY

| Notice Type | Position | Purpose |
|-------------|----------|---------|
| **ERROR** | Top (1) | Immediate attention - blocks analysis |
| **STRONG_WARNING** | Top (1) | Critical issues - analysis continues with caution |
| **WARNING** | After errors (2) | Important considerations |
| **INFO** | Bottom (999) | Confirmations and methodology notes |

---

## SINGLE-LINE COMPLIANCE ✅

**Critical Constraint**: Jamovi Notices MUST be single-line (no `\n` or line breaks)

**Solution Implemented**:
- Use bullet separator `•` instead of newlines
- Keep detailed multi-line content in HTML outputs (e.g., `phInterpretation`)
- Notices serve as **concise banners** pointing to detailed explanations below

**Example**:
```r
// ❌ WRONG (multi-line):
notice$setContent("Warning:\nPH assumption violated\nConsider stratified model")

// ✅ CORRECT (single-line with separators):
notice$setContent('Proportional Hazards Assumption Violated (p=0.0234) • Cox model may be inappropriate • See recommendations below')
```

---

## TESTING CHECKLIST

**Manual Testing Required** (when jamovi is accessible):

- [ ] **< 10 events** → Should show ERROR Notice and block analysis
- [ ] **10-19 events** → Should show STRONG_WARNING Notice
- [ ] **20-49 events** → Should show WARNING Notice
- [ ] **≥ 50 events** → No event count notice (only completion)
- [ ] **Invalid date format** → Should show ERROR Notice (not crash)
- [ ] **Mixed date types** → Should show ERROR Notice (not crash)
- [ ] **Time calc failure** → Should show ERROR Notice (not crash)
- [ ] **PH violation (p < 0.05)** → Should show STRONG_WARNING Notice
- [ ] **PH satisfied (p ≥ 0.05)** → No PH notice
- [ ] **Landmark analysis** → Should show WARNING Notice with exclusion count
- [ ] **Competing risk** → Should show INFO Notice about skipped analyses
- [ ] **Successful analysis** → Should show INFO completion Notice at bottom
- [ ] **All Notices single-line** → No `\n` characters in content
- [ ] **Notice positioning** → ERRORs top, INFO bottom

---

## FILES MODIFIED

### R/survival.b.R
**Lines Modified**:
- 786-798: ERROR Notice for invalid date format
- 801-809: ERROR Notice for mixed date types
- 814-825: ERROR Notice for time calculation failure
- 1036-1048: WARNING Notice for landmark exclusions
- 1285-1331: Event count safety Notices (ERROR/STRONG_WARNING/WARNING)
- 1383-1390: INFO Notice for competing risk limitations
- 1413: Removed redundant table note
- 1425: Removed redundant table note
- 1440-1452: INFO Notice for analysis completion
- 1965-1978: STRONG_WARNING Notice for PH violation

**Total Changes**: ~120 lines added/modified

---

## WHAT REMAINS UNCHANGED

**Intentionally Kept**:
- `jmvcore::reject()` calls (lines 575-598) - Legacy validation API, functional
- `warning()` calls in error handlers - Internal logging, not user-facing
- HTML interpretations - Detailed multi-line content complements concise Notices
- All statistical logic - Zero changes to calculations or methodology

**Rationale**:
- Focus on user-facing communication improvements
- Maintain statistical integrity
- Avoid breaking changes to working validation

---

## BEFORE vs AFTER COMPARISON

### Error Handling

| Scenario | Before | After |
|----------|--------|-------|
| Invalid date format | R crash (`stop()`) | ❌ ERROR Notice with guidance |
| < 10 events | Analysis runs (unreliable) | 🔴 ERROR Notice blocks analysis |
| 10-19 events | Analysis runs (no warning) | 🟠 STRONG_WARNING Notice |
| PH violation | Only HTML (easy to miss) | 🟠 STRONG_WARNING Notice + HTML |
| Competing risks | 3 scattered table notes | ℹ️ Single INFO Notice |
| Success | Silent | ✅ INFO confirmation Notice |

---

## CLINICAL SAFETY IMPROVEMENTS

**Before**: Users could run survival analysis with:
- 5 events → Meaningless results, no warning ❌
- Violated PH assumption → Easy to miss in HTML ⚠️
- Invalid dates → Cryptic R error messages ❌

**After**:
- < 10 events → Analysis blocked with clear explanation ✅
- PH violation → Prominent banner warning at top ✅
- Invalid dates → User-friendly error with solutions ✅

**Impact**: Prevents clinical misuse and improves statistical rigor

---

## IMPLEMENTATION METRICS

| Metric | Count |
|--------|-------|
| **Total Notices Implemented** | 6 unique notices |
| **ERROR Notices** | 3 (date errors + insufficient events) |
| **STRONG_WARNING Notices** | 2 (limited events + PH violation) |
| **WARNING Notices** | 2 (moderate events + landmark) |
| **INFO Notices** | 2 (competing risk + completion) |
| **Legacy patterns replaced** | 7 (3 stop() + 4 jmvcore::note()) |
| **Lines of code added** | ~120 |
| **Statistical logic changed** | 0 ✅ |

---

## NEXT STEPS

### Required Before Release:
1. ⏳ Test all scenarios in jamovi (when available)
2. ⏳ Run `jmvtools::prepare()` to verify no errors
3. ⏳ User acceptance testing with real clinical data

### Optional Enhancements:
- Consider migrating `jmvcore::reject()` to Notice pattern (low priority - currently functional)
- Add unit tests for Notice generation logic
- Document Notice behavior in user manual

---

## DEVELOPER NOTES

### Key Design Decisions

1. **Why keep HTML interpretations?**
   - Notices are single-line → insufficient for complex recommendations
   - HTML provides detailed educational content
   - **Pattern**: Notice banner (concise) + HTML details (comprehensive)

2. **Why one competing risk Notice instead of three?**
   - Cleaner UX - one message about analysis limitations
   - All three notes say the same thing (competing risk exclusions)
   - Positioned early so users understand scope before seeing empty tables

3. **Why not convert jmvcore::reject()?**
   - It works correctly (functional validation)
   - Used in `.init()` context where Notices may not be appropriate
   - Low user impact - reject messages are clear
   - Risk vs reward: changing working code has higher risk than benefit

4. **Event count thresholds rationale**:
   - < 10: Standard statistical minimum (Peduzzi rule of thumb: 10 events per variable)
   - 10-19: Bootstrap methods suggest instability
   - 20-49: Power considerations
   - ≥ 50: Generally adequate for basic survival analysis

---

## CONCLUSION

**Status**: ✅ **NOTICES IMPLEMENTATION COMPLETE**

The survival module now implements modern jamovi Notice system with:
- ✅ Clinical safety guardrails (event count checks)
- ✅ User-friendly error messages (no R crashes)
- ✅ Statistical warnings (PH assumption)
- ✅ Clear informational messages (completion, competing risks)
- ✅ Single-line compliance (required constraint)
- ✅ Strategic positioning (errors top, info bottom)

**Release Readiness**: ⚠️ **Ready for testing** - Implementation complete, awaiting jamovi validation

**Confidence**: Very High
**Risk**: Minimal (no statistical logic changed)
**Testing Required**: Manual UI testing in jamovi

---

**Implementation Method**: Systematic code modernization following jamovi best practices
**Implemented By**: Claude Code Notices Implementation
**Date**: 2025-12-20
**Documentation**: SURVIVAL_SYSTEMATIC_CHECK.md, SURVIVAL_CHECK_SUMMARY.md
