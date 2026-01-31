# rpasurvival Dynamic Visibility Implementation

**Date:** 2026-01-31
**Module:** rpasurvival (Recursive Partitioning Analysis for Survival)
**Feature:** Dynamic instructions visibility control using `setVisible()` in .b.R

---

## Overview

Implemented dynamic visibility control for the instructions panel using the `setVisible()` method in the .b.R backend, following jamovi best practices as demonstrated in `jrecode.b.R`, `jiwillsurvive.b.R`, and other reference implementations.

---

## Implementation Pattern

### Two-Part Approach

1. **Static Definition (.r.yaml)**
   - Set default `visible: true` state
   - Allows for initial display on module load

2. **Dynamic Control (.b.R)**
   - Use `self$results$instructions$setVisible(TRUE/FALSE)` to control visibility based on runtime conditions
   - Show instructions when inputs are missing or errors occur
   - Hide instructions when analysis successfully proceeds

---

## Files Modified

### 1. jamovi/rpasurvival.r.yaml

**Change:** Set instructions to be visible by default

```yaml
- name: instructions
  title: Instructions
  type: Html
  visible: true  # Changed from conditional expression
```

**Rationale:**
- Default visible state allows instructions to display on initial load
- Backend `.b.R` takes over control during runtime based on analysis state

---

### 2. R/rpasurvival.b.R

**Changes:** Added `setVisible()` calls at strategic points in `.run()` method

#### A. Show Instructions - Missing Required Inputs (Line ~103)

```r
.run = function() {
    # Check for required inputs
    if (is.null(self$options$time) || is.null(self$options$event) ||
        length(self$options$predictors) == 0) {
        # Show instructions when inputs are missing
        self$results$instructions$setVisible(TRUE)
        private$.addNotice("INFO", "Awaiting Input",
            "Please select Survival Time, Event Status, and at least one Predictor Variable.")
        private$.renderNotices()
        return()
    }

    # Hide instructions when analysis starts
    self$results$instructions$setVisible(FALSE)

    # ... analysis proceeds
}
```

#### B. Show Instructions - Invalid Time Values (Line ~127)

```r
# Check for negative times
if (any(timeVar < 0, na.rm = TRUE)) {
    self$results$instructions$setVisible(TRUE)
    private$.addNotice("ERROR", "Invalid Time Values",
        "Survival time must be non-negative. Check your data.")
    private$.renderNotices()
    return()
}
```

#### C. Show Instructions - Insufficient Events (Line ~143)

```r
if (nEvents < 10) {
    self$results$instructions$setVisible(TRUE)
    private$.addNotice("ERROR", "Insufficient Events",
        paste0("Only ", nEvents, " events observed. Need at least 10 events for RPA."))
    private$.renderNotices()
    return()
}
```

#### D. Show Instructions - Severe Overfit Risk (Line ~167)

```r
if (nPredictors > nEvents / 10) {
    self$results$instructions$setVisible(TRUE)
    private$.addNotice("ERROR", "Severe Overfit Risk",
        paste0("You have ", nPredictors, " predictors but only ", nEvents, " events. ",
               "This will cause severe overfitting. Recommend ≤ ", floor(nEvents/10),
               " predictors. Please reduce the number of predictor variables."))
    private$.renderNotices()
    return()
}
```

#### E. Show Instructions - Model Fitting Failed (Line ~246)

```r
}, error = function(e) {
    self$results$instructions$setVisible(TRUE)
    private$.addNotice("ERROR", "Model Fitting Failed",
        paste0("Error: ", e$message))
    private$.renderNotices()
    return()
})
```

#### F. Show Instructions - No Splits Found (Line ~268)

```r
# Check if tree has splits
if (nrow(tree$frame) == 1) {
    self$results$instructions$setVisible(TRUE)
    private$.addNotice("WARNING", "No Splits Found",
        "Tree has no splits. Try reducing minbucket or cp parameters.")
    private$.renderNotices()
    return()
}
```

---

## Behavior Logic

### Instructions are VISIBLE when:

1. **Initial State**: Module first opened (no variables selected)
2. **Missing Inputs**: Time, event, or predictors not selected
3. **Data Errors**:
   - Negative survival times detected
   - Insufficient events (< 10)
   - Too many predictors relative to events
4. **Analysis Errors**:
   - Model fitting fails (rpart error)
   - No splits generated (tree too simple)

### Instructions are HIDDEN when:

1. **Analysis Proceeds**: All required inputs provided and valid
2. **Successful Execution**: Analysis runs without errors

---

## User Experience Flow

### Scenario 1: First Use
```
User opens module
→ Instructions VISIBLE (visible: true in .r.yaml)
→ User sees helpful guidance on how to use the analysis
```

### Scenario 2: Successful Analysis
```
User selects time, event, and predictors
→ .run() executes
→ Required inputs check PASSES
→ Instructions HIDDEN (setVisible(FALSE) at line 111)
→ Results displayed
```

### Scenario 3: Error Encountered
```
User provides only 5 events
→ .run() executes
→ Insufficient events check FAILS
→ Instructions VISIBLE (setVisible(TRUE) at line 143)
→ Error notice displayed
→ User can read instructions again for guidance
```

### Scenario 4: Recovery from Error
```
User fixes data issue (adds more events)
→ .run() executes again
→ All checks PASS
→ Instructions HIDDEN (setVisible(FALSE) at line 111)
→ Results displayed successfully
```

---

## Reference Implementations

This pattern follows established jamovi best practices as seen in:

### jrecode.b.R (Primary Reference)

```r
# Show instructions when no variable selected
if (is.null(dep)) {
    self$results$instructions$setVisible(TRUE)
    return()
}

# Hide instructions when analysis proceeds
self$results$instructions$setVisible(FALSE)
self$results$levels_table$setVisible(self$options$show_levels)
self$results$code_output$setVisible(self$options$show_code)
```

### jiwillsurvive.b.R

```r
# Dynamic visibility based on analysis mode
if (mode == "survival") {
    self$results$survivalPlot$setVisible(TRUE)
    self$results$followupPlot$setVisible(FALSE)
} else if (mode == "followup") {
    self$results$followupPlot$setVisible(TRUE)
    self$results$survivalPlot$setVisible(FALSE)
}
```

### ihcdiagnostic.b.R

```r
# Complex conditional visibility
if (condition) {
    self$results$clinicalSummary$setVisible(TRUE)
} else {
    self$results$clinicalSummary$setVisible(FALSE)
}
```

---

## Design Principles Applied

### 1. **Helpful by Default**
Instructions visible on initial load guide new users through the analysis setup.

### 2. **Get Out of the Way**
Once analysis runs successfully, instructions hide to make room for results.

### 3. **Reappear on Error**
When errors occur, instructions return to provide context and guidance for fixing issues.

### 4. **Consistent UX**
Behavior matches other jamovi modules (jrecode, jiwillsurvive, etc.) for familiar user experience.

### 5. **Progressive Disclosure**
Show help when needed, hide when not needed - reduces cognitive load.

---

## Testing Scenarios

### Test 1: Initial Load
**Action:** Open rpasurvival module
**Expected:** Instructions visible
**Reason:** Default visible: true, no .run() executed yet

### Test 2: Select Variables
**Action:** Select time, event, and 1 predictor
**Expected:** Instructions hidden, results appear
**Reason:** setVisible(FALSE) at line 111

### Test 3: Remove Required Variable
**Action:** Deselect time variable
**Expected:** Instructions reappear
**Reason:** setVisible(TRUE) at line 103

### Test 4: Provide Invalid Data (Negative Times)
**Action:** Use data with negative survival times
**Expected:** Instructions visible, error notice shown
**Reason:** setVisible(TRUE) at line 127

### Test 5: Fix Data and Re-run
**Action:** Fix data, re-run analysis
**Expected:** Instructions hidden, results appear
**Reason:** setVisible(FALSE) at line 111

### Test 6: Insufficient Events
**Action:** Use dataset with only 5 events
**Expected:** Instructions visible, error notice
**Reason:** setVisible(TRUE) at line 143

### Test 7: Too Many Predictors
**Action:** Select 15 predictors with only 50 events
**Expected:** Instructions visible, overfit warning
**Reason:** setVisible(TRUE) at line 167

---

## Benefits Over .r.yaml-Only Approach

### Previous Implementation (.r.yaml conditional)
```yaml
visible: (time == null || event == null || predictors.length == 0)
```

**Limitations:**
- Only handles missing inputs
- Cannot respond to runtime errors (negative times, insufficient events, etc.)
- Static logic in YAML cannot access computed values

### Current Implementation (.b.R dynamic control)

**Advantages:**
1. **Runtime Awareness**: Can show instructions when any error occurs
2. **Comprehensive**: Handles all error scenarios, not just missing inputs
3. **Flexible**: Can implement complex visibility logic based on calculated values
4. **User-Friendly**: Instructions reappear whenever user needs guidance
5. **Maintainable**: Visibility logic lives with error handling logic in .b.R

---

## Code Locations Summary

| Visibility State | File | Line(s) | Trigger Condition |
|-----------------|------|---------|-------------------|
| VISIBLE (default) | rpasurvival.r.yaml | 11 | Initial load |
| VISIBLE | rpasurvival.b.R | 103 | Missing inputs |
| HIDDEN | rpasurvival.b.R | 111 | Analysis proceeds |
| VISIBLE | rpasurvival.b.R | 127 | Negative times |
| VISIBLE | rpasurvival.b.R | 143 | Insufficient events |
| VISIBLE | rpasurvival.b.R | 167 | Too many predictors |
| VISIBLE | rpasurvival.b.R | 246 | Model fitting error |
| VISIBLE | rpasurvival.b.R | 268 | No splits found |

---

## Validation

**Command:** `jmvtools::prepare()`
**Result:** ✅ Successful - No errors or warnings
**Status:** Ready for testing in jamovi UI

---

**Status:** ✅ COMPLETE

Dynamic visibility control implemented following jamovi best practices with comprehensive error handling and user-friendly behavior.
