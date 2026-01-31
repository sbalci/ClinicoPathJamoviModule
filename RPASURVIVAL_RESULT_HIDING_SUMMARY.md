# rpasurvival Result Hiding Implementation

**Date:** 2026-01-31
**Module:** rpasurvival (Recursive Partitioning Analysis for Survival)
**Feature:** Hide plots and tables when analysis fails to prevent user confusion

---

## Problem Statement

### User Issue

When the RPA analysis cannot generate a partition (due to errors like insufficient events, no splits found, etc.), empty plot and table placeholders remain visible while error messages are displayed. This creates confusion:

- User sees empty placeholders
- User waits for results to appear
- User doesn't understand that analysis has failed
- Poor UX - unclear whether analysis is running or failed

### Example Scenarios

1. **Insufficient events** (< 10 events) → Tree cannot be built
2. **Too many predictors** → Severe overfit risk
3. **No splits found** → Tree algorithm fails to partition
4. **Model fitting error** → rpart crashes
5. **Invalid data** → Negative survival times

---

## Solution

### Strategy

Implement dynamic result visibility management:

1. **Hide all results** when any error occurs
2. **Show all results** only when analysis succeeds
3. **Provide clear error messages** via notices
4. **Show instructions** to guide user on fixing the issue

---

## Implementation

### New Helper Methods

Added two new private methods to manage result visibility:

#### 1. `.hideResults()` - Hide all analytical outputs

```r
.hideResults = function() {
    self$results$summary$setVisible(FALSE)
    self$results$interpretation$setVisible(FALSE)
    self$results$report$setVisible(FALSE)
    self$results$treeplot$setVisible(FALSE)
    self$results$riskgrouptable$setVisible(FALSE)
    self$results$kmplot$setVisible(FALSE)
    self$results$logranktest$setVisible(FALSE)
    self$results$cptable$setVisible(FALSE)
    self$results$varimp$setVisible(FALSE)
    self$results$coxmodel$setVisible(FALSE)
}
```

**When called:** All error return paths

**Effect:** User sees only:
- Instructions (visible)
- Error notices (explaining what went wrong)
- No empty placeholders

#### 2. `.showResults()` - Show results based on user options

```r
.showResults = function() {
    # Show main results
    self$results$summary$setVisible(self$options$showSummary)
    self$results$interpretation$setVisible(self$options$showInterpretation)
    self$results$report$setVisible(self$options$showReport)

    # Show outputs based on user options
    self$results$treeplot$setVisible(self$options$treeplot)
    self$results$riskgrouptable$setVisible(self$options$riskgrouptable)
    self$results$kmplot$setVisible(self$options$kmplot)
    self$results$logranktest$setVisible(self$options$kmplot)
    self$results$cptable$setVisible(self$options$cptable)
    self$results$varimp$setVisible(self$options$variableimportance)
    self$results$coxmodel$setVisible(self$options$riskgrouptable)
}
```

**When called:** At end of successful analysis

**Effect:** User sees:
- All requested outputs (based on their option selections)
- Populated tables and plots
- Summary/interpretation/report (if requested)

---

## Modified Error Paths

All error return paths now call `private$.hideResults()`:

### 1. Missing Required Inputs

**Location:** `R/rpasurvival.b.R` line ~104

```r
if (is.null(self$options$time) || is.null(self$options$event) ||
    length(self$options$predictors) == 0) {
    self$results$instructions$setVisible(TRUE)
    private$.hideResults()  # NEW
    private$.addNotice("INFO", "Awaiting Input", ...)
    private$.renderNotices()
    return()
}
```

**User sees:**
- ✅ Instructions
- ✅ "Awaiting Input" notice
- ❌ No empty plots/tables

---

### 2. Invalid Time Values (Negative Times)

**Location:** `R/rpasurvival.b.R` line ~132

```r
if (any(timeVar < 0, na.rm = TRUE)) {
    self$results$instructions$setVisible(TRUE)
    private$.hideResults()  # NEW
    private$.addNotice("ERROR", "Invalid Time Values", ...)
    private$.renderNotices()
    return()
}
```

**User sees:**
- ✅ Instructions
- ✅ "Invalid Time Values" error
- ❌ No empty plots/tables

---

### 3. Insufficient Events

**Location:** `R/rpasurvival.b.R` line ~149

```r
if (nEvents < 10) {
    self$results$instructions$setVisible(TRUE)
    private$.hideResults()  # NEW
    private$.addNotice("ERROR", "Insufficient Events", ...)
    private$.renderNotices()
    return()
}
```

**User sees:**
- ✅ Instructions
- ✅ "Insufficient Events" error with count
- ❌ No empty plots/tables

---

### 4. Severe Overfit Risk

**Location:** `R/rpasurvival.b.R` line ~168

```r
if (nPredictors > nEvents / 10) {
    self$results$instructions$setVisible(TRUE)
    private$.hideResults()  # NEW
    private$.addNotice("ERROR", "Severe Overfit Risk", ...)
    private$.renderNotices()
    return()
}
```

**User sees:**
- ✅ Instructions
- ✅ "Severe Overfit Risk" error with recommendation
- ❌ No empty plots/tables

---

### 5. Model Fitting Failed

**Location:** `R/rpasurvival.b.R` line ~248

```r
}, error = function(e) {
    self$results$instructions$setVisible(TRUE)
    private$.hideResults()  # NEW
    private$.addNotice("ERROR", "Model Fitting Failed", ...)
    private$.renderNotices()
    return()
})
```

**User sees:**
- ✅ Instructions
- ✅ "Model Fitting Failed" error with technical details
- ❌ No empty plots/tables

---

### 6. No Splits Found

**Location:** `R/rpasurvival.b.R` line ~270

```r
if (nrow(tree$frame) == 1) {
    self$results$instructions$setVisible(TRUE)
    private$.hideResults()  # NEW
    private$.addNotice("WARNING", "No Splits Found", ...)
    private$.renderNotices()
    return()
}
```

**User sees:**
- ✅ Instructions
- ✅ "No Splits Found" warning with suggestions
- ❌ No empty plots/tables

---

## Success Path

When analysis completes successfully:

**Location:** `R/rpasurvival.b.R` line ~682

```r
# Show all results (analysis succeeded)
private$.showResults()  # NEW

# Render notices
private$.renderNotices()
```

**User sees:**
- ❌ Instructions hidden
- ✅ All requested outputs (plots, tables, summaries)
- ✅ Success notices (if any warnings)

---

## User Experience Flow

### Scenario 1: Insufficient Data

```
1. User selects time, event, 2 predictors
2. Dataset has only 5 events
3. .run() executes → nEvents < 10 check FAILS
4. private$.hideResults() called
   → All plots/tables hidden
5. Instructions shown
6. Error notice: "Only 5 events observed. Need at least 10 events for RPA."
7. User understands: Need more data, not a technical error
```

### Scenario 2: Too Many Predictors

```
1. User selects 20 predictors with 50 events
2. .run() executes → nPredictors > nEvents/10 check FAILS
3. private$.hideResults() called
   → All plots/tables hidden
4. Instructions shown
5. Error notice: "You have 20 predictors but only 50 events. Recommend ≤ 5 predictors."
6. User understands: Need to reduce predictors
7. User deselects 15 predictors
8. Analysis succeeds → private$.showResults() called
9. Results appear
```

### Scenario 3: No Splits Generated

```
1. User runs RPA with very strict parameters
2. Tree algorithm cannot find any valid splits
3. private$.hideResults() called
   → All plots/tables hidden
4. Instructions shown
5. Warning notice: "Tree has no splits. Try reducing minbucket or cp parameters."
6. User understands: Parameters too strict
7. User adjusts minbucket from 50 to 20
8. Analysis succeeds → private$.showResults() called
9. Tree plot, KM plot, tables all appear
```

---

## Benefits

### 1. Clear Communication

**Before:**
- Empty plot placeholders visible
- Error message buried in notices
- User confused: "Is it still running?"

**After:**
- Only instructions and error message visible
- No visual clutter
- Clear: "Analysis failed, here's why"

### 2. No False Expectations

**Before:**
- User sees "Kaplan-Meier Curves by Risk Group" header
- Expects plot to appear
- Waits indefinitely

**After:**
- No plot headers visible
- User immediately understands analysis stopped
- Can fix issue and re-run

### 3. Actionable Guidance

**Before:**
- Error message + empty placeholders
- User unsure what to do

**After:**
- Error message + instructions + no visual distractions
- User focuses on error message
- Instructions provide context on how to use RPA

### 4. Professional UX

Matches behavior of production-grade statistical software:
- SAS: Hides output when PROC fails
- SPSS: Shows error, no empty output windows
- Stata: Displays error message, suppresses failed output

---

## Technical Details

### Results Hidden on Error

All 10 analytical results are hidden when any error occurs:

| Result Element | Type | Hidden? |
|---------------|------|---------|
| `summary` | Html | ✅ |
| `interpretation` | Html | ✅ |
| `report` | Html | ✅ |
| `treeplot` | Image | ✅ |
| `riskgrouptable` | Table | ✅ |
| `kmplot` | Image | ✅ |
| `logranktest` | Table | ✅ |
| `cptable` | Table | ✅ |
| `varimp` | Table | ✅ |
| `coxmodel` | Table | ✅ |

**NOT hidden:**
- `instructions` - Explicitly shown on error
- `notices` - Used to display error messages

### Results Shown on Success

Visibility controlled by user options:

```r
# Main explanatory outputs (user-controlled)
summary:         showSummary option
interpretation:  showInterpretation option
report:          showReport option

# Analytical outputs (user-controlled)
treeplot:        treeplot option
riskgrouptable:  riskgrouptable option
kmplot:          kmplot option
logranktest:     kmplot option (tied to KM plot)
cptable:         cptable option
varimp:          variableimportance option
coxmodel:        riskgrouptable option (tied to risk table)
```

---

## Code Locations

| Feature | File | Line(s) | Description |
|---------|------|---------|-------------|
| `.hideResults()` method | rpasurvival.b.R | ~67-77 | Hides all analytical results |
| `.showResults()` method | rpasurvival.b.R | ~79-93 | Shows results based on options |
| Missing inputs | rpasurvival.b.R | ~104 | Calls hideResults() |
| Negative times | rpasurvival.b.R | ~132 | Calls hideResults() |
| Insufficient events | rpasurvival.b.R | ~149 | Calls hideResults() |
| Overfit risk | rpasurvival.b.R | ~168 | Calls hideResults() |
| Model fitting error | rpasurvival.b.R | ~248 | Calls hideResults() |
| No splits found | rpasurvival.b.R | ~270 | Calls hideResults() |
| Success path | rpasurvival.b.R | ~682 | Calls showResults() |

---

## Testing Recommendations

### Test Case 1: Missing Inputs
**Action:** Open rpasurvival without selecting variables
**Expected:** Instructions visible, all results hidden
**Verify:** No plot/table placeholders shown

### Test Case 2: Insufficient Events
**Action:** Use dataset with only 5 events
**Expected:** Error notice, instructions visible, all results hidden
**Verify:** User sees clear error message, no empty plots

### Test Case 3: Successful Analysis
**Action:** Provide valid inputs, run analysis
**Expected:** Instructions hidden, results visible (based on options)
**Verify:** All requested plots/tables populated and visible

### Test Case 4: Error → Fix → Success
**Action:**
1. Run with insufficient events (error)
2. Add more data
3. Re-run (success)
**Expected:**
1. First run: Results hidden
2. Second run: Results visible
**Verify:** Clean transition from error to success state

### Test Case 5: No Splits Found
**Action:** Set very high minbucket (e.g., 100) with small dataset
**Expected:** Warning notice, instructions visible, all results hidden
**Verify:** User understands need to adjust parameters

---

## Validation

**Command:** `jmvtools::prepare()`
**Result:** ✅ Successful - No errors or warnings

**Files Modified:**
- `R/rpasurvival.b.R` (added 2 helper methods, updated 6 error paths)

**Files Auto-Regenerated:**
- `R/rpasurvival.h.R`
- `jamovi/rpasurvival.src.js`

---

## Related Pattern

This implements the same UX pattern as:
- `jrecode.b.R` - Shows instructions when no variable selected
- Error handling best practices in jamovi modules

---

**Status:** ✅ COMPLETE

The result hiding feature has been successfully implemented. When RPA analysis fails, users now see clear error messages and instructions without confusing empty plot/table placeholders.
