# psychopdaROC Fixes - Confusion Matrices and Plots Not Displaying

**Date:** 2026-01-02
**Module:** psychopdaROC (Advanced ROC Analysis)
**Issues Fixed:** Confusion matrices showing placeholder, ROC plots not displaying

## Problems Identified

### 1. Confusion Matrix Placeholder Issue

**Location:** [R/psychopdaROC.b.R:1419](../R/psychopdaROC.b.R#L1419)

**Problem:**
The confusion matrix generation code was commented out and replaced with a placeholder:

```r
# Commented out code:
# html_table <- private$.formatSensSpecTable(
#   Title = paste("Confusion Matrix for", var, "at Optimal Cutpoint =",
#                 round(results$optimal_cutpoint[1], 3)),
#   TP = tp, FP = fp, TN = tn, FN = fn
# )

# Placeholder being used instead:
html_table <- "<table><tr><td>Placeholder</td></tr></table>"
```

**Root Cause:**
The actual confusion matrix formatting function `.formatSensSpecTable()` exists in the code (line 4173) but was disabled, likely during testing or development.

**Fix Applied:**
Uncommented the proper confusion matrix generation code to use the existing `.formatSensSpecTable()` helper function.

### 2. ROC Plots Not Visible Issue

**Location:** [R/psychopdaROC.b.R:1909](../R/psychopdaROC.b.R#L1909)

**Problem:**
The visibility setting for ROC plots was commented out:

```r
if (self$options$plotROC) {
  # self$results$plotROC$setVisible(TRUE)  # This was commented out!
}
```

**Root Cause:**
Even though plot data was being prepared and setState() was being called correctly (lines 2405-2416), the plots themselves were never made visible to the user.

**Fix Applied:**
Uncommented the visibility setting so plots will display when the `plotROC` option is enabled.

## Changes Made

### File: R/psychopdaROC.b.R

#### Change 1: Enable Confusion Matrix Display (lines 1413-1418)

```r
# BEFORE (lines 1414-1419):
# html_table <- private$.formatSensSpecTable(
#   Title = paste("Confusion Matrix for", var, "at Optimal Cutpoint =",
#                 round(results$optimal_cutpoint[1], 3)),
#   TP = tp, FP = fp, TN = tn, FN = fn
# )
html_table <- "<table><tr><td>Placeholder</td></tr></table>"

# AFTER (lines 1414-1418):
html_table <- private$.formatSensSpecTable(
  Title = paste("Confusion Matrix for", var, "at Optimal Cutpoint =",
                round(results$optimal_cutpoint[1], 3)),
  TP = tp, FP = fp, TN = tn, FN = fn
)
```

#### Change 2: Enable ROC Plot Visibility (line 1909)

```r
# BEFORE (line 1909):
if (self$options$plotROC) {
  # self$results$plotROC$setVisible(TRUE)
}

# AFTER (line 1909):
if (self$options$plotROC) {
  self$results$plotROC$setVisible(TRUE)
}
```

## Verification

### What Should Now Work:

1. **Confusion Matrices** (when `sensSpecTable` option is enabled):
   - Display properly formatted HTML tables showing TP, FP, TN, FN values
   - Show confusion matrix at the optimal cutpoint for each test variable
   - Formatted using the `.formatSensSpecTable()` helper with proper styling

2. **ROC Plots** (when `plotROC` option is enabled):
   - Display ROC curves with sensitivity vs (1 - specificity)
   - Show individual plots for each variable OR combined plots (based on `combinePlots` option)
   - Mark optimal cutpoints on curves (when `showOptimalPoint` is enabled)
   - Support additional plot options: smoothing, confidence bands, clean publication-ready format

3. **Additional Plots** (remain unchanged, already working):
   - Criterion plots (Sensitivity/Specificity vs Threshold)
   - Prevalence plots (Predictive Values vs Prevalence)
   - Dot plots (Test Value Distribution)
   - Precision-Recall curves
   - Fixed sensitivity/specificity ROC curves

## Testing Recommendations

To verify the fixes:

```r
# 1. Load the updated module in jamovi or R
library(ClinicoPath)

# 2. Test with sample data
result <- psychopdaROC(
  data = your_data,
  dependentVars = c("biomarker1", "biomarker2"),
  classVar = "disease_status",
  positiveClass = "Disease",
  plotROC = TRUE,              # Enable ROC plots
  sensSpecTable = TRUE,         # Enable confusion matrices
  combinePlots = TRUE,          # Test combined plot mode
  showOptimalPoint = TRUE       # Mark optimal points
)

# 3. Verify outputs:
# - Check that confusion matrices display with TP/FP/TN/FN values (not "Placeholder")
# - Check that ROC plots are visible with proper curves
# - Check that optimal cutpoints are marked on plots
```

## Related Files

- **Definition Files (unchanged):**
  - `jamovi/psychopdaROC.a.yaml` - Analysis options definition
  - `jamovi/psychopdaROC.r.yaml` - Results structure definition (defines sensSpecTable and plotROC)
  - `jamovi/psychopdaROC.u.yaml` - User interface definition

- **Documentation (auto-updated):**
  - `man/psychopdaROC.Rd` - R documentation file
  - `R/psychopdaROC.h.R` - Auto-generated header file

## Notes

- The helper function `.formatSensSpecTable()` (line 4173) was already implemented and working - it just needed to be called
- Plot setState() calls were already correct (lines 2405-2416, 2452-2464) - only visibility was the issue
- All other plot types (criterion, prevalence, dot plots, etc.) were already functioning correctly
- No changes needed to YAML definition files - the structure was correct

## Build Status

Documentation updated successfully with `devtools::document()`.

Module ready for testing in jamovi after running:
```bash
jmvtools::install()
```
