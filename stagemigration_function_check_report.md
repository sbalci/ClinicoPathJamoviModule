# STAGEMIGRATION FUNCTION CHECKER REPORT
**Analysis Date:** 2026-01-26  
**Profile:** standard  
**Working Directory:** /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule

---

## EXECUTIVE SUMMARY

**Module:** stagemigration (Advanced TNM Stage Migration Analysis)  
**Version:** 0.0.31  
**Overall Status:** ⚠️ **NEEDS CRITICAL FIXES**

### Key Metrics
| Metric | Value | Status |
|--------|-------|--------|
| Options Defined | 231 | ✅ |
| Options Used | 190 (82.3%) | ⚠️ 41 unused |
| Results Defined | ~180 | ✅ |
| Results Populated | 42 (23.3%) | ❌ ~138 unpopulated |
| Code Size | 28,878 lines | ⚠️ Very large |
| Prepare Status | ✅ PASS | ✅ |
| Variable Escaping | ❌ FAIL | Function exists but unused |

### Critical Issues
1. **Variable escaping function defined but NEVER USED (0 calls)** - CRITICAL
2. **~138 result outputs unpopulated (76.7%)** - HIGH
3. **41 options defined but unused (17.7%)** - MEDIUM
4. **Massive codebase (28K lines)** - MEDIUM (maintainability)

---

## 1. ARGS WIRING (Options → Backend)

### Summary Statistics
- **Total options defined:** 231
- **Total options referenced:** 190 (82.3%)
- **Unused options:** 41 (17.7%)

### ARG EFFECTS TABLE

| Flag | Effect | Details |
|------|--------|---------|
| **apply_escape_vars** | ❌ CRITICAL FAIL | Function exists at line 209 but has 0 calls |
| **align_labelled_logic** | ✅ PASS | 69 factor/labelled references found |
| **regroup_ui** | ➖ SKIPPED | Flag set to false |
| **remove_placeholders** | ➖ SKIPPED | Flag set to false |
| **defaults_false** | ➖ SKIPPED | Flag set to false |
| **style_welcome** | ➖ SKIPPED | Flag set to false |
| **run_prepare** | ✅ PASS | Module compiled successfully |
| **run_document** | ⚠️ WARN | Failed due to treecompare.h.R error (different module) |

### UNUSED OPTIONS (41 total)

**Category: Random Forest Analysis (15 options)**
- calculateVariableImportance
- forestBootstrap
- forestBootstrapSamples
- forestCovariates
- forestDiscriminationMetrics
- forestImportanceType
- forestMTry
- forestMinNodeSize
- forestModelType
- forestNTrees
- forestPredictionTimePoints
- forestStagingComparison
- performForestValidation
- generateForestSummary
- generateSurvivalPredictions

**Category: Cure Models (5 options)**
- cureConfidenceLevel
- cureFractionEstimation
- cureGoodnessOfFit
- cureModelComparison
- generateCureSummary

**Category: Frailty Models (6 options)**
- frailtyAdvancedInference
- frailtyDistribution
- frailtyHeterogeneityTest
- frailtyModelSelection
- frailtyPredictiveAccuracy
- frailtyVarianceComponents

**Category: Clinical Utility (2 options)**
- clinicalUtilityComparison
- clinicalUtilityCostEffectiveness

**Category: Competing Risks (3 options)**
- cifConfidenceLevel
- generateCIFPlots
- stratifyByStaging

**Category: Multi-State Models (4 options)**
- msStratifyByStaging
- multiStateGraphics
- multiStateModel
- multiStateValidation

**Category: Other (6 options)**
- complexityMode
- confidenceLevel
- enableAccessibilityFeatures
- performLikelihoodTests
- plotTimeRange
- showConfidenceIntervals
- useOptimismCorrection

**NOTE:** Many unused options appear to be placeholders for advanced features that may be under development.

---

## 2. OUTPUTS WIRING (Results → Backend)

### Summary Statistics
- **Total results defined:** ~180
- **Results with setters:** 42 (23.3%)
- **Unpopulated results:** ~138 (76.7%)

### POPULATED RESULTS (42 total)

These results have setter calls in R/stagemigration.b.R:

**Explanations (HTML content - 22 items):**
1. abbreviationGlossary
2. adjustedCIndexComparisonExplanation
3. bootstrapValidationExplanation
4. calibrationAnalysisExplanation
5. calibrationPlotsExplanation
6. clinicalInterpretationExplanation
7. competingRisksExplanation
8. crossValidationExplanation
9. dashboardExplanation
10. decisionCurvesExplanation
11. executiveSummaryExplanation
12. forestPlotExplanation
13. interactionTestsExplanation
14. likelihoodTestsExplanation
15. linearTrendTestExplanation
16. migrationHeatmapExplanation
17. multifactorialAnalysisExplanation
18. multifactorialResultsExplanation
19. nestedModelTestsExplanation
20. rmstAnalysisExplanation
21. rocComparisonExplanation
22. sankeyDiagramExplanation
23. stageMigrationEffectExplanation
24. statisticalSummaryExplanation
25. stepwiseResultsExplanation
26. stratifiedAnalysisExplanation
27. survivalCurvesExplanation
28. willRogersAnalysisExplanation
29. willRogersEvidenceSummaryExplanation

**Visualizations (Images/Plots - 7 items):**
30. calibrationPlots
31. decisionCurves
32. forestPlot
33. migrationHeatmap
34. rocComparisonPlot
35. sankeyDiagram
36. survivalCurves
37. willRogersVisualization
38. migrationSurvivalComparison

**Interactive/Reports (5 items):**
39. copyReadyReport
40. guidedModeProgress
41. methodologyNotes
42. welcomeMessage

### CRITICAL UNPOPULATED RESULTS (Sample - Top 30)

These are LIKELY unpopulated based on grep analysis:

**Core Tables (Migration Analysis):**
1. **migrationMatrix** - Stage migration cross-tabulation
2. **migrationOverview** - Summary statistics table
3. **migrationSummary** - Migration statistics
4. **stageDistribution** - Stage distribution comparison

**Statistical Comparison Tables:**
5. **statisticalComparison** - C-index comparison
6. **concordanceComparison** - Concordance metrics
7. **nriResults** - Net Reclassification Improvement
8. **idiResults** - Integrated Discrimination Improvement
9. **pseudoR2Results** - Pseudo R-squared measures

**Advanced Analysis Tables:**
10. **bootstrapResults** - Bootstrap validation
11. **dcaResults** - Decision curve analysis
12. **rocAnalysis** - ROC analysis results
13. **integratedAUCAnalysis** - Integrated AUC
14. **calibrationAnalysis** - Calibration metrics

**Multifactorial Analysis:**
15. **multifactorialResults** - Adjusted models
16. **adjustedCIndexComparison** - Adjusted concordance
17. **nestedModelTests** - Likelihood ratio tests
18. **stepwiseResults** - Stepwise selection
19. **interactionTests** - Interaction tests
20. **stratifiedAnalysis** - Stratified results

**Survival Tests:**
21. **homogeneityTests** - Stage homogeneity
22. **trendTests** - Linear trend tests
23. **likelihoodTests** - LR tests
24. **linearTrendTest** - Trend test results

**Clinical Interpretation:**
25. **clinicalInterpretation** - Clinical guidance
26. **executiveSummary** - Executive summary
27. **statisticalSummary** - Statistical summary
28. **effectSizes** - Effect size table

**Advanced Features:**
29. **comparativeAnalysisDashboard** - Dashboard table
30. **landmarkAnalysisResults** - Landmark analysis

...and approximately 108 more unpopulated results.

### MISSING SETTERS - Example Code Needed

**Example 1: Migration Matrix**
```r
# In .run() method - populate migration matrix
if (self$options$showMigrationMatrix) {
    matrix_table <- self$results$migrationMatrix
    
    # Dynamic columns
    for (new_stage in levels(newStage)) {
        matrix_table$addColumn(
            name = private$.escapeVar(new_stage),
            title = new_stage,
            type = 'integer'
        )
    }
    
    # Populate rows
    for (old_stage in levels(oldStage)) {
        row_data <- list(.name = old_stage)
        for (new_stage in levels(newStage)) {
            count <- sum(oldStage == old_stage & newStage == new_stage, na.rm = TRUE)
            row_data[[private$.escapeVar(new_stage)]] <- count
        }
        matrix_table$setRow(rowKey = old_stage, values = row_data)
    }
}
```

**Example 2: Statistical Comparison**
```r
# Populate statistical comparison table
if (self$options$showStatisticalComparison) {
    table <- self$results$statisticalComparison
    
    table$setRow(rowKey = "old_stage", values = list(
        metric = "C-index (Old)",
        value = format(c_old, digits = 3),
        ci = sprintf("[%0.3f, %0.3f]", ci_old[1], ci_old[2]),
        p = if(p_value < 0.001) "<0.001" else format(p_value, digits = 3)
    ))
    
    table$setRow(rowKey = "new_stage", values = list(
        metric = "C-index (New)",
        value = format(c_new, digits = 3),
        ci = sprintf("[%0.3f, %0.3f]", ci_new[1], ci_new[2]),
        p = if(p_value < 0.001) "<0.001" else format(p_value, digits = 3)
    ))
}
```

---

## 3. VARIABLE SAFETY (Escaping)

### Current Implementation

**Function Definition (Line 209):**
```r
.escapeVar = function(x) {
    # Handle variables with spaces/special characters
    gsub("[^A-Za-z0-9_]+", "_", make.names(x))
}
```

### Critical Issue: FUNCTION NEVER USED

**Usage Analysis:**
- ✅ Function EXISTS at line 209
- ❌ Function CALLS: **0** (zero)
- ❌ Variable access: All use raw column names without escaping

**Consequence:**
Variables with spaces, special characters, or Unicode will cause errors:
- `data[["Patient Name"]]` → ERROR
- `data[["Age (years)"]]` → ERROR
- `data[["Stage-T"]]` → ERROR

### PATCHES REQUIRED

**Patch 1: Core Variable Access**

**File:** R/stagemigration.b.R  
**Search for:** `data[[self$options$`

```diff
# BEFORE (unsafe)
- oldStage <- data[[self$options$oldStage]]
- newStage <- data[[self$options$newStage]]
- survivalTime <- data[[self$options$survivalTime]]
- eventVar <- data[[self$options$event]]

# AFTER (safe)
+ oldStage <- data[[private$.escapeVar(self$options$oldStage)]]
+ newStage <- data[[private$.escapeVar(self$options$newStage)]]
+ survivalTime <- data[[private$.escapeVar(self$options$survivalTime)]]
+ eventVar <- data[[private$.escapeVar(self$options$event)]]
```

**Patch 2: Covariate Access**

```diff
# BEFORE
- for (cov in self$options$continuousCovariates) {
-     cov_data[[cov]] <- data[[cov]]
- }

# AFTER
+ for (cov in self$options$continuousCovariates) {
+     safe_name <- private$.escapeVar(cov)
+     cov_data[[safe_name]] <- data[[safe_name]]
+ }
```

**Patch 3: Dynamic Column Names in Results**

```diff
# BEFORE
- matrix_table$addColumn(name = stage_name, ...)

# AFTER
+ matrix_table$addColumn(name = private$.escapeVar(stage_name), ...)
```

**Implementation Priority:** 🔴 **CRITICAL**

---

## 4. LABELLED LOGIC

### Analysis
- ✅ Factor/labelled references: **69 instances**
- ✅ Uses: `as.factor()`, `levels()`, appropriate type checking

### Sample Patterns Found
```r
as.factor(variable)
levels(factor_var)
is.factor(x)
```

### Assessment
✅ **ADEQUATE** - Module appears to handle factor/labelled variables appropriately.

**Note:** Aligns with jamovi best practices for ordinal/nominal variable handling.

---

## 5. VALIDATION RESULTS

### jmvtools::prepare()

**Status:** ✅ **PASSED**

Output:
```
wrote: stagemigration.h.R
modified: stagemigration.u.yaml
  - added ctrl: performCompetingRisksAdvanced
  - added ctrl: generateCRSummary
  [... additional controls added ...]
wrote: stagemigration.u.yaml
wrote: stagemigration.src.js
```

**Notes:**
- stagemigration.a.yaml: No errors
- stagemigration.r.yaml: No errors
- stagemigration.u.yaml: Modified successfully
- .h.R file generated successfully

**Errors in other modules:**
- treemedical.r.yaml: duplicated mapping key (line 145)

### devtools::document()

**Status:** ⚠️ **FAILED** (unrelated error)

Error:
```
Error: repeated formal argument 'plot_parttree' 
(/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/treecompare.h.R:38:13)
```

**Note:** Error is in treecompare.h.R, NOT in stagemigration. The stagemigration module itself appears valid.

---

## 6. TESTING CHECKLIST

### Status Summary

| Requirement | Status | Notes |
|------------|--------|-------|
| **Variable escaping** | ❌ CRITICAL FAIL | Function exists but has 0 uses |
| **Options wiring** | ⚠️ PARTIAL PASS | 190/231 used (82.3%) |
| **Outputs population** | ❌ FAIL | Only 42/180 populated (23.3%) |
| **Prepare compiles** | ✅ PASS | Module compiled successfully |
| **Document builds** | ⚠️ WARN | Error in different module |
| **Factor handling** | ✅ PASS | Adequate labelled logic (69 refs) |
| **Error handling** | ✅ PASS | .safeExecute pattern implemented |
| **Code organization** | ⚠️ WARN | 28K lines - very large |

### Testing Requirements

**Must Test:**
1. Variables with spaces in names
2. Variables with special characters (parentheses, hyphens, Unicode)
3. Factor variables with labels
4. Missing data handling
5. Small sample sizes (n < 30)
6. Single-stage scenarios
7. All analysis type presets

---

## 7. RECOMMENDED ACTIONS

### IMMEDIATE (Critical Priority)

#### 1. Implement Variable Escaping 🔴 CRITICAL
**Why:** Function exists but unused - variables with spaces will cause errors  
**Action:**
- Apply `private$.escapeVar()` to ALL data column access
- Apply to dynamic column names in results
- Apply to covariate loops

**Estimated effort:** 2-4 hours  
**Files affected:** R/stagemigration.b.R (~50-100 locations)

**Example locations to fix:**
- Line ~2900+: Core variable extraction
- Line ~4000+: Covariate handling
- Line ~5000+: Result table population

#### 2. Populate Critical Results 🔴 HIGH
**Why:** Core tables unpopulated - users get no output  
**Action:**
- Add setters for top 20 critical results
- Priority order:
  1. migrationMatrix (most important)
  2. statisticalComparison (C-index)
  3. migrationOverview
  4. nriResults (if calculateNRI enabled)
  5. idiResults (if calculateIDI enabled)
  6. bootstrapResults (if performBootstrap enabled)

**Estimated effort:** 8-16 hours  
**Files affected:** R/stagemigration.b.R (multiple locations in .run())

### MEDIUM PRIORITY

#### 3. Clean Up Unused Options 🟡 MEDIUM
**Why:** 41 unused options confuse users and bloat configuration  
**Action:**
- Document which options are placeholders
- Remove truly unused options
- Implement high-priority unused features (e.g., performLikelihoodTests)

**Estimated effort:** 4-8 hours  
**Files affected:** jamovi/stagemigration.a.yaml, jamovi/stagemigration.u.yaml

#### 4. Code Refactoring 🟡 MEDIUM
**Why:** 28K lines in single file is unmaintainable  
**Action:**
- Extract helper functions to separate files
- Create modules for:
  - Will Rogers detection
  - Bootstrap validation
  - Clinical interpretation
  - Plot generation

**Estimated effort:** 16-40 hours  
**Files to create:** R/stagemigration_helpers.R, R/stagemigration_plots.R, etc.

### LOW PRIORITY

#### 5. Documentation 🟢 LOW
**Why:** Complex module needs user guidance  
**Action:**
- Create vignette: "TNM Stage Migration Analysis Workflow"
- Add example data
- Document clinical preset behavior

**Estimated effort:** 4-8 hours  
**Files to create:** vignettes/stagemigration-workflow.Rmd, data/example_tnm.rda

#### 6. Comprehensive Testing 🟢 LOW
**Why:** Ensure all code paths work correctly  
**Action:**
- Create test suite with edge cases
- Test all analysis type combinations
- Test with various cancer types

**Estimated effort:** 8-16 hours  
**Files to create:** tests/testthat/test-stagemigration.R

---

## 8. DETAILED PATCHES

### Patch Set 1: Variable Escaping (CRITICAL)

**File:** R/stagemigration.b.R  
**Priority:** 🔴 CRITICAL

#### Location 1: Core Variables (~line 2900)

```diff
         # Extract core variables
-        oldStage <- data[[self$options$oldStage]]
-        newStage <- data[[self$options$newStage]]
-        survivalTime <- data[[self$options$survivalTime]]
-        eventVar <- data[[self$options$event]]
+        oldStageVar <- private$.escapeVar(self$options$oldStage)
+        newStageVar <- private$.escapeVar(self$options$newStage)
+        survTimeVar <- private$.escapeVar(self$options$survivalTime)
+        eventVarName <- private$.escapeVar(self$options$event)
+        
+        oldStage <- data[[oldStageVar]]
+        newStage <- data[[newStageVar]]
+        survivalTime <- data[[survTimeVar]]
+        eventVar <- data[[eventVarName]]
```

#### Location 2: Continuous Covariates

```diff
         if (!is.null(self$options$continuousCovariates) && 
             length(self$options$continuousCovariates) > 0) {
             
             for (cov in self$options$continuousCovariates) {
-                cov_data[[cov]] <- data[[cov]]
+                safe_cov <- private$.escapeVar(cov)
+                cov_data[[safe_cov]] <- data[[safe_cov]]
             }
         }
```

#### Location 3: Categorical Covariates

```diff
         if (!is.null(self$options$categoricalCovariates) && 
             length(self$options$categoricalCovariates) > 0) {
             
             for (cov in self$options$categoricalCovariates) {
-                cov_data[[cov]] <- as.factor(data[[cov]])
+                safe_cov <- private$.escapeVar(cov)
+                cov_data[[safe_cov]] <- as.factor(data[[safe_cov]])
             }
         }
```

#### Location 4: Dynamic Column Names in Results

```diff
         # Add dynamic columns to migration matrix
         for (stage in levels(newStage)) {
             matrix_table$addColumn(
-                name = stage,
+                name = private$.escapeVar(stage),
                 title = stage,
                 type = 'integer'
             )
         }
```

### Patch Set 2: Critical Result Population (HIGH PRIORITY)

**File:** R/stagemigration.b.R  
**Priority:** 🔴 HIGH

#### Migration Matrix

```r
# Add to .run() method after data preparation

if (self$options$showMigrationMatrix) {
    matrix_table <- self$results$migrationMatrix
    
    # Get stage levels
    old_levels <- levels(as.factor(oldStage))
    new_levels <- levels(as.factor(newStage))
    
    # Add dynamic columns for each new stage
    for (new_stage in new_levels) {
        matrix_table$addColumn(
            name = private$.escapeVar(new_stage),
            title = new_stage,
            type = 'integer'
        )
    }
    
    # Populate matrix rows
    for (old_level in old_levels) {
        row_data <- list(.name = old_level)
        
        for (new_level in new_levels) {
            count <- sum(oldStage == old_level & newStage == new_level, na.rm = TRUE)
            row_data[[private$.escapeVar(new_level)]] <- count
        }
        
        matrix_table$setRow(rowKey = old_level, values = row_data)
    }
}
```

#### Statistical Comparison

```r
# Add after C-index calculation

if (self$options$showStatisticalComparison) {
    table <- self$results$statisticalComparison
    
    # Old staging C-index
    table$setRow(rowKey = "old", values = list(
        metric = "C-index (Original)",
        value = format(round(c_index_old$concordance, 3), nsmall = 3),
        ci = sprintf("[%0.3f, %0.3f]", 
                    c_index_old$concordance - 1.96*c_index_old$std.err,
                    c_index_old$concordance + 1.96*c_index_old$std.err),
        p = ""
    ))
    
    # New staging C-index
    table$setRow(rowKey = "new", values = list(
        metric = "C-index (New)",
        value = format(round(c_index_new$concordance, 3), nsmall = 3),
        ci = sprintf("[%0.3f, %0.3f]",
                    c_index_new$concordance - 1.96*c_index_new$std.err,
                    c_index_new$concordance + 1.96*c_index_new$std.err),
        p = ""
    ))
    
    # Difference
    diff <- c_index_new$concordance - c_index_old$concordance
    table$setRow(rowKey = "diff", values = list(
        metric = "Difference",
        value = format(round(diff, 3), nsmall = 3),
        ci = "",
        p = format.pval(p_value_comparison, digits = 3)
    ))
}
```

#### Migration Overview

```r
# Add to .run() after migration matrix

if (self$options$showMigrationOverview) {
    table <- self$results$migrationOverview
    
    n_total <- length(oldStage)
    n_concordant <- sum(as.character(oldStage) == as.character(newStage), na.rm = TRUE)
    n_upstaged <- sum(as.numeric(newStage) > as.numeric(oldStage), na.rm = TRUE)
    n_downstaged <- sum(as.numeric(newStage) < as.numeric(oldStage), na.rm = TRUE)
    
    table$setRow(rowKey = "total", values = list(
        statistic = "Total Patients",
        value = as.character(n_total),
        percentage = "100.0%"
    ))
    
    table$setRow(rowKey = "concordant", values = list(
        statistic = "Stage Unchanged",
        value = as.character(n_concordant),
        percentage = sprintf("%0.1f%%", 100 * n_concordant / n_total)
    ))
    
    table$setRow(rowKey = "upstaged", values = list(
        statistic = "Upstaged",
        value = as.character(n_upstaged),
        percentage = sprintf("%0.1f%%", 100 * n_upstaged / n_total)
    ))
    
    table$setRow(rowKey = "downstaged", values = list(
        statistic = "Downstaged",
        value = as.character(n_downstaged),
        percentage = sprintf("%0.1f%%", 100 * n_downstaged / n_total)
    ))
}
```

---

## 9. CONCLUSION

### Overall Assessment: ⚠️ **NEEDS CRITICAL WORK**

**Strengths:**
- ✅ Comprehensive option set (231 options)
- ✅ Compiles successfully (prepare() passed)
- ✅ Has variable escaping function defined
- ✅ Uses error handling patterns
- ✅ Adequate factor/labelled handling
- ✅ Some key results are populated (42 items)

**Critical Weaknesses:**
- ❌ Variable escaping function NEVER USED (0 calls)
- ❌ Majority of results unpopulated (~138/180 = 76.7%)
- ⚠️ 41 unused options (17.7%)
- ⚠️ Massive codebase (28,878 lines) - maintainability concern

### Release Readiness: ❌ **NOT READY FOR PRODUCTION**

**Blocking Issues:**
1. **CRITICAL:** Variable escaping not implemented - will fail with non-standard variable names
2. **HIGH:** Core result tables unpopulated - users get minimal output
3. **MEDIUM:** Large number of unused options suggests incomplete features

### Recommended Timeline

**Phase 1 (CRITICAL - 1 week):**
1. Implement variable escaping throughout (2-4 hours)
2. Populate top 10 critical results (8-12 hours)
3. Test with edge-case data (4 hours)

**Phase 2 (HIGH - 2 weeks):**
4. Populate remaining important results (16-24 hours)
5. Clean up unused options (8 hours)
6. Add basic testing suite (8 hours)

**Phase 3 (MEDIUM - 1 month):**
7. Refactor into modular structure (40 hours)
8. Complete documentation (8 hours)
9. Comprehensive testing (16 hours)

### Next Steps

1. **Immediate:** Apply Patch Set 1 (Variable Escaping)
2. **This week:** Apply Patch Set 2 (Critical Results)
3. **Next week:** Test with real TNM data
4. **Within month:** Address unused options and refactor

---

## APPENDICES

### Appendix A: Full List of Unused Options (50 items)

See section 1 for complete details.

### Appendix B: Test Data Requirements

**Edge Cases to Test:**
1. Variables with spaces: "Patient Name", "Age (years)"
2. Variables with hyphens: "Stage-T", "Node-Status"
3. Variables with unicode: "Diagnóstico", "Статус"
4. Small samples: n = 10, 20, 30
5. Single-stage data: All same stage
6. Perfect concordance: oldStage == newStage
7. Complete disagreement: All stages changed
8. Missing data: NA in stages, survival, event

### Appendix C: File Paths

**Analysis Files:**
- .a.yaml: /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/stagemigration.a.yaml
- .b.R: /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/stagemigration.b.R
- .r.yaml: /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/stagemigration.r.yaml
- .u.yaml: /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/stagemigration.u.yaml
- .h.R: /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/stagemigration.h.R (auto-generated)

---

**Report Generated:** 2026-01-26  
**Checker Version:** function-checker v1.0  
**Contact:** See CLAUDE.md for development guidelines

