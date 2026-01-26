# Stage Migration Integration Map

**Purpose:** Complete mapping of dataflow across all stagemigration files
**Date:** 2026-01-26
**Status:** Integration Guide for .a.yaml → .b.R → utils → .r.yaml

---

## 🔄 Dataflow Overview

```
User Input (jamovi UI)
       ↓
.u.yaml (UI controls) ───┐
       ↓                  │
.a.yaml (options) ────────┤ Must Match!
       ↓                  │
.b.R (backend) ───────────┘
       ↓
utility functions (utils, validation, discrimination, competing-risks)
       ↓
results populated
       ↓
.r.yaml (outputs) ← Must match what .b.R populates
       ↓
Display to user
```

---

## 📋 Core Variable Options

| .a.yaml Option | .u.yaml Control | .b.R Access | Utility Function | .r.yaml Output |
|---------------|----------------|-------------|------------------|----------------|
| `data` | VariableSupplier | `self$data` | `stagemigration_validateData()` | N/A (internal) |
| `oldStage` | VariablesListBox | `self$options$oldStage` | `validateStagingVars()` | All tables |
| `newStage` | VariablesListBox | `self$options$newStage` | `validateStagingVars()` | All tables |
| `survivalTime` | VariablesListBox | `self$options$survivalTime` | `validateData()` | All tables |
| `event` | VariablesListBox | `self$options$event` | `createEventBinary()` | All tables |
| `eventLevel` | LevelSelector | `self$options$eventLevel` | `createEventBinary()` | N/A |

**Integration Status:** ✅ All core variables must be validated before any analysis

**Required Change in .b.R:**
```r
# At start of .run() method:
validation <- stagemigration_validateData(self$data, self$options)

if (!validation$valid) {
    # Show errors
    for (error in validation$errors) {
        # Display to user
    }
    return()
}

# Use validated data
data <- validation$data
old_stage <- self$options$oldStage
new_stage <- self$options$newStage
# etc.
```

---

## 🎛️ New Option: complexityMode (MUST ADD)

**Status:** ⚠️ **MISSING FROM CURRENT .a.yaml** - Must be added

**Add to .a.yaml:**
```yaml
- name: complexityMode
  title: 'Analysis Complexity Mode'
  type: List
  options:
    - name: quick
      title: Quick Clinical Check (5-10 min)
    - name: standard
      title: Standard Analysis (30-60 min)
    - name: comprehensive
      title: Comprehensive Analysis (1-2 hours)
    - name: custom
      title: Custom Configuration
  default: quick
  description: >-
    Controls which features are enabled. Quick mode shows only essential outputs,
    Standard adds common validation metrics, Comprehensive includes all methods,
    Custom allows manual control of all options.
```

**Add to .u.yaml:** Already present in reorganized version at top of Section 2

**Use in .b.R:**
```r
# Determine active features based on mode
mode <- self$options$complexityMode

# Quick mode: basic outputs only
if (mode == "quick") {
    # Enable: migration matrix, overview, C-index
    # Disable: NRI, bootstrap, ROC, etc.
}

# Standard mode: add common metrics
if (mode %in% c("standard", "comprehensive", "custom")) {
    # Enable: NRI, survival curves
}

# Comprehensive mode: all methods
if (mode %in% c("comprehensive", "custom")) {
    # Enable: bootstrap, ROC, DCA
}
```

**No .r.yaml changes needed** - results visibility controlled by existing options

---

## 📊 Analysis Type Options

| .a.yaml Option | Current Values | .b.R Usage | Recommendation |
|---------------|---------------|------------|----------------|
| `analysisType` | basic, standard, comprehensive, publication | `self$options$analysisType` | ⚠️ **DEPRECATE** - Use `complexityMode` instead |
| `clinicalPreset` | routine_clinical, research_study, publication_ready, custom | `self$options$clinicalPreset` | ✅ Keep - Sets defaults |

**Integration Decision:**
- Keep both but make `analysisType` hidden (backward compatibility)
- Primary UI control: `complexityMode` (simpler names: quick/standard/comprehensive/custom)
- `clinicalPreset` sets initial defaults for complex options
- `analysisType` = `complexityMode` internally for backward compatibility

---

## 🔬 Discrimination Metrics

### Net Reclassification Improvement (NRI)

| Component | Status | Location |
|-----------|--------|----------|
| **.a.yaml option** | ✅ `calculateNRI` (line 214) | Exists |
| **.a.yaml time points** | ✅ `nriTimePoints` (line 222) | Exists |
| **.u.yaml control** | ✅ CheckBox + TextBox | Exists |
| **.b.R usage** | ⚠️ Custom code | **REPLACE with:** |
| **Utility function** | ✅ `stagemigration_calculateNRI()` | **stagemigration-discrimination.R** |
| **.r.yaml output** | ✅ `nriResults` (line 271) | Exists |

**Required Change in .b.R:**
```r
# OLD (delete custom NRI code, ~100 lines):
# if (self$options$calculateNRI) {
#     ... manual NRI calculation ...
# }

# NEW (single function call):
if (self$options$calculateNRI) {
    # Parse time points
    time_points_str <- self$options$nriTimePoints
    time_points <- if (!is.null(time_points_str) && time_points_str != "") {
        as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*")))
    } else {
        c(12, 24, 60)  # Default
    }

    # Calculate NRI
    nri_results <- stagemigration_calculateNRI(
        data = data,
        old_stage = old_stage,
        new_stage = new_stage,
        time_var = time_var,
        event_var = "event_binary",
        time_points = time_points
    )

    # Populate nriResults table
    if (!is.null(nri_results) && !is.null(nri_results[[1]])) {
        for (tp_name in names(nri_results)) {
            tp_result <- nri_results[[tp_name]]
            if (!is.null(tp_result$nri_overall)) {
                row <- list(
                    TimePoint = tp_result$time_point,
                    NRI = tp_result$nri_overall,
                    NRI_CI_Lower = tp_result$ci_lower,
                    NRI_CI_Upper = tp_result$ci_upper,
                    NRI_Plus = tp_result$nri_events,
                    NRI_Minus = tp_result$nri_nonevents,
                    p_value = tp_result$p_value
                )
                self$results$nriResults$addRow(rowKey=tp_name, values=row)
            }
        }
    }
}
```

---

### Integrated Discrimination Improvement (IDI)

| Component | Status | Location |
|-----------|--------|----------|
| **.a.yaml option** | ✅ `calculateIDI` (line 230) | Exists |
| **.u.yaml control** | ✅ CheckBox | Exists |
| **.b.R usage** | ⚠️ Custom code | **REPLACE with:** |
| **Utility function** | ✅ `stagemigration_calculateIDI()` | **stagemigration-discrimination.R** |
| **.r.yaml output** | ✅ `idiResults` (line 321) | Exists |

**Required Change in .b.R:**
```r
# OLD (delete custom IDI code, ~80 lines)

# NEW:
if (self$options$calculateIDI) {
    idi_results <- stagemigration_calculateIDI(
        data = data,
        old_stage = old_stage,
        new_stage = new_stage,
        time_var = time_var,
        event_var = "event_binary",
        time_point = 60  # Or from options
    )

    # Populate idiResults table
    if (!is.null(idi_results$idi)) {
        self$results$idiResults$addRow(rowKey=1, values=list(
            IDI = idi_results$idi,
            IDI_CI_Lower = idi_results$idi_ci_lower,
            IDI_CI_Upper = idi_results$idi_ci_upper,
            p_value = idi_results$idi_p_value,
            Interpretation = if (idi_results$idi_p_value < 0.05) {
                paste0("Significant improvement (IDI = ",
                       round(idi_results$idi, 3), ")")
            } else {
                "No significant improvement"
            }
        ))
    }
}
```

---

### C-Index Comparison

| Component | Status | Location |
|-----------|--------|----------|
| **.a.yaml option** | ✅ `showStatisticalComparison` (line 423) | Exists |
| **.a.yaml bootstrap** | ✅ `performBootstrap` (line 271) | Exists |
| **.a.yaml reps** | ✅ `bootstrapReps` (line 279) | Exists |
| **.u.yaml control** | ✅ Multiple CheckBoxes | Exists |
| **.b.R usage** | ⚠️ Calls helper function | **REPLACE with:** |
| **Utility function** | ✅ `stagemigration_calculateConcordance()` | **stagemigration-discrimination.R** |
| **.r.yaml output** | ✅ `statisticalComparison`, `concordanceComparison` | Both exist |

**Required Change in .b.R:**
```r
# OLD (calls stagemigration_calculateAdvancedMetrics from helpers.R)

# NEW:
concordance_results <- stagemigration_calculateConcordance(
    data = data,
    old_stage = old_stage,
    new_stage = new_stage,
    time_var = time_var,
    event_var = "event_binary",
    perform_bootstrap = self$options$performBootstrap,
    bootstrap_reps = self$options$bootstrapReps %||% 1000,
    checkpoint_callback = private$.checkpoint
)

# Populate statisticalComparison table
if (self$options$showStatisticalComparison) {
    comp_table <- self$results$statisticalComparison

    # Old C-index
    comp_table$addRow(rowKey="c_old", values=list(
        metric = "Old Staging C-Index",
        value = round(concordance_results$old_c, 3),
        ci = paste0(round(concordance_results$old_c - 1.96*concordance_results$old_c_se, 3),
                   " - ",
                   round(concordance_results$old_c + 1.96*concordance_results$old_c_se, 3)),
        interpretation = stagemigration_interpretCIndex(concordance_results$old_c)
    ))

    # New C-index
    comp_table$addRow(rowKey="c_new", values=list(
        metric = "New Staging C-Index",
        value = round(concordance_results$new_c, 3),
        ci = paste0(round(concordance_results$new_c - 1.96*concordance_results$new_c_se, 3),
                   " - ",
                   round(concordance_results$new_c + 1.96*concordance_results$new_c_se, 3)),
        interpretation = stagemigration_interpretCIndex(concordance_results$new_c)
    ))

    # Improvement
    comp_table$addRow(rowKey="c_improvement", values=list(
        metric = "C-Index Improvement",
        value = round(concordance_results$c_improvement, 3),
        ci = paste0(round(concordance_results$c_improvement_ci_lower, 3),
                   " - ",
                   round(concordance_results$c_improvement_ci_upper, 3)),
        interpretation = if (concordance_results$c_improvement_p < 0.05) {
            paste0("Statistically significant (p=",
                   format.pval(concordance_results$c_improvement_p, digits=3), ")")
        } else {
            "Not statistically significant"
        }
    ))
}
```

---

## 🧪 Missing Features (Now Implemented)

### Competing Risks Analysis

| Component | Status | Location |
|-----------|--------|----------|
| **.a.yaml option** | ✅ `performCompetingRisks` (line 800) | Exists |
| **.a.yaml event var** | ✅ `competingEventVar` (line 811) | Exists |
| **.u.yaml control** | ✅ CheckBox + VariablesListBox | Exists (but in wrong section) |
| **.b.R usage** | ❌ Shows "not supported" message | **IMPLEMENT:** |
| **Utility function** | ✅ `stagemigration_competingRisksAnalysis()` | **stagemigration-competing-risks.R** |
| **.r.yaml output** | ⚠️ **MISSING** - Need to add | **ADD NEW TABLES:** |

**Required Changes:**

**1. Add to .r.yaml (after line ~400):**
```yaml
# Competing Risks Analysis
- name: competingRisksExplanation
  title: Understanding Competing Risks Analysis
  type: Html
  visible: (performCompetingRisks && showExplanations)
  clearWith:
    - performCompetingRisks

- name: competingRisksSummary
  title: Competing Risks Summary
  type: Table
  visible: (performCompetingRisks)
  clearWith:
    - oldStage
    - newStage
    - survivalTime
    - event
    - competingEventVar
  columns:
    - name: statistic
      title: "Statistic"
      type: text
    - name: old_stage_value
      title: "Old Staging"
      type: number
    - name: new_stage_value
      title: "New Staging"
      type: number
    - name: p_value
      title: "P-value"
      type: number
      format: zto,pvalue

- name: grayTestResults
  title: "Gray's Test for Equality of Cumulative Incidence"
  type: Table
  visible: (performCompetingRisks)
  clearWith:
    - oldStage
    - newStage
    - competingEventVar
  columns:
    - name: staging_system
      title: "Staging System"
      type: text
    - name: chi_square
      title: "Chi-Square"
      type: number
    - name: df
      title: "df"
      type: integer
    - name: p_value
      title: "P-value"
      type: number
      format: zto,pvalue
    - name: interpretation
      title: "Interpretation"
      type: text
```

**2. Add to .b.R:**
```r
if (self$options$performCompetingRisks) {
    # Validate competing event variable
    competing_event_var <- self$options$competingEventVar

    if (is.null(competing_event_var) || competing_event_var == "") {
        # Show error
        self$results$competingRisksSummary$setNote("error",
            "Please select a competing event variable")
        return()
    }

    # Run competing risks analysis
    competing_results <- stagemigration_competingRisksAnalysis(
        data = data,
        old_stage = old_stage,
        new_stage = new_stage,
        time_var = time_var,
        event_var = competing_event_var,
        event_of_interest = self$options$eventLevel,  # Primary event
        checkpoint_callback = private$.checkpoint
    )

    # Check for errors
    if (!is.null(competing_results$error)) {
        self$results$competingRisksSummary$setNote("error",
            competing_results$error)
        return()
    }

    # Populate Gray's test results
    if (!is.null(competing_results$gray_test_old)) {
        self$results$grayTestResults$addRow(rowKey="old", values=list(
            staging_system = "Old Staging",
            chi_square = competing_results$gray_test_old$statistic,
            df = competing_results$gray_test_old$df,
            p_value = competing_results$gray_test_old$p_value,
            interpretation = competing_results$gray_test_old$interpretation
        ))
    }

    if (!is.null(competing_results$gray_test_new)) {
        self$results$grayTestResults$addRow(rowKey="new", values=list(
            staging_system = "New Staging",
            chi_square = competing_results$gray_test_new$statistic,
            df = competing_results$gray_test_new$df,
            p_value = competing_results$gray_test_new$p_value,
            interpretation = competing_results$gray_test_new$interpretation
        ))
    }

    # Populate summary table
    self$results$competingRisksSummary$addRow(rowKey="events", values=list(
        statistic = "Event of Interest",
        old_stage_value = NA,
        new_stage_value = NA,
        p_value = NA
    ))
    # Add more rows for different competing risks metrics...
}
```

---

### RMST Analysis

| Component | Status | Location |
|-----------|--------|----------|
| **.a.yaml option** | ✅ `calculateRMST` (line 788) | Exists |
| **.u.yaml control** | ✅ CheckBox | Exists |
| **.b.R usage** | ❌ Not implemented | **IMPLEMENT:** |
| **Utility function** | ✅ `stagemigration_calculateRMST()` | **stagemigration-competing-risks.R** |
| **.r.yaml output** | ⚠️ **MISSING** - Need to add | **ADD NEW TABLES:** |

**Required Changes:**

**1. Add to .r.yaml:**
```yaml
# RMST Analysis
- name: rmstExplanation
  title: Understanding Restricted Mean Survival Time (RMST)
  type: Html
  visible: (calculateRMST && showExplanations)

- name: rmstResults
  title: "RMST by Stage Group"
  type: Table
  visible: (calculateRMST)
  columns:
    - name: staging_system
      title: "Staging System"
      type: text
    - name: stage
      title: "Stage"
      type: text
    - name: rmst
      title: "RMST (months)"
      type: number
    - name: se
      title: "SE"
      type: number
    - name: ci_lower
      title: "95% CI Lower"
      type: number
    - name: ci_upper
      title: "95% CI Upper"
      type: number

- name: rmstComparison
  title: "RMST Pairwise Comparison"
  type: Table
  visible: (calculateRMST)
  columns:
    - name: comparison
      title: "Comparison"
      type: text
    - name: rmst_diff
      title: "RMST Difference"
      type: number
    - name: se
      title: "SE"
      type: number
    - name: p_value
      title: "P-value"
      type: number
      format: zto,pvalue
```

**2. Add to .b.R:**
```r
if (self$options$calculateRMST) {
    rmst_results <- stagemigration_calculateRMST(
        data = data,
        old_stage = old_stage,
        new_stage = new_stage,
        time_var = time_var,
        event_var = "event_binary",
        tau = NULL,  # Auto-select
        checkpoint_callback = private$.checkpoint
    )

    if (!is.null(rmst_results$error)) {
        self$results$rmstResults$setNote("error", rmst_results$error)
        return()
    }

    # Populate RMST by stage
    for (stage in names(rmst_results$rmst_by_old_stage)) {
        stage_result <- rmst_results$rmst_by_old_stage[[stage]]
        if (!is.null(stage_result$rmst)) {
            self$results$rmstResults$addRow(rowKey=paste0("old_", stage), values=list(
                staging_system = "Old Staging",
                stage = stage,
                rmst = stage_result$rmst,
                se = stage_result$se,
                ci_lower = stage_result$lower,
                ci_upper = stage_result$upper
            ))
        }
    }

    for (stage in names(rmst_results$rmst_by_new_stage)) {
        stage_result <- rmst_results$rmst_by_new_stage[[stage]]
        if (!is.null(stage_result$rmst)) {
            self$results$rmstResults$addRow(rowKey=paste0("new_", stage), values=list(
                staging_system = "New Staging",
                stage = stage,
                rmst = stage_result$rmst,
                se = stage_result$se,
                ci_lower = stage_result$lower,
                ci_upper = stage_result$upper
            ))
        }
    }

    # Populate pairwise comparison
    if (!is.null(rmst_results$pairwise_comparison)) {
        # Add comparison rows...
    }
}
```

---

### Cutpoint Analysis

| Component | Status | Location |
|-----------|--------|----------|
| **.a.yaml option** | ✅ `performOptimalCutpoint` (line 828) | Exists |
| **.a.yaml method** | ✅ `cutpointMethod` (line 850) | Exists |
| **.a.yaml var** | ✅ `continuousStageVariable` (line 839) | Exists |
| **.u.yaml control** | ✅ CheckBox + ComboBox | Exists |
| **.b.R usage** | ❌ Not implemented | **IMPLEMENT:** |
| **Utility function** | ✅ `stagemigration_cutpointAnalysis()` | **stagemigration-competing-risks.R** |
| **.r.yaml output** | ⚠️ **MISSING** - Need to add | **ADD NEW TABLES:** |

**Required Changes:**

**1. Add to .r.yaml:**
```yaml
# Cutpoint Analysis
- name: cutpointExplanation
  title: Understanding Optimal Cutpoint Analysis
  type: Html
  visible: (performOptimalCutpoint && showExplanations)

- name: cutpointResults
  title: "Optimal Cutpoint Results"
  type: Table
  visible: (performOptimalCutpoint)
  columns:
    - name: method
      title: "Method"
      type: text
    - name: cutpoint
      title: "Optimal Cutpoint"
      type: number
    - name: statistic
      title: "Test Statistic"
      type: number
    - name: p_value
      title: "P-value"
      type: number
      format: zto,pvalue
    - name: interpretation
      title: "Interpretation"
      type: text
```

**2. Add to .b.R:**
```r
if (self$options$performOptimalCutpoint) {
    continuous_var <- self$options$continuousStageVariable
    method <- self$options$cutpointMethod %||% "maxstat"

    if (is.null(continuous_var) || continuous_var == "") {
        self$results$cutpointResults$setNote("error",
            "Please select a continuous stage variable")
        return()
    }

    cutpoint_results <- stagemigration_cutpointAnalysis(
        data = data,
        stage_var = continuous_var,
        time_var = time_var,
        event_var = "event_binary",
        method = method
    )

    if (!is.null(cutpoint_results$error)) {
        self$results$cutpointResults$setNote("error", cutpoint_results$error)
    } else {
        self$results$cutpointResults$addRow(rowKey=method, values=list(
            method = cutpoint_results$method,
            cutpoint = cutpoint_results$optimal_cutpoint,
            statistic = cutpoint_results$statistic,
            p_value = cutpoint_results$p_value,
            interpretation = cutpoint_results$interpretation
        ))
    }
}
```

---

## 🔧 Utility Function Integration Checklist

### In .b.R File Header

**Add after roxygen documentation:**
```r
#' @include stagemigration-utils.R
#' @include stagemigration-validation.R
#' @include stagemigration-discrimination.R
#' @include stagemigration-competing-risks.R
```

---

### In .run() Method - Data Validation Section

**Replace lines ~900-1100 (scattered validation) with:**
```r
# ========================================================================
# STEP 1: COMPREHENSIVE DATA VALIDATION
# ========================================================================

validation <- stagemigration_validateData(self$data, self$options)

if (!validation$valid) {
    # Display validation errors to user
    error_html <- "<div style='color: red; font-weight: bold;'>"
    error_html <- paste0(error_html, "<h3>Data Validation Errors:</h3><ul>")
    for (error in validation$errors) {
        error_html <- paste0(error_html, "<li>", error, "</li>")
    }
    error_html <- paste0(error_html, "</ul></div>")

    if (!is.null(validation$warnings) && length(validation$warnings) > 0) {
        error_html <- paste0(error_html, "<h4>Warnings:</h4><ul>")
        for (warning in validation$warnings) {
            error_html <- paste0(error_html, "<li>", warning, "</li>")
        }
        error_html <- paste0(error_html, "</ul>")
    }

    # Show in welcome message or dedicated error output
    self$results$welcomeMessage$setContent(error_html)
    self$results$welcomeMessage$setVisible(TRUE)
    return()
}

# Validation passed - use validated data
data <- validation$data
metadata <- validation$metadata

# Extract validated metadata for later use
old_stage_levels <- metadata$old_stage_levels
new_stage_levels <- metadata$new_stage_levels
sample_adequacy <- metadata$sample_adequacy
data_quality <- metadata$data_quality

# Show data quality summary if enabled
if (self$options$showExplanations && data_quality$overall_quality != "GOOD") {
    quality_html <- paste0("<div style='background: #fff3cd; padding: 10px;'>",
                          "<h4>Data Quality: ", data_quality$overall_quality, "</h4>")
    for (warning in data_quality$warnings) {
        quality_html <- paste0(quality_html, "<p>", warning, "</p>")
    }
    quality_html <- paste0(quality_html, "</div>")
    # Add to appropriate result output
}
```

---

### Formula Building - Replace Manual Construction

**Replace all instances of:**
```r
# OLD:
old_formula <- as.formula(paste("Surv(", time_var, ", event_binary) ~ ", old_stage))
```

**With:**
```r
# NEW:
old_formula <- stagemigration_buildFormula(
    time_var = self$options$survivalTime,
    event_var = "event_binary",
    predictors = old_stage
)
```

---

### Variable Escaping - Use Consistently

**Anywhere a variable name is used in formulas or data access:**
```r
# Use escaping helper
safe_var <- stagemigration_escapeVar(var_name)

# For data frame access with special characters:
data[[stagemigration_escapeVar(var_name, backticks = FALSE)]]
```

---

## 📐 .r.yaml Output Verification

### Checklist: Ensure Each Option Has Corresponding Output

| .a.yaml Option | .r.yaml Output | Status | Action |
|---------------|---------------|--------|--------|
| `showMigrationOverview` | `migrationOverview` | ✅ Exists | None |
| `showMigrationMatrix` | `migrationMatrix` | ✅ Exists | None |
| `showStageDistribution` | `stageDistribution` | ✅ Exists | None |
| `showStatisticalComparison` | `statisticalComparison` | ✅ Exists | None |
| `showConcordanceComparison` | `concordanceComparison` | ✅ Exists | None |
| `calculateNRI` | `nriResults` | ✅ Exists | None |
| `calculateIDI` | `idiResults` | ✅ Exists | None |
| `performBootstrap` | (integrated in concordance) | ✅ OK | None |
| `performROCAnalysis` | `rocComparison`, `rocPlots` | ✅ Exists | None |
| `performDCA` | `dcaResults`, `dcaPlots` | ✅ Exists | None |
| `performCalibration` | `calibrationResults` | ✅ Exists | None |
| `showSurvivalCurves` | `survivalCurves` (Image) | ✅ Exists | None |
| `showMigrationHeatmap` | `migrationHeatmap` (Image) | ✅ Exists | None |
| `advancedMigrationAnalysis` | `willRogersAnalysis` | ✅ Exists | None |
| `showWillRogersVisualization` | `willRogersPlots` | ✅ Exists | None |
| `enableMultifactorialAnalysis` | `multifactorialResults` | ✅ Exists | None |
| `performCompetingRisks` | **Missing** | ❌ **ADD** | See above |
| `calculateRMST` | **Missing** | ❌ **ADD** | See above |
| `performOptimalCutpoint` | **Missing** | ❌ **ADD** | See above |
| `generateCopyReadyReport` | `copyReadyReport` | ✅ Exists | None |
| `showExplanations` | Various `*Explanation` items | ✅ Exists | None |

---

## ⚙️ Constants Usage

**Throughout .b.R, replace magic numbers with constants:**

```r
# OLD:
if (n_events < 10) { ... }
if (event_rate < 0.05) { ... }
if (c_index < 0.6) { ... }

# NEW:
if (n_events < STAGEMIGRATION_CONSTANTS$MIN_EVENTS_CRITICAL) { ... }
if (event_rate < STAGEMIGRATION_CONSTANTS$MIN_EVENT_RATE) { ... }
if (c_index < STAGEMIGRATION_CONSTANTS$CINDEX_POOR) { ... }
```

---

## 🚦 Integration Priority

### Phase 1 (Critical - Week 1)
1. ✅ Add `complexityMode` to .a.yaml
2. ✅ Add utility function source/includes to .b.R
3. ✅ Replace data validation with `stagemigration_validateData()`
4. ✅ Replace C-index code with `stagemigration_calculateConcordance()`
5. ✅ Replace NRI code with `stagemigration_calculateNRI()`
6. ✅ Replace IDI code with `stagemigration_calculateIDI()`

### Phase 2 (Important - Week 2)
7. ✅ Add competing risks outputs to .r.yaml
8. ✅ Implement competing risks in .b.R
9. ✅ Add RMST outputs to .r.yaml
10. ✅ Implement RMST in .b.R
11. ✅ Add cutpoint outputs to .r.yaml
12. ✅ Implement cutpoint in .b.R

### Phase 3 (Polish - Week 3)
13. ✅ Replace all variable escaping with helper
14. ✅ Replace all magic numbers with constants
15. ✅ Add complexity mode logic throughout .b.R
16. ✅ Update .u.yaml with reorganized version
17. ✅ Test all dataflows end-to-end

---

## 🧪 Testing Checklist

After integration:

- [ ] All options in .a.yaml have corresponding .u.yaml controls
- [ ] All .u.yaml controls reference valid .a.yaml options
- [ ] All .b.R option access matches .a.yaml names exactly
- [ ] All result populations in .b.R match .r.yaml item names
- [ ] Utility functions called with correct arguments
- [ ] Labelled data (SPSS) converts correctly
- [ ] Variables with spaces/special characters work
- [ ] Competing risks analysis runs without errors
- [ ] RMST analysis produces valid results
- [ ] Cutpoint analysis identifies optimal cutpoints
- [ ] Complexity modes hide/show appropriate sections
- [ ] Bootstrap validation completes successfully
- [ ] All tables populate with correct data types
- [ ] No crashes with minimal data (n=30)
- [ ] Performance acceptable with large data (n=1000)

---

## 📞 Support

**Questions?** This integration map should guide the complete refactoring.

**Key Files:**
- `.a.yaml` - Options definition
- `.u.yaml` - UI controls (use reorganized version)
- `.b.R` - Backend logic (main integration work)
- `.r.yaml` - Result outputs (add missing items)
- `*-utils.R`, `*-validation.R`, `*-discrimination.R`, `*-competing-risks.R` - Utility modules

**Next Step:** Create integration script that systematically updates .b.R

---

**End of Integration Map**
