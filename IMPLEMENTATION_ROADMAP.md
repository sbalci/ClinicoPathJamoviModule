# ClinicoPath Enhancement Implementation Roadmap

**Date Created:** 2025-11-03
**Strategic Vision:** Bridge "Clinico" (clinical outcomes) with "Path" (pathology/omics) domains

## Executive Summary

This document provides a detailed, actionable roadmap for implementing high-priority statistical enhancements to the ClinicoPath Jamovi Modules based on recent methodological advances published in Biostatistics, Journal of Statistical Software, Statistical Methods in Medical Research, and The R Journal.

---

## Implementation Status Overview

### Completed Features ✅
- Simon-Makuch Time-Dependent Survival Analysis (UI reorganized, single variable list)
- Git repository cleanup (README.html removed from history)
- Comprehensive module architecture audit
- **AFT Models for multisurvival** (2025-11-03) - 100% complete ⭐
  - ✅ 4 AFT options added to multisurvival.a.yaml (lines 789-856)
  - ✅ Complete results structure in multisurvival.r.yaml (lines 92-176)
  - ✅ `.calculate_aft()` function in multisurvival.b.R (163 lines, lines 1924-2087)
  - ✅ UI controls in multisurvival.u.yaml (lines 122-147)
  - ✅ Time Ratios with natural language interpretation
  - ✅ 5 distribution choices (Weibull, Exponential, Log-normal, Log-logistic, Gaussian)
  - ✅ Compiled successfully with jmvtools::prepare() and devtools::document()
- **SurvMetrics for multisurvival** (2025-11-03) - 100% complete ⭐
  - ✅ 4 SurvMetrics options added to multisurvival.a.yaml (lines 885-931)
  - ✅ Complete results structure in multisurvival.r.yaml (lines 179-249)
  - ✅ `.calculate_survmetrics()` function in multisurvival.b.R (241 lines, lines 2072-2312)
  - ✅ `.plotSurvMetrics()` plot function in multisurvival.b.R (lines 2314-2374)
  - ✅ UI controls in multisurvival.u.yaml (lines 148-176)
  - ✅ C-index with confidence intervals and interpretation
  - ✅ Brier Score at user-specified time points
  - ✅ Integrated Brier Score (IBS) with configurable resolution
  - ✅ Performance plots with ggplot2
  - ✅ Graceful degradation if SurvMetrics package not installed (uses fallback methods)
  - ✅ Comprehensive HTML summaries with educational content
  - ✅ Compiled successfully with jmvtools::prepare() and devtools::document()
- **Cure Fraction Models** (2025-11-03) - 100% complete ⭐
  - ✅ smcure integration (mixture cure models)
  - ✅ flexsurvcure integration (non-mixture cure models)
  - ✅ cuRe package integration with background mortality (lines 195-227 in curemodels.a.yaml)
  - ✅ npcure package integration for nonparametric estimation (lines 228-280 in curemodels.a.yaml)
  - ✅ Enhanced model_type with 5 choices (mixture, nonmixture, cure, npcure, all)
  - ✅ `.fitCuReModel()` function in curemodels.b.R (82 lines, lines 319-400)
  - ✅ `.fitNpCureModel()` function in curemodels.b.R (87 lines, lines 403-489)
  - ✅ `.formatCuReResults()` formatting function (58 lines, lines 621-678)
  - ✅ `.formatNpCureResults()` formatting function (60 lines, lines 681-740)
  - ✅ Updated `.run()` function to call new model types (lines 134-140)
  - ✅ Background mortality support via cuRe
  - ✅ Nonparametric flexible cure probability estimation via npcure
  - ✅ Comprehensive error handling with helpful suggestions
  - ✅ Compiled successfully with jmvtools::prepare() and devtools::document()
- **Recurrent Event Analysis (reReg)** (2025-11-03) - 100% complete ⭐
  - ✅ 8 reReg-specific options added to recurrentsurvival.a.yaml (lines 330-414)
  - ✅ 5 reReg model types: cox_LWYY, cox_GL, am_GL, am_LWYY, sc_XCYH
  - ✅ Standard error methods: bootstrap, sandwich, model-based
  - ✅ `.fitReRegModel()` function in recurrentsurvival.b.R (114 lines, lines 644-757)
  - ✅ `.formatReRegResults()` formatting function (85 lines, lines 760-844)
  - ✅ `.generateReRegPlots()` visualization function (35 lines, lines 847-882)
  - ✅ Updated `.performRecurrentAnalysis()` to call reReg when enabled (lines 214-216)
  - ✅ Event plot generation (reReg::plotEvents)
  - ✅ MCF (Mean Cumulative Function) plot generation
  - ✅ Educational content distinguishing recurrent events from competing risks
  - ✅ Comprehensive error handling with diagnostic suggestions
  - ✅ Compiled successfully with jmvtools::prepare() and devtools::document()

### Partially Implemented Features 🚧
(None currently - all priority features completed!)

### Priority Queue for Implementation 📋

---

## Phase 1: Quick Wins (1-2 months)

### 1.1 Complete Cure Fraction Models ✅ COMPLETED
**Priority Score:** 9.5/10
**Complexity:** Medium
**Current Status:** 100% Complete (2025-11-03)

**Completed Work:**
1. ✅ cuRe package integration for background mortality
2. ✅ npcure package for nonparametric cure estimation
3. ✅ Enhanced model selection with 5 model types

**Implementation Steps:**

```r
# In curemodels.b.R, add:

.fitCuReModel = function(data, time_var, status_var, predictors) {
  if (!requireNamespace('cuRe', quietly = TRUE)) {
    stop("cuRe package required. Install: install.packages('cuRe')")
  }

  # Prepare background mortality data
  # cuRe allows incorporating general population mortality rates
  fit <- cuRe::fit.cure.model(
    formula = Surv(time, status) ~ covariates,
    data = data,
    bhazard = "background_hazard_variable",  # If available
    dist = self$options$survival_dist
  )

  return(fit)
}

.fitNpCureModel = function(data, time_var, status_var, covariate) {
  if (!requireNamespace('npcure', quietly = TRUE)) {
    stop("npcure package required")
  }

  # Nonparametric cure probability estimation
  fit <- npcure::probcure(
    time = data[[time_var]],
    status = data[[status_var]],
    x = data[[covariate]],  # Single continuous covariate
    testimate = seq(0, max(data[[time_var]]), length.out = 100)
  )

  return(fit)
}
```

**Files to Modify:**
- `jamovi/curemodels.a.yaml` - Add options for cuRe (background mortality) and npcure
- `R/curemodels.b.R` - Add `.fitCuReModel()` and `.fitNpCureModel()` methods
- `jamovi/curemodels.u.yaml` - Add UI guidance for package selection

**Acceptance Criteria:**
- [x] cuRe integration with background mortality support
- [x] npcure nonparametric cure probability estimation
- [x] Enhanced model selection with 5 model types (mixture, nonmixture, cure, npcure, all)
- [x] Comprehensive output with cure fraction estimates and CIs
- [x] Error handling with helpful diagnostic suggestions
- [x] `.fitCuReModel()` and `.formatCuReResults()` functions implemented
- [x] `.fitNpCureModel()` and `.formatNpCureResults()` functions implemented
- [x] Module compiles successfully with `jmvtools::prepare()`
- [x] Documentation generated with `devtools::document()`

---

### 1.2 AFT Models for multisurvival ✅ COMPLETED
**Priority Score:** 6.5/10
**Complexity:** LOW
**Current Status:** 100% Complete (2025-11-03)

**Why AFT Models:**
- Intuitive interpretation: "Time Ratios" vs "Hazard Ratios"
- Alternative when PH assumption violated
- Natural language: "Treatment makes patients live 1.5x longer"

**Implementation Strategy:**

```yaml
# Add to multisurvival.a.yaml after line ~100:

- name: aft_model
  title: 'Use AFT Model (Alternative to Cox PH)'
  type: Bool
  default: false
  description:
    ui: >
      Use Accelerated Failure Time model instead of Cox Proportional Hazards.
      AFT models provide intuitive "time ratios" showing how covariates
      accelerate or decelerate survival time.
    R: >
      Fit AFT model using survival::survreg instead of Cox model

- name: aft_distribution
  title: 'AFT Distribution'
  type: List
  options:
    - title: Weibull
      name: weibull
    - title: Exponential
      name: exponential
    - title: Log-normal
      name: lognormal
    - title: Log-logistic
      name: loglogistic
  default: weibull
  enable: (aft_model)
  description:
    ui: >
      Distribution for survival times in AFT model.
      Weibull is most flexible (allows both PH and AFT interpretation).
    R: >
      Distribution family for AFT model
```

```r
# Add to multisurvival.b.R:

.fitAFTModel = function() {

  if (!self$options$aft_model) return()

  # Build formula
  formula_str <- paste0(
    "Surv(", self$options$elapsedtime, ", ",
    self$options$outcome, ") ~ ",
    paste(c(self$options$explanatory, self$options$contexpl), collapse = " + ")
  )

  # Fit AFT model
  aft_fit <- survival::survreg(
    formula = as.formula(formula_str),
    data = self$data,
    dist = self$options$aft_distribution %||% "weibull"
  )

  # Extract and format results
  coef_table <- summary(aft_fit)$table

  # Convert coefficients to Time Ratios (TR)
  time_ratios <- exp(coef_table[, "Value"])

  # Populate results table
  table <- self$results$aftTable
  for (i in seq_along(time_ratios)) {
    row <- list()
    row$variable <- rownames(coef_table)[i]
    row$coefficient <- coef_table[i, "Value"]
    row$timeRatio <- time_ratios[i]
    row$se <- coef_table[i, "Std. Error"]
    row$z <- coef_table[i, "z"]
    row$p <- coef_table[i, "p"]

    # Natural language interpretation
    if (time_ratios[i] > 1) {
      interpretation <- paste0("Decelerates time to event by factor of ",
                             round(time_ratios[i], 2))
    } else {
      interpretation <- paste0("Accelerates time to event by factor of ",
                             round(1/time_ratios[i], 2))
    }
    row$interpretation <- interpretation

    table$addRow(rowKey = i, values = row)
  }
}
```

**Files to Modify:**
- `jamovi/multisurvival.a.yaml` - Add AFT options
- `jamovi/multisurvival.r.yaml` - Add AFT results table
- `jamovi/multisurvival.u.yaml` - Add AFT checkbox and distribution selector
- `R/multisurvival.b.R` - Add `.fitAFTModel()` method

**Acceptance Criteria:**
- [x] AFT model option in UI
- [x] Distribution selection (Weibull, Exponential, Lognormal, Loglogistic, Gaussian)
- [x] Results table with Time Ratios and natural language interpretation
- [x] Module compiles successfully (jmvtools::prepare + devtools::document)
- [x] 4 options: use_aft, aft_distribution, aft_show_hr_equivalent, aft_show_interpretation
- [x] Natural language summary with model information (AIC, BIC, scale parameter)

---

### 1.3 Enhanced Survival Metrics (SurvMetrics) ✅ COMPLETED
**Priority Score:** 7.0/10
**Complexity:** LOW
**Current Status:** 100% Complete (2025-11-03)

**Package:** `SurvMetrics`

**Implementation:**

```r
# Add to multisurvival.b.R after Cox model fitting:

.calculateSurvivalMetrics = function(cox_fit, data) {

  if (!requireNamespace('SurvMetrics', quietly = TRUE)) {
    return()
  }

  # Extract predicted values
  pred_risk <- predict(cox_fit, type = "risk")
  pred_time <- predict(cox_fit, type = "expected")

  actual_time <- data[[self$options$elapsedtime]]
  actual_status <- data[[self$options$outcome]]

  # Calculate metrics
  metrics <- list()

  # Concordance Index (C-index) with tie handling
  metrics$cindex <- SurvMetrics::Cindex(
    Surv.rsp = Surv(actual_time, actual_status),
    Surv.rsp.new = NULL,
    lpnew = pred_risk,
    ties = "average"
  )

  # Brier Score at time points
  time_points <- quantile(actual_time[actual_status == 1],
                         probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  metrics$brier <- SurvMetrics::Brier(
    object = cox_fit,
    times = time_points
  )

  # Integrated Brier Score
  metrics$ibs <- SurvMetrics::IBS(
    object = cox_fit,
    times = seq(0, max(actual_time), length.out = 100)
  )

  # Mean Absolute Error for survival time
  metrics$mae <- mean(abs(actual_time - pred_time), na.rm = TRUE)

  # Populate metrics table
  table <- self$results$metricsTable
  table$addRow(rowKey = "cindex", values = list(
    metric = "Concordance Index (C-index)",
    value = metrics$cindex$cindex,
    ci_lower = metrics$cindex$lower,
    ci_upper = metrics$cindex$upper,
    interpretation = "Higher is better (0.5 = random, 1.0 = perfect)"
  ))

  table$addRow(rowKey = "ibs", values = list(
    metric = "Integrated Brier Score",
    value = metrics$ibs,
    interpretation = "Lower is better (0 = perfect prediction)"
  ))

  return(metrics)
}
```

**Files to Modify:**
- `R/multisurvival.b.R` - Add `.calculateSurvivalMetrics()` method
- `jamovi/multisurvival.r.yaml` - Add metrics table
- `jamovi/multisurvival.u.yaml` - Add checkbox "Show Model Performance Metrics"

**Acceptance Criteria:**
- [x] SurvMetrics options in multisurvival.a.yaml
- [x] Results structure with table and plots in multisurvival.r.yaml
- [x] `.calculate_survmetrics()` function implemented
- [x] `.plotSurvMetrics()` function implemented
- [x] UI controls with time points and IBS configuration
- [x] C-index with confidence intervals
- [x] Brier Score at configurable time points
- [x] Integrated Brier Score (IBS)
- [x] Performance plots (Brier Score over time)
- [x] Graceful package dependency handling
- [x] Educational HTML summaries
- [x] Module compiles with `jmvtools::prepare()`
- [x] Documentation generated with `devtools::document()`

---

## Phase 2: Moderate Complexity (3-6 months)

### 2.1 Recurrent Event Analysis (reReg) ✅ COMPLETED
**Priority Score:** 8.0/10
**Complexity:** Medium
**Current Status:** 100% Complete (2025-11-03)

**Check Status:**
```bash
ls jamovi/recurrentsurvival.* R/recurrentsurvival.*
```

**Key Features:**
- Joint modeling of recurrent events + terminal event
- Distinguishes from competing risks (educate users)
- Visualizations: Event plots, MCF plots

**Implementation Guide:**

```yaml
# recurrentsurvival.a.yaml structure:

options:
  - name: subject_id
    title: 'Subject ID'
    type: Variable

  - name: recurrent_time
    title: 'Recurrent Event Time'
    type: Variable

  - name: terminal_time
    title: 'Terminal Event Time'
    type: Variable

  - name: recurrent_status
    title: 'Recurrent Event Status'
    type: Variable

  - name: terminal_status
    title: 'Terminal Event Status'
    type: Variable

  - name: covariates
    title: 'Covariates'
    type: Variables
```

```r
# Main implementation:
.fitRecurrentModel = function() {

  # Prepare data for reReg
  reDat <- reReg::reSurv(
    time1 = self$data[[self$options$recurrent_time]],
    event = self$data[[self$options$recurrent_status]],
    terminal = self$data[[self$options$terminal_time]],
    id = self$data[[self$options$subject_id]]
  )

  # Fit joint model
  fit <- reReg::reReg(
    formula = reDat ~ covariates,
    data = self$data,
    model = "cox.LWYY",  # Shared frailty model
    se = "bootstrap"
  )

  # Generate event plot
  plotEvents(fit, id = 1:20)  # Show first 20 subjects

  # Generate MCF plot
  plot(fit, mcf = TRUE)
}
```

**Educational Note for UI:**
```
ℹ️ Recurrent Events vs. Competing Risks:
- Use RECURRENT EVENTS when: Same event can happen multiple times
  (e.g., hospital readmissions, tumor recurrences)
- Use COMPETING RISKS when: One of several different outcomes occurs
  (e.g., death from cancer vs. death from other causes)
```

---

### 2.2 Covariate-Adjusted ROC (tram)
**Priority Score:** 8.5/10
**Complexity:** Medium-High
**Current Status:** Not started

**Revolutionary Feature:** ROC curves as a function of covariates

**Implementation:**

```r
# New module: covariateROC.b.R

.fitTramModel = function() {

  if (!requireNamespace('tram', quietly = TRUE)) {
    stop("tram package required. Install from CRAN.")
  }

  # Fit transformation model
  tram_fit <- tram::Colr(
    formula = test_result ~ disease_status | covariates,
    data = self$data
  )

  # Calculate AUC across covariate values
  covariate_vals <- seq(
    min(self$data[[covariate_name]]),
    max(self$data[[covariate_name]]),
    length.out = 50
  )

  aucs <- sapply(covariate_vals, function(val) {
    newdata <- data.frame(covariate = val)
    pred <- predict(tram_fit, newdata = newdata, type = "distribution")
    # Calculate AUC from predicted distributions
    # ... (tram-specific AUC calculation)
  })

  # Plot: AUC vs Covariate Value
  plot(covariate_vals, aucs,
       xlab = "Covariate Value",
       ylab = "Area Under Curve",
       main = "Covariate-Adjusted ROC")
}
```

**Key Advantage:** Handles censored and ordinal test data natively.

---

## Phase 3: Advanced Features (6-12 months)

### 3.1 Causal Mediation Analysis
**Priority Score:** 8.5/10
**Complexity:** High
**Strategic Impact:** HIGHEST (enables "ClinicoPath" vision)

**Three-Tier Implementation:**

**Tier 1: Basic Mediation (mediation package)**
```r
# causalmediation.b.R
fit_med <- mediation::mediate(
  model.m = lm(mediator ~ treatment + covariates),
  model.y = lm(outcome ~ treatment + mediator + covariates),
  treat = "treatment",
  mediator = "mediator",
  boot = TRUE, sims = 1000
)

# Output: NDE, NIE, Total Effect
```

**Tier 2: Comprehensive Suite (CMAverse)**
- DAG visualization
- Multiple mediators
- Sensitivity analysis

**Tier 3: HIGH-DIMENSIONAL Mediation (hdmax2) 🌟 KILLER FEATURE**
```r
# When user selects entire omics dataset as mediators:
if (ncol(mediators) > 100) {
  # High-dimensional mediation
  fit <- hdmax2::hdmax2(
    Y = outcome,
    M = mediators_matrix,  # 50,000 methylation probes
    X = treatment,
    Z = covariates
  )

  # Automatic mediator selection
  # Output: Significant mediators + indirect effects
}
```

This enables questions like: "Which of 500,000 methylation sites mediate trauma → cortisol?"

---

### 3.2 Mendelian Randomization
**Priority Score:** 6.5/10
**Complexity:** Medium
**New User Base:** Genetic epidemiologists

**Data Type:** GWAS summary statistics (NOT individual-level data)

**UI:**
```
📁 Upload Exposure GWAS Summary (.csv)
📁 Upload Outcome GWAS Summary (.csv)

Column Mapping:
- SNP ID: [dropdown]
- Beta: [dropdown]
- Standard Error: [dropdown]
- P-value: [dropdown]
```

**Implementation:**
```r
mr_fit <- MendelianRandomization::mr_perform(
  MendelianRandomization::mr_input(
    bx = exposure_betas,
    bxse = exposure_se,
    by = outcome_betas,
    byse = outcome_se,
    snps = snp_ids
  ),
  methods = c("IVW", "MR-Egger", "Weighted median")
)

# Comprehensive sensitivity analysis
MendelianRandomization::mr_forest(mr_fit)
MendelianRandomization::mr_funnel(mr_fit)
```

---

## Implementation Priorities by Complexity vs. Impact

```
HIGH IMPACT, LOW COMPLEXITY (DO FIRST):
├─ AFT Models (6.5/10 priority, LOW complexity) ⭐ START HERE
├─ Complete Cure Models (9.5/10, MEDIUM complexity)
└─ SurvMetrics (7.0/10, LOW complexity)

HIGH IMPACT, MEDIUM COMPLEXITY (DO NEXT):
├─ Recurrent Events (8.0/10)
├─ Covariate ROC (8.5/10)
└─ Basic Causal Mediation (8.5/10)

HIGH IMPACT, HIGH COMPLEXITY (STRATEGIC):
├─ HD Mediation (omics integration) 🌟 FLAGSHIP
└─ Mendelian Randomization (6.5/10)
```

---

## Dependencies and Package Requirements

### Add to DESCRIPTION:
```
Suggests:
    smcure (>= 2.0),
    cuRe,
    npcure,
    flexcure,
    reReg (>= 2.0),
    SurvMetrics,
    tram,
    mediation,
    CMAverse,
    hdmax2,
    MendelianRandomization,
    RobustBF,
    RESI
```

---

## Testing Protocol

For each new feature:
1. Unit tests in `tests/testthat/test-{module}.R`
2. Example datasets in `data/`
3. Vignette: `vignettes/jsurvival-{XX}-{feature}-comprehensive.qmd`
4. Compile: `jmvtools::prepare()`
5. Document: `devtools::document()`
6. Check: `devtools::check()`

---

## Commit Strategy

Each feature should be a separate commit:
```bash
git add jamovi/{module}.*.yaml R/{module}.b.R
git commit -m "feat({module}): Add {feature} with {package}

- Implements {key capability}
- Adds {UI elements}
- Provides {output format}

Refs: {Journal citation}
Closes: #{issue}"
```

---

## Success Metrics

1. **User Adoption:** Track downloads of enhanced modules
2. **Citations:** Monitor citations of ClinicoPath in papers using new features
3. **Methodological Coverage:** % of recent Biostatistics/JSS methods implemented
4. **Differentiation:** Unique features vs. competitors (SPSS, Stata, R Commander)

---

## Strategic Vision Realization

**Current State:** Excellent clinical analysis (descriptives, Cox, ROC)
**Future State:** Complete ClinicoPath ecosystem

```
Clinical Data → Statistical Analysis → Omics Integration → Causal Inference
     ↓                    ↓                   ↓                  ↓
  tableone          multisurvival      HD Mediation         TSCI/MR
  crosstable        curemodels         tram ROC            CMAverse
  survival          recurrent events    hibayes
```

**Tagline:** "From Clinical Observations to Causal Mechanisms - All in One Click"

---

## Next Immediate Actions (This Week)

1. ✅ Complete Simon-Makuch UI reorganization (DONE)
2. ✅ Git cleanup - remove README.html (DONE)
3. ⏭️ Implement AFT Models in multisurvival (2-3 hours)
4. ⏭️ Complete cuRe + npcure integration in curemodels (3-4 hours)
5. ⏭️ Add SurvMetrics to multisurvival (2 hours)
6. 📝 Create implementation vignettes for developers

**Total Quick Wins:** 3 features, ~8-9 hours of focused development

---

**Document Version:** 1.0
**Last Updated:** 2025-11-03
**Maintainer:** Claude Code with ClinicoPath Development Team
