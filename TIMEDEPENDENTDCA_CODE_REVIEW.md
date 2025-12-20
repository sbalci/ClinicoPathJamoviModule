# CODE REVIEW: `timedependentdca`

**Overall Quality**: ⭐⭐⭐⭐ (4/5)

**Maintainability**: HIGH
**Performance**: GOOD
**User Experience**: GOOD
**Mathematical/Statistical Correctness**: CORRECT with minor caveats
**Clinical & Release Readiness**: NEEDS_VALIDATION

---

## STRENGTHS

### 1. Excellent Notice System Implementation ✅
**Location**: [R/timedependentdca.b.R:76-100, 112-174, 195-306](R/timedependentdca.b.R#L76-L306)

- Comprehensive migration to modern jamovi Notices API
- 18 distinct notices covering all error/warning scenarios
- Single-line messages (no newlines) as required by API
- Auto-positioning by severity (ERROR→1, WARNING→50, INFO→999)
- Statistical validation guards (events < 10, censoring > 80%)
- Clear, actionable messages with specific guidance

**Example**:
```r
sprintf('Only %d events observed. Time-dependent decision curve analysis
requires at least 10 events for stable estimates. Results may be unreliable;
consider collecting more data or extending follow-up.', n_events)
```

### 2. Solid Mathematical Foundation ✅
**Location**: [R/timedependentdca.b.R:464-523](R/timedependentdca.b.R#L464-L523)

**Net Benefit Formula (Correctly Implemented)**:
```r
# Line 507: Net Benefit for Model
nb_model[i] <- tp_rate - fp_rate * (pt / (1 - pt))

# Line 511: Net Benefit for Treat All
nb_treat_all[i] <- event_rate - (1 - event_rate) * (pt / (1 - pt))
```

**TP/FP Rate Calculation (Valid Approach)**:
```r
# Lines 494-503: Using KM survival within high-risk group
surv_high_risk <- survfit(Surv(time[is_high_risk], event[is_high_risk]) ~ 1)
s_high_risk <- summary(surv_high_risk, times = t_eval, extend = TRUE)$surv[1]

tp_rate <- (1 - s_high_risk) * (n_high_risk / n)  # P(event by t | high risk) * P(high risk)
fp_rate <- s_high_risk * (n_high_risk / n)        # P(no event by t | high risk) * P(high risk)
```

✅ **Verification**: Matches standard DCA methodology for time-to-event outcomes

### 3. Robust Error Handling & Data Validation ✅
**Location**: [R/timedependentdca.b.R:192-307](R/timedependentdca.b.R#L192-L307)

- Time > 0 validation with count of invalid values
- Event coding (0/1) validation
- Missing data handling with informative statistics
- Time point parsing with example syntax
- Extrapolation warnings when time points exceed follow-up
- Statistical power checks (events, sample size, censoring)

### 4. Methodological Transparency ✅
**Location**: [R/timedependentdca.b.R:156-162](R/timedependentdca.b.R#L156-L162)

```r
'Net benefit estimates use Kaplan-Meier survival within risk groups at each threshold.
Inverse probability of censoring weighting (IPCW) and time-dependent censoring adjustments
are not implemented. Interpret with caution when censoring is heavy or informative.'
```

Honest disclosure of methodological limitations prevents misuse.

### 5. Flexible Risk Estimation Methods ✅
**Location**: [R/timedependentdca.b.R:365-460](R/timedependentdca.b.R#L365-L460)

Supports 3 methods with appropriate fallbacks:
1. **Direct**: Uses predictor as probability
2. **Kaplan-Meier**: Stratified KM by predictor quantiles
3. **Cox PH**: Cox regression predictions

---

## CRITICAL ISSUES

### ❌ ISSUE 1: Confidence Intervals Not Implemented
**Severity**: MAJOR (Clinical Release Blocker)
**Location**: [R/timedependentdca.b.R:339, 472-523](R/timedependentdca.b.R#L339)

**Problem**:
- `random_seed` parameter exists (Line 339) but is never used for bootstrapping
- No confidence intervals computed for net benefit estimates
- Tables show point estimates only ([Lines 592-598](R/timedependentdca.b.R#L592-L598))

**Clinical Impact**:
- Clinicians cannot assess uncertainty in net benefit estimates
- Unable to determine if differences between strategies are statistically meaningful
- Violates reporting guidelines for DCA (Vickers et al., 2016)

**Evidence**:
```r
# Line 339: Seed is set but not used
set.seed(self$options$random_seed)  # ❌ No bootstrap follows

# Lines 592-598: Table has no CI columns
row <- list(
    time_point = result$time_point,
    threshold = result$thresholds[idx],
    net_benefit = result$nb_model[idx],  # ❌ Point estimate only
    # Missing: nb_model_lower, nb_model_upper
)
```

**Recommended Fix**:
```r
# Add bootstrap CI calculation
.calculateBootstrapCI = function(n_boot = 1000) {
    boot_results <- matrix(NA, nrow = n_boot, ncol = length(thresholds))

    for (b in 1:n_boot) {
        boot_idx <- sample(1:n, n, replace = TRUE)
        boot_nb <- calculate_nb(time[boot_idx], event[boot_idx],
                                predictor[boot_idx], thresholds, t_eval)
        boot_results[b, ] <- boot_nb
    }

    ci_lower <- apply(boot_results, 2, quantile, probs = 0.025)
    ci_upper <- apply(boot_results, 2, quantile, probs = 0.975)

    return(list(lower = ci_lower, upper = ci_upper))
}
```

---

### ❌ ISSUE 2: "Direct" Probability Method Has No Validation
**Severity**: MAJOR (Patient Safety Risk)
**Location**: [R/timedependentdca.b.R:365-367](R/timedependentdca.b.R#L365-L367)

**Problem**:
```r
if (estimate_method == "direct") {
    prob_event <- predictor  # ❌ No validation that predictor is in [0,1]
}
```

**Clinical Impact**:
- If predictor is not a probability (e.g., risk score 0-100), results will be incorrect
- Net benefit calculations will be mathematically invalid
- Could lead to incorrect treatment decisions

**Recommended Fix**:
```r
if (estimate_method == "direct") {
    # Validate that predictor is probability-scaled
    if (any(predictor < 0, na.rm = TRUE) || any(predictor > 1, na.rm = TRUE)) {
        private$.addNotice(
            type = jmvcore::NoticeType$ERROR,
            content = sprintf('Direct probability method requires predictor in range [0,1]. Found min=%.3f, max=%.3f. Please scale predictor or use Kaplan-Meier/Cox estimation.',
                              min(predictor, na.rm=TRUE), max(predictor, na.rm=TRUE)),
            name = 'invalidProbabilityRange',
            position = 1
        )
        return(NULL)
    }
    prob_event <- predictor
}
```

---

### ⚠️ ISSUE 3: Incomplete Plot Implementation
**Severity**: MODERATE (Feature Incomplete)
**Location**: [R/timedependentdca.b.R:671, 682-712](R/timedependentdca.b.R#L682-L712)

**Problem**:
```r
plot_by_tp <- self$options$plot_by_timepoint  # Line 671: Option read but not used

if (!plot_by_tp) {
    # Only overlay plot implemented
    # ❌ No separate-plot-per-timepoint implementation
}
# Missing: else { ... separate plots ... }
```

**User Impact**: Option exists in UI but does nothing when enabled

**Recommended Fix**:
```r
if (!plot_by_tp) {
    # Current overlay implementation
} else {
    # Add separate plot per time point
    n_tp <- length(private$.results_by_time)
    par(mfrow = c(ceiling(n_tp/2), 2))

    for (tp_name in names(private$.results_by_time)) {
        result <- private$.results_by_time[[tp_name]]
        plot(result$thresholds, result$nb_model, type = "l",
             main = sprintf("Net Benefit at t = %s", tp_name),
             xlab = "Threshold Probability", ylab = "Net Benefit")
        # Add reference lines...
    }
    par(mfrow = c(1,1))  # Reset
}
```

---

## IMPROVEMENT OPPORTUNITIES

### 📊 ISSUE 4: Rainbow Colors Not Colorblind-Safe
**Severity**: MINOR (Accessibility)
**Location**: [R/timedependentdca.b.R:693, 734](R/timedependentdca.b.R#L693)

**Problem**:
```r
colors <- rainbow(length(private$.results_by_time))  # ❌ Not CB-safe
```

**Fix**: Use viridis or ColorBrewer palettes
```r
# Option 1: Viridis (if available)
if (requireNamespace("viridisLite", quietly = TRUE)) {
    colors <- viridisLite::viridis(length(private$.results_by_time))
} else {
    # Option 2: Manual CB-safe palette
    cb_palette <- c("#0072B2", "#E69F00", "#009E73", "#F0E442",
                    "#CC79A7", "#56B4E9", "#D55E00")
    colors <- rep(cb_palette, length.out = length(private$.results_by_time))
}
```

---

### 📊 ISSUE 5: Table Shows Too Many Rows (20 thresholds)
**Severity**: MINOR (UX)
**Location**: [R/timedependentdca.b.R:583](R/timedependentdca.b.R#L583)

**Problem**: With 3 time points, table has 60 rows (3 × 20) - overwhelming

**Recommendation**:
- Default to 5-10 representative thresholds
- Add option to control number of thresholds shown
- Consider summary table with only optimal threshold per time point

---

### 📈 ISSUE 6: Missing Clinical Interpretation Guidance
**Severity**: MODERATE (Clinical UX)

**Missing Features**:
1. No natural-language summary of results
2. No example interpretation sentences
3. No guidance on "clinically meaningful" net benefit thresholds
4. No comparison to published benchmarks

**Recommended Additions**:

```r
# Add to .r.yaml
- name: clinicalSummary
  title: "Clinical Interpretation"
  type: Html
  visible: (showClinicalGuidance)  # Controlled by UI checkbox

# Add to .b.R
.generateClinicalSummary = function() {
    optimal <- private$.results_by_time[[1]]$optimal_threshold
    max_nb <- private$.results_by_time[[1]]$max_net_benefit

    summary <- sprintf(
        "<h4>Clinical Summary</h4>
        <p>At the optimal threshold of <b>%.1f%%</b>, using this model provides
        a net benefit of <b>%.3f</b>, meaning approximately <b>%.0f additional
        true positives per 100 patients</b> compared to treating all patients.</p>
        <p><b>Interpretation:</b> %s</p>",
        optimal * 100, max_nb, max_nb * 100,
        if (max_nb > 0.10) "Strong clinical utility - model significantly improves decisions"
        else if (max_nb > 0.05) "Moderate clinical utility - model provides some benefit"
        else "Limited clinical utility - marginal improvement over default strategies"
    )

    return(summary)
}
```

---

## ENHANCEMENT SUGGESTIONS

### 🎯 Enhancement 1: Add Bootstrapped Confidence Intervals

**Priority**: HIGH (Required for publication)

```r
# Add to .a.yaml
- name: compute_ci
  title: "Compute Bootstrap Confidence Intervals"
  type: Bool
  default: false

- name: n_bootstrap
  title: "Number of Bootstrap Samples"
  type: Number
  min: 100
  max: 5000
  default: 1000
  visible: (compute_ci)

# Add to .r.yaml - update netBenefitTable columns
columns:
  # ... existing columns ...
  - name: nb_lower
    title: "NB (95% CI Lower)"
    type: number
    visible: (compute_ci)
  - name: nb_upper
    title: "NB (95% CI Upper)"
    type: number
    visible: (compute_ci)
```

---

### 🎯 Enhancement 2: Natural-Language Summary (Clinician-Friendly)

**Priority**: MODERATE

```yaml
# Add to .u.yaml
- type: CollapseBox
  label: "Output Options"
  collapsed: true
  children:
    - type: CheckBox
      name: showClinicalSummary
      label: "Show Clinical Summary (natural-language)"
    - type: CheckBox
      name: showInterpretationGuide
      label: "Show Interpretation Guide"

# Add to .r.yaml
- name: clinicalSummary
  title: "Clinical Summary"
  type: Html
  visible: (showClinicalSummary)
```

```r
# Add to .b.R
if (isTRUE(self$options$showClinicalSummary)) {
    summary_html <- sprintf("
        <h4>Plain-Language Summary</h4>
        <p>This analysis evaluated whether a prediction model improves clinical
        decision-making for %d patients with %d events (%.1f%% event rate) across
        %d time points.</p>

        <p><b>Key Finding:</b> At time point %.0f, the model achieves maximum net
        benefit of %.3f at a threshold probability of %.1f%%. This means the model
        correctly identifies approximately <b>%d additional true positive cases per
        100 patients</b> compared to treating everyone.</p>

        <p><b>Clinical Recommendation:</b> %s</p>
        ",
        n_obs, n_events, 100 * n_events / n_obs, n_timepoints,
        best_time_point, max_nb, optimal_threshold * 100,
        round(max_nb * 100),
        if (max_nb > 0.10) "Strong evidence for clinical use of this model"
        else if (max_nb > 0.05) "Model provides moderate benefit; consider local validation"
        else "Limited added value; traditional strategies may be sufficient"
    )

    self$results$clinicalSummary$setVisible(TRUE)
    self$results$clinicalSummary$setContent(summary_html)
}
```

---

### 🎯 Enhancement 3: Copy-Ready Report Sentences

**Priority**: MODERATE

```r
# Add to .r.yaml
- name: reportSentence
  title: "Report Sentence (copy-ready)"
  type: Preformatted
  visible: (showReportSentence)

# Add to .b.R
if (isTRUE(self$options$showReportSentence)) {
    report <- sprintf(
        "Time-dependent decision curve analysis was performed using %s estimation
        method across %d time points (%s). The optimal threshold probability was
        %.2f at time %.0f, yielding a maximum net benefit of %.3f (95%% CI: %.3f-%.3f).
        Compared to treating all patients, the model identified %d additional true
        positive cases per 100 patients at this threshold.",
        self$options$estimate_survival,
        n_timepoints,
        paste(private$.time_points, collapse = ", "),
        optimal_threshold,
        best_time_point,
        max_nb,
        ci_lower,  # From bootstrap
        ci_upper,
        round(max_nb * 100)
    )

    self$results$reportSentence$setContent(report)
}
```

---

## MATHEMATICAL/STATISTICAL CORRECTIONS

### ✅ Formula Verification

**Net Benefit** (Line 507):
```r
NB = TP_rate - FP_rate × (pt / (1 - pt))
```
✅ CORRECT - Matches Vickers & Elkin (2006) Medical Decision Making

**Treat All** (Line 511):
```r
NB_all = event_rate - (1 - event_rate) × (pt / (1 - pt))
```
✅ CORRECT

**Interventions Avoided** (Line 539):
```r
IA = (NB_model - NB_all) / (pt / (1 - pt)) × 100
```
✅ CORRECT - Standardized net reduction in interventions per 100 patients

### ✅ Time-Dependent Adaptation

Using KM survival within risk groups (Lines 494-503) is a valid approximation for time-dependent DCA. However:

**Limitations** (Already Disclosed in Notice):
- No IPCW adjustment
- Assumes non-informative censoring
- May be biased with heavy censoring (>80% - guarded by STRONG_WARNING)

**Alternative (More Rigorous)**:
Consider implementing IPCW-weighted estimates as optional enhancement:
```r
# Inverse Probability of Censoring Weighting
censoring_weights <- 1 / survfit(Surv(time, 1-event) ~ 1)$surv[match(time, times)]
weighted_tp_rate <- sum((1-event[is_high_risk]) * censoring_weights[is_high_risk]) / n
```

---

## CLINICAL & RELEASE READINESS

### Current Status: NEEDS_VALIDATION

**Ready for Clinical Use**: ❌ NO
**Ready for Research Use**: ⚠️ WITH CAVEATS

### Blockers for Clinical Release:

1. **❌ No confidence intervals** - Required by TRIPOD reporting guidelines
2. **❌ No external validation examples** - Need reference datasets with published results
3. **❌ Direct probability method unsafe** - Missing [0,1] validation
4. **⚠️ Incomplete feature** - plot_by_timepoint does nothing

### Required Before Clinical Release:

- [ ] Implement bootstrap confidence intervals
- [ ] Add validation against published DCA examples
- [ ] Validate direct probability input range
- [ ] Complete or remove plot_by_timepoint option
- [ ] Add clinical interpretation guidance
- [ ] Conduct simulation study for power (events vs net benefit precision)
- [ ] Document test cases with expected outputs
- [ ] Add unit tests for core calculations

### Validation Checklist:

```r
# Create test case matching published example
# Reference: Vickers et al. (2008) BMC Med Inform Decis Mak

test_data <- data.frame(
    time = c(...),  # From published dataset
    event = c(...),
    risk_score = c(...)
)

# Run analysis
result <- timedependentdca(
    data = test_data,
    time = 'time',
    event = 'event',
    predictor = 'risk_score',
    time_points = "365",
    estimate_survival = 'direct'
)

# Compare to published net benefit at pt=0.20
expected_nb <- 0.142  # From paper
calculated_nb <- result$nb_model[which.min(abs(result$thresholds - 0.20))]

stopifnot(abs(calculated_nb - expected_nb) < 0.01)
```

---

## CLINICIAN-FRIENDLY UX IMPROVEMENTS

### Clinician-Friendly UX & Explanations

| Area | Status | Priority |
|---|:---:|---|
| Plain-language labels/tooltips | ☐ | HIGH |
| Micro-explanations per option | ☑ | Done (.init) |
| Glossary entries present | ☐ | MODERATE |
| Guided flow (wizard) | ☐ | LOW |
| Misuse warnings/guards | ☑ | Done (Notices) |
| Example interpretations in outputs | ☐ | HIGH |
| Report sentence templates | ☐ | HIGH |
| Sensible defaults & presets | ☑ | Done |
| Accessibility (CB-safe, font) | ☐ | MODERATE |
| i18n (TR/EN) coverage | ☐ | LOW |
| Natural-language summary | ☐ | HIGH |
| About/How-to section | ☑ | Done (.init) |
| Caveats & assumptions panel | ☑ | Done (notices) |
| Guidance links/examples | ☐ | MODERATE |

### Recommended Additions:

#### 1. Tooltips for Options (HIGH Priority)

```yaml
# In .u.yaml
- type: VariablesListBox
  name: time
  maxItemCount: 1
  label: "Time Variable (days/months from baseline)"  # ← Clarify units

- type: VariablesListBox
  name: predictor
  maxItemCount: 1
  label: "Risk Predictor (0-1 probability or risk score)"  # ← Clarify expected range

- type: TextBox
  name: time_points
  label: "Time Points (comma-separated, e.g., '365, 730, 1095' for 1, 2, 3 years)"
```

#### 2. Glossary Panel (MODERATE Priority)

```yaml
# Add to .r.yaml
- name: glossary
  title: "Glossary of Terms"
  type: Html
  visible: (showGlossary)
```

```r
# Add to .b.R
glossary_html <- "
<h4>Key Terms</h4>
<dl>
  <dt><b>Net Benefit:</b></dt>
  <dd>A measure of clinical utility that weighs true positives against false positives,
      accounting for the relative harm of unnecessary treatment. Higher is better.</dd>

  <dt><b>Threshold Probability:</b></dt>
  <dd>The risk level at which you would recommend treatment. For example, 0.20 (20%) means
      you'd treat if the patient's risk exceeds 20%.</dd>

  <dt><b>Interventions Avoided:</b></dt>
  <dd>How many unnecessary treatments per 100 patients are prevented by using the model
      instead of treating everyone.</dd>

  <dt><b>Kaplan-Meier (KM):</b></dt>
  <dd>A method for estimating survival probabilities that accounts for censoring
      (patients lost to follow-up).</dd>
</dl>
"
```

#### 3. Example Interpretation Blocks (HIGH Priority)

```r
# Add below each key result table
interpretation_html <- sprintf("
<div style='background: #f0f8ff; padding: 10px; margin: 10px 0; border-left: 4px solid #0072B2;'>
  <b>Example Interpretation:</b> At the optimal threshold of %.1f%%, the model
  provides a net benefit of %.3f. This means that for every 100 patients, using
  this model instead of treating everyone would correctly identify %d additional
  patients who truly need treatment, while avoiding %d unnecessary treatments.
</div>
",
optimal_threshold * 100,
max_nb,
round(max_nb * 100),
round(interventions_avoided)
)
```

---

## ACTION ITEMS

### CRITICAL (Must Fix Before Release):
- [ ] **Implement bootstrap confidence intervals** ([Issue #1](#-issue-1-confidence-intervals-not-implemented))
- [ ] **Add [0,1] validation for direct probability method** ([Issue #2](#-issue-2-direct-probability-method-has-no-validation))
- [ ] **Complete plot_by_timepoint implementation** ([Issue #3](#-issue-3-incomplete-plot-implementation))
- [ ] **Create validation test suite with published reference data**

### HIGH Priority (Clinical UX):
- [ ] Add natural-language summary option ([Enhancement #2](#-enhancement-2-natural-language-summary-clinician-friendly))
- [ ] Add copy-ready report sentences ([Enhancement #3](#-enhancement-3-copy-ready-report-sentences))
- [ ] Add example interpretation blocks below tables
- [ ] Add tooltips with units/examples to all variable selectors

### MODERATE Priority (Quality):
- [ ] Replace rainbow colors with colorblind-safe palette ([Issue #4](#-issue-4-rainbow-colors-not-colorblind-safe))
- [ ] Reduce default number of thresholds shown in table ([Issue #5](#-issue-5-table-shows-too-many-rows-20-thresholds))
- [ ] Add glossary panel option
- [ ] Add "recommended" badges to default options

### LOW Priority (Enhancement):
- [ ] Implement IPCW-weighted estimates as advanced option
- [ ] Add guided wizard mode
- [ ] Add TR/EN internationalization
- [ ] Add clinical presets (e.g., "Cancer recurrence 1/2/3 years")

---

## PERFORMANCE OPTIMIZATION

Current performance is GOOD for typical datasets (n < 1000). For larger datasets:

**Potential Bottleneck** ([Line 494](R/timedependentdca.b.R#L494)):
```r
# Inside threshold loop - survfit called 100 times per time point
surv_high_risk <- survfit(Surv(time[is_high_risk], event[is_high_risk]) ~ 1)
```

**Optimization**:
```r
# Pre-compute survival curves for all possible risk groups
# Cache results indexed by n_high_risk
.cached_km <- list()

cache_key <- paste(sort(which(is_high_risk)), collapse = "_")
if (!is.null(.cached_km[[cache_key]])) {
    s_high_risk <- .cached_km[[cache_key]]
} else {
    surv_high_risk <- survfit(Surv(time[is_high_risk], event[is_high_risk]) ~ 1)
    s_high_risk <- summary(surv_high_risk, times = t_eval, extend = TRUE)$surv[1]
    .cached_km[[cache_key]] <- s_high_risk
}
```

---

## FINAL RECOMMENDATION

**Current Grade**: B+ (Good implementation with critical gaps)

**Clinical Release Readiness**: **NOT READY**

**Research Use Readiness**: **READY** with documented limitations

### Path to Clinical Release:

**Short-term (1-2 weeks)**:
1. Add bootstrap CI
2. Fix direct probability validation
3. Complete/remove incomplete features
4. Add clinical interpretation examples

**Medium-term (1 month)**:
5. Conduct external validation
6. Create test suite
7. Add natural-language summaries
8. Improve accessibility

**Long-term (Future versions)**:
9. Implement IPCW weighting
10. Add guided wizard
11. Internationalization

### Recommended Next Steps:

```bash
# 1. Create branch for CI implementation
git checkout -b feature/bootstrap-confidence-intervals

# 2. Implement bootstrap CIs (see Enhancement #1)
# 3. Add direct probability validation (see Issue #2)
# 4. Create test suite

# 5. Before merge:
Rscript -e "devtools::test()"
Rscript -e "jmvtools::check()"
```

---

**Review Date**: 2025-12-20
**Reviewer**: Claude Code AI
**Module Version**: 0.0.32
**Next Review**: After CI implementation
