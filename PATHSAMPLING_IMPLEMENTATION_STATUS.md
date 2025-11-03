# Pathsampling Improvements - Implementation Status

**Date:** 2025-10-31
**Version:** 2.0.0 (In Progress)
**Status:** Phase 1 Complete - UI & Options Ready

---

## ✅ COMPLETED

### 1. Analysis Options Added (pathsampling.a.yaml)

**Critical Improvements:**
- ✅ `showHeterogeneityTest` - Test for different detection probabilities across groups
- ✅ `useGeometricCI` - Use theoretical CI when bootstrap ceiling detected
- ✅ `ciMethod` - Choose CI calculation method (auto/bootstrap/geometric/both)

**Important Improvements:**
- ✅ `showModelFit` - Goodness-of-fit test for model validation
- ✅ `showObsPred` - Observed vs predicted comparison table
- ✅ `showMarginalInterpretation` - Cost-benefit analysis of marginal gains

**User-Requested Features:**
- ✅ `appendVariables` - Add calculated variables to dataset
- ✅ `appendPrefix` - Customizable prefix for appended variables (default: "ps_")
- ✅ `autoDetectHeterogeneity` - Auto-detect and warn about mixed populations

**Status:** All 9 new options added with backward compatibility (all default to FALSE or safe defaults)

---

### 2. UI Elements Added (pathsampling.u.yaml)

**New Collapse Box:** "Model Validation & Enhancements"

**Sections:**
- ✅ Confidence Interval Method (dropdown + checkbox)
- ✅ Model Assessment (3 checkboxes)
- ✅ Heterogeneity Analysis (2 checkboxes)
- ✅ Data Export Options (checkbox + text input for prefix)

**Status:** UI complete and integrated

---

## 🔄 IN PROGRESS

### 3. Results Definitions (pathsampling.r.yaml)

**Need to Add:**

```yaml
# Heterogeneity test results
- name: heterogeneityTest
  title: Heterogeneity Test
  type: Table
  visible: (showHeterogeneityTest)
  clearWith:
    - sampleType
    - firstDetection
  columns:
    - name: test_statistic
      title: χ² Statistic
      type: number
    - name: df
      title: df
      type: integer
    - name: p_value
      title: p-value
      type: number
      format: zto,pvalue
    - name: interpretation
      title: Interpretation
      type: text

# Model fit assessment
- name: modelFitTest
  title: Model Fit Assessment
  type: Table
  visible: (showModelFit)
  columns:
    - name: chi_sq
      title: χ²
      type: number
    - name: df
      title: df
      type: integer
    - name: p_value
      title: p-value
      type: number
      format: zto,pvalue
    - name: fit_quality
      title: Model Fit
      type: text

# Observed vs Predicted
- name: obsPredTable
  title: Observed vs Predicted Detection
  type: Table
  visible: (showObsPred)
  rows: (maxSamples)
  columns:
    - name: n_samples
      title: Samples
      type: integer
    - name: observed
      title: Observed
      type: number
      format: pc
    - name: predicted
      title: Predicted
      type: number
      format: pc
    - name: difference
      title: Difference
      type: number
      format: pc
    - name: assessment
      title: Assessment
      type: text
```

**Status:** Specification ready, needs implementation

---

### 4. Backend Implementation (pathsampling.b.R)

**Need to Implement:**

**A. Helper Functions:**

```r
# 1. Heterogeneity testing
.testHeterogeneity <- function(first_detection, groups) {
  # Likelihood ratio test
  # H0: single q for all groups
  # H1: separate q for each group

  # Calculate pooled q (null model)
  q_pooled <- 1 / mean(first_detection, na.rm = TRUE)
  ll_null <- sum(dgeom(first_detection - 1, q_pooled, log = TRUE), na.rm = TRUE)

  # Calculate group-specific q (alternative model)
  ll_alt <- 0
  for (group in unique(groups)) {
    group_data <- first_detection[groups == group & !is.na(first_detection)]
    if (length(group_data) > 0) {
      q_group <- 1 / mean(group_data)
      ll_alt <- ll_alt + sum(dgeom(group_data - 1, q_group, log = TRUE))
    }
  }

  # LR statistic
  lr_stat <- 2 * (ll_alt - ll_null)
  df <- length(unique(groups)) - 1
  p_value <- pchisq(lr_stat, df, lower.tail = FALSE)

  list(
    statistic = lr_stat,
    df = df,
    p_value = p_value,
    interpretation = ifelse(p_value < 0.05,
                           "Significant heterogeneity detected",
                           "No significant heterogeneity")
  )
}

# 2. Geometric CI calculation
.calculateGeometricCI <- function(n_samples, q_mle, q_ci_lower, q_ci_upper) {
  # Calculate cumulative detection probability CI using geometric model
  # P(detect in ≤n) = 1 - (1-q)^n

  prob_point <- 1 - (1 - q_mle)^n_samples
  prob_lower <- 1 - (1 - q_ci_lower)^n_samples
  prob_upper <- 1 - (1 - q_ci_upper)^n_samples

  list(
    point = prob_point,
    lower = prob_lower,
    upper = prob_upper
  )
}

# 3. Model fit test
.testModelFit <- function(first_detection, q_estimate) {
  # Chi-square goodness of fit

  # Observed frequencies
  first_vals <- sort(unique(first_detection[!is.na(first_detection)]))
  observed <- sapply(first_vals, function(x) sum(first_detection == x, na.rm = TRUE))

  # Expected frequencies
  n_total <- length(first_detection[!is.na(first_detection)])
  expected <- sapply(first_vals, function(x) {
    n_total * dgeom(x - 1, q_estimate)
  })

  # Chi-square
  chi_sq <- sum((observed - expected)^2 / expected)
  df <- length(first_vals) - 1  # -1 for estimated parameter
  p_value <- pchisq(chi_sq, df, lower.tail = FALSE)

  list(
    chi_sq = chi_sq,
    df = df,
    p_value = p_value,
    fit_quality = ifelse(p_value > 0.05, "Good fit", "Poor fit")
  )
}

# 4. Observed vs Predicted
.calculateObsPred <- function(first_detection, q_estimate, max_samples) {
  results <- data.frame(
    n_samples = 1:max_samples,
    observed = NA_real_,
    predicted = NA_real_,
    difference = NA_real_,
    assessment = NA_character_
  )

  n_positive <- sum(!is.na(first_detection))

  for (i in 1:max_samples) {
    # Observed
    n_detected_by_i <- sum(first_detection <= i, na.rm = TRUE)
    obs <- n_detected_by_i / n_positive

    # Predicted (geometric)
    pred <- 1 - (1 - q_estimate)^i

    # Difference
    diff <- obs - pred

    # Assessment
    abs_diff <- abs(diff)
    assess <- ifelse(abs_diff < 0.05, "✅ Good fit",
                    ifelse(abs_diff < 0.10, "⚠️ Fair fit",
                           "❌ Poor fit"))

    results[i, ] <- list(i, obs, pred, diff, assess)
  }

  results
}

# 5. Append variables to data
.appendCalculatedVariables <- function(data, q_estimate, recommended_samples,
                                      first_detection, prefix = "ps_") {
  # Add cumulative probabilities
  for (i in 1:10) {  # Up to 10 samples
    var_name <- paste0(prefix, "cumulative_prob_", i)
    data[[var_name]] <- 1 - (1 - q_estimate)^i
  }

  # Add detection categories
  detection_cat <- ifelse(is.na(first_detection), "Negative",
                         ifelse(first_detection <= 2, "Early",
                                ifelse(first_detection <= 5, "Standard", "Late")))
  data[[paste0(prefix, "detection_category")]] <- factor(detection_cat)

  # Add recommended samples (constant for all cases)
  data[[paste0(prefix, "recommended_samples")]] <- recommended_samples

  # Add detected by N
  for (i in c(3, 5, 7, 10)) {
    var_name <- paste0(prefix, "detected_by_", i)
    data[[var_name]] <- !is.na(first_detection) & first_detection <= i
  }

  return(data)
}
```

**B. Integration in .run() method:**

```r
# After calculating q_estimate and bootstrap CI...

# HETEROGENEITY TEST
if (self$options$showHeterogeneityTest && !is.null(sampleTypeData)) {
  het_result <- private$.testHeterogeneity(firstDetectionData, sampleTypeData)

  heterogeneityTest <- self$results$heterogeneityTest
  heterogeneityTest$addRow(rowKey = "test", values = list(
    test_statistic = het_result$statistic,
    df = het_result$df,
    p_value = het_result$p_value,
    interpretation = het_result$interpretation
  ))
}

# IMPROVED CI METHOD
if (self$options$useGeometricCI && self$options$ciMethod == "auto") {
  # Check for ceiling effect
  if (bootstrapCIUpper >= 0.999 && bootstrapCILower >= 0.999) {
    # Use geometric CI instead
    geometric_ci <- private$.calculateGeometricCI(
      n_samples = bootstrapTargetIdx,
      q_mle = pEstimate,
      q_ci_lower = # calculate q CI from bootstrap
      q_ci_upper = # calculate q CI from bootstrap
    )
    # Update CI values with note
  }
}

# MODEL FIT TEST
if (self$options$showModelFit) {
  fit_result <- private$.testModelFit(firstDetectionData, pEstimate)

  modelFitTest <- self$results$modelFitTest
  modelFitTest$addRow(rowKey = "fit", values = list(
    chi_sq = fit_result$chi_sq,
    df = fit_result$df,
    p_value = fit_result$p_value,
    fit_quality = fit_result$fit_quality
  ))
}

# OBS VS PRED TABLE
if (self$options$showObsPred) {
  obs_pred <- private$.calculateObsPred(firstDetectionData, pEstimate, maxSamp)

  obsPredTable <- self$results$obsPredTable
  for (i in 1:nrow(obs_pred)) {
    obsPredTable$addRow(rowKey = paste0("n_", i), values = as.list(obs_pred[i, ]))
  }
}

# APPEND VARIABLES
if (self$options$appendVariables) {
  # Get original data
  data_with_vars <- private$.appendCalculatedVariables(
    data = self$data,
    q_estimate = pEstimate,
    recommended_samples = bootstrapTargetIdx,
    first_detection = firstDetectionData,
    prefix = self$options$appendPrefix
  )

  # Return modified data (jamovi will add columns)
  # This requires special handling in jamovi
}
```

**Status:** Partially implemented - needs completion and testing

---

## ⏳ TODO

### Immediate (Needed for Release):

1. **Add result definitions to pathsampling.r.yaml**
   - [ ] heterogeneityTest table
   - [ ] modelFitTest table
   - [ ] obsPredTable table
   - [ ] heterogeneityText html output
   - [ ] modelFitText html output

2. **Implement backend functions in pathsampling.b.R**
   - [ ] `.testHeterogeneity()` function
   - [ ] `.calculateGeometricCI()` function
   - [ ] `.testModelFit()` function
   - [ ] `.calculateObsPred()` function
   - [ ] `.appendCalculatedVariables()` function
   - [ ] Integration in `.run()` method
   - [ ] HTML outputs for new analyses

3. **Update marginal benefit table**
   - [ ] Add "Cost-Benefit" column when `showMarginalInterpretation = TRUE`
   - [ ] Add interpretation text below table

4. **Auto-detect heterogeneity**
   - [ ] Calculate CV of q across groups
   - [ ] Add warning if CV > 0.30
   - [ ] Report composition in clinical summary

5. **Testing**
   - [ ] Test with omentum data (mixed population)
   - [ ] Test with homogeneous data
   - [ ] Test with small sample (n < 20)
   - [ ] Test appendVariables functionality
   - [ ] Verify backward compatibility

---

### Future Enhancements:

- [ ] Sample size planning calculator
- [ ] Diagnostic plots (residuals, Q-Q plot)
- [ ] Enhanced context-specific summaries
- [ ] Bayesian analysis option

---

## TESTING PLAN

### Test Dataset 1: Omentum (Mixed Population)
```r
# Should trigger heterogeneity warning
# Should use geometric CI (bootstrap ceiling)
# Should show good model fit for microscopic-only
```

### Test Dataset 2: Homogeneous Population
```r
# Should pass heterogeneity test
# Bootstrap CI should work well
# Model fit should be excellent
```

### Test Dataset 3: Small Sample (n=15)
```r
# Should warn about small sample
# Geometric CI more stable than bootstrap
# Wide confidence intervals expected
```

---

## BACKWARD COMPATIBILITY VERIFICATION

**Test Checklist:**
- [ ] Existing .omv files produce identical output with new version
- [ ] All new options default to FALSE (no breaking changes)
- [ ] No changes to existing table/plot outputs
- [ ] Performance not degraded

---

## DOCUMENTATION UPDATES NEEDED

1. **Function help:**
   - [ ] Document new options in pathsampling help
   - [ ] Add examples using new features
   - [ ] Update vignettes

2. **NEWS.md:**
   - [ ] Document version 2.0.0 changes
   - [ ] List new features
   - [ ] Note backward compatibility

---

## ESTIMATED TIME TO COMPLETION

- Results definitions: 1 hour
- Backend implementation: 4-6 hours
- Testing & debugging: 2-3 hours
- Documentation: 1-2 hours

**Total: 8-12 hours**

---

## SUMMARY

**Completed:**
- ✅ All 9 new options defined in .a.yaml
- ✅ UI elements added in .u.yaml
- ✅ Improvement plan documented

**Next Steps:**
1. Add results definitions to .r.yaml
2. Implement backend functions in .b.R
3. Test thoroughly with multiple datasets
4. Update documentation

**Status:** Ready for phase 2 implementation

---

**Created:** 2025-10-31
**Author:** Pathsampling Development Team
**Version:** Draft 1.0
