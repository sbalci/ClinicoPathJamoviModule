# pathsampling Statistical Methodology Review & Action Plan

**Date:** 2025-11-23
**Reviewer Critique:** Statistical methodology flaws
**Status:** CRITICAL - NOT PRODUCTION READY

---

## Executive Summary

This review identifies **4 critical statistical methodology flaws** in pathsampling that make it **NOT READY FOR CLINICAL USE** despite having mathematically correct implementations.

**Key Distinction:**
- ✅ **Mathematical correctness**: Beta-Binomial PMF formula is correct (validated in `test-pathsampling-statistical-validation.R`)
- ❌ **Statistical methodology**: How the function USES the formula is flawed (parameter estimation, selection bias, model assumptions)

---

## Critical Issues Identified

### 1. ❌ Selection Bias: Over-Optimistic Sensitivity Estimates

**Location:** R/pathsampling.b.R:1570-1576, 2108-2178, 4135-4169

**Problem:**
```r
# Bootstrap only resamples DETECTED positives
positiveCases <- !is.na(firstDetectionData)      # Only detected cases
positiveIndices <- which(positiveCases)
nPositiveCases <- length(positiveIndices)

for (iter in 1:nBoot) {
    bootIndices <- sample(positiveIndices, nPositiveCases, replace = TRUE)
    # ...
}
```

**Why this is wrong:**
- Assumes all true positives were detected
- Excludes any undetected positives (false negatives)
- Creates **upward bias** in sensitivity estimates
- Recommendations will suggest **too few samples** for desired confidence

**Clinical Impact:**
- Pathologists may under-sample based on biased recommendations
- Risk of missing lesions increases

**Example:**
```
True dataset: 100 cases, 80 detected, 20 missed (true sensitivity = 80%)
Bootstrap treats 80 detected as "universe" → estimates sensitivity ≈ 95%+
Clinical recommendations based on 95% will fail to achieve target confidence
```

### 2. ❌ Binomial Model Under Invalid Independence Assumption

**Location:** R/pathsampling.b.R:794-1080

**Problem:**
```r
# Line 808-816: Estimates p from pooled data
pEstimate <- total_positive_samples / total_samples_positive

# Lines 828-836: DETECTS heterogeneity and WARNS
if (cv > 0.5) {
    private$.empiricalHeterogeneity <- sprintf("⚠️ HIGH HETEROGENEITY...")
}

# BUT: Line 1000+ Still outputs recommendations table!
```

**Why this is wrong:**
- Binomial model assumes **independent** sampling (each sample has equal, independent probability p)
- Tumor/margin contexts violate this (clustered/dependent sampling)
- Code DETECTS violation and WARNS but **still outputs targets**

**Clinical Impact:**
- Recommendations appear in output even when model is invalid
- Users may not read/understand warnings
- Under-sampling in clustered disease contexts

**Current behavior:**
- ✅ Detects heterogeneity (CV > 0.5)
- ✅ Generates warning message
- ❌ Still populates recommendation table with priorities
- ❌ Users see numbers without understanding they're invalid

**What should happen:**
- Clear tables when independence is violated
- Force users to switch to bootstrap/empirical methods
- Only output binomial results when CV < 0.3

### 3. ❌ Beta-Binomial: Biased Parameter Estimation with Varying N_i

**Location:** R/pathsampling.b.R:2755-2875 (specifically lines 2802-2835)

**Problem:**
```r
# Line 2802-2806: UNWEIGHTED mean and variance
p <- successStatesData / totalPopulationData
p <- p[is.finite(p)]

mu <- mean(p, na.rm = TRUE)      # ❌ WRONG: Should be weighted
var <- var(p, na.rm = TRUE)      # ❌ WRONG: Should account for N_i

# Lines 2831-2835: Method-of-moments
term <- (mu * (1 - mu) / var) - 1
alpha <- mu * term
beta <- (1 - mu) * term
```

**Why this is wrong:**

For beta-binomial with **varying denominators N_i**, each proportion p_i = K_i/N_i has different precision:
- Large N_i → more precise estimate
- Small N_i → less precise estimate

**Unweighted mean** treats all p_i equally:
- p̄ = (p₁ + p₂ + ... + pₙ)/n
- Gives same weight to N₁=5 and N₁₀₀=100

**Correct weighted mean**:
- p̄ = Σ(N_i × p_i) / Σ(N_i) = Σ(K_i) / Σ(N_i)
- Automatically weights by precision

**Clinical Impact:**
- **Biased α/β estimates** → incorrect detection curves
- Small studies with extreme p_i can distort overall estimates
- May recommend too few or too many samples

**Example:**
```
Case 1: 1/5 = 0.20 (N=5, small sample)
Case 2: 50/100 = 0.50 (N=100, large sample)

CURRENT (unweighted): mean = (0.20 + 0.50)/2 = 0.35
CORRECT (weighted): mean = (1+50)/(5+100) = 51/105 = 0.486

Bias = -0.136 (underestimates p by 13.6%)
```

**Correct Implementation Options:**

**Option A: Weighted Method-of-Moments**
```r
# Weighted mean
N_total <- sum(totalPopulationData)
K_total <- sum(successStatesData)
mu <- K_total / N_total

# Weighted variance (complex - see below)
# ... proper N-weighted variance formula
```

**Option B: Use VGAM package (RECOMMENDED)**
```r
library(VGAM)
fit <- vglm(cbind(successStatesData, totalPopulationData - successStatesData) ~ 1,
            family = betabinomial, data = data.frame(...))
alpha <- exp(coef(fit)[1])
beta <- exp(coef(fit)[2])
```

### 4. ❌ Population vs Conditional Estimates Conflated

**Location:** R/pathsampling.b.R:2340-2380

**Problem:**
- Uses detected-case prevalence from same dataset
- Assumes no false negatives in prevalence calculation
- population_detection = prevalence × sensitivity
- But prevalence is estimated from same biased sample

**Clinical Impact:**
- Real-world detection rates overestimated
- Misleads clinicians about population-level performance

---

## Test Coverage Status

### ✅ What I Already Created

**File:** `tests/testthat/test-pathsampling-statistical-validation.R`

**Coverage:** 36 tests validating:
- Beta-Binomial PMF mathematical correctness ✅
- Cliff's Delta formula ✅
- Hodges-Lehmann estimator ✅
- Utility functions ✅
- Edge cases ✅

### ❌ What's Still Missing

**No tests for:**
- ❌ Parameter estimation methodology (weighted vs unweighted)
- ❌ Selection bias in bootstrap
- ❌ Model choice logic (binomial vs empirical)
- ❌ Clinical validation scenarios
- ❌ End-to-end integration tests

---

## Action Plan: Path to Production Readiness

### Phase 1: CRITICAL FIXES (Required for Release)

#### 1.1 Fix Beta-Binomial Parameter Estimation

**Priority:** CRITICAL
**Estimated Effort:** 2-4 hours

**Options:**
1. **Option A (Recommended):** Use VGAM for estimation
   - Add `VGAM` to `Imports` in DESCRIPTION
   - Replace lines 2802-2835 with `vglm()` call
   - Pro: Trusted, validated, proper MLE
   - Con: Additional dependency

2. **Option B:** Implement weighted MoM correctly
   - Weighted mean: `sum(K_i) / sum(N_i)`
   - Weighted variance (complex formula)
   - Pro: No new dependency
   - Con: More code to validate

**Implementation:**
```r
# Add to DESCRIPTION:
# Imports: ..., VGAM

# Replace lines 2802-2835:
if (requireNamespace("VGAM", quietly = TRUE)) {
    betaData <- data.frame(
        success = successStatesData,
        total = totalPopulationData
    )

    tryCatch({
        fit <- VGAM::vglm(
            cbind(success, total - success) ~ 1,
            family = VGAM::betabinomial,
            data = betaData
        )

        # Extract parameters
        coefs <- coef(fit)
        alpha <- exp(coefs[1])
        beta <- exp(coefs[2])

    }, error = function(e) {
        modelRejected <- TRUE
        betaBinomNotes <- c(betaBinomNotes,
            sprintf("❌ Beta-binomial fit failed: %s", e$message))
    })
} else {
    stop("VGAM package required for beta-binomial analysis")
}
```

#### 1.2 Address Selection Bias in Bootstrap/Empirical

**Priority:** CRITICAL
**Estimated Effort:** 4-6 hours

**Problem:** Bootstrap only uses detected positives

**Solution Options:**

**Option A: Add explicit warnings + require gold standard**
```r
# Before bootstrap (line 1570):
if (is.null(goldStandardPositives)) {
    warning_notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'bootstrapSelectionBias',
        type = jmvcore::NoticeType$STRONG_WARNING
    )
    warning_notice$setContent(
        '⚠️ SELECTION BIAS: Bootstrap uses only DETECTED positives.
        Sensitivity estimates assume 100% detection and will be OVEROPTIMISTIC.
        Provide gold-standard positive count for unbiased estimates, or
        interpret results as CONDITIONAL probability (given detection occurred).'
    )
    self$results$insert(2, warning_notice)
}
```

**Option B: Add option for gold-standard total positives**
```r
# Add to .a.yaml:
- name: goldStandardPositives
  title: Gold-Standard Total Positives (Optional)
  type: Integer
  description: |
    Total number of true positives (including undetected).
    If provided, enables unbiased sensitivity estimation.

# Modify bootstrap:
if (!is.null(self$options$goldStandardPositives)) {
    # Use gold standard denominator
    nTruePositives <- self$options$goldStandardPositives
    sensitivity <- nDetected / nTruePositives
} else {
    # Conditional estimate only
    # Add strong warning
}
```

#### 1.3 Enforce Model Choice Logic

**Priority:** HIGH
**Estimated Effort:** 2-3 hours

**Current:** Warns but still outputs invalid binomial recommendations

**Fix:**
```r
# After heterogeneity check (line 828):
if (cv > 0.5) {
    # HIGH heterogeneity - DISABLE binomial
    private$.empiricalHeterogeneity <- "..."

    # Clear binomial results
    binomialTable <- self$results$binomialTable
    binomialTable$clearRows()

    # Force user to alternative methods
    binomialText <- self$results$binomialText
    binomialText$setContent(sprintf(
        "<div style='...'>
            <p><strong>❌ Binomial Model Not Applicable</strong></p>
            <p>High heterogeneity (CV=%.2f) violates independence assumption.</p>
            <p><strong>Use instead:</strong> Bootstrap or Beta-Binomial analysis</p>
        </div>", cv
    ))

    return()  # Don't populate binomial table
}
```

### Phase 2: TEST COVERAGE (Required for Confidence)

#### 2.1 Parameter Estimation Tests

**Create:** `tests/testthat/test-pathsampling-parameter-estimation.R`

```r
test_that("Beta-binomial uses weighted estimation", {
    # Case: varying N_i
    testData <- data.frame(
        case_id = c("A", "B", "C"),
        total_pop = c(5, 50, 100),      # Varying N
        success = c(1, 25, 50)           # Proportions: 0.2, 0.5, 0.5
    )

    # Unweighted mean would be: (0.2 + 0.5 + 0.5)/3 = 0.40
    # Weighted mean should be: (1+25+50)/(5+50+100) = 76/155 = 0.49

    # Run pathsampling (would need to extract internal α/β)
    # ...

    # Verify α/(α+β) ≈ 0.49, NOT 0.40
    expect_true(abs(estimated_mean - 0.49) < 0.01)
    expect_true(abs(estimated_mean - 0.40) > 0.05)  # NOT unweighted
})

test_that("Bootstrap warns about selection bias", {
    testData <- data.frame(
        case_id = paste0("Case", 1:10),
        first_detection = c(1, 2, 3, NA, NA, 1, 2, NA, 1, 3),  # 3 NAs (undetected)
        total_samples = rep(10, 10)
    )

    result <- pathsampling(data = testData, ...)

    # Should contain selection bias warning
    expect_true(any(grepl("SELECTION BIAS", result$notices)))
    # Or should require gold standard
    expect_error(
        pathsampling(data = testData, showBootstrap = TRUE,
                    goldStandardPositives = NULL),
        "Gold standard required"
    )
})

test_that("Binomial disabled when independence violated", {
    # High heterogeneity data
    testData <- data.frame(
        case_id = paste0("Case", 1:10),
        total_samples = rep(20, 10),
        positive_count = c(1, 1, 1, 18, 19, 19, 2, 2, 18, 18)  # CV > 0.5
    )

    result <- pathsampling(data = testData, ...)

    # Binomial table should be empty
    expect_equal(nrow(result$binomialTable), 0)
    # Should have warning
    expect_true(any(grepl("independence", result$warnings, ignore.case = TRUE)))
})
```

#### 2.2 Clinical Validation Tests

**Create:** `tests/testthat/test-pathsampling-clinical-scenarios.R`

```r
test_that("Tumor margin scenario: clustered disease", {
    # Simulated tumor margin data with spatial clustering
    # High heterogeneity expected
    # ...
})

test_that("Lymph node scenario: varying N", {
    # Different node counts per case
    # Should use weighted beta-binomial
    # ...
})

test_that("Omentum scenario: empirical method appropriate", {
    # Large samples, can use empirical
    # ...
})
```

### Phase 3: DOCUMENTATION (Required for Users)

#### 3.1 Add Methodological Documentation

**Create:** `vignettes/pathsampling-statistical-methods.Rmd`

Content:
- Assumptions of each model (binomial, bootstrap, beta-binomial)
- When each method is appropriate
- Known limitations and biases
- How to interpret results
- Gold standard requirements

#### 3.2 Update Function Documentation

Add to `@description`:
```
IMPORTANT LIMITATIONS:
- Bootstrap/empirical methods estimate CONDITIONAL sensitivity (given detection).
  Provide gold-standard positive count for unbiased estimates.
- Binomial model requires INDEPENDENT sampling. Disabled automatically when
  heterogeneity (CV > 0.5) detected.
- Beta-binomial appropriate for overdispersed data with varying sample sizes.
```

---

## Production Readiness Checklist

### Before Release

- [ ] Fix 1.1: Beta-binomial weighted estimation (VGAM or corrected MoM)
- [ ] Fix 1.2: Selection bias warnings + gold standard option
- [ ] Fix 1.3: Enforce model choice logic (disable when invalid)
- [ ] Test 2.1: Parameter estimation validation tests
- [ ] Test 2.2: Clinical scenario tests
- [ ] Test 2.3: Model choice logic tests
- [ ] Doc 3.1: Statistical methods vignette
- [ ] Doc 3.2: Update function documentation
- [ ] Validation: Test with known clinical datasets
- [ ] Review: Statistical review by independent biostatistician

### Current Status

**Mathematical Implementation:** ✅ CORRECT (36/36 tests passing)

**Statistical Methodology:** ❌ FLAWED
- Beta-binomial estimation: ❌ Biased (unweighted)
- Selection bias: ❌ No warnings/mitigation
- Model choice: ❌ Outputs invalid results
- Test coverage: ❌ Methodological gaps

**Overall Readiness:** ❌ NOT PRODUCTION READY

**Estimated Effort to Fix:** 10-15 hours of development + validation

---

## Comparison: diagnosticmeta vs pathsampling

| Aspect | diagnosticmeta | pathsampling |
|--------|----------------|--------------|
| **Uses established packages** | ✅ mada, metafor | ⚠️ Partial (base R, custom Beta-Binom) |
| **Mathematical correctness** | ✅ Validated | ✅ Validated (my tests) |
| **Statistical methodology** | ✅ Sound | ❌ Flawed (this review) |
| **Test coverage** | ✅ Comprehensive | ⚠️ Partial (math only) |
| **Selection bias** | N/A (meta-analysis) | ❌ Not addressed |
| **Model assumptions** | ✅ Enforced | ❌ Warned but not enforced |
| **Production ready** | ✅ YES (after our fixes) | ❌ NO (needs methodology fixes) |

---

## Recommendation

**DO NOT RELEASE pathsampling for clinical use** until:

1. ✅ Beta-binomial estimation corrected (VGAM or weighted MoM)
2. ✅ Selection bias explicitly warned and documented
3. ✅ Invalid model results disabled (not just warned)
4. ✅ Comprehensive methodological tests added
5. ✅ Independent statistical validation completed

**Timeline:** Allow 2-3 weeks for fixes + validation before considering production release.

**Alternative:** Release with EXPERIMENTAL status and clear warnings that it's for research/validation only, not clinical decision-making.
