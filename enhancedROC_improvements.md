# enhancedROC: Improvement Suggestions

**Author:** Improvement Agent (Senior Biostatistician & R Developer perspective)
**Date:** 2026-03-02
**Files Reviewed:**
- `R/enhancedROC.b.R`
- `jamovi/enhancedroc.a.yaml`
- `jamovi/enhancedroc.r.yaml`
- `jamovi/enhancedroc.u.yaml`
- `tests/testthat/test-enhancedROC-basic.R`

---

## Executive Summary

The enhancedROC module is architecturally sound and already contains many clinical-grade features. However, several critical issues reduce its reliability and usability for clinicians and pathologists. The most important findings are:

1. **Many options declared in .a.yaml are stub/unimplemented** — they appear in the UI but silently do nothing (only an "INFO" notice is shown). This is confusing and potentially misleading to clinical users.
2. **No NRI/IDI calculation** — these are standard reclassification metrics in modern diagnostic research and are missing entirely.
3. **Time-dependent ROC is commented out** — this is essential for biomarker studies with survival endpoints.
4. **Default options are poorly calibrated** — nearly everything is `false` by default, so a naive user gets almost no output.
5. **Calibration implementation may not work robustly** for non-logistic predictors (raw continuous biomarkers without probability calibration).
6. **Tests are too shallow** — they only check class type, not numerical correctness or output content.

---

## 1. Missing Statistical Features

### 1.1 Net Reclassification Improvement (NRI) and Integrated Discrimination Improvement (IDI)

**Status: Not implemented.** These are the most-used metrics when comparing a new biomarker to an existing model (e.g., adding a new IHC marker to a clinical risk score).

**Priority: HIGH for pathology/clinical research.**

- NRI (continuous and categorical) is implemented in the `nricens` package.
- IDI is a straightforward numeric calculation.
- Both require a baseline model and a new model (already available in `comparative` analysis mode).
- Should be gated behind `analysisType == "comparative"` and a new bool option `nriIdiAnalysis`.

**Suggested approach:**
```r
# In comparative mode, compute NRI/IDI between predictor 1 vs predictor 2
library(nricens)
nri_result <- nricens::nribin(
  event = binary_outcome,
  p.std = predictions_model1,  # standardized to (0,1)
  p.new = predictions_model2,
  updown = "diff",  # or "category"
  cut = optimal_cutoff
)
```

**Output table needed:** Add to `.r.yaml` under `comparative` mode:
```yaml
- name: nriIdiTable
  title: Reclassification Metrics (NRI/IDI)
  type: Table
  visible: (nriIdiAnalysis && analysisType:comparative)
```

### 1.2 Time-Dependent ROC (Currently Commented Out)

**Status: Declared as option but commented out.** The `.a.yaml` has commented-out options for `timeDependentROC`, `timePoints`, and `tdRocMethod`.

**Priority: HIGH** — many pathology studies use survival endpoints (OS, DFS), and the standard AUC is inappropriate for censored outcomes.

- The `timeROC` package implements Blanche et al. (2013) incident/dynamic AUC.
- The `survivalROC` package implements Heagerty et al. (2000).
- Integration requires: a time variable, event indicator, and one or more time points.
- This is especially important for Ki-67, HER2, and other prognostic biomarkers.

**Recommendation:** Uncomment and implement the time-dependent ROC options. Requires adding time variable input to `.a.yaml`.

### 1.3 DeLong Test for Multiple Curves

**Status: Implemented for pairwise comparisons.** However, a global test (does any curve differ from the others?) is missing.

**Gap:** When comparing 3+ biomarkers, pairwise DeLong comparisons inflate Type I error. A global multivariate test using the DeLong covariance matrix should be added.

```r
# Global test across k curves
# Uses pROC::roc.test() or manual multivariate Wald test
```

### 1.4 Penalized/Bias-Corrected AUC

**Status: Not implemented.** The bootstrap internal validation (`internalValidation`) option exists but is marked as TODO in the unimplemented list.

**Issue:** Standard AUC is optimistically biased when evaluated on training data. Harrell's optimism-corrected AUC (as done in the `survival` module for C-index) should be implemented here.

**Recommendation:** Implement the `.bootstrapValidation` approach from the `jsurvival` module (already proven to work) adapted for binary outcomes.

### 1.5 Decision Curve Analysis (DCA) Integration

**Status: Partially present as `clinicalUtilityCurve` and `decisionImpactCurves`, but DCA is not fully implemented.**

Real decision curve analysis (Vickers & Elkin 2006) computes net benefit across a range of threshold probabilities and plots:
- Net benefit of the test
- Net benefit of treating all
- Net benefit of treating none

The current implementation seems to plot something related to utility but not the standard DCA. This should use the `dcurves` package for validated output.

---

## 2. Clinical Interpretation Aids

### 2.1 AUC Interpretation Thresholds

**Status: Implemented** via `private$.interpretAUC()`. The AUC Summary table already has `auc_interpretation` and `clinical_utility` columns. This is **well done**.

**Minor improvement:** Add a visual flag (e.g., colored HTML badge) in the AUC summary for quick visual scanning, similar to how statistical significance is shown with asterisks.

### 2.2 Number Needed to Diagnose (NND)

**Status: Listed as `nntCalculation` option but marked as unimplemented in the TODO list.**

NND = 1 / (Sensitivity + Specificity - 1) = 1 / Youden Index

This is trivial to implement — it is just the reciprocal of the Youden Index. Since the Youden Index is already computed, this should be a 2-line addition.

```r
nnd <- if (optimal$youden_index > 0) round(1 / optimal$youden_index) else Inf
```

**Priority: Easy win.** Clinicians often find NND more interpretable than Youden Index.

### 2.3 Predictive Summary Index (PSI)

**Status: Not mentioned.** PSI = PPV + NPV - 1. This is a useful single-number summary of predictive performance that complements AUC (which measures discrimination only). It should be added to the clinical metrics table.

### 2.4 Post-Test Probability Visualization

**Status: Not present.** A Fagan nomogram or pre/post-test probability table would help clinicians translate LR+ and LR- into clinical decisions. This is one of the most practically useful outputs for pathologists.

**Suggestion:** Add a simple table showing post-test probability at common pre-test prevalences (e.g., 5%, 10%, 20%, 30%, 50%) for the optimal cutoff.

### 2.5 Confidence Intervals for Sensitivity and Specificity at Optimal Cutoff

**Status: Listed as `bootstrapCutoffCI` but marked as unimplemented.**

The Wilson interval (exact binomial CI) is trivial to compute without bootstrap:
```r
binom.test(TP, TP + FN)$conf.int  # for sensitivity
binom.test(TN, TN + FP)$conf.int  # for specificity
```

This should be implemented immediately using `stats::binom.test()` since the data is already available. No bootstrap needed for exact CIs.

---

## 3. Plot Improvements

### 3.1 Distribution Plot at Cutpoint ("Dotplot" or Violin/Box)

**Status: Not present.** This is arguably the most important supplementary plot for clinical audiences. It shows the distribution of the biomarker in disease-positive and disease-negative groups, with the optimal cutoff drawn as a vertical line. This allows clinicians to judge whether the cutoff makes clinical sense (e.g., is it consistent with clinical experience?).

**Suggested implementation using ggplot2:**
```r
ggplot(data, aes(x = predictor, fill = outcome)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = optimal_cutoff, linetype = "dashed") +
  labs(title = "Biomarker Distribution at Optimal Cutoff")
```

**Priority: HIGH for clinical acceptance.**

### 3.2 Precision-Recall Curve Plot

**Status: Present as `prcPlot`, only visible when `detectImbalance` is true.**

This is a design flaw. The PR curve is important for any imbalanced dataset and should be accessible independently of the imbalance detection mechanism. Consider making it available as a standalone option (`showPRCurve = TRUE`) in addition to the imbalance detection pathway.

### 3.3 Calibration Plot

**Status: Implemented** as `calibrationPlotImage` with `calibrationAnalysis && calibrationPlot`. This appears well-designed.

**Critical issue:** The calibration analysis assumes the predictor values are probabilities (0-1). For raw biomarker values (e.g., Ki-67 percentage, HER2 H-score), calibration analysis is not meaningful without first fitting a logistic regression model to convert values to predicted probabilities. The code should:
1. Detect whether predictors are on the (0,1) scale
2. If not, fit a logistic regression model and use predicted probabilities for calibration
3. Warn the user clearly if calibration is applied to non-probability values

### 3.4 Forest Plot for Subgroup Analysis

**Status: Not present.** For multi-predictor analysis, a forest plot showing AUC with 95% CIs across predictors would be highly useful. This is standard in systematic reviews and meta-analyses.

### 3.5 Waterfall Plot of Predicted Probabilities

**Status: Not present.** A waterfall/bar plot showing individual predicted probabilities sorted in descending order, colored by outcome, is useful for visualizing discriminative ability and identifying intermediate-risk patients.

---

## 4. Performance and Code Quality

### 4.1 Stub/Unimplemented Options are Misleading

**This is the most serious code quality issue.**

The following options are declared in `.a.yaml`, appear in the UI, but are explicitly listed as "not yet implemented" in `.b.R` (lines 247-277):

- `harrellCIndex`, `unoCStatistic`, `incidentDynamic`, `cumulativeDynamic`
- `competingRisksConcordance`
- `splineCalibration`, `eoRatio`, `namDagostino`, `greenwoodNam`
- `calibrationBelt`, `calibrationDensity`
- `optimismCorrection`, `externalValidation`
- `decisionImpactCurves`, `netBenefitRegression`, `modelUpdating`, `transportability`
- `bootstrapPartialAUC`, `bootstrapCutoffCI`
- `nntCalculation`

**Recommendation:** Options that are not yet implemented should either:
1. Be removed from `.a.yaml` entirely until implemented, OR
2. Be clearly marked `(Coming Soon)` in their title in `.a.yaml`, OR
3. Be disabled/greyed out using visibility conditions

Showing stub features to clinicians erodes trust. A clinician who enables "Harrell C-Index" and sees only an INFO message saying "planned for future release" will lose confidence in the entire module.

### 4.2 Redundant ROC Object Computation

In `.runROCAnalysis()`, the ROC object is computed with `ci = TRUE` (DeLong) and then optionally recomputed with bootstrap CI via `pROC::ci.auc()`. This is correct but could be made clearer by always computing the DeLong CI first and only calling `ci.auc()` for bootstrap override.

No functional redundancy was found — the implementation handles this correctly.

### 4.3 `caret::confusionMatrix` Dependency

The code uses `caret::confusionMatrix` extensively for confusion matrix calculations. The `caret` package is heavy (many dependencies). For simple 2x2 confusion matrix calculations, using base R or `yardstick` from the `tidymodels` ecosystem would reduce dependency weight.

**Recommendation for future refactoring:** Replace `caret::confusionMatrix` with a custom helper or `yardstick::conf_mat` for simpler dependency management.

### 4.4 Error Messages in HTML vs Notices Panel

There is inconsistency in how errors are reported:
- Some errors go to `self$results$results$instructions$setContent()` (the instructions HTML panel)
- Other errors/warnings go through `private$.addNotice()` to the notices HTML panel

This splits user feedback across two different UI locations. **Recommendation:** Standardize all user-facing feedback through the `notices` panel only. The `instructions` panel should only show static usage instructions.

### 4.5 `private$.data` Overwritten Mid-Run

At line 146, `private$.data <- analysisData` overwrites the raw data with the cleaned/filtered data. This is the right approach for consistency, but the comment says "CRITICAL FIX" which signals this was a past bug.

**Risk:** If `.init()` or any other function accesses `private$.data` between initialization and `.run()`, it will see different data. Consider renaming to `private$.analysisData` to make the intent explicit.

### 4.6 `%||%` Operator Undefined in Local Scope

The code uses `%||%` (null coalescing operator) in multiple places (e.g., line 649: `self$options$bootstrapSamples %||% 200`). This operator is defined in `rlang` but may not be available without explicitly loading it. The code should either:
1. `@importFrom rlang %||%` in the file header, or
2. Define it locally: `` `%||%` <- function(a, b) if (is.null(a)) b else a ``

---

## 5. Better Defaults

### 5.1 Current Default State

With all defaults as-is, a user who opens enhancedROC and selects variables gets:
- No AUC table (`aucTable = false`)
- No optimal cutoffs (`optimalCutoffs = false`)
- No ROC curve plot (`rocCurve = false`)
- No diagnostic metrics (`diagnosticMetrics = false`)

The only outputs visible by default are:
- Instructions HTML panel
- Analysis Summary HTML
- Clinical Report HTML

This means a new user gets essentially no numerical output without manually enabling each table. **This is a poor default configuration for a clinical tool.**

### 5.2 Recommended Defaults

For a clinical diagnostic tool, the following should be `true` by default:

| Option | Recommended Default | Rationale |
|--------|---------------------|-----------|
| `rocCurve` | `true` | Primary output — always expected |
| `aucTable` | `true` | Core statistical result |
| `optimalCutoffs` | `true` | Most clinically useful table |
| `diagnosticMetrics` | `true` | Sensitivity/specificity at optimal cutoff |
| `youdenOptimization` | `true` | Standard optimal cutoff method |
| `showCutoffPoints` | `true` | Visual marker on ROC curve |
| `aucTable` | `true` | Standard first output |
| `clinicalMetrics` | `false` | Requires prevalence input — keep optional |
| `calibrationAnalysis` | `false` | Advanced feature — keep optional |

### 5.3 `clinicalContext` Default

The default `clinicalContext = "general"` is appropriate. However, the presets system should more prominently guide users to select a context, as it affects cutoff optimization strategy.

### 5.4 `confidenceLevel` Default

`95` is appropriate. No change needed.

### 5.5 `bootstrapSamples` Default

`1000` samples may be slow for large datasets. Consider making the default `500` with a note that 2000 is recommended for publication-quality results.

### 5.6 `direction` Default

`auto` is appropriate. The auto-detect with explicit INFO notice (lines 717-722) is a good design — it surfaces the decision to the user.

---

## 6. Statistical Accuracy Concerns

### 6.1 Calibration for Non-Probability Predictors

**Critical issue for clinical use.** The Hosmer-Lemeshow test and Brier score assume the predictor values are calibrated probabilities in (0, 1). When users supply raw biomarker values (e.g., Ki-67% which ranges 0-100, or a continuous biomarker score), the calibration analysis will either:
1. Fail silently with incorrect results
2. Give misleading calibration metrics

**The code does not check or warn about this.** Before running calibration analysis, the code should:
```r
if (any(predictor_values < 0 | predictor_values > 1)) {
  private$.addNotice(
    type = "STRONG_WARNING",
    title = "Calibration: Non-Probability Predictor Detected",
    content = "Calibration metrics assume predictor values are probabilities (0-1).
               For raw biomarker values, a logistic regression model will be fitted
               to obtain probability estimates before calibration assessment."
  )
  # Then fit logistic model and use predicted probabilities
}
```

### 6.2 Multiple Testing in Pairwise DeLong Comparisons

When comparing k predictors, k(k-1)/2 pairwise DeLong tests are performed. There is **no correction for multiple testing** (no Bonferroni, Holm, BH, or Bonferroni-Holm adjustment). For k=5 predictors, this is 10 tests with inflated Type I error.

**Recommendation:** Add a multiple testing correction option (Bonferroni or BH) for pairwise comparisons, and show adjusted p-values alongside raw p-values.

### 6.3 Youden Index as Default Cutoff Method

The Youden Index equally weights sensitivity and specificity. For clinical practice:
- Screening tests prioritize sensitivity (minimize false negatives)
- Confirmatory tests prioritize specificity (minimize false positives)

The clinical presets partially address this, but the **default** behavior should clearly state that Youden Index assumes equal cost of false positives and false negatives.

A notice should be added when the optimal cutoff is computed:
```
"Optimal cutoff based on Youden Index, which assumes equal cost of
false positives and false negatives. For screening applications,
consider using the sensitivity-constrained threshold instead."
```

### 6.4 Bootstrap CIs for Optimal Cutoff: Sampling Distribution Issue

Bootstrap CIs for optimal cutoffs require care — the cutoff itself is a data-adaptive statistic that has a complex sampling distribution. Simple percentile bootstrap of the cutoff value can give misleading intervals. The BCa bootstrap is more appropriate, and this is already the default (`bootstrapMethod = "bca"`). **This is correctly designed.**

However, the confidence interval for *sensitivity and specificity at the optimal cutoff* (option `bootstrapCutoffCI`) must use a bootstrap that re-optimizes the cutoff in each bootstrap sample. If the current implementation (when eventually coded) simply evaluates sensitivity/specificity at the fixed optimal cutoff in bootstrap samples, the CIs will be too narrow (they ignore cutoff selection uncertainty). This must be documented in the statistical methods.

---

## 7. Summary of Prioritized Recommendations

### HIGH PRIORITY (implement before production release)

1. **Remove or clearly mark stub options** that are unimplemented — they mislead clinical users
2. **Set better defaults**: `rocCurve`, `aucTable`, `optimalCutoffs`, `diagnosticMetrics`, `youdenOptimization` all `true`
3. **Implement exact binomial CIs** for sensitivity/specificity (trivial, no new dependencies)
4. **Add NND** (Number Needed to Diagnose) — trivial computation, high clinical value
5. **Add calibration guard** for non-probability predictors with appropriate warning and logistic regression fallback
6. **Add distribution plot** (density/violin at cutpoint) — highest clinical impact per implementation cost

### MEDIUM PRIORITY (next version)

7. **Implement NRI/IDI** for comparative analysis (use `nricens` package)
8. **Add multiple testing correction** for pairwise DeLong comparisons
9. **Add PSI** (Predictive Summary Index) to clinical metrics table
10. **Add post-test probability table** at multiple prevalences
11. **Implement time-dependent ROC** (uncomment and wire up options, use `timeROC` package)

### LOW PRIORITY (future roadmap)

12. **Decision Curve Analysis** (proper implementation using `dcurves` package)
13. **Forest plot** for multi-predictor AUC comparison
14. **Standardize error reporting** to single notices panel (minor UX improvement)
15. **Reduce `caret` dependency** (long-term code quality improvement)
16. **Optimism-corrected AUC** via bootstrap (adapt from `jsurvival` C-index implementation)

---

## 8. Test Coverage Gaps

The current tests (`test-enhancedROC-basic.R`) only check:
- Function exists and runs without error
- Returns `enhancedROCResults` class

They do **not** verify:
- AUC numerical accuracy (compare against known `pROC::auc()` values)
- Correct optimal cutoff (compare against `pROC::coords(roc, "best")`)
- Correct sensitivity/specificity values
- That CI bounds are valid (lower < AUC < upper)
- Calibration output correctness
- That DeLong comparison p-value matches `pROC::roc.test()`

**Recommendation:** Add numerical accuracy tests using well-known datasets with known AUC values (e.g., the built-in `aSAH` dataset from `pROC` has AUC = 0.6085 for `ndka`, providing a ground truth for comparison).

---

*This document was prepared as part of the enhancedROC team review (Task #7).*
