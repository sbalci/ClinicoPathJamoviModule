# Statistical Accuracy Review: enhancedROC.b.R

**Reviewer:** Biostatistics Expert Agent (Task #1)
**Date:** 2026-03-02
**File reviewed:** `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/enhancedROC.b.R`
**YAML reviewed:** `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/enhancedroc.a.yaml`

---

## Summary

The enhancedROC module is architecturally sound and uses pROC as its core engine, which is the gold-standard R package for ROC analysis. Most core calculations delegate to well-validated functions. However, several issues were found that range from critical to minor. The module should NOT be considered fully release-ready for clinical use in its current form without addressing the critical and major issues.

---

## CRITICAL ISSUES

### 1. Standard Error Derivation for AUC is Incorrect (Line 1050)

**What the code does:**
```r
std_error = if(!is.null(roc_obj$ci) && !self$options$useBootstrap)
    (roc_obj$ci[3] - roc_obj$ci[1]) / (2 * qnorm((1 + self$options$confidenceLevel / 100) / 2))
else NA
```

**What it should do:**
This back-computes the standard error from a CI by dividing the half-width by the z-score. However, `roc_obj$ci` from pROC contains the **DeLong CI**, not a symmetric normal-approximation CI. The DeLong CI is derived from a variance formula, so the standard error is available directly via the DeLong method formula. Furthermore, `confidenceLevel` is stored as a percentage (e.g., 95), so `self$options$confidenceLevel / 100` gives 0.95, and `(1 + 0.95) / 2 = 0.975`, which gives `qnorm(0.975) ≈ 1.96` — this part is correct.

However, the DeLong CI may not be perfectly symmetric about the AUC estimate (it is computed on the logit scale internally in some implementations), so the back-computed SE can be slightly off. More importantly, the SE label in the table implies it can be used for further calculation, which is misleading.

**Recommendation:** Either report the SE from the DeLong formula directly (pROC's `var(roc_obj)` gives the variance), or remove the SE column entirely and just show the CI bounds. Add a note that the displayed SE is an approximation.

**Severity: MAJOR**

---

### 2. Hosmer-Lemeshow Test Formula is Non-Standard (Lines 3512-3516)

**What the code does:**
```r
denom <- exp * (1 - exp/n_cnt)
valid <- !is.na(denom) & abs(denom) > 1e-10
hl_stat <- if (any(valid)) sum((obs[valid] - exp[valid])^2 / denom[valid]) else NA
```

**What it should do:**
The standard Hosmer-Lemeshow statistic is:
$$\hat{C} = \sum_{g=1}^{G} \frac{(O_g - E_g)^2}{E_g(1 - E_g/n_g)}$$

This matches the code. However, the degrees of freedom should be `G - 2` only if the probabilities come from a model with 2 parameters (intercept + 1 predictor). The code uses `df <- actual_groups - 2`, which is correct for a simple 1-predictor logistic regression but may be wrong in other cases (e.g., if the user provides raw probabilities).

Additionally, the standard HL test uses groups created by **equal-quantile binning of predicted probabilities** (decile groups). The code does this correctly with `quantile(probs, probs = seq(0, 1, length.out = n_groups + 1))`. However, when duplicate quantile breakpoints exist, the code falls back to `as.factor(probs)` which creates groups by unique probability values — this is non-standard and may produce many groups or very few groups, invalidating the test.

**Recommendation:** When duplicate quantile breaks exist, use `cut2` from Hmisc or manually merge bins to ensure exactly `n_groups` non-empty groups. Document the df assumption clearly.

**Severity: MAJOR**

---

### 3. Bootstrap Internal Validation Uses Wrong Model (Lines 4027-4072)

**What the code does:**
Fits a simple logistic regression `y ~ x` on each bootstrap sample to estimate optimism-corrected AUC.

**What it should do:**
Per Harrell (2015), the bootstrap validation procedure should **re-fit the same model that produced the original AUC**. If the user is evaluating a raw biomarker (no model fitting), the procedure should:
1. Use the raw biomarker scores directly (ranking-based AUC, without logistic regression)
2. The apparent AUC = `pROC::roc(y_boot, x_boot)$auc`
3. The test AUC = `pROC::roc(y_original, x_boot)$auc` using same data ordering

Instead, the code is fitting a logistic regression which transforms the predictor, potentially producing optimism estimates that differ from the true optimism for the raw biomarker AUC.

**Reference:** Harrell FE (2015). Regression Modeling Strategies, 2nd ed. Springer. Chapter 5.

**Severity: MAJOR**

---

### 4. Precision-Recall AUC Calculation Uses Left-Endpoint Integration (Line 2164)

**What the code does:**
```r
auprc <- sum(diff(recall) * precision[-1], na.rm = TRUE)
```

**What it should do:**
The correct trapezoidal rule for AUPRC is:
```r
auprc <- sum(diff(recall) * (precision[-1] + precision[-length(precision)]) / 2)
```

Using only `precision[-1]` (right endpoints) is equivalent to a left/right Riemann sum, not the trapezoidal rule. For precision-recall curves this matters because precision can change sharply. The standard approach (as in scikit-learn's `average_precision_score`) uses step interpolation where the average precision is computed as:
$$AP = \sum_{n} (R_n - R_{n-1}) P_n$$
which matches `sum(diff(recall) * precision[-1])`. However, this is the step interpolation formula, not the trapezoidal formula. The choice between step and trapezoidal has significant impact on the AUPRC value.

The code should be consistent about which convention it uses and document it clearly. If using step interpolation (which is common for AUPRC), the current formula is correct. If claiming to use trapezoidal integration, it is wrong.

**Recommendation:** Either use the standard step interpolation formula (consistent with sklearn's `average_precision_score`) or clearly switch to trapezoidal and update the formula. Document which convention is used in the output.

**Severity: MODERATE** (more of a documentation/clarity issue if step interpolation is intended)

---

## MAJOR ISSUES

### 5. Scaled Brier Score Formula is Non-Standard (Lines 3443-3446)

**What the code does:**
```r
brier_max <- prev * (1 - prev)^2 + (1 - prev) * prev^2
scaled_brier <- if (brier_max > 1e-10) 1 - (brier / brier_max) else NA_real_
```

**What it should do:**
The scaled Brier score (also called Brier Skill Score) is typically:
$$BS_{scaled} = 1 - \frac{BS}{BS_{max}}$$

where `BS_max = prev * (1 - prev)` (the Brier score for a naive classifier that always predicts the prevalence). The code instead computes `BS_max = prev * (1-prev)^2 + (1-prev) * prev^2`, which simplifies to `prev * (1-prev) * [(1-prev) + prev] = prev * (1-prev)`. So the formula is mathematically equivalent.

Wait — let me verify: `prev*(1-prev)^2 + (1-prev)*prev^2 = prev*(1-prev)*(1-prev+prev) = prev*(1-prev)*1 = prev*(1-prev)`. Yes, the formula is equivalent to the standard one. This is correct.

**Verdict: Correct** (initially appeared wrong, algebraically equivalent to standard formula).

---

### 6. CROC AUC Interpretation is Misleading (Lines 1598-1604)

**What the code does:**
```r
early_retrieval_gain <- croc_result$croc_auc - roc_auc
```
This is labeled as "Early Retrieval Gain" and interpreted as how much the classifier benefits from early-retrieval weighting.

**What it should do:**
The CROC (Concentrated ROC) AUC is not directly comparable to the standard AUC because it measures the area under the curve in a *transformed* FPR space. The "gain" (CROC AUC - standard AUC) can be positive or negative for mathematical reasons unrelated to "early retrieval performance." The CROC AUC and standard AUC are measured on different scales (the CROC FPR axis is a nonlinear transformation), so their difference is not a meaningful quantity.

**Reference:** Swamidass SJ, et al. (2010). A CROC stronger than ROC: measuring, visualizing and optimizing early retrieval. Bioinformatics, 26(10):1348-1356.

**Recommendation:** Remove or clarify the "early_retrieval_gain" metric. The CROC AUC should be interpreted on its own scale, compared only against other CROC AUCs computed with the same alpha parameter.

**Severity: MAJOR**

---

### 7. Calibration Analysis Approach is Clinically Misleading (Lines 3423-3435)

**What the code does:**
When the predictor is NOT in [0,1] range, it fits a logistic regression model `glm(y ~ x, family=binomial)` and uses the resulting predicted probabilities for calibration.

**What it should do:**
This approach computes calibration of the *logistic regression model*, not calibration of the *raw biomarker*. For a raw biomarker (e.g., a blood test value in mg/dL), calibration in the traditional sense requires the biomarker to already represent probabilities (e.g., from a prediction model). Fitting a logistic regression and then testing its calibration is circular (the model will trivially produce well-calibrated probabilities for a single predictor).

The notice warns users about this, but the fact that calibration results are still shown creates risk of misinterpretation.

**Recommendation:** For predictors not in [0,1], the calibration analysis should be skipped entirely with a clear message, OR the section should be clearly relabeled as "Logistic Regression Model Calibration" to distinguish it from direct biomarker calibration.

**Severity: MAJOR (Clinical Safety)**

---

### 8. McNemar Test Applied Incorrectly for Sensitivity/Specificity Comparison (Lines 2529-2583)

**What the code does:**
For comparing sensitivity between two models, the code restricts analysis to `mask <- outcome == pos_class` (true positives only), then applies McNemar's test on whether `class1` and `class2` agree on these subjects.

**What it should do:**
McNemar's test for comparing sensitivities should use a 2x2 table of cases where:
- Model 1 correct / Model 2 correct
- Model 1 correct / Model 2 incorrect
- Model 1 incorrect / Model 2 correct
- Model 1 incorrect / Model 2 incorrect

The code does create such a table, but only among the positive cases. This is the correct approach (McNeil & Hanley, 1984) for paired sensitivity comparison. **However**, the test uses `correct = TRUE` (Yates' continuity correction), which is generally not recommended for McNemar's test in modern practice because it can be overly conservative. The exact McNemar test is preferred.

**Reference:** McNeil BJ, Hanley JA (1984). Statistical approaches to the analysis of receiver operating characteristic (ROC) curves. Med Decis Making, 4(2):137-150.

**Recommendation:** Remove `correct = TRUE` or use `stats::mcnemar.test(cont_table, correct = FALSE)` and note that exact McNemar is preferred.

**Severity: MINOR**

---

## MINOR ISSUES

### 9. Direction Convention Documentation in `.applyDirectionCutoff` (Lines 844-853)

**What the code does:**
The comment correctly states the pROC direction convention:
```
# pROC direction "<" means controls < cases: HIGHER values = positive
```

The implementation is:
```r
if (roc_direction == "<") {
    return(predictor_values >= cutoff)  # Higher values = positive
}
```

**Assessment:** This is correct. In pROC, `direction = "<"` means `controls < cases`, so higher predictor values classify as cases (positive). Using `>=` for positive classification is correct.

**Verdict: Correct**

---

### 10. Default Positive Class Selection (Lines 531-534)

**What the code does:**
```r
positive_class <- available_levels[2]  # Second level is positive by default
```

**What it should do:**
This is a common convention (second factor level = positive/disease class), which aligns with pROC's behavior. However, R factor levels are alphabetical by default, which can lead to unexpected positive class selection (e.g., "Disease" vs "No_Disease" — alphabetically "Disease" < "No_Disease", so "No_Disease" would be the second level and thus "positive" by default).

**Recommendation:** Add a more prominent warning to users about the default positive class selection. The current message exists but is subtle.

**Severity: MINOR**

---

### 11. Binomial CI uses Clopper-Pearson (Lines 2137-2141)

**What the code does:**
```r
bt <- suppressWarnings(binom.test(successes, n))
```

**Assessment:** `binom.test()` computes the exact Clopper-Pearson CI, which is the most conservative (widest) exact CI. This is appropriate and widely accepted for sensitivity/specificity CIs.

**Verdict: Correct**

---

### 12. PPV/NPV Calculation Correctly Uses Bayes' Theorem (Lines 1273-1276)

**What the code does:**
```r
ppv_denom <- (sens * prevalence) + ((1 - spec) * (1 - prevalence))
ppv <- if (ppv_denom > 1e-10) (sens * prevalence) / ppv_denom else NA_real_
npv_denom <- ((1 - sens) * prevalence) + (spec * (1 - prevalence))
npv <- if (npv_denom > 1e-10) (spec * (1 - prevalence)) / npv_denom else NA_real_
```

**Assessment:** These are the correct Bayes' theorem formulas for PPV and NPV given sensitivity, specificity, and prevalence. Division-by-zero guards are present.

**Verdict: Correct**

---

### 13. Likelihood Ratio Calculations (Lines 1279-1294)

**What the code does:**
```r
lr_pos <- if (abs(1 - spec) < 1e-10) Inf else sens / (1 - spec)
lr_neg <- if (abs(spec) < 1e-10 && abs(1 - sens) < 1e-10) NA_real_
         else if (abs(spec) < 1e-10) Inf
         else (1 - sens) / spec
dor <- lr_pos / lr_neg
```

**Issues found:**
- `LR-` formula: `(1-sens)/spec` is correct.
- The case `abs(spec) < 1e-10` returning `Inf` for `lr_neg` is incorrect: if spec=0, then FPR=1 (all test positive), LR- = (1-sens)/0 = Inf, but this represents that a negative test result is impossible (since all are positive). This edge case indicates a broken test and should return NA.
- When `spec=1`, `lr_pos = Inf`, which is correct for a perfect rule-in test.
- The DOR edge case handling (lines 1287-1294) is reasonable but the comment "Inf/Inf is indeterminate" is slightly incorrect: DOR = LR+/LR- where LR+ = sens/(1-spec) and LR- = (1-sens)/spec. When both spec=1 and sens=1, both LR+ and LR- involve 0 denominators, so DOR is indeterminate. This case is handled correctly.

**Verdict: Mostly correct, minor edge case for LR- when spec=0**

---

### 14. Youden Index as Primary Cutoff Method (Lines 931-941)

**What the code does:**
Uses J = Sensitivity + Specificity - 1 as the primary cutoff optimization criterion.

**Assessment:** Youden (1950) is the most commonly used criterion and is appropriate. However, the module lacks other cutoff methods that are clinically important:
- **Closest to (0,1)** (minimizes Euclidean distance from perfect classifier)
- **Cost-based optimal cutoff** (when FN and FP costs differ)
- **Concordance probability method**

The `pROC::coords(roc_obj, "best", ...)` function supports these via the `best.method` argument (options: "youden", "closest.topleft"). The current code uses `pROC::coords(roc_obj, "best", ...)` for the non-Youden case (line 881), which defaults to "youden" anyway — so the only option is Youden in both paths.

**Recommendation:** Expose `best.method` as a user option to allow "closest.topleft" selection for clinical use cases where equal weighting of sensitivity and specificity is not appropriate.

**Severity: MODERATE (missing feature, not an error)**

---

### 15. Partial AUC Normalization is Correct (Lines 1553-1561)

**What the code does:**
Uses `pROC::auc(..., partial.auc.correct = TRUE)` for normalization.

**Assessment:** pROC implements the McClish (1989) normalization, which scales the partial AUC to [0.5, 1.0] where 0.5 = chance performance within the range and 1.0 = perfect performance. This is the standard approach.

**Verdict: Correct**

---

### 16. ROC Curve AUC via Trapezoidal Rule (pROC delegation)

**What the code does:**
Delegates to `pROC::roc()` and `pROC::auc()`.

**Assessment:** pROC uses the Mann-Whitney U statistic formula which is equivalent to the trapezoidal rule for empirical ROC curves. The DeLong variance estimator is implemented correctly within pROC. This is appropriate and validated.

**Verdict: Correct**

---

### 17. Multi-Class AUC Uses Hand-Till Method (Lines 3641-3656)

**What the code does:**
Uses `pROC::multiclass.roc()` which implements Hand & Till (2001).

**What it should do:**
The Hand-Till method is appropriate for multi-class AUC with ordinal predictors. However, the code also claims to calculate OVR (One-vs-Rest) AUC separately (lines 3669-3689), and reports the Hand-Till result as "macro average" in the average table. This is incorrect — the Hand-Till method is pairwise (OVO), not a macro average of OVR AUCs.

**Reference:** Hand DJ, Till RJ (2001). A Simple Generalisation of the Area Under the ROC Curve for Multiple Class Classification Problems. Mach Learn, 45:171-186.

**Recommendation:** Clearly distinguish between Hand-Till pairwise AUC and macro-averaged OVR AUC in the output. These are different metrics and should not be presented in the same "averaging method" row.

**Severity: MAJOR**

---

### 18. Convex Hull Calculation Uses Full chull() (Lines 1692-1739)

**What the code does:**
Computes the full convex hull using `grDevices::chull()`, then filters to the upper half (above the diagonal).

**Assessment:** This is a reasonable implementation. The ROC convex hull should connect (0,0) to (1,1) via the points that maximize performance. Using chull and filtering for `TPR >= FPR` is correct in principle.

**One concern:** The code adds (0,0) and (1,1) as corner points, then uses `chull()`. The `chull()` function returns points on the outer convex hull in counterclockwise order. For ROC analysis, we want the UPPER convex hull. The current filtering (`hull_tpr_sorted >= hull_fpr_sorted - 1e-10`) captures points above or on the diagonal, which is correct for the upper hull concept. The issue is that the full `chull()` may include lower hull points that are below the random classifier line — these are filtered out correctly.

**Verdict: Correct with caveats** (the implementation works but the ROC convex hull should formally be computed using the upper convex hull algorithm directly, not via full hull + filtering, to avoid edge cases).

---

### 19. Decision Curve Net Benefit Formula (Line 3857, 3938)

**What the code does:**
```r
net_benefit <- (tp / n) - (fp / n) * (threshold_prob / (1 - threshold_prob))
```

**Assessment:** This is the correct Decision Curve Analysis (DCA) net benefit formula from Vickers & Elkin (2006):
$$NB = \frac{TP}{n} - \frac{FP}{n} \cdot \frac{p_t}{1-p_t}$$

where $p_t$ is the threshold probability.

**Verdict: Correct**

**Reference:** Vickers AJ, Elkin EB (2006). Decision curve analysis: a novel method for evaluating prediction models. Med Decis Making, 26(6):565-574.

---

### 20. Calibration Slope/Intercept Calculation (Lines 3455-3471)

**What the code does:**
```r
logit_probs <- qlogis(probs)
cal_model <- glm(y_binary ~ logit_probs, family = binomial)
intercept <- coef(cal_model)[1]
slope <- coef(cal_model)[2]
cal_large_model <- glm(y_binary ~ offset(logit_probs), family = binomial)
cal_in_large <- coef(cal_large_model)[1]
```

**Assessment:** This is the correct Cox calibration regression approach. A well-calibrated model has slope = 1 and intercept = 0. The calibration-in-the-large (using `offset`) fixes the slope to 1 and estimates the overall miscalibration as the intercept. This is statistically correct.

The threshold for interpretation:
- `slope > 1.1` → Under-fitting
- `slope < 0.9` → Over-fitting

These thresholds are reasonable clinical cut-offs.

**Verdict: Correct**

---

### 21. Sample Size Threshold (Line 337)

**What the code does:**
```r
valid = !is.null(self$data) && nrow(self$data) >= 20
```

**Assessment:** The minimum of 20 observations is mentioned, but later (line 580) the code accepts as few as 10 observations. The minimum sample size for reliable ROC analysis is context-dependent, but 10 events per class (EPV criterion) is a common standard. The inconsistency between 20 (overall) and 10 (after NA removal) could allow analyses with as few as 5 cases and 5 controls, which is insufficient for any reliable inference.

**Recommendation:** Enforce a minimum of 10 events per class (EPV criterion). Provide a warning when EPV < 30 (recommended for stable cutoff estimation).

**Severity: MINOR**

---

## FEATURES MARKED AS UNIMPLEMENTED

The following features are listed in the analysis options (enhancedroc.a.yaml) but are explicitly marked as not implemented in the code (lines 248-268). This is handled transparently with an INFO notice to users, which is appropriate.

- Harrell C-Index
- Uno C-Statistic
- Incident/Dynamic AUC
- Cumulative/Dynamic AUC
- Competing Risks Concordance
- Spline Calibration
- E/O Ratio
- Nam-D'Agostino Test
- Greenwood-Nam-D'Agostino Test
- Calibration Belt
- Calibration Density
- Optimism Correction (separate from bootstrap)
- External Validation
- Decision Impact Curves
- Net Benefit Regression
- Model Updating
- Transportability
- Bootstrap CI for Partial AUC (as separate option)
- Bootstrap CI for Cutoffs (as separate option)
- NNT Calculation
- Weighted/Micro Multi-Class AUC Averaging

---

## CLINICAL READINESS ASSESSMENT

| Component | Status | Notes |
|-----------|--------|-------|
| AUC Calculation | PASS | Delegates to pROC, well-validated |
| DeLong CI | PASS | Correctly uses pROC |
| Bootstrap CI | PASS | Correctly uses pROC ci.auc |
| AUC SE Calculation | FAIL | Non-standard back-computation |
| Youden Index | PASS | Standard formula correctly implemented |
| Optimal Cutoff | PASS | Correct pROC delegation |
| Sensitivity/Specificity CI | PASS | Exact Clopper-Pearson |
| PPV/NPV with prevalence | PASS | Bayes' theorem correct |
| Likelihood Ratios | MOSTLY PASS | Minor edge case for LR- when spec=0 |
| Diagnostic Odds Ratio | PASS | |
| ROC Comparison (DeLong) | PASS | Correctly uses pROC::roc.test |
| Partial AUC | PASS | McClish normalization correct |
| Hosmer-Lemeshow Test | WARN | DF assumption; fallback for duplicate quantiles is non-standard |
| Calibration Slope | PASS | Cox calibration regression correct |
| Brier Score | PASS | |
| Scaled Brier Score | PASS | Algebraically equivalent to standard formula |
| Bootstrap Validation | FAIL | Fits wrong model (logistic regression instead of direct biomarker AUC) |
| CROC Analysis | WARN | Early retrieval gain metric is misleading |
| Multi-Class AUC | FAIL | Mislabels Hand-Till as "macro average" |
| Decision Curve (Net Benefit) | PASS | Correct Vickers & Elkin formula |
| McNemar Test | WARN | Uses Yates correction which is conservative |
| AUPRC Calculation | WARN | Needs documentation whether step or trapezoidal integration is used |

---

## PRIORITY FIXES

1. **CRITICAL:** Fix bootstrap internal validation to use direct AUC estimation without logistic regression re-fitting
2. **MAJOR:** Remove or fix AUC SE back-calculation (use `sqrt(var(roc_obj))` from pROC or DeLong variance directly)
3. **MAJOR:** Fix multi-class average labeling — Hand-Till is not "macro average of OVR"
4. **MAJOR:** Add HL test fallback when duplicate quantile breaks exist
5. **MAJOR:** Add warning or block calibration analysis for non-probability predictors
6. **MODERATE:** Remove CROC "early retrieval gain" metric or clarify its interpretation
7. **MINOR:** Change McNemar test to `correct = FALSE`
8. **MINOR:** Document AUPRC integration convention

---

## REFERENCES

- DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: a nonparametric approach. Biometrics, 44(3):837-845.
- Youden WJ (1950). Index for rating diagnostic tests. Cancer, 3(1):32-35.
- Hosmer DW, Lemeshow S (2000). Applied Logistic Regression, 2nd ed. Wiley.
- Hand DJ, Till RJ (2001). A Simple Generalisation of the Area Under the ROC Curve for Multiple Class Classification Problems. Mach Learn, 45:171-186.
- Harrell FE (2015). Regression Modeling Strategies, 2nd ed. Springer.
- McClish DK (1989). Analyzing a portion of the ROC curve. Med Decis Making, 9(3):190-195.
- Swamidass SJ, et al. (2010). A CROC stronger than ROC. Bioinformatics, 26(10):1348-1356.
- Vickers AJ, Elkin EB (2006). Decision curve analysis. Med Decis Making, 26(6):565-574.
- Robin X, et al. (2011). pROC. BMC Bioinformatics, 12:77.
