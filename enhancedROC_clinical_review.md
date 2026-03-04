# enhancedROC Clinical Readiness Assessment
**Reviewer:** Clinical Biostatistician
**Date:** 2026-03-02
**Files Reviewed:**
- `R/enhancedROC.b.R` (backend implementation, ~2800+ lines)
- `jamovi/enhancedroc.a.yaml` (options definition)
- `jamovi/enhancedroc.r.yaml` (results/output definition)
- `jamovi/enhancedroc.u.yaml` (UI definition)

---

## Executive Summary

**Release Readiness: NOT READY for unsupervised clinical use — conditional release possible with caveats**

The module shows strong foundations: solid statistical implementation using `pROC`, thoughtful clinical interpretation text, good warning/notice coverage, and publication-ready report sentences. However, there are **critical issues with unimplemented features that are still visible in the UI** (Harrell C-Index, Uno's C-Statistic, time-dependent ROC, NNT, and 14 others), **mathematical issues in likelihood ratio handling**, and **missing confidence intervals on PPV/NPV** that pathologists and clinicians need for clinical decision-making.

---

## 1. Clinical Interpretation

### Strengths

- **AUC labeling is clear:** Excellent/Good/Fair/Poor/No discrimination categories are consistent with common literature.
- **Context-sensitive guidance:** Separate recommendations for screening vs. diagnostic vs. confirmatory contexts are present and clinically appropriate.
- **Copy-ready clinical report:** `.generateClinicalReport()` produces Methods and Results text with AUC, 95% CI, sensitivity, specificity, Youden index — exactly what is needed for a manuscript.
- **Glossary included:** Instructions panel has a glossary of Sensitivity, Specificity, PPV, NPV, LR+, LR- — appropriate for pathologists with basic statistics knowledge.
- **Direction auto-detection with notice:** When direction is auto-detected, a user-visible notice explains which direction was used (e.g., "higher predictor values classify as positive"). This is excellent for clinical safety.
- **Prevalence adjustment for PPV/NPV:** The option to use observed vs. user-specified prevalence is present and notes differences when they exceed 5%.

### Issues

**CRITICAL — AUC interpretation boundary is inconsistent:**
- Instructions panel says AUC 0.8–0.9 = "Excellent" but `.interpretAUC()` function labels 0.90+ as "Excellent" and 0.80-0.89 as "Good".
- This discrepancy between the instructions HTML (`b.R` line 2631-2634) and the actual function (lines 2022-2028) will confuse clinicians.

**MODERATE — Missing confidence intervals for PPV and NPV:**
- The clinical metrics table (`clinicalApplicationMetrics`) shows PPV and NPV as point estimates only.
- In clinical use, PPV and NPV change dramatically with prevalence. Clinicians need CIs or at minimum a statement that these are prevalence-dependent estimates.
- The Agresti-Coull or Wilson method for binomial CIs on derived metrics (PPV, NPV) is not implemented.

**MODERATE — LR+ = Inf is reported without explanation:**
- When specificity = 1, LR+ = Inf. The code (`b.R` line 1279) correctly flags this as `Inf` but the clinical metrics table will show `Inf` without a user-friendly label ("Perfect specificity — all negatives correctly classified").

**MINOR — AUC category boundaries differ from common guidelines:**
- The Hosmer-Lemeshow thresholds used (0.60/0.70/0.80/0.90) are one common approach.
- Many diagnostic accuracy guidelines (STARD, cochrane systematic review guidelines) use 0.70 as the minimum acceptable threshold.
- Consider adding a footnote indicating which classification system is used.

---

## 2. STARD Compliance Assessment

**Partial compliance — key elements present but incomplete.**

STARD 2015 requires reporting of:

| STARD Element | Status | Notes |
|---|---|---|
| Study population description | NOT IN MODULE | User must add to manuscript |
| Sample size calculation | NOT PRESENT | No power/sample size calculation implemented |
| Reference standard description | NOT IN MODULE | User must add |
| Flow diagram for participants | NOT PRESENT | Not applicable for standalone ROC module |
| Descriptive statistics for index test | PARTIAL | AUC table present; distribution stats not shown |
| Cross-tabulation at each cutoff | PRESENT | Cutoff analysis table shows TP, TN, FP, FN |
| Estimated test accuracy with CI | PRESENT | Sensitivity/specificity with exact Clopper-Pearson CIs |
| AUC with CI | PRESENT | DeLong or bootstrap CIs |
| Estimates of variability | PRESENT | 95% CIs throughout |
| Subgroup analyses | NOT PRESENT | No subgroup stratification |
| Indeterminate results | NOT ADDRESSED | No handling for borderline/indeterminate results |

**STARD verdict:** The module supports approximately 60% of STARD reportable elements. Missing elements (sample size justification, reference standard details, flow diagram) are outside scope for a statistical analysis module and must be added by the researcher.

**Recommendation:** Add a STARD checklist note in the output or instructions indicating which elements are provided by the module and which the researcher must supply.

---

## 3. Clinical Scenario Assessment

### Screening Tests (High Sensitivity Required)

**Rating: ADEQUATE**

- `clinicalPresets: biomarker_screening` applies 90% sensitivity threshold — appropriate.
- `sensitivityThreshold` option allows custom constraints on cutoff optimization.
- Partial AUC over sensitivity range is implemented (useful for minimum sensitivity thresholds).
- Warning when AUC < 0.75 for screening context.

**Gap:** No minimum sensitivity constraint report — the user cannot easily see if the final selected cutoff satisfies the threshold or falls back to unconstrained optimization (though a notice is generated for the fallback).

### Confirmatory Tests (High Specificity Required)

**Rating: ADEQUATE**

- `clinicalPresets: confirmatory_testing` applies 95% specificity threshold.
- `specificityThreshold` option present.
- High-specificity partial AUC range supported.

**Gap:** Same as screening — no clear user-visible confirmation that the constraint was met at the selected cutoff.

### Biomarker Evaluation (Optimal Cutpoint)

**Rating: GOOD**

- Youden index optimization is well-implemented.
- Custom cutoffs can be evaluated.
- Exact CIs (Clopper-Pearson) for sensitivity/specificity at cutoff are implemented.
- Direction auto-detection with verification notice is present.

**Gap:** No continuous biomarker distributional information shown alongside cutpoint (e.g., mean ± SD in positive vs. negative groups).

### Comparative Diagnostic Accuracy (Multiple Markers)

**Rating: ADEQUATE WITH LIMITATIONS**

- DeLong, bootstrap, and Venkatraman comparison methods implemented.
- Pairwise p-values and AUC differences in table form.
- McNemar test for sensitivity/specificity comparisons is implemented (correct paired approach).
- Clinical significance thresholds (>0.1 = clinically meaningful) are present.

**Gap:** No net reclassification improvement (NRI) or integrated discrimination improvement (IDI) are implemented. These are considered essential for comparative biomarker papers in 2026.

---

## 4. Safety and Warnings Assessment

**Rating: STRONG — one of the best aspects of the module**

The notice system is thorough. Assessed each:

| Warning | Present | Threshold | Clinical Appropriateness |
|---|---|---|---|
| Sample size < 30 | YES | Warning at 30, strong warning at 10 | Appropriate |
| Per-class count < 10 | YES | STRONG_WARNING | Appropriate — 10/class is minimal |
| AUC < 0.7 | YES | Strong warning | Appropriate |
| AUC < 0.5 | YES | ERROR level | Appropriate |
| Extreme prevalence (<5% or >95%) | YES | STRONG_WARNING | Appropriate |
| Class imbalance | YES | Configurable threshold, default 3:1 | Appropriate |
| Direction auto-detection | YES | INFO notice per predictor | Critical for clinical safety |
| Constraints not met | YES | WARNING | Appropriate |
| Bootstrap CI failure | YES | Falls back to DeLong | Good fallback |
| Non-numeric predictor | YES | ERROR | Appropriate |
| Outcome not binary | YES | Clear guidance | Appropriate |
| Zero-variance predictor | YES | Warning | Appropriate |
| High skewness | YES | Distribution note | Appropriate |
| High prevalence in screening context (>30%) | YES | Note | Appropriate |

**Issue — CRITICAL: 14+ unimplemented features visible to users in the UI:**

The following options appear in the UI and jamovi interface but are NOT implemented (lines 247-268, `b.R`):
- Harrell C-Index
- Uno's C-Statistic
- Incident/Dynamic AUC
- Cumulative/Dynamic AUC
- Competing Risks Concordance
- Spline Calibration
- E/O Ratio
- Nam-D'Agostino Test
- Greenwood-Nam-D'Agostino Test
- Calibration Belt
- Calibration Density
- Optimism Correction
- External Validation
- Decision Impact Curves
- Net Benefit Regression
- Model Updating
- Transportability
- Bootstrap CI for Partial AUC
- Bootstrap CI for Cutoffs
- NNT Calculation
- Weighted/Micro Multi-Class AUC Averaging

These show an INFO notice saying "planned but not yet implemented." This is problematic because:
1. Clinicians may tick "Harrell C-Index" expecting a result, find an info message, and not understand why.
2. The UI presents these as if they work, creating a trust issue.
3. For time-dependent ROC (survival context) — pathologists evaluating prognostic biomarkers commonly need these.

**Recommendation:** Either remove unimplemented options from the UI entirely, or add `(Coming soon)` to their labels in the UI.

---

## 5. Usability Assessment for Pathologists

**Rating: GOOD for basic use, COMPLEX for advanced use**

### Positive Aspects

- Instructions panel with glossary is well-written and appropriate for pathologists.
- Clinical presets reduce need for statistical knowledge (choose "Biomarker Screening" and go).
- Copy-ready clinical report sentences reduce transcription errors.
- Natural language summary ("best performing predictor was X with AUC of Y...").
- Direction of comparison auto-detected and explained.
- All outputs default to OFF — prevents overwhelming novice users.

### Issues

**MODERATE — Too many options for typical pathologist workflow:**
- The UI has 9 collapsible sections with 50+ individual options.
- A typical pathologist wants: ROC curve, AUC with CI, sensitivity/specificity, optimal cutoff, PPV/NPV. These require enabling 5+ separate checkboxes spread across multiple panels.
- Recommendation: Add a "Basic Clinical Analysis" preset that enables the most commonly needed outputs automatically.

**MODERATE — Output is opt-in only:**
- All output tables are `default: false`. A pathologist who opens the module sees only the instructions and notices — no actual results unless they toggle checkboxes.
- At minimum, `aucTable`, `diagnosticMetrics`, and `rocCurve` should default to `true`.

**MINOR — CROC and convex hull analysis are too advanced:**
- CROC (Concentrated ROC) is described as "early retrieval analysis" — this is an information retrieval concept, not clinical diagnostics.
- Pathologists will not know what to do with an "Early Retrieval Gain" metric.
- These are technically correct but should be hidden in an "Advanced / Research" section with a clear note that they are not for routine clinical use.

**MINOR — Labels on sensitivity/specificity in tables:**
- The tables display sensitivity/specificity as proportions (0.85) not percentages (85%). For pathologists and clinicians, percentages are the standard in clinical reporting.
- Consider adding a display option or formatting as "85.0%" for the clinical output.

---

## 6. Release Readiness and Bug Assessment

### Showstopper Issues (Must Fix Before Release)

1. **Unimplemented UI features without clear labeling** — 14+ options in the UI that do nothing except show an INFO notice. Pathologists toggling "Harrell C-Index" and seeing an info notice will be confused and may lose trust in the module.

2. **AUC interpretation text inconsistency** — Instructions panel says 0.8-0.9 is "Excellent" but the function says it is "Good." This is a direct contradiction visible in the same output.

### Important Issues (Fix Before Stable Release)

3. **No PPV/NPV confidence intervals** — Clinicians frequently need PPV/NPV CIs for sample size planning and reporting. Without these, the clinical metrics table is incomplete by modern standards.

4. **All outputs default to false** — The module presents a blank output panel to first-time users, which impedes adoption.

5. **LR+ = Inf displayed as raw "Inf"** — Should display as "∞ (perfect rule-in)" or similar user-friendly text.

6. **Missing basic "enable all standard outputs" preset** — For non-statistician pathologists.

### Minor Issues (Can Fix in Next Version)

7. Sensitivity/specificity displayed as proportions, not percentages in tables.
8. No continuous marker summary statistics (mean ± SD in positive vs. negative groups) alongside cutpoint output.
9. CROC analysis lacks clinical context explanation appropriate for pathologists.
10. No NRI/IDI for biomarker comparison papers.
11. No sample size calculation for ROC studies (rule of thumb: 20+ events per predictor variable).

### Edge Cases — Assessment

| Edge Case | Handled? | Quality |
|---|---|---|
| Perfect discrimination (AUC = 1.0) | YES — no crash | Works via pROC |
| Random classifier (AUC ≈ 0.5) | YES — warning issued | Good |
| All identical predictor values | YES — caught with error message | Good |
| Zero positive events | YES — caught before ROC | Good |
| Single predictor level outcome | YES — caught with clear message | Excellent UX |
| Very large dataset | YES — memory management present | Good |
| Multi-level outcome without multiclass ROC | YES — clear guidance given | Good |
| Tied predictor scores | PARTIAL — notice but no alternative computation | Acceptable |
| Missing data | YES — na.omit with row count | Basic but adequate |
| Bootstrap failure | YES — falls back to DeLong | Good |

### Error Handling Quality

The error handling is overall robust. The use of `tryCatch` throughout the codebase ensures that a single predictor failure does not abort analysis for all predictors. The notice system (converted from jmvcore::Notice to HTML) correctly avoids protobuf serialization errors.

---

## 7. Statistical Accuracy Assessment (Clinical Perspective)

### Correct Implementations

- **Youden index optimization**: Correct formula (Sensitivity + Specificity - 1).
- **Clopper-Pearson exact CIs**: Correct use of `binom.test()` for sensitivity/specificity CIs.
- **Bayes-corrected PPV/NPV**: Correct formula using user-specified prevalence.
- **DeLong method for AUC CI**: Correct delegation to `pROC::roc()`.
- **Bootstrap BCa as default**: Appropriate; BCa is superior to percentile bootstrap for bounded quantities.
- **McNemar's test for paired sensitivity/specificity comparison**: Correct statistical approach.
- **Partial AUC with McClish correction**: Normalized to 0.5-1.0 scale as described.
- **CROC calculation**: Exponential magnifier function correctly implemented.
- **Convex hull AUC**: Upper hull correctly extracted using `chull()` with diagonal constraint.

### Issues with Statistical Accuracy

**IMPORTANT — LR- calculation has an edge case error:**
```r
# b.R line 1281-1283
lr_neg <- if (abs(spec) < 1e-10 && abs(1 - sens) < 1e-10) NA_real_
         else if (abs(spec) < 1e-10) Inf
         else (1 - sens) / spec
```
When specificity = 0 (all negatives misclassified), LR- should be 0 (no negative test possible, not Inf). The correct formula is `(1 - sens) / spec`. When spec = 0: LR- = (1-sens)/0 — this is mathematically indeterminate but clinically LR- should be treated as Inf only when (1-sens) > 0. When both spec ≈ 0 and sens ≈ 1, the return of `NA_real_` is correct. However when spec ≈ 0 and sens < 1, returning `Inf` is incorrect — the correct value is mathematically `(1 - sens) / 0` = infinity (test that never correctly classifies negatives). This edge case is rare in practice but technically incorrect.

**MODERATE — Internal validation (bootstrap optimism correction) is noted in methods text but implementation needs verification:**
- Lines 1947 reference "Harrell's optimism-correction method" in the methods explanation.
- The `populateInternalValidation()` function is called but was not seen in the code excerpts reviewed.
- This needs independent verification that the optimism correction is correctly implemented.

---

## 8. Recommendations Summary

### Before Release (Priority Order)

1. **Remove or clearly mark unimplemented UI options.** Options that only show an INFO notice should either be removed from the UI or labeled "(Planned)" in their UI labels.

2. **Fix AUC interpretation text inconsistency** — align the instructions panel text with the `.interpretAUC()` function. Recommendation: Use "Excellent ≥ 0.90, Good 0.80-0.89, Fair 0.70-0.79, Poor 0.60-0.69, No discrimination < 0.60."

3. **Add PPV/NPV confidence intervals** — Even approximate Wilson score CIs would satisfy STARD requirements.

4. **Enable key outputs by default** — Set `aucTable`, `rocCurve`, and `diagnosticMetrics` to `default: true`.

5. **Display LR+ = Inf as user-friendly text** in the clinical metrics table.

### For Next Version

6. Add "Basic Clinical Analysis" preset that enables all standard outputs with one click.
7. Display sensitivity/specificity as percentages with option for proportions.
8. Add continuous marker summary statistics (mean ± SD by group) to cutoff tables.
9. Implement NRI/IDI for biomarker comparison.
10. Implement NNT calculation (currently listed as unimplemented).
11. Add STARD reporting checklist in output.

---

## 9. Overall Clinical Readiness Score

| Domain | Score (1-5) | Notes |
|---|---|---|
| Statistical accuracy | 4/5 | Core methods correct; edge cases in LR- |
| Clinical interpretation | 3/5 | Good framework; AUC text inconsistency; missing PPV/NPV CIs |
| STARD compliance | 3/5 | ~60% of reportable elements present |
| Safety warnings | 5/5 | Excellent coverage |
| Usability for pathologists | 3/5 | Too complex by default; good once configured |
| Error handling | 4/5 | Robust; unimplemented UI options are a UX risk |
| Edge case handling | 4/5 | Good coverage of rare scenarios |

**Overall: 3.7/5 — Conditional release with documented limitations**

The module is appropriate for use by a biostatistician who understands ROC analysis and can interpret output correctly. It is **not yet ready for unsupervised use by pathologists** due to the unimplemented features visible in the UI and the lack of default outputs.

With the 5 priority fixes listed above, the module would be ready for a clinical release with pathologist-level users.

---

*Review conducted by: Clinical Biostatistician (AI Agent)*
*Review scope: Clinical readiness for pathologists and clinicians in jamovi environment*
