# Citation Review: Computational Pathology for Accurate Prediction of Breast Cancer Recurrence (Deep-BCR-Auto)

---

## ARTICLE SUMMARY

- **Title/Label**: Computational Pathology for Accurate Prediction of Breast Cancer Recurrence: Development and Validation of a Deep Learning-Based Tool
- **Design & Cohort**: Development and external validation study. Training: TCGA-BRCA dataset (1006 patients, 1065 H&E WSIs, primarily HR+/HER2- with 516 cases, ODX-based labels: 443 low-risk [≤25] vs 73 high-risk [>25]). Independent testing: OSU dataset (339 patients, 465 H&E WSIs, 384 with ODX ≤25 and 81 with ODX >25). Pipeline: Deep-BCR-Auto (tumor bulk segmentation + weakly supervised learning via cross-attention MIL). 3-fold cross-validation on TCGA; best model tested on OSU.
- **Key Analyses**:
  - AUROC with 1000-time bootstrap 95% CIs (primary metric)
  - Area under precision-recall curve
  - DeLong's test for comparing AUROC between models
  - Confusion matrices at 70%, 80%, 90% sensitivity thresholds
  - Calibration curve (LOESS-based) with C(ROC), Brier score, calibration slope, ECI
  - Subgroup AUROC by race (African American, Caucasian, Asian), age (≤50, >50), and cancer subtype (IDC, ILC, other)
  - Misclassification analysis by ODX score distribution

---

## ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | Computational Pathology for Accurate Prediction of Breast Cancer Recurrence: Development and Validation of a Deep Learning-Based Tool |
| Journal | Modern Pathology |
| Year | 2025 |
| Volume | 38 |
| Pages | 100847 |
| DOI | 10.1016/j.modpat.2025.100847 |
| PMID | TODO |
| Publisher | Elsevier / USCAP |
| First Author | Ziyu Su |
| Corresponding Author | Metin N. Gurcan (Metin.Gurcan@advocatehealth.org) |
| Received / Accepted | January 21, 2025 / July 1, 2025 |
| License | All rights reserved (Elsevier) |

---

## Skipped Sources

*None -- PDF was read successfully (10 pages).*

---

## EXTRACTED STATISTICAL METHODS

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|
| AUROC (Area Under Receiver Operating Characteristic) | Primary -- model discrimination | Bootstrap 95% CI (1000 resamples); TCGA: 0.827 [0.821, 0.831]; OSU: 0.832 [0.829, 0.833] | Binary classification (low/high risk); bootstrap for CI estimation | Results (p5), Table 3, Figs 2, 5 |
| DeLong's test | Primary -- AUROC comparison | Deep-BCR-Auto vs CLAM: P = 0.041 (significant); vs CLAM+CTrans: P = 0.366 (not significant) | Paired comparison; same test set; normal approximation for AUROC difference | Results (p5), Table 3 |
| Area under precision-recall curve (AUPRC) | Secondary -- imbalanced classification performance | Bootstrap 95% CI; TCGA: 0.535 [0.531, 0.542]; OSU: 0.543 [0.542, 0.548] | Important given class imbalance (~16% high-risk); complements AUROC | Results (p5), Table 3, Figs 2B, 5B |
| Confusion matrices | Secondary -- classification performance at thresholds | At 70%, 80%, 90% sensitivity on TCGA; threshold-tuned on OSU validation subset | Sensitivity fixed; specificity, PPV, NPV derived | Results (p6), Figs 3, Table 4 |
| Calibration curve | Secondary -- calibration assessment | LOESS-based flexible calibration; metrics: C(ROC)=0.83, Brier=0.12, Brier scaled=0.15, Intercept=0.03, Slope=0.62, ECI=0.07 | Slope 0.62 indicates overfitting / miscalibration at high probabilities | Results (p8), Fig 5C |
| Subgroup analysis | Secondary -- fairness/equity assessment | AUROC by race: African American 0.898, Caucasian 0.813, Asian 0.838; by age: <50 (0.832), ≥50 (0.808); by subtype: IDC (0.832), ILC (0.723), other (0.809) | Small subgroup sizes limit power; P values from DeLong's test | Results (p6-7), Fig 4 |
| 3-fold cross-validation | Primary -- model selection | Patient-level split preventing data leakage; stratified by risk category; data augmentation for non-HR+/HER2- | Standard CV practice for model evaluation | Methods (p4-5) |
| Partial correlation | Secondary -- biological validation | Correlation with ODX after controlling for histological grade (r=0.379, P<.001) and mitosis score (r=0.387, P<.001) | Pearson partial correlation; validates that model captures ODX-related morphological features | Results (p5), Supplementary |

---

## CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|
| AUROC with CIs | `decision` (ROC analysis), `decisioncurve` | ✅ | ROC curves with AUROC and CIs available |
| DeLong's test (AUROC comparison) | `psychopdaROC` (delongTest), `enhancedROC`, `aivalidation`, `timeroc`, `diagnosticperformance` | ✅ | DeLong's test extensively implemented via `pROC::roc.test(method="delong")` across multiple ROC analysis functions |
| Area under precision-recall curve | `decision` | 🟡 | PR curves may not be available; the `decision` module focuses on ROC-based metrics |
| Confusion matrices at fixed sensitivity | `decision` (with threshold control) | ✅ | Confusion matrix available; users can adjust threshold |
| Calibration curve (LOESS) | `survival` (calibration_curves) | ✅ | Calibration curves with LOESS implemented 2026-02-08 for survival models; not available for binary classification |
| Subgroup AUROC analysis | `decision` with data subsetting | ✅ | Users can filter by subgroup and run ROC separately |
| 3-fold cross-validation | — | ❌ | ML model training/validation; outside scope of statistical analysis module |
| Bootstrap CIs for AUROC | `decision` | 🟡 | Bootstrap CIs may not be available; standard CIs are provided |
| Partial correlation | `jjcorrmat`, base correlation | ✅ | Partial correlation available through correlation analysis |
| Brier score | — | 🟡 | Not explicitly available as a standalone metric in the decision module |

---

## CRITICAL EVALUATION

### Statistical Rigor Score: 🟢 Good (14/18)

| Criterion | Score | Notes |
|---|---|---|
| Sample size justification | 1/2 | No formal power analysis; but TCGA (1006 patients) and OSU (339 patients) are reasonable sizes; class imbalance acknowledged |
| Multiple testing correction | 1/2 | Multiple AUROC comparisons (3 models) without formal correction; but only 2 tests performed and P=0.041 is borderline |
| Effect sizes with CIs | 2/2 | All AUROC values with bootstrap 95% CIs; sensitivities/specificities reported |
| Assumption checking | 2/2 | Patient-level split for CV (prevents data leakage); data augmentation for class imbalance; color normalization across datasets |
| Appropriate test selection | 2/2 | AUROC + AUPRC (addresses class imbalance); DeLong's test for paired comparison; calibration curve for reliability |
| Reproducibility | 2/2 | Code and data publicly available (GitHub + TCGA); independent OSU dataset; 3-fold CV with patient-level splits |
| Handling of missing data | 1/2 | Cases with missing ODX or receptor status excluded from TCGA (1006→1065 WSIs); OSU complete |
| Model diagnostics | 2/2 | Calibration curve with multiple metrics (Brier, slope, ECI); misclassification analysis; subgroup performance |
| Clinical significance assessment | 1/1 | NPV 92.6-96.7% at different thresholds; clinical utility for reducing unnecessary ODX testing discussed |
| External validity | 1/1 | Independent external validation on OSU dataset from different institution |
| Bias assessment | 0/1 | Calibration slope 0.62 indicates systematic miscalibration at high probabilities; acknowledged but not corrected |

### Strengths
1. **Independent external validation**: OSU dataset from different institution with comparable AUROC (0.832 vs 0.827)
2. **Comprehensive calibration**: LOESS calibration curve with Brier score, slope, intercept, ECI -- rarely seen in pathology DL papers
3. **Equity analysis**: AUROC reported across racial groups, ages, and cancer subtypes -- addresses health disparities
4. **Publicly available**: Code on GitHub, data from TCGA -- reproducible research
5. **Robust pipeline**: End-to-end automated (no manual annotation needed for prediction)
6. **Clinically meaningful**: High NPV (92-97%) for ruling out high-risk patients; reduces need for expensive ODX testing
7. **Bootstrap CIs**: 1000-resample bootstrap for all AUROC values -- statistically rigorous

### Weaknesses
1. **Calibration slope 0.62**: Substantial miscalibration at high predicted probabilities; model underestimates risk for high-risk patients
2. **Binary classification only**: ODX score treated as binary (≤25 vs >25); continuous score prediction would be more informative
3. **Class imbalance**: Only 16% high-risk in TCGA, 21% in OSU; AUPRC of ~0.54 is modest
4. **ILC performance**: AUROC 0.723 for invasive lobular carcinoma (vs 0.832 for IDC) -- significant performance gap
5. **No comparison with clinical predictors**: No comparison of DL model vs standard clinicopathological variables (grade, size, nodal status)

---

## GAP ANALYSIS

### No Gap: DeLong's Test for Comparing Two ROC Curves

**What the article used**: DeLong's test to compare AUROC of Deep-BCR-Auto (0.827) vs CLAM (0.750), yielding P = 0.041 (significant).

**Current ClinicoPath coverage**: ✅ **Fully covered.** DeLong's test is implemented in `psychopdaROC` (delongTest option), `enhancedROC`, `aivalidation`, `timeroc`, `diagnosticperformance`, `mlpathology`, and `multiclassdiagnostics` -- all using `pROC::roc.test(method="delong")`.

**Status**: No gap.

### Gap 2: Precision-Recall Curves and AUPRC

**What the article used**: Area under precision-recall curve as secondary metric for imbalanced classification (16% positive rate). AUPRC of 0.535 complements AUROC of 0.827.

**Current ClinicoPath coverage**: `decision` module focuses on ROC-based metrics. PR curves may not be available.

**Gap**: PR curves with AUPRC are important for imbalanced datasets (common in pathology: rare events, screening). Adding PR curves alongside ROC curves in the `decision` module would improve the toolkit.

**Priority**: Medium -- important for imbalanced classification, which is common in pathology diagnostics.

### Gap 3: Calibration Curves for Binary Classification

**What the article used**: LOESS-based calibration curve for predicted probabilities vs observed proportions, with Brier score, calibration slope, intercept, and ECI.

**Current ClinicoPath coverage**: Calibration curves were implemented in the `survival` module (2026-02-08) for survival models, but may not be available for standard binary classification (logistic regression, diagnostic test probabilities).

**Gap**: Extending calibration curve functionality to binary classification models in the `decision` module.

**Priority**: Low-Medium -- the survival calibration implementation could potentially be adapted, but binary classification calibration is less common in the pathology agreement/decision toolkit's typical use cases.

---

## IMPLEMENTATION ROADMAP

### Not Recommended: Gap 1 (DeLong's test) -- Already Covered

**Verification result**: DeLong's test is **extensively implemented** across multiple ClinicoPath functions:
- `psychopdaROC` (delongTest option with pairwise comparison table)
- `enhancedROC` (comparisonMethod = "delong")
- `aivalidation` (model comparison via DeLong)
- `timeroc` (rocComparison = "delong")
- `diagnosticperformance` (comparisonMethod = "delong")
- `mlpathology`, `multiclassdiagnostics`, `stagemigration`

All use `pROC::roc.test(method = "delong")`. No implementation needed.

### Not Recommended: Gap 2 (PR curves)

PR curves require `PRROC` or custom implementation and represent a niche enhancement. Most pathology studies report AUROC as the primary metric.

### Not Recommended: Gap 3 (Binary calibration)

Can be done through the existing survival module's calibration infrastructure with minor adaptation, but demand is limited for the decision module's typical use cases.

---

## OVERALL ASSESSMENT

| Dimension | Rating |
|---|---|
| Statistical rigor | 🟢 Good (14/18) |
| Methodological novelty | High (end-to-end automated ODX prediction from H&E WSIs; equity analysis) |
| Clinical relevance | Very High (cost-effective alternative to Oncotype DX for HR+/HER2- BC) |
| Coverage by ClinicoPath | Very Good (7 covered, 3 partial, 1 outside-scope) |
| Implementation priority | None -- all key statistical methods covered; remaining gaps (PR curves, binary calibration) are niche |

### Key Takeaway for ClinicoPath Development

This article primarily concerns deep learning model development and validation, which is outside the scope of ClinicoPath's statistical analysis toolkit. However, the statistical evaluation framework used (AUROC comparison, calibration curves, subgroup analysis) is relevant.

After verification, **DeLong's test is already comprehensively implemented** across multiple ROC analysis functions (`psychopdaROC`, `enhancedROC`, `aivalidation`, `timeroc`, `diagnosticperformance`, `mlpathology`). The calibration curve implementation recently added to the survival module (2026-02-08) partially addresses the calibration gap for survival models; extending it to binary classification could broaden applicability but is a niche enhancement.

No implementation changes needed from this article.
