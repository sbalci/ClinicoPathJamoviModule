# Testing psychopdaROC Function

All data files are available in `data/` folder as `.rda`, `.csv`, `.xlsx`, and `.omv`. Open the `.omv` (or `.csv`) in jamovi.

---

## 1. BASIC ROC ANALYSIS (Default Cutpoint)

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 1 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | Default `maximize_metric` with `youden`, direction `>=`. Verify resultsTable (cutpoint, sens, spec, AUC), aucSummaryTable, simpleResultsTable |
| 2 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `sensSpecTable`: on (confusion matrix), `showThresholdTable`: on (`maxThresholds`: 20) |
| 3 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `direction`: `<=` (lower = positive), verify cutpoint changes appropriately |

**Options covered:** `dependentVars`, `classVar`, `positiveClass`, `direction` (>=, <=), `method` (maximize_metric), `metric` (youden), `sensSpecTable`, `showThresholdTable`, `maxThresholds`

---

## 2. CUTPOINT OPTIMIZATION METHODS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 4 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | Cycle through `metric`: youden, sum_sens_spec, accuracy, sum_ppv_npv, prod_sens_spec, prod_ppv_npv, cohens_kappa, F1_score |
| 5 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `metric`: abs_d_sens_spec (minimize |Sens-Spec|), then odds_ratio, risk_ratio, roc01, p_chisquared |
| 6 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: minimize_metric, `metric`: misclassification_cost |
| 7 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: maximize_loess_metric, `metric`: youden |
| 8 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: minimize_loess_metric, `metric`: abs_d_sens_spec |
| 9 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: maximize_boot_metric, `boot_runs`: 200, `seed`: 123 |
| 10 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: minimize_boot_metric, `boot_runs`: 200 |
| 11 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: oc_youden_kernel |
| 12 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: oc_youden_normal |
| 13 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: oc_manual, `specifyCutScore`: "7.5" |
| 14 | `psychopdaROC_costbenefit` | outcome / "Event" | risk_score | `method`: oc_cost_ratio, `costratioFP`: 0.1 (FN costs 10x more) |
| 15 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: oc_equal_sens_spec |
| 16 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `method`: oc_closest_01 |

**Options covered:** All 12 `method` values (maximize_metric, minimize_metric, maximize_loess_metric, minimize_loess_metric, maximize_boot_metric, minimize_boot_metric, oc_youden_kernel, oc_youden_normal, oc_manual, oc_cost_ratio, oc_equal_sens_spec, oc_closest_01), all 16 `metric` values, `specifyCutScore`, `costratioFP`, `boot_runs`, `seed`

---

## 3. CUTPOINT FINE-TUNING

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 17 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `tol_metric`: 0.05 then 0.01 then 0.10 — verify cutpoint stability |
| 18 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `break_ties`: mean then median then c (report all) |
| 19 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `allObserved`: on — verify all observed values shown as potential cutpoints |

**Options covered:** `tol_metric`, `break_ties` (mean/median/c), `allObserved`

---

## 4. MULTIPLE TEST VARIABLES (Comparative)

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 20 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3, combined_score | `plotROC`: on, `combinePlots`: on. Verify overlay ROC plot with all markers |
| 21 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `delongTest`: on. Verify delongComparisonTable with pairwise AUC comparisons |
| 22 | `psychopdaROC_cardiac` | mi_status / "MI" | troponin, creatinine, bnp | `combinePlots`: on, `delongTest`: on. Verify 3-way comparison |

**Options covered:** multiple `dependentVars`, `combinePlots`, `delongTest`

---

## 5. ROC VISUALIZATION OPTIONS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 23 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `plotROC`: on, `showOptimalPoint`: on, `showConfidenceBands`: on |
| 24 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `smoothing`: on, `displaySE`: on (SE bands on smoothed curve) |
| 25 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `cleanPlot`: on (publication-ready, no annotations) |
| 26 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `combinePlots`: on, `legendPosition`: right then bottom then topleft then topright then none |
| 27 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `directLabel`: on (labels on curves instead of legend) |
| 28 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `interactiveROC`: on — verify interactive HTML plot |

**Options covered:** `plotROC`, `showOptimalPoint`, `displaySE`, `smoothing`, `showConfidenceBands`, `cleanPlot`, `legendPosition` (none/right/bottom/topleft/topright), `directLabel`, `interactiveROC`

---

## 6. DIAGNOSTIC PLOTS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 29 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `showCriterionPlot`: on — Sensitivity/Specificity vs Threshold |
| 30 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `showPrevalencePlot`: on — PPV/NPV vs Prevalence |
| 31 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `showDotPlot`: on — Test value distribution by class |
| 32 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `precisionRecallCurve`: on — Precision-Recall curve |

**Options covered:** `showCriterionPlot`, `showPrevalencePlot`, `showDotPlot`, `precisionRecallCurve`

---

## 7. PREVALENCE & PRIOR SETTINGS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 33 | `psychopdaROC_rare` | rare_disease / "Disease" | biomarker | `usePriorPrev`: on, `priorPrev`: 0.05 (5% population prevalence). Verify PPV/NPV adjustment |
| 34 | `psychopdaROC_rare` | rare_disease / "Disease" | biomarker | `usePriorPrev`: off — verify sample prevalence used instead |

**Options covered:** `usePriorPrev`, `priorPrev`

---

## 8. SUBGROUP ANALYSIS

| # | File | Class Var / Positive Class | Test Variables | Subgroup | Options to Test |
|---|------|----------------------------|----------------|----------|-----------------|
| 35 | `psychopdaROC_subgroup` | disease / "Disease" | test_score | age_group | Verify separate ROC curves and results per subgroup |
| 36 | `psychopdaROC_subgroup` | disease / "Disease" | test_score | sex | Different grouping variable |
| 37 | `psychopdaROC_large` | disease_status / "Disease" | biomarker1, biomarker2 | site | Large dataset with site-based subgroups |

**Options covered:** `subGroup`

---

## 9. ADVANCED ROC ANALYSIS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 38 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `partialAUC`: on, `partialAUCfrom`: 0.8, `partialAUCto`: 1.0. Verify partialAUCTable |
| 39 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `partialAUCfrom`: 0.9, `partialAUCto`: 1.0 (high specificity region) |
| 40 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `rocSmoothingMethod`: binormal then density then fitdistr |
| 41 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `bootstrapCI`: on, `bootstrapReps`: 2000. Verify bootstrapCITable |
| 42 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `quantileCIs`: on, `quantiles`: "0.1,0.25,0.5,0.75,0.9" |

**Options covered:** `partialAUC`, `partialAUCfrom`, `partialAUCto`, `rocSmoothingMethod` (none/binormal/density/fitdistr), `bootstrapCI`, `bootstrapReps`, `quantileCIs`, `quantiles`

---

## 10. FIXED SENSITIVITY / SPECIFICITY ANALYSIS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 43 | `psychopdaROC_screening` | cancer / "Cancer" | psa_level | `fixedSensSpecAnalysis`: on, `fixedAnalysisType`: sensitivity, `fixedSensitivityValue`: 0.95. Verify fixedSensSpecTable |
| 44 | `psychopdaROC_screening` | cancer / "Cancer" | psa_level | `fixedAnalysisType`: specificity, `fixedSpecificityValue`: 0.90 |
| 45 | `psychopdaROC_screening` | cancer / "Cancer" | psa_level | `fixedInterpolation`: linear then nearest then stepwise |
| 46 | `psychopdaROC_screening` | cancer / "Cancer" | psa_level | `showFixedROC`: on, `showFixedExplanation`: on |

**Options covered:** `fixedSensSpecAnalysis`, `fixedAnalysisType` (sensitivity/specificity), `fixedSensitivityValue`, `fixedSpecificityValue`, `fixedInterpolation` (linear/nearest/stepwise), `showFixedROC`, `showFixedExplanation`

---

## 11. MODEL COMPARISON (IDI / NRI)

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 47 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `calculateIDI`: on, `refVar`: marker1, `idiNriBootRuns`: 1000. Verify idiTable |
| 48 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `calculateNRI`: on, `refVar`: marker1, `nriThresholds`: "0.3,0.7". Verify nriTable |
| 49 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `calculateNRI`: on, `nriThresholds`: "" (continuous NRI, no thresholds) |
| 50 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `compareClassifiers`: on. Verify rocComparisonTable (AUC, AUPRC, Brier, F1, accuracy, balanced accuracy) |

**Options covered:** `calculateIDI`, `calculateNRI`, `refVar`, `nriThresholds`, `idiNriBootRuns`, `compareClassifiers`

---

## 12. EFFECT SIZE & POWER ANALYSIS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 51 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3 | `effectSizeAnalysis`: on. Verify effectSizeTable (Cohen's d, Glass' delta, Hedges' g) and effectSizePlot |
| 52 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `powerAnalysis`: on, `powerAnalysisType`: post_hoc, `significanceLevel`: 0.05, `targetPower`: 0.80 |
| 53 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `powerAnalysisType`: prospective, `expectedAUCDifference`: 0.10 |
| 54 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `powerAnalysisType`: sample_size, `correlationROCs`: 0.5. Verify powerAnalysisTable and powerCurvePlot |

**Options covered:** `effectSizeAnalysis`, `powerAnalysis`, `powerAnalysisType` (post_hoc/prospective/sample_size), `expectedAUCDifference`, `targetPower`, `significanceLevel`, `correlationROCs`

---

## 13. BOOTSTRAP ROC WITH PRIOR WEIGHTING

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 55 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `bayesianAnalysis`: on, `priorAUC`: 0.7, `priorPrecision`: 10. Verify bayesianROCTable and bayesianTracePlot |
| 56 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `priorAUC`: 0.9 (strong prior belief), `priorPrecision`: 50 (high confidence) — verify prior influence metric |

**Options covered:** `bayesianAnalysis`, `priorAUC`, `priorPrecision`

---

## 14. CLINICAL UTILITY ANALYSIS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 57 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `clinicalUtilityAnalysis`: on, `treatmentThreshold`: "0.05,0.5,0.05", `harmBenefitRatio`: 0.25. Verify clinicalUtilityTable and decisionCurvePlot |
| 58 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `interventionCost`: on, `harmBenefitRatio`: 1.0 |

**Options covered:** `clinicalUtilityAnalysis`, `treatmentThreshold`, `harmBenefitRatio`, `interventionCost`

---

## 15. META-ANALYSIS (3+ Variables Required)

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 59 | `psychopdaROC_advanced` | diagnosis / "Positive" | marker_excellent, marker_good, marker_moderate, marker_fair, marker_poor | `metaAnalysis`: on, `metaAnalysisMethod`: both, `heterogeneityTest`: on. Verify metaAnalysisTable, metaAnalysisWarning |
| 60 | `psychopdaROC_advanced` | diagnosis / "Positive" | marker_excellent, marker_good, marker_moderate | `metaAnalysisMethod`: fixed then random |
| 61 | `psychopdaROC_advanced` | diagnosis / "Positive" | marker_excellent, marker_good, marker_moderate, marker_fair, marker_poor | `forestPlot`: on. Verify metaAnalysisForestPlot |
| 62 | `psychopdaROC_advanced` | diagnosis / "Positive" | marker_excellent, marker_good, marker_moderate | `overrideMetaAnalysisWarning`: on — verify warning bypass |

**Options covered:** `metaAnalysis`, `metaAnalysisMethod` (fixed/random/both), `heterogeneityTest`, `forestPlot`, `overrideMetaAnalysisWarning`

---

## 16. CLINICAL MODE PRESETS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 63 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `clinicalMode`: basic. Verify minimal output (simpleResultsTable, aucSummaryTable, resultsTable) |
| 64 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `clinicalMode`: advanced. Verify additional tables appear |
| 65 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `clinicalMode`: comprehensive. Verify full research-grade output, clinicalInterpretationTable |
| 66 | `psychopdaROC_screening` | cancer / "Cancer" | psa_level | `clinicalPreset`: screening — verify high-sensitivity optimization |
| 67 | `psychopdaROC_cardiac` | mi_status / "MI" | troponin | `clinicalPreset`: confirmation — verify high-specificity optimization |
| 68 | `psychopdaROC_test` | disease_status / "Disease" | biomarker | `clinicalPreset`: balanced, then research |

**Options covered:** `clinicalMode` (basic/advanced/comprehensive), `clinicalPreset` (none/screening/confirmation/balanced/research)

---

## 17. EDGE CASES

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 69 | `psychopdaROC_perfect` | condition / "Positive" | perfect_test | Perfect separation (AUC=1.0). Verify handling and any warnings |
| 70 | `psychopdaROC_poor` | status / "Positive" | poor_marker | No discrimination (AUC~0.5). Verify appropriate warnings |
| 71 | `psychopdaROC_overlap` | diagnosis / "Positive" | test_value | Moderate overlap (AUC~0.70). Verify standard results |
| 72 | `psychopdaROC_small` | class / "Positive" | marker | Small sample (n=30). Verify sample size warnings and stability |
| 73 | `psychopdaROC_imbalanced` | rare_outcome / "Positive" | predictor | Extreme imbalance (2% event rate). Verify imbalance notices |
| 74 | `psychopdaROC_constant` | outcome / "Positive" | constant_marker | Constant predictor (all=50). Verify proper error handling |
| 75 | `psychopdaROC_missing` | diagnosis / "Positive" | test_a, test_b | Missing data (~5-10%). Verify listwise deletion and notes |
| 76 | `psychopdaROC_rare` | rare_disease / "Disease" | biomarker | Rare disease (5% prevalence). Verify prevalence-related warnings |
| 77 | `psychopdaROC_large` | disease_status / "Disease" | biomarker1, biomarker2 | Large dataset (n=500). Verify scaling and performance |

**Options covered:** AUC=1.0, AUC=0.5, small n, extreme imbalance, constant predictor, missing data, rare disease, large n

---

## 18. TIME-DEPENDENT & SPECTRUM ANALYSIS

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 78 | `psychopdaROC_timedep` | outcome / "Event" | baseline_marker, followup_marker | Compare baseline vs follow-up marker. `delongTest`: on, `combinePlots`: on |
| 79 | `psychopdaROC_spectrum` | binary_status / "Positive" | continuous_marker | Disease spectrum data. Default analysis. Subgroup: severity |

**Options covered:** longitudinal marker comparison, disease spectrum scenarios

---

## 19. COMPREHENSIVE PUBLICATION-READY SCENARIO

| # | File | Class Var / Positive Class | Test Variables | Options to Test |
|---|------|----------------------------|----------------|-----------------|
| 80 | `psychopdaROC_multibiomarker` | diagnosis / "Positive" | marker1, marker2, marker3, combined_score | `clinicalMode`: comprehensive, `method`: maximize_metric, `metric`: youden, `delongTest`: on, `calculateIDI`: on, `calculateNRI`: on, `refVar`: marker1, `effectSizeAnalysis`: on, `bootstrapCI`: on, `bootstrapReps`: 2000, `clinicalUtilityAnalysis`: on, `plotROC`: on, `combinePlots`: on, `cleanPlot`: on, `sensSpecTable`: on. Verify all tables and plots appear correctly together |

---

## TEST DATA FILES

| File | N | Class Levels | Prevalence | Key Features | Primary Use |
|------|---|--------------|------------|--------------|-------------|
| `psychopdaROC_test` | 200 | 2 (Healthy/Disease) | ~50% | 1 biomarker, age, sex | Basic single-marker testing |
| `psychopdaROC_screening` | 250 | 2 (No Cancer/Cancer) | ~25% | PSA, CA125, age, risk factors | Cancer screening |
| `psychopdaROC_cardiac` | 180 | 2 (No MI/MI) | ~30% | Troponin, creatinine, BNP | Cardiac biomarkers |
| `psychopdaROC_multibiomarker` | 220 | 2 (Negative/Positive) | ~30% | 3 markers + combined score | Multi-marker comparison |
| `psychopdaROC_subgroup` | 200 | 2 (No Disease/Disease) | ~30% | test_score, age_group, sex | Subgroup analysis |
| `psychopdaROC_perfect` | 100 | 2 (Negative/Positive) | 50% | Perfect separation | AUC=1.0 edge case |
| `psychopdaROC_poor` | 150 | 2 (Negative/Positive) | ~50% | No discrimination | AUC~0.5 edge case |
| `psychopdaROC_overlap` | 190 | 2 (Negative/Positive) | ~50% | Moderate overlap | AUC~0.70 |
| `psychopdaROC_rare` | 300 | 2 (No Disease/Disease) | 5% | Single biomarker | Rare disease |
| `psychopdaROC_costbenefit` | 160 | 2 (No Event/Event) | ~30% | Risk score, cost columns | Cost-benefit optimization |
| `psychopdaROC_spectrum` | 170 | 3 severity + binary | varies | Continuous marker | Disease spectrum |
| `psychopdaROC_timedep` | 140 | 2 (No Event/Event) | ~30% | Baseline + follow-up markers | Longitudinal comparison |
| `psychopdaROC_small` | 30 | 2 (Negative/Positive) | ~50% | Single marker | Small sample edge case |
| `psychopdaROC_imbalanced` | 200 | 2 (No Event/Event) | 2% | Single predictor | Extreme imbalance |
| `psychopdaROC_missing` | 150 | 2 (Negative/Positive) | ~30% | 2 tests + covariate, 5-10% NA | Missing data handling |
| `psychopdaROC_constant` | 80 | 2 (No Event/Event) | ~50% | Constant marker (all=50) | Zero variance edge case |
| `psychopdaROC_large` | 500 | 2 (No Disease/Disease) | ~30% | 2 biomarkers, age, sex, site | Large sample / subgroups |
| `psychopdaROC_advanced` | 250 | 2 (Negative/Positive) | ~30% | 5 markers (AUC 0.65-0.90), site | Meta-analysis / advanced |

---

## COMPLETE OPTION COVERAGE CHECKLIST

### Core Input
- [x] `dependentVars` — all tests
- [x] `classVar` — all tests
- [x] `positiveClass` — all tests
- [x] `subGroup` — #35, #36, #37, #79
- [x] `direction` (>=, <=) — #1, #3

### Clinical Mode & Presets
- [x] `clinicalMode` (basic/advanced/comprehensive) — #63, #64, #65
- [x] `clinicalPreset` (none/screening/confirmation/balanced/research) — #66, #67, #68

### Cutpoint Optimization
- [x] `method` — all 12 methods: #4-#16
  - maximize_metric — #4
  - minimize_metric — #6
  - maximize_loess_metric — #7
  - minimize_loess_metric — #8
  - maximize_boot_metric — #9
  - minimize_boot_metric — #10
  - oc_youden_kernel — #11
  - oc_youden_normal — #12
  - oc_manual — #13
  - oc_cost_ratio — #14
  - oc_equal_sens_spec — #15
  - oc_closest_01 — #16
- [x] `metric` — all 16 metrics: #4, #5, #6
  - youden, sum_sens_spec, accuracy, sum_ppv_npv, prod_sens_spec, prod_ppv_npv, cohens_kappa, F1_score
  - abs_d_sens_spec, abs_d_ppv_npv, odds_ratio, risk_ratio, misclassification_cost, total_utility, roc01, p_chisquared
- [x] `specifyCutScore` — #13
- [x] `costratioFP` — #14

### Cutpoint Fine-Tuning
- [x] `tol_metric` — #17
- [x] `break_ties` (mean/median/c) — #18
- [x] `boot_runs` — #9, #10
- [x] `seed` — #9
- [x] `allObserved` — #19

### Prevalence & Cost
- [x] `usePriorPrev` — #33, #34
- [x] `priorPrev` — #33

### Output Tables
- [x] `sensSpecTable` (confusion matrix) — #2
- [x] `showThresholdTable`, `maxThresholds` — #2
- [x] `delongTest` — #21, #22

### ROC Visualization
- [x] `plotROC` — #23
- [x] `combinePlots` — #20, #26
- [x] `cleanPlot` — #25
- [x] `showOptimalPoint` — #23
- [x] `displaySE` — #24
- [x] `smoothing` — #24
- [x] `showConfidenceBands` — #23
- [x] `legendPosition` (none/right/bottom/topleft/topright) — #26
- [x] `directLabel` — #27
- [x] `interactiveROC` — #28

### Diagnostic Plots
- [x] `showCriterionPlot` — #29
- [x] `showPrevalencePlot` — #30
- [x] `showDotPlot` — #31
- [x] `precisionRecallCurve` — #32

### Advanced ROC
- [x] `partialAUC`, `partialAUCfrom`, `partialAUCto` — #38, #39
- [x] `rocSmoothingMethod` (none/binormal/density/fitdistr) — #40
- [x] `bootstrapCI`, `bootstrapReps` — #41
- [x] `quantileCIs`, `quantiles` — #42

### Fixed Sensitivity/Specificity
- [x] `fixedSensSpecAnalysis` — #43
- [x] `fixedAnalysisType` (sensitivity/specificity) — #43, #44
- [x] `fixedSensitivityValue` — #43
- [x] `fixedSpecificityValue` — #44
- [x] `fixedInterpolation` (linear/nearest/stepwise) — #45
- [x] `showFixedROC` — #46
- [x] `showFixedExplanation` — #46

### Model Comparison
- [x] `compareClassifiers` — #50
- [x] `calculateIDI` — #47
- [x] `calculateNRI` — #48, #49
- [x] `refVar` — #47, #48
- [x] `nriThresholds` — #48, #49
- [x] `idiNriBootRuns` — #47

### Effect Size & Power
- [x] `effectSizeAnalysis` — #51
- [x] `powerAnalysis` — #52
- [x] `powerAnalysisType` (post_hoc/prospective/sample_size) — #52, #53, #54
- [x] `expectedAUCDifference` — #53
- [x] `targetPower` — #52
- [x] `significanceLevel` — #52
- [x] `correlationROCs` — #54

### Bootstrap ROC with Prior Weighting
- [x] `bayesianAnalysis` — #55
- [x] `priorAUC` — #55, #56
- [x] `priorPrecision` — #55, #56

### Clinical Utility
- [x] `clinicalUtilityAnalysis` — #57
- [x] `treatmentThreshold` — #57
- [x] `harmBenefitRatio` — #57, #58
- [x] `interventionCost` — #58

### Meta-Analysis
- [x] `metaAnalysis` — #59
- [x] `metaAnalysisMethod` (fixed/random/both) — #59, #60
- [x] `heterogeneityTest` — #59
- [x] `forestPlot` — #61
- [x] `overrideMetaAnalysisWarning` — #62
