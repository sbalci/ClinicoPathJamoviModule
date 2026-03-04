# Testing Enhanced ROC Function

All data files are available in `data/` folder as `.rda`, `.csv`, `.xlsx`, and `.omv`. Open the `.omv` (or `.csv`) in jamovi.

---

## 1. SINGLE BIOMARKER (Default Analysis)

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 1 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | Default single ROC, `aucTable`, `optimalCutoffs`, `diagnosticMetrics`, `youdenOptimization`, direction: auto. Verify AUC ~0.80, instructions HTML, analysisSummary |
| 2 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `cutoffTable` (detailed cutoff analysis), `clinicalMetrics` (PPV/NPV/LR), `prevalence`: 0.30, `useObservedPrevalence`: on |
| 3 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `direction`: higher then lower (verify AUC changes or warning), `customCutoffs`: "5.0,7.5,10.0" |

**Options covered:** `outcome`, `positiveClass`, `predictors`, `analysisType` (single), `direction`, `youdenOptimization`, `customCutoffs`, `aucTable`, `optimalCutoffs`, `diagnosticMetrics`, `cutoffTable`, `clinicalMetrics`, `prevalence`, `useObservedPrevalence`

---

## 2. COMPARATIVE ROC (Multiple Predictors)

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 4 | `enhancedroc_comparative` | cancer_status / "Cancer" | established_marker, novel_marker1, novel_marker2, imaging_score, genetic_risk_score | `analysisType`: comparative, `pairwiseComparisons`: on, `comparisonMethod`: delong. Verify AUC comparison table, `rocCurve` (overlay plot) |
| 5 | `enhancedroc_comparative` | cancer_status / "Cancer" | established_marker, novel_marker1 | `comparisonMethod`: bootstrap then venkatraman, `showMetricsDiff`: on, `statisticalComparison`: on |
| 6 | `enhancedroc_comparative` | cancer_status / "Cancer" | established_marker, novel_marker1, novel_marker2, imaging_score, genetic_risk_score | `analysisType`: comprehensive, `comprehensive_output`: on, `clinical_interpretation`: on. Verify comprehensiveAnalysisSummary, clinicalReport, methodsExplanation |

**Options covered:** `analysisType` (comparative, comprehensive), `pairwiseComparisons`, `comparisonMethod` (delong/bootstrap/venkatraman), `showMetricsDiff`, `statisticalComparison`, `comprehensive_output`, `clinical_interpretation`

---

## 3. BOOTSTRAP & CONFIDENCE INTERVALS

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 7 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `useBootstrap`: on, `bootstrapSamples`: 500, `bootstrapMethod`: bca. Verify CI changes in aucSummary |
| 8 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `bootstrapMethod`: percentile then basic, `stratifiedBootstrap`: on |
| 9 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `confidenceLevel`: 90 (change from 95), `bootstrapCutoffCI`: on, `bootstrapPartialAUC`: on (with `partialAuc` on) |

**Options covered:** `confidenceLevel`, `useBootstrap`, `bootstrapSamples`, `bootstrapMethod` (bca/percentile/basic), `bootstrapCutoffCI`, `bootstrapPartialAUC`, `stratifiedBootstrap`

---

## 4. ROC CURVE PLOT OPTIONS

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 10 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `rocCurve`: on, `showCutoffPoints`: on, `showConfidenceBands`: on, `plotTheme`: clinical |
| 11 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `plotTheme`: classic then modern, `plotWidth`: 800, `plotHeight`: 800 |
| 12 | `enhancedroc_comparative` | cancer_status / "Cancer" | established_marker, novel_marker1 | Comparative ROC plot: verify overlay curves with different colors |

**Options covered:** `rocCurve`, `showCutoffPoints`, `showConfidenceBands`, `plotTheme` (classic/modern/clinical), `plotWidth`, `plotHeight`

---

## 5. YOUDEN INDEX & CUTOFF ANALYSIS PLOTS

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 13 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `youdenOptimization`: on — verify youdenIndexPlot appears |
| 14 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `cutoffTable`: on — verify cutoffAnalysisPlot appears |
| 15 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `clinicalMetrics`: on — verify clinicalDecisionPlot appears |

**Options covered:** Youden Index plot, Cutoff Analysis plot, Clinical Decision plot (all triggered by corresponding table options)

---

## 6. ADVANCED ROC METHODS

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 16 | `enhancedroc_validation` | outcome / "Positive" | biomarker | `smoothMethod`: binormal then kernel. Verify smoothed ROC curve |
| 17 | `enhancedroc_validation` | outcome / "Positive" | biomarker | `partialAuc`: on, `partialAucType`: specificity, `partialRange`: "0.8,1.0". Verify partialAucAnalysis table |
| 18 | `enhancedroc_validation` | outcome / "Positive" | biomarker | `partialAucType`: sensitivity, `partialRange`: "0.9,1.0" |
| 19 | `enhancedroc_validation` | outcome / "Positive" | biomarker | `crocAnalysis`: on, `crocAlpha`: 7.0. Verify crocAnalysisTable and crocCurvePlot |
| 20 | `enhancedroc_validation` | outcome / "Positive" | biomarker | `convexHull`: on. Verify convexHullTable and convexHullPlot |
| 21 | `enhancedroc_tiedscores` | disease / "Disease" | ordinal_score, rounded_lab, composite_score | `tiedScoreHandling`: average then upper then lower |

**Options covered:** `smoothMethod` (none/binormal/kernel), `partialAuc`, `partialAucType` (specificity/sensitivity), `partialRange`, `crocAnalysis`, `crocAlpha`, `convexHull`, `tiedScoreHandling` (average/upper/lower)

---

## 7. SENSITIVITY & SPECIFICITY THRESHOLDS

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 22 | `enhancedroc_screening` | screening_indication / "Screen Positive" | sensitive_marker | `sensitivityThreshold`: 0.95 (high sensitivity for screening), `specificityThreshold`: 0.5 |
| 23 | `enhancedroc_confirmatory` | confirmed_diagnosis / "Confirmed" | specific_marker | `sensitivityThreshold`: 0.5, `specificityThreshold`: 0.95 (high specificity for confirmation) |

**Options covered:** `sensitivityThreshold`, `specificityThreshold`

---

## 8. CLASS IMBALANCE DETECTION

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 24 | `enhancedroc_imbalanced` | rare_disease / "Positive" | screening_marker, confirmatory_marker | `detectImbalance`: on, `imbalanceThreshold`: 3.0, `showImbalanceWarning`: on, `recommendPRC`: on. Verify imbalanceMetrics table, precisionRecallTable, prcPlot |
| 25 | `enhancedroc_imbalanced` | rare_disease / "Positive" | screening_marker | `imbalanceThreshold`: 1.5 (more sensitive threshold) — verify wider flagging |

**Options covered:** `detectImbalance`, `imbalanceThreshold`, `showImbalanceWarning`, `recommendPRC`

---

## 9. CLINICAL CONTEXT & PRESETS

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 26 | `enhancedroc_screening` | screening_indication / "Screen Positive" | sensitive_marker | `clinicalContext`: screening, `clinicalPresets`: biomarker_screening. Verify auto-configured high sensitivity thresholds |
| 27 | `enhancedroc_confirmatory` | confirmed_diagnosis / "Confirmed" | specific_marker | `clinicalContext`: diagnosis, `clinicalPresets`: confirmatory_testing. Verify auto-configured high specificity thresholds |
| 28 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `clinicalContext`: prognosis, `clinicalPresets`: diagnostic_validation. Verify balanced thresholds |
| 29 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1, biomarker2, clinical_risk_score | `clinicalPresets`: research_comprehensive. Verify comprehensive output, calibration, validation all enabled |
| 30 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `clinicalContext`: monitoring, `clinicalPresets`: custom |

**Options covered:** `clinicalContext` (screening/diagnosis/prognosis/monitoring/general), `clinicalPresets` (custom/biomarker_screening/diagnostic_validation/confirmatory_testing/research_comprehensive)

---

## 10. CALIBRATION ANALYSIS

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 31 | `enhancedroc_calibration` | outcome / "Event" | predicted_prob, risk_score | `calibrationAnalysis`: on, `calibrationPlot`: on, `brierScore`: on, `calibrationMetrics`: on. Verify calibrationSummary table and calibrationPlotImage |
| 32 | `enhancedroc_calibration` | outcome / "Event" | predicted_prob | `hosmerLemeshow`: on, `hlGroups`: 10 then 5. Verify hosmerLemeshowTable |

**Options covered:** `calibrationAnalysis`, `calibrationPlot`, `hosmerLemeshow`, `hlGroups`, `brierScore`, `calibrationMetrics`

---

## 11. MULTI-CLASS ROC

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 33 | `enhancedroc_multiclass` | disease_severity / (any level) | biomarker_A, biomarker_B, imaging_severity_score | `multiClassROC`: on, `multiClassStrategy`: ovr, `multiClassAveraging`: macro. Verify multiClassAUC, multiClassAverage tables, multiClassROCPlot |
| 34 | `enhancedroc_multiclass` | disease_severity / (any level) | biomarker_A | `multiClassStrategy`: ovo, `multiClassAveraging`: weighted |

**Options covered:** `multiClassROC`, `multiClassStrategy` (ovr/ovo), `multiClassAveraging` (macro/weighted)

---

## 12. CLINICAL IMPACT & UTILITY

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 35 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `clinicalImpact`: on, `nntCalculation`: on, `decisionImpactTable`: on. Verify clinicalImpactTable |
| 36 | `enhancedroc_biomarker` | disease_status / "Disease" | biomarker1 | `clinicalUtilityCurve`: on. Verify clinicalUtilityPlot |

**Options covered:** `clinicalImpact`, `nntCalculation`, `clinicalUtilityCurve`, `decisionImpactTable`

---

## 13. EDGE CASES

| # | File | Outcome / Positive Class | Predictors | Options to Test |
|---|------|--------------------------|------------|-----------------|
| 37 | `enhancedroc_small` | disease / "Positive" | marker | Small sample (n=60). Verify sample size warnings. `useBootstrap`: on, `bootstrapSamples`: 200 |
| 38 | `enhancedroc_screening` | screening_indication / "Screen Positive" | sensitive_marker | Low prevalence (~8%). Verify prevalence warnings for PPV interpretation |
| 39 | `enhancedroc_tiedscores` | disease / "Disease" | ordinal_score | Many tied values. Verify tied score handling and notices |

**Options covered:** small sample handling, low prevalence warnings, tied score edge cases

---

## 14. NOT YET IMPLEMENTED (Planned Features)

These options exist in YAML but are **not yet implemented** in the backend. Verify they show appropriate "not implemented" notices or are silently skipped:

| # | Options to Verify | Expected Behavior |
|---|-------------------|-------------------|
| 40 | `splineCalibration`, `splineKnots` | Should be no-op or show "planned" notice |
| 41 | `eoRatio`, `namDagostino`, `greenwoodNam` | Should be no-op or show "planned" notice |
| 42 | `calibrationBelt`, `calibrationDensity` | Should be no-op or show "planned" notice |
| 43 | `harrellCIndex`, `unoCStatistic` | Should be no-op or show "planned" notice |
| 44 | `incidentDynamic`, `cumulativeDynamic`, `competingRisksConcordance` | Should be no-op |
| 45 | `internalValidation`, `validationMethod`, `optimismCorrection` | Should be no-op |
| 46 | `externalValidation`, `decisionImpactCurves`, `netBenefitRegression` | Should be no-op |
| 47 | `modelUpdating`, `transportability` | Should be no-op |

---

## TEST DATA FILES

| File | N | Outcome Column | Outcome Levels | Prevalence | Key Predictors | Primary Use |
|------|---|----------------|----------------|------------|----------------|-------------|
| `enhancedroc_biomarker` | 300 | disease_status | Healthy/Disease | 30% | biomarker1, biomarker2, biomarker3, clinical_risk_score | Single biomarker validation |
| `enhancedroc_comparative` | 400 | cancer_status | No Cancer/Cancer | 25% | established_marker, novel_marker1, novel_marker2, imaging_score, genetic_risk_score | Compare diagnostic tests |
| `enhancedroc_imbalanced` | 500 | rare_disease | Negative/Positive | 5% | screening_marker, confirmatory_marker, combined_risk | Imbalanced data handling |
| `enhancedroc_multiclass` | 350 | disease_severity | Normal/Mild/Moderate/Severe | Mixed | biomarker_A, biomarker_B, imaging_severity_score | Multi-class severity grading |
| `enhancedroc_calibration` | 300 | outcome | No Event/Event | 35% | predictor1, predictor2, predicted_prob, risk_score | Calibration assessment |
| `enhancedroc_screening` | 600 | screening_indication | Screen Negative/Screen Positive | 8% | sensitive_marker, imaging_marker, panel_score | Screening context |
| `enhancedroc_confirmatory` | 250 | confirmed_diagnosis | Not Confirmed/Confirmed | 45% | specific_marker, pathology_score, molecular_signature | Confirmatory testing |
| `enhancedroc_small` | 60 | disease | Negative/Positive | 30% | marker | Small sample / unit testing |
| `enhancedroc_validation` | 350 | outcome | Negative/Positive | 30% | model_prob, miscalibrated_prob, biomarker, risk_score | CROC / convex hull / validation |
| `enhancedroc_tiedscores` | 200 | disease | No Disease/Disease | 35% | ordinal_score, rounded_lab, composite_score | Tied score handling |

---

## COMPLETE OPTION COVERAGE CHECKLIST

### Core Input
- [x] `outcome` — all tests
- [x] `positiveClass` — all tests
- [x] `predictors` — all tests

### Analysis Type & Direction
- [x] `analysisType` (single/comparative/comprehensive) — #1, #4, #6
- [x] `direction` (auto/higher/lower) — #3

### Cutoff Options
- [x] `youdenOptimization` — #1, #13
- [x] `customCutoffs` — #3
- [x] `sensitivityThreshold` — #22, #23
- [x] `specificityThreshold` — #22, #23

### Bootstrap & CI
- [x] `confidenceLevel` — #9
- [x] `useBootstrap` — #7, #8, #37
- [x] `bootstrapSamples` — #7, #37
- [x] `bootstrapMethod` (bca/percentile/basic) — #7, #8
- [x] `bootstrapCutoffCI` — #9
- [x] `bootstrapPartialAUC` — #9
- [x] `stratifiedBootstrap` — #8

### Comparison
- [x] `pairwiseComparisons` — #4
- [x] `comparisonMethod` (delong/bootstrap/venkatraman) — #4, #5

### Output Display
- [x] `rocCurve` — #1, #10
- [x] `aucTable` — #1
- [x] `cutoffTable` — #2, #14
- [x] `optimalCutoffs` — #1
- [x] `diagnosticMetrics` — #1
- [x] `clinicalMetrics` — #2, #15

### Advanced ROC
- [x] `smoothMethod` (none/binormal/kernel) — #16
- [x] `partialAuc`, `partialAucType`, `partialRange` — #17, #18
- [x] `crocAnalysis`, `crocAlpha` — #19
- [x] `convexHull` — #20
- [x] `tiedScoreHandling` (average/upper/lower) — #21

### Class Imbalance
- [x] `detectImbalance` — #24
- [x] `imbalanceThreshold` — #24, #25
- [x] `showImbalanceWarning` — #24
- [x] `recommendPRC` — #24

### Clinical Context
- [x] `prevalence`, `useObservedPrevalence` — #2
- [x] `clinicalContext` (screening/diagnosis/prognosis/monitoring/general) — #26-#30
- [x] `clinicalPresets` (custom/biomarker_screening/diagnostic_validation/confirmatory_testing/research_comprehensive) — #26-#29

### Output Format
- [x] `comprehensive_output` — #6
- [x] `clinical_interpretation` — #6

### Calibration
- [x] `calibrationAnalysis` — #31
- [x] `calibrationPlot` — #31
- [x] `hosmerLemeshow`, `hlGroups` — #32
- [x] `brierScore` — #31
- [x] `calibrationMetrics` — #31

### Multi-Class
- [x] `multiClassROC` — #33
- [x] `multiClassStrategy` (ovr/ovo) — #33, #34
- [x] `multiClassAveraging` (macro/weighted) — #33, #34

### Clinical Impact
- [x] `clinicalImpact` — #35
- [x] `nntCalculation` — #35
- [x] `clinicalUtilityCurve` — #36
- [x] `decisionImpactTable` — #35

### Plot Settings
- [x] `plotTheme` (classic/modern/clinical) — #10, #11
- [x] `plotWidth`, `plotHeight` — #11
- [x] `showCutoffPoints` — #10
- [x] `showConfidenceBands` — #10

### Comparison Display
- [x] `showMetricsDiff` — #5
- [x] `statisticalComparison` — #5

### Not Yet Implemented (Verify No Errors)
- [x] `splineCalibration`, `splineKnots` — #40
- [x] `eoRatio`, `namDagostino`, `greenwoodNam` — #41
- [x] `calibrationBelt`, `calibrationDensity` — #42
- [x] `harrellCIndex`, `unoCStatistic` — #43
- [x] `incidentDynamic`, `cumulativeDynamic`, `competingRisksConcordance` — #44
- [x] `internalValidation`, `validationMethod`, `optimismCorrection` — #45
- [x] `externalValidation`, `decisionImpactCurves`, `netBenefitRegression` — #46
- [x] `modelUpdating`, `transportability` — #47
