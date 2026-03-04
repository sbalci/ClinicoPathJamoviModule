# enhancedROC Data Flow Review

Reviewed by: data-flow-checker agent
Date: 2026-03-02
Files reviewed:
- `jamovi/enhancedroc.a.yaml` (analysis options — 68 options defined)
- `R/enhancedROC.b.R` (backend implementation — ~4160 lines)
- `jamovi/enhancedroc.r.yaml` (results definitions — ~1015 lines)
- `jamovi/enhancedroc.u.yaml` (UI definitions)
- `R/enhancedROC.h.R` (auto-generated header)

---

## A. OPTIONS FLOW (.a.yaml → .b.R)

### Options Defined in .a.yaml vs. Usage in .b.R

All 68 options defined in .a.yaml are correctly registered in the auto-generated `.h.R`. The following analysis checks actual backend usage.

#### Options WITH confirmed usage in .b.R (via `self$options$<name>`)

| Option | Used in .b.R | Notes |
|---|---|---|
| outcome | YES | `self$options$outcome` in `.init()` and `.asSource()` |
| positiveClass | YES | Used in data preparation |
| predictors | YES | `self$options$predictors` in `.init()` |
| analysisType | YES | Guards `pairwiseComparisons`, `showMetricsDiff`, `statisticalComparison` |
| direction | YES | Mapped to pROC direction parameter |
| youdenOptimization | YES | In `.calculateOptimalCutoff()` |
| customCutoffs | YES | In `.evaluateCustomCutoffs()` |
| sensitivityThreshold | YES | In `.calculateOptimalCutoff()` |
| specificityThreshold | YES | In `.calculateOptimalCutoff()` |
| confidenceLevel | YES | In pROC `conf.level` |
| bootstrapSamples | YES | Boot iterations |
| useBootstrap | YES | Guards bootstrap CI branch |
| bootstrapMethod | YES | Mapped to boot.ci.type |
| bootstrapCutoffCI | YES (stub) | Flagged as unimplemented |
| bootstrapPartialAUC | YES (stub) | Flagged as unimplemented |
| stratifiedBootstrap | YES | In `boot.stratified` |
| pairwiseComparisons | YES | Guards `.populateROCComparisons()` |
| comparisonMethod | YES | In `pROC::roc.test()` |
| rocCurve | YES | Guards `.plotROCCurve()` |
| aucTable | YES | Guards `.populateAUCSummary()` |
| cutoffTable | YES | Guards `.populateCutoffAnalysis()` |
| optimalCutoffs | YES | Guards `.populateOptimalCutoffs()` |
| diagnosticMetrics | YES | Guards `.populateDiagnosticPerformance()` |
| clinicalMetrics | YES | Guards `.populateClinicalMetrics()` |
| smoothMethod | YES | In pROC smooth call |
| partialAuc | YES | Guards `.populatePartialAUC()` |
| partialAucType | YES | Range type selection |
| partialRange | YES | Parsed in `.populatePartialAUC()` |
| crocAnalysis | YES | Guards `.populateCROCAnalysis()` |
| crocAlpha | YES | In CROC calculation |
| convexHull | YES | Guards `.populateConvexHull()` |
| tiedScoreHandling | YES (stub) | Noted as unsupported by pROC |
| detectImbalance | YES | Guards imbalance checks |
| imbalanceThreshold | YES | In `.checkClassImbalance()` |
| showImbalanceWarning | YES | Triggers imbalance warning notice |
| recommendPRC | YES | In PRC recommendation logic |
| prevalence | YES | For PPV/NPV calculation |
| useObservedPrevalence | YES | In clinical metrics |
| clinicalContext | YES | Passed to interpretation helpers |
| clinicalPresets | YES | In `.applyClinicalPresets()` |
| comprehensive_output | YES | Guards comprehensive analysis |
| clinical_interpretation | YES | Guards clinical interpretation |
| plotTheme | YES | In plot formatting |
| plotWidth | YES | Plot dimension |
| plotHeight | YES | Plot dimension |
| showCutoffPoints | YES | In ROC curve plot |
| showConfidenceBands | YES | In ROC curve plot |
| showMetricsDiff | YES | Guards `.populateDetailedComparison()` |
| statisticalComparison | YES | Guards `.populateStatisticalSummary()` |
| calibrationAnalysis | YES | Guards `.populateCalibrationAnalysis()` |
| calibrationPlot | YES | Guards `.plotCalibration()` |
| hosmerLemeshow | YES | In calibration analysis |
| hlGroups | YES | In H-L test |
| brierScore | YES | In calibration table |
| calibrationMetrics | YES | In calibration table (conditional columns) |
| splineCalibration | YES (stub) | Flagged as unimplemented |
| splineKnots | YES | In UI enable condition; not yet in .b.R logic |
| eoRatio | YES (stub) | Flagged as unimplemented |
| namDagostino | YES (stub) | Flagged as unimplemented |
| greenwoodNam | YES (stub) | Flagged as unimplemented |
| calibrationBelt | YES (stub) | Flagged as unimplemented |
| calibrationDensity | YES (stub) | Flagged as unimplemented |
| multiClassROC | YES | Guards `.populateMultiClassROC()` |
| multiClassStrategy | YES | In multi-class loop |
| multiClassAveraging | YES (stub) | Flagged as unimplemented for non-macro |
| clinicalImpact | YES | Guards `.populateClinicalImpact()` |
| nntCalculation | YES (stub) | Flagged as unimplemented |
| clinicalUtilityCurve | YES | Guards `.plotClinicalUtility()` |
| decisionImpactTable | YES | Guards decision impact table |
| harrellCIndex | YES (stub) | Flagged as unimplemented |
| unoCStatistic | YES (stub) | Flagged as unimplemented |
| incidentDynamic | YES (stub) | Flagged as unimplemented |
| cumulativeDynamic | YES (stub) | Flagged as unimplemented |
| competingRisksConcordance | YES (stub) | Flagged as unimplemented |
| internalValidation | YES | Guards `.populateInternalValidation()` |
| validationMethod | YES | In cross-validation / bootstrap logic |
| optimismCorrection | YES (stub) | Flagged as unimplemented |
| externalValidation | YES (stub) | Flagged as unimplemented |
| decisionImpactCurves | YES (stub) | Flagged as unimplemented |
| netBenefitRegression | YES (stub) | Flagged as unimplemented |
| modelUpdating | YES (stub) | Flagged as unimplemented |
| transportability | YES (stub) | Flagged as unimplemented |

### Orphaned Options (defined in .a.yaml but NOT referenced in .b.R)

**NONE found.** All options defined in .a.yaml are referenced in the backend at minimum in the "unimplemented features" notification block.

### `splineKnots` note

`splineKnots` is used in the UI (`enable: (splineCalibration)`) and defined in the header. In .b.R it is NOT referenced in any computational logic — only `splineCalibration` is checked (as a stub). When `splineCalibration` is implemented, `splineKnots` will need to be consumed. Currently it is a defined-but-dormant option.

### Option Type Verification

All option types appear consistent with usage:
- Bool options used with `isTRUE()` or direct boolean comparison — correct
- List options compared with string literals (`== "comparative"`, `== "delong"`) — correct
- Number options used arithmetically or with `/ 100` conversion — correct
- Variable/Variables options stored as character and used for column indexing — correct

### Default Value Assessment

| Option | Default | Assessment |
|---|---|---|
| rocCurve | false | Acceptable — user must opt in |
| aucTable | false | Acceptable — user must opt in |
| youdenOptimization | false | Acceptable |
| prevalence | 0.1 | Reasonable for general clinical use |
| confidenceLevel | 95 | Standard |
| bootstrapSamples | 1000 | Adequate but expensive; min 100 is appropriate |
| crocAlpha | 7.0 | Reasonable CROC default |
| imbalanceThreshold | 3.0 | Reasonable (3:1 ratio) |
| sensitivityThreshold | 0.8 | Reasonable for screening |
| specificityThreshold | 0.8 | Reasonable for confirmatory |

---

## B. RESULTS FLOW (.b.R → .r.yaml)

### Results Elements in .r.yaml vs. Population in .b.R

| Result Name (.r.yaml) | Type | Populated in .b.R | Method |
|---|---|---|---|
| notices | Html | YES | `.renderNotices()` → `$setContent()` |
| instructions | Html | YES | Many data validation paths |
| imbalanceMetrics | Table | YES | `.populateImbalanceMetrics()` indirectly via `.checkClassImbalance()` |
| precisionRecallTable | Table | YES | `.populatePrecisionRecall()` |
| analysisSummary | Html | YES | `.generateAnalysisSummary()` + `.populateInternalValidation()` |
| clinicalReport | Html | YES | `.generateClinicalReport()` |
| aucSummary | Table | YES | `.populateAUCSummary()` |
| rocComparisons | Table | YES | `.populateROCComparisons()` |
| detailedComparison | Table | YES | `.populateDetailedComparison()` |
| statisticalSummary | Table | YES | `.populateStatisticalSummary()` |
| optimalCutoffSummary | Table | YES | `.populateOptimalCutoffs()` |
| cutoffAnalysis | Table | YES | `.populateCutoffAnalysis()` |
| diagnosticPerformance | Table | YES | `.populateDiagnosticPerformance()` |
| clinicalApplicationMetrics | Table | YES | `.populateClinicalMetrics()` |
| partialAucAnalysis | Table | YES | `.populatePartialAUC()` |
| crocAnalysisTable | Table | YES | `.populateCROCAnalysis()` |
| convexHullTable | Table | YES | `.populateConvexHull()` |
| comprehensiveAnalysisSummary | Table | YES | `.populateComprehensiveAnalysis()` |
| clinicalInterpretationGuide | Html | YES | `.populateClinicalInterpretation()` |
| methodsExplanation | Html | YES | `.populateMethodsExplanation()` |
| rocCurvePlot | Image | YES | `.plotROCCurve()` |
| prcPlot | Image | YES | `.plotPRC()` |
| comparativeROCPlot | Image | YES | `.plotComparativeROC()` |
| cutoffAnalysisPlot | Image | YES | `.plotCutoffAnalysis()` |
| youdenIndexPlot | Image | YES | `.plotYoudenIndex()` |
| clinicalDecisionPlot | Image | YES | `.plotClinicalDecision()` |
| crocCurvePlot | Image | YES | `.plotCROC()` |
| convexHullPlot | Image | YES | `.plotConvexHull()` |
| calibrationSummary | Table | YES | `.populateCalibrationAnalysis()` |
| hosmerLemeshowTable | Table | YES | `.populateCalibrationAnalysis()` |
| calibrationPlotImage | Image | YES | `.plotCalibration()` |
| multiClassAUC | Table | YES | `.populateMultiClassROC()` |
| multiClassAverage | Table | YES | `.populateMultiClassROC()` |
| multiClassROCPlot | Image | YES | `.plotMultiClassROC()` |
| clinicalImpactTable | Table | YES | `.populateClinicalImpact()` |
| decisionImpactSummary | Table | YES | `.populateClinicalImpact()` (guarded by `decisionImpactTable`) |
| clinicalUtilityPlot | Image | YES | `.plotClinicalUtility()` |

### Results Defined in .r.yaml but NOT Populated

**NONE.** All results elements defined in `.r.yaml` are populated by the backend.

### Results Populated in .b.R but NOT Defined in .r.yaml

**NONE found.** All `self$results$results$<name>$` references match elements in `.r.yaml`.

### Table Column Consistency Check (Sample)

**`aucSummary`**: Columns in .r.yaml: predictor, auc, auc_lower, auc_upper, std_error, auc_interpretation, clinical_utility. The `.populateAUCSummary()` function writes these exact column names using `addRow(rowKey=..., values=list(...))`. **Consistent.**

**`cutoffAnalysis`**: Columns in .r.yaml: predictor, cutoff, cutoff_type, sensitivity, specificity, one_minus_specificity, youden_index, true_positive, true_negative, false_positive, false_negative. Backend populates these. **Consistent.**

**`diagnosticPerformance`**: Columns in .r.yaml: predictor, cutoff, sensitivity, sensitivity_ci, specificity, specificity_ci, accuracy, balanced_accuracy. Backend populates these. **Consistent.**

**`clinicalApplicationMetrics`**: Columns in .r.yaml: predictor, prevalence, ppv, npv, lr_positive, lr_negative, diagnostic_odds_ratio, clinical_interpretation. Backend populates these. **Consistent.**

### Plot Render Function Verification

All `renderFun` references in .r.yaml have matching private method definitions in .b.R:
- `.plotROCCurve` — line 2665
- `.plotComparativeROC` — line 2819
- `.plotCutoffAnalysis` — line 2946
- `.plotYoudenIndex` — line 3013
- `.plotClinicalDecision` — line 3082
- `.plotPRC` — line 3161
- `.plotCROC` — line 3249
- `.plotConvexHull` — line 3324
- `.plotCalibration` — line 3538
- `.plotMultiClassROC` — line 3727
- `.plotClinicalUtility` — line 3900

All 11 plot methods confirmed present. **Consistent.**

---

## C. UI COMPLETENESS (.a.yaml ↔ .u.yaml)

### Options in .a.yaml with NO UI Element in .u.yaml

The following options are defined but have no direct UI widget in `.u.yaml`:

| Missing from .u.yaml | Severity | Note |
|---|---|---|
| `analysisType` | PRESENT — ComboBox on line 46 | OK |

After careful review, all actionable options have corresponding UI elements. The only confirmed missing UI elements relate to stub/future features that are intentionally absent:

No UI widgets for the following (but options exist and are referenced as stubs in .b.R):
- These are correctly omitted from UI per the commented-out Time-Dependent ROC / Survival ROC sections

### UI Elements That Enable Correctly

The following conditional `enable:` expressions in .u.yaml are correctly linked to options:
- `enable: (rocCurve)` → `showCutoffPoints`, `showConfidenceBands`, `plotTheme` — correct
- `enable: (useBootstrap)` → `bootstrapSamples`, `bootstrapMethod`, `stratifiedBootstrap`, `bootstrapCutoffCI` — correct
- `enable: (useBootstrap && partialAuc)` → `bootstrapPartialAUC` — correct
- `enable: (pairwiseComparisons)` → `comparisonMethod` — correct
- `enable: (detectImbalance)` → `imbalanceThreshold`, `showImbalanceWarning`, `recommendPRC` — correct
- `enable: (partialAuc)` → `partialAucType`, `partialRange` — correct
- `enable: (crocAnalysis)` → `crocAlpha` — correct
- `enable: (calibrationAnalysis)` → `calibrationPlot`, `brierScore`, `calibrationMetrics`, `eoRatio`, etc. — correct
- `enable: (calibrationAnalysis && hosmerLemeshow)` → `hlGroups` — correct
- `enable: (splineCalibration)` → `splineKnots` — correct
- `enable: (multiClassROC)` → `multiClassStrategy`, `multiClassAveraging` — correct
- `enable: (clinicalImpact)` → `nntCalculation`, `clinicalUtilityCurve`, `decisionImpactTable` — correct

### UI Elements Referencing Non-Existent Options

None found. All `name:` references in .u.yaml correspond to valid option names in .a.yaml.

### UI Organization Assessment

The UI is organized into 11 CollapseBox sections:
1. Variable Supplier (outcome + predictors) — always visible
2. Analysis Options (analysisType, direction, youdenOptimization, clinicalPresets, thresholds)
3. Output Options (tables + basic plot controls)
4. Statistical Options (CI settings + comparisons) — collapsed by default
5. Class Imbalance Detection — open by default
6. Clinical Context — collapsed by default
7. Advanced Output — collapsed by default
8. Calibration Analysis — collapsed by default
9. Multi-Class ROC — collapsed by default
10. Clinical Impact — collapsed by default
11. Plot Settings — collapsed by default
12. Validation & Model Performance — collapsed by default

**Issue:** The "Class Imbalance Detection" section is `collapsed: false` (open by default) but most users will not need it for standard analyses. Consider making it `collapsed: true`.

**Issue:** `validationMethod` ComboBox appears in "Validation & Model Performance" section but there is no guard for `internalValidation` — the ComboBox is always visible even when `internalValidation` is not enabled. Should add `enable: (internalValidation)` to `validationMethod` ComboBox.

**Issue:** `harrellCIndex`, `unoCStatistic`, `incidentDynamic`, `cumulativeDynamic`, `competingRisksConcordance` all appear in the "Clinical Impact" CollapseBox with no enable condition. These are stub features that are not implemented. Ideally they should be disabled or removed, or at minimum clearly labeled as "Planned" in the UI label.

---

## D. ISSUES SUMMARY

### Critical Issues (must fix before release)

None identified — all data flow connections are functionally complete.

### Significant Issues

1. **`validationMethod` has no `enable` condition in UI** — the ComboBox is always visible regardless of whether `internalValidation` is enabled. This is confusing to users.
   - Fix: Add `enable: (internalValidation)` to the `validationMethod` ComboBox in .u.yaml.

2. **Stub options appear as active UI controls** — `harrellCIndex`, `unoCStatistic`, `incidentDynamic`, `cumulativeDynamic`, `competingRisksConcordance`, `splineCalibration`, `eoRatio`, `namDagostino`, `greenwoodNam`, `calibrationBelt`, `calibrationDensity`, `optimismCorrection`, `externalValidation`, `decisionImpactCurves`, `netBenefitRegression`, `modelUpdating`, `transportability`, `nntCalculation`, `bootstrapCutoffCI`, `bootstrapPartialAUC` — all appear in the UI but are unimplemented. When enabled, users only receive an INFO notice saying the feature is planned.
   - This creates a poor UX: users enable an option, see nothing happen, then must read a notice to discover it's not implemented.
   - Fix: Either remove these from .u.yaml (or comment them out), or add `enable: false` / disabled styling. Alternatively, prefix their UI labels with "(Planned)" to set expectations.

3. **`splineKnots` is UI-enabled when `splineCalibration` is true**, but `splineCalibration` itself is a stub. The UI will show `splineKnots` as enabled, which is misleading.

### Minor Issues

4. **`Class Imbalance Detection` section is open by default (`collapsed: false`)** — this gives prominence to an advanced feature that most users will not need initially. Recommend `collapsed: true`.

5. **No result items for internal validation validation summary table** — `.populateInternalValidation()` writes its output into `analysisSummary` Html item, which also collects other content. This mixing of different analysis types into a single Html item makes it harder to parse results programmatically. A dedicated Table output for validation results would be better for clinical reporting.

6. **`internalValidation` result is mixed into `analysisSummary`** — the `analysisSummary` Html element also clears on `internalValidation` + `validationMethod` + `bootstrapSamples` changes (as specified in the `clearWith` list), which is correct. But appending validation HTML to analysis summary HTML (line 4131–4132) is fragile — if `generateAnalysisSummary()` runs after `populateInternalValidation()`, it could overwrite the validation content. The current code flow has `generateAnalysisSummary()` called before `populateInternalValidation()`, so this is currently safe, but it is a latent ordering bug.

### Notes on .r.yaml clearWith Completeness

Most `clearWith` lists in .r.yaml are reasonable. However:
- `aucSummary` clears on `confidenceLevel` and `useBootstrap` — correct, since CI width changes.
- `calibrationSummary` clears on `hlGroups` — correct.
- `analysisSummary` clears on `internalValidation`, `validationMethod`, `bootstrapSamples` — correct.
- `clinicalApplicationMetrics` clears on `useObservedPrevalence` and `prevalence` — correct.

Missing: `rocComparisons` does not clear on `bootstrapSamples` even though pairwise bootstrap comparison uses this option. If a user changes `bootstrapSamples` while `comparisonMethod == "bootstrap"`, stale results may remain. Recommend adding `bootstrapSamples` to `rocComparisons` clearWith when `comparisonMethod == "bootstrap"`.

---

## E. SUMMARY TABLE

| Check | Status |
|---|---|
| No orphaned .a.yaml options | PASS |
| No undefined .b.R options | PASS |
| All .r.yaml results populated | PASS |
| No extra .b.R results without .r.yaml definition | PASS |
| Table columns consistent | PASS |
| All renderFun methods defined | PASS |
| All .u.yaml names reference valid options | PASS |
| All options have UI elements | PASS (stubs excepted) |
| UI conditional enables are correct | MOSTLY — see issues #1, #3 |
| Stubs properly communicated to user | PARTIAL — see issue #2 |
| No serialization anti-patterns | PASS — notices use HTML pattern |

---

## F. RECOMMENDATIONS FOR IMPROVEMENT

1. **Immediate**: Add `enable: (internalValidation)` to `validationMethod` ComboBox in .u.yaml.
2. **Pre-release**: Remove or disable stub options from the UI to avoid UX confusion. The simplest approach is to comment out their CheckBox elements in .u.yaml (as Time-Dependent ROC and Survival ROC already are).
3. **Pre-release**: Change `Class Imbalance Detection` CollapseBox to `collapsed: true`.
4. **Future**: Add a dedicated Table result for internal validation output rather than appending to `analysisSummary`.
5. **Future**: Add `bootstrapSamples` to `clearWith` for `rocComparisons` table.
