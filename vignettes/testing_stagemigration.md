# Advanced TNM Stage Migration Analysis - Testing Checklist

## Test Datasets

| Dataset | File | Observations | Variables | Description |
|---------|------|-------------|-----------|-------------|
| Combined | `data/stagemigration_combined.rda` (`combined_data`) | 2100 | 8: patient_id, age, sex, old_stage, new_stage, survival_time, event, cancer_type | Three cancer types pooled |
| Breast cancer | `data/stagemigration_breast_cancer.rda` | ~700 | 8 | Breast cancer cohort |
| Lung cancer | `data/stagemigration_lung_cancer.rda` | ~700 | 8 | Lung cancer cohort |
| Colorectal cancer | `data/stagemigration_colorectal_cancer.rda` | ~700 | 8 | Colorectal cancer cohort |
| Small sample | `data/stagemigration_small_sample.rda` | ~30-50 | 8 | Edge case: minimal sample |
| Problematic data | `data/stagemigration_problematic.rda` | varies | 8 | Edge cases: missing, extreme values |
| Large performance | `data/stagemigration_large_performance.rda` | 10000+ | 8 | Performance benchmarking |

---

## 1. Basic Functionality (4 tests)

### Test 1.1: Minimal Variable Assignment
- **Data:** `stagemigration_combined.rda`
- **Options:** Assign `old_stage` -> oldStage, `new_stage` -> newStage, `survival_time` -> survivalTime, `event` -> event, select event level
- **Expected:** Welcome message disappears. `migrationOverview` and `migrationMatrix` tables populate. No errors.
- **Pass:** [ ]

### Test 1.2: Migration Overview Accuracy
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `showMigrationOverview = TRUE`
- **Expected:** Overview table shows total patients, migration count, migration percentage, upstaged count, downstaged count, net migration. Values are internally consistent (upstaged + downstaged + unchanged = total).
- **Pass:** [ ]

### Test 1.3: Migration Matrix Completeness
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `showMigrationMatrix = TRUE`
- **Expected:** Cross-tabulation matrix where row sums match old_stage distribution and column sums match new_stage distribution. Diagonal shows unchanged patients.
- **Pass:** [ ]

### Test 1.4: Stage Distribution Comparison
- **Data:** `stagemigration_colorectal_cancer.rda`
- **Options:** `showStageDistribution = TRUE`
- **Expected:** Table shows count and percentage for both staging systems. Change column shows net gain/loss per stage. Percentages sum to 100% for each system.
- **Pass:** [ ]

---

## 2. Analysis Types (3 tests)

### Test 2.1: Basic Analysis Mode
- **Data:** `stagemigration_combined.rda`
- **Options:** `analysisType = "basic"`, default options
- **Expected:** Only migration overview and migration matrix tables visible. No advanced statistical tables.
- **Pass:** [ ]

### Test 2.2: Comprehensive Analysis Mode
- **Data:** `stagemigration_combined.rda`
- **Options:** `analysisType = "comprehensive"`, enable `calculateNRI`, `calculateIDI`, `performBootstrap`
- **Expected:** All enabled tables populate. NRI table shows time-point-specific values. IDI table shows improvement metric. Bootstrap table shows apparent, bootstrap mean, optimism, and corrected values.
- **Pass:** [ ]

### Test 2.3: Publication Ready Mode
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `analysisType = "publication"`, `clinicalPreset = "publication_ready"`
- **Expected:** All standard tables, visualizations (heatmap, ROC, survival curves), and copy-ready report populate. Outputs are suitable for manuscript figures and tables.
- **Pass:** [ ]

---

## 3. NRI/IDI/Pseudo-R2 (6 tests)

### Test 3.1: NRI at Default Time Points
- **Data:** `stagemigration_combined.rda`
- **Options:** `calculateNRI = TRUE`, `nriTimePoints = "12, 24, 60"`
- **Expected:** NRI table shows 3 rows (one per time point). Each row has NRI, NRI+, NRI-, 95% CI, and p-value. NRI = NRI+ + NRI-.
- **Pass:** [ ]

### Test 3.2: NRI Clinical Threshold Interpretation
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `calculateNRI = TRUE`, `nriClinicalThreshold = 0.20`
- **Expected:** NRI values below 0.20 are interpreted as clinically non-significant even if statistically significant. Values above 0.20 are flagged as clinically meaningful.
- **Pass:** [ ]

### Test 3.3: NRI with Custom Time Points
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `calculateNRI = TRUE`, `nriTimePoints = "6, 12, 36, 60, 120"`
- **Expected:** NRI table has 5 rows. No errors from time points exceeding maximum follow-up (graceful handling).
- **Pass:** [ ]

### Test 3.4: IDI Calculation
- **Data:** `stagemigration_combined.rda`
- **Options:** `calculateIDI = TRUE`
- **Expected:** IDI table shows IDI value, 95% CI, p-value, and clinical interpretation. Positive IDI indicates improved discrimination.
- **Pass:** [ ]

### Test 3.5: Pseudo-R2 Measures
- **Data:** `stagemigration_combined.rda`
- **Options:** `calculatePseudoR2 = TRUE`
- **Expected:** Table shows Nagelkerke, McFadden, and Cox-Snell R2 for both systems. Improvement column shows difference. All values between 0 and 1.
- **Pass:** [ ]

### Test 3.6: Combined Reclassification (NRI + IDI + Pseudo-R2)
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `calculateNRI = TRUE`, `calculateIDI = TRUE`, `calculatePseudoR2 = TRUE`
- **Expected:** All three tables populate simultaneously without conflict. Results are consistent (if NRI is large positive, IDI should also be positive).
- **Pass:** [ ]

---

## 4. Bootstrap & Cross-Validation (4 tests)

### Test 4.1: Bootstrap Validation
- **Data:** `stagemigration_combined.rda`
- **Options:** `performBootstrap = TRUE`, `bootstrapReps = 200` (reduced for speed)
- **Expected:** Bootstrap table shows metrics with Apparent, Bootstrap Mean, Bootstrap SE, 95% CI, Optimism, Optimism Corrected, Success Rate. Optimism-corrected C-index should be <= apparent C-index.
- **Pass:** [ ]

### Test 4.2: Bootstrap with Full Repetitions
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `performBootstrap = TRUE`, `bootstrapReps = 1000`
- **Expected:** Stable results with narrow bootstrap SE. Analysis completes within reasonable time (under 2 minutes for 700 obs). Success Rate should be > 90%.
- **Pass:** [ ]

### Test 4.3: K-Fold Cross-Validation
- **Data:** `stagemigration_combined.rda`
- **Options:** `performCrossValidation = TRUE`, `cvFolds = 5`
- **Expected:** Cross-validation table shows 5 fold rows plus summary. Each fold has train/test N, events, C-index for both systems, and difference. Cross-validation plot renders.
- **Pass:** [ ]

### Test 4.4: Internal-External Cross-Validation
- **Data:** `stagemigration_combined.rda`
- **Options:** `performCrossValidation = TRUE`, `institutionVariable = "cancer_type"` (using cancer_type as proxy for institution)
- **Expected:** Leave-one-center-out CV using cancer_type groups. Each fold validates on one cancer type, trains on others. Results reflect transportability.
- **Pass:** [ ]

---

## 5. Will Rogers Effect (3 tests)

### Test 5.1: Basic Will Rogers Analysis
- **Data:** `stagemigration_combined.rda`
- **Options:** `showWillRogersAnalysis = TRUE`
- **Expected:** `willRogersBasicAnalysis` table shows per-stage comparison of unchanged vs. migrated patients. Median survival and p-value for each stage.
- **Pass:** [ ]

### Test 5.2: Advanced Will Rogers Evidence Assessment
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `advancedMigrationAnalysis = TRUE`, `cancerType = "lung"`
- **Expected:** `willRogersEvidenceSummary` with multi-criteria traffic-light assessment. `willRogersEnhancedAnalysis` with before/after median survival. `willRogersStageDetail` with per-stage migration breakdown. `landmarkAnalysisResults` uses lung-specific landmark times (3, 6, 12, 24 months).
- **Pass:** [ ]

### Test 5.3: Will Rogers Visualizations
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `showWillRogersVisualization = TRUE`, `showMigrationSurvivalComparison = TRUE`
- **Expected:** `willRogersVisualization` (800x600 plot) renders showing before/after survival by stage. `migrationSurvivalComparison` (1400x700 plot) renders showing side-by-side KM curves.
- **Pass:** [ ]

---

## 6. DCA & Clinical Utility (3 tests)

### Test 6.1: Decision Curve Analysis
- **Data:** `stagemigration_combined.rda`
- **Options:** `performDCA = TRUE`, `showDecisionCurves = TRUE`
- **Expected:** `dcaResults` table shows threshold, net benefit for both systems, and improvement. `decisionCurves` plot renders with net benefit curves crossing the "treat all"/"treat none" reference lines.
- **Pass:** [ ]

### Test 6.2: DCA with Advanced Migration
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `advancedMigrationAnalysis = TRUE`, `performDCA = TRUE`
- **Expected:** `decisionCurveAnalysis` table (from advanced module) populates with time-point-specific threshold probabilities and net benefit comparison.
- **Pass:** [ ]

### Test 6.3: Clinical Interpretation Output
- **Data:** `stagemigration_combined.rda`
- **Options:** `showClinicalInterpretation = TRUE`, `cancerType = "colorectal"`
- **Expected:** `clinicalInterpretation` table has rows with Metric, Value, Interpretation, Recommendation. Recommendations use cancer-specific thresholds.
- **Pass:** [ ]

---

## 7. Calibration (4 tests)

### Test 7.1: Calibration Analysis Table
- **Data:** `stagemigration_combined.rda`
- **Options:** `performCalibration = TRUE`
- **Expected:** `calibrationAnalysis` table shows H-L Chi2, df, p-value, calibration slope, intercept, and CI for slope. Both staging systems compared.
- **Pass:** [ ]

### Test 7.2: Calibration Plots
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `performCalibration = TRUE`, `showCalibrationPlots = TRUE`
- **Expected:** `calibrationPlots` (800x400) renders with predicted vs. observed probability plots. Diagonal reference line visible.
- **Pass:** [ ]

### Test 7.3: Calibration via Advanced Migration
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `advancedMigrationAnalysis = TRUE` (calibration table visible when advancedMigrationAnalysis is enabled)
- **Expected:** `calibrationAnalysis` table populates even without explicit `performCalibration = TRUE`.
- **Pass:** [ ]

### Test 7.4: Optimism Correction
- **Data:** `stagemigration_combined.rda`
- **Options:** `performCalibration = TRUE`, `performBootstrap = TRUE`, `useOptimismCorrection = TRUE`
- **Expected:** Optimism-corrected metrics in bootstrap table. Calibration slope should ideally be close to 1.0. Corrected values are always <= apparent values.
- **Pass:** [ ]

---

## 8. Competing Risks (3 tests)

### Test 8.1: Basic Competing Risks
- **Data:** `stagemigration_combined.rda` (if event variable has multiple levels) or create test data with competing events
- **Options:** `performCompetingRisks = TRUE`
- **Expected:** `competingRisksEventDistribution` table shows per-stage event counts (primary, competing, censored). `competingRisksComparison` summarizes system comparison.
- **Pass:** [ ]

### Test 8.2: Advanced Competing Risks (Fine-Gray)
- **Data:** `stagemigration_combined.rda`
- **Options:** `performCompetingRisksAdvanced = TRUE`, `competingRisksMethod = "comprehensive"`
- **Expected:** `fineGrayResults` table shows subdistribution HRs per stage per system. `causeSpecificResults` shows cause-specific HRs. `cifSummary` shows CIF estimates at specified time points.
- **Pass:** [ ]

### Test 8.3: Competing Risks C-Index
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `performCompetingRisksAdvanced = TRUE`, `calculateCRCIndex = TRUE`
- **Expected:** `competingRisksCIndex` table shows per-event-type C-index for both systems with CIs and improvement. Values between 0.5 and 1.0.
- **Pass:** [ ]

---

## 9. Random Forest [Experimental] (3 tests)

### Test 9.1: Basic Random Survival Forest
- **Data:** `stagemigration_combined.rda`
- **Options:** `performRandomForestAnalysis = TRUE`, `forestNTrees = 100` (reduced for speed)
- **Expected:** `forestModelPerformance` table shows C-index, OOB error, and Brier score for both staging systems. No errors from RSF fitting.
- **Pass:** [ ]

### Test 9.2: Variable Importance
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `performRandomForestAnalysis = TRUE`, `calculateVariableImportance = TRUE`, `forestNTrees = 200`
- **Expected:** `forestVariableImportance` table ranks variables by importance score. Staging variables should appear in the ranking. Clinical relevance column provides interpretation.
- **Pass:** [ ]

### Test 9.3: Forest vs. Cox Comparison
- **Data:** `stagemigration_combined.rda`
- **Options:** `performRandomForestAnalysis = TRUE`, `forestDiscriminationMetrics = TRUE`, `forestNTrees = 200`
- **Expected:** `forestCoxComparison` table shows Cox vs. Forest C-index for both systems. Best method column identifies superior approach.
- **Pass:** [ ]

---

## 10. Survival Curves & Plots (4 tests)

### Test 10.1: Separate Survival Curves
- **Data:** `stagemigration_combined.rda`
- **Options:** `showSurvivalCurves = TRUE`, `survivalPlotType = "separate"`
- **Expected:** `survivalCurves` plot renders with stacked panels (one per staging system). Plot height ~1000px. Each panel shows KM curves by stage.
- **Pass:** [ ]

### Test 10.2: Side-by-Side Survival Curves
- **Data:** `stagemigration_breast_cancer.rda`
- **Options:** `showSurvivalCurves = TRUE`, `survivalPlotType = "sidebyside"`, `showConfidenceIntervals = TRUE`, `showRiskTables = TRUE`
- **Expected:** Two panels side by side (wider plot ~1200px). Confidence bands visible. Risk table below each panel.
- **Pass:** [ ]

### Test 10.3: Migration Heatmap
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `showMigrationHeatmap = TRUE`
- **Expected:** `migrationHeatmap` (700x500) renders. Color intensity reflects patient counts. Diagonal (unchanged) is typically darkest. Labels show patient counts.
- **Pass:** [ ]

### Test 10.4: Sankey Flow Diagram
- **Data:** `stagemigration_colorectal_cancer.rda`
- **Options:** `showSankeyDiagram = TRUE`
- **Expected:** `sankeyDiagram` (900x600) renders. Left side shows old stages, right shows new stages. Flow width proportional to patient count. Major migration patterns clearly visible.
- **Pass:** [ ]

---

## 11. Clinical Presets (3 tests)

### Test 11.1: Routine Clinical Preset
- **Data:** `stagemigration_combined.rda`
- **Options:** `clinicalPreset = "routine_clinical"`, all core variables assigned
- **Expected:** Migration overview, migration matrix, stage distribution, statistical comparison, and NRI tables visible. No bootstrap, ROC, or DCA. Explanations enabled. Note on `migrationOverview` confirms preset.
- **Pass:** [ ]

### Test 11.2: Research Study Preset
- **Data:** `stagemigration_lung_cancer.rda`
- **Options:** `clinicalPreset = "research_study"`, all core variables assigned
- **Expected:** All tables from routine_clinical plus concordance comparison, IDI, bootstrap, ROC, and DCA tables. Copy-ready report generated.
- **Pass:** [ ]

### Test 11.3: Custom Preset Override
- **Data:** `stagemigration_combined.rda`
- **Options:** `clinicalPreset = "custom"`, manually enable only `showMigrationMatrix = TRUE`, `calculateNRI = TRUE`
- **Expected:** Only migration matrix and NRI tables visible. No other tables or presets applied.
- **Pass:** [ ]

---

## 12. Edge Cases (6 tests)

### Test 12.1: Small Sample (N < 50)
- **Data:** `stagemigration_small_sample.rda`
- **Options:** Comprehensive analysis with bootstrap
- **Expected:** Analysis completes without crashing. Sample size warnings in output. Bootstrap may show lower success rate. NRI/IDI may show wide CIs or NA values for time points with insufficient events.
- **Pass:** [ ]

### Test 12.2: All Censored (No Events)
- **Data:** Create test data with event = 0 for all patients, or use `stagemigration_problematic.rda` if it contains this scenario
- **Options:** Basic analysis
- **Expected:** Graceful error message: "Insufficient events for survival analysis" or similar. No R crash. Welcome message or validation error displayed.
- **Pass:** [ ]

### Test 12.3: Identical Staging (old_stage == new_stage)
- **Data:** `stagemigration_combined.rda` with new_stage manually set equal to old_stage (or test subset)
- **Options:** `showMigrationOverview = TRUE`, `showMigrationMatrix = TRUE`
- **Expected:** Migration rate = 0%. All patients on diagonal. C-index difference = 0. NRI = 0. No division-by-zero errors.
- **Pass:** [ ]

### Test 12.4: Single Stage Level
- **Data:** Create or filter data where all patients are Stage I in both systems
- **Options:** Basic analysis
- **Expected:** Graceful error: "Staging variables must have 2+ levels" or equivalent. Migration matrix is 1x1. Cox model cannot be fitted (handled gracefully).
- **Pass:** [ ]

### Test 12.5: Large Dataset Performance
- **Data:** `stagemigration_large_performance.rda` (10000+ obs)
- **Options:** `analysisType = "comprehensive"`, `optimizeForLargeDatasets = TRUE`
- **Expected:** Analysis completes within 5 minutes. Memory optimization kicks in (integer conversion, factor compression). No out-of-memory errors. All tables populate correctly.
- **Pass:** [ ]

### Test 12.6: Missing Data Handling
- **Data:** `stagemigration_problematic.rda`
- **Options:** Comprehensive analysis
- **Expected:** Missing values are removed (complete case analysis). Validation warnings shown for each variable with missing data. Sample size in output reflects complete cases only. No silent data loss.
- **Pass:** [ ]

---

## 13. Display Options (3 tests)

### Test 13.1: Explanations Toggle
- **Data:** `stagemigration_combined.rda`
- **Options:** `showExplanations = TRUE`, enable several analysis modules
- **Expected:** Explanation Html items visible where defined (bootstrap, Will Rogers, DCA, calibration, clinical interpretation, etc.). Explanations are informative and accurate.
- **Pass:** [ ]

### Test 13.2: Abbreviation Glossary
- **Data:** `stagemigration_combined.rda`
- **Options:** `showAbbreviationGlossary = TRUE`
- **Expected:** `abbreviationGlossary` Html item renders with comprehensive list of abbreviations (NRI, IDI, DCA, C-index, HR, CI, KM, ROC, AUC, RMST, CIF, etc.).
- **Pass:** [ ]

### Test 13.3: Copy-Ready Report with Turkish Language
- **Data:** `stagemigration_combined.rda`
- **Options:** `generateCopyReadyReport = TRUE`, `preferredLanguage = "tr"`
- **Expected:** `copyReadyReport` Html renders with Turkish headers (Klinik Ozet, Yontem, Temel Bulgular, Klinik Degerlendirme, Oneri). Scientific terms remain in English/international format.
- **Pass:** [ ]

---

## COMPLETE OPTION COVERAGE CHECKLIST

Below is every option defined in `stagemigration.a.yaml` with its test coverage status. Mark each option as Covered (C) or Not Covered (N) after running tests.

### Core Variables
- [ ] `oldStage` -- Test 1.1
- [ ] `newStage` -- Test 1.1
- [ ] `survivalTime` -- Test 1.1
- [ ] `event` -- Test 1.1
- [ ] `eventLevel` -- Test 1.1

### Clinical Presets & Configuration
- [ ] `clinicalPreset` -- Tests 11.1, 11.2, 11.3
- [ ] `enableGuidedMode` -- Manual test needed
- [ ] `generateCopyReadyReport` -- Test 13.3
- [ ] `enableAccessibilityFeatures` -- Manual test needed (TODO: Wire to .b.R)
- [ ] `preferredLanguage` -- Test 13.3
- [ ] `enableProgressIndicators` -- Manual test needed
- [ ] `optimizeForLargeDatasets` -- Test 12.5
- [ ] `complexityMode` -- Manual test needed (TODO: Wire to .b.R)
- [ ] `analysisType` -- Tests 2.1, 2.2, 2.3
- [ ] `confidenceLevel` -- Manual test: change to 0.90 and verify CI widths

### Advanced Statistical Methods
- [ ] `calculateNRI` -- Tests 3.1, 3.2, 3.3
- [ ] `nriTimePoints` -- Tests 3.1, 3.3
- [ ] `calculateIDI` -- Test 3.4
- [ ] `performROCAnalysis` -- Manual test: enable and verify `rocAnalysis` table
- [ ] `rocTimePoints` -- Manual test: change time points
- [ ] `performDCA` -- Test 6.1
- [ ] `performCalibration` -- Tests 7.1, 7.2

### Validation & Bootstrap
- [ ] `performBootstrap` -- Tests 4.1, 4.2
- [ ] `bootstrapReps` -- Tests 4.1, 4.2
- [ ] `performCrossValidation` -- Tests 4.3, 4.4
- [ ] `cvFolds` -- Test 4.3
- [ ] `institutionVariable` -- Test 4.4

### Clinical Significance
- [ ] `clinicalSignificanceThreshold` -- Manual test: change to 0.05 and verify interpretation
- [ ] `nriClinicalThreshold` -- Test 3.2

### Homogeneity & Trend
- [ ] `performHomogeneityTests` -- Manual test: enable and verify `homogeneityTests` table
- [ ] `performTrendTests` -- Manual test: enable and verify `trendTests` table

### Model Comparison
- [ ] `performLikelihoodTests` -- Manual test: enable and verify `likelihoodTests` and `linearTrendTest`
- [ ] `calculatePseudoR2` -- Test 3.5

### Table Display
- [ ] `showMigrationOverview` -- Test 1.2
- [ ] `showMigrationSummary` -- Manual test: enable and verify `migrationSummary`
- [ ] `showStageDistribution` -- Test 1.4
- [ ] `showMigrationMatrix` -- Test 1.3
- [ ] `showStatisticalComparison` -- Manual test: verify `statisticalComparison` and `enhancedLRComparison`
- [ ] `showConcordanceComparison` -- Manual test: verify `concordanceComparison`

### Visualizations
- [ ] `showMigrationHeatmap` -- Test 10.3
- [ ] `showSankeyDiagram` -- Test 10.4
- [ ] `showROCComparison` -- Manual test: enable and verify ROC plot
- [ ] `showCalibrationPlots` -- Test 7.2
- [ ] `showDecisionCurves` -- Test 6.1
- [ ] `showForestPlot` -- Manual test: enable and verify HR forest plot
- [ ] `showWillRogersAnalysis` -- Test 5.1
- [ ] `showWillRogersVisualization` -- Test 5.3
- [ ] `showMigrationSurvivalComparison` -- Test 5.3

### Survival Curves
- [ ] `showSurvivalCurves` -- Tests 10.1, 10.2
- [ ] `survivalPlotType` -- Tests 10.1, 10.2
- [ ] `showConfidenceIntervals` -- Test 10.2
- [ ] `showRiskTables` -- Test 10.2
- [ ] `plotTimeRange` -- Manual test: set to "60" and verify X-axis limit

### Reporting & Interpretation
- [ ] `showClinicalInterpretation` -- Test 6.3
- [ ] `showStatisticalSummary` -- Manual test: enable and verify `statisticalSummary`
- [ ] `showMethodologyNotes` -- Manual test: enable and verify `methodologyNotes`
- [ ] `includeEffectSizes` -- Manual test: enable and verify `effectSizes`
- [ ] `advancedMigrationAnalysis` -- Test 5.2
- [ ] `generateExecutiveSummary` -- Manual test: enable and verify `executiveSummary`
- [ ] `cancerType` -- Tests 5.2, 6.3
- [ ] `useOptimismCorrection` -- Test 7.4
- [ ] `showExplanations` -- Test 13.1
- [ ] `showAbbreviationGlossary` -- Test 13.2

### Stage Migration Effect & RMST
- [ ] `calculateSME` -- Manual test: enable and verify `stageMigrationEffect` and `stageMigrationEffectAssessment`
- [ ] `calculateRMST` -- Manual test: enable and verify `rmstByStage` and `rmstComparison`

### Multifactorial Analysis
- [ ] `enableMultifactorialAnalysis` -- Manual test: enable with covariates
- [ ] `continuousCovariates` -- Manual test: assign age as continuous covariate
- [ ] `categoricalCovariates` -- Manual test: assign sex as categorical covariate
- [ ] `multifactorialComparisonType` -- Manual test: try each option
- [ ] `baselineModel` -- Manual test: try each option
- [ ] `performInteractionTests` -- Manual test: enable and verify `interactionTests`
- [ ] `stratifiedAnalysis` -- Manual test: enable and verify `stratifiedAnalysisTable`
- [ ] `showMultifactorialTables` -- Manual test: enable and verify `multifactorialResults`
- [ ] `showAdjustedCIndexComparison` -- Manual test: enable and verify `adjustedCIndexComparison`
- [ ] `showNestedModelTests` -- Manual test: enable and verify `nestedModelTests`
- [ ] `showStepwiseResults` -- Manual test: enable and verify `stepwiseResults`

### Competing Risks
- [ ] `performCompetingRisks` -- Test 8.1
- [ ] `competingEventVar` -- Test 8.1
- [ ] `performCompetingRisksAdvanced` -- Test 8.2
- [ ] `competingRisksMethod` -- Test 8.2
- [ ] `cifTimePoints` -- Manual test: change time points
- [ ] `competingEventLevels` -- Manual test: specify event categories
- [ ] `primaryEventLevel` -- Manual test: specify primary event
- [ ] `generateCIFPlots` -- Manual test (TODO: Wire to .b.R)
- [ ] `performGrayTest` -- Manual test: enable and verify Gray test p-value in `cifSummary`
- [ ] `cifConfidenceLevel` -- Manual test (TODO: Wire to .b.R)
- [ ] `competingRisksCovariates` -- Manual test: add covariates
- [ ] `stratifyByStaging` -- Manual test: enable
- [ ] `calculateCRCIndex` -- Test 8.3
- [ ] `generateCRSummary` -- Manual test: enable and verify `competingRisksSummary`

### Optimal Cut-point
- [ ] `performOptimalCutpoint` -- Manual test: assign continuous variable
- [ ] `continuousStageVariable` -- Manual test: assign age or tumor size
- [ ] `cutpointMethod` -- Manual test: try each method
- [ ] `cutpointRange` -- Manual test: change range
- [ ] `multipleTestingCorrection` -- Manual test: try each correction
- [ ] `validateCutpoint` -- Manual test: enable and verify `cutpointValidation`
- [ ] `cutpointBootstrap` -- Manual test: enable
- [ ] `cutpointBootstrapReps` -- Manual test: set to 200
- [ ] `generateStagingSystem` -- Manual test: enable and verify `generatedStagingSystem`
- [ ] `stagingSystemLevels` -- Manual test: set to 4

### SHAP Interpretability
- [ ] `performSHAPAnalysis` -- Manual test: enable
- [ ] `shapAnalysisType` -- Manual test: try each option
- [ ] `shapCovariates` -- Manual test: assign variables
- [ ] `shapSampleSize` -- Manual test: set to 50
- [ ] `shapBackgroundSamples` -- Manual test: set to 25
- [ ] `shapExplanationType` -- Manual test: try each option
- [ ] `generateSHAPPlots` -- Manual test (TODO: Wire to .b.R)
- [ ] `shapPatientProfiles` -- Manual test: try each option
- [ ] `shapInteractionAnalysis` -- Manual test: enable and verify `shapInteractions`
- [ ] `shapClinicalThresholds` -- Manual test: change thresholds

### Random Survival Forests
- [ ] `performRandomForestAnalysis` -- Test 9.1
- [ ] `forestModelType` -- Manual test: try each option
- [ ] `forestNTrees` -- Tests 9.1, 9.2
- [ ] `forestMTry` -- Manual test: set to specific number
- [ ] `forestMinNodeSize` -- Manual test: set to 10
- [ ] `forestCovariates` -- Manual test: assign covariates
- [ ] `calculateVariableImportance` -- Test 9.2
- [ ] `forestImportanceType` -- Manual test (TODO: Wire to .b.R)
- [ ] `performForestValidation` -- Manual test: enable
- [ ] `forestPredictionTimePoints` -- Manual test: change time points
- [ ] `generateSurvivalPredictions` -- Manual test: enable and verify `forestSurvivalPredictions`
- [ ] `forestDiscriminationMetrics` -- Test 9.3
- [ ] `forestStagingComparison` -- Manual test (TODO: Wire to .b.R)
- [ ] `forestBootstrap` -- Manual test (TODO: Wire to .b.R)
- [ ] `forestBootstrapSamples` -- Manual test (TODO: Wire to .b.R)
- [ ] `generateForestSummary` -- Manual test (TODO: Wire to .b.R)
- [ ] `rfAnalyzeOldStage` -- Manual test: disable and verify exclusion
- [ ] `rfAnalyzeNewStage` -- Manual test: disable and verify exclusion
- [ ] `rfMtryAuto` -- Manual test: disable and verify manual mtry
- [ ] `rfBootstrapType` -- Manual test: switch to by.node
- [ ] `rfSamplingType` -- Manual test: switch to swor
- [ ] `rfMinimalDepth` -- Manual test: enable

### Cure Models
- [ ] `performCureModelAnalysis` -- Manual test: enable
- [ ] `cureModelType` -- Manual test: try each option
- [ ] `cureDistribution` -- Manual test: try each distribution
- [ ] `cureAnalyzeOldStage` -- Manual test: enable
- [ ] `cureAnalyzeNewStage` -- Manual test: enable
- [ ] `cureFractionEstimation` -- Manual test (TODO: Wire to .b.R)
- [ ] `cureConfidenceLevel` -- Manual test (TODO: Wire to .b.R)
- [ ] `cureBootstrapCI` -- Manual test: enable and verify `cureModelBootstrap`
- [ ] `cureBootstrapReps` -- Manual test: set to 200
- [ ] `cureTimeHorizon` -- Manual test: set to 60
- [ ] `curePlateauThreshold` -- Manual test: change threshold
- [ ] `cureCovariates` -- Manual test: assign variables
- [ ] `cureModelComparison` -- Manual test (TODO: Wire to .b.R)
- [ ] `cureStageSpecificAnalysis` -- Manual test: enable and verify `stageSpecificCureAnalysis`
- [ ] `cureGoodnessOfFit` -- Manual test (TODO: Wire to .b.R)
- [ ] `generateCureSummary` -- Manual test (TODO: Wire to .b.R)

### Multi-State Models
- [ ] `performMultiStateAnalysis` -- Manual test: enable with state variable
- [ ] `multiStateModel` -- Manual test (TODO: Wire to .b.R)
- [ ] `stateVariable` -- Manual test: assign variable
- [ ] `transitionTimeVariable` -- Manual test: assign variable
- [ ] `multiStateStates` -- Manual test: define states
- [ ] `absorptionStates` -- Manual test: define absorption states
- [ ] `multiStateCovariates` -- Manual test: assign covariates
- [ ] `calculateTransitionProbabilities` -- Manual test: enable and verify `transitionProbabilities`
- [ ] `multiStateTimePoints` -- Manual test: change time points
- [ ] `generateTransitionMatrix` -- Manual test: enable and verify `transitionIntensities`
- [ ] `multiStateGraphics` -- Manual test (TODO: Wire to .b.R)
- [ ] `msStratifyByStaging` -- Manual test (TODO: Wire to .b.R)
- [ ] `multiStateValidation` -- Manual test (TODO: Wire to .b.R)
- [ ] `generateMSMSummary` -- Manual test: enable and verify `multiStateSummary`

### Frailty Models
- [ ] `performFrailtyModelsAnalysis` -- Manual test: enable with cluster variable
- [ ] `frailtyClusterVariable` -- Manual test: assign institution/center variable
- [ ] `frailtyDistribution` -- Manual test (TODO: Wire to .b.R)
- [ ] `frailtyBootstrap` -- Manual test: enable
- [ ] `frailtyBootstrapSamples` -- Manual test: set to 200
- [ ] `frailtyVarianceComponents` -- Manual test (TODO: Wire to .b.R)
- [ ] `frailtyHeterogeneityTest` -- Manual test (TODO: Wire to .b.R)
- [ ] `frailtyClusterComparison` -- Manual test: enable
- [ ] `frailtyModelSelection` -- Manual test (TODO: Wire to .b.R)
- [ ] `frailtyPredictiveAccuracy` -- Manual test (TODO: Wire to .b.R)
- [ ] `frailtyDiagnostics` -- Manual test: enable
- [ ] `frailtyAdvancedInference` -- Manual test (TODO: Wire to .b.R)

### Win Ratio
- [ ] `performWinRatioAnalysis` -- Manual test: enable
- [ ] `winRatioEndpoints` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioDeathVariable` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioSecondaryEndpoint` -- Manual test: assign variable
- [ ] `wrSecondaryDirection` -- Manual test: try each option
- [ ] `winRatioTertiaryEndpoint` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioTimeVariables` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioMatchingStrategy` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioConfidenceMethod` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioBootstrapSamples` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioConfidenceLevel` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioHandleTies` -- Manual test (TODO: Wire to .b.R)
- [ ] `winRatioSensitivityAnalysis` -- Manual test: enable
- [ ] `winRatioGeneralizedPairwise` -- Manual test (TODO: Wire to .b.R)

### Interval Censoring
- [ ] `performIntervalCensoringAnalysis` -- Manual test: enable with left/right time variables
- [ ] `intervalCensoringLeftTime` -- Manual test: assign variable
- [ ] `intervalCensoringRightTime` -- Manual test: assign variable
- [ ] `intervalCensoringDistribution` -- Manual test: try each distribution
- [ ] `intervalCensoringModel` -- Manual test: try each model type
- [ ] `intervalCensoringBootstrap` -- Manual test: enable
- [ ] `intervalCensoringBootstrapSamples` -- Manual test: set to 200
- [ ] `intervalCensoringCompareStages` -- Manual test: enable and verify comparison table
- [ ] `intervalCensoringPlots` -- Manual test (TODO: Wire to .b.R)
- [ ] `intervalCensoringDiagnostics` -- Manual test: enable
- [ ] `intervalCensoringPredictionTime` -- Manual test: change time points
- [ ] `intervalCensoringConfidenceLevel` -- Manual test: change to 0.90
- [ ] `intervalCensoringAdjustVariables` -- Manual test: assign covariates

### Informative Censoring
- [ ] `performInformativeCensoringAnalysis` -- Manual test: enable
- [ ] `informativeCensoringTestMethod` -- Manual test: try each method
- [ ] `informativeCensoringCovariates` -- Manual test (TODO: Wire to .b.R)
- [ ] `informativeCensoringLandmarkTimes` -- Manual test: change times
- [ ] `informativeCensoringAdjustmentMethod` -- Manual test: try each method
- [ ] `informativeCensoringIPWVariables` -- Manual test (TODO: Wire to .b.R)
- [ ] `informativeCensoringSensitivityRange` -- Manual test: change range
- [ ] `informativeCensoringBootstrap` -- Manual test: enable
- [ ] `informativeCensoringBootstrapSamples` -- Manual test: set to 200
- [ ] `informativeCensoringAlpha` -- Manual test: change to 0.10
- [ ] `informativeCensoringPlots` -- Manual test (TODO: Wire to .b.R)
- [ ] `informativeCensoringCompareStages` -- Manual test: enable

### Concordance Probability
- [ ] `performConcordanceProbabilityAnalysis` -- Manual test: enable
- [ ] `concordanceProbabilityMethods` -- Manual test: try each method
- [ ] `concordanceProbabilityTimePoints` -- Manual test: change time points
- [ ] `concordanceProbabilityWeighting` -- Manual test: try each weighting
- [ ] `concordanceProbabilityBootstrap` -- Manual test: enable
- [ ] `concordanceProbabilityBootstrapSamples` -- Manual test (TODO: Wire to .b.R)
- [ ] `concordanceProbabilityConfidenceLevel` -- Manual test: change level
- [ ] `concordanceProbabilityCompareStages` -- Manual test: enable and verify comparison
- [ ] `concordanceProbabilityAdjustVariables` -- Manual test (TODO: Wire to .b.R)
- [ ] `concordanceProbabilityRobustnessAnalysis` -- Manual test: enable
- [ ] `concordanceProbabilityAlpha` -- Manual test: change alpha
- [ ] `concordanceProbabilityDiagnostics` -- Manual test: enable

### Clinical Utility Index
- [ ] `performClinicalUtilityAnalysis` -- Manual test: enable
- [ ] `clinicalUtilityPrevalence` -- Manual test: change prevalence
- [ ] `clinicalUtilityTimePoint` -- Manual test: change to 36
- [ ] `clinicalUtilityThresholds` -- Manual test: try each range
- [ ] `clinicalUtilityNNT` -- Manual test: enable
- [ ] `clinicalUtilityTreatmentEffect` -- Manual test: change HR
- [ ] `clinicalUtilityComparison` -- Manual test (TODO: Wire to .b.R)
- [ ] `clinicalUtilityCostEffectiveness` -- Manual test (TODO: Wire to .b.R)
- [ ] `clinicalUtilityCostPerIntervention` -- Manual test: change cost
- [ ] `clinicalUtilityBootstrap` -- Manual test: enable
- [ ] `clinicalUtilityBootstrapSamples` -- Manual test: set to 200
- [ ] `clinicalUtilityTimeVarying` -- Manual test: enable

---

**Total options in .a.yaml:** ~180+
**Options covered by numbered tests:** ~55
**Options requiring manual testing:** ~125 (many are experimental/TODO-wired)
