# Advanced TNM Stage Migration Analysis Documentation

## Feature Summary

The Advanced TNM Stage Migration Analysis (`stagemigration`) is a comprehensive jamovi function designed for validating whether a new TNM staging system provides superior prognostic discrimination compared to an existing system. It implements state-of-the-art statistical methods drawn from the staging validation literature, including concordance analysis, reclassification metrics (NRI/IDI), decision curve analysis, bootstrap validation, and Will Rogers phenomenon detection, all within a clinician-friendly interface.

The primary target users are surgical and anatomical pathologists evaluating AJCC/TNM edition transitions, cancer registry analysts performing population-level staging comparisons, and clinical oncologists assessing institution-specific staging modifications. The function is also suited for biostatisticians supporting multi-institutional staging harmonization studies and biomarker-enhanced staging proposals. Clinical presets (Routine, Research, Publication) simplify the workflow for non-statistician users while preserving full manual configuration for advanced users.

The function spans 40+ statistical methods organized across 25+ analysis modules. Capabilities range from basic migration matrices and stage distribution comparisons through advanced discrimination metrics (C-index, NRI, IDI, pseudo-R2, time-dependent ROC, RMST), clinical utility assessment (DCA, net benefit), validation frameworks (bootstrap, k-fold cross-validation, optimism correction), and experimental modules (random survival forests, SHAP interpretability, cure models, multi-state models, frailty models, win ratio, competing risks, interval censoring, informative censoring detection, and concordance probability estimation). Version 0.0.31 of the analysis YAML.

---

## Feature Details

### Core Input Variables

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Original staging variable | `oldStage` (Variable, factor) | Original TNM Staging System | Used in all result tables | `.validateData()`, `.calculateBasicMigration()` |
| New staging variable | `newStage` (Variable, factor) | New/Revised TNM Staging System | Used in all result tables | `.validateData()`, `.calculateBasicMigration()` |
| Follow-up time | `survivalTime` (Variable, numeric) | Follow-up Time (months) | Used in survival-dependent tables | `.validateData()` |
| Event indicator | `event` (Variable, factor/numeric) | Event Status (Death/Outcome) | Used in survival-dependent tables | `.validateData()`, `createEventBinary()` |
| Event level selector | `eventLevel` (Level, linked to event) | Event Level | -- | `.validateData()` |

### Clinical Presets

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Analysis preset | `clinicalPreset` (List: routine_clinical, research_study, publication_ready, custom) | Clinical Analysis Preset | `migrationOverview` (note) | `.applyClinicalPreset()` |
| Cancer type | `cancerType` (List: general, lung, breast, colorectal, prostate, headneck, melanoma, other) | Cancer Type (Optional) | `clinicalInterpretation`, `landmarkAnalysisResults` | Cancer-specific thresholds and landmark times |
| Complexity mode | `complexityMode` (List: quick, standard, comprehensive, custom) | Analysis Complexity Mode | Controls progressive disclosure | TODO: Wire to .b.R |
| Analysis scope | `analysisType` (List: basic, standard, comprehensive, publication) | Analysis Scope | Controls which modules execute | `.run()` flow control |
| Confidence level | `confidenceLevel` (Number, 0.80-0.99, default 0.95) | Confidence Level | All CI calculations | `.ciProbs()` |
| Guided mode | `enableGuidedMode` (Bool) | Guided Analysis Mode | `guidedModeProgress` (Html) | `.generateGuidedModeProgress()` |
| Copy-ready report | `generateCopyReadyReport` (Bool) | Generate Copy-Ready Clinical Summary | `copyReadyReport` (Html) | `.generateCopyReadyReport()` |
| Accessibility features | `enableAccessibilityFeatures` (Bool) | Accessibility Features | -- | TODO: Wire to .b.R |
| Output language | `preferredLanguage` (List: en, tr) | Output Language | Localized text in report | `.generateCopyReadyReport()` |
| Progress indicators | `enableProgressIndicators` (Bool) | Show Progress Indicators | -- | `.showProgressIndicator()` |
| Large dataset optimization | `optimizeForLargeDatasets` (Bool) | Large Dataset Optimization | -- | `.optimizeMemoryUsage()` |

### Migration Analysis

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Migration overview table | `showMigrationOverview` (Bool, default true) | Migration Overview Table | `migrationOverview` (Table) | `.calculateBasicMigration()` |
| Migration matrix | `showMigrationMatrix` (Bool, default true) | Migration Matrix Table | `migrationMatrix` (Table) | `.calculateBasicMigration()` |
| Stage distribution | `showStageDistribution` (Bool) | Stage Distribution Comparison Table | `stageDistribution` (Table) | `.calculateBasicMigration()` |
| Migration summary | `showMigrationSummary` (Bool) | Migration Summary Table | `migrationSummary` (Table) | Chi-square/Fisher tests |
| Advanced migration | `advancedMigrationAnalysis` (Bool) | Advanced Migration Analysis | `monotonicityCheck`, `willRogersAnalysis`, `willRogersEnhancedAnalysis`, `willRogersStageDetail`, `stageSpecificCIndex`, `enhancedPseudoR2`, `enhancedReclassificationMetrics`, `proportionalHazardsTest`, `decisionCurveAnalysis`, `comparativeAnalysisDashboard`, `integratedAUCAnalysis`, `calibrationAnalysis`, `willRogersEvidenceSummary`, `willRogersClinicalRecommendation`, `enhancedMigrationPatternAnalysis`, `landmarkAnalysisResults`, `advancedMigrationHeatmapStats` | Comprehensive multi-table analysis |
| Migration overview explanation | -- | -- | `migrationOverviewExplanation` (Html, visible: false) | `.setExplanationContent()` |
| Migration matrix explanation | -- | -- | `migrationMatrixExplanation` (Html, visible: false) | `.setExplanationContent()` |
| Stage distribution explanation | -- | -- | `stageDistributionExplanation` (Html, visible: false) | `.setExplanationContent()` |
| Migration summary explanation | -- | -- | `migrationSummaryExplanation` (Html, visible: false) | `.setExplanationContent()` |

### Concordance & Discrimination

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Statistical comparison | `showStatisticalComparison` (Bool) | Statistical Comparison Table | `statisticalComparison` (Table), `enhancedLRComparison` (Table) | `survival::concordance()`, `survival::coxph()` |
| Concordance comparison | `showConcordanceComparison` (Bool) | Concordance Comparison Table | `concordanceComparison` (Table) | `survival::concordance()` |
| Time-dependent ROC | `performROCAnalysis` (Bool) | Time-dependent ROC Analysis | `rocAnalysis` (Table) | `timeROC::timeROC()` |
| ROC time points | `rocTimePoints` (String, default "12, 24, 36, 60") | ROC Time Points (months) | `rocAnalysis` | Parsed as numeric vector |
| Likelihood ratio tests | `performLikelihoodTests` (Bool) | Likelihood Ratio Tests | `likelihoodTests` (Table), `linearTrendTest` (Table) | `stats::anova()` on Cox models |
| Effect sizes | `includeEffectSizes` (Bool) | Include Effect Sizes | `effectSizes` (Table) | Cohen's d, Cramer's V calculations |
| Stage homogeneity | `performHomogeneityTests` (Bool) | Stage Homogeneity Tests | `homogeneityTests` (Table) | Within-stage log-rank tests |
| Stage trend tests | `performTrendTests` (Bool) | Stage Trend Analysis | `trendTests` (Table) | Ordered stage trend analysis |

### Reclassification Metrics

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Net Reclassification Improvement | `calculateNRI` (Bool) | Net Reclassification Improvement (NRI) | `nriResults` (Table) | NRI calculation with NRI+, NRI- components |
| NRI time points | `nriTimePoints` (String, default "12, 24, 60") | NRI Time Points (months) | `nriResults` | Parsed as numeric vector |
| NRI clinical threshold | `nriClinicalThreshold` (Number, 0.10-0.50, default 0.20) | NRI Clinical Threshold | `nriResults` interpretation | Clinical significance gate |
| Integrated Discrimination Improvement | `calculateIDI` (Bool) | Integrated Discrimination Improvement (IDI) | `idiResults` (Table) | IDI with CI and interpretation |
| Pseudo R-squared | `calculatePseudoR2` (Bool) | Pseudo R-squared Measures | `pseudoR2Results` (Table) | Nagelkerke, McFadden, Cox-Snell |
| Clinical significance threshold | `clinicalSignificanceThreshold` (Number, 0.01-0.10, default 0.02) | Clinical Significance Threshold | Used in interpretation | Minimum C-index improvement |
| Stage Migration Effect | `calculateSME` (Bool) | Calculate Stage Migration Effect | `stageMigrationEffect` (Table), `stageMigrationEffectAssessment` (Table) | SME formula calculation |
| RMST metrics | `calculateRMST` (Bool) | Calculate RMST Metrics | `rmstByStage` (Table), `rmstComparison` (Table) | `stagemigration_calculateRMST()` |

### Calibration

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Calibration analysis | `performCalibration` (Bool) | Calibration Analysis | `calibrationAnalysis` (Table) | Hosmer-Lemeshow, calibration slope/intercept |
| Calibration plots | `showCalibrationPlots` (Bool) | Calibration Plots | `calibrationPlots` (Image, renderFun: `.plotCalibration`) | ggplot2 calibration visualization |
| Optimism correction | `useOptimismCorrection` (Bool) | Apply Optimism Correction | `bootstrapResults` optimism column | Bootstrap-based correction |

### DCA & Clinical Utility

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Decision curve analysis | `performDCA` (Bool) | Decision Curve Analysis | `dcaResults` (Table) | `dcurves::dca()` |
| Decision curves plot | `showDecisionCurves` (Bool) | Decision Curves | `decisionCurves` (Image, renderFun: `.plotDecisionCurves`) | ggplot2 net benefit curves |
| Clinical interpretation | `showClinicalInterpretation` (Bool) | Clinical Interpretation Guide | `clinicalInterpretation` (Table) | Metric/Value/Interpretation/Recommendation |
| Executive summary | `generateExecutiveSummary` (Bool) | Executive Summary | `executiveSummary` (Table) | Category/Finding/Evidence/Strength |
| Statistical summary | `showStatisticalSummary` (Bool) | Statistical Summary Table | `statisticalSummary` (Table) | Method/Result/CI/p/Significance |
| Methodology notes | `showMethodologyNotes` (Bool) | Methodology Notes | `methodologyNotes` (Html) | Static explanatory content |
| Clinical utility index | `performClinicalUtilityAnalysis` (Bool) | Clinical Utility Index Analysis | -- | Sensitivity/specificity with prevalence |
| Disease prevalence | `clinicalUtilityPrevalence` (Number, 0.01-0.99, default 0.2) | Disease Prevalence | -- | Used in clinical utility calculations |
| Clinical utility time point | `clinicalUtilityTimePoint` (Integer, 1-240, default 60) | Time Point for Analysis (months) | -- | Assessment horizon |
| Risk threshold range | `clinicalUtilityThresholds` (List: conservative, standard, aggressive, comprehensive) | Risk Threshold Range | -- | DCA threshold grid |
| NNT analysis | `clinicalUtilityNNT` (Bool) | Number Needed to Treat Analysis | -- | NNT/NNH calculations |
| Treatment effect (HR) | `clinicalUtilityTreatmentEffect` (Number, 0.1-2.0, default 0.7) | Treatment Effect (Hazard Ratio) | -- | Assumed treatment HR |
| Comparative utility | `clinicalUtilityComparison` (Bool) | Comparative Utility Assessment | -- | TODO: Wire to .b.R |
| Cost-effectiveness | `clinicalUtilityCostEffectiveness` (Bool) | Cost-Effectiveness Integration | -- | TODO: Wire to .b.R |
| Cost per intervention | `clinicalUtilityCostPerIntervention` (Integer, 100-100000, default 5000) | Cost Per Intervention ($) | -- | ICER calculations |
| Clinical utility bootstrap | `clinicalUtilityBootstrap` (Bool) | Bootstrap Validation | -- | CI for utility metrics |
| Clinical utility bootstrap samples | `clinicalUtilityBootstrapSamples` (Integer, 100-2000, default 500) | Bootstrap Samples | -- | Replication count |
| Time-varying utility | `clinicalUtilityTimeVarying` (Bool) | Time-Varying Utility Analysis | -- | Multi-timepoint utility |

### Bootstrap & Validation

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Bootstrap validation | `performBootstrap` (Bool) | Bootstrap Validation | `bootstrapResults` (Table) | `boot::boot()` with optimism correction |
| Bootstrap repetitions | `bootstrapReps` (Number, 100-2000, default 1000) | Bootstrap Repetitions | `bootstrapResults` | `.getBootstrapReps()` |
| Cross-validation | `performCrossValidation` (Bool) | Cross-Validation | `crossValidationResults` (Table), `crossValidationPlot` (Image, renderFun: `.plotCrossValidation`) | K-fold CV with C-index comparison |
| CV folds | `cvFolds` (Number, 3-10, default 5) | CV Folds | `crossValidationResults` | Fold count |
| Institution variable | `institutionVariable` (Variable, factor/numeric) | Institution Variable | `crossValidationResults` | Internal-external CV by center |

### Survival Curves

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Show survival curves | `showSurvivalCurves` (Bool) | Show Survival Curves | `survivalCurves` (Image, 900x700, renderFun: `.plotSurvivalCurves`) | `survminer::ggsurvplot()` |
| Plot type | `survivalPlotType` (List: separate, sidebyside, overlay) | Survival Plot Display | `survivalCurves` | Layout switching in `.init()` |
| Confidence intervals | `showConfidenceIntervals` (Bool) | Show Confidence Intervals | `survivalCurves` | CI bands on KM curves |
| Risk tables | `showRiskTables` (Bool) | Show Risk Tables | `survivalCurves` | At-risk table below curves |
| Plot time range | `plotTimeRange` (String, default "auto") | Plot Time Range (months) | `survivalCurves` | X-axis limit |

### Will Rogers Analysis

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Will Rogers basic analysis | `showWillRogersAnalysis` (Bool) | Will Rogers Phenomenon Analysis | `willRogersBasicAnalysis` (Table) | Unchanged vs. migrated survival comparison |
| Will Rogers visualization | `showWillRogersVisualization` (Bool) | Will Rogers Effect Visualization | `willRogersVisualization` (Image, 800x600, renderFun: `.plotWillRogersEffect`) | Before/after stage survival |
| Migration survival comparison | `showMigrationSurvivalComparison` (Bool) | Migration Survival Curve Comparison | `migrationSurvivalComparison` (Image, 1400x700, renderFun: `.plotMigrationSurvivalComparison`) | KM curves before/after reclassification |
| Will Rogers evidence summary | (via `advancedMigrationAnalysis`) | -- | `willRogersEvidenceSummary` (Table), `willRogersClinicalRecommendation` (Table), `willRogersEnhancedAnalysis` (Table), `willRogersStageDetail` (Table) | Multi-criteria evidence framework with traffic-light grading |

### Competing Risks

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Basic competing risks | `performCompetingRisks` (Bool) | Perform Competing Risks Analysis | `competingRisksEventDistribution` (Table), `competingRisksComparison` (Table) | `stagemigration-competing-risks.R` |
| Competing event variable | `competingEventVar` (Variable, factor/numeric) | Competing Event Variable | `competingRisksEventDistribution` | Multi-level event detection |
| Advanced competing risks | `performCompetingRisksAdvanced` (Bool) | Advanced Competing Risks Analysis | `fineGrayResults`, `causeSpecificResults`, `cifSummary`, `competingRisksCIndex`, `competingRisksSummary` | Fine-Gray, cause-specific models |
| Competing risks method | `competingRisksMethod` (List: finegray, causespecific, comprehensive) | Competing Risks Method | `fineGrayResults`, `causeSpecificResults` | Method selection |
| CIF time points | `cifTimePoints` (String, default "12, 24, 36, 60") | CIF Time Points (months) | `cifSummary` | Cumulative incidence timepoints |
| Competing event levels | `competingEventLevels` (String) | Competing Event Categories | -- | Event category parsing |
| Primary event level | `primaryEventLevel` (String, default "cancer_death") | Primary Event of Interest | -- | Primary event for CIF |
| CIF plots | `generateCIFPlots` (Bool) | Generate CIF Plots | -- | TODO: Wire to .b.R |
| Gray test | `performGrayTest` (Bool) | Gray Test for CIF Equality | `cifSummary` Gray_Test_P column | CIF equality testing |
| CIF confidence level | `cifConfidenceLevel` (Number, 0.80-0.99, default 0.95) | CIF Confidence Level | -- | TODO: Wire to .b.R |
| CR covariates | `competingRisksCovariates` (Variables) | Competing Risks Covariates | `fineGrayResults` | Adjusted Fine-Gray models |
| Stratify by staging | `stratifyByStaging` (Bool) | Stratify Analysis by Staging System | -- | Separate CR analysis per system |
| CR C-index | `calculateCRCIndex` (Bool) | Competing Risks C-Index | `competingRisksCIndex` (Table) | CR-specific discrimination |
| CR summary | `generateCRSummary` (Bool) | Competing Risks Summary Table | `competingRisksSummary` (Table) | Comprehensive CR summary |

### Random Forest [Experimental]

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Random forest analysis | `performRandomForestAnalysis` (Bool) | Random Survival Forests | `forestModelPerformance` (Table) | `randomForestSRC::rfsrc()` |
| Forest model type | `forestModelType` (List: rsf, cforest, extraTrees, ensemble) | Forest Model Type | `forestModelPerformance` | TODO: Wire cforest/extraTrees |
| Number of trees | `forestNTrees` (Number, 50-2000, default 500) | Number of Trees | `forestModelPerformance` | rfsrc ntree parameter |
| Variables per split | `forestMTry` (String, default "auto") | Variables per Split (mtry) | -- | Auto or user-specified |
| Min node size | `forestMinNodeSize` (Number, 1-50, default 3) | Minimum Node Size | -- | Terminal node size |
| Forest covariates | `forestCovariates` (Variables) | Forest Covariates | `forestVariableImportance` | Additional predictors |
| Variable importance | `calculateVariableImportance` (Bool) | Calculate Variable Importance | `forestVariableImportance` (Table) | Permutation importance |
| Importance type | `forestImportanceType` (List: permutation, vimp, minimal_depth, comprehensive) | Importance Measure | `forestVariableImportance` | TODO: Wire to .b.R |
| Forest validation | `performForestValidation` (Bool) | Forest Cross-Validation | `forestModelPerformance` OOB metrics | OOB + CV validation |
| Prediction time points | `forestPredictionTimePoints` (String, default "12, 24, 36, 60") | Prediction Time Points (months) | `forestSurvivalPredictions` | Stage-specific predictions |
| Survival predictions | `generateSurvivalPredictions` (Bool) | Generate Survival Predictions | `forestSurvivalPredictions` (Table) | Patient-level risk |
| Forest discrimination | `forestDiscriminationMetrics` (Bool) | Forest Discrimination Analysis | `forestCoxComparison` (Table) | C-index, IBS, AUC |
| Forest staging comparison | `forestStagingComparison` (Bool) | Forest-Based Staging Comparison | `forestStagingComparisonTable` (Table) | TODO: Wire to .b.R |
| Forest bootstrap | `forestBootstrap` (Bool) | Bootstrap Forest Validation | -- | TODO: Wire to .b.R |
| Forest bootstrap samples | `forestBootstrapSamples` (Number, 50-500, default 100) | Bootstrap Samples | -- | TODO: Wire to .b.R |
| Forest summary | `generateForestSummary` (Bool) | Forest Analysis Summary | `forestAnalysisSummary` (Table) | TODO: Wire to .b.R |
| RF analyze old stage | `rfAnalyzeOldStage` (Bool, default true) | Analyze Old Staging System (RF) | -- | Include old stage in RF |
| RF analyze new stage | `rfAnalyzeNewStage` (Bool, default true) | Analyze New Staging System (RF) | -- | Include new stage in RF |
| RF mtry auto | `rfMtryAuto` (Bool, default true) | Auto-select mtry (RF) | -- | Auto mtry selection |
| RF bootstrap type | `rfBootstrapType` (List: by.root, by.node) | Bootstrap Type (RF) | -- | RSF sampling strategy |
| RF sampling type | `rfSamplingType` (List: swr, swor) | Sampling Type (RF) | -- | With/without replacement |
| RF minimal depth | `rfMinimalDepth` (Bool) | Minimal Depth Analysis (RF) | -- | Variable selection via depth |

### Advanced [Experimental]

#### Multi-State Models

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Multi-state analysis | `performMultiStateAnalysis` (Bool) | Multi-State Survival Models | `stateOccupancy` (Table), `multiStateComparison` (Table) | Multi-state model fitting |
| Multi-state model type | `multiStateModel` (List: illness_death, progression, relapse_death, custom, comprehensive) | Multi-State Model Type | -- | TODO: Wire to .b.R |
| State variable | `stateVariable` (Variable) | Disease State Variable | -- | Patient state indicator |
| Transition time variable | `transitionTimeVariable` (Variable) | Transition Time Variable | -- | Time of state changes |
| State definitions | `multiStateStates` (String, default "healthy, disease, death") | State Definitions | -- | State enumeration |
| Absorption states | `absorptionStates` (String, default "death") | Absorption States | -- | Terminal states |
| Multi-state covariates | `multiStateCovariates` (Variables) | Multi-State Covariates | -- | Adjusted multi-state models |
| Transition probabilities | `calculateTransitionProbabilities` (Bool) | Calculate Transition Probabilities | `transitionProbabilities` (Table) | State-pair probabilities |
| Multi-state time points | `multiStateTimePoints` (String, default "6, 12, 24, 36, 60") | Multi-State Time Points (months) | `transitionProbabilities` | Prediction timepoints |
| Transition matrix | `generateTransitionMatrix` (Bool) | Generate Transition Matrix | `transitionIntensities` (Table) | Hazard rate matrix |
| Multi-state graphics | `multiStateGraphics` (Bool) | Multi-State Visualizations | -- | TODO: Wire to .b.R |
| Stratify by staging | `msStratifyByStaging` (Bool) | Stratify by Staging System | -- | TODO: Wire to .b.R |
| Multi-state validation | `multiStateValidation` (Bool) | Multi-State Model Validation | -- | TODO: Wire to .b.R |
| MSM summary | `generateMSMSummary` (Bool) | Multi-State Summary Table | `multiStateSummary` (Table) | Summary with recommendations |

#### Frailty Models

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Frailty models | `performFrailtyModelsAnalysis` (Bool) | Frailty Models Analysis | -- | `coxme::coxme()` mixed-effects Cox |
| Cluster variable | `frailtyClusterVariable` (Variable) | Cluster Variable | -- | Institution/center random effect |
| Frailty distribution | `frailtyDistribution` (List: gamma, gaussian, log-normal) | Frailty Distribution | -- | TODO: Wire to .b.R |
| Frailty bootstrap | `frailtyBootstrap` (Bool) | Bootstrap Validation | -- | Parameter stability |
| Frailty bootstrap samples | `frailtyBootstrapSamples` (Integer, 100-2000, default 500) | Bootstrap Samples | -- | Replicate count |
| Variance components | `frailtyVarianceComponents` (Bool) | Variance Components Analysis | -- | TODO: Wire to .b.R |
| Heterogeneity test | `frailtyHeterogeneityTest` (Bool) | Heterogeneity Testing | -- | TODO: Wire to .b.R |
| Cluster comparison | `frailtyClusterComparison` (Bool) | Cluster-Specific Analysis | -- | Per-cluster staging evaluation |
| Frailty model selection | `frailtyModelSelection` (Bool) | Model Selection | -- | TODO: Wire to .b.R |
| Frailty predictive accuracy | `frailtyPredictiveAccuracy` (Bool) | Predictive Accuracy Assessment | -- | TODO: Wire to .b.R |
| Frailty diagnostics | `frailtyDiagnostics` (Bool) | Model Diagnostics | -- | Residuals, influence |
| Frailty advanced inference | `frailtyAdvancedInference` (Bool) | Advanced Statistical Inference | -- | TODO: Wire to .b.R |

#### Win Ratio

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Win ratio analysis | `performWinRatioAnalysis` (Bool) | Win Ratio Analysis | -- | Composite endpoint comparison |
| Endpoint hierarchy | `winRatioEndpoints` (List: death_progression_response, death_recurrence_remission, death_hospitalization_qol, custom) | Endpoint Hierarchy | -- | TODO: Wire to .b.R |
| Death variable | `winRatioDeathVariable` (Variable) | Death/Primary Endpoint Variable | -- | TODO: Wire to .b.R |
| Secondary endpoint | `winRatioSecondaryEndpoint` (Variable) | Secondary Endpoint Variable | -- | Tier-2 comparison |
| Secondary direction | `wrSecondaryDirection` (List: higher, lower) | Secondary Endpoint Direction | -- | Improvement direction |
| Tertiary endpoint | `winRatioTertiaryEndpoint` (Variable) | Tertiary Endpoint Variable | -- | TODO: Wire to .b.R |
| Time variables | `winRatioTimeVariables` (Variables) | Time Variables for Endpoints | -- | TODO: Wire to .b.R |
| Matching strategy | `winRatioMatchingStrategy` (List: all_pairs, matched_pairs, stratified, propensity_matched) | Matching Strategy | -- | TODO: Wire to .b.R |
| CI method | `winRatioConfidenceMethod` (List: bootstrap, asymptotic, permutation) | Confidence Interval Method | -- | TODO: Wire to .b.R |
| Bootstrap samples | `winRatioBootstrapSamples` (Integer, 100-5000, default 1000) | Bootstrap Samples | -- | TODO: Wire to .b.R |
| Confidence level | `winRatioConfidenceLevel` (Number, 0.8-0.99, default 0.95) | Confidence Level | -- | TODO: Wire to .b.R |
| Tie handling | `winRatioHandleTies` (List: split, ignore, next_endpoint) | Tie Handling Strategy | -- | TODO: Wire to .b.R |
| Sensitivity analysis | `winRatioSensitivityAnalysis` (Bool) | Sensitivity Analysis | -- | Endpoint ordering robustness |
| Generalized pairwise | `winRatioGeneralizedPairwise` (Bool) | Generalized Pairwise Comparison | -- | TODO: Wire to .b.R |

#### SHAP Interpretability

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| SHAP analysis | `performSHAPAnalysis` (Bool) | SHAP Model Interpretability | `shapGlobalImportance` (Table), `shapSummaryStats` (Table) | SHAP value computation |
| SHAP analysis type | `shapAnalysisType` (List: global, individual, comprehensive) | SHAP Analysis Type | `shapGlobalImportance`, `shapIndividualExplanations` | Scope of SHAP output |
| SHAP covariates | `shapCovariates` (Variables) | Variables for SHAP Analysis | `shapGlobalImportance` | Additional features |
| SHAP sample size | `shapSampleSize` (Number, 50-1000, default 100) | SHAP Sample Size | -- | Patient subsample |
| SHAP background samples | `shapBackgroundSamples` (Number, 10-500, default 50) | SHAP Background Samples | -- | Baseline samples |
| SHAP explanation type | `shapExplanationType` (List: tree, kernel, linear, auto) | SHAP Explanation Method | -- | Algorithm selection |
| SHAP plots | `generateSHAPPlots` (Bool) | Generate SHAP Plots | -- | TODO: Wire to .b.R |
| Patient profiles | `shapPatientProfiles` (List: high_risk, low_risk, representative, all) | Patient Profile Examples | `shapIndividualExplanations` (Table) | Profile selection |
| Interaction analysis | `shapInteractionAnalysis` (Bool) | SHAP Interaction Analysis | `shapInteractions` (Table) | Feature interaction SHAP |
| Clinical thresholds | `shapClinicalThresholds` (String, default "0.25, 0.50, 0.75") | Clinical Risk Thresholds | -- | Decision boundaries |

#### Cure Models

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Cure model analysis | `performCureModelAnalysis` (Bool) | Cure Models Analysis | `cureFractionEstimates` (Table), `cureModelParameters` (Table) | Mixture/promotion cure models |
| Cure model type | `cureModelType` (List: mixture, promotion, both) | Cure Model Type | `cureModelParameters` | Model specification |
| Cure distribution | `cureDistribution` (List: weibull, exponential, lognormal, loglogistic) | Survival Distribution | `cureModelParameters` | Susceptible distribution |
| Analyze old staging | `cureAnalyzeOldStage` (Bool) | Analyze Original Staging | `cureFractionEstimates` | Per-system cure model |
| Analyze new staging | `cureAnalyzeNewStage` (Bool) | Analyze New Staging | `cureFractionEstimates` | Per-system cure model |
| Cure fraction method | `cureFractionEstimation` (List: parametric, nonparametric, both) | Cure Fraction Method | -- | TODO: Wire to .b.R |
| Cure confidence level | `cureConfidenceLevel` (Number, 0.80-0.99, default 0.95) | Confidence Level | -- | TODO: Wire to .b.R |
| Cure bootstrap CI | `cureBootstrapCI` (Bool) | Bootstrap Confidence Intervals | `cureModelBootstrap` (Table) | Bootstrap CIs for cure fractions |
| Cure bootstrap reps | `cureBootstrapReps` (Number, 100-2000, default 500) | Bootstrap Replications | `cureModelBootstrap` | Replication count |
| Cure time horizon | `cureTimeHorizon` (Number, 12-240, default 120) | Cure Assessment Time (months) | -- | Cure assessment window |
| Plateau threshold | `curePlateauThreshold` (Number, 0.01-0.10, default 0.05) | Plateau Detection Threshold | -- | KM plateau sensitivity |
| Cure covariates | `cureCovariates` (Variables) | Cure Model Covariates | -- | Adjusted cure models |
| Cure model comparison | `cureModelComparison` (Bool) | Model Comparison Analysis | `cureModelComparisonTable` (Table) | TODO: Wire to .b.R |
| Stage-specific cure | `cureStageSpecificAnalysis` (Bool) | Stage-Specific Cure Analysis | `stageSpecificCureAnalysis` (Table) | Per-stage cure patterns |
| Cure GoF | `cureGoodnessOfFit` (Bool) | Goodness-of-Fit Testing | -- | TODO: Wire to .b.R |
| Cure summary | `generateCureSummary` (Bool) | Cure Model Summary | `cureAnalysisSummary` (Table) | TODO: Wire to .b.R |

#### Interval Censoring

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Interval censoring | `performIntervalCensoringAnalysis` (Bool) | Interval Censoring Analysis | `intervalCensoringOverview`, `intervalCensoringNonparametric`, `intervalCensoringParametric`, `intervalCensoringComparison`, `intervalCensoringDiagnosticsTable`, `intervalCensoringSummary` | `icenReg` package |
| Left interval time | `intervalCensoringLeftTime` (Variable) | Left Interval Time Variable | -- | Left endpoint |
| Right interval time | `intervalCensoringRightTime` (Variable) | Right Interval Time Variable | -- | Right endpoint |
| IC distribution | `intervalCensoringDistribution` (List: weibull, loglogistic, lognormal, exponential, gamma) | Distribution for Parametric Model | `intervalCensoringParametric` | AFT distribution |
| IC model type | `intervalCensoringModel` (List: nonparametric, parametric, both) | Model Type | -- | NPMLE or parametric |
| IC bootstrap | `intervalCensoringBootstrap` (Bool) | Bootstrap Confidence Intervals | `intervalCensoringNonparametric` | CI for NPMLE |
| IC bootstrap samples | `intervalCensoringBootstrapSamples` (Integer, 100-10000, default 1000) | Bootstrap Samples | -- | Replicates |
| IC compare stages | `intervalCensoringCompareStages` (Bool) | Compare Staging Systems | `intervalCensoringComparison` | LR test, AIC/BIC |
| IC plots | `intervalCensoringPlots` (Bool) | Generate Survival Plots | -- | TODO: Wire to .b.R |
| IC diagnostics | `intervalCensoringDiagnostics` (Bool) | Model Diagnostics | `intervalCensoringDiagnosticsTable` | Convergence, residuals |
| IC prediction times | `intervalCensoringPredictionTime` (String, default "12, 24, 36, 60") | Prediction Time Points | `intervalCensoringNonparametric` | Prediction grid |
| IC confidence level | `intervalCensoringConfidenceLevel` (Number, 0.8-0.99, default 0.95) | Confidence Level | -- | CI level |
| IC adjust variables | `intervalCensoringAdjustVariables` (Variables) | Adjustment Variables | `intervalCensoringParametric` | Covariates in AFT model |

#### Informative Censoring

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Informative censoring detection | `performInformativeCensoringAnalysis` (Bool) | Informative Censoring Detection | `informativeCensoringOverview`, `informativeCensoringTests`, `informativeCensoringByStage`, `informativeCensoringAdjustment`, `informativeCensoringSensitivity`, `informativeCensoringDiagnostics`, `informativeCensoringSummary` | Multi-test detection suite |
| Test method | `informativeCensoringTestMethod` (List: all_tests, correlation_test, regression_test, competing_risks, landmark_analysis) | Test Method | `informativeCensoringTests` | Test selection |
| Censoring covariates | `informativeCensoringCovariates` (Variables) | Censoring Mechanism Covariates | -- | TODO: Wire to .b.R |
| Landmark times | `informativeCensoringLandmarkTimes` (String, default "12, 24, 36, 60") | Landmark Time Points | -- | Landmark analysis grid |
| Adjustment method | `informativeCensoringAdjustmentMethod` (List: none, ipw, multiple_imputation, sensitivity_analysis) | Adjustment Method | `informativeCensoringAdjustment` | Bias correction |
| IPW variables | `informativeCensoringIPWVariables` (Variables) | IPW Model Variables | -- | TODO: Wire to .b.R |
| Sensitivity range | `informativeCensoringSensitivityRange` (String, default "0.8, 0.9, 1.0, 1.1, 1.2") | Sensitivity Analysis Range | `informativeCensoringSensitivity` | HR multipliers |
| IC bootstrap | `informativeCensoringBootstrap` (Bool) | Bootstrap Confidence Intervals | -- | CI for adjusted estimates |
| IC bootstrap samples | `informativeCensoringBootstrapSamples` (Integer, 100-5000, default 1000) | Bootstrap Samples | -- | Replicates |
| IC alpha | `informativeCensoringAlpha` (Number, 0.01-0.10, default 0.05) | Significance Level | `informativeCensoringTests` | Test significance |
| IC plots | `informativeCensoringPlots` (Bool) | Generate Diagnostic Plots | -- | TODO: Wire to .b.R |
| IC compare stages | `informativeCensoringCompareStages` (Bool) | Compare Censoring Across Stages | `informativeCensoringByStage` | Stage-specific censoring |

#### Concordance Probability

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Concordance probability | `performConcordanceProbabilityAnalysis` (Bool) | Concordance Probability Analysis | `concordanceProbabilityOverview`, `concordanceProbabilityEstimates`, `concordanceProbabilityTimeDependentComplex`, `concordanceProbabilityComparison` | Multi-method concordance |
| Methods | `concordanceProbabilityMethods` (List: all_methods, harrell_c, uno_c, time_dependent, ipcw_concordance, weighted_concordance) | Concordance Methods | `concordanceProbabilityEstimates` | Method selection |
| Time points | `concordanceProbabilityTimePoints` (String, default "12, 24, 36, 60, 120") | Time Points for Assessment | `concordanceProbabilityTimeDependentComplex` | Assessment grid |
| Weighting | `concordanceProbabilityWeighting` (List: uniform, sample_size, event_rate, follow_up, inverse_variance) | Weighting Strategy | -- | Weight specification |
| CP bootstrap | `concordanceProbabilityBootstrap` (Bool) | Bootstrap Confidence Intervals | `concordanceProbabilityEstimates` | CI for concordance |
| CP bootstrap samples | `concordanceProbabilityBootstrapSamples` (Integer, 100-5000, default 1000) | Bootstrap Samples | -- | TODO: Wire to .b.R |
| CP confidence level | `concordanceProbabilityConfidenceLevel` (Number, 0.8-0.99, default 0.95) | Confidence Level | -- | CI level |
| CP compare stages | `concordanceProbabilityCompareStages` (Bool) | Compare Staging Systems | `concordanceProbabilityComparison` | System comparison |
| CP adjust variables | `concordanceProbabilityAdjustVariables` (Variables) | Adjustment Variables | -- | TODO: Wire to .b.R |
| CP robustness | `concordanceProbabilityRobustnessAnalysis` (Bool) | Robustness Analysis | -- | Sensitivity to assumptions |
| CP alpha | `concordanceProbabilityAlpha` (Number, 0.01-0.10, default 0.05) | Significance Level | -- | Test significance |
| CP diagnostics | `concordanceProbabilityDiagnostics` (Bool) | Diagnostic Assessment | -- | Convergence, influence |

#### Optimal Cut-point Determination

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Optimal cut-point | `performOptimalCutpoint` (Bool) | Optimal Cut-point Determination | `optimalCutpointAnalysis` (Table) | `stagemigration_cutpointAnalysis()` |
| Continuous variable | `continuousStageVariable` (Variable, numeric) | Continuous Variable for Cut-point Analysis | `optimalCutpointAnalysis` | Biomarker/measurement |
| Cut-point method | `cutpointMethod` (List: maxstat, minpvalue, surv_cutpoint, comprehensive) | Cut-point Method | `optimalCutpointAnalysis` | Statistical method |
| Search range | `cutpointRange` (String, default "0.1, 0.9") | Cut-point Search Range | -- | Proportion bounds |
| Multiple testing | `multipleTestingCorrection` (List: bonferroni, BH, holm, none) | Multiple Testing Correction | `optimalCutpointAnalysis` | p-value adjustment |
| Validate cut-point | `validateCutpoint` (Bool) | Cross-validate Cut-point | `cutpointValidation` (Table) | CV stability |
| Cut-point bootstrap | `cutpointBootstrap` (Bool) | Bootstrap Cut-point Validation | `cutpointValidation` (Table) | Bootstrap CI |
| Cut-point bootstrap reps | `cutpointBootstrapReps` (Number, 100-1000, default 500) | Cut-point Bootstrap Repetitions | -- | Replicates |
| Generate staging system | `generateStagingSystem` (Bool) | Generate New Staging System | `generatedStagingSystem` (Table) | Auto-create categories |
| Staging levels | `stagingSystemLevels` (Number, 2-6, default 3) | Number of Staging Levels | `generatedStagingSystem` | Category count |

### Display Options

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Explanations | `showExplanations` (Bool, default true) | Explanations for Results | 14 Html explanation items (gated by visibility) | `.setExplanationContent()` |
| Abbreviation glossary | `showAbbreviationGlossary` (Bool) | Show Abbreviation Glossary | `abbreviationGlossary` (Html) | Static glossary content |
| Welcome message | -- | -- | `welcomeMessage` (Html) | Shown when core variables missing |
| Debug views | -- | -- | `mydataview`, `mydataview2` (Preformatted, hidden) | Debug output |

### Visualization (10 Plot Options)

| Plot | YAML Argument (.a.yaml) | Results Section (.r.yaml) | renderFun (.b.R) |
|------|------------------------|--------------------------|-----------------|
| Migration heatmap | `showMigrationHeatmap` | `migrationHeatmap` (Image, 700x500) | `.plotMigrationHeatmap` |
| Sankey flow diagram | `showSankeyDiagram` | `sankeyDiagram` (Image, 900x600) | `.plotSankeyDiagram` |
| ROC comparison | `showROCComparison` | `rocComparisonPlot` (Image, 800x600) | `.plotROCComparison` |
| Forest plot (HR) | `showForestPlot` | `forestPlot` (Image, 700x600) | `.plotForest` |
| Calibration plots | `showCalibrationPlots` | `calibrationPlots` (Image, 800x400) | `.plotCalibration` |
| Decision curves | `showDecisionCurves` | `decisionCurves` (Image, 700x500) | `.plotDecisionCurves` |
| Survival curves | `showSurvivalCurves` | `survivalCurves` (Image, 900x700) | `.plotSurvivalCurves` |
| Will Rogers visualization | `showWillRogersVisualization` | `willRogersVisualization` (Image, 800x600) | `.plotWillRogersEffect` |
| Migration survival comparison | `showMigrationSurvivalComparison` | `migrationSurvivalComparison` (Image, 1400x700) | `.plotMigrationSurvivalComparison` |
| Cross-validation plot | `performCrossValidation` | `crossValidationPlot` (Image, 600x400) | `.plotCrossValidation` |

### Multifactorial Analysis

| Feature | YAML Argument (.a.yaml) | UI Label | Results Section (.r.yaml) | R Method (.b.R) |
|---------|------------------------|----------|--------------------------|-----------------|
| Enable multifactorial | `enableMultifactorialAnalysis` (Bool) | Enable Multifactorial Analysis | -- | Adjusted model comparisons |
| Continuous covariates | `continuousCovariates` (Variables, numeric) | Continuous Covariates | `multifactorialResults` | Age, tumor size, etc. |
| Categorical covariates | `categoricalCovariates` (Variables, factor) | Categorical Covariates | `multifactorialResults` | Sex, histology, etc. |
| Comparison type | `multifactorialComparisonType` (List: adjusted_cindex, nested_models, stepwise, comprehensive) | Multifactorial Comparison Type | -- | Analysis scope |
| Baseline model | `baselineModel` (List: covariates_only, original_plus_covariates, new_plus_covariates) | Baseline Model | -- | Reference model |
| Interaction tests | `performInteractionTests` (Bool) | Test Stage-Covariate Interactions | `interactionTests` (Table) | Stage x covariate interactions |
| Stratified analysis | `stratifiedAnalysis` (Bool) | Stratified Analysis | `stratifiedAnalysisTable` (Table) | Per-subgroup evaluation |
| Multifactorial tables | `showMultifactorialTables` (Bool) | Show Multifactorial Tables | `multifactorialResults` (Table) | Display gate |
| Adjusted C-index | `showAdjustedCIndexComparison` (Bool) | Show Adjusted C-index Comparison | `adjustedCIndexComparison` (Table) | Covariate-adjusted C |
| Nested model tests | `showNestedModelTests` (Bool) | Show Nested Model Tests | `nestedModelTests` (Table) | LR tests for nesting |
| Stepwise results | `showStepwiseResults` (Bool) | Show Stepwise Selection Results | `stepwiseResults` (Table) | AIC-based selection |
