# Advanced TNM Stage Migration Analysis

Comprehensive analysis for validating TNM staging system improvements
using state-of-the-art statistical methods. This analysis provides
pathologists with robust tools to evaluate whether a new staging system
provides superior prognostic discrimination compared to existing
systems. Includes advanced metrics: Net Reclassification Improvement
(NRI), Integrated Discrimination Improvement (IDI), time-dependent ROC
analysis, decision curve analysis, bootstrap validation, and
comprehensive clinical interpretation guidance.

## Usage

``` r
stagemigration(
  data,
  oldStage = NULL,
  newStage = NULL,
  survivalTime = NULL,
  event = NULL,
  eventLevel,
  clinicalPreset = "routine_clinical",
  enableGuidedMode = FALSE,
  generateCopyReadyReport = FALSE,
  enableAccessibilityFeatures = FALSE,
  preferredLanguage = "en",
  enableProgressIndicators = FALSE,
  optimizeForLargeDatasets = FALSE,
  complexityMode = "quick",
  analysisType = "comprehensive",
  confidenceLevel = 0.95,
  calculateNRI = FALSE,
  nriTimePoints = "12, 24, 60",
  calculateIDI = FALSE,
  performROCAnalysis = FALSE,
  rocTimePoints = "12, 24, 36, 60",
  performDCA = FALSE,
  performCalibration = FALSE,
  performBootstrap = FALSE,
  bootstrapReps = 1000,
  performCrossValidation = FALSE,
  cvFolds = 5,
  institutionVariable = NULL,
  clinicalSignificanceThreshold = 0.02,
  nriClinicalThreshold = 0.2,
  performHomogeneityTests = FALSE,
  performTrendTests = FALSE,
  performLikelihoodTests = FALSE,
  calculatePseudoR2 = FALSE,
  showMigrationOverview = TRUE,
  showMigrationSummary = FALSE,
  showStageDistribution = FALSE,
  showMigrationMatrix = TRUE,
  showStatisticalComparison = FALSE,
  showConcordanceComparison = FALSE,
  showMigrationHeatmap = FALSE,
  showSankeyDiagram = FALSE,
  showROCComparison = FALSE,
  showCalibrationPlots = FALSE,
  showDecisionCurves = FALSE,
  showForestPlot = FALSE,
  showWillRogersAnalysis = FALSE,
  showWillRogersVisualization = FALSE,
  showMigrationSurvivalComparison = FALSE,
  showSurvivalCurves = FALSE,
  survivalPlotType = "separate",
  showConfidenceIntervals = FALSE,
  showRiskTables = FALSE,
  plotTimeRange = "auto",
  showClinicalInterpretation = FALSE,
  showStatisticalSummary = FALSE,
  showMethodologyNotes = FALSE,
  includeEffectSizes = FALSE,
  advancedMigrationAnalysis = FALSE,
  generateExecutiveSummary = FALSE,
  cancerType = "general",
  useOptimismCorrection = FALSE,
  enableMultifactorialAnalysis = FALSE,
  continuousCovariates = NULL,
  categoricalCovariates = NULL,
  multifactorialComparisonType = "comprehensive",
  baselineModel = "covariates_only",
  performInteractionTests = FALSE,
  stratifiedAnalysis = FALSE,
  showMultifactorialTables = FALSE,
  showAdjustedCIndexComparison = FALSE,
  showNestedModelTests = FALSE,
  showStepwiseResults = FALSE,
  showExplanations = TRUE,
  showAbbreviationGlossary = FALSE,
  calculateSME = FALSE,
  calculateRMST = FALSE,
  performCompetingRisks = FALSE,
  competingEventVar = NULL,
  performOptimalCutpoint = FALSE,
  continuousStageVariable = NULL,
  cutpointMethod = "maxstat",
  cutpointRange = "0.1, 0.9",
  multipleTestingCorrection = "bonferroni",
  validateCutpoint = FALSE,
  cutpointBootstrap = FALSE,
  cutpointBootstrapReps = 500,
  generateStagingSystem = FALSE,
  stagingSystemLevels = 3,
  performSHAPAnalysis = FALSE,
  shapAnalysisType = "comprehensive",
  shapCovariates = NULL,
  shapSampleSize = 100,
  shapBackgroundSamples = 50,
  shapExplanationType = "auto",
  generateSHAPPlots = FALSE,
  shapPatientProfiles = "representative",
  shapInteractionAnalysis = FALSE,
  shapClinicalThresholds = "0.25, 0.50, 0.75",
  performCompetingRisksAdvanced = FALSE,
  competingRisksMethod = "comprehensive",
  cifTimePoints = "12, 24, 36, 60",
  competingEventLevels = "cancer_death, other_death, censored",
  primaryEventLevel = "cancer_death",
  generateCIFPlots = FALSE,
  performGrayTest = FALSE,
  cifConfidenceLevel = 0.95,
  competingRisksCovariates = NULL,
  stratifyByStaging = FALSE,
  calculateCRCIndex = FALSE,
  generateCRSummary = FALSE,
  performMultiStateAnalysis = FALSE,
  multiStateModel = "illness_death",
  stateVariable = NULL,
  transitionTimeVariable = NULL,
  multiStateStates = "healthy, disease, death",
  absorptionStates = "death",
  multiStateCovariates = NULL,
  calculateTransitionProbabilities = FALSE,
  multiStateTimePoints = "6, 12, 24, 36, 60",
  generateTransitionMatrix = FALSE,
  multiStateGraphics = FALSE,
  msStratifyByStaging = FALSE,
  multiStateValidation = FALSE,
  generateMSMSummary = FALSE,
  performRandomForestAnalysis = FALSE,
  forestModelType = "rsf",
  forestNTrees = 500,
  forestMTry = "auto",
  forestMinNodeSize = 3,
  forestCovariates = NULL,
  calculateVariableImportance = FALSE,
  forestImportanceType = "permutation",
  performForestValidation = FALSE,
  forestPredictionTimePoints = "12, 24, 36, 60",
  generateSurvivalPredictions = FALSE,
  forestDiscriminationMetrics = FALSE,
  forestStagingComparison = FALSE,
  forestBootstrap = FALSE,
  forestBootstrapSamples = 100,
  generateForestSummary = FALSE,
  rfAnalyzeOldStage = TRUE,
  rfAnalyzeNewStage = TRUE,
  rfMtryAuto = TRUE,
  rfBootstrapType = "by.root",
  rfSamplingType = "swr",
  rfMinimalDepth = FALSE,
  performCureModelAnalysis = FALSE,
  cureModelType = "mixture",
  cureDistribution = "weibull",
  cureAnalyzeOldStage = FALSE,
  cureAnalyzeNewStage = FALSE,
  cureFractionEstimation = "parametric",
  cureConfidenceLevel = 0.95,
  cureBootstrapCI = FALSE,
  cureBootstrapReps = 500,
  cureTimeHorizon = 120,
  curePlateauThreshold = 0.05,
  cureCovariates = NULL,
  cureModelComparison = FALSE,
  cureStageSpecificAnalysis = FALSE,
  cureGoodnessOfFit = FALSE,
  generateCureSummary = FALSE,
  performIntervalCensoringAnalysis = FALSE,
  intervalCensoringLeftTime = NULL,
  intervalCensoringRightTime = NULL,
  intervalCensoringDistribution = "weibull",
  intervalCensoringModel = "both",
  intervalCensoringBootstrap = FALSE,
  intervalCensoringBootstrapSamples = 1000,
  intervalCensoringCompareStages = FALSE,
  intervalCensoringPlots = FALSE,
  intervalCensoringDiagnostics = FALSE,
  intervalCensoringPredictionTime = "12, 24, 36, 60",
  intervalCensoringConfidenceLevel = 0.95,
  intervalCensoringAdjustVariables = NULL,
  performInformativeCensoringAnalysis = FALSE,
  informativeCensoringTestMethod = "all_tests",
  informativeCensoringCovariates = NULL,
  informativeCensoringLandmarkTimes = "12, 24, 36, 60",
  informativeCensoringAdjustmentMethod = "sensitivity_analysis",
  informativeCensoringIPWVariables = NULL,
  informativeCensoringSensitivityRange = "0.8, 0.9, 1.0, 1.1, 1.2",
  informativeCensoringBootstrap = FALSE,
  informativeCensoringBootstrapSamples = 1000,
  informativeCensoringAlpha = 0.05,
  informativeCensoringPlots = FALSE,
  informativeCensoringCompareStages = FALSE,
  performConcordanceProbabilityAnalysis = FALSE,
  concordanceProbabilityMethods = "all_methods",
  concordanceProbabilityTimePoints = "12, 24, 36, 60, 120",
  concordanceProbabilityWeighting = "uniform",
  concordanceProbabilityBootstrap = FALSE,
  concordanceProbabilityBootstrapSamples = 1000,
  concordanceProbabilityConfidenceLevel = 0.95,
  concordanceProbabilityCompareStages = FALSE,
  concordanceProbabilityAdjustVariables = NULL,
  concordanceProbabilityRobustnessAnalysis = FALSE,
  concordanceProbabilityAlpha = 0.05,
  concordanceProbabilityDiagnostics = FALSE,
  performWinRatioAnalysis = FALSE,
  winRatioEndpoints = "death_progression_response",
  winRatioDeathVariable = NULL,
  winRatioSecondaryEndpoint = NULL,
  wrSecondaryDirection = "higher",
  winRatioTertiaryEndpoint = NULL,
  winRatioTimeVariables = NULL,
  winRatioMatchingStrategy = "all_pairs",
  winRatioConfidenceMethod = "bootstrap",
  winRatioBootstrapSamples = 1000,
  winRatioConfidenceLevel = 0.95,
  winRatioHandleTies = "next_endpoint",
  winRatioSensitivityAnalysis = FALSE,
  winRatioGeneralizedPairwise = FALSE,
  performFrailtyModelsAnalysis = FALSE,
  frailtyClusterVariable = NULL,
  frailtyDistribution = "gamma",
  frailtyBootstrap = FALSE,
  frailtyBootstrapSamples = 500,
  frailtyVarianceComponents = FALSE,
  frailtyHeterogeneityTest = FALSE,
  frailtyClusterComparison = FALSE,
  frailtyModelSelection = FALSE,
  frailtyPredictiveAccuracy = FALSE,
  frailtyDiagnostics = FALSE,
  frailtyAdvancedInference = FALSE,
  performClinicalUtilityAnalysis = FALSE,
  clinicalUtilityPrevalence = 0.2,
  clinicalUtilityTimePoint = 60,
  clinicalUtilityThresholds = "standard",
  clinicalUtilityNNT = FALSE,
  clinicalUtilityTreatmentEffect = 0.7,
  clinicalUtilityComparison = FALSE,
  clinicalUtilityCostEffectiveness = FALSE,
  clinicalUtilityCostPerIntervention = 5000,
  clinicalUtilityBootstrap = FALSE,
  clinicalUtilityBootstrapSamples = 500,
  clinicalUtilityTimeVarying = FALSE
)
```

## Arguments

- data:

  The dataset containing staging and survival information for TNM
  validation analysis.

- oldStage:

  CLINICAL EXAMPLE: Select your current staging variable such as
  'TNM7_Stage' containing values like: Stage I, Stage IIA, Stage IIB,
  Stage IIIA, Stage IIIB, Stage IV. TECHNICAL DETAILS: The original
  staging variable (e.g., TNM 7th edition, AJCC 7th edition). Should be
  coded as ordered factor with appropriate stage levels for meaningful
  comparison.

- newStage:

  CLINICAL EXAMPLE: Select your new staging variable such as
  'TNM8_Stage' containing the same patients but potentially different
  stage assignments based on revised criteria (e.g., T descriptor
  changes, nodal assessment updates). TECHNICAL DETAILS: The proposed
  new staging variable (e.g., TNM 8th edition, revised staging). Should
  use the same coding structure as the original staging system for valid
  comparison.

- survivalTime:

  CLINICAL EXAMPLE: Select your follow-up time variable such as
  'OS_months' containing values like: 12.5, 24.8, 36.2, 45.0 (months
  from diagnosis to death or last contact). TECHNICAL DETAILS: Time to
  event or censoring in consistent units (months recommended). For
  overall survival analysis, use time from diagnosis to death or last
  follow-up. For disease-free survival, use time from treatment to
  recurrence or last follow-up.

- event:

  CLINICAL EXAMPLE: Select your event variable such as 'Death_Status'
  containing values like: 0 (alive/censored), 1 (dead/event occurred) OR
  "Alive", "Dead" OR "No Event", "Death", "Disease Progression".
  TECHNICAL DETAILS: Event indicator (1 = event occurred, 0 = censored)
  or factor with event levels. For overall survival, event = death from
  any cause. For disease-specific survival, event = death from the
  specific disease being studied.

- eventLevel:

  The level indicating event occurrence when using factor variables.

- clinicalPreset:

  Choose a preset tailored to your clinical workflow. 'Routine Clinical'
  provides essential validation metrics for daily practice. 'Research
  Study' adds advanced statistics for academic research. 'Publication
  Ready' includes all methods and visualizations for manuscripts. Choose
  'Custom' to manually configure all options.

- enableGuidedMode:

  Enable step-by-step guidance for clinical users. Provides contextual
  help, assumption checking, and clinical interpretation assistance
  throughout the analysis. Highly recommended for users new to staging
  validation methods.

- generateCopyReadyReport:

  Generate plain-language clinical summary paragraphs that can be copied
  directly into reports or manuscripts. Includes key findings,
  statistical significance, clinical interpretation, and implementation
  recommendations.

- enableAccessibilityFeatures:

  Enable accessibility features including color-blind safe palettes,
  high contrast visualizations, larger font sizes, and enhanced table
  readability. Ensures outputs are accessible to users with visual
  impairments.

- preferredLanguage:

  Choose the language for explanations, labels, and clinical
  interpretations. Scientific terminology and statistical results remain
  standardized.

- enableProgressIndicators:

  Show progress bars and status updates for long-running analyses such
  as bootstrap validation and cross-validation. Helps users track
  analysis progress.

- optimizeForLargeDatasets:

  Enable memory-efficient processing for large datasets (\>10,000
  patients). Uses chunked processing and optimized algorithms to reduce
  memory usage and improve performance while maintaining statistical
  accuracy.

- complexityMode:

  Controls UI complexity and feature availability. Quick mode shows
  essential outputs only (migration matrix, C-index, simple
  recommendation). Standard adds common validation metrics (NRI,
  survival curves, Will Rogers). Comprehensive enables all methods
  (bootstrap, ROC, DCA). Custom allows manual control of all options.

- analysisType:

  Determines the scope of statistical analysis performed. Comprehensive
  analysis includes all available methods for thorough staging system
  validation.

- confidenceLevel:

  Confidence level for all confidence intervals and hypothesis tests.

- calculateNRI:

  Calculate Net Reclassification Improvement to quantify improvement in
  risk classification between staging systems. Essential for staging
  validation.

- nriTimePoints:

  Comma-separated time points for NRI calculation (e.g., "12, 24, 60"
  for 1, 2, and 5-year survival). Use clinically relevant time points.

- calculateIDI:

  Calculate Integrated Discrimination Improvement to measure improvement
  in risk prediction accuracy between staging systems.

- performROCAnalysis:

  Perform time-dependent ROC analysis to compare discriminative ability
  of staging systems over time.

- rocTimePoints:

  Time points for ROC analysis. Should include clinically important
  survival milestones for the specific cancer type.

- performDCA:

  Perform Decision Curve Analysis to assess clinical utility and net
  benefit of the new staging system for clinical decision making.

- performCalibration:

  Assess calibration of risk predictions from both staging systems.
  Important for validating accuracy of survival predictions.

- performBootstrap:

  Perform bootstrap validation with optimism correction to assess
  internal validity of results. Recommended for all staging validation
  studies.

- bootstrapReps:

  Number of bootstrap repetitions for internal validation. 1000
  repetitions recommended for stable results.

- performCrossValidation:

  Perform k-fold cross-validation for additional validation.
  Computationally intensive but provides robust validation.

- cvFolds:

  Number of folds for cross-validation when enabled.

- institutionVariable:

  Optional variable indicating institution or study center for
  multi-institutional validation. When provided, performs
  internal-external cross-validation using k-1 centers for development
  and remaining center for validation. Essential for multi-center
  staging validation studies.

- clinicalSignificanceThreshold:

  Minimum improvement in C-index considered clinically significant.
  Default 0.02 based on oncology literature recommendations.

- nriClinicalThreshold:

  Minimum NRI improvement considered clinically meaningful. Default 0.20
  (20 percent net reclassification improvement).

- performHomogeneityTests:

  Test homogeneity within stages and monotonic trend across stages.
  Essential for validating stage ordering and grouping.

- performTrendTests:

  Test for monotonic trend in survival across stage levels. Validates
  that higher stages consistently have worse prognosis.

- performLikelihoodTests:

  Perform formal likelihood ratio tests comparing nested staging models.
  Provides statistical significance testing for staging improvement.

- calculatePseudoR2:

  Calculate multiple pseudo R-squared measures for model comparison
  (Nagelkerke, McFadden, Cox-Snell).

- showMigrationOverview:

  Display overview table showing the fundamental migration statistics
  including: total number of patients, number and percentage of patients
  who migrated stages, direction of migration (upstaged vs downstaged),
  and net migration effect. This is the essential first table for
  understanding the overall impact of the new staging system.

- showMigrationSummary:

  Display statistical summary of migration patterns including overall
  migration rate and formal statistical tests. Shows Chi-square test
  results for independence and Fisher's exact test p-values to determine
  if the migration patterns are statistically significant. Essential for
  validating whether observed changes are due to genuine staging
  improvements or random variation.

- showStageDistribution:

  Display side-by-side comparison of how patients are distributed across
  stages in both the original and new staging systems. Shows the count
  and percentage of patients in each stage, along with the net change.
  This helps identify which stages are gaining or losing patients and
  whether the new system creates better separation between prognostic
  groups.

- showMigrationMatrix:

  Display detailed cross-tabulation matrix showing exactly how patients
  moved between stages. Rows represent the original staging system and
  columns represent the new staging system. Diagonal values indicate
  patients who remained in the same stage, while off-diagonal values
  show stage migrations. This is essential for understanding the
  specific migration patterns and identifying which stages are most
  affected by the new criteria.

- showStatisticalComparison:

  Display table with C-index comparisons and other statistical metrics.

- showConcordanceComparison:

  Display detailed concordance comparison between staging systems.

- showMigrationHeatmap:

  Display a color-coded heatmap visualization of the migration matrix.
  Darker colors indicate more patients, with the diagonal showing
  patients who remained in the same stage. This visual representation
  makes it easy to identify migration patterns at a glance - upstaging
  appears above the diagonal, downstaging below. Essential for
  presentations and publications.

- showSankeyDiagram:

  Display a Sankey flow diagram showing patient migration patterns
  between original and new staging systems. Flow thickness represents
  the number of patients moving between stages, making it easy to
  visualize dominant migration patterns. Excellent for presentations and
  understanding the overall reclassification impact.

- showROCComparison:

  Display time-dependent ROC curves comparing staging systems.

- showCalibrationPlots:

  Display calibration plots for both staging systems.

- showDecisionCurves:

  Display decision curves showing net benefit of staging systems.

- showForestPlot:

  Display forest plot with stage-specific hazard ratios and confidence
  intervals.

- showWillRogersAnalysis:

  Detailed analysis of Will Rogers phenomenon with survival comparisons
  between migrated and non-migrated patients within stages.

- showWillRogersVisualization:

  Display visualization showing how stage migration affects survival
  within each stage. Shows before/after survival curves demonstrating
  the Will Rogers paradox where both stages appear to improve.

- showMigrationSurvivalComparison:

  Display Kaplan-Meier survival curves comparing the same stages before
  and after patient migration. Shows how survival curves change when
  patients are reclassified between staging systems, providing visual
  evidence of the Will Rogers phenomenon and staging system
  improvements.

- showSurvivalCurves:

  Display survival curves comparing the staging systems.

- survivalPlotType:

  Controls display of survival curves for staging system comparison.

- showConfidenceIntervals:

  Display confidence intervals around survival curves and other
  estimates.

- showRiskTables:

  Display at-risk tables below survival curves.

- plotTimeRange:

  Maximum time for survival plots. Use "auto" for automatic range or
  specify maximum months (e.g., "60" for 5-year follow-up).

- showClinicalInterpretation:

  Display comprehensive clinical interpretation of all statistical
  results with guidance for staging system adoption decisions.

- showStatisticalSummary:

  Display comprehensive table summarizing all statistical comparisons.

- showMethodologyNotes:

  Display detailed notes on statistical methods used and their
  interpretation.

- includeEffectSizes:

  Calculate and display effect sizes for all comparisons to assess
  practical significance beyond statistical significance.

- advancedMigrationAnalysis:

  Perform comprehensive stage migration analysis including monotonicity
  checks, Will Rogers phenomenon detection, stage-specific validation,
  and enhanced discrimination metrics. Provides detailed assessment of
  staging system quality and migration patterns.

- generateExecutiveSummary:

  Generate executive summary with key findings and recommendations for
  clinical and research stakeholders.

- cancerType:

  Optional cancer type specification for customized thresholds and
  interpretation guidelines based on cancer-specific literature.

- useOptimismCorrection:

  Apply optimism correction to performance metrics using bootstrap
  validation to avoid overly optimistic estimates.

- enableMultifactorialAnalysis:

  Enable advanced multifactorial stage migration analysis that includes
  additional covariates in the comparison. This allows for adjusted
  comparisons between staging systems after accounting for other
  prognostic factors.

- continuousCovariates:

  Continuous variables to include as covariates in the multifactorial
  analysis (e.g., age, tumor size, biomarker levels). These will be
  included in Cox regression models for both staging systems.

- categoricalCovariates:

  Categorical variables to include as covariates in the multifactorial
  analysis (e.g., sex, histology, treatment type). These will be
  included in Cox regression models for both staging systems.

- multifactorialComparisonType:

  Type of multifactorial comparison to perform. Comprehensive includes
  all methods for thorough evaluation of staging systems in the context
  of other prognostic factors.

- baselineModel:

  Baseline model for multifactorial comparisons. This determines the
  reference model against which staging systems are compared.

- performInteractionTests:

  Test for interactions between staging systems and covariates. This
  helps identify if the staging system performance varies across
  different patient subgroups.

- stratifiedAnalysis:

  Perform stratified analysis by categorical covariates to evaluate
  staging system performance within subgroups.

- showMultifactorialTables:

  Display detailed tables showing multifactorial model results,
  including adjusted hazard ratios and model comparison statistics.

- showAdjustedCIndexComparison:

  Display comparison of C-indices for staging systems adjusted for
  covariates. This shows the discriminative ability of each staging
  system after accounting for other prognostic factors.

- showNestedModelTests:

  Display likelihood ratio tests comparing nested models to formally
  test the added value of each staging system over the baseline model.

- showStepwiseResults:

  Display results of stepwise model selection showing which variables
  (including staging systems) are selected in the final model.

- showExplanations:

  Include detailed explanations for results.

- showAbbreviationGlossary:

  Display a comprehensive glossary of all abbreviations, statistical
  terms, and technical terminology used in the stage migration analysis.
  This provides a quick reference for interpreting dashboard values and
  understanding statistical outputs.

- calculateSME:

  Calculate Stage Migration Effect Formula (SME) to quantify the
  cumulative difference in survival between corresponding stages of old
  and new staging systems. SME = Σ(S_new_i - S_old_i) where S represents
  stage-specific survival rates. Positive values indicate Will Rogers
  phenomenon (apparent improvement in new system), while negative values
  suggest understaging.

- calculateRMST:

  Calculate Restricted Mean Survival Time (RMST) metrics for robust
  discrimination assessment. RMST provides clinically interpretable
  survival measures that are independent of proportional hazards
  assumptions. Particularly valuable when median survival is not reached
  or when comparing absolute survival benefits between staging systems.

- performCompetingRisks:

  Perform competing risks analysis for scenarios with multiple event
  types (e.g., cancer-specific death vs. other causes). Implements
  Fine-Gray subdistribution hazard models and Cumulative Incidence
  Function (CIF) analysis. Essential when competing events prevent
  observation of primary outcome and standard survival analysis may be
  biased.

- competingEventVar:

  Optional variable indicating competing events (events other than
  primary outcome). If not specified, the analysis will attempt to
  detect competing risks from multi-level event variables. For cancer
  studies, this typically represents non-cancer deaths when primary
  outcome is cancer-specific death.

- performOptimalCutpoint:

  Determine optimal cut-points for continuous variables that create the
  most statistically significant separation in survival outcomes. Uses
  maximal selected rank statistics with appropriate multiple testing
  corrections. Essential for developing new staging criteria from
  continuous biomarkers or measurements.

- continuousStageVariable:

  Continuous variable (e.g., tumor size, biomarker level, age) for
  optimal cut-point determination. The analysis will find the cut-point
  that maximizes the separation in survival outcomes while controlling
  for multiple testing.

- cutpointMethod:

  Method for optimal cut-point determination. Maximal selected rank
  statistics provides the most rigorous approach with proper multiple
  testing correction.

- cutpointRange:

  Proportion range for cut-point search (e.g., "0.1, 0.9" excludes outer
  10 percent to maintain statistical power). Prevents extreme cut-points
  that create unbalanced groups.

- multipleTestingCorrection:

  Multiple testing correction method for cut-point determination.
  Bonferroni is most conservative; use when testing many cut-points.

- validateCutpoint:

  Perform cross-validation to assess stability of optimal cut-point.
  Helps identify robust cut-points that are not dependent on specific
  data characteristics.

- cutpointBootstrap:

  Use bootstrap validation to assess cut-point stability and derive
  confidence intervals. Provides robust assessment of cut-point
  reliability.

- cutpointBootstrapReps:

  Number of bootstrap repetitions for cut-point validation.

- generateStagingSystem:

  Automatically generate a new staging system based on optimal
  cut-points. Creates categorical staging variable from continuous
  measurements using determined cut-points with appropriate stage
  labeling.

- stagingSystemLevels:

  Number of staging levels to create from optimal cut-points (e.g., 3
  for Low/Intermediate/High or 4 for Stages I-IV).

- performSHAPAnalysis:

  Perform Shapley Additive Explanations (SHAP) analysis to explain which
  factors are driving the predictions of staging models. SHAP provides
  both global feature importance and individual patient-level
  explanations for complex staging decisions.

- shapAnalysisType:

  Type of SHAP analysis to perform. Global analysis shows overall
  feature importance across all patients, individual analysis explains
  specific patient predictions, comprehensive includes both approaches.

- shapCovariates:

  Additional variables to include in SHAP interpretability analysis
  alongside staging variables. Include key clinical variables that might
  influence staging decisions or patient outcomes.

- shapSampleSize:

  Number of patients to use for SHAP analysis. Larger samples provide
  more comprehensive explanations but require more computation time.
  Recommended: 100-200 for routine analysis, 500+ for detailed research.

- shapBackgroundSamples:

  Number of background samples for SHAP baseline calculation. More
  samples provide more stable explanations but increase computation
  time.

- shapExplanationType:

  SHAP explanation method to use. Auto-detect chooses the most
  appropriate method based on the model type. TreeSHAP is fastest for
  tree models, Kernel SHAP works with any model but is slower.

- generateSHAPPlots:

  Generate SHAP visualization plots including summary plots, bar plots,
  and dependence plots for model interpretability.

- shapPatientProfiles:

  Types of patient profiles to include in individual SHAP explanations.
  Helps understand how different patient characteristics influence
  staging-based predictions.

- shapInteractionAnalysis:

  Perform SHAP interaction analysis to identify important feature
  interactions. Shows how combinations of features affect predictions
  beyond individual feature effects.

- shapClinicalThresholds:

  Comma-separated risk thresholds for clinical decision boundaries in
  SHAP analysis (e.g., "0.25, 0.50, 0.75" for low/moderate/high risk).
  Used to interpret SHAP values in clinical context.

- performCompetingRisksAdvanced:

  Perform comprehensive competing risks analysis using Fine-Gray
  subdistribution hazard models and Cumulative Incidence Function (CIF)
  analysis. Essential when competing events prevent observation of
  primary outcome and standard survival analysis may be biased.

- competingRisksMethod:

  Method for competing risks analysis. Fine-Gray models cumulative
  incidence, cause-specific models instantaneous hazard rates.
  Comprehensive includes both approaches for complete assessment.

- cifTimePoints:

  Time points for Cumulative Incidence Function analysis (e.g., "12, 24,
  36, 60" for 1, 2, 3, and 5-year analysis). Use clinically relevant
  time points for the specific cancer type.

- competingEventLevels:

  Comma-separated list of event categories for competing risks analysis.
  Typically includes primary event (cancer death), competing events
  (other causes), and censoring indicator.

- primaryEventLevel:

  Specify the primary event of interest for competing risks analysis
  (e.g., "cancer_death", "disease_progression", "cardiovascular_death").
  Must match one of the competing event categories.

- generateCIFPlots:

  Generate Cumulative Incidence Function plots showing probability of
  each event type over time. Essential for visualizing competing risks
  patterns and staging system comparisons.

- performGrayTest:

  Perform Gray's test for equality of cumulative incidence functions
  across staging groups. Tests whether CIF curves differ significantly
  between stages for each event type.

- cifConfidenceLevel:

  Confidence level for Cumulative Incidence Function confidence
  intervals and statistical tests.

- competingRisksCovariates:

  Additional variables to include in competing risks regression models.
  Include important prognostic factors that may influence both primary
  and competing events.

- stratifyByStaging:

  Perform separate competing risks analysis for each staging system
  (original vs new) to compare their performance in the presence of
  competing events.

- calculateCRCIndex:

  Calculate C-index specifically adapted for competing risks analysis.
  Provides discrimination metrics that properly account for competing
  events when evaluating staging system performance.

- generateCRSummary:

  Generate comprehensive summary table with Fine-Gray regression
  results, cumulative incidence estimates, and staging system
  comparisons in competing risks context.

- performMultiStateAnalysis:

  Perform multi-state survival analysis for complex disease progression
  scenarios where patients can transition between multiple health states
  over time. Essential for modeling disease progression, remission,
  relapse, and death in oncology.

- multiStateModel:

  Type of multi-state model to fit. Illness-Death models progression
  from healthy to disease to death. Progression models include stable,
  progressive, terminal states. Comprehensive fits multiple models for
  comparison.

- stateVariable:

  Variable indicating patient disease states (e.g., stable, progressive,
  deceased, remission). Should contain all possible states that patients
  can transition between during follow-up.

- transitionTimeVariable:

  Time variable indicating when state transitions occurred. For multiple
  transitions per patient, use comma-separated times or separate records
  for each transition.

- multiStateStates:

  Comma-separated list of all possible states in order of progression
  (e.g., "healthy, disease, death" or "stable, progressive, remission,
  death"). Must match levels in the state variable.

- absorptionStates:

  Comma-separated list of absorbing states that patients cannot leave
  once entered (e.g., "death", "terminal"). These represent final
  outcomes in the disease process.

- multiStateCovariates:

  Additional variables to include in multi-state models as covariates.
  Include important prognostic factors that may influence transition
  rates between states.

- calculateTransitionProbabilities:

  Calculate state transition probabilities between all possible state
  pairs. Provides insight into likelihood of disease progression,
  remission, and mortality transitions.

- multiStateTimePoints:

  Time points for transition probability calculations (e.g., "6, 12, 24,
  36, 60" for 6-month intervals up to 5 years). Use clinically relevant
  time points for disease monitoring.

- generateTransitionMatrix:

  Generate comprehensive transition intensity matrix showing hazard
  rates for all possible state transitions. Essential for understanding
  disease progression dynamics.

- multiStateGraphics:

  Generate multi-state model visualizations including state transition
  diagrams, probability plots, and Aalen-Johansen estimator curves for
  state occupancy probabilities.

- msStratifyByStaging:

  Perform separate multi-state analysis for each staging system to
  compare their ability to predict disease transitions and progression
  patterns.

- multiStateValidation:

  Perform model validation including goodness-of-fit testing, residual
  analysis, and cross-validation for multi-state models. Computationally
  intensive but provides robust model assessment.

- generateMSMSummary:

  Generate comprehensive summary table with transition intensities,
  hazard ratios, and state occupancy probabilities comparing staging
  systems in multi-state framework.

- performRandomForestAnalysis:

  Perform Random Survival Forest analysis as a non-parametric
  alternative to Cox proportional hazards models. Provides robust
  predictions through ensemble methods without proportional hazards
  assumptions, ideal for complex interactions and non-linear
  relationships.

- forestModelType:

  Type of random forest model to fit. RSF is the standard approach,
  conditional inference forests handle categorical variables better,
  extra trees provide additional randomization. Ensemble combines
  multiple approaches for maximum robustness.

- forestNTrees:

  Number of trees in the random forest. More trees generally improve
  performance but increase computation time. 500 trees provide good
  balance between accuracy and speed for most applications.

- forestMTry:

  Number of variables randomly selected at each split. Use "auto" for
  automatic selection (sqrt of total variables), or specify a number.
  Lower values increase randomization, higher values may improve
  accuracy.

- forestMinNodeSize:

  Minimum number of observations in terminal nodes. Larger values
  prevent overfitting but may reduce model flexibility. Recommended:
  3-10 for survival data depending on sample size.

- forestCovariates:

  Additional variables to include in random forest models alongside
  staging variables. Include important clinical variables for
  comprehensive non-parametric survival modeling.

- calculateVariableImportance:

  Calculate variable importance measures using permutation-based
  methods. Shows which variables contribute most to survival
  predictions, complementing SHAP analysis with forest-specific
  importance metrics.

- forestImportanceType:

  Type of variable importance measure. Permutation importance is most
  interpretable, VIMP is RF-specific, minimal depth shows variable
  selection frequency. Comprehensive provides all measures.

- performForestValidation:

  Perform out-of-bag validation and cross-validation for random forest
  models. Provides robust assessment of model performance including
  prediction error rates and concordance indices.

- forestPredictionTimePoints:

  Time points for survival probability predictions from random forest
  models (e.g., "12, 24, 36, 60" for 1, 2, 3, and 5-year predictions).
  Use clinically relevant time points for staging comparison.

- generateSurvivalPredictions:

  Generate individual patient survival probability predictions at
  specified time points. Provides personalized risk assessments based on
  random forest ensemble predictions.

- forestDiscriminationMetrics:

  Calculate discrimination metrics specifically for random forest models
  including C-index, Integrated Brier Score, and time-dependent AUC.
  Compares forest performance with traditional Cox models.

- forestStagingComparison:

  Use random forest models to compare staging systems through
  non-parametric ensemble methods. Provides robust staging comparison
  without proportional hazards assumptions.

- forestBootstrap:

  Perform bootstrap validation of random forest models with multiple
  bootstrap samples. Provides confidence intervals for forest-based
  predictions and importance measures.

- forestBootstrapSamples:

  Number of bootstrap samples for forest validation. More samples
  provide more stable confidence intervals but increase computation
  time.

- generateForestSummary:

  Generate comprehensive summary of random forest analysis including
  model performance, variable importance, staging comparison, and
  clinical recommendations based on ensemble predictions.

- rfAnalyzeOldStage:

  Include old staging system in random forest analysis

- rfAnalyzeNewStage:

  Include new staging system in random forest analysis

- rfMtryAuto:

  Automatically select number of variables to try at each split

- rfBootstrapType:

  Bootstrap sampling strategy for random forest

- rfSamplingType:

  Sampling method for random forest bootstrap

- rfMinimalDepth:

  Perform minimal depth variable selection analysis

- performCureModelAnalysis:

  Perform cure model analysis for populations where a fraction of
  patients may be effectively cured. Uses mixture models to separate
  susceptible and cured populations, particularly relevant for cancer
  staging analysis.

- cureModelType:

  Type of cure model to fit. Mixture models assume a cured fraction with
  infinite survival, promotion time models use biological mechanisms,
  and both provides comprehensive comparison.

- cureDistribution:

  Underlying survival distribution for the susceptible population in
  cure models. Weibull is most flexible, exponential is simplest,
  log-normal and log-logistic provide alternative hazard shapes.

- cureAnalyzeOldStage:

  Fit cure models to original staging system to estimate cure fractions
  and survival patterns for susceptible patients in each stage.

- cureAnalyzeNewStage:

  Fit cure models to new staging system to estimate cure fractions and
  survival patterns, enabling comparison of staging discrimination for
  both cured and susceptible populations.

- cureFractionEstimation:

  Method for estimating cure fractions. Parametric uses maximum
  likelihood with specified distributions, non-parametric uses
  Kaplan-Meier plateau detection, both provides validation.

- cureConfidenceLevel:

  Confidence level for cure model parameter estimates and cure fraction
  confidence intervals. Standard 95 percent provides balance between
  precision and coverage.

- cureBootstrapCI:

  Calculate bootstrap confidence intervals for cure fractions and model
  parameters. Provides robust uncertainty quantification especially for
  small samples or complex models.

- cureBootstrapReps:

  Number of bootstrap replications for confidence interval calculation.
  More replications provide more stable intervals but increase
  computation time.

- cureTimeHorizon:

  Time horizon for cure assessment in months. Patients surviving beyond
  this time without events are considered potentially cured. Typical
  values: 60-120 months for most cancers.

- curePlateauThreshold:

  Threshold for detecting survival curve plateau in non-parametric cure
  fraction estimation. Lower values detect smaller plateaus but may be
  more sensitive to noise.

- cureCovariates:

  Additional variables to include in cure model analysis alongside
  staging variables. Can affect both cure probability and survival of
  susceptible patients.

- cureModelComparison:

  Compare cure models between staging systems using likelihood ratio
  tests, AIC/BIC criteria, and cure fraction differences. Assesses which
  staging system better identifies cured patients.

- cureStageSpecificAnalysis:

  Perform separate cure model analysis for each stage group to
  understand stage-specific cure patterns and survival of susceptible
  patients. Essential for staging validation.

- cureGoodnessOfFit:

  Perform goodness-of-fit tests for cure models including
  Kolmogorov-Smirnov tests and visual diagnostic plots. Validates model
  assumptions and identifies potential misspecification.

- generateCureSummary:

  Generate comprehensive summary of cure model analysis including cure
  fractions by stage, model comparison results, and clinical
  interpretation for staging system evaluation.

- performIntervalCensoringAnalysis:

  Perform interval censoring analysis for events detected between
  visits. This handles cases where the exact event time is unknown but
  falls within a known interval (e.g., between clinic visits). Uses
  icenReg package for non-parametric and parametric interval-censored
  survival analysis.

- intervalCensoringLeftTime:

  Variable containing the left endpoint of the censoring interval. For
  exact observations, this should equal the right endpoint. For
  left-censored observations, use 0 or NA.

- intervalCensoringRightTime:

  Variable containing the right endpoint of the censoring interval. For
  right-censored observations, use Inf or a large value. For exact
  observations, this should equal the left endpoint.

- intervalCensoringDistribution:

  Distribution assumption for parametric interval-censored regression.
  Weibull is most commonly used and provides good flexibility.

- intervalCensoringModel:

  Type of interval censoring model to fit. Non-parametric uses
  non-parametric maximum likelihood estimation (NPMLE). Parametric fits
  accelerated failure time models with specified distribution.

- intervalCensoringBootstrap:

  Calculate bootstrap confidence intervals for non-parametric estimates.
  This provides uncertainty quantification for the survival function
  estimates with interval-censored data.

- intervalCensoringBootstrapSamples:

  Number of bootstrap samples for confidence interval calculation. More
  samples provide more accurate intervals but increase computation time.

- intervalCensoringCompareStages:

  Compare survival functions between different staging systems
  accounting for interval censoring. Provides likelihood ratio tests and
  information criteria for model comparison.

- intervalCensoringPlots:

  Generate survival plots specifically designed for interval-censored
  data, including non-parametric survival function estimates and
  comparison plots between staging systems.

- intervalCensoringDiagnostics:

  Perform model diagnostics including convergence assessment, residual
  analysis for parametric models, and goodness-of-fit tests for
  interval-censored regression models.

- intervalCensoringPredictionTime:

  Comma-separated list of time points (in months) for survival
  probability predictions. These will be used for staging system
  comparison and clinical interpretation of interval-censored survival
  estimates.

- intervalCensoringConfidenceLevel:

  Confidence level for interval estimates and hypothesis tests. Standard
  choices are 0.90, 0.95, or 0.99.

- intervalCensoringAdjustVariables:

  Additional variables to include in parametric interval-censored
  regression models for adjusted survival analysis. These will be
  included as covariates in the accelerated failure time model.

- performInformativeCensoringAnalysis:

  Perform tests for informative censoring to validate the assumption
  that censoring is non-informative. Informative censoring occurs when
  the censoring mechanism is related to the failure time, potentially
  biasing survival estimates. This analysis provides tests and
  adjustments for non-random censoring patterns.

- informativeCensoringTestMethod:

  Method for testing informative censoring. Correlation tests examine
  relationship between censoring and survival times. Regression tests
  model censoring as outcome. Competing risks treats censoring as
  competing event. Landmark analysis examines censoring patterns.

- informativeCensoringCovariates:

  Variables that may be associated with the censoring mechanism. These
  could include clinical factors, treatment decisions, or administrative
  factors that might influence when patients are censored from the
  study.

- informativeCensoringLandmarkTimes:

  Comma-separated list of landmark time points (in months) for landmark
  analysis of censoring patterns. Analysis examines whether censoring
  probabilities differ across staging groups at these specific time
  points.

- informativeCensoringAdjustmentMethod:

  Method for adjusting survival estimates when informative censoring is
  detected. IPW uses inverse probability weighting. Multiple imputation
  imputes censored failure times. Sensitivity analysis explores range of
  possible bias effects.

- informativeCensoringIPWVariables:

  Variables to include in inverse probability weighting model for
  censoring probability estimation. Should include factors that predict
  censoring but are not affected by the outcome.

- informativeCensoringSensitivityRange:

  Comma-separated list of sensitivity parameters for bias analysis.
  These represent hazard ratio multipliers for exploring potential bias
  from informative censoring (1.0 = no bias assumption).

- informativeCensoringBootstrap:

  Calculate bootstrap confidence intervals for adjusted survival
  estimates and bias-corrected parameters. Provides uncertainty
  quantification for informative censoring adjustments.

- informativeCensoringBootstrapSamples:

  Number of bootstrap samples for confidence interval calculation in
  informative censoring analysis. More samples provide more accurate
  intervals but increase computation time.

- informativeCensoringAlpha:

  Significance level for testing informative censoring hypotheses. Used
  for determining whether censoring appears to be informative and for
  confidence interval construction.

- informativeCensoringPlots:

  Generate diagnostic plots for informative censoring assessment
  including censoring probability over time, correlation plots, and
  sensitivity analysis visualizations.

- informativeCensoringCompareStages:

  Compare censoring patterns across different staging groups to assess
  whether censoring differs by stage, which could indicate stage-related
  informative censoring that affects staging system evaluation.

- performConcordanceProbabilityAnalysis:

  Perform advanced concordance probability analysis for heavily censored
  data. This provides alternative concordance measures beyond
  traditional C-index, including Harrell's C-index modifications, Uno's
  C-index for heavily censored data, and time-dependent concordance
  measures specifically designed for staging system evaluation with high
  censoring rates.

- concordanceProbabilityMethods:

  Concordance probability estimation methods. Harrell C-index is
  traditional but may be biased with heavy censoring. Uno C-index uses
  inverse probability weighting for censoring. Time-dependent measures
  evaluate concordance at specific time points. IPCW and weighted
  methods provide robust alternatives.

- concordanceProbabilityTimePoints:

  Comma-separated list of time points (in months) for time-dependent
  concordance assessment. These will be used for evaluating staging
  system discrimination at clinically relevant time horizons.

- concordanceProbabilityWeighting:

  Weighting strategy for concordance probability estimation. Uniform
  gives equal weight to all pairs. Sample size weights by stage
  frequency. Event rate weights by observed events. Follow-up weights by
  observation time. Inverse variance uses precision weighting.

- concordanceProbabilityBootstrap:

  Calculate bootstrap confidence intervals for concordance probability
  estimates. This provides uncertainty quantification for discrimination
  measures, especially important for heavily censored data where
  traditional standard errors may be unreliable.

- concordanceProbabilityBootstrapSamples:

  Number of bootstrap samples for confidence interval calculation. More
  samples provide more accurate intervals but increase computation time.
  Recommended minimum 500 for reliable confidence intervals.

- concordanceProbabilityConfidenceLevel:

  Confidence level for concordance probability confidence intervals.
  Standard choices are 0.90, 0.95, or 0.99 for 90 percent, 95 percent,
  or 99 percent confidence intervals respectively.

- concordanceProbabilityCompareStages:

  Compare concordance probabilities between different staging systems
  using hypothesis tests and confidence interval overlap assessment.
  Provides statistical evidence for staging system discrimination
  differences accounting for heavy censoring.

- concordanceProbabilityAdjustVariables:

  Additional variables to include in adjusted concordance analysis.
  These variables will be included alongside staging in multivariable
  models to assess staging contribution to discrimination beyond other
  prognostic factors.

- concordanceProbabilityRobustnessAnalysis:

  Perform robustness analysis for concordance probability estimates
  including sensitivity to censoring assumptions, outlier influence, and
  temporal stability assessment for comprehensive validation of staging
  system discrimination.

- concordanceProbabilityAlpha:

  Significance level for concordance probability hypothesis tests and
  confidence interval construction. Used for testing differences between
  staging systems and assessing statistical significance of
  discrimination improvements.

- concordanceProbabilityDiagnostics:

  Perform diagnostic assessment of concordance probability estimates
  including convergence checks, influence diagnostics, and sensitivity
  analysis to ensure reliable discrimination assessment for staging
  system evaluation.

- performWinRatioAnalysis:

  Perform win ratio analysis for composite endpoint analysis in staging
  comparison. The win ratio is a novel method for analyzing composite
  endpoints that respects the clinical hierarchy of outcomes and
  provides intuitive interpretation for staging system evaluation.

- winRatioEndpoints:

  Clinical hierarchy of endpoints for win ratio analysis. More important
  outcomes are prioritized in the analysis. Death is typically the most
  important endpoint, followed by disease-specific outcomes. The
  hierarchy determines how patient pairs are compared.

- winRatioDeathVariable:

  Variable indicating death or primary endpoint occurrence (1 = event, 0
  = no event). This is typically the most important outcome in the
  hierarchy and is compared first when evaluating patient pairs.

- winRatioSecondaryEndpoint:

  Variable for secondary endpoint (e.g., disease progression,
  recurrence). This endpoint is evaluated when the primary endpoint
  comparison is tied. Can be binary (event/no event) or continuous (time
  to event).

- wrSecondaryDirection:

  Direction of improvement for secondary endpoint in win ratio

- winRatioTertiaryEndpoint:

  Variable for tertiary endpoint (e.g., response, quality of life). This
  endpoint is evaluated when both primary and secondary comparisons are
  tied. Can be binary or continuous.

- winRatioTimeVariables:

  Time variables corresponding to each endpoint in the hierarchy. Should
  be provided in the same order as the endpoints. Used for time-to-event
  comparisons when endpoints are not binary.

- winRatioMatchingStrategy:

  Strategy for forming patient pairs for comparison. All pairs compares
  every patient from one group with every patient from another. Matched
  pairs uses pre-specified matching. Stratified performs within-stage
  comparisons. Propensity matching balances baseline characteristics.

- winRatioConfidenceMethod:

  Method for calculating confidence intervals for the win ratio.
  Bootstrap is most robust but computationally intensive. Asymptotic
  uses large sample theory. Permutation provides exact p-values.

- winRatioBootstrapSamples:

  Number of bootstrap samples for confidence interval calculation when
  using bootstrap method. More samples provide more accurate intervals
  but increase computation time.

- winRatioConfidenceLevel:

  Confidence level for win ratio confidence intervals and hypothesis
  tests. Standard choices are 0.90, 0.95, or 0.99 for 90 percent, 95
  percent, or 99 percent confidence intervals respectively.

- winRatioHandleTies:

  Strategy for handling tied comparisons. Split assigns 0.5 wins to
  each. Ignore excludes tied pairs from analysis. Next endpoint proceeds
  to compare the next outcome in the hierarchy for tied pairs.

- winRatioSensitivityAnalysis:

  Perform sensitivity analysis for win ratio including assessment of
  endpoint ordering impact, missing data influence, and robustness to
  matching strategy choices.

- winRatioGeneralizedPairwise:

  Use generalized pairwise comparison (GPC) framework which extends win
  ratio to include continuous outcomes and provides additional metrics
  like net benefit and win odds.

- performFrailtyModelsAnalysis:

  Perform frailty models analysis for clustered survival data using
  mixed-effects Cox models (coxme) for multi-institutional data with
  center-specific random effects and clustering adjustments.

- frailtyClusterVariable:

  Variable defining clusters/institutions for frailty modeling (e.g.,
  hospital, center, surgeon). Used to account for unobserved
  heterogeneity and clustering effects in survival analysis.

- frailtyDistribution:

  Distribution assumption for the frailty (random effects) terms. Gamma
  distribution is most common and provides multiplicative frailty
  effects on the hazard function.

- frailtyBootstrap:

  Perform bootstrap validation for frailty model parameters and variance
  components to assess model stability and provide robust confidence
  intervals.

- frailtyBootstrapSamples:

  Number of bootstrap samples for frailty model validation. Higher
  values provide more stable estimates but increase computational time.

- frailtyVarianceComponents:

  Analyze variance components to quantify the proportion of total
  variation explained by cluster-level random effects vs
  individual-level factors.

- frailtyHeterogeneityTest:

  Test for significant frailty/heterogeneity using likelihood ratio
  tests comparing frailty models to standard Cox models without random
  effects.

- frailtyClusterComparison:

  Perform cluster-specific survival analysis comparing staging systems
  within each cluster/institution to assess consistency of staging
  performance across centers.

- frailtyModelSelection:

  Perform systematic model selection comparing different frailty
  distributions and model specifications using AIC/BIC criteria and
  likelihood ratio tests.

- frailtyPredictiveAccuracy:

  Assess predictive accuracy of frailty models using cross-validation
  and concordance measures accounting for clustering structure in the
  data.

- frailtyDiagnostics:

  Comprehensive model diagnostics including residual analysis, influence
  detection, and goodness-of-fit assessment for frailty models with
  clustering adjustments.

- frailtyAdvancedInference:

  Advanced statistical inference including profile likelihood confidence
  intervals, score tests, and robust variance estimation for complex
  frailty model specifications.

- performClinicalUtilityAnalysis:

  Perform clinical utility index analysis combining
  sensitivity/specificity with disease prevalence to assess clinical
  decision-making value of staging systems beyond statistical
  discrimination.

- clinicalUtilityPrevalence:

  Disease prevalence (proportion with events) for clinical utility
  calculations. Can be estimated from study data or specified based on
  population characteristics.

- clinicalUtilityTimePoint:

  Time point (in months) for clinical utility assessment. Should
  represent clinically relevant decision-making horizon for the specific
  cancer type and staging system.

- clinicalUtilityThresholds:

  Range of risk thresholds for clinical utility assessment. Different
  ranges appropriate for different clinical decision contexts and
  treatment aggressiveness preferences.

- clinicalUtilityNNT:

  Calculate Number Needed to Treat (NNT) and Number Needed to Harm (NNH)
  based on staging-guided interventions with configurable treatment
  effect assumptions.

- clinicalUtilityTreatmentEffect:

  Assumed treatment effect (hazard ratio) for calculating NNT/NNH.
  Should reflect realistic treatment benefits for staging-guided
  interventions in the specific clinical context.

- clinicalUtilityComparison:

  Compare clinical utility between staging systems using net benefit
  difference analysis and utility improvement quantification across
  different risk thresholds.

- clinicalUtilityCostEffectiveness:

  Include basic cost-effectiveness considerations in clinical utility
  assessment with configurable cost assumptions for staging-guided
  interventions and outcomes.

- clinicalUtilityCostPerIntervention:

  Estimated cost per staging-guided intervention for cost-effectiveness
  analysis. Should reflect realistic healthcare costs in the relevant
  healthcare system and setting.

- clinicalUtilityBootstrap:

  Perform bootstrap validation for clinical utility metrics including
  confidence intervals for NNT, net benefit differences, and utility
  improvement measures.

- clinicalUtilityBootstrapSamples:

  Number of bootstrap samples for clinical utility validation. Higher
  values provide more stable estimates but increase computational time.

- clinicalUtilityTimeVarying:

  Assess clinical utility across multiple time points to understand how
  staging system value changes over time horizon and identify optimal
  decision timing.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$welcomeMessage` |  |  |  |  | a html |
| `results$copyReadyReport` |  |  |  |  | a html |
| `results$guidedModeProgress` |  |  |  |  | a html |
| `results$mydataview` |  |  |  |  | a preformatted |
| `results$mydataview2` |  |  |  |  | a preformatted |
| `results$migrationOverviewExplanation` |  |  |  |  | a html |
| `results$migrationOverview` |  |  |  |  | a table |
| `results$migrationMatrixExplanation` |  |  |  |  | a html |
| `results$migrationMatrix` |  |  |  |  | a table |
| `results$stageDistributionExplanation` |  |  |  |  | a html |
| `results$stageDistribution` |  |  |  |  | a table |
| `results$migrationSummaryExplanation` |  |  |  |  | a html |
| `results$migrationSummary` |  |  |  |  | a table |
| `results$statisticalComparisonExplanation` |  |  |  |  | a html |
| `results$statisticalComparison` |  |  |  |  | a table |
| `results$concordanceComparisonExplanation` |  |  |  |  | a html |
| `results$concordanceComparison` |  |  |  |  | a table |
| `results$nriResultsExplanation` |  |  |  |  | a html |
| `results$nriResults` |  |  |  |  | a table |
| `results$idiResultsExplanation` |  |  |  |  | a html |
| `results$idiResults` |  |  |  |  | a table |
| `results$multifactorialAnalysisExplanation` |  |  |  |  | a html |
| `results$multifactorialResults` |  |  |  |  | a table |
| `results$multifactorialResultsExplanation` |  |  |  |  | a html |
| `results$adjustedCIndexComparison` |  |  |  |  | a table |
| `results$adjustedCIndexComparisonExplanation` |  |  |  |  | a html |
| `results$nestedModelTests` |  |  |  |  | a table |
| `results$nestedModelTestsExplanation` |  |  |  |  | a html |
| `results$stepwiseResults` |  |  |  |  | a table |
| `results$stepwiseResultsExplanation` |  |  |  |  | a html |
| `results$interactionTests` |  |  |  |  | a table |
| `results$interactionTestsExplanation` |  |  |  |  | a html |
| `results$stratifiedAnalysisTable` |  |  |  |  | a table |
| `results$stratifiedAnalysisExplanation` |  |  |  |  | a html |
| `results$rocAnalysis` |  |  |  |  | a table |
| `results$integratedAUCAnalysis` |  |  |  |  | a table |
| `results$dcaResultsExplanation` |  |  |  |  | a html |
| `results$dcaResults` |  |  |  |  | a table |
| `results$pseudoR2ResultsExplanation` |  |  |  |  | a html |
| `results$pseudoR2Results` |  |  |  |  | a table |
| `results$decisionCurvesExplanation` |  |  |  |  | a html |
| `results$decisionCurves` |  |  |  |  | an image |
| `results$bootstrapResults` |  |  |  |  | a table |
| `results$bootstrapValidationExplanation` |  |  |  |  | a html |
| `results$willRogersAnalysisExplanation` |  |  |  |  | a html |
| `results$willRogersBasicAnalysis` |  |  |  |  | a table |
| `results$likelihoodTestsExplanation` |  |  |  |  | a html |
| `results$likelihoodTests` |  |  |  |  | a table |
| `results$linearTrendTestExplanation` |  |  |  |  | a html |
| `results$linearTrendTest` |  |  |  |  | a table |
| `results$homogeneityTestsExplanation` |  |  |  |  | a html |
| `results$homogeneityTests` |  |  |  |  | a table |
| `results$trendTestsExplanation` |  |  |  |  | a html |
| `results$trendTests` |  |  |  |  | a table |
| `results$clinicalInterpretationExplanation` |  |  |  |  | a html |
| `results$clinicalInterpretation` |  |  |  |  | a table |
| `results$executiveSummaryExplanation` |  |  |  |  | a html |
| `results$executiveSummary` |  |  |  |  | a table |
| `results$statisticalSummaryExplanation` |  |  |  |  | a html |
| `results$statisticalSummary` |  |  |  |  | a table |
| `results$effectSizesExplanation` |  |  |  |  | a html |
| `results$effectSizes` |  |  |  |  | a table |
| `results$methodologyNotes` |  |  |  |  | a html |
| `results$migrationHeatmapExplanation` |  |  |  |  | a html |
| `results$migrationHeatmap` |  |  |  |  | an image |
| `results$sankeyDiagram` |  |  |  |  | an image |
| `results$rocComparisonExplanation` |  |  |  |  | a html |
| `results$rocComparisonPlot` |  |  |  |  | an image |
| `results$forestPlotExplanation` |  |  |  |  | a html |
| `results$forestPlot` |  |  |  |  | an image |
| `results$calibrationAnalysisExplanation` |  |  |  |  | a html |
| `results$calibrationAnalysis` |  |  |  |  | a table |
| `results$calibrationPlotsExplanation` |  |  |  |  | a html |
| `results$calibrationPlots` |  |  |  |  | an image |
| `results$advancedMigrationExplanation` |  |  |  |  | a html |
| `results$monotonicityCheck` |  |  |  |  | a table |
| `results$willRogersAnalysis` |  |  |  |  | a table |
| `results$willRogersVisualization` |  |  |  |  | an image |
| `results$migrationSurvivalComparison` |  |  |  |  | an image |
| `results$willRogersEnhancedAnalysis` |  |  |  |  | a table |
| `results$willRogersStageDetail` |  |  |  |  | a table |
| `results$stageSpecificCIndex` |  |  |  |  | a table |
| `results$enhancedPseudoR2` |  |  |  |  | a table |
| `results$enhancedReclassificationMetrics` |  |  |  |  | a table |
| `results$proportionalHazardsTest` |  |  |  |  | a table |
| `results$decisionCurveAnalysis` |  |  |  |  | a table |
| `results$survivalCurvesExplanation` |  |  |  |  | a html |
| `results$survivalCurves` |  |  |  |  | an image |
| `results$dashboardExplanation` |  |  |  |  | a html |
| `results$comparativeAnalysisDashboard` |  |  |  |  | a table |
| `results$willRogersEvidenceSummaryExplanation` |  |  |  |  | a html |
| `results$willRogersEvidenceSummary` |  |  |  |  | a table |
| `results$willRogersClinicalRecommendation` |  |  |  |  | a table |
| `results$enhancedMigrationPatternAnalysis` |  |  |  |  | a table |
| `results$landmarkAnalysisResults` |  |  |  |  | a table |
| `results$advancedMigrationHeatmapStats` |  |  |  |  | a table |
| `results$abbreviationGlossary` |  |  |  |  | a html |
| `results$crossValidationExplanation` |  |  |  |  | a html |
| `results$crossValidationResults` |  |  |  |  | a table |
| `results$crossValidationPlot` |  |  |  |  | an image |
| `results$enhancedLRComparison` |  |  |  |  | a table |
| `results$stageMigrationEffectExplanation` |  |  |  |  | a html |
| `results$stageMigrationEffect` |  |  |  |  | a table |
| `results$stageMigrationEffectAssessment` |  |  |  |  | a table |
| `results$rmstAnalysisExplanation` |  |  |  |  | a html |
| `results$rmstByStage` |  |  |  |  | a table |
| `results$rmstComparison` |  |  |  |  | a table |
| `results$competingRisksExplanation` |  |  |  |  | a html |
| `results$competingRisksEventDistribution` |  |  |  |  | a table |
| `results$competingRisksComparison` |  |  |  |  | a table |
| `results$optimalCutpointAnalysis` |  |  |  |  | a table |
| `results$cutpointValidation` |  |  |  |  | a table |
| `results$generatedStagingSystem` |  |  |  |  | a table |
| `results$shapGlobalImportance` |  |  |  |  | a table |
| `results$shapIndividualExplanations` |  |  |  |  | a table |
| `results$shapInteractions` |  |  |  |  | a table |
| `results$shapSummaryStats` |  |  |  |  | a table |
| `results$fineGrayResults` |  |  |  |  | a table |
| `results$causeSpecificResults` |  |  |  |  | a table |
| `results$cifSummary` |  |  |  |  | a table |
| `results$competingRisksCIndex` |  |  |  |  | a table |
| `results$competingRisksSummary` |  |  |  |  | a table |
| `results$transitionIntensities` |  |  |  |  | a table |
| `results$transitionProbabilities` |  |  |  |  | a table |
| `results$stateOccupancy` |  |  |  |  | a table |
| `results$multiStateComparison` |  |  |  |  | a table |
| `results$multiStateSummary` |  |  |  |  | a table |
| `results$forestVariableImportance` |  |  |  |  | a table |
| `results$forestModelPerformance` |  |  |  |  | a table |
| `results$forestSurvivalPredictions` |  |  |  |  | a table |
| `results$forestCoxComparison` |  |  |  |  | a table |
| `results$forestStagingComparisonTable` |  |  |  |  | a table |
| `results$forestAnalysisSummary` |  |  |  |  | a table |
| `results$cureFractionEstimates` |  |  |  |  | a table |
| `results$cureModelParameters` |  |  |  |  | a table |
| `results$cureModelComparisonTable` |  |  |  |  | a table |
| `results$stageSpecificCureAnalysis` |  |  |  |  | a table |
| `results$cureModelBootstrap` |  |  |  |  | a table |
| `results$cureAnalysisSummary` |  |  |  |  | a table |
| `results$intervalCensoringOverview` |  |  |  |  | a table |
| `results$intervalCensoringNonparametric` |  |  |  |  | a table |
| `results$intervalCensoringParametric` |  |  |  |  | a table |
| `results$intervalCensoringComparison` |  |  |  |  | a table |
| `results$intervalCensoringDiagnosticsTable` |  |  |  |  | a table |
| `results$intervalCensoringSummary` |  |  |  |  | a table |
| `results$informativeCensoringOverview` |  |  |  |  | a table |
| `results$informativeCensoringTests` |  |  |  |  | a table |
| `results$informativeCensoringByStage` |  |  |  |  | a table |
| `results$informativeCensoringAdjustment` |  |  |  |  | a table |
| `results$informativeCensoringSensitivity` |  |  |  |  | a table |
| `results$informativeCensoringDiagnostics` |  |  |  |  | a table |
| `results$informativeCensoringSummary` |  |  |  |  | a table |
| `results$concordanceProbabilityOverview` |  |  |  |  | a table |
| `results$concordanceProbabilityEstimates` |  |  |  |  | a table |
| `results$concordanceProbabilityTimeDependentComplex` |  |  |  |  | a table |
| `results$concordanceProbabilityComparison` |  |  |  |  | a table |
| `results$concordanceProbabilityRobustness` |  |  |  |  | a table |
| `results$concordanceProbabilityDiagnosticsTable` |  |  |  |  | a table |
| `results$concordanceProbabilitySummary` |  |  |  |  | a table |
| `results$winRatioOverview` |  |  |  |  | a table |
| `results$winRatioPrimaryResults` |  |  |  |  | a table |
| `results$winRatioEndpointContributions` |  |  |  |  | a table |
| `results$winRatioStageSpecific` |  |  |  |  | a table |
| `results$winRatioSensitivityResults` |  |  |  |  | a table |
| `results$winRatioGeneralizedPairwiseResults` |  |  |  |  | a table |
| `results$winRatioSummary` |  |  |  |  | a table |
| `results$frailtyModelsOverview` |  |  |  |  | a table |
| `results$frailtyModelsComparison` |  |  |  |  | a table |
| `results$frailtyModelsVarianceComponents` |  |  |  |  | a table |
| `results$frailtyModelsClusterSpecific` |  |  |  |  | a table |
| `results$frailtyModelsBootstrap` |  |  |  |  | a table |
| `results$frailtyModelsDiagnostics` |  |  |  |  | a table |
| `results$frailtyModelsSummary` |  |  |  |  | a table |
| `results$clinicalUtilityOverview` |  |  |  |  | a table |
| `results$clinicalUtilityComparisonTable` |  |  |  |  | a table |
| `results$clinicalUtilityNNTTable` |  |  |  |  | a table |
| `results$clinicalUtilityNetBenefit` |  |  |  |  | a table |
| `results$clinicalUtilityTimeVaryingTable` |  |  |  |  | a table |
| `results$clinicalUtilityBootstrapTable` |  |  |  |  | a table |
| `results$clinicalUtilitySummary` |  |  |  |  | a table |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$migrationOverview$asDF`

`as.data.frame(results$migrationOverview)`
