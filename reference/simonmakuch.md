# Simon-Makuch Time-Dependent Survival Analysis

Performs comprehensive survival analysis with time-dependent variables
using the Simon-Makuch method. This specialized analysis addresses the
critical challenge of immortal time bias when studying treatments,
biomarkers, or exposures that change during patient follow-up.
**Clinical Applications:** - Treatment effectiveness when therapy timing
varies (e.g., adjuvant therapy, targeted agents) - Biomarker evolution
studies (e.g., mutation status, protein expression changes) - Disease
progression impact on outcomes (e.g., metastasis development, grade
progression) - Healthcare intervention timing effects (e.g., surgery,
radiation, supportive care) **Key Statistical Methods:** - Simon-Makuch
plots: Modified Kaplan-Meier curves for time-dependent exposures -
Landmark analysis: Conditional survival from fixed time points -
Time-dependent Cox regression: Hazard ratios accounting for exposure
timing - Immortal time bias assessment: Comparison of naive vs. proper
methods - Mantel-Byar testing: Specialized tests for time-dependent
comparisons **Why Use Simon-Makuch Analysis:** Standard survival
analysis assumes fixed baseline characteristics, leading to immortal
time bias when exposures change during follow-up. Simon-Makuch methods
properly account for the timing of exposure changes, providing unbiased
survival estimates and valid statistical comparisons.

## Usage

``` r
simonmakuch(
  data,
  survivalTime,
  event,
  eventLevel,
  timeDepVariable,
  timeDepTime,
  timeDepStatus,
  exposedLevel,
  enableMultipleTimeDep = FALSE,
  additionalTimeDepVars,
  analysisType = "comprehensive",
  confidenceLevel = 0.95,
  performLandmarkAnalysis = FALSE,
  landmarkTimes = "6, 12, 24",
  landmarkWindow = 1,
  performTimeDependentCox = FALSE,
  timeDependentCovariates,
  testTimeVaryingEffect = FALSE,
  assessImmortalTimeBias = FALSE,
  naiveComparison = FALSE,
  showSimonMakuchPlot = TRUE,
  showLandmarkPlots = FALSE,
  showCumulativeIncidencePlot = FALSE,
  plotTimeRange = "auto",
  showConfidenceIntervals = TRUE,
  showRiskTables = TRUE,
  plotExposureStatus = FALSE,
  performLogRankTest = TRUE,
  performMantelByarTest = FALSE,
  performTimeDepLRTest = FALSE,
  handleTieBreaking = "efron",
  robustVariance = FALSE,
  clusterVariable,
  showSurvivalEstimates = TRUE,
  showHazardRatios = TRUE,
  showLandmarkResults = FALSE,
  showExposurePatterns = FALSE,
  showModelDiagnostics = FALSE,
  performBootstrapValidation = FALSE,
  bootstrapSamples = 500,
  performSensitivityAnalysis = FALSE,
  showExplanations = TRUE,
  showMethodologyNotes = FALSE,
  includeClinicalGuidance = FALSE
)
```

## Arguments

- data:

  The dataset containing survival and time-dependent variable
  information.

- survivalTime:

  Time to event or censoring in consistent units (months recommended).
  This should be the total follow-up time from study entry.

- event:

  Event indicator (1 = event occurred, 0 = censored) or factor with
  event levels. For overall survival, this represents death from any
  cause.

- eventLevel:

  The level indicating event occurrence when using factor variables.

- timeDepVariable:

  The main time-dependent variable of interest (e.g., treatment status,
  biomarker status, disease progression). This variable can change
  during follow-up.

- timeDepTime:

  Time when the time-dependent variable changes status. Use 0 for
  baseline status, and actual time for when the variable changes (e.g.,
  time of treatment initiation, biomarker conversion).

- timeDepStatus:

  Status of the time-dependent variable at each time point. For example:
  "Unexposed", "Exposed", or "Pre-treatment", "Post-treatment".

- exposedLevel:

  The level that represents the "exposed" or "active" status of the
  time-dependent variable.

- enableMultipleTimeDep:

  Enable analysis of multiple time-dependent variables simultaneously.
  Useful for complex exposure patterns or multiple biomarkers.

- additionalTimeDepVars:

  Additional time-dependent variables to include in the analysis. Each
  should have corresponding time and status variables.

- analysisType:

  Type of Simon-Makuch analysis to perform. Comprehensive includes all
  methods for thorough time-dependent survival analysis.

- confidenceLevel:

  Confidence level for survival estimates and statistical tests.

- performLandmarkAnalysis:

  Perform landmark analysis at specified time points. This method
  analyzes survival from specific landmark times, avoiding immortal time
  bias when studying time-dependent exposures.

- landmarkTimes:

  Comma-separated list of landmark time points for analysis. Patients
  must survive to each landmark time to be included in that analysis.

- landmarkWindow:

  Time window around landmark time for determining exposure status.
  Exposure status is determined within this window before the landmark.

- performTimeDependentCox:

  Perform Cox regression with time-dependent covariates. This provides
  hazard ratios that properly account for changing exposure status.

- timeDependentCovariates:

  Additional covariates to include in time-dependent Cox regression.
  These can be baseline variables or other time-dependent variables.

- testTimeVaryingEffect:

  Test whether the effect of the time-dependent variable changes over
  time (non-proportional hazards for time-dependent covariates).

- assessImmortalTimeBias:

  Assess and correct for immortal time bias in the analysis. Compares
  naive analysis (without proper time-dependent handling) with corrected
  Simon-Makuch analysis.

- naiveComparison:

  Include comparison with naive analysis that ignores the time-dependent
  nature of the exposure. This demonstrates the importance of proper
  time-dependent analysis.

- showSimonMakuchPlot:

  Display Simon-Makuch survival curves that properly account for
  time-dependent exposure status changes during follow-up.

- showLandmarkPlots:

  Display survival curves from each landmark time point, showing how the
  effect of exposure varies with timing.

- showCumulativeIncidencePlot:

  Display cumulative incidence curves for time-dependent exposures,
  useful for understanding exposure patterns over time.

- plotTimeRange:

  Maximum time for survival plots. Use "auto" for automatic range or
  specify maximum months (e.g., "60" for 5-year follow-up).

- showConfidenceIntervals:

  Display confidence intervals around survival curves.

- showRiskTables:

  Display at-risk tables below survival curves, stratified by
  time-dependent exposure status.

- plotExposureStatus:

  Display plot showing how exposure status changes over time for the
  study population.

- performLogRankTest:

  Perform log-rank test comparing survival between exposure groups,
  properly accounting for time-dependent nature.

- performMantelByarTest:

  Perform Mantel-Byar test, which is specifically designed for comparing
  survival with time-dependent exposures.

- performTimeDepLRTest:

  Perform time-dependent log-rank test that allows for changing effects
  of exposure over time.

- handleTieBreaking:

  Method for handling tied event times in Cox regression.

- robustVariance:

  Use robust (sandwich) variance estimation for Cox regression to
  account for potential model misspecification.

- clusterVariable:

  Variable identifying clusters for robust variance estimation (e.g.,
  hospital, physician, family).

- showSurvivalEstimates:

  Display table with survival estimates at key time points, stratified
  by time-dependent exposure status.

- showHazardRatios:

  Display hazard ratios from time-dependent Cox regression with
  confidence intervals and p-values.

- showLandmarkResults:

  Display detailed results from landmark analysis at each specified time
  point.

- showExposurePatterns:

  Display summary of exposure patterns, including time to exposure,
  exposure duration, and switching patterns.

- showModelDiagnostics:

  Display diagnostic plots and statistics for time-dependent Cox
  regression models.

- performBootstrapValidation:

  Perform bootstrap validation to assess stability of results and
  provide bias-corrected estimates.

- bootstrapSamples:

  Number of bootstrap samples for validation analysis.

- performSensitivityAnalysis:

  Perform sensitivity analysis with different assumptions about exposure
  timing and immortal time handling.

- showExplanations:

  Include detailed explanations for Simon-Makuch methodology and
  interpretation of results.

- showMethodologyNotes:

  Display detailed notes on the statistical methods used and their
  appropriate interpretation.

- includeClinicalGuidance:

  Include guidance for clinical interpretation of time-dependent
  survival analysis results.

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$welcomeMessage`              |     |     |     |     | a html   |
| `results$simonMakuchExplanation`      |     |     |     |     | a html   |
| `results$exposurePatterns`            |     |     |     |     | a table  |
| `results$survivalEstimates`           |     |     |     |     | a table  |
| `results$timeDependentCox`            |     |     |     |     | a table  |
| `results$hazardRatios`                |     |     |     |     | a table  |
| `results$statisticalTests`            |     |     |     |     | a table  |
| `results$landmarkResults`             |     |     |     |     | a table  |
| `results$immortalTimeBias`            |     |     |     |     | a table  |
| `results$timeVaryingEffects`          |     |     |     |     | a table  |
| `results$modelDiagnostics`            |     |     |     |     | a table  |
| `results$bootstrapValidation`         |     |     |     |     | a table  |
| `results$sensitivityAnalysis`         |     |     |     |     | a table  |
| `results$landmarkAnalysisExplanation` |     |     |     |     | a html   |
| `results$immortalTimeBiasExplanation` |     |     |     |     | a html   |
| `results$timeDependentCoxExplanation` |     |     |     |     | a html   |
| `results$clinicalGuidance`            |     |     |     |     | a html   |
| `results$methodologyNotes`            |     |     |     |     | a html   |
| `results$simonMakuchPlotExplanation`  |     |     |     |     | a html   |
| `results$simonMakuchPlot`             |     |     |     |     | an image |
| `results$landmarkPlots`               |     |     |     |     | an image |
| `results$cumulativeIncidencePlot`     |     |     |     |     | an image |
| `results$exposureStatusPlot`          |     |     |     |     | an image |
| `results$diagnosticPlots`             |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$exposurePatterns$asDF`

`as.data.frame(results$exposurePatterns)`
