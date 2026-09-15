# Prediction Model Builder

Build and validate prediction models for medical decision making.
Creates multiple logistic regression models with predicted probabilities
that can be directly used in Decision Curve Analysis.

## Usage

``` r
modelbuilder(
  data,
  clinicalPreset = "none",
  outcome,
  outcomePositive,
  splitData = FALSE,
  randomSeed = 123,
  buildBasicModel = TRUE,
  basicPredictors,
  basicModelName = "basic_model",
  buildEnhancedModel = FALSE,
  enhancedPredictors,
  enhancedModelName = "enhanced_model",
  buildBiomarkerModel = FALSE,
  biomarkerPredictors,
  biomarkerModelName = "biomarker_model",
  buildCustomModel = FALSE,
  customPredictors,
  customModelName = "custom_model",
  includeInteractions = FALSE,
  interactionTerms = "",
  useStepwise = FALSE,
  stepwiseDirection = "both",
  selectionCriterion = "aic",
  transformVariables = FALSE,
  transformMethod = "log",
  missingDataMethod = "complete_cases",
  imputationSets = 5,
  crossValidation = FALSE,
  cvFolds = 5,
  bootstrapValidation = FALSE,
  bootstrapReps = 1000,
  showModelSummary = TRUE,
  showPerformanceMetrics = TRUE,
  showCalibrationPlots = FALSE,
  showROCCurves = FALSE,
  compareModels = FALSE,
  createPredictions = FALSE,
  exportForDCA = FALSE,
  calculateNRI = FALSE,
  nriThresholds = "0.05, 0.10, 0.20",
  calculateIDI = FALSE,
  penalizedRegression = FALSE,
  penaltyType = "lasso",
  generateRiskScore = FALSE,
  riskScorePoints = "simple"
)
```

## Arguments

- data:

  The data as a data frame.

- clinicalPreset:

  Predefined clinical scenarios with optimized settings and variable
  recommendations.

- outcome:

  Binary outcome variable to predict (e.g., disease presence, adverse
  event occurrence).

- outcomePositive:

  Which level of the outcome variable represents the positive case.

- splitData:

  Split data into training (70 percent) and validation (30 percent) sets
  for unbiased model evaluation.

- randomSeed:

  Random seed for reproducible data splitting and results.

- buildBasicModel:

  Build a basic clinical model using demographic and primary risk
  factors.

- basicPredictors:

  Variables for the basic clinical model (e.g., age, sex, primary risk
  factors).

- basicModelName:

  Name for the basic model (used as column name for predictions).

- buildEnhancedModel:

  Build an enhanced model with additional clinical variables.

- enhancedPredictors:

  Variables for the enhanced model (should include basic predictors plus
  additional ones).

- enhancedModelName:

  Name for the enhanced model (used as column name for predictions).

- buildBiomarkerModel:

  Build a model incorporating biomarkers or advanced diagnostics.

- biomarkerPredictors:

  Variables for the biomarker model (clinical variables plus
  biomarkers).

- biomarkerModelName:

  Name for the biomarker model (used as column name for predictions).

- buildCustomModel:

  Build a custom model with user-specified variables.

- customPredictors:

  Variables for the custom model.

- customModelName:

  Name for the custom model (used as column name for predictions).

- includeInteractions:

  Include two-way interactions between predictor variables.

- interactionTerms:

  Specify interactions (e.g., "age*sex, diabetes*smoking"). Leave empty
  for all pairwise.

- useStepwise:

  Use stepwise variable selection to optimize models.

- stepwiseDirection:

  Direction for stepwise variable selection.

- selectionCriterion:

  Criterion for stepwise variable selection.

- transformVariables:

  Apply automatic transformations to continuous variables.

- transformMethod:

  Type of transformation to apply to continuous variables.

- missingDataMethod:

  Method for handling missing data in predictor variables.

- imputationSets:

  Number of imputation sets for multiple imputation.

- crossValidation:

  Perform k-fold cross-validation for robust model assessment.

- cvFolds:

  Number of folds for cross-validation.

- bootstrapValidation:

  Perform bootstrap validation to assess optimism and shrinkage.

- bootstrapReps:

  Number of bootstrap replications for validation.

- showModelSummary:

  Display regression coefficients and model statistics.

- showPerformanceMetrics:

  Display AUC, calibration, and other performance measures.

- showCalibrationPlots:

  Display calibration plots for each model.

- showROCCurves:

  Display ROC curves for discrimination assessment.

- compareModels:

  Display side-by-side comparison of all models.

- createPredictions:

  Add predicted probability columns to dataset for DCA use.

- exportForDCA:

  Format output specifically for use in Decision Curve Analysis module.

- calculateNRI:

  Calculate NRI comparing enhanced models to basic model.

- nriThresholds:

  Risk thresholds for NRI calculation (comma-separated).

- calculateIDI:

  Calculate IDI comparing models.

- penalizedRegression:

  Use penalized regression (LASSO/Ridge) for variable selection.

- penaltyType:

  Type of penalty for regularized regression.

- generateRiskScore:

  Create integer risk score from best model for clinical use.

- riskScorePoints:

  Method for creating clinical risk score.

## Value

A results object containing:

|                                 |     |     |     |     |                    |
|---------------------------------|-----|-----|-----|-----|--------------------|
| `results$instructions`          |     |     |     |     | a html             |
| `results$clinicalGuidance`      |     |     |     |     | a html             |
| `results$dataSummary`           |     |     |     |     | a html             |
| `results$clinicalSummary`       |     |     |     |     | a html             |
| `results$glossary`              |     |     |     |     | a html             |
| `results$dcaReadyMessage`       |     |     |     |     | a html             |
| `results$reportSentences`       |     |     |     |     | a html             |
| `results$exportOptions`         |     |     |     |     | a html             |
| `results$basicModelSummary`     |     |     |     |     | a table            |
| `results$enhancedModelSummary`  |     |     |     |     | a table            |
| `results$biomarkerModelSummary` |     |     |     |     | a table            |
| `results$customModelSummary`    |     |     |     |     | a table            |
| `results$modelComparisonTable`  |     |     |     |     | a table            |
| `results$validationResults`     |     |     |     |     | a table            |
| `results$nriTable`              |     |     |     |     | a table            |
| `results$idiTable`              |     |     |     |     | a table            |
| `results$riskScoreTable`        |     |     |     |     | a table            |
| `results$rocCurvesPlot`         |     |     |     |     | an image           |
| `results$calibrationPlotsArray` |     |     |     |     | an array of images |
| `results$modelComparisonPlot`   |     |     |     |     | an image           |
| `results$validationPlot`        |     |     |     |     | an image           |
| `results$dcaPreparationSummary` |     |     |     |     | a html             |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$basicModelSummary$asDF`

`as.data.frame(results$basicModelSummary)`
