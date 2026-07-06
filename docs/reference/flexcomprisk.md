# Flexible Competing Risks Models

Performs flexible competing risks analysis using advanced regression
techniques including spline-based hazards, time-varying effects, and
robust prediction models with comprehensive model validation.

## Usage

``` r
flexcomprisk(
  data,
  time,
  event,
  covs,
  eventOfInterest = 1,
  modelType = "splines",
  splineType = "rcs",
  splineDf = 4,
  timeInteractions,
  validationType = "cv",
  cvFolds = 5,
  bootstrapSamples = 500,
  predictionTimes = "1, 3, 5",
  conf = 0.95,
  showModelComparison = TRUE,
  showValidationResults = TRUE,
  showPredictions = TRUE,
  showEducational = TRUE,
  plotCumulativeIncidence = TRUE,
  plotModelComparison = FALSE,
  plotValidation = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Follow-up time variable

- event:

  Event type variable (0=censored, 1=event of interest, 2=competing
  event, etc.)

- covs:

  Covariates to include in the model

- eventOfInterest:

  Numeric code for the event of interest (primary event)

- modelType:

  Type of flexible competing risks model to fit

- splineType:

  Type of splines for flexible parametric models

- splineDf:

  Degrees of freedom for spline functions

- timeInteractions:

  Variables with time-varying effects (interactions with time)

- validationType:

  Method for model validation and performance assessment

- cvFolds:

  Number of folds for cross-validation

- bootstrapSamples:

  Number of bootstrap samples for validation

- predictionTimes:

  Comma-separated list of time points for prediction

- conf:

  Confidence level for confidence intervals

- showModelComparison:

  Compare performance across different model types

- showValidationResults:

  Display cross-validation and bootstrap results

- showPredictions:

  Display predicted cumulative incidence at specified time points

- showEducational:

  Display educational information about flexible competing risks models

- plotCumulativeIncidence:

  Display cumulative incidence function plots

- plotModelComparison:

  Display performance comparison plots across models

- plotValidation:

  Display validation and calibration plots

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                 |     |     |     |     | a html   |
| `results$educationalInfo`      |     |     |     |     | a html   |
| `results$modelSummary`         |     |     |     |     | a table  |
| `results$coefficientsTable`    |     |     |     |     | a table  |
| `results$predictionsTable`     |     |     |     |     | a table  |
| `results$modelComparisonTable` |     |     |     |     | a table  |
| `results$validationResults`    |     |     |     |     | a table  |
| `results$timeVaryingEffects`   |     |     |     |     | a table  |
| `results$methodsInfo`          |     |     |     |     | a html   |
| `results$recommendationsInfo`  |     |     |     |     | a html   |
| `results$cifPlot`              |     |     |     |     | an image |
| `results$modelComparisonPlot`  |     |     |     |     | an image |
| `results$validationPlot`       |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
