# Tumor Growth Models

Tumor Growth Models

## Usage

``` r
tumorgrowth(
  data,
  time,
  tumorSize,
  patientId = NULL,
  covariates = list(),
  growthModel = "gompertz",
  treatmentEffect = NULL,
  treatmentTime = NULL,
  modelApproach = "nlme",
  confidenceLevel = 95,
  modelSummary = TRUE,
  growthParameters = TRUE,
  showSummary = TRUE,
  growthCurves = TRUE,
  residualAnalysis = FALSE,
  treatmentAnalysis = FALSE,
  predictions = FALSE,
  doubleTime = TRUE,
  initialSize,
  maxSize,
  predictionTime = 30,
  mcmcSamples = 5000,
  plotWidth = 600,
  plotHeight = 450
)
```

## Arguments

- data:

  the data as a data frame

- time:

  Time variable (days, weeks, months) from baseline

- tumorSize:

  Tumor size measurement (volume, diameter, etc.)

- patientId:

  Patient identifier for longitudinal data

- covariates:

  Covariates affecting tumor growth

- growthModel:

  Mathematical model for tumor growth kinetics

- treatmentEffect:

  Variable indicating treatment status or type

- treatmentTime:

  Time when treatment was initiated

- modelApproach:

  Statistical approach for parameter estimation

- confidenceLevel:

  Confidence level for parameter intervals

- modelSummary:

  Display model parameter estimates and fit statistics

- growthParameters:

  Show biological interpretation of growth parameters

- showSummary:

  Display plain-English summary of growth model results

- growthCurves:

  Display individual and population growth curves

- residualAnalysis:

  Perform residual diagnostics

- treatmentAnalysis:

  Analyze treatment effects on growth parameters

- predictions:

  Generate future growth predictions

- doubleTime:

  Calculate tumor doubling time

- initialSize:

  Initial tumor size (if not measured)

- maxSize:

  Theoretical maximum size for logistic models

- predictionTime:

  Time units ahead for growth predictions

- mcmcSamples:

  Number of MCMC samples for Bayesian estimation

- plotWidth:

  Width of the growth plots in pixels. Note: Due to jamovi framework
  limitations, plot dimensions are currently fixed in the output schema
  and this option does not affect rendering. Future versions may support
  dynamic plot sizing.

- plotHeight:

  Height of the growth plots in pixels. Note: Due to jamovi framework
  limitations, plot dimensions are currently fixed in the output schema
  and this option does not affect rendering. Future versions may support
  dynamic plot sizing.

## Value

A results object containing:

|                                  |     |     |     |     |                |
|----------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                |     |     |     |     | a preformatted |
| `results$todo`                   |     |     |     |     | a html         |
| `results$summary`                |     |     |     |     | a html         |
| `results$naturalLanguageSummary` |     |     |     |     | a html         |
| `results$modelTable`             |     |     |     |     | a table        |
| `results$growthParametersTable`  |     |     |     |     | a table        |
| `results$doublingTimeTable`      |     |     |     |     | a table        |
| `results$treatmentEffectTable`   |     |     |     |     | a table        |
| `results$growthCurvesPlot`       |     |     |     |     | an image       |
| `results$residualPlot`           |     |     |     |     | an image       |
| `results$predictionPlot`         |     |     |     |     | an image       |
| `results$fitStatistics`          |     |     |     |     | a table        |
| `results$clinicalInterpretation` |     |     |     |     | a html         |
| `results$glossary`               |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelTable$asDF`

`as.data.frame(results$modelTable)`
