# Semi-Markov Models

Fits Semi-Markov models where transition intensities depend on time
since last transition rather than absolute time. Suitable for processes
where the 'clock' resets at each transition using SemiMarkov package.

## Usage

``` r
semimarkov(
  data,
  time,
  event,
  subject,
  covs,
  modelType = "parametric",
  transitionStructure = "progressive",
  distributionType = "weibull",
  estimationMethod = "ml",
  timeScale = "auto",
  predictionHorizons = "1, 3, 5, 10",
  timePoints = "0.5, 1, 2, 3, 5",
  conf = 0.95,
  maxIterations = 1000,
  tolerance = 1e-06,
  bootstrapSamples = 500,
  showTransitionRates = TRUE,
  showSojournDistribution = TRUE,
  showTransitionProbs = TRUE,
  showStateProbabilities = TRUE,
  showReliabilityAnalysis = FALSE,
  showModelDiagnostics = TRUE,
  showPredictions = TRUE,
  showEducational = TRUE,
  plotSojournDistributions = TRUE,
  plotTransitionIntensities = FALSE,
  plotStateProbabilities = TRUE,
  plotReliabilityFunctions = FALSE,
  plotModelDiagnostics = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Follow-up time variable

- event:

  Event/state variable indicating transitions between states

- subject:

  Subject identifier for longitudinal tracking

- covs:

  Covariates affecting transition intensities

- modelType:

  Type of Semi-Markov model to fit

- transitionStructure:

  Structure of allowed transitions between states

- distributionType:

  Distribution for sojourn times in each state

- estimationMethod:

  Method for parameter estimation

- timeScale:

  Time scale for analysis and reporting

- predictionHorizons:

  Comma-separated list of time horizons for predictions

- timePoints:

  Time points for transition probability estimation

- conf:

  Confidence level for confidence intervals

- maxIterations:

  Maximum iterations for convergence

- tolerance:

  Convergence tolerance for optimization

- bootstrapSamples:

  Number of bootstrap samples for confidence intervals

- showTransitionRates:

  Display estimated transition rates and parameters

- showSojournDistribution:

  Display sojourn time distribution parameters

- showTransitionProbs:

  Display transition probabilities over time

- showStateProbabilities:

  Display state occupation probabilities

- showReliabilityAnalysis:

  Display reliability and survival function analysis

- showModelDiagnostics:

  Display goodness-of-fit and diagnostic tests

- showPredictions:

  Display future state predictions

- showEducational:

  Display educational information about Semi-Markov models

- plotSojournDistributions:

  Display sojourn time distribution plots

- plotTransitionIntensities:

  Display transition intensity functions over time

- plotStateProbabilities:

  Display state occupation probability plots

- plotReliabilityFunctions:

  Display reliability and hazard function plots

- plotModelDiagnostics:

  Display diagnostic and goodness-of-fit plots

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                    |     |     |     |     | a html   |
| `results$educationalInfo`         |     |     |     |     | a html   |
| `results$modelSummary`            |     |     |     |     | a table  |
| `results$transitionRates`         |     |     |     |     | a table  |
| `results$sojournDistribution`     |     |     |     |     | a table  |
| `results$transitionProbabilities` |     |     |     |     | a table  |
| `results$stateProbabilities`      |     |     |     |     | a table  |
| `results$reliabilityAnalysis`     |     |     |     |     | a table  |
| `results$covariateEffects`        |     |     |     |     | a table  |
| `results$modelDiagnostics`        |     |     |     |     | a table  |
| `results$predictionsTable`        |     |     |     |     | a table  |
| `results$methodsInfo`             |     |     |     |     | a html   |
| `results$interpretationGuide`     |     |     |     |     | a html   |
| `results$sojournDistributionPlot` |     |     |     |     | an image |
| `results$stateProbabilityPlot`    |     |     |     |     | an image |
| `results$transitionIntensityPlot` |     |     |     |     | an image |
| `results$reliabilityPlot`         |     |     |     |     | an image |
| `results$diagnosticsPlot`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
