# Markov Multi-State Models

Fits Markov multi-state models for analyzing transitions between
different health states over time. Supports homogeneous and
non-homogeneous models with covariate effects using mstate and msm
packages.

## Usage

``` r
markovmultistate(
  data,
  time,
  event,
  subject,
  covs,
  modelType = "homogeneous",
  transitionMatrix = "progressive",
  baselineHazard = "exponential",
  estimationMethod = "ml",
  timeUnits = "auto",
  predictionTimes = "1, 3, 5",
  transitionTimes = "0.5, 1, 2, 3, 5",
  conf = 0.95,
  maxIterations = 1000,
  tolerance = 1e-06,
  showTransitionIntensities = TRUE,
  showTransitionProbs = TRUE,
  showStateProbabilities = TRUE,
  showMeanSojournTimes = TRUE,
  showExpectedRewards = FALSE,
  showModelFit = TRUE,
  showPredictions = TRUE,
  showEducational = TRUE,
  plotStateProbs = TRUE,
  plotTransitionProbs = FALSE,
  plotHazardRatios = FALSE,
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

  Type of multi-state model to fit

- transitionMatrix:

  Structure of allowed transitions between states

- baselineHazard:

  Baseline hazard function specification

- estimationMethod:

  Method for parameter estimation

- timeUnits:

  Time scale for analysis and reporting

- predictionTimes:

  Comma-separated list of time points for state predictions

- transitionTimes:

  Time points for transition probability estimation

- conf:

  Confidence level for confidence intervals

- maxIterations:

  Maximum iterations for model fitting

- tolerance:

  Convergence tolerance for optimization

- showTransitionIntensities:

  Display estimated transition intensity matrix

- showTransitionProbs:

  Display transition probability matrices at specified times

- showStateProbabilities:

  Display state occupation probabilities over time

- showMeanSojournTimes:

  Display expected time spent in each state

- showExpectedRewards:

  Display expected rewards/utilities for health economics

- showModelFit:

  Display model fit statistics and diagnostics

- showPredictions:

  Display individual and population-level predictions

- showEducational:

  Display educational information about multi-state models

- plotStateProbs:

  Display state probability plots over time

- plotTransitionProbs:

  Display transition probability heatmaps

- plotHazardRatios:

  Display hazard ratio plots for covariates

- plotModelDiagnostics:

  Display model diagnostic plots and residuals

## Value

A results object containing:

|                                     |     |     |     |     |          |
|-------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                      |     |     |     |     | a html   |
| `results$educationalInfo`           |     |     |     |     | a html   |
| `results$modelSummary`              |     |     |     |     | a table  |
| `results$transitionIntensities`     |     |     |     |     | a table  |
| `results$transitionProbabilities`   |     |     |     |     | a table  |
| `results$stateProbabilities`        |     |     |     |     | a table  |
| `results$meanSojournTimes`          |     |     |     |     | a table  |
| `results$covariateEffects`          |     |     |     |     | a table  |
| `results$modelFitStatistics`        |     |     |     |     | a table  |
| `results$predictionsTable`          |     |     |     |     | a table  |
| `results$methodsInfo`               |     |     |     |     | a html   |
| `results$interpretationGuide`       |     |     |     |     | a html   |
| `results$stateTransitionDiagram`    |     |     |     |     | an image |
| `results$stateProbabilityPlot`      |     |     |     |     | an image |
| `results$transitionProbabilityPlot` |     |     |     |     | an image |
| `results$hazardRatioPlot`           |     |     |     |     | an image |
| `results$diagnosticsPlot`           |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
