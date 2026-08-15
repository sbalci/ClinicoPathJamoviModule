# Treatment Switching Analysis

Treatment Switching Analysis

## Usage

``` r
treatmentswitching(
  data,
  time,
  event,
  treatment,
  switchingTime,
  actualTreatment,
  covariates = list(),
  switchingMethod = "ipcw",
  switchingDirection = "control_to_treatment",
  censoringAssumption = "common_treatment",
  confidenceLevel = 95,
  treatmentEffect = TRUE,
  switchingPatterns = TRUE,
  survivalCurves = TRUE,
  sensitivityAnalysis = FALSE,
  causalEstimate = TRUE,
  methodComparison = TRUE,
  bootstrapSamples = 1000,
  psModel = "",
  accelerationFactor,
  treatmentLag = 0,
  plotWidth = 700,
  plotHeight = 500
)
```

## Arguments

- data:

  the data as a data frame

- time:

  Time to event or censoring

- event:

  Event status (0 = censored, 1 = event)

- treatment:

  Original randomized treatment assignment

- switchingTime:

  Time when treatment switching occurred (NA if no switching)

- actualTreatment:

  Actual treatment received after any switching

- covariates:

  Baseline covariates for adjustment

- switchingMethod:

  Method for handling treatment switching

- switchingDirection:

  Direction of treatment switching allowed

- censoringAssumption:

  Assumptions for handling censoring in switching analysis

- confidenceLevel:

  Confidence level for interval estimation

- treatmentEffect:

  Display treatment effect estimates for different methods

- switchingPatterns:

  Summarize patterns of treatment switching

- survivalCurves:

  Display survival curves with switching adjustments

- sensitivityAnalysis:

  Perform sensitivity analysis for switching assumptions

- causalEstimate:

  Estimate causal treatment effects adjusting for switching

- methodComparison:

  Compare results across different switching methods

- bootstrapSamples:

  Number of bootstrap samples for uncertainty estimation

- psModel:

  Formula for propensity score model (for IPCW method)

- accelerationFactor:

  Acceleration factor for RPSFT method (estimated if not specified)

- treatmentLag:

  Lag time before treatment effect begins

- plotWidth:

  Width of survival plots in pixels

- plotHeight:

  Height of survival plots in pixels

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                   |     |     |     |     | a html   |
| `results$summary`                |     |     |     |     | a html   |
| `results$switchingPatternsTable` |     |     |     |     | a table  |
| `results$treatmentEffectTable`   |     |     |     |     | a table  |
| `results$causalEffectTable`      |     |     |     |     | a table  |
| `results$methodComparisonTable`  |     |     |     |     | a table  |
| `results$survivalCurvesPlot`     |     |     |     |     | an image |
| `results$switchingPatternsPlot`  |     |     |     |     | an image |
| `results$sensitivityPlot`        |     |     |     |     | an image |
| `results$diagnosticsTable`       |     |     |     |     | a table  |
| `results$sensitivityTable`       |     |     |     |     | a table  |
| `results$bootstrapTable`         |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$switchingPatternsTable$asDF`

`as.data.frame(results$switchingPatternsTable)`
