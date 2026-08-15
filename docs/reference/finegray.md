# Fine-Gray Competing Risks Regression

Fine-Gray subdistribution hazard regression for competing risks data.
Models the cumulative incidence function (CIF) while accounting for
competing events. Provides sub-hazard ratios (sHR) for covariate
effects.

## Usage

``` r
finegray(
  data,
  survivalTime = NULL,
  status = NULL,
  eventOfInterest,
  censorLevel,
  covariates = NULL,
  groupVar = NULL,
  showCoefficientTable = TRUE,
  exponentiate = TRUE,
  confLevel = 0.95,
  showGrayTest = TRUE,
  showModelFit = TRUE,
  showCIFPlot = TRUE,
  cifPlotBy = "group",
  cifPlotTimes = "",
  showRiskTable = TRUE,
  colorScheme = "default",
  cifConfInt = FALSE,
  cifConfLevel = 0.95,
  predictAt = "",
  predictCovariatePattern = "mean",
  customCovariateValues = "",
  showInterpretation = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- survivalTime:

  Time to event or censoring. Must be numeric and positive.

- status:

  Event status with multiple levels: 0 or first level = censored, 1 or
  second level = event of interest, 2+ = competing events.

- eventOfInterest:

  Which level of status represents the event of interest. Other
  non-censored levels are treated as competing events.

- censorLevel:

  Which level of status represents censoring.

- covariates:

  Predictor variables for the regression model. Can include continuous
  and categorical variables.

- groupVar:

  Optional grouping variable for cumulative incidence plots.

- showCoefficientTable:

  Display table of sub-hazard ratios with confidence intervals.

- exponentiate:

  Report exponentiated coefficients as sub-hazard ratios (sHR). If
  false, reports log sub-hazard ratios.

- confLevel:

  Confidence level for confidence intervals.

- showGrayTest:

  Perform Gray's test for comparing cumulative incidence functions
  between groups (requires grouping variable).

- showModelFit:

  Display model fit statistics (pseudo-R², AIC, BIC).

- showCIFPlot:

  Display cumulative incidence function plot.

- cifPlotBy:

  How to display the CIF plot.

- cifPlotTimes:

  Comma-separated time points for CIF plot risk table. If empty,
  automatically determined.

- showRiskTable:

  Display numbers at risk below CIF plot.

- colorScheme:

  Color palette for plots.

- cifConfInt:

  Display confidence bands around CIF curves.

- cifConfLevel:

  Confidence level for CIF confidence bands.

- predictAt:

  Comma-separated time points for predictions. Calculates predicted CIF
  at these times for covariate patterns.

- predictCovariatePattern:

  Covariate pattern for predictions.

- customCovariateValues:

  Custom covariate values for predictions (comma-separated). Format:
  "age=65,stage=III,treatment=chemo"

- showInterpretation:

  Display interpretation guidance for Fine-Gray results.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$procedureNotes` |  |  |  |  | a html |
| `results$modelInfo` |  |  |  |  | a table |
| `results$shrTable` |  |  |  |  | a table |
| `results$grayTestTable` |  |  |  |  | a table |
| `results$modelFitTable` |  |  |  |  | a table |
| `results$comparisonTable` |  |  |  |  | a table |
| `results$predictionTable` |  |  |  |  | a table |
| `results$cifPlot` |  |  |  |  | an image |
| `results$stackedCIFPlot` |  |  |  |  | an image |
| `results$kmvscifPlot` |  |  |  |  | Demonstrates bias in using 1-KM for competing risks |
| `results$causeSpecificPlot` |  |  |  |  | an image |
| `results$diagnosticPlots` |  |  |  |  | an image |
| `results$influenceTable` |  |  |  |  | a table |
| `results$interpretation` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelInfo$asDF`

`as.data.frame(results$modelInfo)`
