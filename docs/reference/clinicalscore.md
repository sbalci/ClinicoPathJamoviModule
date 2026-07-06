# Clinical Scoring System Generator

Builds clinical integer-point scoring systems from regression models
using three published methods (Sullivan/D'Agostino, Schneeweiss,
Beta10). Supports logistic, Cox, linear, and ordinal regression with
automatic variable categorization, calibration, discrimination, nomogram
generation, and TRIPOD compliance.

## Usage

``` r
clinicalscore(
  data,
  modelType = "logistic",
  outcome,
  outcomeLevel,
  elapsedtime = NULL,
  explanatory,
  autoCategorize = TRUE,
  categorizeMethod = "median",
  customBreaks = "",
  scoringMethod = "schneeweiss",
  maxPoints = 10,
  bootstrapValidation = FALSE,
  bootstrapN = 200,
  scoreLookup = TRUE,
  showNomogram = TRUE,
  showCalibration = TRUE,
  calibrationGroups = 4,
  showDiscrimination = TRUE,
  showDecisionCurve = FALSE,
  showTRIPOD = FALSE,
  suitabilityCheck = TRUE,
  showSummary = FALSE,
  showExplanations = FALSE,
  random_seed = 42
)
```

## Arguments

- data:

  .

- modelType:

  .

- outcome:

  .

- outcomeLevel:

  .

- elapsedtime:

  .

- explanatory:

  .

- autoCategorize:

  Automatically convert continuous predictors into categories for
  integer-point assignment. If disabled, continuous predictors
  contribute points based on above/below median.

- categorizeMethod:

  .

- customBreaks:

  Comma-separated cutpoints for manual categorization. Example:
  "3,10,20" for Ki-67 would create G1/G2a/G2b/G3.

- scoringMethod:

  .

- maxPoints:

  .

- bootstrapValidation:

  .

- bootstrapN:

  .

- scoreLookup:

  .

- showNomogram:

  .

- showCalibration:

  .

- calibrationGroups:

  .

- showDiscrimination:

  .

- showDecisionCurve:

  .

- showTRIPOD:

  .

- suitabilityCheck:

  .

- showSummary:

  .

- showExplanations:

  .

- random_seed:

  .

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$notices`             |     |     |     |     | a html   |
| `results$suitabilityReport`   |     |     |     |     | a html   |
| `results$modelSummary`        |     |     |     |     | a table  |
| `results$coefficients`        |     |     |     |     | a table  |
| `results$scoringTable`        |     |     |     |     | a table  |
| `results$methodComparison`    |     |     |     |     | a table  |
| `results$scoringPerformance`  |     |     |     |     | a table  |
| `results$lookupTable`         |     |     |     |     | a table  |
| `results$discriminationTable` |     |     |     |     | a table  |
| `results$validationTable`     |     |     |     |     | a table  |
| `results$calibrationPlot`     |     |     |     |     | an image |
| `results$nomogramPlot`        |     |     |     |     | an image |
| `results$scoreDistPlot`       |     |     |     |     | an image |
| `results$decisionCurveTable`  |     |     |     |     | a table  |
| `results$decisionCurvePlot`   |     |     |     |     | an image |
| `results$tripodChecklist`     |     |     |     |     | a html   |
| `results$summaryText`         |     |     |     |     | a html   |
| `results$explanations`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
