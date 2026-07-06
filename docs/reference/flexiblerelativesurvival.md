# Flexible Relative Survival Analysis

Flexible Relative Survival Analysis

## Usage

``` r
flexiblerelativesurvival(
  data,
  timeVar,
  statusVar,
  ageVar,
  sexVar,
  yearVar,
  covariates,
  expectedType = "lifetable",
  smoothingMethod = "splines",
  knotsTime = 5,
  knotsAge = 3,
  smoothingParameter = 1,
  confidenceLevel = 95,
  plotRelativeSurvival = TRUE,
  plotExcessHazard = TRUE,
  plotLifeExpectancy = FALSE,
  predictTimePoints = "1,2,3,5,10",
  standardizeAge = TRUE
)
```

## Arguments

- data:

  .

- timeVar:

  .

- statusVar:

  .

- ageVar:

  .

- sexVar:

  .

- yearVar:

  .

- covariates:

  .

- expectedType:

  .

- smoothingMethod:

  .

- knotsTime:

  .

- knotsAge:

  .

- smoothingParameter:

  .

- confidenceLevel:

  .

- plotRelativeSurvival:

  .

- plotExcessHazard:

  .

- plotLifeExpectancy:

  .

- predictTimePoints:

  .

- standardizeAge:

  .

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`         |     |     |     |     | a html   |
| `results$modelSummary`         |     |     |     |     | a table  |
| `results$coefficients`         |     |     |     |     | a table  |
| `results$relativeSurvival`     |     |     |     |     | a table  |
| `results$smoothingInfo`        |     |     |     |     | a table  |
| `results$goodnessOfFit`        |     |     |     |     | a table  |
| `results$relativeSurvivalPlot` |     |     |     |     | an image |
| `results$excessHazardPlot`     |     |     |     |     | an image |
| `results$lifeExpectancyPlot`   |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
