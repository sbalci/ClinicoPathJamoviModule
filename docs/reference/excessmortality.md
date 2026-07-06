# Excess Mortality Analysis

Excess Mortality Analysis

## Usage

``` r
excessmortality(
  data,
  timeVar,
  statusVar,
  ageVar,
  sexVar,
  covariates,
  expectedRate = "population",
  splineType = "bs",
  nknots = 4,
  degree = 3,
  confidenceLevel = 95,
  plotHazard = TRUE,
  plotSurvival = TRUE,
  plotCumHazard = FALSE,
  timePoints = "1,2,3,5,10"
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

- covariates:

  .

- expectedRate:

  .

- splineType:

  .

- nknots:

  .

- degree:

  .

- confidenceLevel:

  .

- plotHazard:

  .

- plotSurvival:

  .

- plotCumHazard:

  .

- timePoints:

  .

## Value

A results object containing:

|                         |     |     |     |     |          |
|-------------------------|-----|-----|-----|-----|----------|
| `results$instructions`  |     |     |     |     | a html   |
| `results$modelSummary`  |     |     |     |     | a table  |
| `results$coefficients`  |     |     |     |     | a table  |
| `results$predictions`   |     |     |     |     | a table  |
| `results$goodnessOfFit` |     |     |     |     | a table  |
| `results$hazardPlot`    |     |     |     |     | an image |
| `results$survivalPlot`  |     |     |     |     | an image |
| `results$cumHazardPlot` |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
