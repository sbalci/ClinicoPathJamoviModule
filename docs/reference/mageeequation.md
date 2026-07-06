# Magee Equation - Oncotype DX Estimator

Implements the Magee Equations (Klein et al. 2013, PMID: 23503643) for
estimating Oncotype DX Recurrence Score from standard pathology
variables. Provides all four published equations (Original + 3 new) with
risk category classification and concordance analysis.

## Usage

``` r
mageeequation(
  data,
  nuclearGrade,
  mitosis,
  erHscore,
  prHscore,
  her2Status,
  her2Positive,
  nottinghamScore = NULL,
  tumorSize = NULL,
  ki67 = NULL,
  actualRS = NULL,
  erFormat = "hscore",
  prFormat = "hscore",
  equations = "all",
  lowCutoff = 15,
  highCutoff = 26,
  showIndividual = TRUE,
  showConcordance = TRUE,
  showDistribution = TRUE
)
```

## Arguments

- data:

  .

- nuclearGrade:

  .

- mitosis:

  .

- erHscore:

  .

- prHscore:

  .

- her2Status:

  .

- her2Positive:

  .

- nottinghamScore:

  .

- tumorSize:

  .

- ki67:

  .

- actualRS:

  .

- erFormat:

  .

- prFormat:

  .

- equations:

  .

- lowCutoff:

  .

- highCutoff:

  .

- showIndividual:

  .

- showConcordance:

  .

- showDistribution:

  .

## Value

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$todo`             |     |     |     |     | a html   |
| `results$notices`          |     |     |     |     | a html   |
| `results$equationInfo`     |     |     |     |     | a html   |
| `results$summaryTable`     |     |     |     |     | a table  |
| `results$riskTable`        |     |     |     |     | a table  |
| `results$concordanceTable` |     |     |     |     | a table  |
| `results$individualTable`  |     |     |     |     | a table  |
| `results$distributionPlot` |     |     |     |     | an image |
| `results$concordancePlot`  |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`
