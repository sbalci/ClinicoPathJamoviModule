# Overall, Cause Specific, and Competing Survival

Overall, Cause Specific, and Competing Survival.

## Usage

``` r
competingsurvival(
  data,
  explanatory = NULL,
  overalltime,
  outcome,
  dod,
  dooc,
  awd,
  awod,
  analysistype = "overall",
  graystest = FALSE,
  subdistribution = FALSE,
  timepoints = "12,24,36,60",
  confidencelevel = 0.95,
  showrisksets = FALSE,
  showStackedPlot = FALSE,
  showKMvsCIF = FALSE,
  cifColors = "default"
)
```

## Arguments

- data:

  The data as a data frame.

- explanatory:

  .

- overalltime:

  .

- outcome:

  .

- dod:

  .

- dooc:

  .

- awd:

  .

- awod:

  .

- analysistype:

  .

- graystest:

  Perform Gray's test to compare cumulative incidence functions between
  groups

- subdistribution:

  Use Fine-Gray subdistribution hazard model for competing risks
  regression

- timepoints:

  Comma-separated time points (in months) for cumulative incidence
  estimates

- confidencelevel:

  Confidence level for hazard ratio confidence intervals

- showrisksets:

  Display number at risk table below cumulative incidence plot

- showStackedPlot:

  Display stacked probability plot with all competing events

- showKMvsCIF:

  Plot comparison between 1-KM and CIF to show competing risk bias

- cifColors:

  Color scheme for CIF plots

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$summary`        |     |     |     |     | a html   |
| `results$survivalTable`  |     |     |     |     | a table  |
| `results$cuminc`         |     |     |     |     | a table  |
| `results$comprisksPlot`  |     |     |     |     | an image |
| `results$stackedPlot`    |     |     |     |     | an image |
| `results$kmvscifPlot`    |     |     |     |     | an image |
| `results$interpretation` |     |     |     |     | a html   |
| `results$assumptions`    |     |     |     |     | a html   |
| `results$fineGrayTable`  |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$survivalTable$asDF`

`as.data.frame(results$survivalTable)`
