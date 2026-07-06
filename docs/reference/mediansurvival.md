# Median Survival Comparisons

Performs statistical comparisons of median survival times between
groups. Provides confidence intervals, hypothesis tests, and robust
estimators for median survival with handling of censored data and group
comparisons.

## Usage

``` r
mediansurvival(
  data,
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  confidence_method = "brookmeyer_crowley",
  confidence_level = 0.95,
  test_method = "logrank",
  multiple_comparison = "holm",
  show_survival_plot = TRUE,
  show_median_lines = TRUE,
  show_confidence_bands = TRUE,
  show_risk_table = FALSE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  Time variable for survival analysis.

- outcome:

  Event indicator variable.

- explanatory:

  Grouping variable for comparison.

- outcomeLevel:

  Level of outcome variable indicating event.

- confidence_method:

  Method for constructing confidence intervals for median survival.

- confidence_level:

  Confidence level for median survival intervals.

- test_method:

  Statistical test method for comparing median survival times.

- multiple_comparison:

  Method for multiple comparison adjustment.

- show_survival_plot:

  Display Kaplan-Meier survival curves with median indicators.

- show_median_lines:

  Add median survival lines to survival plot.

- show_confidence_bands:

  Display confidence bands around survival curves.

- show_risk_table:

  Include numbers at risk table below survival plot.

- showSummaries:

  Generate natural language summaries.

- showExplanations:

  Show methodology explanations.

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                 |     |     |     |     | a html   |
| `results$medianTable`          |     |     |     |     | a table  |
| `results$comparisonTable`      |     |     |     |     | a table  |
| `results$survivalPlot`         |     |     |     |     | an image |
| `results$medianComparisonPlot` |     |     |     |     | an image |
| `results$analysisSummary`      |     |     |     |     | a html   |
| `results$methodExplanation`    |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$medianTable$asDF`

`as.data.frame(results$medianTable)`
