# Weighted Log-Rank Tests

Performs weighted log-rank tests for comparing survival distributions
between groups. These tests provide enhanced power for detecting
differences at specific time points using various weighting schemes.
Includes Gehan-Wilcoxon, Tarone-Ware, Peto-Peto, and custom weight
function approaches for comprehensive survival comparison.

## Usage

``` r
weightedlogrank(
  data,
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  gehan_wilcoxon = TRUE,
  tarone_ware = TRUE,
  peto_peto = TRUE,
  modified_peto = FALSE,
  standard_logrank = TRUE,
  multiple_comparison = "holm",
  confidence_level = 0.95,
  show_kaplan_meier = TRUE,
  show_weight_functions = FALSE,
  show_test_statistics = FALSE,
  show_sample_sizes = TRUE,
  show_events = TRUE,
  median_survival = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  Time variable for survival analysis

- outcome:

  Event indicator variable

- explanatory:

  Grouping variable for comparison

- outcomeLevel:

  Level of outcome variable indicating event

- gehan_wilcoxon:

  Perform Gehan-Wilcoxon weighted test

- tarone_ware:

  Perform Tarone-Ware weighted test

- peto_peto:

  Perform Peto-Peto weighted test

- modified_peto:

  Perform Modified Peto weighted test

- standard_logrank:

  Include standard unweighted log-rank test

- multiple_comparison:

  Method for multiple comparison adjustment

- confidence_level:

  Confidence level for intervals

- show_kaplan_meier:

  Display Kaplan-Meier survival curves

- show_weight_functions:

  Display weight function plots

- show_test_statistics:

  Plot test statistics over time

- show_sample_sizes:

  Display sample sizes by group

- show_events:

  Display event counts by group

- median_survival:

  Display median survival times by group

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show methodology explanations

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$groupSummary`        |     |     |     |     | a html   |
| `results$weightedTestsTable`  |     |     |     |     | a table  |
| `results$survivalPlot`        |     |     |     |     | an image |
| `results$weightFunctionsPlot` |     |     |     |     | an image |
| `results$testStatisticsPlot`  |     |     |     |     | an image |
| `results$analysisSummary`     |     |     |     |     | a html   |
| `results$methodExplanation`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$weightedTestsTable$asDF`

`as.data.frame(results$weightedTestsTable)`
