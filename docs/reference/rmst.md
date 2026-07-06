# Restricted Mean Survival Time Tests

Performs restricted mean survival time (RMST) analysis for comparing
survival distributions between groups. RMST provides a clinically
meaningful measure of survival benefit by estimating the area under the
survival curve up to a specific time point (tau), representing the
average survival time within the restriction period.

## Usage

``` r
rmst(
  data,
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  tau_method = "auto",
  tau_value = 365,
  tau_percentile = 80,
  confidence_level = 0.95,
  bootstrap_ci = FALSE,
  bootstrap_n = 1000,
  show_rmst_table = TRUE,
  show_rmst_plot = TRUE,
  show_difference_test = TRUE,
  show_ratio_test = FALSE,
  show_tau_analysis = FALSE,
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

  Level of the outcome variable that represents the event of interest.

- tau_method:

  Method for selecting the restriction time tau.

- tau_value:

  Manual specification of restriction time tau (used when method is
  'manual').

- tau_percentile:

  Percentile of maximum follow-up time to use as tau (used when method
  is 'percentile').

- confidence_level:

  Confidence level for all confidence intervals.

- bootstrap_ci:

  Use bootstrap method for confidence intervals (more robust for small
  samples).

- bootstrap_n:

  Number of bootstrap iterations for confidence interval estimation.

- show_rmst_table:

  Display table with RMST estimates and confidence intervals by group.

- show_rmst_plot:

  Display survival curves with highlighted RMST areas.

- show_difference_test:

  Perform statistical test for difference in RMST between groups.

- show_ratio_test:

  Perform statistical test for ratio of RMST between groups.

- show_tau_analysis:

  Display sensitivity analysis across different tau values.

- showSummaries:

  Generate natural language summary of results.

- showExplanations:

  Show detailed methodology explanations.

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$rmstTable`         |     |     |     |     | a table  |
| `results$comparisonTable`   |     |     |     |     | a table  |
| `results$rmstPlot`          |     |     |     |     | an image |
| `results$tauAnalysisPlot`   |     |     |     |     | an image |
| `results$tauAnalysisTable`  |     |     |     |     | a table  |
| `results$analysisSummary`   |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$rmstTable$asDF`

`as.data.frame(results$rmstTable)`
