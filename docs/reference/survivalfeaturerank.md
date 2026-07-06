# Survival Feature Ranking

Performs univariate survival analysis for multiple features to identify
potential prognostic factors. This analysis runs a separate Cox
proportional hazards model for each selected feature and ranks them by
statistical significance, hazard ratio, or concordance index. Useful for
biomarker screening, exploratory analysis, and feature selection before
building multivariable models.

## Usage

``` r
survivalfeaturerank(
  data,
  survtime = NULL,
  event = NULL,
  eventLevel,
  features = NULL,
  rankBy = "pvalue",
  showCI = TRUE,
  adjustPValues = FALSE,
  adjustMethod = "fdr",
  showForestPlot = TRUE,
  forestStyle = "standard",
  showTopKM = FALSE,
  topN = 5,
  kmLayout = "separate",
  endplot = 60,
  byplot = 12,
  risktable = FALSE,
  pplot = TRUE,
  showSummary = TRUE,
  alphaLevel = 0.05,
  showFullTable = TRUE
)
```

## Arguments

- data:

  The dataset to be analyzed, provided as a data frame.

- survtime:

  The numeric variable representing follow-up time until the event or
  censoring.

- event:

  The event indicator variable (e.g., death, recurrence). Should be
  coded as 1 = event, 0 = censored, or as a factor where one level
  represents the event.

- eventLevel:

  The level of the event variable that represents the event of interest.
  For numeric variables, typically "1". For factors, select the
  appropriate level.

- features:

  Variables to be tested individually for association with survival.
  Each variable will be tested in a separate univariate Cox model. Can
  include both categorical and continuous variables.

- rankBy:

  Criterion for ranking features: - pvalue: Sorts by statistical
  significance (smallest p-value first) - hazard: Sorts by effect size
  (hazard ratio furthest from 1) - cindex: Sorts by discriminative
  ability (highest C-index first)

- showCI:

  If true, displays 95 percent confidence intervals for hazard ratios in
  the results table.

- adjustPValues:

  If true, applies multiple testing correction to p-values. Recommended
  when testing many features to control false discovery rate.

- adjustMethod:

  Method for p-value adjustment. FDR (Benjamini-Hochberg) is recommended
  for exploratory analysis. Bonferroni is most conservative.

- showForestPlot:

  If true, generates a forest plot showing hazard ratios and confidence
  intervals for all features, sorted by the ranking criterion.

- forestStyle:

  Visual style for the forest plot.

- showTopKM:

  If true, generates Kaplan-Meier survival curves for the top-ranked
  features. Useful for visualizing the survival differences for the most
  important predictors.

- topN:

  Number of top-ranked features for which to generate Kaplan-Meier
  plots. Only used if "Show Kaplan-Meier Plots for Top Features" is
  enabled.

- kmLayout:

  Layout for multiple Kaplan-Meier plots. Separate shows one plot per
  feature, faceted shows all in a grid.

- endplot:

  Maximum follow-up time to display on Kaplan-Meier plots.

- byplot:

  Interval for time axis labels on plots.

- risktable:

  If true, displays the number at risk below Kaplan-Meier plots.

- pplot:

  If true, displays log-rank test p-values on Kaplan-Meier plots.

- showSummary:

  If true, displays summary statistics including total number of
  features tested, number of significant features, and interpretation
  guidance.

- alphaLevel:

  Significance level for highlighting significant features in the
  results table.

- showFullTable:

  If true, shows results for all tested features. If false, shows only
  statistically significant features.

## Value

A results object containing:

|                              |     |     |     |     |                |
|------------------------------|-----|-----|-----|-----|----------------|
| `results$instructions`       |     |     |     |     | a html         |
| `results$summaryText`        |     |     |     |     | a preformatted |
| `results$rankingTable`       |     |     |     |     | a table        |
| `results$forestPlot`         |     |     |     |     | an image       |
| `results$topFeaturesHeading` |     |     |     |     | a html         |
| `results$kmPlot1`            |     |     |     |     | an image       |
| `results$kmPlot2`            |     |     |     |     | an image       |
| `results$kmPlot3`            |     |     |     |     | an image       |
| `results$kmPlot4`            |     |     |     |     | an image       |
| `results$kmPlot5`            |     |     |     |     | an image       |
| `results$kmPlot6`            |     |     |     |     | an image       |
| `results$kmPlot7`            |     |     |     |     | an image       |
| `results$kmPlot8`            |     |     |     |     | an image       |
| `results$kmPlot9`            |     |     |     |     | an image       |
| `results$kmPlot10`           |     |     |     |     | an image       |
| `results$exportRanking`      |     |     |     |     | an output      |
| `results$interpretation`     |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$rankingTable$asDF`

`as.data.frame(results$rankingTable)`
