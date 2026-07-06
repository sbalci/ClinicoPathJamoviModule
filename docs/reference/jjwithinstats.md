# Box-Violin Plots to Compare Within Groups

Wrapper Function for ggstatsplot::ggwithinstats and
ggstatsplot::grouped_ggwithinstats to generate violin plots for repeated
measurements and within-subjects analysis with statistical annotations
and significance testing.

## Usage

``` r
jjwithinstats(
  data,
  dep1,
  dep2,
  dep3 = NULL,
  dep4 = NULL,
  pointpath = FALSE,
  centralitypath = FALSE,
  centralityplotting = FALSE,
  centralitytype = "parametric",
  clinicalpreset = "custom",
  typestatistics = "parametric",
  pairwisecomparisons = FALSE,
  pairwisedisplay = "significant",
  padjustmethod = "holm",
  effsizetype = "biased",
  violin = TRUE,
  boxplot = TRUE,
  point = TRUE,
  mytitle = "Within Group Comparison",
  xtitle = "",
  ytitle = "",
  originaltheme = FALSE,
  resultssubtitle = FALSE,
  bfmessage = FALSE,
  conflevel = 0.95,
  k = 2,
  plotwidth = 650,
  plotheight = 450,
  addGGPubrPlot = FALSE,
  ggpubrPlotType = "boxplot",
  ggpubrPalette = "jco",
  ggpubrAddStats = FALSE,
  ggpubrShowLines = FALSE,
  ggpubrAddPoints = FALSE,
  showExplanations = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- dep1:

  .

- dep2:

  .

- dep3:

  .

- dep4:

  .

- pointpath:

  .

- centralitypath:

  .

- centralityplotting:

  Display mean/median lines across time points to visualize the overall
  trend. Helps identify if the group as a whole is improving, declining,
  or stable over time.

- centralitytype:

  .

- clinicalpreset:

  Choose a preset optimized for common clinical scenarios, or select
  Custom for manual configuration.

- typestatistics:

  Parametric: Assumes normal distribution, most powerful when
  appropriate. Nonparametric: No distribution assumptions, robust for
  skewed biomarker data. Robust: Uses trimmed means, reduces outlier
  influence. Bayesian: Provides evidence strength rather than p-values.

- pairwisecomparisons:

  Enable to see which specific time points differ significantly (e.g.,
  baseline vs month 3, month 3 vs month 6). Useful for identifying when
  changes occur during treatment or disease progression.

- pairwisedisplay:

  .

- padjustmethod:

  .

- effsizetype:

  .

- violin:

  .

- boxplot:

  .

- point:

  .

- mytitle:

  .

- xtitle:

  .

- ytitle:

  .

- originaltheme:

  .

- resultssubtitle:

  .

- bfmessage:

  Whether to display Bayes Factor in the subtitle when using Bayesian
  analysis.

- conflevel:

  Confidence level for confidence intervals.

- k:

  Number of decimal places for displaying statistics in the subtitle.

- plotwidth:

  Width of the plot in pixels. Default is 650.

- plotheight:

  Height of the plot in pixels. Default is 450.

- addGGPubrPlot:

  Add publication-ready plot using ggpubr package for
  within-subjects/paired data.

- ggpubrPlotType:

  Type of ggpubr plot for within-subjects data. Paired plot shows
  individual trajectories.

- ggpubrPalette:

  Color palette for ggpubr plot.

- ggpubrAddStats:

  Add statistical comparison p-values to ggpubr plot.

- ggpubrShowLines:

  Show individual subject trajectories for paired plot.

- ggpubrAddPoints:

  Overlay individual data points on ggpubr plot.

- showExplanations:

  Show explanations of the statistical results.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$warnings`       |     |     |     |     | a html   |
| `results$notices`        |     |     |     |     | a html   |
| `results$interpretation` |     |     |     |     | a html   |
| `results$explanations`   |     |     |     |     | a html   |
| `results$plot`           |     |     |     |     | an image |
| `results$summary`        |     |     |     |     | a html   |
| `results$ggpubrPlot`     |     |     |     |     | an image |
