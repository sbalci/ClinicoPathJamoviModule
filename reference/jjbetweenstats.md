# Box-Violin Plots to Compare Between Groups

Wrapper Function for ggstatsplot::ggbetweenstats and
ggstatsplot::grouped_ggbetweenstats to generate Box-Violin Plots for
comparing continuous variables between groups with statistical
annotations.

## Usage

``` r
jjbetweenstats(
  data,
  dep,
  group,
  grvar = NULL,
  centralityplotting = FALSE,
  centralitytype = "parametric",
  typestatistics = "parametric",
  pairwisecomparisons = FALSE,
  pairwisedisplay = "significant",
  padjustmethod = "holm",
  effsizetype = "biased",
  mytitle = "Between Group Comparison",
  xtitle = "",
  ytitle = "",
  originaltheme = FALSE,
  resultssubtitle = FALSE,
  bfmessage = FALSE,
  k = 2,
  conflevel = 0.95,
  varequal = FALSE,
  multiEndpointCorrection = "none",
  plotwidth = 650,
  plotheight = 450,
  colorblindSafe = FALSE,
  showexplanations = FALSE,
  addGGPubrPlot = FALSE,
  ggpubrPlotType = "boxplot",
  ggpubrPalette = "jco",
  ggpubrAddStats = TRUE,
  ggpubrAddPoints = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- dep:

  .

- group:

  .

- grvar:

  .

- centralityplotting:

  .

- centralitytype:

  .

- typestatistics:

  .

- pairwisecomparisons:

  .

- pairwisedisplay:

  .

- padjustmethod:

  .

- effsizetype:

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

- k:

  Number of decimal places for displaying statistics in the subtitle.

- conflevel:

  Confidence level for confidence intervals.

- varequal:

  Whether to assume equal variances across groups for parametric tests.

- multiEndpointCorrection:

  Display guidance for multiple testing correction when analyzing
  multiple dependent variables. IMPORTANT: This option does NOT
  automatically adjust p-values. It provides instructions for manual
  correction. When testing multiple endpoints (e.g., cholesterol,
  glucose, triglycerides), the family-wise error rate increases. For
  example, testing 3 variables at alpha=0.05 gives actual error rate of
  ~14 percent. Select a correction method to see step-by-step
  instructions for manually applying that correction to the p-values
  shown in the plots.

- plotwidth:

  Width of the plot in pixels. Default is 650.

- plotheight:

  Height of the plot in pixels. Default is 450.

- colorblindSafe:

  Whether to use colorblind-safe color palette for plot elements.

- showexplanations:

  Display additional explanatory content including about section,
  summary, assumptions, interpretation guide, and copy-ready report
  template.

- addGGPubrPlot:

  Add publication-ready plot using ggpubr package. This provides an
  alternative visualization with publication-quality aesthetics.

- ggpubrPlotType:

  Type of ggpubr plot to display when addGGPubrPlot is enabled.

- ggpubrPalette:

  Color palette for ggpubr plot.

- ggpubrAddStats:

  Add statistical comparison p-values to ggpubr plot.

- ggpubrAddPoints:

  Overlay individual data points on ggpubr plot.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$todo`            |     |     |     |     | a html   |
| `results$mecGuidance`     |     |     |     |     | a html   |
| `results$diagnostics`     |     |     |     |     | a html   |
| `results$clinicalSummary` |     |     |     |     | a html   |
| `results$about`           |     |     |     |     | a html   |
| `results$summary`         |     |     |     |     | a html   |
| `results$assumptions`     |     |     |     |     | a html   |
| `results$interpretation`  |     |     |     |     | a html   |
| `results$report`          |     |     |     |     | a html   |
| `results$plot2`           |     |     |     |     | an image |
| `results$plot`            |     |     |     |     | an image |
| `results$ggpubrPlot`      |     |     |     |     | an image |
| `results$ggpubrPlot2`     |     |     |     |     | an image |
