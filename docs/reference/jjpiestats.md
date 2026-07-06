# Pie Charts

'Wrapper Function for ggstatsplot::ggpiestats and
ggstatsplot::grouped_ggpiestats to generate Pie Charts with statistical
analysis.'

## Usage

``` r
jjpiestats(
  data,
  dep,
  group = NULL,
  grvar = NULL,
  typestatistics = "parametric",
  originaltheme = FALSE,
  counts = NULL,
  ratio = "",
  paired = FALSE,
  label = "percentage",
  digits = 2,
  conflevel = 0.95,
  proportiontest = FALSE,
  bfmessage = FALSE,
  messages = FALSE,
  clinicalpreset = "custom",
  showexplanations = FALSE,
  resultssubtitle = FALSE,
  showSummary = FALSE,
  showAssumptions = FALSE,
  showInterpretation = FALSE,
  addGGPubrDonut = FALSE,
  ggpubrDonutPalette = "jco"
)
```

## Arguments

- data:

  The data as a data frame.

- dep:

  The categorical variable to display in the pie chart. This variable
  will be used to create pie slices representing the proportion of each
  category. Must be a factor variable.

- group:

  Optional grouping variable for creating contingency table analysis.
  When specified, creates a 2x2 or 2xN contingency table and performs
  association tests (Chi-square, Fisher's exact test, etc.).

- grvar:

  Optional variable to create separate pie charts for each level of this
  grouping variable. Creates a panel of pie charts, one for each group
  level (e.g., separate charts by treatment center or study site).

- typestatistics:

  Type of statistical test for categorical association analysis.
  'parametric' uses Pearson's Chi-square test, 'nonparametric' uses
  contingency table tests, 'robust' uses robust association measures,
  'bayes' provides Bayesian analysis with Bayes factors.

- originaltheme:

  Whether to apply the original ggstatsplot theme layer to the plot. If
  TRUE, uses ggstatsplot's default styling. If FALSE, uses jamovi's
  default ggplot2 theme for consistency with other analyses.

- counts:

  A variable in data containing counts, or NULL if each row represents a
  single observation. Use this when your data is already
  aggregated/tabulated.

- ratio:

  A comma-separated list of expected proportions for the proportion test
  (should sum to 1). For example: '0.5,0.5' for two equal groups or
  '0.25,0.25,0.25,0.25' for four equal groups. Leave empty for equal
  theoretical proportions.

- paired:

  Logical indicating whether data came from a within-subjects or
  repeated measures design study (Default: FALSE). If TRUE, McNemar's
  test will be used. If FALSE, Pearson's chi-square test will be used.

- label:

  What information needs to be displayed on the label in each pie slice.

- digits:

  Number of digits after decimal point for statistical results.

- conflevel:

  Confidence/credible interval level.

- proportiontest:

  Decides whether proportion test for x variable is to be carried out
  for each level of y.

- bfmessage:

  Display Bayes Factor in favor of the null hypothesis. Only relevant
  for parametric test.

- messages:

  Display statistical messages in console. Disabling improves
  performance.

- clinicalpreset:

  Predefined configurations for common clinical scenarios. Automatically
  sets appropriate statistical methods and parameters.

- showexplanations:

  Display detailed explanations of statistical methods, assumptions, and
  clinical interpretations to guide analysis and result interpretation.

- resultssubtitle:

  Whether to display statistical test results as subtitle in the pie
  chart. Shows test statistics, p-values, effect sizes, and confidence
  intervals for categorical association tests. Provides detailed
  statistical summary.

- showSummary:

  Display natural-language summary paragraph with key findings suitable
  for copying into reports and publications.

- showAssumptions:

  Display statistical assumptions panel with detected violations,
  warnings about chi-square expected counts, and Fisher exact test
  recommendations.

- showInterpretation:

  Display effect size interpretation guidelines (Cramér's V thresholds),
  clinical context based on analysis preset, and methodological notes.

- addGGPubrDonut:

  Add modern donut chart variant using ggpubr.

- ggpubrDonutPalette:

  Color palette for donut chart.

## Value

A results object containing:

|                          |     |     |     |     |                |
|--------------------------|-----|-----|-----|-----|----------------|
| `results$notices`        |     |     |     |     | a preformatted |
| `results$about`          |     |     |     |     | a html         |
| `results$summary`        |     |     |     |     | a html         |
| `results$assumptions`    |     |     |     |     | a html         |
| `results$interpretation` |     |     |     |     | a html         |
| `results$report`         |     |     |     |     | a html         |
| `results$todo`           |     |     |     |     | a html         |
| `results$plot4`          |     |     |     |     | an image       |
| `results$plot2`          |     |     |     |     | an image       |
| `results$plot1`          |     |     |     |     | an image       |
| `results$donutPlot`      |     |     |     |     | an image       |
