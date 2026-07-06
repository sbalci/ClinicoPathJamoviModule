# Bar Charts

'Wrapper Function for ggstatsplot::ggbarstats and
ggstatsplot::grouped_ggbarstats to generate Bar Charts.'

## Usage

``` r
jjbarstats(
  data,
  dep,
  group,
  grvar = NULL,
  counts = NULL,
  excl = FALSE,
  typestatistics = "parametric",
  pairwisecomparisons = FALSE,
  pairwisedisplay = "significant",
  padjustmethod = "holm",
  originaltheme = FALSE,
  resultssubtitle = FALSE,
  messages = FALSE,
  paired = FALSE,
  label = "percentage",
  digits = 2,
  digitsperc = 0,
  proportiontest = FALSE,
  bfmessage = FALSE,
  conflevel = 0.95,
  ratio = "",
  clinicalpreset = "custom",
  showexplanations = FALSE,
  showSummary = TRUE,
  showAssumptions = TRUE,
  showInterpretation = FALSE,
  addGGPubrBalloon = FALSE,
  ggpubrBalloonPalette = "jco"
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

- counts:

  A variable in data containing counts, or NULL if each row represents a
  single observation.

- excl:

  .

- typestatistics:

  .

- pairwisecomparisons:

  .

- pairwisedisplay:

  .

- padjustmethod:

  .

- originaltheme:

  .

- resultssubtitle:

  Display statistical test results in plot subtitle. Disabling improves
  performance significantly.

- messages:

  Display statistical messages in console. Disabling improves
  performance.

- paired:

  Logical indicating whether data came from a within-subjects or
  repeated measures design study. If TRUE, McNemar's test will be used.
  If FALSE, Pearson's chi-square test will be used.

- label:

  What information needs to be displayed on the label in each bar
  segment.

- digits:

  Number of digits after decimal point for statistical results.

- digitsperc:

  Number of decimal places for percentage labels.

- proportiontest:

  Decides whether proportion test for x variable is to be carried out
  for each level of y.

- bfmessage:

  Display Bayes Factor in favor of the null hypothesis. Only relevant
  for parametric test.

- conflevel:

  Confidence/credible interval level.

- ratio:

  A comma-separated list of expected proportions for the proportion test
  (should sum to 1). For example: '0.5,0.5' for two equal groups or
  '0.25,0.25,0.25,0.25' for four equal groups. Leave empty for equal
  theoretical proportions.

- clinicalpreset:

  Predefined configurations for common clinical scenarios. Automatically
  sets appropriate statistical methods and parameters.

- showexplanations:

  Display detailed explanations of statistical methods, assumptions, and
  clinical interpretations to guide analysis and result interpretation.

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

- addGGPubrBalloon:

  Add balloon plot for contingency table visualization using ggpubr.

- ggpubrBalloonPalette:

  Color palette for balloon plot.

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
| `results$plot2`          |     |     |     |     | an image       |
| `results$plot`           |     |     |     |     | an image       |
| `results$balloonPlot`    |     |     |     |     | an image       |
