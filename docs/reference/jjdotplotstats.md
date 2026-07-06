# Dot Chart

Wrapper Function for ggstatsplot::ggbetweenstats and
ggstatsplot::grouped_ggbetweenstats to generate dot-style comparisons of
continuous variables between groups with statistical annotations and
significance testing.

## Usage

``` r
jjdotplotstats(
  data,
  dep,
  group,
  grvar,
  typestatistics = "parametric",
  effsizetype = "biased",
  centralityplotting = FALSE,
  centralitytype = "parametric",
  mytitle = "",
  xtitle = "",
  ytitle = "",
  originaltheme = FALSE,
  resultssubtitle = FALSE,
  testvalue = 0,
  bfmessage = FALSE,
  conflevel = 0.95,
  k = 2,
  testvalueline = FALSE,
  centralityparameter = "mean",
  centralityk = 2,
  plotwidth = 650,
  plotheight = 450
)
```

## Arguments

- data:

  The data as a data frame.

- dep:

  A continuous numeric variable for which the distribution will be
  displayed across different groups using dot plots.

- group:

  A categorical variable that defines the groups for comparison. Each
  level will be displayed as a separate group in the dot plot.

- grvar:

  Optional grouping variable to create separate dot plots for each level
  of this variable (grouped analysis).

- typestatistics:

  Choose the appropriate statistical test: Parametric (t-test) assumes
  normal distribution and equal variances; Nonparametric
  (Mann-Whitney U) makes no distribution assumptions; Robust uses
  trimmed means to handle outliers; Bayesian provides evidence strength
  via Bayes factors.

- effsizetype:

  Effect size quantifies practical significance: Cohen's d shows
  standardized difference between groups (small=0.2, medium=0.5,
  large=0.8); Hedge's g corrects for small samples; Eta/Omega-squared
  show proportion of variance explained (small=0.01, medium=0.06,
  large=0.14).

- centralityplotting:

  Display lines showing the central tendency (mean, median, or trimmed
  mean) for each group. Helps visualize group differences at a glance.

- centralitytype:

  Type of central tendency to display: Mean is the average but sensitive
  to outliers; Median is the middle value and robust to outliers;
  Trimmed mean excludes extreme values; Bayesian provides probabilistic
  estimate.

- mytitle:

  Main title for the plot. Leave blank for automatic title generation
  based on your variables.

- xtitle:

  Label for the horizontal axis showing the continuous variable values.
  Leave blank to use variable name.

- ytitle:

  Label for the vertical axis showing the group categories. Leave blank
  to use variable name.

- originaltheme:

  Use the original ggstatsplot theme instead of jamovi's default theme.
  The original theme may be more suitable for publications.

- resultssubtitle:

  Display statistical test results (p-value, effect size, confidence
  interval) as a subtitle below the plot. Recommended for most analyses.

- testvalue:

  Reference value for hypothesis testing (usually 0 for group
  comparisons). Can be changed to test against a specific clinically
  meaningful value.

- bfmessage:

  Display Bayes Factor interpretation (evidence strength) when using
  Bayesian analysis. BF \> 3 indicates moderate evidence, BF \> 10
  strong evidence.

- conflevel:

  Confidence level for intervals (0.95 = 95 percent confidence
  interval). This represents the probability that the true population
  parameter lies within the calculated interval. 95 percent is standard
  for most analyses.

- k:

  Number of decimal places for statistical results (p-values, effect
  sizes). More decimal places show greater precision but may not be
  clinically meaningful.

- testvalueline:

  Display a vertical reference line at the test value. Useful for
  showing clinically significant thresholds or normal reference ranges.

- centralityparameter:

  Which central tendency measure to show as a vertical line on the plot.
  Mean is sensitive to outliers; median is more robust for skewed data.

- centralityk:

  Decimal places for central tendency values displayed on the plot.
  Should match the precision meaningful for your measurement scale.

- plotwidth:

  Width of the plot in pixels. Larger values provide more detail but may
  not fit well in reports. Default: 650 pixels.

- plotheight:

  Height of the plot in pixels. Adjust based on number of groups to
  ensure readability. Default: 450 pixels.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$todo`            |     |     |     |     | a html   |
| `results$notices`         |     |     |     |     | a html   |
| `results$plot2`           |     |     |     |     | an image |
| `results$plot`            |     |     |     |     | an image |
| `results$interpretation`  |     |     |     |     | a html   |
| `results$assumptions`     |     |     |     |     | a html   |
| `results$reportSentence`  |     |     |     |     | a html   |
| `results$guidedSteps`     |     |     |     |     | a html   |
| `results$recommendations` |     |     |     |     | a html   |
