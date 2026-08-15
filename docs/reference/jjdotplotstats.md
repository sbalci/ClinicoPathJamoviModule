# Horizontal Box-Violin Comparison

Compares a continuous variable across groups and draws the comparison
horizontally - values on the x axis, group labels down the y axis - with
an optional vertical reference line. Wraps ggstatsplot::ggbetweenstats
and ggstatsplot::grouped_ggbetweenstats, so the figure is a box-violin
plot with the individual observations shown, and the test is a
between-groups comparison using every observation.

## Usage

``` r
jjdotplotstats(
  data,
  dep,
  group,
  grvar = NULL,
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

  Position of the optional reference line, in the units of the dependent
  variable. Use it to mark a clinically meaningful threshold such as an
  upper limit of normal. No hypothesis test is performed against this
  value; it only draws a line, and only when 'Reference value line' is
  ticked.

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

  Draw a dashed vertical line at 'Reference Line Value'. Useful for
  marking a clinical threshold or a normal reference limit. This is a
  visual annotation only.

- centralityparameter:

  Which central tendency measure to show as a vertical line on the plot.
  Mean is sensitive to outliers; median is more robust for skewed data.

- centralityk:

  Deprecated and ignored. The statistics package no longer accepts a
  separate precision for the centrality labels; they follow 'Statistical
  Precision (Decimal Places)'. Retained so existing scripts keep
  running, and removed from the user interface.

- plotwidth:

  Width of the plot in pixels. Larger values provide more detail but may
  not fit well in reports. Default: 650 pixels.

- plotheight:

  Height of the plot in pixels. Adjust based on number of groups to
  ensure readability. Default: 450 pixels.

## Value

A results object containing:

|                   |     |     |     |     |          |
|-------------------|-----|-----|-----|-----|----------|
| `results$todo`    |     |     |     |     | a html   |
| `results$notices` |     |     |     |     | a html   |
| `results$plot2`   |     |     |     |     | an image |
| `results$plot`    |     |     |     |     | an image |

## Details

This analysis was previously titled "Dot Chart", which described neither
the figure nor the statistic: it draws violins and boxplots, not a dot
chart, and it is a between-groups test rather than a one-sample one. For
a genuine Cleveland dot chart - one summary point per label, tested
against a reference value - use "Dot Chart (Summary vs Reference
Value)", which wraps ggstatsplot::ggdotplotstats.

Prefer this over "Box-Violin Plots to Compare Between Groups" when the
group labels are long or numerous, since the horizontal layout gives
them room, or when a clinical threshold line is useful.
