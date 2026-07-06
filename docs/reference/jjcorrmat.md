# Correlation Matrix

Wrapper Function for ggstatsplot::ggcorrmat and
ggstatsplot::grouped_ggcorrmat to generate correlation matrix
visualizations with significance testing.

## Usage

``` r
jjcorrmat(
  data,
  dep,
  grvar,
  typestatistics = "parametric",
  matrixtype = "upper",
  matrixmethod = "square",
  siglevel = 0.05,
  conflevel = 0.95,
  padjustmethod = "holm",
  k = 2,
  partial = FALSE,
  naHandling = "listwise",
  lowcolor = "#E69F00",
  midcolor = "white",
  highcolor = "#009E73",
  title = "",
  subtitle = "",
  caption = "",
  showexplanations = FALSE,
  plotwidth = 600,
  plotheight = 450
)
```

## Arguments

- data:

  The data as a data frame.

- dep:

  List of continuous variables for which the correlation matrix will be
  computed and visualized. All variables must be numeric.

- grvar:

  Optional grouping variable to create separate correlation matrices for
  each level of the grouping variable.

- typestatistics:

  Type of correlation analysis to perform. 'parametric' uses Pearson
  correlation, 'nonparametric' uses Spearman's rho, 'robust' uses
  percentage bend correlation, 'bayes' computes Bayes factors.

- matrixtype:

  Display upper triangular, lower triangular or full matrix.

- matrixmethod:

  The visualization method of correlation matrix to be used.

- siglevel:

  Significance level for marking correlations as insignificant.

- conflevel:

  Confidence level for confidence intervals.

- padjustmethod:

  Adjustment method for multiple comparisons.

- k:

  Number of decimal places for displaying correlation coefficients.

- partial:

  Compute partial correlations instead of zero-order correlations.
  Partial correlations control for all other variables in the analysis.

- naHandling:

  Choose how missing values are handled. Listwise drops rows with
  missing values in selected variables; pairwise uses available data for
  each pair.

- lowcolor:

  Color for low (negative) correlation values.

- midcolor:

  Color for mid (zero) correlation values.

- highcolor:

  Color for high (positive) correlation values.

- title:

  Title for the correlation matrix plot.

- subtitle:

  Subtitle for the correlation matrix plot.

- caption:

  Caption for the correlation matrix plot.

- showexplanations:

  Show detailed explanations of the analysis, including assumptions,
  interpretation, and a copy-ready report.

- plotwidth:

  Width of the correlation matrix plot in pixels. Default is 600.

- plotheight:

  Height of the correlation matrix plot in pixels. Default is 450.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$warnings`       |     |     |     |     | a html   |
| `results$interpretation` |     |     |     |     | a html   |
| `results$about`          |     |     |     |     | a html   |
| `results$summary`        |     |     |     |     | a html   |
| `results$assumptions`    |     |     |     |     | a html   |
| `results$plot2`          |     |     |     |     | an image |
| `results$plot`           |     |     |     |     | an image |
| `results$table`          |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$table$asDF`

`as.data.frame(results$table)`
