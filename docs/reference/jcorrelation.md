# Correlation Analysis

Comprehensive correlation analysis including Pearson, Spearman, and
Kendall correlations with significance tests, confidence intervals, and
natural language reporting. Suitable for exploring relationships between
continuous variables.

## Usage

``` r
jcorrelation(
  data,
  vars,
  group = NULL,
  method = "pearson",
  alternative = "two.sided",
  ci = TRUE,
  ciWidth = 95,
  flag = TRUE,
  flagAlpha = 0.05,
  plots = TRUE,
  plotType = "matrix",
  report = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  A vector of strings naming the variables to correlate.

- group:

  Variable to split the analysis by.

- method:

  The correlation method to use: 'pearson' (default), 'spearman', or
  'kendall'.

- alternative:

  The alternative hypothesis: 'two.sided' (default), 'greater', or
  'less'.

- ci:

  TRUE (default) or FALSE, provide confidence intervals.

- ciWidth:

  Confidence interval level (default: 95 percent).

- flag:

  TRUE (default) or FALSE, flag significant correlations.

- flagAlpha:

  Alpha level for flagging significant correlations (default: 0.05).

- plots:

  TRUE (default) or FALSE, provide correlation plots.

- plotType:

  Type of correlation plot: 'matrix' (default), 'pairs', or 'network'.

- report:

  TRUE (default) or FALSE, provide natural language interpretation.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$matrix` |  |  |  |  | correlation matrix with significance tests |
| `results$tests` |  |  |  |  | detailed correlation tests for each pair of variables |
| `results$summary` |  |  |  |  | summary of correlation analysis |
| `results$report` |  |  |  |  | a html |
| `results$plot` |  |  |  |  | an image |
| `results$plotMatrix` |  |  |  |  | an image |
| `results$plotPairs` |  |  |  |  | an image |
| `results$plotNetwork` |  |  |  |  | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$matrix$asDF`

`as.data.frame(results$matrix)`
