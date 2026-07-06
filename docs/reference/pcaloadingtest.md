# PCA Loading Significance Test

Performs permutation-based significance testing for PCA loadings using
the permV method (Linting et al., 2011). This approach permutes one
variable at a time, providing variable-specific and component-specific
significance thresholds with higher statistical power and proper Type I
error control.

## Usage

``` r
pcaloadingtest(
  data,
  vars,
  ncomp = 3,
  nperm = 1000,
  seed = 123,
  componentfilter = 0,
  center = TRUE,
  scale = TRUE,
  conflevel = 0.95,
  adjustmethod = "BH",
  colorlow = "steelblue1",
  colormid = "white",
  colorhigh = "firebrick1",
  plotwidth = 700,
  plotheight = 450
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Continuous variables to include in Principal Component Analysis.
  Select at least 3 numeric variables.

- ncomp:

  Number of principal components to test loadings for (1 to 10).

- nperm:

  Number of permutations per variable (100-5000). Total permutations =
  nperm × number of variables. Higher values provide more accurate
  p-values but take longer.

- seed:

  Seed for permutation reproducibility. Use the same seed to obtain
  identical results.

- componentfilter:

  Filter results table by component. 0 shows all components, 1 shows
  only PC1, 2 shows only PC2, etc.

- center:

  Center variables to have mean = 0 before PCA.

- scale:

  Scale variables to have standard deviation = 1 before PCA.

- conflevel:

  Confidence level for confidence intervals (0.80-0.99).

- adjustmethod:

  Method for adjusting p-values within each component. BH
  (Benjamini-Hochberg) controls false discovery rate.

- colorlow:

  Color for negative loadings in barplot.

- colormid:

  Midpoint color for zero loading.

- colorhigh:

  Color for positive loadings in barplot.

- plotwidth:

  Width of the plot in pixels.

- plotheight:

  Height of the plot in pixels.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a preformatted |
| `results$todo` |  |  |  |  | a html |
| `results$clinicalGuide` |  |  |  |  | a html |
| `results$results` |  |  |  |  | Permutation-based significance testing for each variable-component loading |
| `results$variance` |  |  |  |  | a table |
| `results$loadingplot` |  |  |  |  | Barplot showing loadings with confidence intervals from permutation |
| `results$scree` |  |  |  |  | Scree/variance explained plot |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$results$asDF`

`as.data.frame(results$results)`

## Details

Instead of arbitrary cutoffs (e.g., \|loading\| \> 0.5), this test
provides objective, hypothesis-tested thresholds for each variable on
each component. Procrustes rotation is applied to handle sign reflection
and component indeterminacy issues.
