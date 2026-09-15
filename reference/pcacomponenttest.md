# PCA Component Significance Test

Performs SEQUENTIAL permutation-based significance testing to determine
which principal components explain more variance than expected by random
chance. This provides an objective, hypothesis-tested approach to
component retention.

## Usage

``` r
pcacomponenttest(
  data,
  vars,
  ncomp = 5,
  nperm = 1000,
  stop_rule = TRUE,
  seed = 123,
  center = TRUE,
  scale = TRUE,
  conflevel = 0.95,
  adjustmethod = "BH",
  showpercent = TRUE,
  colororiginal = "#0072B2",
  colorpermuted = "#E69F00",
  showScreePlot = FALSE,
  showLoadingsPlot = FALSE,
  nLoadings = 5,
  plotwidth = 600,
  plotheight = 450,
  showSummary = FALSE,
  showGuide = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Continuous variables to include in Principal Component Analysis.
  Select at least 3 numeric variables.

- ncomp:

  Number of principal components to test for significance (1 to 20).
  Testing will be performed for PC1 through the specified number of
  components.

- nperm:

  Number of permutations to generate null distribution (100-10000).
  Higher values provide more accurate p-values but take longer. Minimum
  p-value = 1/(nperm+1). For p\<0.001, use nperm\>=1000.

- stop_rule:

  Stop testing after the first non-significant component (Sequential
  Testing). Recommended: TRUE to prevent Type I error inflation. If
  FALSE, all components up to ncomp are tested (Batch Testing).

- seed:

  Random seed for reproducibility of permutation results.

- center:

  Center variables to have mean = 0 before PCA. Recommended: TRUE for
  most analyses.

- scale:

  Scale variables to have standard deviation = 1 before PCA.
  Recommended: TRUE when variables have different units or scales.

- conflevel:

  Confidence level for confidence intervals (0.80-0.99). Default: 0.95
  for 95 percent CI.

- adjustmethod:

  Method for adjusting p-values for multiple testing. BH
  (Benjamini-Hochberg) controls false discovery rate.

- showpercent:

  Display variance accounted for (VAF) as percentage (0-100) instead of
  proportion (0-1).

- colororiginal:

  Color for original VAF line/points. Use color names or hex codes.
  Default is color-blind safe blue.

- colorpermuted:

  Color for permuted VAF line/points. Use color names or hex codes.
  Default is color-blind safe orange.

- showScreePlot:

  Display a scree plot of eigenvalues.

- showLoadingsPlot:

  Display a plot of variable loadings for significant components.

- nLoadings:

  Number of top variables to display in loadings plot.

- plotwidth:

  Width of the plot in pixels.

- plotheight:

  Height of the plot in pixels.

- showSummary:

  Display natural-language summary of results with clinical
  interpretation. Provides plain-language explanation of significant
  components and variance explained.

- showGuide:

  Display guide explaining how to interpret VAF, p-values, and clinical
  implications. Includes definitions of key terms and guidance for
  clinical use.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a preformatted |
| `results$todo` |  |  |  |  | a html |
| `results$warnings` |  |  |  |  | a html |
| `results$results` |  |  |  |  | Statistical significance of principal components based on permutation testing |
| `results$vafplot` |  |  |  |  | Visualization comparing original VAF to permuted null distribution |
| `results$screePlot` |  |  |  |  | an image |
| `results$loadingsPlot` |  |  |  |  | an image |
| `results$summary` |  |  |  |  | a html |
| `results$guide` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$results$asDF`

`as.data.frame(results$results)`

## Details

The test uses the Buja & Eyuboglu (1992) sequential method where:

1.  Each component is tested against a permutation null distribution

2.  Significant components have their variance REMOVED before testing
    the next component

3.  Testing STOPS when the first non-significant component is found

This sequential approach prevents inflated Type I errors that occur when
all components are tested against the same null distribution (batch
testing).

CRITICAL: Requires centered and scaled data for valid correlation-based
interpretation. Without centering/scaling, the test compares raw
variance instead of correlation structure.
