# Syndromic Plot (PCA Loadings)

Syndromic plots visualize Principal Component Analysis (PCA) loadings
using a unique triangular design with directional arrows. This
visualization method was developed by Ferguson et al. (2013) for
interpreting complex disease patterns.

## Usage

``` r
jjsyndromicplot(
  data,
  vars,
  missing = "listwise",
  component = 1,
  cutoff = 0.5,
  center = TRUE,
  scale = TRUE,
  arrowsize = 10,
  textsize = 9,
  repel = TRUE,
  plotlegend = TRUE,
  plotcutoff = TRUE,
  varorder = "absdecreasing",
  colorlow = "steelblue1",
  colormid = "white",
  colorhigh = "firebrick1",
  plotwidth = 600,
  plotheight = 600,
  clinicalPreset = "none",
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Continuous variables to include in Principal Component Analysis.
  Select at least 3 numeric variables (e.g., biomarker levels, clinical
  measurements, gene expression values). PCA will identify patterns of
  correlation among these variables.

- missing:

  How to handle missing values. 'listwise' excludes cases with any
  missing values. 'mean_imputation' replaces missing values with the
  variable mean.

- component:

  Which principal component to visualize (1 = PC1, 2 = PC2, etc.). PC1
  typically explains the most variance in the data.

- cutoff:

  Minimum absolute loading value to display a variable (0 to 1).
  Variables with \|loading\| \>= cutoff will be shown as arrows. Higher
  values show fewer, more important variables. Typical range: 0.3-0.6.

- center:

  Center variables to have mean = 0 before PCA. Recommended: TRUE for
  most analyses.

- scale:

  Scale variables to have standard deviation = 1 before PCA.
  Recommended: TRUE when variables have different units or scales.

- arrowsize:

  Controls the width of arrows proportional to loading magnitude. Higher
  values = thicker arrows. Range: 1-30.

- textsize:

  Font size for variable labels and loading values. Range: 4-20.

- repel:

  Use ggrepel to prevent overlapping variable labels. Set to FALSE for
  fixed label positions.

- plotlegend:

  Display the color gradient legend at bottom of plot.

- plotcutoff:

  Highlight the cutoff threshold region in the legend.

- varorder:

  Order of variables around the circle (starting at 12 o'clock,
  counterclockwise). 'abs decreasing' places strongest loadings first.

- colorlow:

  Color for negative loadings. Use color names or hex codes (e.g.,
  "#0000FF").

- colormid:

  Midpoint color for zero loading. Usually white or light gray.

- colorhigh:

  Color for positive loadings. Use color names or hex codes (e.g.,
  "#FF0000").

- plotwidth:

  Width of the plot in pixels.

- plotheight:

  Height of the plot in pixels.

- clinicalPreset:

  Apply a set of predefined settings for common clinical analysis
  scenarios.

- showExplanations:

  Show explanations of the PCA results and syndromic plot.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$todo` |  |  |  |  | a html |
| `results$warnings` |  |  |  |  | a html |
| `results$notices` |  |  |  |  | a preformatted |
| `results$explanations` |  |  |  |  | a html |
| `results$loadings` |  |  |  |  | Standardized loadings for the selected principal component |
| `results$plot` |  |  |  |  | Syndromic visualization of PCA loadings |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$loadings$asDF`

`as.data.frame(results$loadings)`

## Details

Arrow width and color represent the magnitude and direction of variable
loadings on the selected principal component. Only variables with
loadings above the specified cutoff threshold are displayed.

The triangle center displays the component label and variance accounted
for (VAF).
