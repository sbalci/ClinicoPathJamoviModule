# PCA Loading Heatmap & Barmap

Creates heatmap and barmap visualizations of PCA loadings for easy
interpretation of component structure. Heatmaps show all loadings as a
color-coded matrix, while barmaps display loading patterns as bar charts
across components.

## Usage

``` r
pcaloadingheatmap(
  data,
  vars,
  ncomp = 4,
  cutoff = 0.5,
  center = TRUE,
  scale = TRUE,
  textvalues = TRUE,
  starvalues = FALSE,
  textsize = 2,
  plotlegend = TRUE,
  plotcutoff = TRUE,
  gradientcolor = TRUE,
  colorlow = "steelblue1",
  colormid = "white",
  colorhigh = "firebrick1",
  plotwidth = 600,
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

  Number of principal components to display (1 to 10).

- cutoff:

  Threshold for marking loadings as significant (0 to 1). Loadings with
  \|loading\| \>= cutoff will be marked with stars if star_values is
  enabled.

- center:

  Center variables before PCA.

- scale:

  Scale variables before PCA.

- textvalues:

  Display numeric loading values in heatmap/barmap cells.

- starvalues:

  Display stars (\*) for loadings \>= cutoff threshold. Only relevant if
  textvalues is FALSE.

- textsize:

  Font size for text in plots.

- plotlegend:

  Display legend in barmap plot.

- plotcutoff:

  Display horizontal lines at ±cutoff threshold in barmap.

- gradientcolor:

  Use gradient colors proportional to loading values. If FALSE, uses
  binary colors for positive/negative.

- colorlow:

  Color for negative loadings.

- colormid:

  Midpoint color for zero loading.

- colorhigh:

  Color for positive loadings.

- plotwidth:

  Width of plots in pixels.

- plotheight:

  Height of plots in pixels.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a preformatted |
| `results$todo` |  |  |  |  | a html |
| `results$variance` |  |  |  |  | a table |
| `results$heatmap` |  |  |  |  | Heatmap visualization of PCA loadings across components |
| `results$scree` |  |  |  |  | Scree/variance explained plot |
| `results$barmap` |  |  |  |  | Barmap visualization of PCA loadings showing magnitude and direction |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$variance$asDF`

`as.data.frame(results$variance)`

## Details

Both visualizations support optional significance indicators (stars) for
loadings above a specified cutoff threshold. These publication-ready
plots help identify which variables contribute most strongly to each
component.
