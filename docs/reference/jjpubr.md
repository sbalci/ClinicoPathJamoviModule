# Publication-Ready Plots (ggpubr)

Create publication-ready plots using the ggpubr package. Provides
easy-to-use functions for creating customized plots with statistical
annotations for scientific publications.

## Usage

``` r
jjpubr(
  data,
  plotType = "boxplot",
  xvar,
  yvar,
  groupvar = NULL,
  facetvar = NULL,
  addStats = FALSE,
  statMethod = "auto",
  pairwiseComparisons = FALSE,
  addCorr = FALSE,
  corrMethod = "pearson",
  addSmoothLine = FALSE,
  addMarginal = FALSE,
  addDensity = FALSE,
  bins = 30,
  addMean = FALSE,
  addMedian = FALSE,
  addPoints = FALSE,
  pointAlpha = 0.6,
  addMeanSD = FALSE,
  palette = "jco",
  fillColor = "#0073C2FF",
  title = "",
  xlab = "",
  ylab = "",
  legendPosition = "right",
  theme = "pubr",
  plotWidth = 600,
  plotHeight = 500,
  showExplanations = FALSE,
  clinicalPreset = "custom"
)
```

## Arguments

- data:

  the data as a data frame

- plotType:

  Type of plot: boxplot, violin, scatter, histogram, density, barplot,
  dotplot, line, errorplot

- xvar:

  X-axis variable

- yvar:

  Y-axis variable (continuous)

- groupvar:

  Grouping variable for coloring points/boxes

- facetvar:

  Variable for faceting plots into panels

- addStats:

  Whether to add statistical comparison p-values

- statMethod:

  Method for statistical comparison. For \>2 groups: omnibus test
  followed by post-hoc pairwise comparisons if significant. For 2
  groups: direct comparison. Bonferroni correction applied to multiple
  comparisons.

- pairwiseComparisons:

  Whether to show post-hoc pairwise comparisons. For \>2 groups, only
  shown if omnibus test is significant (proper hierarchical testing).

- addCorr:

  Whether to add correlation statistics

- corrMethod:

  Correlation method

- addSmoothLine:

  Whether to add smooth trend line

- addMarginal:

  Whether to add marginal histograms

- addDensity:

  Whether to add density curve overlay

- bins:

  Number of histogram bins

- addMean:

  Whether to add mean line

- addMedian:

  Whether to add median line

- addPoints:

  Whether to add jittered points

- pointAlpha:

  Alpha transparency for points

- addMeanSD:

  Whether to show mean ± SD

- palette:

  Color palette name

- fillColor:

  Fill color for single-variable plots

- title:

  Plot title

- xlab:

  X-axis label

- ylab:

  Y-axis label

- legendPosition:

  Legend position

- theme:

  Theme name

- plotWidth:

  Plot width in pixels

- plotHeight:

  Plot height in pixels

- showExplanations:

  Whether to show detailed explanations

- clinicalPreset:

  Clinical analysis preset

## Value

A results object containing:

|                        |     |     |     |     |                |
|------------------------|-----|-----|-----|-----|----------------|
| `results$notices`      |     |     |     |     | a preformatted |
| `results$todo`         |     |     |     |     | a html         |
| `results$plot`         |     |     |     |     | an image       |
| `results$summary`      |     |     |     |     | a html         |
| `results$statistics`   |     |     |     |     | a table        |
| `results$correlation`  |     |     |     |     | a table        |
| `results$descriptives` |     |     |     |     | a table        |
| `results$plotInfo`     |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$statistics$asDF`

`as.data.frame(results$statistics)`

## Details

Available plot types:

- Box plots with statistical comparisons

- Violin plots with distribution visualization

- Scatter plots with correlation analysis

- Histograms with density overlays

- Density plots for distribution analysis

- Bar plots for categorical data

Each plot type supports:

- Automatic statistical testing and p-value annotations

- Publication-ready themes

- Customizable color palettes

- Grouping and faceting options
