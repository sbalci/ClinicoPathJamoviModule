# Histogram

'Wrapper Function for ggstatsplot::gghistostats and
ggstatsplot::grouped_gghistostats to generate Histogram.'

## Usage

``` r
jjhistostats(
  data,
  dep,
  grvar,
  typestatistics = "parametric",
  centralityline = FALSE,
  changebinwidth = FALSE,
  binwidth = 1.1,
  resultssubtitle = FALSE,
  showInterpretation = FALSE,
  clinicalPreset = "custom",
  enableOneSampleTest = FALSE,
  test.value = 0,
  conf.level = 0.95,
  bf.message = FALSE,
  digits = 2,
  xlab = "",
  title = "",
  subtitle = "",
  caption = "",
  centralitytype = "default",
  binfill = "skyblue",
  bincolor = "black",
  binalpha = 0.7,
  centralitylinecolor = "blue",
  centralitylinewidth = 1,
  centralitylinetype = "dashed",
  plotwidth = 600,
  plotheight = 450,
  addGGPubrPlot = FALSE,
  ggpubrPalette = "#0073C2FF",
  ggpubrAddDensity = FALSE,
  ggpubrAddMean = FALSE,
  addDistributionDiagnostics = FALSE,
  ggpubrDensityColor = "#0073C2FF",
  ggpubrShowQQ = FALSE,
  ggpubrShowECDF = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- dep:

  One or more continuous numeric variables for which histograms will be
  created. Multiple variables will be displayed in separate panels.

- grvar:

  Optional grouping variable to create separate histograms for each
  level of this variable (grouped analysis).

- typestatistics:

  Type of statistical test for normality assessment. 'parametric' uses
  Shapiro-Wilk test, 'nonparametric' uses Anderson-Darling test,
  'robust' uses robust normality tests, 'bayes' provides Bayesian
  analysis.

- centralityline:

  Whether to display a vertical line indicating the measure of central
  tendency (mean for parametric, median for nonparametric).

- changebinwidth:

  Whether to manually specify the bin width. If FALSE, automatic bin
  width calculation will be used.

- binwidth:

  Manual bin width for histogram. Only used when changebinwidth is TRUE.
  Smaller values create more bins, larger values create fewer bins.

- resultssubtitle:

  Whether to display statistical test results as subtitle in the plot,
  including normality test results and descriptive statistics.

- showInterpretation:

  Generate clinical interpretation of histogram results including
  distribution shape, normality assessment, and practical implications
  for clinical data. Note: Uses simplified heuristics (skewness
  threshold and sample size) as initial screening guidance, not formal
  diagnostic criteria. Should be supplemented with formal normality
  tests and expert judgment.

- clinicalPreset:

  Predefined configurations optimized for common clinical data types.
  Automatically sets appropriate statistical methods and visualization
  options.

- enableOneSampleTest:

  Whether to enable and display one-sample hypothesis test against a
  specified test value. When disabled, only descriptive statistics and
  distribution visualization are shown.

- test.value:

  Value to compare the sample against in one-sample test. Only used when
  enableOneSampleTest is TRUE. Note: Testing against 0 is rarely
  clinically meaningful for most biomedical data. Consider using a
  clinically relevant threshold (e.g., reference range limit, treatment
  cutoff, or population norm) for meaningful hypothesis testing.

- conf.level:

  Confidence level for confidence intervals (between 0 and 1).

- bf.message:

  Whether to display Bayes Factor in the subtitle when using Bayesian
  analysis.

- digits:

  Number of decimal places for displaying statistics in the subtitle.

- xlab:

  Custom label for the x-axis. If empty, variable name will be used.

- title:

  Title for the plot.

- subtitle:

  Subtitle for the plot (overrides statistical results if provided).

- caption:

  Caption text to display at the bottom of the plot.

- centralitytype:

  Type of central tendency measure to display. 'Default' uses the
  appropriate measure based on the statistical test type selected.

- binfill:

  Fill color for histogram bins.

- bincolor:

  Border color for histogram bins.

- binalpha:

  Transparency level for histogram bins (0 = fully transparent, 1 =
  opaque).

- centralitylinecolor:

  Color of the vertical centrality line.

- centralitylinewidth:

  Width of the vertical centrality line.

- centralitylinetype:

  Line type for the vertical centrality line.

- plotwidth:

  Width of each plot in pixels. Default is 600.

- plotheight:

  Height of each plot in pixels. Default is 450.

- addGGPubrPlot:

  Add publication-ready histogram using ggpubr package.

- ggpubrPalette:

  Fill color for ggpubr histogram (hex code).

- ggpubrAddDensity:

  Add density curve overlay to ggpubr histogram.

- ggpubrAddMean:

  Add vertical line at mean for ggpubr histogram.

- addDistributionDiagnostics:

  Add density plot, ECDF, and QQ plot for distribution assessment using
  ggpubr.

- ggpubrDensityColor:

  Fill color for density plot (hex code).

- ggpubrShowQQ:

  Show QQ plot for normality assessment.

- ggpubrShowECDF:

  Show empirical cumulative distribution function plot.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$plot2`          |     |     |     |     | an image |
| `results$plot`           |     |     |     |     | an image |
| `results$interpretation` |     |     |     |     | a html   |
| `results$ggpubrPlot`     |     |     |     |     | an image |
| `results$ggpubrPlot2`    |     |     |     |     | an image |
| `results$densityPlot`    |     |     |     |     | an image |
| `results$qqPlot`         |     |     |     |     | an image |
| `results$ecdfPlot`       |     |     |     |     | an image |
