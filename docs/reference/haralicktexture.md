# Haralick Texture Analysis

Haralick Texture Analysis

## Usage

``` r
haralicktexture(
  data,
  texture_features,
  x_coord,
  y_coord,
  group_var,
  outcome_var,
  analysis_focus = "comprehensive",
  feature_selection = "all",
  correlation_threshold = 0.9,
  normality_testing = TRUE,
  outlier_detection = TRUE,
  outlier_method = "iqr",
  show_distribution_plots = TRUE,
  show_correlation_plot = TRUE,
  show_spatial_plot = FALSE,
  texture_interpretation = "clinical",
  biomarker_context = "general"
)
```

## Arguments

- data:

  the data as a data frame

- texture_features:

  Haralick texture feature measurements (entropy, contrast, correlation,
  etc.)

- x_coord:

  X spatial coordinate for spatial analysis

- y_coord:

  Y spatial coordinate for spatial analysis

- group_var:

  grouping variable for stratified analysis

- outcome_var:

  clinical outcome for prognostic analysis

- analysis_focus:

  primary focus of texture analysis

- feature_selection:

  method for feature selection/filtering

- correlation_threshold:

  threshold for identifying highly correlated features

- normality_testing:

  perform normality testing for distribution assessment

- outlier_detection:

  identify and flag potential outliers

- outlier_method:

  method for outlier detection

- show_distribution_plots:

  display histograms and distribution plots

- show_correlation_plot:

  display correlation matrix heatmap

- show_spatial_plot:

  display spatial distribution plot (requires coordinates)

- texture_interpretation:

  level of interpretation and guidance provided

- biomarker_context:

  biomarker context for specialized interpretation

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$summary` |  |  |  |  | Copy-ready summary with key findings and clinical interpretation |
| `results$about` |  |  |  |  | What this analysis does, when to use it, and how to interpret results |
| `results$interpretation` |  |  |  |  | a html |
| `results$texturetable` |  |  |  |  | Descriptive statistics for each texture feature |
| `results$correlationtable` |  |  |  |  | Correlation between texture features |
| `results$groupcomparisontable` |  |  |  |  | Statistical comparison across groups |
| `results$normalitytable` |  |  |  |  | Shapiro-Wilk and other normality tests |
| `results$outliertable` |  |  |  |  | Identified outliers in texture features |
| `results$distributionplot` |  |  |  |  | Histogram and density plots for texture features |
| `results$heatmapplot` |  |  |  |  | Visual correlation matrix between features |
| `results$boxplot` |  |  |  |  | Boxplots comparing texture features across groups |
| `results$clinicalinterpretation` |  |  |  |  | Context-specific clinical interpretation |
| `results$prognosticsummary` |  |  |  |  | Prognostic relevance of texture features |
| `results$missingdata` |  |  |  |  | Missing data patterns and impact |
| `results$variability` |  |  |  |  | Coefficient of variation and reliability |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$texturetable$asDF`

`as.data.frame(results$texturetable)`
