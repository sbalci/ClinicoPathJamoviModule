# Feature Quality Assessment

Comprehensive feature quality assessment for clinical and pathology
research data. Provides essential data quality control including missing
data analysis, outlier detection, distribution analysis, correlation
assessment, and variance evaluation. Implements multiple quality metrics
with clinical interpretation and actionable recommendations. Critical
for ensuring data integrity before statistical analysis and meeting
regulatory standards for clinical research.

## Usage

``` r
featurequality(
  data,
  features,
  group_var = NULL,
  analysis_scope = "comprehensive",
  missing_data_analysis = TRUE,
  distribution_analysis = TRUE,
  outlier_detection = TRUE,
  outlier_method = "multiple",
  outlier_threshold = 3,
  correlation_analysis = TRUE,
  correlation_threshold = 0.8,
  variance_analysis = TRUE,
  low_variance_threshold = 0.01,
  normality_testing = TRUE,
  normality_method = "multiple",
  skewness_analysis = TRUE,
  feature_importance = FALSE,
  importance_method = "random_forest",
  data_transformation = TRUE,
  quality_score = TRUE,
  detailed_plots = TRUE,
  plot_distributions = TRUE,
  plot_correlations = TRUE,
  plot_outliers = TRUE,
  plot_missing = TRUE,
  export_recommendations = FALSE,
  clinical_context = TRUE,
  batch_processing = TRUE,
  confidence_level = 0.95,
  random_seed = 123
)
```

## Arguments

- data:

  .

- features:

  .

- group_var:

  .

- analysis_scope:

  .

- missing_data_analysis:

  .

- distribution_analysis:

  .

- outlier_detection:

  .

- outlier_method:

  .

- outlier_threshold:

  .

- correlation_analysis:

  .

- correlation_threshold:

  .

- variance_analysis:

  .

- low_variance_threshold:

  .

- normality_testing:

  .

- normality_method:

  .

- skewness_analysis:

  .

- feature_importance:

  .

- importance_method:

  .

- data_transformation:

  .

- quality_score:

  .

- detailed_plots:

  .

- plot_distributions:

  .

- plot_correlations:

  .

- plot_outliers:

  .

- plot_missing:

  .

- export_recommendations:

  .

- clinical_context:

  .

- batch_processing:

  .

- confidence_level:

  .

- random_seed:

  .

## Value

A results object containing:

|                                          |     |     |     |     |          |
|------------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`                   |     |     |     |     | a html   |
| `results$progress`                       |     |     |     |     | a html   |
| `results$quality_summary`                |     |     |     |     | a table  |
| `results$missing_data_analysis`          |     |     |     |     | a table  |
| `results$distribution_analysis`          |     |     |     |     | a table  |
| `results$outlier_analysis`               |     |     |     |     | a table  |
| `results$correlation_analysis`           |     |     |     |     | a table  |
| `results$variance_analysis`              |     |     |     |     | a table  |
| `results$normality_analysis`             |     |     |     |     | a table  |
| `results$feature_importance`             |     |     |     |     | a table  |
| `results$transformation_recommendations` |     |     |     |     | a table  |
| `results$overall_recommendations`        |     |     |     |     | a html   |
| `results$clinical_interpretation`        |     |     |     |     | a html   |
| `results$distribution_plot`              |     |     |     |     | an image |
| `results$correlation_plot`               |     |     |     |     | an image |
| `results$outlier_plot`                   |     |     |     |     | an image |
| `results$missing_plot`                   |     |     |     |     | an image |
| `results$quality_dashboard`              |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$quality_summary$asDF`

`as.data.frame(results$quality_summary)`
