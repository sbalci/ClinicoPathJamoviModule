# Supervised Principal Components Cox Regression

Supervised Principal Components (SuperPC) for Cox regression with
high-dimensional data. This method first screens features using
univariate Cox regression, then applies PCA to selected features, and
finally fits a Cox model using the principal components. Particularly
effective for genomics data where many features are correlated.

## Usage

``` r
superpc(
  data,
  time,
  event,
  features,
  threshold = 0.1,
  n_components = 5,
  cv_folds = 10,
  standardize = TRUE,
  screening_method = "univariate_cox",
  pca_method = "standard",
  validation_method = "cv",
  plot_screening = TRUE,
  plot_pca = TRUE,
  plot_survival = TRUE,
  export_components = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- time:

  survival time variable

- event:

  event indicator (1=event, 0=censored)

- features:

  high-dimensional feature variables (e.g., gene expression)

- threshold:

  p-value threshold for initial feature screening

- n_components:

  number of principal components to extract

- cv_folds:

  number of folds for cross-validation

- standardize:

  standardize features before analysis

- screening_method:

  method for initial feature screening

- pca_method:

  principal component analysis method

- validation_method:

  model validation approach

- plot_screening:

  display feature screening results

- plot_pca:

  display principal component analysis plots

- plot_survival:

  display survival curves by risk groups

- export_components:

  export principal component scores to data

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`             |     |     |     |     | a html   |
| `results$analysis_summary`         |     |     |     |     | a table  |
| `results$feature_screening`        |     |     |     |     | a table  |
| `results$principal_components`     |     |     |     |     | a table  |
| `results$component_loadings`       |     |     |     |     | a table  |
| `results$model_performance`        |     |     |     |     | a table  |
| `results$risk_groups`              |     |     |     |     | a table  |
| `results$cross_validation_results` |     |     |     |     | a table  |
| `results$feature_screening_plot`   |     |     |     |     | an image |
| `results$pca_biplot`               |     |     |     |     | an image |
| `results$variance_explained_plot`  |     |     |     |     | an image |
| `results$survival_curves_plot`     |     |     |     |     | an image |
| `results$component_heatmap`        |     |     |     |     | an image |
| `results$clinical_interpretation`  |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$analysis_summary$asDF`

`as.data.frame(results$analysis_summary)`
