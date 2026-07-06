# Batch Effect Control & Quality Assessment

Advanced batch effect detection and correction for tabular data
analysis. Essential for digital pathology and multi-institutional
studies where technical variation can confound biological signals
("garbage in, garbage out").

## Usage

``` r
batcheffect(
  data,
  features,
  batch_var,
  biological_var,
  perform_pca = TRUE,
  perform_combat = TRUE,
  feature_quality = TRUE,
  redundancy_analysis = TRUE,
  variance_threshold = 0.01,
  correlation_threshold = 0.95,
  outlier_method = "robust",
  outlier_threshold = 3,
  pca_components = 3,
  missing_threshold = 20,
  show_plots = TRUE,
  combat_parametric = TRUE,
  save_corrected = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- features:

  Numeric feature variables to assess for batch effects (e.g.,
  biomarkers, image features)

- batch_var:

  Categorical variable indicating batch/technical groups (e.g.,
  institution, date, instrument)

- biological_var:

  Optional biological grouping variable to preserve during batch
  correction

- perform_pca:

  Perform Principal Component Analysis to visualize batch effects

- perform_combat:

  Apply ComBat correction to remove batch effects while preserving
  biological variation

- feature_quality:

  Assess individual feature quality including distribution analysis and
  outlier detection

- redundancy_analysis:

  Analyze feature redundancy and correlation structure

- variance_threshold:

  Threshold for removing low-variance features (0 = no filtering)

- correlation_threshold:

  Threshold for identifying highly correlated redundant features

- outlier_method:

  Method for outlier detection in feature quality assessment

- outlier_threshold:

  Threshold for outlier detection (standard deviations or IQR
  multiplier)

- pca_components:

  Number of principal components to compute and visualize

- missing_threshold:

  Maximum percentage of missing values allowed per feature

- show_plots:

  Generate diagnostic plots including PCA plots and quality assessments

- combat_parametric:

  Use parametric ComBat (faster) vs non-parametric (more robust)

- save_corrected:

  Add ComBat-corrected features to the dataset

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Instructions for batch effect analysis and quality control |
| `results$summary` |  |  |  |  | Overall summary of data quality and batch effect assessment |
| `results$batch_detection` |  |  |  |  | Statistical tests and metrics for batch effect detection |
| `results$feature_quality` |  |  |  |  | Quality metrics for individual features |
| `results$redundancy` |  |  |  |  | Analysis of highly correlated and redundant features |
| `results$combat_results` |  |  |  |  | Results and effectiveness of ComBat batch correction |
| `results$pca_plot` |  |  |  |  | PCA plots showing batch effects before and after correction |
| `results$quality_plot` |  |  |  |  | Distribution and quality assessment plots for features |
| `results$correlation_plot` |  |  |  |  | Correlation matrix heatmap for redundancy analysis |
| `results$interpretation` |  |  |  |  | Clinical context and quality control recommendations |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`

## Details

Implements PCA visualization, ComBat correction, feature quality
assessment, and comprehensive quality control metrics for
high-dimensional data.
