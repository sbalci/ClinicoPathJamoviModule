# Outlier Detection

Outlier detection using multiple statistical methods from the easystats
performance package. This module provides comprehensive outlier
detection through univariate methods (Z-scores, IQR, confidence
intervals), multivariate methods (Mahalanobis distance, MCD, OPTICS,
LOF), and composite scoring across multiple algorithms. Complements
existing data quality assessment modules with state-of-the-art outlier
detection capabilities. Perfect for clinical research data quality
control and preprocessing.

## Usage

``` r
outlierdetection(
  data,
  vars = NULL,
  method_category = "composite",
  univariate_methods = "zscore_robust",
  multivariate_methods = "mahalanobis",
  composite_threshold = 0.5,
  zscore_threshold = 3.29,
  iqr_multiplier = 1.7,
  confidence_level = 0.999,
  show_outlier_table = TRUE,
  show_method_comparison = FALSE,
  show_exclusion_summary = FALSE,
  show_visualization = FALSE,
  show_interpretation = FALSE,
  sampleThreshold = 10000,
  sampleSize = 5000,
  seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Continuous variables to analyze for outliers. The module will detect
  outliers based on the selected variables using the chosen detection
  methods.

- method_category:

  Category of outlier detection methods to use. CLINICAL GUIDANCE:
  Univariate methods analyze each variable separately (ideal for lab
  values, vital signs). Multivariate methods consider relationships
  between variables (useful for correlated biomarkers). Composite
  combines multiple approaches for robust detection (recommended for
  most clinical data). Examples: Use univariate for hemoglobin levels,
  multivariate for complete blood count panels.

- univariate_methods:

  Specific univariate method for outlier detection when univariate
  category is selected.

- multivariate_methods:

  Specific multivariate method for outlier detection when multivariate
  category is selected.

- composite_threshold:

  Threshold for composite outlier score (0.1-1.0). Default 0.5 means
  observations classified as outliers by at least half of the methods
  are considered outliers.

- zscore_threshold:

  Threshold for Z-score based methods. CLINICAL EXAMPLES: 3.0 = 99.7
  percent confidence (standard screening for most lab values), 3.29 =
  99.9 percent confidence (stringent, for critical values like cardiac
  enzymes), 2.5 = 98.8 percent confidence (sensitive detection for
  research). Recommended: 3.29 for clinical quality control.

- iqr_multiplier:

  Multiplier for IQR-based outlier detection. CLINICAL EXAMPLES: 1.5 =
  Tukey's standard (sensitive, may flag ~0.7 percent of normal data),
  1.7 = conservative (recommended for clinical screening), 2.0 = very
  conservative (for critical biomarkers). Useful for non-normal
  distributions common in clinical data.

- confidence_level:

  Confidence level for interval-based methods (ETI, HDI). Default 99.9
  percent identifies the most extreme observations.

- show_outlier_table:

  Display a comprehensive table of outlier detection results including
  outlier scores, distances, and classification for each observation.

- show_method_comparison:

  Compare results across different detection methods when using
  composite approach.

- show_exclusion_summary:

  Provide summary of observations recommended for exclusion and impact
  analysis.

- show_visualization:

  Generate plots showing outlier detection results and distribution of
  outlier scores.

- show_interpretation:

  Display detailed interpretation of outlier detection results and
  methodological notes.

- sampleThreshold:

  Row count above which a random subsample is analysed instead of the
  full dataset. Datasets at or below this size are always analysed in
  full. Raise it to analyse a large dataset completely at the cost of
  speed.

- sampleSize:

  Number of rows retained when subsampling. Outliers among the rows that
  are not sampled cannot be detected, so the reported count is a lower
  bound; larger values recover more of them at the cost of speed.

- seed:

  Random seed used for the reproducible random subsample that is drawn
  when the dataset exceeds the subsampling threshold. Change this value
  to draw a different sample; the default of 123 reproduces the previous
  fixed behaviour.

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$warnings`          |     |     |     |     | a html   |
| `results$plot`              |     |     |     |     | an image |
| `results$outlier_table`     |     |     |     |     | a html   |
| `results$method_comparison` |     |     |     |     | a html   |
| `results$exclusion_summary` |     |     |     |     | a html   |
| `results$interpretation`    |     |     |     |     | a html   |
