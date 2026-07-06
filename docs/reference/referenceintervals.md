# Reference Interval Establishment

Robust statistical methods for establishing clinical reference intervals
following CLSI EP28-A3c guidelines. Supports parametric and
non-parametric approaches, robust methods for outlier handling, and
partitioning analysis for demographic factors. Essential for laboratory
standardization and clinical interpretation of test results.

## Usage

``` r
referenceintervals(
  data,
  measurement,
  age,
  gender,
  ethnicity,
  additional_factors,
  ri_method = "robust_nonparametric",
  confidence_level = 0.95,
  reference_percentiles = "2.5,97.5",
  minimum_sample_size = 120,
  outlier_detection = "horn",
  transformation_test = TRUE,
  partitioning_analysis = TRUE,
  age_partitioning = "none",
  age_breakpoints = "18,65",
  harris_boyd_test = TRUE,
  verification_study = FALSE,
  transferability_assessment = FALSE,
  uncertainty_estimation = TRUE,
  clinical_interpretation = TRUE,
  quality_assessment = TRUE,
  literature_comparison = FALSE,
  distribution_plots = TRUE,
  partitioning_plots = TRUE,
  age_trend_plots = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- measurement:

  Laboratory test measurement values from reference population

- age:

  Age of reference subjects for age-specific intervals

- gender:

  Gender for sex-specific reference intervals

- ethnicity:

  Ethnicity for population-specific intervals

- additional_factors:

  Additional variables for partitioning analysis

- ri_method:

  Statistical method for calculating reference intervals

- confidence_level:

  Confidence level for reference interval limits

- reference_percentiles:

  Lower and upper percentiles for reference interval

- minimum_sample_size:

  Minimum required sample size (CLSI recommends=120)

- outlier_detection:

  Method for identifying and handling outliers

- transformation_test:

  Test normality and suggest appropriate transformations

- partitioning_analysis:

  Analyze need for demographic partitioning

- age_partitioning:

  Method for age-related partitioning

- age_breakpoints:

  Comma-separated age breakpoints for custom grouping

- harris_boyd_test:

  Apply Harris-Boyd test for need to partition

- verification_study:

  Perform verification with independent samples

- transferability_assessment:

  Assess transferability to different populations

- uncertainty_estimation:

  Estimate uncertainty of reference interval limits

- clinical_interpretation:

  Provide clinical interpretation guidelines

- quality_assessment:

  Assess quality of reference interval establishment

- literature_comparison:

  Compare with published reference intervals

- distribution_plots:

  Generate distribution and normality plots

- partitioning_plots:

  Create partitioning visualization plots

- age_trend_plots:

  Generate age-related trend visualizations

## Value

A results object containing:

|                          |     |     |     |     |         |
|--------------------------|-----|-----|-----|-----|---------|
| `results$instructions`   |     |     |     |     | a html  |
| `results$summary`        |     |     |     |     | a table |
| `results$riResults`      |     |     |     |     | a table |
| `results$normalityTest`  |     |     |     |     | a table |
| `results$referencePaths` |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
