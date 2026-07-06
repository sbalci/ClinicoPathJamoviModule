# Method Comparison Analysis

Comprehensive method comparison analysis including Bland-Altman plots,
Passing-Bablok regression, and Deming regression. Essential for
validating new laboratory methods, diagnostic tests, and measurement
techniques.

## Usage

``` r
methodcomparison(
  data,
  method1,
  method2,
  grouping,
  comparison_method = "bland_altman",
  confidence_level = 0.95,
  bland_altman_options = TRUE,
  limits_method = "standard",
  proportional_bias = TRUE,
  outlier_detection = TRUE,
  passing_bablok_options = FALSE,
  pb_alpha = 0.05,
  deming_options = FALSE,
  error_ratio = 1,
  correlation_analysis = TRUE,
  concordance_correlation = TRUE,
  regression_comparison = TRUE,
  calibration_analysis = FALSE,
  clinical_limits = FALSE,
  clinical_lower = -10,
  clinical_upper = 10,
  transformation = "none",
  plots_options = TRUE,
  bland_altman_plot = TRUE,
  scatter_plot = TRUE,
  residual_plots = FALSE,
  mountain_plot = FALSE,
  missing_treatment = "complete",
  bootstrap_samples = 1000
)
```

## Arguments

- data:

  The data as a data frame.

- method1:

  .

- method2:

  .

- grouping:

  .

- comparison_method:

  Primary method comparison approach

- confidence_level:

  Confidence level for limits of agreement and regression

- bland_altman_options:

  Enable Bland-Altman specific analysis options

- limits_method:

  Method for calculating limits of agreement

- proportional_bias:

  Test if bias changes with measurement magnitude

- outlier_detection:

  Identify potential outliers in method comparison

- passing_bablok_options:

  Enable Passing-Bablok specific analysis options

- pb_alpha:

  Significance level for Passing-Bablok regression

- deming_options:

  Enable Deming regression specific options

- error_ratio:

  Ratio of error variances (σ²y/σ²x) for Deming regression

- correlation_analysis:

  Include Pearson and Spearman correlation analysis

- concordance_correlation:

  Calculate Lin concordance correlation coefficient for agreement
  assessment

- regression_comparison:

  Compare ordinary least squares vs method comparison regressions

- calibration_analysis:

  Compute calibration metrics assessing how well the test method tracks
  the reference method. Includes calibration-in-the-large (intercept;
  ideal = 0), calibration slope (ideal = 1), and R-squared. A
  well-calibrated method has slope near 1 and intercept near 0. Useful
  for validating AI scoring tools, automated assays, and new measurement
  methods against gold standards.

- clinical_limits:

  Specify clinical acceptability criteria

- clinical_lower:

  Lower bound for clinically acceptable difference

- clinical_upper:

  Upper bound for clinically acceptable difference

- transformation:

  Apply transformation to improve normality

- plots_options:

  Control plot generation and formatting

- bland_altman_plot:

  Generate Bland-Altman plot with limits of agreement

- scatter_plot:

  Scatter plot with regression lines

- residual_plots:

  Generate residual plots for regression diagnostics

- mountain_plot:

  Generate mountain plot (percentile-based differences)

- missing_treatment:

  How to handle missing data

- bootstrap_samples:

  Number of bootstrap samples for confidence intervals

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                 |     |     |     |     | a html   |
| `results$summary`              |     |     |     |     | a html   |
| `results$descriptiveStats`     |     |     |     |     | a table  |
| `results$correlationTable`     |     |     |     |     | a table  |
| `results$concordanceTable`     |     |     |     |     | a table  |
| `results$blandAltmanStats`     |     |     |     |     | a table  |
| `results$passingBablokResults` |     |     |     |     | a table  |
| `results$demingResults`        |     |     |     |     | a table  |
| `results$regressionComparison` |     |     |     |     | a table  |
| `results$biasAnalysis`         |     |     |     |     | a table  |
| `results$outlierAnalysis`      |     |     |     |     | a table  |
| `results$clinicalAssessment`   |     |     |     |     | a table  |
| `results$blandAltmanPlot`      |     |     |     |     | an image |
| `results$scatterPlot`          |     |     |     |     | an image |
| `results$residualPlots`        |     |     |     |     | an image |
| `results$mountainPlot`         |     |     |     |     | an image |
| `results$calibrationTable`     |     |     |     |     | a table  |
| `results$methodGuidance`       |     |     |     |     | a html   |
| `results$technicalNotes`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptiveStats$asDF`

`as.data.frame(results$descriptiveStats)`
