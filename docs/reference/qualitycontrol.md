# Laboratory Quality Control Statistics

Comprehensive laboratory quality control statistics including control
charts (Shewhart, CUSUM, EWMA), sigma metrics, method validation
protocols, and reference interval establishment for clinical laboratory
management.

## Usage

``` r
qualitycontrol(
  data,
  measurement_var,
  time_var,
  control_level,
  batch_var,
  analysis_type = "control_charts",
  control_chart_type = "all_charts",
  target_value,
  target_sd,
  control_limits_method = "classical",
  custom_lower_limit,
  custom_upper_limit,
  cusum_target,
  cusum_k_value = 0.5,
  cusum_h_value = 5,
  ewma_lambda = 0.2,
  sigma_calculation = "total_error",
  allowable_error = 10,
  reference_method = "nonparametric",
  reference_percentile = "95",
  outlier_detection = "tukey",
  pt_target_value,
  pt_acceptable_range,
  validation_components = "comprehensive",
  precision_design = "ep5_a3",
  replicate_days = 20,
  replicates_per_day = 2,
  confidence_level = 0.95,
  westgard_rules = TRUE,
  trend_analysis = TRUE,
  capability_analysis = FALSE,
  generate_plots = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- measurement_var:

  Continuous variable containing laboratory measurement values for
  quality control analysis

- time_var:

  Optional time variable for temporal control chart analysis (sequence
  number, date, time)

- control_level:

  Optional variable indicating control material level (e.g., Level 1,
  Level 2, Normal, Pathological)

- batch_var:

  Optional batch or lot identifier for batch-to-batch variation analysis

- analysis_type:

  Primary type of quality control analysis to perform

- control_chart_type:

  Type of control chart to generate for quality control monitoring

- target_value:

  Target value for control charts and sigma metrics (if known)

- target_sd:

  Target standard deviation for sigma metrics (if known)

- control_limits_method:

  Method for calculating control chart limits

- custom_lower_limit:

  Custom lower control limit (when using custom limits method)

- custom_upper_limit:

  Custom upper control limit (when using custom limits method)

- cusum_target:

  Target value for CUSUM chart (defaults to sample mean)

- cusum_k_value:

  CUSUM slack parameter k (typically 0.5 \* sigma)

- cusum_h_value:

  CUSUM decision interval h (typically 4-5 \* sigma)

- ewma_lambda:

  EWMA smoothing parameter lambda (0 \< lambda \<= 1)

- sigma_calculation:

  Method for calculating laboratory sigma metrics

- allowable_error:

  Total allowable error percentage for sigma metrics calculation

- reference_method:

  Statistical method for establishing reference intervals

- reference_percentile:

  Percentile range for reference interval calculation

- outlier_detection:

  Method for detecting and handling outliers in reference interval
  estimation

- pt_target_value:

  Target value for proficiency testing analysis

- pt_acceptable_range:

  Acceptable range for proficiency testing (± percent or ± absolute
  units)

- validation_components:

  Components of analytical method validation to assess

- precision_design:

  Design for precision assessment following laboratory standards

- replicate_days:

  Number of days for precision study (CLSI EP5-A3 recommends \>=20 days)

- replicates_per_day:

  Number of replicates per day for precision study

- confidence_level:

  Confidence level for statistical calculations and intervals

- westgard_rules:

  Apply Westgard multi-rule quality control decision criteria

- trend_analysis:

  Perform statistical trend analysis for systematic changes

- capability_analysis:

  Calculate process capability indices (Cp, Cpk, Pp, Ppk)

- generate_plots:

  Generate comprehensive quality control visualization plots

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$descriptiveStats`       |     |     |     |     | a table  |
| `results$controlChartsResults`   |     |     |     |     | a table  |
| `results$sigmaMetrics`           |     |     |     |     | a table  |
| `results$methodValidation`       |     |     |     |     | a table  |
| `results$precisionComponents`    |     |     |     |     | a table  |
| `results$referenceIntervals`     |     |     |     |     | a table  |
| `results$proficiencyTesting`     |     |     |     |     | a table  |
| `results$westgardRules`          |     |     |     |     | a table  |
| `results$trendAnalysis`          |     |     |     |     | a table  |
| `results$capabilityAnalysis`     |     |     |     |     | a table  |
| `results$outlierAnalysis`        |     |     |     |     | a table  |
| `results$qualityGoals`           |     |     |     |     | a table  |
| `results$controlChart`           |     |     |     |     | an image |
| `results$sigmaMetricsPlot`       |     |     |     |     | an image |
| `results$methodValidationPlot`   |     |     |     |     | an image |
| `results$referenceIntervalsPlot` |     |     |     |     | an image |
| `results$trendPlot`              |     |     |     |     | an image |
| `results$methodExplanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptiveStats$asDF`

`as.data.frame(results$descriptiveStats)`
