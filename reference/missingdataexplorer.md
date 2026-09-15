# Missing Data Pattern Explorer

Comprehensive missing data pattern exploration and visualization for
clinical research. Analyzes missing data mechanisms, patterns, and
correlations to inform imputation strategies. Includes advanced
diagnostics for missing completely at random (MCAR), missing at random
(MAR), and missing not at random (MNAR) assessments. Essential for
understanding missingness before imputation and for regulatory
documentation of missing data handling strategies.

## Usage

``` r
missingdataexplorer(
  data,
  analysis_vars,
  group_var,
  time_var,
  id_var,
  pattern_analysis = TRUE,
  mechanism_testing = TRUE,
  correlation_analysis = TRUE,
  temporal_analysis = FALSE,
  group_comparison = FALSE,
  mcar_test = "little",
  min_pattern_freq = 0.05,
  max_patterns_display = 20,
  pattern_plot = TRUE,
  correlation_plot = TRUE,
  temporal_plot = FALSE,
  upset_plot = TRUE,
  cumulative_plot = FALSE,
  monotonic_test = TRUE,
  dropout_analysis = FALSE,
  informative_missingness = TRUE,
  completeness_threshold = 0.8,
  case_completeness = TRUE,
  variable_importance = TRUE,
  chi_square_test = TRUE,
  logistic_regression = FALSE,
  survival_analysis = FALSE,
  detailed_patterns = TRUE,
  summary_statistics = TRUE,
  clinical_interpretation = TRUE,
  export_patterns = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- analysis_vars:

  Variables to include in missing data pattern analysis

- group_var:

  Grouping variable for comparing missingness patterns (e.g., treatment
  arm, study site)

- time_var:

  Time variable for temporal missingness analysis (e.g., visit number,
  time since baseline)

- id_var:

  Subject identifier for longitudinal missingness analysis

- pattern_analysis:

  Analyze missing data patterns and frequencies

- mechanism_testing:

  Test missing data mechanisms (MCAR, MAR, MNAR)

- correlation_analysis:

  Analyze correlations between missingness indicators

- temporal_analysis:

  Analyze missingness patterns over time

- group_comparison:

  Compare missingness patterns between groups

- mcar_test:

  Method for testing Missing Completely At Random assumption

- min_pattern_freq:

  Minimum frequency for reporting missing data patterns (5 percent =
  0.05)

- max_patterns_display:

  Maximum number of missing data patterns to display in detail

- pattern_plot:

  Generate missing data pattern plots

- correlation_plot:

  Generate missingness correlation heatmap

- temporal_plot:

  Generate temporal missingness plots

- upset_plot:

  Generate UpSet plot for pattern visualization

- cumulative_plot:

  Generate cumulative missingness over variables plot

- monotonic_test:

  Test whether missingness follows monotonic patterns

- dropout_analysis:

  Analyze dropout patterns in longitudinal data

- informative_missingness:

  Test whether missingness is informative (related to outcomes)

- completeness_threshold:

  Threshold for acceptable variable completeness (80 percent = 0.8)

- case_completeness:

  Analyze completeness at the case/subject level

- variable_importance:

  Assess which variables predict missingness in others

- chi_square_test:

  Chi-square tests for independence of missingness

- logistic_regression:

  Logistic regression to predict missingness patterns

- survival_analysis:

  Survival analysis for time to dropout/missingness

- detailed_patterns:

  Show detailed information for each missing data pattern

- summary_statistics:

  Include comprehensive summary statistics

- clinical_interpretation:

  Provide clinical interpretation and recommendations

- export_patterns:

  Export missing data patterns for external analysis

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$summary_statistics` |     |     |     |     | a table  |
| `results$mcar_test_results`  |     |     |     |     | a table  |
| `results$missing_patterns`   |     |     |     |     | a table  |
| `results$correlation_matrix` |     |     |     |     | a table  |
| `results$pattern_plot`       |     |     |     |     | an image |
| `results$upset_plot`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary_statistics$asDF`

`as.data.frame(results$summary_statistics)`
