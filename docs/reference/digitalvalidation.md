# Digital Pathology Validation

Digital Pathology Validation

## Usage

``` r
digitalvalidation(
  data,
  reference,
  test,
  platform_var,
  validation_type = "both",
  acceptance_criteria = "fda_standard",
  custom_correlation_threshold = 0.9,
  custom_icc_threshold = 0.75,
  bias_assessment = TRUE,
  decision_threshold1 = 0,
  decision_threshold2 = 0,
  decision_threshold3 = 0,
  bootstrap_ci = TRUE,
  bootstrap_n = 1000,
  show_validation_plots = TRUE,
  generate_report = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- reference:

  reference/gold standard measurements

- test:

  test method measurements to be validated

- platform_var:

  platform or batch identifier for stratified analysis

- validation_type:

  type of validation analysis to perform

- acceptance_criteria:

  regulatory acceptance criteria for validation

- custom_correlation_threshold:

  minimum acceptable correlation coefficient

- custom_icc_threshold:

  minimum acceptable ICC value

- bias_assessment:

  perform systematic and proportional bias assessment

- decision_threshold1:

  first clinical threshold for decision impact analysis

- decision_threshold2:

  second clinical threshold for decision impact analysis

- decision_threshold3:

  third clinical threshold for decision impact analysis

- bootstrap_ci:

  use bootstrap method for robust confidence intervals

- bootstrap_n:

  number of bootstrap replicates

- show_validation_plots:

  display validation and residual plots

- generate_report:

  generate comprehensive validation report

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$interpretation` |  |  |  |  | a html |
| `results$performancetable` |  |  |  |  | Comprehensive validation performance metrics |
| `results$biastable` |  |  |  |  | Systematic and proportional bias analysis |
| `results$agreementtable` |  |  |  |  | ICC, CCC, and correlation measures |
| `results$thresholdtable` |  |  |  |  | Impact at different clinical thresholds |
| `results$validationplot` |  |  |  |  | Scatter plot with regression and agreement lines |
| `results$blandaltmanplot` |  |  |  |  | Agreement analysis with limits of agreement |
| `results$residualplot` |  |  |  |  | Residual patterns for bias detection |
| `results$validationreport` |  |  |  |  | Complete validation report with regulatory framework |
| `results$acceptancesummary` |  |  |  |  | Pass/fail status for regulatory criteria |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performancetable$asDF`

`as.data.frame(results$performancetable)`
