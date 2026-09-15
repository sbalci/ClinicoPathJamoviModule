# Six Sigma Metrics for Laboratory Performance

Six Sigma methodology for laboratory performance evaluation and quality
assessment. Calculates sigma metrics, process capability indices, and
quality goal index (QGI) for analytical methods. Supports ISO 15189
compliance and quality improvement initiatives. Essential for laboratory
method validation and performance monitoring.

## Usage

``` r
sigmametrics(
  data,
  bias,
  cv,
  tea,
  analyte,
  laboratory,
  method,
  sigma_calculation = "standard",
  quality_goals = "clia",
  custom_tea = 10,
  confidence_level = 0.95,
  benchmark_comparison = TRUE,
  process_capability = TRUE,
  quality_goal_index = TRUE,
  method_validation = TRUE,
  improvement_recommendations = TRUE,
  regulatory_compliance = TRUE,
  cost_of_quality = FALSE,
  defect_rates = TRUE,
  control_planning = FALSE,
  sigma_plots = TRUE,
  normalized_plots = TRUE,
  quality_scorecard = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- bias:

  Method bias as percentage from target value

- cv:

  Coefficient of variation (precision) as percentage

- tea:

  Total allowable error specification as percentage

- analyte:

  Analyte or test name for identification

- laboratory:

  Laboratory or site identifier for comparative analysis

- method:

  Analytical method identifier

- sigma_calculation:

  Method for calculating sigma metrics

- quality_goals:

  Source for quality goal specifications

- custom_tea:

  Custom total allowable error specification

- confidence_level:

  Confidence level for uncertainty calculations

- benchmark_comparison:

  Compare against benchmark sigma levels

- process_capability:

  Calculate Cp, Cpk, and other capability indices

- quality_goal_index:

  Calculate quality goal index metrics

- method_validation:

  Evaluate method performance for validation

- improvement_recommendations:

  Provide specific improvement recommendations

- regulatory_compliance:

  Assess compliance with regulatory standards

- cost_of_quality:

  Estimate cost implications of quality levels

- defect_rates:

  Calculate expected defect rates per million

- control_planning:

  Generate QC planning recommendations

- sigma_plots:

  Create sigma metric and capability plots

- normalized_plots:

  Generate normalized operating specifications chart

- quality_scorecard:

  Create comprehensive quality scorecard visualization

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`               |     |     |     |     | a html   |
| `results$dataInfo`                   |     |     |     |     | a table  |
| `results$sigmaResults`               |     |     |     |     | a table  |
| `results$processCapability`          |     |     |     |     | a table  |
| `results$qualityGoalIndex`           |     |     |     |     | a table  |
| `results$benchmarkComparison`        |     |     |     |     | a table  |
| `results$methodValidation`           |     |     |     |     | a table  |
| `results$regulatoryCompliance`       |     |     |     |     | a table  |
| `results$improvementRecommendations` |     |     |     |     | a table  |
| `results$defectRateAnalysis`         |     |     |     |     | a table  |
| `results$costOfQuality`              |     |     |     |     | a table  |
| `results$controlPlanning`            |     |     |     |     | a table  |
| `results$sigmaPlot`                  |     |     |     |     | an image |
| `results$normalizedChart`            |     |     |     |     | an image |
| `results$qualityScorecard`           |     |     |     |     | an image |
| `results$methodExplanation`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
