# Laboratory Control Charts (Shewhart, CUSUM, EWMA)

Statistical process control charts for laboratory quality monitoring.
Implements Shewhart charts, CUSUM (Cumulative Sum), and EWMA
(Exponentially Weighted Moving Average) charts for detecting systematic
errors, shifts, and trends in laboratory measurements. Essential for ISO
15189 compliance and quality assurance programs.

## Usage

``` r
labcontrolcharts(
  data,
  measurement,
  run_number,
  batch_id,
  control_level,
  chart_type = "shewhart",
  control_limits = "3sigma",
  target_mean = 0,
  target_sd = 0,
  cusum_k = 0.5,
  cusum_h = 4,
  ewma_lambda = 0.2,
  westgard_rules = "13s,22s,R4s",
  baseline_runs = 20,
  violation_detection = TRUE,
  trend_analysis = TRUE,
  shift_detection = TRUE,
  performance_metrics = TRUE,
  corrective_actions = TRUE,
  qc_summary = TRUE,
  export_violations = FALSE,
  control_plots = TRUE,
  histogram_plot = TRUE,
  trend_plot = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- measurement:

  Laboratory measurement values to monitor

- run_number:

  Sequential run number or time point identifier

- batch_id:

  Batch or lot identifier for stratified analysis

- control_level:

  Control material level (e.g., Level 1, Level 2, Level 3)

- chart_type:

  Type of control chart to generate

- control_limits:

  Method for calculating control limits

- target_mean:

  Target or expected mean value (0 = calculate from data)

- target_sd:

  Target or expected standard deviation (0 = calculate from data)

- cusum_k:

  CUSUM reference value (typically 0.5σ)

- cusum_h:

  CUSUM decision interval (typically 4-5σ)

- ewma_lambda:

  EWMA smoothing parameter (0.05-0.25 typical)

- westgard_rules:

  Comma-separated Westgard rules (e.g., "13s,22s,R4s,41s,10x")

- baseline_runs:

  Number of initial runs to establish baseline

- violation_detection:

  Identify and mark control rule violations

- trend_analysis:

  Perform trend detection and analysis

- shift_detection:

  Detect systematic shifts in measurements

- performance_metrics:

  Calculate ARL, power, and false alarm rates

- corrective_actions:

  Provide corrective action recommendations

- qc_summary:

  Calculate comprehensive QC statistics

- export_violations:

  Export detailed list of violations

- control_plots:

  Generate control chart visualizations

- histogram_plot:

  Create measurement distribution histogram

- trend_plot:

  Generate trend analysis visualization

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$dataInfo`               |     |     |     |     | a table  |
| `results$controlLimits`          |     |     |     |     | a table  |
| `results$violationSummary`       |     |     |     |     | a table  |
| `results$violationDetails`       |     |     |     |     | a table  |
| `results$trendAnalysis`          |     |     |     |     | a table  |
| `results$shiftDetection`         |     |     |     |     | a table  |
| `results$performanceMetrics`     |     |     |     |     | a table  |
| `results$qcStatistics`           |     |     |     |     | a table  |
| `results$correctiveActions`      |     |     |     |     | a table  |
| `results$westgardInterpretation` |     |     |     |     | a table  |
| `results$shewhartChart`          |     |     |     |     | an image |
| `results$cusumChart`             |     |     |     |     | an image |
| `results$ewmaChart`              |     |     |     |     | an image |
| `results$distributionPlot`       |     |     |     |     | an image |
| `results$trendPlot`              |     |     |     |     | an image |
| `results$methodExplanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
