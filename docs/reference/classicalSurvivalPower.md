# Classical Survival Power Analysis (Intermediate)

Legacy power analysis and sample size calculation for survival studies
using Lachin-Foulkes and Schoenfeld methods from gsDesign package.

## Usage

``` r
classicalSurvivalPower(
  data,
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,
  hazard_treatment = 0.042,
  hazard_ratio = 0.6,
  study_duration = 24,
  accrual_duration = 12,
  dropout_rate = 0,
  allocation_ratio = 1,
  alpha = 0.025,
  beta = 0.1,
  power = 0.9,
  sided = "one_sided",
  entry_type = "unif",
  gamma = 0,
  sample_size_input = 100,
  events_input = 50,
  show_summary = TRUE,
  show_formulas = FALSE,
  show_interpretation = TRUE,
  show_power_plot = FALSE,
  show_timeline_plot = FALSE,
  power_plot_range = "auto",
  export_results = FALSE,
  export_power_curve = FALSE
)
```

## Arguments

- data:

  The data as a data frame (optional for power calculations).

- calculation_type:

  Type of power calculation to perform.

- method:

  Method for power/sample size calculation.

- hazard_control:

  Event hazard rate for control group (events per person-time unit).

- hazard_treatment:

  Event hazard rate for treatment group (events per person-time unit).

- hazard_ratio:

  Hazard ratio (treatment/control) for Schoenfeld method.

- study_duration:

  Maximum study duration (months or years).

- accrual_duration:

  Patient accrual (recruitment) duration.

- dropout_rate:

  Equal dropout hazard rate for both groups.

- allocation_ratio:

  Randomization ratio (treatment:control).

- alpha:

  Type I error rate (significance level).

- beta:

  Type II error rate (1 - power).

- power:

  Statistical power (1 - beta).

- sided:

  One-sided or two-sided statistical test.

- entry_type:

  Pattern of patient entry into the study.

- gamma:

  Rate parameter for exponential entry (0 if uniform entry).

- sample_size_input:

  Total sample size when calculating power.

- events_input:

  Number of events when calculating power with Schoenfeld method.

- show_summary:

  Whether to display comprehensive study design summary.

- show_formulas:

  Whether to display mathematical formulas used.

- show_interpretation:

  Whether to include clinical interpretation of results.

- show_power_plot:

  Whether to display power curve visualization.

- show_timeline_plot:

  Whether to display study timeline visualization for Lachin-Foulkes
  method.

- power_plot_range:

  Sample size range for power plots (format 'min,max' or 'auto').

- export_results:

  Whether to export detailed results for external analysis.

- export_power_curve:

  Whether to export power curve data points.

## Value

A results object containing:

|                                |     |     |     |     |           |
|--------------------------------|-----|-----|-----|-----|-----------|
| `results$instructions`         |     |     |     |     | a html    |
| `results$power_results`        |     |     |     |     | a html    |
| `results$formulas`             |     |     |     |     | a html    |
| `results$interpretation`       |     |     |     |     | a html    |
| `results$power_plot`           |     |     |     |     | an image  |
| `results$timeline_plot`        |     |     |     |     | an image  |
| `results$exported_results`     |     |     |     |     | an output |
| `results$exported_power_curve` |     |     |     |     | an output |
| `results$export_summary`       |     |     |     |     | a html    |

## Details

This is the original survival power analysis function. For enhanced
functionality including competing risks, RMST analysis, and additional
methods, also consider 'Comprehensive Survival Power Analysis'.
