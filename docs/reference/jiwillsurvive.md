# Intuitive Survival Analysis

Intuitive survival analysis using the iwillsurvive package with
user-friendly interface, automatic data preparation, and comprehensive
visualization options.

## Usage

``` r
jiwillsurvive(
  data,
  analysis_type = "survival_model",
  time_var,
  event_var,
  group_var,
  start_date_var,
  end_date_var,
  derive_followup = FALSE,
  followup_units = "days",
  confidence_level = 0.95,
  show_risk_table = TRUE,
  show_median_survival = TRUE,
  show_confidence_bands = TRUE,
  show_censoring_marks = TRUE,
  plot_style = "iwillsurvive",
  color_palette = "default",
  plot_title = "",
  x_label = "",
  y_label = "",
  time_breaks = "",
  legend_position = "right",
  followup_plot_type = "histogram",
  show_statistics = TRUE,
  show_survival_table = TRUE,
  show_interpretation = TRUE,
  time_points = "",
  export_data = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- analysis_type:

  Type of survival analysis to perform.

- time_var:

  Time-to-event or follow-up time variable.

- event_var:

  Event indicator variable (1=event, 0=censored).

- group_var:

  Variable for group comparison.

- start_date_var:

  Start date for follow-up calculation.

- end_date_var:

  End date for follow-up calculation.

- derive_followup:

  Whether to automatically derive follow-up time from dates.

- followup_units:

  Units for follow-up time calculation.

- confidence_level:

  Confidence level for survival estimates.

- show_risk_table:

  Whether to display risk table below survival plot.

- show_median_survival:

  Whether to display median survival times.

- show_confidence_bands:

  Whether to display confidence intervals around survival curves.

- show_censoring_marks:

  Whether to mark censored observations on survival curves.

- plot_style:

  Visual style for survival plots.

- color_palette:

  Color palette for group comparisons.

- plot_title:

  Custom title for the survival plot.

- x_label:

  Custom label for time axis.

- y_label:

  Custom label for survival probability axis.

- time_breaks:

  Custom time points for axis (comma-separated).

- legend_position:

  Position of the legend in the plot.

- followup_plot_type:

  Type of follow-up visualization.

- show_statistics:

  Whether to display statistical test results.

- show_survival_table:

  Whether to display survival summary table.

- show_interpretation:

  Whether to include clinical interpretation.

- time_points:

  Specific time points for survival estimates (comma-separated).

- export_data:

  Whether to include processed data in output.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$instructions`   |     |     |     |     | a html   |
| `results$survivalPlot`   |     |     |     |     | an image |
| `results$survivalStats`  |     |     |     |     | a html   |
| `results$survivalTable`  |     |     |     |     | a table  |
| `results$interpretation` |     |     |     |     | a html   |
| `results$kmPlot`         |     |     |     |     | an image |
| `results$kmStats`        |     |     |     |     | a html   |
| `results$kmTable`        |     |     |     |     | a table  |
| `results$followupPlot`   |     |     |     |     | an image |
| `results$prepText`       |     |     |     |     | a html   |
| `results$dataOutput`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$survivalTable$asDF`

`as.data.frame(results$survivalTable)`
