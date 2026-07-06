# Clinical Research Visualization with visR

Clinical and medical research focused visualizations using the visR
package with sensible defaults based on graphical principles.

## Usage

``` r
jvisr(
  data,
  analysis_type = "kaplan_meier",
  time_var,
  event_var,
  strata_var,
  cdisc_format = FALSE,
  aval_var,
  cnsr_var,
  fun_type = "surv",
  confidence_interval = TRUE,
  risk_table = TRUE,
  quantiles = FALSE,
  p_value = TRUE,
  legend_position = "right",
  time_label = "Time",
  time_units = "",
  survival_label = "",
  title = "",
  theme_style = "visr",
  color_palette = "default",
  show_summary = TRUE,
  show_interpretation = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- analysis_type:

  Type of clinical visualization to generate.

- time_var:

  Time-to-event variable for survival analysis.

- event_var:

  Event indicator variable (1=event, 0=censored).

- strata_var:

  Variable for stratified analysis.

- cdisc_format:

  Whether to assume CDISC ADaM ADTTE data format.

- aval_var:

  Analysis value for CDISC format (time variable).

- cnsr_var:

  Censor variable for CDISC format (1=censored, 0=event).

- fun_type:

  Function scale for survival plots.

- confidence_interval:

  Whether to display confidence intervals.

- risk_table:

  Whether to include risk table below plot.

- quantiles:

  Whether to display survival quantiles.

- p_value:

  Whether to display statistical p-value.

- legend_position:

  Position of the legend in the plot.

- time_label:

  Label for the time axis.

- time_units:

  Units for time measurement.

- survival_label:

  Label for the survival probability axis.

- title:

  Main title for the plot.

- theme_style:

  Visual theme for the plot.

- color_palette:

  Color palette for group stratification.

- show_summary:

  Whether to display summary statistics table.

- show_interpretation:

  Whether to include clinical interpretation.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$plot` |  |  |  |  | Main clinical visualization plot using visR package |
| `results$summary` |  |  |  |  | Summary statistics and data information |
| `results$interpretation` |  |  |  |  | Clinical interpretation of the results |
