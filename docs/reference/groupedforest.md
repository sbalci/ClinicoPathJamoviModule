# Grouped Hazard Forest Plot

Creates grouped hazard regression forest plots to show treatment vs
control comparisons across different variants or subgroups. This module
performs Cox proportional hazards regression separately for each
subgroup defined by a grouping variable and presents the hazard ratios
in a single forest plot. Perfect for showing treatment effects across
patient variants, genetic subtypes, or clinical subgroups.

## Usage

``` r
groupedforest(
  data,
  time_var,
  event_var,
  treatment_var,
  grouping_var,
  covariates,
  reference_treatment = "",
  plot_title = "Grouped Hazard Forest Plot",
  show_overall = TRUE,
  show_statistics = TRUE,
  confidence_level = 0.95,
  sort_by_hr = FALSE,
  show_counts = TRUE,
  plot_theme = "clinical",
  hr_range = "auto",
  custom_hr_min = 0.1,
  custom_hr_max = 10,
  interaction_test = FALSE,
  export_data = FALSE
)
```

## Arguments

- data:

  The data as a data frame containing survival variables.

- time_var:

  Numeric variable representing follow-up time until the event or last
  observation.

- event_var:

  Variable indicating whether the event occurred (1) or was censored
  (0).

- treatment_var:

  Treatment variable (e.g., treatment vs control). This will be the
  primary variable for which hazard ratios are calculated within each
  subgroup.

- grouping_var:

  Variable defining subgroups/variants for separate Cox regression
  analyses. Each level will have its own hazard ratio calculation.

- covariates:

  Optional covariates to include in the Cox regression models for
  adjustment.

- reference_treatment:

  Specify the reference level for treatment variable. If empty, will use
  the first level.

- plot_title:

  Custom title for the forest plot.

- show_overall:

  If TRUE, includes an overall analysis (all groups combined) in the
  forest plot.

- show_statistics:

  If TRUE, displays a detailed statistics table with hazard ratios,
  confidence intervals, and p-values.

- confidence_level:

  Confidence level for confidence intervals (e.g., 0.95 for 95 percent
  CI).

- sort_by_hr:

  If TRUE, sorts subgroups by hazard ratio magnitude in the forest plot.

- show_counts:

  If TRUE, displays sample sizes for each subgroup in the forest plot.

- plot_theme:

  Visual theme for the forest plot.

- hr_range:

  Range for displaying hazard ratios on the x-axis.

- custom_hr_min:

  Custom minimum hazard ratio for x-axis (when HR Range = Custom).

- custom_hr_max:

  Custom maximum hazard ratio for x-axis (when HR Range = Custom).

- interaction_test:

  If TRUE, performs a test for interaction between treatment and
  grouping variable.

- export_data:

  If TRUE, makes forest plot data available for export.

## Value

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$todo`             |     |     |     |     | a html   |
| `results$forest_plot`      |     |     |     |     | an image |
| `results$statistics_table` |     |     |     |     | a html   |
| `results$overall_stats`    |     |     |     |     | a html   |
| `results$interaction_test` |     |     |     |     | a html   |
| `results$sample_sizes`     |     |     |     |     | a html   |
| `results$interpretation`   |     |     |     |     | a html   |
