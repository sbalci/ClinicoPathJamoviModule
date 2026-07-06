# Smooth Hazard Estimation & Analysis

Smooth Hazard Estimation & Analysis

## Usage

``` r
smoothhazard(
  data,
  time_var,
  status_var,
  covariates,
  strata_var,
  method = "kernel",
  bandwidth = 0,
  bandwidth_method = "automatic",
  confidence_level = 0.95,
  time_grid = 50,
  boundary_correction = TRUE,
  kernel_type = "epanechnikov",
  hazard_plot = TRUE,
  cumulative_hazard_plot = FALSE,
  confidence_bands = TRUE,
  comparison_plot = FALSE,
  diagnostic_plots = FALSE,
  hazard_summary = TRUE,
  peak_analysis = FALSE,
  model_comparison = FALSE,
  export_hazard = FALSE,
  bootstrap_ci = FALSE,
  bootstrap_samples = 500
)
```

## Arguments

- data:

  the data as a data frame

- time_var:

  Time to event or censoring variable

- status_var:

  Event status variable (1=event, 0=censored)

- covariates:

  Covariates for stratified hazard estimation

- strata_var:

  Variable for stratified analysis

- method:

  Method for hazard function smoothing

- bandwidth:

  Bandwidth parameter for smoothing (0 = automatic selection)

- bandwidth_method:

  Method for bandwidth selection

- confidence_level:

  Confidence level for confidence intervals

- time_grid:

  Number of time points for hazard estimation

- boundary_correction:

  Apply boundary correction for hazard estimation

- kernel_type:

  Kernel function for smoothing

- hazard_plot:

  Generate hazard function plot

- cumulative_hazard_plot:

  Generate cumulative hazard function plot

- confidence_bands:

  Include confidence bands in plots

- comparison_plot:

  Compare different smoothing methods

- diagnostic_plots:

  Generate diagnostic plots for bandwidth selection

- hazard_summary:

  Summary statistics for estimated hazard function

- peak_analysis:

  Identify and analyze hazard function peaks

- model_comparison:

  Compare with parametric models

- export_hazard:

  Export hazard function estimates to results

- bootstrap_ci:

  Use bootstrap for confidence interval estimation

- bootstrap_samples:

  Number of bootstrap samples for confidence intervals

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Instructions and guidance for smooth hazard estimation |
| `results$data_summary` |  |  |  |  | Summary of input data and analysis parameters |
| `results$hazard_estimates` |  |  |  |  | Estimated hazard function values at specified time points |
| `results$hazard_summary` |  |  |  |  | Summary statistics for the estimated hazard function |
| `results$bandwidth_selection` |  |  |  |  | Results from bandwidth selection procedure |
| `results$hazard_plot` |  |  |  |  | Plot of the estimated hazard function over time |
| `results$cumulative_hazard_plot` |  |  |  |  | Plot of the estimated cumulative hazard function |
| `results$comparison_plot` |  |  |  |  | Comparison of different smoothing methods |
| `results$diagnostic_plots` |  |  |  |  | Diagnostic plots for bandwidth selection |
| `results$peak_analysis` |  |  |  |  | Analysis of hazard function peaks and inflection points |
| `results$model_comparison` |  |  |  |  | Comparison with parametric survival models |
| `results$bootstrap_results` |  |  |  |  | Bootstrap confidence interval results |
| `results$hazard_export` |  |  |  |  | Detailed hazard function estimates for export |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$hazard_estimates$asDF`

`as.data.frame(results$hazard_estimates)`
