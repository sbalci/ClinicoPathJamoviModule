# Spatial Bayesian Survival Analysis

Advanced spatial Bayesian survival analysis for modeling geographic
patterns in survival outcomes. Incorporates spatial correlation
structures, disease mapping capabilities, and hierarchical Bayesian
modeling for clinical research with geographic components.

## Usage

``` r
spatialbayesiansurvival(
  data,
  time,
  status,
  statusLevel,
  predictors,
  spatial_coords,
  region_id = NULL,
  adjacency_matrix = "",
  spatial_model = "car",
  spatial_prior = "icar",
  distance_method = "great_circle",
  neighborhood_threshold = 50,
  num_neighbors = 5,
  baseline_hazard = "weibull",
  piecewise_intervals = 5,
  spline_knots = 5,
  mcmc_samples = 5000,
  mcmc_burnin = 2000,
  mcmc_thin = 1,
  mcmc_chains = 3,
  covariate_priors = "normal_weak",
  covariate_prior_sd = 10,
  spatial_precision_prior = "gamma",
  spatial_precision_shape = 1,
  spatial_precision_rate = 0.5,
  model_comparison = TRUE,
  cross_validation = FALSE,
  cv_folds = 5,
  model_selection_criteria = "dic",
  spatial_prediction = TRUE,
  prediction_grid_size = 50,
  prediction_time_points = "12,24,36,60",
  confidence_level = 0.95,
  show_model_summary = TRUE,
  show_parameter_estimates = TRUE,
  show_spatial_effects = TRUE,
  show_residual_maps = TRUE,
  show_survival_maps = TRUE,
  show_hazard_maps = FALSE,
  show_convergence_diagnostics = TRUE,
  show_model_comparison = TRUE,
  show_interpretation = TRUE,
  standardize_predictors = TRUE,
  include_intercept = TRUE,
  missing_data_handling = "complete_cases",
  parallel_processing = TRUE,
  n_cores = 2,
  clinical_context = "cancer_epidemiology",
  set_seed = TRUE,
  seed_value = 42
)
```

## Arguments

- data:

  The data as a data frame for spatial Bayesian survival analysis.

- time:

  .

- status:

  .

- statusLevel:

  .

- predictors:

  .

- spatial_coords:

  Longitude and Latitude coordinates (or X, Y coordinates)

- region_id:

  Administrative region or area identifier

- adjacency_matrix:

  Path to adjacency matrix file or leave empty for distance-based

- spatial_model:

  .

- spatial_prior:

  .

- distance_method:

  .

- neighborhood_threshold:

  Distance threshold for defining neighbors (km)

- num_neighbors:

  Number of nearest neighbors to consider

- baseline_hazard:

  .

- piecewise_intervals:

  Number of intervals for piecewise constant hazard

- spline_knots:

  Number of knots for B-spline baseline hazard

- mcmc_samples:

  .

- mcmc_burnin:

  .

- mcmc_thin:

  .

- mcmc_chains:

  .

- covariate_priors:

  .

- covariate_prior_sd:

  .

- spatial_precision_prior:

  .

- spatial_precision_shape:

  .

- spatial_precision_rate:

  .

- model_comparison:

  .

- cross_validation:

  .

- cv_folds:

  .

- model_selection_criteria:

  .

- spatial_prediction:

  .

- prediction_grid_size:

  .

- prediction_time_points:

  Comma-separated list of time points for prediction

- confidence_level:

  .

- show_model_summary:

  .

- show_parameter_estimates:

  .

- show_spatial_effects:

  .

- show_residual_maps:

  .

- show_survival_maps:

  .

- show_hazard_maps:

  .

- show_convergence_diagnostics:

  .

- show_model_comparison:

  .

- show_interpretation:

  .

- standardize_predictors:

  .

- include_intercept:

  .

- missing_data_handling:

  .

- parallel_processing:

  .

- n_cores:

  .

- clinical_context:

  .

- set_seed:

  .

- seed_value:

  .

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`           |     |     |     |     | a table  |
| `results$parameterEstimates`     |     |     |     |     | a table  |
| `results$spatialEffects`         |     |     |     |     | a table  |
| `results$modelFit`               |     |     |     |     | a table  |
| `results$spatialCorrelation`     |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a table  |
| `results$convergenceDiagnostics` |     |     |     |     | a table  |
| `results$survivalPredictions`    |     |     |     |     | a table  |
| `results$spatialValidation`      |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a html   |
| `results$methodsExplanation`     |     |     |     |     | a html   |
| `results$spatialMaps`            |     |     |     |     | an image |
| `results$survivalMaps`           |     |     |     |     | an image |
| `results$hazardMaps`             |     |     |     |     | an image |
| `results$residualMaps`           |     |     |     |     | an image |
| `results$convergencePlots`       |     |     |     |     | an image |
| `results$posteriorPlots`         |     |     |     |     | an image |
| `results$spatialCorrelationPlot` |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
