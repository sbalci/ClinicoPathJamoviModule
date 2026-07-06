# Joint Longitudinal-Survival Models

Joint modeling of longitudinal biomarker trajectories and survival
outcomes. Links repeated measurements over time to survival endpoints
for dynamic risk prediction and personalized medicine applications.

## Usage

``` r
jointmodeling(
  data,
  id,
  time_longitudinal,
  biomarker,
  survival_time,
  survival_status,
  covariates,
  functional_form = "linear",
  random_effects = "intercept_slope",
  error_structure = "independent",
  survival_model = "cox",
  association_structure = "current_value",
  estimation_method = "bayesian",
  mcmc_chains = 3,
  mcmc_iterations = 12000,
  mcmc_burnin = 2000,
  mcmc_thin = 5,
  dynamic_prediction = TRUE,
  prediction_horizon = "0.5,1,2,3",
  prediction_window = 2,
  internal_validation = TRUE,
  cv_folds = 5,
  discrimination_metrics = TRUE,
  plot_trajectories = TRUE,
  plot_mean_trajectory = TRUE,
  plot_survival_curves = TRUE,
  plot_dynamic_auc = TRUE,
  plot_residuals = FALSE,
  competing_risks = FALSE,
  left_truncation = FALSE,
  time_varying_effects = FALSE,
  multiple_biomarkers,
  baseline_hazard = "unspecified",
  prior_specification = "default",
  convergence_diagnostics = TRUE,
  parallel_computation = TRUE
)
```

## Arguments

- data:

  The data as a data frame in long format (multiple rows per patient).

- id:

  Patient ID for linking longitudinal measurements

- time_longitudinal:

  Time axis for biomarker trajectory

- biomarker:

  Longitudinal biomarker variable

- survival_time:

  Survival time variable

- survival_status:

  Survival event indicator

- covariates:

  Additional predictive variables

- functional_form:

  Longitudinal model specification

- random_effects:

  Random effects specification

- error_structure:

  Within-subject error correlation

- survival_model:

  Survival distribution choice

- association_structure:

  Association structure specification

- estimation_method:

  Estimation approach

- mcmc_chains:

  Parallel MCMC chains

- mcmc_iterations:

  MCMC sampling iterations

- mcmc_burnin:

  MCMC warm-up period

- mcmc_thin:

  MCMC thinning parameter

- dynamic_prediction:

  Enable dynamic prediction

- prediction_horizon:

  Prediction time windows

- prediction_window:

  Prediction look-ahead window

- internal_validation:

  Cross-validation assessment

- cv_folds:

  Cross-validation partitions

- discrimination_metrics:

  Compute discrimination measures

- plot_trajectories:

  Generate trajectory plots

- plot_mean_trajectory:

  Show mean trajectory

- plot_survival_curves:

  Generate survival plots

- plot_dynamic_auc:

  Show discrimination over time

- plot_residuals:

  Generate diagnostic plots

- competing_risks:

  Include competing events

- left_truncation:

  Handle delayed study entry

- time_varying_effects:

  Time-dependent association parameters

- multiple_biomarkers:

  Multiple longitudinal outcomes

- baseline_hazard:

  Baseline hazard modeling

- prior_specification:

  Bayesian prior choice

- convergence_diagnostics:

  MCMC quality assessment

- parallel_computation:

  Enable parallel processing

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`        |     |     |     |     | a html   |
| `results$progress`            |     |     |     |     | a html   |
| `results$errors`              |     |     |     |     | a html   |
| `results$warnings`            |     |     |     |     | a html   |
| `results$dataSummary`         |     |     |     |     | a html   |
| `results$longitudinalResults` |     |     |     |     | a html   |
| `results$survivalResults`     |     |     |     |     | a html   |
| `results$jointModelResults`   |     |     |     |     | a html   |
| `results$diagnostics`         |     |     |     |     | a html   |
| `results$dynamicPredictions`  |     |     |     |     | a html   |
| `results$validation`          |     |     |     |     | a html   |
| `results$finalResults`        |     |     |     |     | a html   |
| `results$trajectoryPlot`      |     |     |     |     | an image |
| `results$meanTrajectoryPlot`  |     |     |     |     | an image |
| `results$survivalPlot`        |     |     |     |     | an image |
| `results$dynamicAUCPlot`      |     |     |     |     | an image |
| `results$residualPlot`        |     |     |     |     | an image |

## Details

Note: This analysis can be computationally intensive. Expected runtime:
2-10 minutes depending on data size and model complexity.
