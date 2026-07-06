# Recurrent Event Survival Analysis

Recurrent event survival analysis for subjects who can experience
multiple events over time. Common in cancer recurrence, infections,
hospitalizations, and chronic disease episodes. Handles gap time,
calendar time, and counting process formulations.

## Usage

``` r
recurrentsurvival(
  subject_id,
  event_time,
  event_status = NULL,
  terminal_event = NULL,
  terminal_status = NULL,
  event_type = NULL,
  covariates = NULL,
  stratification_vars = NULL,
  model_type = "ag_model",
  time_scale = "gap_time",
  frailty_distribution = "gamma",
  robust_variance = TRUE,
  cluster_variable = NULL,
  baseline_hazard_smooth = FALSE,
  smoothing_bandwidth = 1,
  confidence_level = 0.95,
  max_events = 10,
  terminal_competing = TRUE,
  recurrence_plots = TRUE,
  gap_time_plots = FALSE,
  cumulative_hazard = TRUE,
  model_diagnostics = TRUE,
  goodness_of_fit = TRUE,
  prediction_times = "12,24,36,60",
  bootstrap_samples = 200,
  convergence_tolerance = 1e-04,
  max_iterations = 500,
  use_reReg = FALSE,
  reReg_model = "cox_LWYY",
  reReg_se = "bootstrap",
  reReg_B = 200,
  show_event_plot = TRUE,
  show_mcf_plot = TRUE,
  event_plot_subjects = 20
)
```

## Arguments

- subject_id:

  Variable identifying unique subjects who can have multiple events

- event_time:

  Time variable for recurrent event occurrence

- event_status:

  Event status indicator for recurrent events

- terminal_event:

  Terminal event that stops the recurrence process

- terminal_status:

  Status indicator for terminal event

- event_type:

  Classification of different types of recurrent events

- covariates:

  Covariates to include in the recurrent event model

- stratification_vars:

  Variables for stratifying the baseline hazard

- model_type:

  Model specification for recurrent event data

- time_scale:

  Time scale specification for recurrent events

- frailty_distribution:

  Distribution assumption for frailty terms

- robust_variance:

  Whether to use robust variance estimation

- cluster_variable:

  Variable for clustering in variance estimation

- baseline_hazard_smooth:

  Whether to smooth the baseline hazard function

- smoothing_bandwidth:

  Bandwidth for baseline hazard smoothing

- confidence_level:

  Confidence level for parameter estimates

- max_events:

  Upper limit on events per subject for analysis

- terminal_competing:

  Whether to model terminal event as competing risk

- recurrence_plots:

  Whether to plot recurrence functions

- gap_time_plots:

  Whether to plot gap time distributions

- cumulative_hazard:

  Whether to plot cumulative hazard functions

- model_diagnostics:

  Whether to include model diagnostics

- goodness_of_fit:

  Whether to test model goodness-of-fit

- prediction_times:

  Time points for recurrence probability predictions

- bootstrap_samples:

  Number of bootstrap resamples

- convergence_tolerance:

  Convergence criterion for iterative algorithms

- max_iterations:

  Maximum number of algorithm iterations

- use_reReg:

  Whether to use reReg package for analysis

- reReg_model:

  Model specification for reReg package

- reReg_se:

  Standard error estimation method for reReg

- reReg_B:

  Bootstrap samples for reReg variance estimation

- show_event_plot:

  Whether to show event plot from reReg

- show_mcf_plot:

  Whether to show MCF plot from reReg

- event_plot_subjects:

  Number of subjects to display in event plot

## Value

A results object containing:

|                                     |     |     |     |     |          |
|-------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`              |     |     |     |     | a html   |
| `results$data_summary`              |     |     |     |     | a html   |
| `results$model_results`             |     |     |     |     | a html   |
| `results$covariate_effects`         |     |     |     |     | a html   |
| `results$recurrence_estimates`      |     |     |     |     | a html   |
| `results$gap_time_summary`          |     |     |     |     | a html   |
| `results$terminal_event_summary`    |     |     |     |     | a html   |
| `results$frailty_estimates`         |     |     |     |     | a html   |
| `results$model_diagnostics_summary` |     |     |     |     | a html   |
| `results$goodness_of_fit_results`   |     |     |     |     | a html   |
| `results$event_frequency_table`     |     |     |     |     | a html   |
| `results$recurrence_plots`          |     |     |     |     | an image |
| `results$gap_time_plots`            |     |     |     |     | an image |
| `results$cumulative_hazard_plot`    |     |     |     |     | an image |
| `results$event_timeline_plot`       |     |     |     |     | an image |
| `results$residual_plots`            |     |     |     |     | an image |
| `results$frailty_distribution_plot` |     |     |     |     | an image |
| `results$multistate_diagram`        |     |     |     |     | an image |
| `results$model_comparison_plot`     |     |     |     |     | an image |
