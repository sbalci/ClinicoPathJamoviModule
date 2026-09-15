# Illness-Death Multi-State Models

Illness-death multi-state models for analyzing disease progression
through three states: healthy (state 0), ill (state 1), and death (state
2). This framework models transitions between states with separate
hazard functions, enabling analysis of disease onset, recovery rates,
and differential mortality risks. The implementation supports both
non-parametric and parametric approaches, competing risks formulation,
reversible and irreversible illness states, time-dependent covariates,
and comprehensive transition probability estimation. Particularly
valuable for chronic disease studies, cancer progression analysis,
hospital readmission studies, and any scenario involving intermediate
health states with potential recovery. The model provides state
occupancy probabilities, mean sojourn times, and transition-specific
hazard ratios with clinical interpretation frameworks for evidence-based
medicine.

## Usage

``` r
illnessdeath(
  data,
  time_entry,
  time_exit,
  state_from,
  state_to,
  subject_id,
  covariates,
  model_structure = "standard",
  transition_specific = FALSE,
  baseline_hazard = "nonparametric",
  common_baseline = FALSE,
  time_varying_covariates,
  time_varying_effects = FALSE,
  interaction_terms = "",
  estimation_method = "partial",
  tie_method = "breslow",
  prediction_times = "1, 2, 5, 10",
  prediction_horizon = 10,
  transition_probabilities = TRUE,
  sojourn_times = TRUE,
  bootstrap_ci = FALSE,
  bootstrap_samples = 500,
  confidence_level = 0.95,
  goodness_of_fit = FALSE,
  residual_analysis = FALSE,
  cross_validation = FALSE,
  show_transition_summary = TRUE,
  show_hazard_ratios = TRUE,
  show_state_probabilities = TRUE,
  show_sojourn_analysis = TRUE,
  plot_transition_diagram = TRUE,
  plot_state_probabilities = TRUE,
  plot_transition_hazards = TRUE,
  plot_cumulative_incidence = FALSE,
  plot_sojourn_distributions = FALSE,
  plot_residuals = FALSE,
  left_truncation = FALSE,
  interval_censoring = FALSE,
  competing_mortality = FALSE,
  stratification_variable,
  frailty_model = FALSE,
  penalization_parameter = 0,
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- time_entry:

  Time of entry into the current state. For interval data, this is the
  start time of the observation interval.

- time_exit:

  Time of exit from the current state or censoring time. Must be greater
  than or equal to entry time.

- state_from:

  Starting state for each transition. Should be coded as 0 = healthy, 1
  = ill, 2 = death (absorbing state).

- state_to:

  Destination state for each transition. Should be coded as 0 = healthy,
  1 = ill, 2 = death. Use same value as from_state for censoring.

- subject_id:

  Unique identifier for each subject. Required for tracking individuals
  through multiple state transitions.

- covariates:

  Variables affecting transition hazards. Can be time-fixed or
  time-varying. Applied to all transitions unless specified otherwise.

- model_structure:

  Type of illness-death model. Standard allows 0→1, 1→2, 0→2
  transitions. Reversible adds 1→0. Progressive restricts to forward
  transitions. Competing treats illness and death as competing from
  healthy state.

- transition_specific:

  Allow different covariate effects for each transition type. When TRUE,
  provides separate hazard ratios for each transition.

- baseline_hazard:

  Specification for baseline hazard functions. Non-parametric provides
  flexibility, parametric options enable extrapolation and smooth
  estimates.

- common_baseline:

  Use same baseline hazard shape for all transitions (with different
  scales). Reduces parameters and increases precision when transitions
  are similar.

- time_varying_covariates:

  Subset of covariates that vary over time. Requires careful data
  structure with time-updated covariate values.

- time_varying_effects:

  Allow covariate effects to change over time using time-interaction
  terms or spline-based methods.

- interaction_terms:

  Specify interaction terms as comma-separated pairs (e.g.,
  'age:treatment, stage:age'). Interactions can be transition-specific
  or applied globally.

- estimation_method:

  Method for parameter estimation. Partial likelihood is standard for
  semi-parametric models, ML for parametric, Bayesian for uncertainty
  quantification, penalized for high-dimensional problems.

- tie_method:

  Method for handling tied transition times. Efron provides better
  approximation, exact is computationally intensive but most accurate.

- prediction_times:

  Comma-separated time points for state occupation probability
  predictions and transition probability matrices.

- prediction_horizon:

  Maximum time horizon for probability predictions and state occupation
  forecasts.

- transition_probabilities:

  Compute transition probability matrices P(s,t) giving probabilities of
  being in each state at time t given state at time s.

- sojourn_times:

  Calculate expected sojourn times (mean time spent) in each state
  before transition to another state or censoring.

- bootstrap_ci:

  Calculate bootstrap confidence intervals for transition probabilities
  and state occupation probabilities.

- bootstrap_samples:

  Number of bootstrap samples for confidence interval estimation. More
  samples provide more stable intervals but increase computation.

- confidence_level:

  Confidence level for all interval estimates including hazard ratios
  and transition probabilities.

- goodness_of_fit:

  Perform goodness of fit tests for multi-state model including Markov
  assumption testing and transition-specific model checks.

- residual_analysis:

  Calculate and analyze residuals for each transition including
  martingale, deviance, and Schoenfeld residuals.

- cross_validation:

  Perform cross-validation to assess prediction accuracy for state
  occupation probabilities.

- show_transition_summary:

  Display summary of observed transitions including counts, rates, and
  basic descriptive statistics.

- show_hazard_ratios:

  Display hazard ratios for each transition with confidence intervals
  and significance tests.

- show_state_probabilities:

  Display state occupation probabilities over time with confidence bands
  and clinical interpretation.

- show_sojourn_analysis:

  Display expected sojourn times in each state with bootstrap confidence
  intervals.

- plot_transition_diagram:

  Create visual diagram showing possible transitions between states with
  hazard ratios and confidence intervals.

- plot_state_probabilities:

  Plot state occupation probabilities over time showing evolution of
  population distribution across states.

- plot_transition_hazards:

  Plot hazard functions for each possible transition with baseline
  hazards and covariate effects.

- plot_cumulative_incidence:

  Plot cumulative incidence functions for each destination state
  starting from each initial state.

- plot_sojourn_distributions:

  Plot distributions of sojourn times in each state with empirical and
  model-based estimates.

- plot_residuals:

  Create residual plots for model diagnostics including
  transition-specific residual analysis.

- left_truncation:

  Account for left truncation when subjects enter study after disease
  onset or state transitions.

- interval_censoring:

  Handle interval-censored transition times when exact transition times
  are unknown but intervals are available.

- competing_mortality:

  Distinguish between disease-related and other-cause mortality in the
  death state for competing risks analysis.

- stratification_variable:

  Variable for stratified analysis allowing different baseline hazards
  for each stratum while maintaining same covariate effects.

- frailty_model:

  Include shared frailty term to account for unobserved heterogeneity
  affecting all transitions for each individual.

- penalization_parameter:

  Penalization parameter for regularized estimation with
  high-dimensional covariates. 0 = no penalization.

- random_seed:

  Random seed for bootstrap sampling and any stochastic procedures to
  ensure reproducible results.

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`             |     |     |     |     | a html   |
| `results$todo`                     |     |     |     |     | a html   |
| `results$transitionSummary`        |     |     |     |     | a table  |
| `results$hazardRatios`             |     |     |     |     | a table  |
| `results$stateProbabilities`       |     |     |     |     | a table  |
| `results$sojournAnalysis`          |     |     |     |     | a table  |
| `results$transitionMatrix`         |     |     |     |     | a table  |
| `results$goodnessOfFit`            |     |     |     |     | a table  |
| `results$modelComparison`          |     |     |     |     | a table  |
| `results$crossValidationResults`   |     |     |     |     | a table  |
| `results$residualAnalysis`         |     |     |     |     | a table  |
| `results$transitionDiagram`        |     |     |     |     | an image |
| `results$stateProbabilitiesPlot`   |     |     |     |     | an image |
| `results$transitionHazardsPlot`    |     |     |     |     | an image |
| `results$cumulativeIncidencePlot`  |     |     |     |     | an image |
| `results$sojournDistributionsPlot` |     |     |     |     | an image |
| `results$residualsPlot`            |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$transitionSummary$asDF`

`as.data.frame(results$transitionSummary)`
