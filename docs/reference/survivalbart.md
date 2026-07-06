# Bayesian Additive Regression Trees (BART) for Survival

Bayesian Additive Regression Trees (BART) for survival analysis,
providing a flexible non-parametric approach to modeling complex
clinical survival data.

## Usage

``` r
survivalbart(
  data,
  time,
  event,
  predictors,
  strata,
  model_type = "aft",
  prior_distribution = "normal",
  n_trees = 200,
  alpha = 0.95,
  beta = 2,
  k = 2,
  q = 0.9,
  nu = 3,
  n_burn = 1000,
  n_post = 2000,
  n_thin = 1,
  n_chains = 1,
  variable_selection = TRUE,
  sparse_prior = FALSE,
  selection_alpha = 1,
  predict_times = "1, 2, 5",
  survival_quantiles = "0.25, 0.5, 0.75",
  credible_level = 0.95,
  cross_validation = FALSE,
  cv_folds = 5,
  convergence_diagnostics = TRUE,
  posterior_prediction = FALSE,
  show_model_summary = TRUE,
  show_variable_importance = TRUE,
  show_survival_summary = TRUE,
  show_convergence = TRUE,
  plot_survival_curves = TRUE,
  plot_variable_importance = TRUE,
  plot_trace = FALSE,
  plot_posterior_predictive = FALSE,
  plot_partial_dependence = FALSE,
  plot_interactions = FALSE,
  probit_link = FALSE,
  cure_fraction_prior = "uniform",
  adaptive_scaling = TRUE,
  parallel_chains = FALSE,
  memory_optimization = TRUE,
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Time to event variable (numeric). For right-censored data, this is the
  time from study entry to event or censoring.

- event:

  Event indicator variable. For survival analysis: 0 = censored, 1 =
  event. Binary coding required for current BART implementation.

- predictors:

  Variables to include in the BART survival model. Can include numeric,
  ordinal, and nominal variables. BART automatically handles variable
  types and interactions without preprocessing.

- strata:

  Optional stratification variable for stratified survival analysis.
  Creates separate BART models for each stratum.

- model_type:

  Type of survival model. AFT models log-survival times directly, PH
  models log-hazard ratios, cure models handle long-term survivors with
  mixture of cured and susceptible populations.

- prior_distribution:

  Prior distribution for error terms in AFT model. Normal corresponds to
  log-normal survival times, extreme value to Weibull survival, logistic
  to log-logistic survival.

- n_trees:

  Number of trees in the BART ensemble. More trees provide greater
  flexibility but increase computation time. 100-300 is typical range.

- alpha:

  Controls tree depth through splitting probability. Higher values favor
  shallower trees, lower values allow deeper trees and more complex
  interactions.

- beta:

  Controls tree depth through splitting probability formula. Higher
  values discourage deep splits, promoting simpler trees.

- k:

  Scale parameter for terminal node prior. Controls magnitude of
  individual tree contributions to overall ensemble.

- q:

  Quantile for error variance prior specification. Higher values assume
  less noise in the survival process.

- nu:

  Degrees of freedom for inverse chi-square prior on error variance.
  Lower values allow more uncertainty in variance estimation.

- n_burn:

  Number of burn-in MCMC iterations to discard. Should be sufficient for
  convergence to stationary distribution.

- n_post:

  Number of posterior MCMC iterations to keep for inference. More
  iterations provide more precise posterior estimates.

- n_thin:

  Keep every nth posterior sample to reduce autocorrelation. Higher
  values reduce effective sample size but may improve mixing.

- n_chains:

  Number of independent MCMC chains. Multiple chains enable convergence
  diagnostics and robust posterior inference.

- variable_selection:

  Include variable selection probabilities in BART model. Automatically
  identifies relevant predictors and reduces overfitting with irrelevant
  variables.

- sparse_prior:

  Use sparse Dirichlet prior for variable selection probabilities.
  Promotes sparser models with fewer active variables.

- selection_alpha:

  Concentration parameter for Dirichlet prior on variable selection
  probabilities. Higher values favor uniform selection.

- predict_times:

  Comma-separated list of time points for survival probability
  predictions. Empty string uses data-driven quantiles.

- survival_quantiles:

  Comma-separated list of quantiles for survival time predictions.
  Provides percentile-based survival time estimates.

- credible_level:

  Level for posterior credible intervals on all estimates. 0.95 provides
  95 percent credible intervals.

- cross_validation:

  Perform k-fold cross-validation to assess out-of-sample performance
  and model generalization.

- cv_folds:

  Number of folds for cross-validation. More folds provide better
  performance estimates but increase computation.

- convergence_diagnostics:

  Compute MCMC convergence diagnostics including effective sample sizes,
  autocorrelation, and potential scale reduction.

- posterior_prediction:

  Perform posterior predictive checks to assess model adequacy by
  comparing observed data to posterior predictive distributions.

- show_model_summary:

  Display comprehensive model summary including BART parameters,
  convergence diagnostics, and posterior summaries.

- show_variable_importance:

  Display variable importance measures based on selection frequencies
  and splitting criteria across all trees.

- show_survival_summary:

  Display survival function summaries including median survival times
  and survival probabilities at key time points.

- show_convergence:

  Display MCMC convergence diagnostics and chain mixing assessment for
  model validation.

- plot_survival_curves:

  Plot individual and population-level survival curves with posterior
  credible bands showing uncertainty.

- plot_variable_importance:

  Create variable importance plot showing inclusion probabilities and
  splitting frequencies across the ensemble.

- plot_trace:

  Generate trace plots for key model parameters to assess MCMC
  convergence and mixing.

- plot_posterior_predictive:

  Create posterior predictive check plots comparing observed survival
  patterns to model predictions.

- plot_partial_dependence:

  Generate partial dependence plots showing marginal effects of
  individual variables on survival hazard.

- plot_interactions:

  Visualize detected interactions between variables with strength and
  confidence measures.

- probit_link:

  Use probit link for binary outcomes in cure models. Alternative to
  logit link for mixture component modeling.

- cure_fraction_prior:

  Prior distribution for cure fraction in cure models. Uniform is
  non-informative, Jeffreys is scale-invariant.

- adaptive_scaling:

  Use adaptive scaling of MCMC proposals to optimize acceptance rates
  and improve sampling efficiency.

- parallel_chains:

  Run MCMC chains in parallel for faster computation when multiple
  chains are specified.

- memory_optimization:

  Use memory-efficient storage for large ensembles and datasets to
  reduce memory footprint.

- random_seed:

  Random seed for reproducible MCMC sampling and tree ensemble
  generation.

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`            |     |     |     |     | a html   |
| `results$todo`                    |     |     |     |     | a html   |
| `results$modelSummary`            |     |     |     |     | a table  |
| `results$variableImportance`      |     |     |     |     | a table  |
| `results$survivalSummary`         |     |     |     |     | a table  |
| `results$convergenceDiagnostics`  |     |     |     |     | a table  |
| `results$crossValidationResults`  |     |     |     |     | a table  |
| `results$posteriorPredictive`     |     |     |     |     | a table  |
| `results$interactionEffects`      |     |     |     |     | a table  |
| `results$survivalCurvesPlot`      |     |     |     |     | an image |
| `results$variableImportancePlot`  |     |     |     |     | an image |
| `results$tracePlot`               |     |     |     |     | an image |
| `results$posteriorPredictivePlot` |     |     |     |     | an image |
| `results$partialDependencePlot`   |     |     |     |     | an image |
| `results$interactionsPlot`        |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`

## Details

Automatically captures non-linear relationships and high-order
interactions without explicit specification. Supports Accelerated
Failure Time (AFT), Proportional Hazards (PH), and Cure models. BART
combines an ensemble of weak learners (trees) with Bayesian priors to
create a powerful predictive model that naturally handles nonlinear
relationships, interactions, and variable selection without requiring
preprocessing. The method provides full posterior distributions for
predictions, built-in uncertainty quantification, and robust performance
across diverse survival scenarios. Particularly effective for complex
survival data with unknown functional forms, high-dimensional
predictors, mixed variable types, and scenarios requiring individualized
survival predictions with credible intervals. Implementation supports
both accelerated failure time and proportional hazards formulations with
comprehensive posterior inference and model diagnostics.
