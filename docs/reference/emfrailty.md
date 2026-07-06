# EM-Algorithm Frailty Models

Implements expectation-maximization (EM) algorithm based frailty models
for survival analysis. Provides efficient estimation of frailty
distributions and variance components in complex survival data with
unobserved heterogeneity. Supports multiple frailty distributions and
convergence diagnostics.

## Usage

``` r
emfrailty(
  data,
  elapsedtime,
  outcome,
  covariates,
  frailty_variable,
  outcomeLevel = "1",
  frailty_distribution = "gamma",
  estimation_method = "em",
  baseline_hazard = "weibull",
  ties_method = "breslow",
  confidence_level = 0.95,
  max_iterations = 100,
  convergence_tolerance = 1e-06,
  em_acceleration = FALSE,
  variance_estimation = "observed_information",
  bootstrap_samples = 500,
  frailty_prediction = TRUE,
  model_selection = FALSE,
  diagnostic_plots = TRUE,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_frailty_analysis = TRUE,
  show_convergence = TRUE,
  show_diagnostics = TRUE,
  show_predictions = TRUE,
  show_comparison = TRUE,
  show_convergence_plots = TRUE,
  show_frailty_plots = TRUE,
  show_residual_plots = TRUE,
  show_survival_plots = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Survival time or follow-up duration variable. Should contain positive
  numeric values representing the time to event or censoring.

- outcome:

  Event indicator variable specifying whether the event of interest
  occurred. Can be coded as 0/1, TRUE/FALSE, or factor levels.

- covariates:

  Predictor variables for the frailty model. Can include continuous
  variables, factors, and interactions. Will be included as fixed
  effects in the model.

- frailty_variable:

  Variable defining the frailty groups (clusters). Each unique value
  represents a different frailty group sharing common unobserved risk
  factors.

- outcomeLevel:

  Specifies which level of the outcome variable represents the event of
  interest. For numeric variables, typically '1'. For factors, specify
  the appropriate level name.

- frailty_distribution:

  Distribution assumed for the frailty terms. Gamma is most commonly
  used and provides conjugate properties for efficient EM estimation.

- estimation_method:

  Method for parameter estimation. Standard EM is most robust, while
  accelerated variants may converge faster for large datasets.

- baseline_hazard:

  Specification for the baseline hazard function. Parametric forms
  provide efficiency while non-parametric allows maximum flexibility.

- ties_method:

  Method for handling tied event times. Breslow is fastest, Efron more
  accurate, exact method most precise but computationally intensive.

- confidence_level:

  Confidence level for parameter confidence intervals and hypothesis
  tests.

- max_iterations:

  Maximum number of EM algorithm iterations before forced termination.

- convergence_tolerance:

  Convergence tolerance for the EM algorithm. Smaller values require
  more precise convergence but may increase computation time.

- em_acceleration:

  Enable acceleration methods (SQUAREM, Anderson) to speed up EM
  convergence. Recommended for large datasets or complex models.

- variance_estimation:

  Method for estimating parameter variances and standard errors.
  Observed information is most common, bootstrap most robust.

- bootstrap_samples:

  Number of bootstrap samples for variance estimation when bootstrap
  method is selected.

- frailty_prediction:

  Compute empirical Bayes predictions of individual frailty values with
  prediction intervals and shrinkage estimates.

- model_selection:

  Perform automatic model selection comparing different frailty
  distributions and baseline hazard specifications using information
  criteria.

- diagnostic_plots:

  Generate comprehensive diagnostic plots including convergence traces,
  residuals, frailty predictions, and model fit assessments.

- show_model_summary:

  Display comprehensive model summary with sample characteristics and
  convergence information.

- show_coefficients:

  Display fixed effects coefficient estimates with confidence intervals
  and tests.

- show_frailty_analysis:

  Display frailty variance estimates, distribution parameters, and
  heterogeneity measures.

- show_convergence:

  Display EM algorithm convergence diagnostics and iteration history.

- show_diagnostics:

  Display comprehensive model diagnostics including fit statistics and
  residual analysis.

- show_predictions:

  Display empirical Bayes frailty predictions with shrinkage analysis.

- show_comparison:

  Compare EM frailty model with standard Cox model and alternative
  specifications.

- show_convergence_plots:

  Display EM algorithm convergence trace plots and diagnostic
  visualizations.

- show_frailty_plots:

  Display frailty distribution plots and empirical Bayes prediction
  visualizations.

- show_residual_plots:

  Display residual diagnostic plots for model adequacy assessment.

- show_survival_plots:

  Display survival curves stratified by frailty groups and predictions.

- showSummaries:

  Generate comprehensive explanatory text summarizing the analysis
  results, methodology, and clinical interpretation guidelines.

- showExplanations:

  Provide detailed explanations of the EM-algorithm frailty methodology,
  assumptions, advantages, and interpretation guidelines.

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`          |     |     |     |     | a table  |
| `results$coefficients`          |     |     |     |     | a table  |
| `results$frailtyAnalysis`       |     |     |     |     | a table  |
| `results$convergenceInfo`       |     |     |     |     | a table  |
| `results$diagnostics`           |     |     |     |     | a table  |
| `results$frailtyPredictions`    |     |     |     |     | a table  |
| `results$modelComparison`       |     |     |     |     | a table  |
| `results$baselineHazard`        |     |     |     |     | a table  |
| `results$heterogeneityAnalysis` |     |     |     |     | a table  |
| `results$shrinkageAnalysis`     |     |     |     |     | a table  |
| `results$convergencePlots`      |     |     |     |     | an image |
| `results$frailtyPlots`          |     |     |     |     | an image |
| `results$residualPlots`         |     |     |     |     | an image |
| `results$survivalPlots`         |     |     |     |     | an image |
| `results$fitPlots`              |     |     |     |     | an image |
| `results$summaryTable`          |     |     |     |     | a html   |
| `results$methodExplanation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
