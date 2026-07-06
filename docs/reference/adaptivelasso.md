# Adaptive LASSO for Cox Models

Adaptive LASSO for Cox proportional hazards models with data-driven
penalty selection and improved variable selection properties. Unlike
standard LASSO, the adaptive LASSO uses weights based on initial
parameter estimates to provide consistent variable selection (oracle
property) and reduced bias for non-zero coefficients. This
implementation supports multiple weight calculation methods,
cross-validation for optimal penalty selection, stability selection for
robust variable identification, and comprehensive model diagnostics.
Particularly effective for high-dimensional survival data where
traditional methods fail, genomic studies with many biomarkers, and
clinical prediction models requiring sparse, interpretable solutions.
The method supports tie handling in refitted Cox models, optional
stratification for Cox modeling, and bootstrap-based stability
selection.

## Usage

``` r
adaptivelasso(
  data,
  time,
  event,
  event_level,
  censor_level,
  predictors,
  strata,
  suitabilityCheck = TRUE,
  weight_method = "ridge",
  alpha = 1,
  gamma = 1,
  cv_folds = 10,
  cv_measure = "deviance",
  lambda_sequence = "auto",
  lambda_custom_max = 1,
  lambda_custom_min = 0.001,
  lambda_single = 0.01,
  lambda_min_ratio = 0.001,
  n_lambda = 100,
  stability_selection = FALSE,
  stability_threshold = 0.6,
  bootstrap_samples = 100,
  subsampling_ratio = 0.8,
  proportional_hazards = TRUE,
  influence_diagnostics = FALSE,
  goodness_of_fit = TRUE,
  risk_groups = 3,
  time_points = "1, 2, 5",
  baseline_survival = TRUE,
  show_coefficients = TRUE,
  show_selection_path = TRUE,
  show_cv_results = TRUE,
  show_diagnostics = TRUE,
  plot_selection_path = TRUE,
  plot_cv_curve = TRUE,
  plot_stability = FALSE,
  plot_survival_curves = FALSE,
  plot_baseline_hazard = FALSE,
  plot_diagnostics = FALSE,
  tie_method = "breslow",
  standardize = TRUE,
  parallel_computing = FALSE,
  n_cores = 2,
  convergence_threshold = 1e-07,
  max_iterations = 10000,
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

  Event indicator variable. For binary outcomes use 0 = censored, 1 =
  event. For multi-level variables, choose the event level of interest.

- event_level:

  Level of the event variable to be treated as the event of interest.
  For binary factors this can be left empty and the second level is
  used.

- censor_level:

  Level of the event variable to be treated as censored (no event). When
  specified together with Event Level, rows matching neither level are
  excluded from analysis. For binary factors this can be left empty and
  the first level is used.

- predictors:

  Variables to include in the adaptive LASSO Cox model. Can include
  continuous, ordinal, and nominal variables. Automatic standardization
  is applied for optimal penalty performance.

- strata:

  Optional stratification variable. When provided, stratified Cox
  structures are used for penalized fitting and refitted Cox summaries.

- suitabilityCheck:

  assess if data is suitable for the selected Adaptive LASSO model

- weight_method:

  Method for calculating adaptive weights. Ridge provides stable weights
  with shrinkage, univariate uses individual Cox regressions, full Cox
  uses complete model (may be unstable), correlation uses marginal
  associations.

- alpha:

  Elastic net mixing parameter. 1.0 = pure LASSO, 0.0 = ridge
  regression, intermediate values combine both penalties for better
  performance with correlated predictors.

- gamma:

  Power parameter for adaptive weights calculation. Higher values
  increase penalty differences between variables, promoting stronger
  variable selection but potentially increasing instability.

- cv_folds:

  Number of folds for cross-validation to select optimal penalty
  parameter. More folds provide better estimates but increase
  computation time.

- cv_measure:

  Performance measure for cross-validation. Deviance (partial
  likelihood) is computationally efficient and is the default. C-index
  (Harrell's concordance) focuses on discrimination ability.

- lambda_sequence:

  How to specify the penalty parameter sequence. Automatic uses a
  data-driven range, custom uses user-defined max/min, and single fits
  only one lambda value.

- lambda_custom_max:

  Maximum lambda value for custom lambda sequence mode. Must be greater
  than Custom Lambda Minimum.

- lambda_custom_min:

  Minimum lambda value for custom lambda sequence mode. Must be smaller
  than Custom Lambda Maximum.

- lambda_single:

  Lambda value used when Lambda Sequence is set to Single Value.

- lambda_min_ratio:

  Ratio of smallest to largest lambda value in automatic sequence.
  Smaller values explore stronger penalties but may lead to
  computational difficulties.

- n_lambda:

  Number of lambda values in the regularization path. More values
  provide finer resolution but increase computation time.

- stability_selection:

  Perform stability selection to identify robust variable selection
  patterns across bootstrap samples. Provides more reliable variable
  selection than single model fitting.

- stability_threshold:

  Minimum selection frequency for variables in stability selection.
  Higher thresholds provide more conservative variable selection but may
  miss important predictors.

- bootstrap_samples:

  Number of bootstrap samples for stability selection and confidence
  intervals. More samples provide more stable estimates.

- subsampling_ratio:

  Proportion of data used in each bootstrap sample for stability
  selection. Smaller ratios increase selection variability.

- proportional_hazards:

  Test proportional hazards assumption for selected variables using
  scaled Schoenfeld residuals and time-interaction tests.

- influence_diagnostics:

  Calculate influence diagnostics including dfbeta, leverage, and
  outlier detection for robust model assessment.

- goodness_of_fit:

  Perform goodness of fit assessment including model deviance,
  concordance statistics, and calibration measures.

- risk_groups:

  Number of risk groups for stratification based on linear predictor.
  Used for Kaplan-Meier curves and risk group analysis.

- time_points:

  Comma-separated list of time points for survival probability
  predictions. Leave empty to suppress the predictions table.

- baseline_survival:

  Estimate and plot baseline survival function for the adaptive LASSO
  Cox model using Breslow estimator.

- show_coefficients:

  Display coefficient estimates for selected variables with standard
  errors and confidence intervals.

- show_selection_path:

  Display complete regularization path showing how coefficients change
  with penalty parameter.

- show_cv_results:

  Display cross-validation results including optimal lambda selection
  and performance across penalty values.

- show_diagnostics:

  Display comprehensive model diagnostics including proportional hazards
  tests, influence measures, and residual analysis.

- plot_selection_path:

  Plot coefficient paths as function of penalty parameter showing
  variable selection progression.

- plot_cv_curve:

  Plot cross-validation performance curve with optimal lambda selection
  and confidence bands.

- plot_stability:

  Plot stability selection results showing selection frequencies across
  bootstrap samples.

- plot_survival_curves:

  Plot Kaplan-Meier survival curves for risk groups defined by adaptive
  LASSO linear predictor.

- plot_baseline_hazard:

  Plot estimated baseline hazard function and cumulative baseline hazard
  from the final model.

- plot_diagnostics:

  Create diagnostic plots including residuals, influential observations,
  and proportional hazards assessment.

- tie_method:

  Method for handling tied survival times. Efron provides better
  approximation but is computationally more intensive.

- standardize:

  Standardize predictors before fitting. Recommended for optimal penalty
  performance with mixed-scale variables.

- parallel_computing:

  Use parallel computing for cross-validation and stability-selection
  resampling to reduce computation time.

- n_cores:

  Number of CPU cores for parallel computation when enabled. More cores
  speed up CV and bootstrap but use more memory.

- convergence_threshold:

  Convergence threshold for coordinate descent algorithm. Smaller values
  provide more precise solutions but increase computation time.

- max_iterations:

  Maximum iterations for coordinate descent algorithm. Increase if
  convergence warnings occur.

- random_seed:

  Random seed for cross-validation folds and bootstrap sampling. Ensures
  reproducible results across analyses.

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$notices`            |     |     |     |     | a html   |
| `results$suitabilityReport`  |     |     |     |     | a html   |
| `results$coefficients`       |     |     |     |     | a table  |
| `results$selectionPath`      |     |     |     |     | a table  |
| `results$cvResults`          |     |     |     |     | a table  |
| `results$stabilityResults`   |     |     |     |     | a table  |
| `results$modelDiagnostics`   |     |     |     |     | a table  |
| `results$performanceMetrics` |     |     |     |     | a table  |
| `results$riskGroups`         |     |     |     |     | a table  |
| `results$predictions`        |     |     |     |     | a table  |
| `results$pathPlot`           |     |     |     |     | an image |
| `results$cvPlot`             |     |     |     |     | an image |
| `results$stabilityPlot`      |     |     |     |     | an image |
| `results$survivalPlot`       |     |     |     |     | an image |
| `results$baselineHazardPlot` |     |     |     |     | an image |
| `results$diagnosticsPlot`    |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficients$asDF`

`as.data.frame(results$coefficients)`
