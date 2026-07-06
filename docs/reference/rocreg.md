# ROC Regression (Covariate-Adjusted ROC)

ROC regression for covariate-adjusted diagnostic accuracy assessment.
Traditional ROC analysis assumes diagnostic test performance is constant
across all patients, but accuracy often varies by patient
characteristics (age, disease stage, scanner type, staining protocol).
ROC regression models how sensitivity and specificity change as
functions of covariates, providing covariate-specific ROC curves and
adjusted summary measures (covariate-adjusted AUC). Essential for
comparing diagnostic tests across heterogeneous populations, adjusting
for confounders in test comparison studies, and identifying patient
subgroups where tests perform differently. Particularly valuable in
digital pathology for accounting for batch effects (different scanners,
institutions, staining protocols) and in biomarker studies for
age/stage-specific performance assessment. Uses binormal,
non-parametric, or Bayesian approaches to model covariate effects on ROC
curves. Provides both population-averaged and covariate-specific
diagnostic accuracy estimates.

## Usage

``` r
rocreg(
  data,
  predictor,
  outcome,
  positive_level,
  covariates,
  covariate_effects = "both",
  method = "binormal",
  adjusted_auc = TRUE,
  covariate_specific_auc = TRUE,
  covariate_profiles = "",
  compare_unadjusted = TRUE,
  test_covariate_effects = TRUE,
  variable_selection = FALSE,
  selection_criterion = "aic",
  test_interactions = FALSE,
  confidence_intervals = TRUE,
  ci_method = "bootstrap",
  bootstrap_samples = 500,
  confidence_level = 0.95,
  plot_covariate_specific_rocs = TRUE,
  plot_auc_by_covariate = TRUE,
  plot_effect_estimates = TRUE,
  plot_residual_diagnostics = FALSE,
  adjustment_purpose = "confounder",
  clinical_application = "general",
  show_interpretation = TRUE,
  standardize_covariates = TRUE,
  missing_handling = "complete",
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- predictor:

  Continuous diagnostic test variable (biomarker level, imaging score,
  AI probability). The test whose accuracy varies by covariates.

- outcome:

  Binary outcome variable (diseased vs non-diseased, positive vs
  negative).

- positive_level:

  Which level represents the diseased/positive state.

- covariates:

  Variables that may affect diagnostic accuracy. Examples: age, sex,
  disease stage, scanner type, institution, staining batch. Both
  continuous and categorical covariates supported.

- covariate_effects:

  Where to model covariate effects. "Both" allows different effects in
  diseased and non-diseased groups, "sensitivity" models effects only in
  diseased, "specificity" only in non-diseased.

- method:

  Method for ROC regression. Binormal assumes normal distributions with
  covariate effects on location/scale, non-parametric uses kernel
  smoothing, semi-parametric combines both, Bayesian provides posterior
  distributions.

- adjusted_auc:

  Calculate population-averaged AUC adjusted for covariates. This is the
  overall AUC accounting for covariate distribution in the population.

- covariate_specific_auc:

  Calculate AUC at specific covariate values to show how diagnostic
  accuracy varies across patient characteristics.

- covariate_profiles:

  Specify covariate values for covariate-specific ROC curves. Example
  for age: "30, 50, 70" to get curves for young/middle/elderly patients.
  Leave empty for automatic selection (quartiles for continuous, all
  levels for categorical).

- compare_unadjusted:

  Show comparison between covariate-adjusted and unadjusted (pooled) ROC
  analysis. Demonstrates impact of covariate adjustment.

- test_covariate_effects:

  Perform statistical tests to determine if covariates significantly
  affect diagnostic accuracy. Likelihood ratio tests or Wald tests
  depending on method.

- variable_selection:

  Use stepwise or information criteria to select which covariates to
  include in final model. Helpful with many potential confounders.

- selection_criterion:

  Criterion for automatic variable selection.

- test_interactions:

  Test for interactions between covariates (e.g., does age effect differ
  by sex? does scanner effect vary by institution?).

- confidence_intervals:

  Calculate confidence intervals for covariate-adjusted AUC and ROC
  curves.

- ci_method:

  Method for confidence interval estimation. Bootstrap recommended for
  small samples or non-parametric methods.

- bootstrap_samples:

  Number of bootstrap samples for CI estimation.

- confidence_level:

  Confidence level for intervals.

- plot_covariate_specific_rocs:

  Display multiple ROC curves for different covariate profiles on same
  plot. Shows how ROC curve changes with patient characteristics.

- plot_auc_by_covariate:

  For continuous covariates, plot how AUC changes as covariate varies.
  Shows non-linear relationships between covariates and diagnostic
  accuracy.

- plot_effect_estimates:

  Forest plot showing estimated covariate effects on diagnostic accuracy
  with confidence intervals.

- plot_residual_diagnostics:

  Display residual plots and goodness-of-fit diagnostics for ROC
  regression model.

- adjustment_purpose:

  Purpose of covariate adjustment for interpretation guidance.

- clinical_application:

  Clinical application context for interpretation.

- show_interpretation:

  Provide interpretation with clinical context and recommendations.

- standardize_covariates:

  Standardize continuous covariates (mean 0, SD

  1.  for better numerical stability and interpretability of effect
      sizes.

- missing_handling:

  Method for handling missing covariate data.

- random_seed:

  Random seed for bootstrap and stochastic procedures.

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`         |     |     |     |     | a html   |
| `results$adjustedAUC`          |     |     |     |     | a table  |
| `results$covariateEffects`     |     |     |     |     | a table  |
| `results$covariateSpecificAUC` |     |     |     |     | a table  |
| `results$covariateROCPlot`     |     |     |     |     | an image |
| `results$aucByCovPlot`         |     |     |     |     | an image |
| `results$effectEstimatesPlot`  |     |     |     |     | an image |
| `results$stratifiedSummary`    |     |     |     |     | a table  |
| `results$groupSpecificAUC`     |     |     |     |     | a table  |
| `results$interpretation`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$adjustedAUC$asDF`

`as.data.frame(results$adjustedAUC)`
