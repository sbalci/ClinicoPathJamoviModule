# Mixed-Effects Cox Regression

Performs Cox proportional hazards regression with random effects for
clustered or hierarchical survival data. This method accounts for
correlation within clusters (e.g., patients within hospitals, multiple
events per patient) using mixed-effects modeling. The random effects
capture cluster-specific variation while estimating population-level
fixed effects.

## Usage

``` r
mixedcox(
  data,
  elapsedtime = NULL,
  tint = FALSE,
  dxdate = NULL,
  fudate = NULL,
  timetypedata = "ymd",
  timetypeoutput = "months",
  outcome = NULL,
  outcomeLevel,
  fixed_effects = NULL,
  continuous_effects = NULL,
  cluster_var = NULL,
  random_effects = "intercept",
  random_slope_var = NULL,
  nested_clustering = FALSE,
  nested_cluster_var = NULL,
  correlation_structure = "unstructured",
  sparse_matrix = TRUE,
  optimization_method = "penalized",
  likelihood_ratio_test = TRUE,
  random_effects_significance = TRUE,
  icc_calculation = TRUE,
  residual_analysis = FALSE,
  influence_diagnostics = FALSE,
  random_effects_prediction = FALSE,
  fixed_effects_plot = TRUE,
  random_effects_plot = FALSE,
  cluster_survival_plot = FALSE,
  n_clusters_plot = 5,
  variance_components = TRUE,
  confidence_intervals = TRUE,
  bootstrap_variance = FALSE,
  bootstrap_samples = 500,
  show_fixed_effects = TRUE,
  show_random_effects = TRUE,
  show_model_comparison = TRUE,
  show_cluster_summary = FALSE,
  showSummaries = FALSE,
  showExplanations = FALSE,
  addClusterEffects = FALSE,
  addFittedValues = FALSE
)
```

## Arguments

- data:

  The dataset for analysis, provided as a data frame. Should contain
  survival variables, fixed effects, and clustering variables.

- elapsedtime:

  The numeric variable representing follow-up time until the event or
  censoring.

- tint:

  If true, survival time will be calculated from diagnosis and follow-up
  dates.

- dxdate:

  Date of diagnosis or start of follow-up. Required if tint = true.

- fudate:

  Follow-up date or date of last observation. Required if tint = true.

- timetypedata:

  Specifies the format of date variables in the input data.

- timetypeoutput:

  The units in which survival time is reported in the output.

- outcome:

  The outcome variable indicating event status (e.g., death,
  recurrence).

- outcomeLevel:

  The level of outcome considered as the event.

- fixed_effects:

  Categorical variables for fixed effects in the mixed-effects model.

- continuous_effects:

  Continuous variables for fixed effects in the mixed-effects model.

- cluster_var:

  Variable defining clusters (e.g., hospital, patient, family).
  Observations within the same cluster are assumed correlated.

- random_effects:

  Type of random effects to include in the model.

- random_slope_var:

  Variable for random slopes when random_effects includes slopes.

- nested_clustering:

  Whether to model nested clustering structure (e.g., patients within
  hospitals).

- nested_cluster_var:

  Higher-level clustering variable for nested structures.

- correlation_structure:

  Correlation structure for random effects.

- sparse_matrix:

  Use sparse matrix methods for computational efficiency with large
  datasets.

- optimization_method:

  Method for optimizing the mixed-effects model likelihood.

- likelihood_ratio_test:

  Perform likelihood ratio test comparing mixed-effects vs standard Cox
  model.

- random_effects_significance:

  Test significance of random effects using appropriate methods.

- icc_calculation:

  Calculate intracluster correlation coefficient (ICC).

- residual_analysis:

  Perform residual analysis for mixed-effects Cox model.

- influence_diagnostics:

  Calculate influence diagnostics for clusters and observations.

- random_effects_prediction:

  Predict random effects (BLUPs) for each cluster.

- fixed_effects_plot:

  Generate forest plot for fixed effects coefficients.

- random_effects_plot:

  Generate plots for random effects distribution.

- cluster_survival_plot:

  Generate survival curves for selected clusters.

- n_clusters_plot:

  Number of clusters to display in cluster-specific plots.

- variance_components:

  Estimate and display variance components for random effects.

- confidence_intervals:

  Calculate confidence intervals for fixed and random effects.

- bootstrap_variance:

  Use bootstrap methods for variance estimation.

- bootstrap_samples:

  Number of bootstrap samples for variance estimation.

- show_fixed_effects:

  Display table of fixed effects estimates.

- show_random_effects:

  Display summary of random effects variance components.

- show_model_comparison:

  Display comparison between mixed-effects and standard Cox models.

- show_cluster_summary:

  Display summary statistics by cluster.

- showSummaries:

  Display natural language summaries alongside tables and plots for
  interpretation of mixed-effects Cox regression results.

- showExplanations:

  Display detailed explanations of mixed-effects Cox regression methods
  and interpretation guidelines.

- addClusterEffects:

  Add predicted random effects (BLUPs) as new variables to dataset.

- addFittedValues:

  Add fitted values from mixed-effects model to dataset.

## Value

A results object containing:

|                                     |     |     |     |     |           |
|-------------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`                      |     |     |     |     | a html    |
| `results$modelSummary`              |     |     |     |     | a html    |
| `results$fixedEffectsTable`         |     |     |     |     | a table   |
| `results$randomEffectsSummary`      |     |     |     |     | a html    |
| `results$varianceTable`             |     |     |     |     | a table   |
| `results$iccTable`                  |     |     |     |     | a table   |
| `results$modelComparison`           |     |     |     |     | a html    |
| `results$likelihoodRatioTable`      |     |     |     |     | a table   |
| `results$clusterSummaryTable`       |     |     |     |     | a table   |
| `results$randomEffectsPredTable`    |     |     |     |     | a table   |
| `results$diagnosticsTable`          |     |     |     |     | a table   |
| `results$fixedEffectsPlot`          |     |     |     |     | an image  |
| `results$randomEffectsPlot`         |     |     |     |     | an image  |
| `results$clusterSurvivalPlot`       |     |     |     |     | an image  |
| `results$residualPlot`              |     |     |     |     | an image  |
| `results$analysisSummary`           |     |     |     |     | a html    |
| `results$methodExplanation`         |     |     |     |     | a html    |
| `results$clusteringExplanation`     |     |     |     |     | a html    |
| `results$randomEffectsExplanation`  |     |     |     |     | a html    |
| `results$iccExplanation`            |     |     |     |     | a html    |
| `results$modelSelectionExplanation` |     |     |     |     | a html    |
| `results$bootstrapTable`            |     |     |     |     | a table   |
| `results$calculatedtime`            |     |     |     |     | an output |
| `results$clusterEffectsOutput`      |     |     |     |     | an output |
| `results$fittedValuesOutput`        |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$fixedEffectsTable$asDF`

`as.data.frame(results$fixedEffectsTable)`
