# Group LASSO for Survival Analysis

Group LASSO for Cox proportional hazards models enabling simultaneous
selection of pre-defined variable groups while maintaining within-group
structure. This method applies the group LASSO penalty (L1/L2 mixed
norm) at the group level using the grpreg package, ensuring entire
groups of variables are selected or excluded together. Ideal for
categorical variables with multiple dummy codes, grouped biomarkers, or
structured predictors like gene pathways. Supports adaptive group
weights and comprehensive cross-validation for optimal penalty
selection. Particularly valuable for genomic survival analysis, clinical
prediction models with natural variable groupings, and scenarios
requiring interpretable group-wise feature selection with preserved
biological or clinical structure.

## Usage

``` r
grouplasso(
  data,
  suitabilityCheck = TRUE,
  time,
  event,
  outcomeLevel,
  censorLevel,
  predictors,
  group_definition = "automatic",
  group_structure = "",
  factor_grouping = TRUE,
  penalty_type = "group_lasso",
  group_weights = "sqrt_size",
  custom_weights = "",
  cv_folds = 10,
  n_lambda = 100,
  lambda_min_ratio = 0.001,
  max_iterations = 10000,
  tolerance = 0.001,
  selection_threshold = 1e-08,
  stability_selection = FALSE,
  bootstrap_samples = 100,
  stability_threshold = 0.6,
  nested_cv = FALSE,
  inner_cv_folds = 5,
  permutation_test = FALSE,
  n_permutations = 100,
  show_group_summary = TRUE,
  show_coefficients = TRUE,
  show_path_summary = TRUE,
  show_cv_results = TRUE,
  plot_regularization_path = TRUE,
  plot_cv_curve = TRUE,
  plot_group_importance = TRUE,
  plot_stability = FALSE,
  plot_group_structure = FALSE,
  showSummary = FALSE,
  showExplanations = FALSE,
  standardize = TRUE,
  adaptive_weights_method = "ridge",
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- suitabilityCheck:

  Run a comprehensive data suitability assessment before analysis.
  Checks sample size, events-per-variable ratio, multicollinearity, and
  whether regularization is needed.

- time:

  Time to event variable (numeric). For right-censored data, this is the
  time from study entry to event or censoring.

- event:

  Event indicator variable. For survival analysis: 0 = censored, 1 =
  event. For competing risks: 0 = censored, 1+ = different event types.

- outcomeLevel:

  Level of `event` that represents the event of interest (coded as 1).
  Rows matching neither this level nor `censorLevel` are excluded as NA.

- censorLevel:

  Level of `event` that represents censoring (coded as 0). Together with
  `outcomeLevel`, this defines a strict two-level encoding: rows whose
  event value matches neither level are treated as missing and excluded.

- predictors:

  Variables to include in the group LASSO Cox model. Variables can be
  grouped based on clinical or statistical criteria. Factor variables
  are automatically converted to dummy variables.

- group_definition:

  Method for defining variable groups. Automatic groups each original
  variable (with its dummy columns) as a separate group. Custom allows
  manual specification. Factor-based groups dummy variables from the
  same factor together.

- group_structure:

  Custom group assignment as comma-separated list. Format: 'var1:group1,
  var2:group1, var3:group2'. Only used when group_definition is
  'custom'.

- factor_grouping:

  When using automatic grouping, automatically group dummy variables
  from the same factor variable into a single group. Ensures that factor
  variables are selected or excluded as complete units.

- penalty_type:

  Type of group penalty via grpreg. Group LASSO (L1/L2 mixed norm)
  selects entire groups simultaneously. Group MCP and Group SCAD provide
  non-convex alternatives with less bias for large coefficients.
  Adaptive Group LASSO uses data-driven penalty weights for improved
  oracle properties.

- group_weights:

  Method for calculating group-specific penalty multipliers passed to
  grpreg. Square root of group size is the standard choice. Custom
  allows user-specified weights.

- custom_weights:

  Custom weights for each group as comma-separated values. Order should
  match group numbering. Only used with custom weights.

- cv_folds:

  Number of folds for cross-validation to select optimal penalty
  parameter. More folds provide better estimates but increase
  computation time.

- n_lambda:

  Number of lambda values in the regularization path. More values
  provide finer resolution but increase computation time.

- lambda_min_ratio:

  Ratio of smallest to largest lambda in automatic sequence. Smaller
  values explore stronger penalties.

- max_iterations:

  Maximum iterations for the coordinate descent algorithm. Increase if
  convergence warnings occur.

- tolerance:

  Convergence tolerance for optimization. Smaller values provide more
  precise solutions but increase computation time.

- selection_threshold:

  Threshold for determining selected groups. Groups with maximum
  coefficient magnitude below this value are considered unselected.

- stability_selection:

  Perform stability selection across subsamples to identify robust group
  selection patterns and reduce selection variability.

- bootstrap_samples:

  Number of subsamples for stability selection. More samples provide
  more stable group selection.

- stability_threshold:

  Minimum selection frequency for groups in stability selection. Higher
  thresholds provide more conservative selection.

- nested_cv:

  Perform nested cross-validation for unbiased performance estimation.
  Provides honest assessment of model performance with optimal
  penalties.

- inner_cv_folds:

  Number of inner CV folds for nested cross-validation. Used for penalty
  selection within each outer fold.

- permutation_test:

  Perform permutation test to assess statistical significance of group
  selection and overall model performance.

- n_permutations:

  Number of permutations for significance testing. More permutations
  provide more accurate p-values.

- show_group_summary:

  Display summary of group definitions, sizes, and selection status with
  penalty parameter information.

- show_coefficients:

  Display coefficient estimates for selected variables organized by
  groups with selection indicators.

- show_path_summary:

  Display summary of regularization path showing group entry and exit
  points along penalty sequence.

- show_cv_results:

  Display cross-validation results including optimal penalty selection
  and performance metrics.

- plot_regularization_path:

  Plot group-wise coefficient paths showing how groups enter/exit the
  model as penalty increases.

- plot_cv_curve:

  Plot cross-validation performance curve with optimal penalty selection
  and confidence bands.

- plot_group_importance:

  Visualize relative importance of selected groups based on coefficient
  norms and selection frequency.

- plot_stability:

  Plot stability selection results showing group selection frequencies
  across subsamples.

- plot_group_structure:

  Visualize group structure and variable assignments.

- showSummary:

  Display a plain-language summary paragraph of the analysis results,
  suitable for pasting into reports or tumor board notes.

- showExplanations:

  Display an educational panel explaining what Group LASSO does, when to
  use it, assumptions, and how to interpret the outputs.

- standardize:

  Standardize variables before fitting. Recommended for optimal penalty
  performance across different variable scales.

- adaptive_weights_method:

  Method for calculating adaptive weights for groups. Only used when
  penalty_type is adaptive_group. Ridge provides stable estimates via
  ridge Cox regression. Univariate uses marginal Cox regression for each
  group separately.

- random_seed:

  Random seed for cross-validation folds and bootstrap sampling. Ensures
  reproducible results across analyses.

## Value

A results object containing:

|                                |     |     |     |     |                |
|--------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`              |     |     |     |     | a preformatted |
| `results$instructions`         |     |     |     |     | a html         |
| `results$todo`                 |     |     |     |     | a html         |
| `results$suitabilityReport`    |     |     |     |     | a html         |
| `results$groupSummary`         |     |     |     |     | a table        |
| `results$coefficients`         |     |     |     |     | a table        |
| `results$pathSummary`          |     |     |     |     | a table        |
| `results$cvResults`            |     |     |     |     | a table        |
| `results$stabilityResults`     |     |     |     |     | a table        |
| `results$modelPerformanceNote` |     |     |     |     | a html         |
| `results$modelPerformance`     |     |     |     |     | a table        |
| `results$nestedCVResults`      |     |     |     |     | a table        |
| `results$permutationResults`   |     |     |     |     | a table        |
| `results$pathPlot`             |     |     |     |     | an image       |
| `results$cvPlot`               |     |     |     |     | an image       |
| `results$importancePlot`       |     |     |     |     | an image       |
| `results$stabilityPlot`        |     |     |     |     | an image       |
| `results$groupStructurePlot`   |     |     |     |     | an image       |
| `results$summary`              |     |     |     |     | a html         |
| `results$explanations`         |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$groupSummary$asDF`

`as.data.frame(results$groupSummary)`
