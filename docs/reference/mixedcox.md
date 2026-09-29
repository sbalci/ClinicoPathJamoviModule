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
  sparse_matrix = TRUE,
  icc_calculation = TRUE,
  show_fixed_effects = TRUE,
  show_random_effects = TRUE,
  show_model_comparison = TRUE
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

- sparse_matrix:

  Use sparse matrix methods for computational efficiency with large
  datasets.

- icc_calculation:

  Show an approximate latent-scale intercept variance fraction; this is not an observed-event ICC.

- show_fixed_effects:

  Display table of fixed effects estimates.

- show_random_effects:

  Display summary of random effects variance components.

- show_model_comparison:

  Display the mixed and standard Cox log-likelihoods and descriptive likelihood-ratio statistic without an inferential p-value.

## Value

A results object containing:

|                                     |     |     |     |     |           |
|-------------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`                      |     |     |     |     | a html    |
| `results$modelSummary`              |     |     |     |     | a html    |
| `results$fixedEffectsTable`         |     |     |     |     | a table   |
| `results$randomEffectsSummary`      |     |     |     |     | a html    |
| `results$modelComparison`           |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$fixedEffectsTable$asDF`

`as.data.frame(results$fixedEffectsTable)`
