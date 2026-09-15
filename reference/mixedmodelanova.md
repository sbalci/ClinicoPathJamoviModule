# Mixed Model ANOVA

Linear mixed effects models with repeated measures

## Usage

``` r
mixedmodelanova(
  data,
  dependent,
  fixed_factors,
  random_factors,
  covariates,
  model_type = "random_intercept",
  interaction_terms = FALSE,
  estimation_method = "reml",
  show_fixed_effects = TRUE,
  show_random_effects = TRUE,
  show_model_fit = TRUE,
  show_assumptions = TRUE,
  show_posthoc = FALSE,
  posthoc_method = "tukey",
  confidence_level = 0.95,
  show_effect_sizes = TRUE,
  show_icc = TRUE,
  show_plots = TRUE,
  show_methodology = FALSE,
  show_references = FALSE
)

mixedmodelanova(
  data,
  dependent,
  fixed_factors,
  random_factors,
  covariates,
  model_type = "random_intercept",
  interaction_terms = FALSE,
  estimation_method = "reml",
  show_fixed_effects = TRUE,
  show_random_effects = TRUE,
  show_model_fit = TRUE,
  show_assumptions = TRUE,
  show_posthoc = FALSE,
  posthoc_method = "tukey",
  confidence_level = 0.95,
  show_effect_sizes = TRUE,
  show_icc = TRUE,
  show_plots = TRUE,
  show_methodology = FALSE,
  show_references = FALSE
)
```

## Arguments

- data:

  .

- dependent:

  .

- fixed_factors:

  .

- random_factors:

  .

- covariates:

  .

- model_type:

  .

- interaction_terms:

  .

- estimation_method:

  .

- show_fixed_effects:

  .

- show_random_effects:

  .

- show_model_fit:

  .

- show_assumptions:

  .

- show_posthoc:

  .

- posthoc_method:

  .

- confidence_level:

  .

- show_effect_sizes:

  .

- show_icc:

  .

- show_plots:

  .

- show_methodology:

  .

- show_references:

  .

## Value

A results object containing tables and plots

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$model_info`       |     |     |     |     | a table  |
| `results$fixed_effects`    |     |     |     |     | a table  |
| `results$anova_table`      |     |     |     |     | a table  |
| `results$random_effects`   |     |     |     |     | a table  |
| `results$model_fit`        |     |     |     |     | a table  |
| `results$icc_table`        |     |     |     |     | a table  |
| `results$effect_sizes`     |     |     |     |     | a table  |
| `results$posthoc`          |     |     |     |     | a table  |
| `results$assumptions`      |     |     |     |     | a table  |
| `results$diagnostic_plots` |     |     |     |     | an image |
| `results$interpretation`   |     |     |     |     | a html   |
| `results$methodology`      |     |     |     |     | a html   |
| `results$references`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$model_info$asDF`

`as.data.frame(results$model_info)`

## Examples

``` r
if (FALSE) { # \dontrun{
data('sleepstudy', package='lme4')
sleepstudy$DaysFactor <- factor(ifelse(sleepstudy$Days < 5, "early", "late"))
mixedmodelanova(data = sleepstudy, dependent = Reaction,
                fixed_factors = DaysFactor,
                random_factors = Subject)
} # }
```
