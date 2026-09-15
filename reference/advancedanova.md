# Advanced ANOVA Suite

Advanced ANOVA analysis with comprehensive post hoc testing, effect
sizes, and assumption checking. Addresses the critical issue where 68
percent of pathology studies fail proper multiple comparisons.

## Usage

``` r
advancedanova(
  data,
  dependent,
  fixed,
  covariates,
  wls,
  model_type = "oneway",
  posthoc_method = "tukey",
  control_group = "",
  assumptions = TRUE,
  effect_sizes = TRUE,
  descriptives = TRUE,
  show_plots = TRUE,
  plot_type = "both",
  confidence_level = 0.95,
  alpha_level = 0.05,
  welch_correction = FALSE,
  robust_anova = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- dependent:

  The dependent variable from `data`, variable must be numeric

- fixed:

  The grouping variable(s) from `data`, variable(s) must be a factor

- covariates:

  Optional covariates for ANCOVA analysis

- wls:

  Optional weights for weighted least squares

- model_type:

  Type of ANOVA model to fit

- posthoc_method:

  Post hoc comparison method

- control_group:

  Control group for Dunnett's test comparisons

- assumptions:

  Check and report ANOVA assumptions

- effect_sizes:

  Calculate eta-squared, omega-squared, and Cohen's f

- descriptives:

  Show descriptive statistics for each group

- show_plots:

  Generate diagnostic and comparison plots

- plot_type:

  Type of plots to generate

- confidence_level:

  Confidence level for effect size confidence intervals

- alpha_level:

  Alpha level for significance testing

- welch_correction:

  Apply Welch correction for unequal variances

- robust_anova:

  Use robust ANOVA methods for non-normal data

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Instructions for using the Advanced ANOVA Suite |
| `results$descriptives` |  |  |  |  | Descriptive statistics for each group |
| `results$assumptions` |  |  |  |  | Tests of ANOVA assumptions |
| `results$anova` |  |  |  |  | Main ANOVA results with effect sizes |
| `results$tukey` |  |  |  |  | Tukey Honestly Significant Difference test results |
| `results$gameshowell` |  |  |  |  | Games-Howell test for unequal variances |
| `results$dunnett` |  |  |  |  | Dunnett's test comparing treatments to control |
| `results$bonferroni` |  |  |  |  | Bonferroni-corrected pairwise comparisons |
| `results$anovaplot` |  |  |  |  | Violin plots with boxplots and group means |
| `results$diagnosticplot` |  |  |  |  | Residual plots and assumption checking |
| `results$meansplot` |  |  |  |  | Group means with confidence intervals |
| `results$interpretation` |  |  |  |  | Clinical context and interpretation guidelines |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptives$asDF`

`as.data.frame(results$descriptives)`
