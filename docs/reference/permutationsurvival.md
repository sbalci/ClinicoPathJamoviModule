# Permutation Tests for Survival

Performs permutation-based tests for comparing survival distributions
between groups. These tests provide non-parametric alternatives to
traditional log-rank tests that do not rely on asymptotic theory, making
them particularly suitable for small samples or when distributional
assumptions are violated. Includes various permutation strategies and
multiple comparison corrections for robust survival analysis.

## Usage

``` r
permutationsurvival(
  data,
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  permutation_method = "approximate",
  n_permutations = 10000,
  test_statistic = "logrank",
  stratify_variable,
  confidence_level = 0.95,
  multiple_comparison = "holm",
  show_permutation_distribution = FALSE,
  show_survival_curves = TRUE,
  show_test_progression = FALSE,
  seed_value = 123,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  Time variable for survival analysis

- outcome:

  Event indicator variable

- explanatory:

  Grouping variable for comparison

- outcomeLevel:

  Level of the outcome variable that represents the event of interest.

- permutation_method:

  Method for generating permutations of group assignments.

- n_permutations:

  Number of permutations to generate for approximate tests.

- test_statistic:

  Test statistic to use for permutation testing.

- stratify_variable:

  Optional stratification variable for stratified permutation tests.

- confidence_level:

  Confidence level for all confidence intervals.

- multiple_comparison:

  Method for adjusting p-values for multiple comparisons.

- show_permutation_distribution:

  Display histogram of permutation test statistics.

- show_survival_curves:

  Display Kaplan-Meier survival curves for comparison groups.

- show_test_progression:

  Display progression of p-values as permutations accumulate.

- seed_value:

  Random seed for reproducible permutation results.

- showSummaries:

  Generate natural language summary of results.

- showExplanations:

  Show detailed methodology explanations.

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                        |     |     |     |     | a html   |
| `results$permutationTestsTable`       |     |     |     |     | a table  |
| `results$groupSummaryTable`           |     |     |     |     | a table  |
| `results$survivalPlot`                |     |     |     |     | an image |
| `results$permutationDistributionPlot` |     |     |     |     | an image |
| `results$testProgressionPlot`         |     |     |     |     | an image |
| `results$analysisSummary`             |     |     |     |     | a html   |
| `results$methodExplanation`           |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$permutationTestsTable$asDF`

`as.data.frame(results$permutationTestsTable)`
