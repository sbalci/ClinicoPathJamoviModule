# Enhanced Chi-Square and Fisher's Tests

Enhanced categorical analysis with chi-square tests, Fisher's exact
tests, effect sizes, and association measures. Addresses the critical
issue where 25 percent of pathology studies use categorical analysis
incorrectly.

## Usage

``` r
categoricaladvanced(
  data,
  rows,
  cols,
  stratify,
  test_type = "enhanced",
  fisher_exact = TRUE,
  effect_sizes = TRUE,
  association_measures = TRUE,
  residual_analysis = TRUE,
  posthoc_comparisons = FALSE,
  correction_method = "bonferroni",
  confidence_level = 0.95,
  exact_threshold = 50,
  simulation_runs = 5000,
  show_expected = TRUE,
  plot_mosaic = TRUE,
  plot_residuals = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- rows:

  Row variable for the contingency table

- cols:

  Column variable for the contingency table

- stratify:

  Optional stratification variable for Mantel-Haenszel tests

- test_type:

  Type of categorical analysis to perform

- fisher_exact:

  Perform Fisher's exact test when appropriate

- effect_sizes:

  Calculate effect sizes (Cramér's V, phi coefficient, Cohen's w)

- association_measures:

  Calculate association measures (Lambda, Tau, Gamma)

- residual_analysis:

  Perform standardized residual analysis

- posthoc_comparisons:

  Perform pairwise comparisons for tables larger than 2×2

- correction_method:

  Multiple testing correction method

- confidence_level:

  Confidence level for effect size intervals

- exact_threshold:

  Maximum total sample size for automatic exact tests

- simulation_runs:

  Number of Monte Carlo simulations for exact p-values

- show_expected:

  Display expected frequencies in contingency table

- plot_mosaic:

  Generate mosaic plot showing association patterns

- plot_residuals:

  Generate standardized residuals plot

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$contingencytable`   |     |     |     |     | a table  |
| `results$chisquaretest`      |     |     |     |     | a table  |
| `results$fishertest`         |     |     |     |     | a table  |
| `results$effectsizes`        |     |     |     |     | a table  |
| `results$associations`       |     |     |     |     | a table  |
| `results$residuals`          |     |     |     |     | a table  |
| `results$posthoc`            |     |     |     |     | a table  |
| `results$stratifiedanalysis` |     |     |     |     | a table  |
| `results$mosaicplot`         |     |     |     |     | an image |
| `results$residualsplot`      |     |     |     |     | an image |
| `results$interpretation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$contingencytable$asDF`

`as.data.frame(results$contingencytable)`
