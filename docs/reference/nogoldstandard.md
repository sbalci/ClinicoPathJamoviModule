# Analysis Without Gold Standard

Analysis of diagnostic tests without a gold standard reference using
multiple statistical approaches. Implements Latent Class Analysis (Hui &
Walter, 1980), Bayesian methods (Joseph et al., 1995), and composite
reference standards for estimating test performance when no perfect
reference test exists.

## Usage

``` r
nogoldstandard(
  data,
  clinicalPreset = "none",
  test1,
  test1Positive,
  test2 = NULL,
  test2Positive,
  test3 = NULL,
  test3Positive,
  test4 = NULL,
  test4Positive,
  test5 = NULL,
  test5Positive,
  method = "latent_class",
  bootstrap = FALSE,
  nboot = 1000,
  alpha = 0.05,
  verbose = FALSE,
  seed = 0
)
```

## Arguments

- data:

  The data as a data frame.

- clinicalPreset:

  Predefined clinical scenarios with optimized settings and method
  recommendations.

- test1:

  First diagnostic test variable.

- test1Positive:

  The positive level for Test 1.

- test2:

  Second diagnostic test variable.

- test2Positive:

  The positive level for Test 2.

- test3:

  Third diagnostic test variable (optional).

- test3Positive:

  The positive level for Test 3.

- test4:

  Fourth diagnostic test variable (optional).

- test4Positive:

  The positive level for Test 4.

- test5:

  Fifth diagnostic test variable (optional).

- test5Positive:

  The positive level for Test 5.

- method:

  Method for analyzing tests without gold standard.

- bootstrap:

  Calculate bootstrap confidence intervals.

- nboot:

  Number of bootstrap samples for confidence intervals.

- alpha:

  Alpha level for confidence intervals.

- verbose:

  Show detailed progress messages during bootstrap analysis.

- seed:

  Base random seed for the reproducible latent-class multi-start search.
  Each start is offset from this base, so changing it shifts the whole
  reproducible sequence; the default (0) reproduces the previous
  behaviour.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a preformatted |
| `results$instructions` |  |  |  |  | a html |
| `results$agreement_stats` |  |  |  |  | a table |
| `results$clinical_summary` |  |  |  |  | a html |
| `results$method_guide` |  |  |  |  | a html |
| `results$prevalence` |  |  |  |  | a table |
| `results$test_metrics` |  |  |  |  | a table |
| `results$model_fit` |  |  |  |  | a table |
| `results$conditional_dependence` |  |  |  |  | Latent class analysis assumes the tests err independently given true disease status. For each pair this compares the observed two-way table with the one the fitted model implies; a residual above 3.84 (the 5 percent point of chi-squared on 1 degree of freedom) is evidence that the pair shares a source of error, which inflates the estimated accuracy. Requires four or more tests: with three the model is just-identified and reproduces every table exactly, so no residual can reveal dependence. |
| `results$diagnostics` |  |  |  |  | Detail of how the estimates were produced: sample size, method, convergence, number of random starts used, and bootstrap failures. Shown only when Verbose output is enabled. |
| `results$crosstab` |  |  |  |  | a table |
| `results$agreement_plot` |  |  |  |  | an image |
| `results$agreement_plot2` |  |  |  |  | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$agreement_stats$asDF`

`as.data.frame(results$agreement_stats)`
