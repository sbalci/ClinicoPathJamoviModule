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
  test2,
  test2Positive,
  test3 = NULL,
  test3Positive,
  test4 = NULL,
  test4Positive,
  test5 = NULL,
  test5Positive,
  method = "all_positive",
  bootstrap = FALSE,
  nboot = 1000,
  alpha = 0.05,
  verbose = FALSE
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

## Value

A results object containing:

|                            |     |     |     |     |                |
|----------------------------|-----|-----|-----|-----|----------------|
| `results$notices`          |     |     |     |     | a preformatted |
| `results$instructions`     |     |     |     |     | a html         |
| `results$agreement_stats`  |     |     |     |     | a table        |
| `results$clinical_summary` |     |     |     |     | a html         |
| `results$method_guide`     |     |     |     |     | a html         |
| `results$prevalence`       |     |     |     |     | a table        |
| `results$test_metrics`     |     |     |     |     | a table        |
| `results$model_fit`        |     |     |     |     | a table        |
| `results$crosstab`         |     |     |     |     | a table        |
| `results$agreement_plot`   |     |     |     |     | an image       |
| `results$agreement_plot2`  |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$agreement_stats$asDF`

`as.data.frame(results$agreement_stats)`
