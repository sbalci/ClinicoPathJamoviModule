# Co-Testing Analysis

Function for analyzing combined results of two concurrent diagnostic
tests. Calculates post-test probabilities based on various scenarios
(either test positive, both positive, both negative).

## Usage

``` r
cotest(
  test1_sens = 0.8,
  test1_spec = 0.9,
  test2_sens = 0.75,
  test2_spec = 0.95,
  indep = FALSE,
  cond_dep_pos = 0.05,
  cond_dep_neg = 0.05,
  prevalence = 0.1,
  fnote = FALSE,
  fagan = FALSE,
  preset = "custom"
)
```

## Arguments

- test1_sens:

  Sensitivity (true positive rate) of Test 1.

- test1_spec:

  Specificity (true negative rate) of Test 1.

- test2_sens:

  Sensitivity (true positive rate) of Test 2.

- test2_spec:

  Specificity (true negative rate) of Test 2.

- indep:

  Assume tests are conditionally independent (default is false for
  safety). Use true only if tests measure completely different
  phenomena.

- cond_dep_pos:

  Conditional dependence between tests for subjects with disease. Value
  between 0 (independence) and 1 (complete dependence).

- cond_dep_neg:

  Conditional dependence between tests for subjects without disease.
  Value between 0 (independence) and 1 (complete dependence).

- prevalence:

  Prior probability (disease prevalence in the population). Requires a
  value between 0.001 and 0.999.

- fnote:

  .

- fagan:

  .

- preset:

  Select a clinical preset or use custom values. Presets load
  evidence-based sensitivity and specificity values from medical
  literature with appropriate dependence parameters and prevalence
  estimates.

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`          |     |     |     |     | a html   |
| `results$notices`               |     |     |     |     | a html   |
| `results$testParamsTable`       |     |     |     |     | a table  |
| `results$cotestResultsTable`    |     |     |     |     | a table  |
| `results$dependenceInfo`        |     |     |     |     | a html   |
| `results$dependenceExplanation` |     |     |     |     | a html   |
| `results$explanation`           |     |     |     |     | a html   |
| `results$plot1`                 |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$testParamsTable$asDF`

`as.data.frame(results$testParamsTable)`
