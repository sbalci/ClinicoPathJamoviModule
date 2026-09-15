# Screening Test Calculator

Calculates positive and negative predictive values for screening and
diagnostic tests using Bayes' theorem. Demonstrates how disease
probability changes with sequential testing and provides Fagan nomograms
for clinical decision-making.

## Usage

``` r
screeningcalculator(
  sens = 0.9,
  spec = 0.8,
  prev = 0.1,
  repeat2 = TRUE,
  repeat3 = TRUE,
  fnote = FALSE,
  fagan = FALSE,
  samplesize = FALSE,
  expected_sens = 0.85,
  expected_spec = 0.8,
  width_sens = 0.1,
  width_spec = 0.1,
  alpha_level = 0.05
)
```

## Arguments

- sens:

  .

- spec:

  .

- prev:

  .

- repeat2:

  .

- repeat3:

  .

- fnote:

  .

- fagan:

  .

- samplesize:

  .

- expected_sens:

  .

- expected_spec:

  .

- width_sens:

  .

- width_spec:

  .

- alpha_level:

  .

## Value

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$explanatoryText`  |     |     |     |     | a html   |
| `results$mathText`         |     |     |     |     | a html   |
| `results$singleTestTable`  |     |     |     |     | a table  |
| `results$repeatTest2Table` |     |     |     |     | a table  |
| `results$repeatTest3Table` |     |     |     |     | a table  |
| `results$plot1`            |     |     |     |     | an image |
| `results$plot2PP`          |     |     |     |     | an image |
| `results$plot2NN`          |     |     |     |     | an image |
| `results$plot3PPP`         |     |     |     |     | an image |
| `results$plot3NNN`         |     |     |     |     | an image |
| `results$sampleSizeTable`  |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$singleTestTable$asDF`

`as.data.frame(results$singleTestTable)`

## Details

This calculator is particularly useful for: • Understanding screening
test performance in different prevalence settings • Sequential testing
scenarios (confirmatory testing) • Teaching Bayesian probability
concepts in medical decision-making • Evaluating diagnostic test chains
in clinical workflows
