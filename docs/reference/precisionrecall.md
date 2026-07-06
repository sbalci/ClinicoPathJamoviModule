# Precision-Recall Curve

Precision-Recall (PRC) curve analysis for evaluating binary classifiers,
especially on imbalanced datasets. Unlike ROC curves, PRC curves show
how precision (positive predictive value) varies with recall
(sensitivity).

## Usage

``` r
precisionrecall(
  data,
  outcome,
  positiveClass,
  scores,
  interpolation = "nonlinear",
  showBaseline = TRUE,
  aucMethod = "trapezoid",
  ci = FALSE,
  ciMethod = "bootstrap",
  ciSamples = 1000,
  ciWidth = 95,
  comparison = FALSE,
  comparisonMethod = "bootstrap",
  showROC = FALSE,
  showFScore = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- outcome:

  Binary outcome variable (0/1, TRUE/FALSE, or factor with 2 levels)

- positiveClass:

  Value representing positive class (disease/event)

- scores:

  One or more continuous variables containing classifier scores or
  predicted probabilities

- interpolation:

  PRC requires non-linear interpolation. Linear shown for educational
  comparison.

- showBaseline:

  Display horizontal baseline at y = P/(P+N) representing random
  classifier

- aucMethod:

  Method for calculating area under PRC curve

- ci:

  .

- ciMethod:

  .

- ciSamples:

  .

- ciWidth:

  .

- comparison:

  Perform statistical comparison of multiple PRC curves

- comparisonMethod:

  .

- showROC:

  Display ROC curve alongside PRC for comparison

- showFScore:

  Display F₁ score iso-lines on PRC plot

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$instructions`    |     |     |     |     | a html   |
| `results$aucTable`        |     |     |     |     | a table  |
| `results$comparisonTable` |     |     |     |     | a table  |
| `results$prcPlot`         |     |     |     |     | an image |
| `results$rocPlot`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$aucTable$asDF`

`as.data.frame(results$aucTable)`

## Details

Based on Saito & Rehmsmeier (2015): "The Precision-Recall Plot Is More
Informative than the ROC Plot When Evaluating Binary Classifiers on
Imbalanced Datasets." PLoS ONE 10(3): e0118432.
