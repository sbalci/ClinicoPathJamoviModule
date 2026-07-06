# Compare Medical Decision Tests

Function for comparing multiple Medical Decision Tests. Compares
sensitivity, specificity, positive predictive value, negative predictive
value, and other metrics between different tests against the same golden
standard. Includes statistical comparison using McNemar's test and
confidence intervals for differences.

## Usage

``` r
decisioncompare(
  data,
  gold,
  goldPositive,
  test1,
  test1Positive,
  test2,
  test2Positive,
  test3 = NULL,
  test3Positive,
  pp = FALSE,
  pprob = 0.3,
  od = FALSE,
  fnote = FALSE,
  ci = FALSE,
  plot = FALSE,
  excludeIndeterminate = FALSE,
  radarplot = FALSE,
  heatmap = FALSE,
  opa = FALSE,
  niMargin = 75,
  ciMethod = "wilson",
  stratify = NULL,
  statComp = FALSE,
  showSummary = FALSE,
  showExplanations = FALSE,
  showReportSentence = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- gold:

  The gold standard reference variable representing true disease status.

- goldPositive:

  The level indicating presence of disease in the gold standard
  variable.

- test1:

  The first diagnostic test variable being evaluated for performance.

- test1Positive:

  The level representing a positive result for Test 1.

- test2:

  The second diagnostic test variable for comparison analysis.

- test2Positive:

  The level representing a positive result for Test 2.

- test3:

  Optional third diagnostic test variable for extended comparison
  analysis.

- test3Positive:

  The level representing a positive result for Test 3.

- pp:

  Enable custom prior probability (prevalence) for predictive value
  calculations.

- pprob:

  Prior probability (disease prevalence in the community). Requires a
  value between 0.001 and 0.999, default 0.300.

- od:

  Boolean selection whether to show frequency tables. Default is
  'false'.

- fnote:

  .

- ci:

  .

- plot:

  Generate comparison plot showing test performance metrics.

- excludeIndeterminate:

  If TRUE, drop rows where test/gold values are neither the specified
  positive level nor a clear negative level to avoid inflating
  specificity/NPV from equivocal results.

- radarplot:

  Generate radar plot for comprehensive test comparison visualization.

- heatmap:

  Generate concordance heatmap comparing per-case test results against
  gold standard.

- opa:

  Show overall percent agreement with 95 percent confidence intervals
  for each test.

- niMargin:

  Noninferiority margin as percentage (default 75). Used when OPA is
  enabled.

- ciMethod:

  CI method: wilson, logit, or exact (Clopper-Pearson).

- stratify:

  Optional stratification variable for subgroup-specific diagnostic
  accuracy analysis.

- statComp:

  Perform statistical comparison between tests (McNemar's test and
  confidence intervals for differences).

- showSummary:

  Boolean to show natural language summary of statistical comparisons.

- showExplanations:

  Boolean to show explanations, glossary, and educational content.

- showReportSentence:

  Boolean to show manuscript-ready report sentence.

## Value

A results object containing:

|                               |     |     |     |     |                |
|-------------------------------|-----|-----|-----|-----|----------------|
| `results$text1`               |     |     |     |     | a preformatted |
| `results$text2`               |     |     |     |     | a html         |
| `results$cTable1`             |     |     |     |     | a table        |
| `results$epirTable1`          |     |     |     |     | a table        |
| `results$cTable2`             |     |     |     |     | a table        |
| `results$epirTable2`          |     |     |     |     | a table        |
| `results$cTable3`             |     |     |     |     | a table        |
| `results$epirTable3`          |     |     |     |     | a table        |
| `results$comparisonTable`     |     |     |     |     | a table        |
| `results$opaTable`            |     |     |     |     | a table        |
| `results$stratifiedTable`     |     |     |     |     | a table        |
| `results$mcnemarTable`        |     |     |     |     | a table        |
| `results$diffTable`           |     |     |     |     | a table        |
| `results$plot1`               |     |     |     |     | an image       |
| `results$plotRadar`           |     |     |     |     | an image       |
| `results$plotHeatmap`         |     |     |     |     | an image       |
| `results$summaryReport`       |     |     |     |     | a html         |
| `results$reportSentence`      |     |     |     |     | a html         |
| `results$explanationsContent` |     |     |     |     | a html         |
| `results$clinicalReport`      |     |     |     |     | a html         |
| `results$aboutAnalysis`       |     |     |     |     | a html         |
| `results$notices`             |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$cTable1$asDF`

`as.data.frame(results$cTable1)`
