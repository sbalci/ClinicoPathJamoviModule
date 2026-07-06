# Medical Decision Calculator

Medical Decision Calculator for diagnostic test evaluation when you have
the four key counts: True Positives (TP), False Positives (FP), True
Negatives (TN), and False Negatives (FN). Calculates comprehensive
diagnostic performance metrics including sensitivity, specificity,
positive and negative predictive values, likelihood ratios, and
post-test probabilities. Supports confidence interval estimation and
Fagan nomogram visualization for clinical decision making.

## Usage

``` r
decisioncalculator(
  TP = 90,
  TN = 80,
  FP = 30,
  FN = 20,
  pp = FALSE,
  pprob = 0.3,
  fnote = FALSE,
  ci = FALSE,
  fagan = FALSE,
  showWelcome = TRUE,
  showSummary = FALSE,
  showAbout = FALSE,
  showGlossary = FALSE,
  multiplecuts = FALSE,
  cutoff1 = "Conservative",
  tp1 = 85,
  fp1 = 10,
  tn1 = 190,
  fn1 = 15,
  cutoff2 = "Aggressive",
  tp2 = 95,
  fp2 = 25,
  tn2 = 175,
  fn2 = 5
)
```

## Arguments

- TP:

  True Positive count: cases with disease that tested positive.

- TN:

  True Negative count: cases without disease that tested negative.

- FP:

  False Positive count: cases without disease that tested positive.

- FN:

  False Negative count: cases with disease that tested negative.

- pp:

  Boolean selection whether to use known population prevalence.

- pprob:

  Prior probability (disease prevalence in the community). Requires a
  value between 0.001 and 0.999, default 0.300.

- fnote:

  Boolean selection whether to show detailed explanatory footnotes.

- ci:

  Boolean selection whether to calculate 95 percent confidence
  intervals.

- fagan:

  Boolean selection whether to generate a Fagan nomogram plot.

- showWelcome:

  Boolean selection whether to show welcome message.

- showSummary:

  Boolean selection whether to show plain-language summary of results.

- showAbout:

  Boolean selection whether to show about and assumptions panels.

- showGlossary:

  Boolean selection whether to show clinical glossary.

- multiplecuts:

  Boolean selection whether to evaluate multiple cut-off scenarios.

- cutoff1:

  Name identifier for cut-off scenario 1.

- tp1:

  .

- fp1:

  .

- tn1:

  .

- fn1:

  .

- cutoff2:

  Name identifier for cut-off scenario 2.

- tp2:

  .

- fp2:

  .

- tn2:

  .

- fn2:

  .

## Value

A results object containing:

|                                |     |     |     |     |                |
|--------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`              |     |     |     |     | a preformatted |
| `results$welcome`              |     |     |     |     | a html         |
| `results$summary`              |     |     |     |     | a html         |
| `results$about`                |     |     |     |     | a html         |
| `results$assumptions`          |     |     |     |     | a html         |
| `results$glossary`             |     |     |     |     | a html         |
| `results$cTable`               |     |     |     |     | a table        |
| `results$nTable`               |     |     |     |     | a table        |
| `results$ratioTable`           |     |     |     |     | a table        |
| `results$advancedMetricsTable` |     |     |     |     | a table        |
| `results$epirTable_ratio`      |     |     |     |     | a table        |
| `results$epirTable_number`     |     |     |     |     | a table        |
| `results$plot1`                |     |     |     |     | an image       |
| `results$multipleCutoffTable`  |     |     |     |     | a table        |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$cTable$asDF`

`as.data.frame(results$cTable)`
