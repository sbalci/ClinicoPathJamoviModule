# Medical Decision

Function for Medical Decision Analysis. Sensitivity, Specificity,
Positive Predictive Value, Negative Predictive Value.

## Usage

``` r
decision(
  data,
  gold,
  goldPositive,
  newtest,
  testPositive,
  goldNegative,
  testNegative,
  pp = FALSE,
  pprob = 0.3,
  od = FALSE,
  fnote = FALSE,
  ci = FALSE,
  fagan = FALSE,
  showNaturalLanguage = FALSE,
  showClinicalInterpretation = FALSE,
  showReportTemplate = FALSE,
  showAboutAnalysis = FALSE,
  showMisclassified = FALSE,
  maxCasesShow = 50
)
```

## Arguments

- data:

  The data as a data frame. The data frame should contain the variables
  specified in the 'variables' option.

- gold:

  The gold standard reference variable representing true disease status.

- goldPositive:

  The level indicating presence of disease in the gold standard
  variable.

- newtest:

  The diagnostic test variable being evaluated for performance.

- testPositive:

  The level representing a positive result for the test under
  evaluation.

- goldNegative:

  The level indicating absence of disease in the gold standard variable.

- testNegative:

  The level representing a negative result for the test under
  evaluation.

- pp:

  Boolean selection whether to use known population prevalence instead
  of study prevalence.

- pprob:

  Population disease prevalence as a proportion between 0.001 and 0.999.

- od:

  Boolean selection whether to show original data frequency tables.

- fnote:

  Boolean selection whether to show detailed explanatory footnotes.

- ci:

  Boolean selection whether to calculate and display 95 percent
  confidence intervals.

- fagan:

  Boolean selection whether to generate a Fagan nomogram plot.

- showNaturalLanguage:

  Boolean selection whether to show the natural language clinical
  summary.

- showClinicalInterpretation:

  Boolean selection whether to show the clinical interpretation guide.

- showReportTemplate:

  Boolean selection whether to show the copy-ready report template.

- showAboutAnalysis:

  Boolean selection whether to show the about analysis section.

- showMisclassified:

  Boolean selection whether to show detailed misclassified cases tables.

- maxCasesShow:

  Maximum number of misclassified cases to display in tables.

## Value

A results object containing:

|                                           |     |     |     |     |           |
|-------------------------------------------|-----|-----|-----|-----|-----------|
| `results$welcome`                         |     |     |     |     | a html    |
| `results$notices`                         |     |     |     |     | a html    |
| `results$rawContingency`                  |     |     |     |     | a table   |
| `results$rawCounts`                       |     |     |     |     | a table   |
| `results$cTable`                          |     |     |     |     | a table   |
| `results$nTable`                          |     |     |     |     | a table   |
| `results$ratioTable`                      |     |     |     |     | a table   |
| `results$missingDataSummary`              |     |     |     |     | a html    |
| `results$epirTable_ratio`                 |     |     |     |     | a table   |
| `results$epirTable_number`                |     |     |     |     | a table   |
| `results$plot1`                           |     |     |     |     | an image  |
| `results$naturalLanguageSummary`          |     |     |     |     | a html    |
| `results$clinicalInterpretation`          |     |     |     |     | a html    |
| `results$reportTemplate`                  |     |     |     |     | a html    |
| `results$aboutAnalysis`                   |     |     |     |     | a html    |
| `results$misclassifiedHeading`            |     |     |     |     | a html    |
| `results$confusionMatrixSummary`          |     |     |     |     | a table   |
| `results$falsePositiveTable`              |     |     |     |     | a table   |
| `results$falseNegativeTable`              |     |     |     |     | a table   |
| `results$misclassificationInterpretation` |     |     |     |     | a html    |
| `results$saveClassifications`             |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$rawContingency$asDF`

`as.data.frame(results$rawContingency)`
