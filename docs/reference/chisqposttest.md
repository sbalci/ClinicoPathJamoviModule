# Chi-Square Post-Hoc Tests

Performs Chi-Square test and post-hoc pairwise comparisons. Post-hoc
pairwise comparisons are ONLY performed when the overall chi-square test
is significant (p \< alpha). This enforces proper statistical workflow
and prevents data dredging. Selecting 'None' for post-hoc method
DISABLES all pairwise testing. If you want unadjusted pairwise
comparisons, this feature is not available (by design, as it would
encourage inappropriate multiple testing). No automated validation
against established packages exists. Use with caution for clinical
decision-making.

## Usage

``` r
chisqposttest(
  data,
  rows,
  cols,
  counts = NULL,
  posthoc = "bonferroni",
  sig = 0.05,
  excl = FALSE,
  exp = FALSE,
  plot = FALSE,
  showResiduals = FALSE,
  showEducational = FALSE,
  showDetailedTables = FALSE,
  residualsCutoff = 2,
  phiCI = FALSE,
  testSelection = "auto",
  exportResults = FALSE,
  showClinicalSummary = FALSE,
  copyReadySentences = FALSE,
  showAssumptionsCheck = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- rows:

  variable in the rows

- cols:

  variable in the columns

- counts:

  Frequency/weight variable for contingency table data. When specified,
  the data is treated as already summarized with counts per combination.

- posthoc:

  Method for p-value adjustment in post-hoc tests. IMPORTANT: Selecting
  'None' DISABLES all pairwise comparisons entirely (not just
  adjustment). Unadjusted pairwise testing is not supported to prevent
  inappropriate multiple testing. Choose Bonferroni (most conservative),
  Holm (less conservative), or FDR (controls false discovery rate) for
  pairwise testing.

- sig:

  alpha level for significance testing

- excl:

  exclude missing values from analysis

- exp:

  show expected values in the table

- plot:

  display plot of standardized residuals

- showResiduals:

  display standardized residuals analysis with interpretation

- showEducational:

  display educational guidance and explanations

- showDetailedTables:

  display individual 2x2 tables for each pairwise comparison

- residualsCutoff:

  critical value for identifying significant residuals (typically 2.0 or
  3.0)

- phiCI:

  calculate bootstrap confidence intervals for the Phi coefficient using
  BCa method (Bias-Corrected and accelerated). Note: This is
  computationally intensive and may take longer for large tables.

- testSelection:

  method for selecting statistical test for pairwise comparisons

- exportResults:

  Export comprehensive analysis results to downloadable format for
  further analysis

- showClinicalSummary:

  Display natural-language summary of results for clinical
  interpretation

- copyReadySentences:

  Generate copy-ready sentences for clinical reports

- showAssumptionsCheck:

  Display validation of chi-square test assumptions

- showGlossary:

  Display glossary of statistical terms with clinical interpretations

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$chisqTable`          |     |     |     |     | a table  |
| `results$assumptionsCheck`    |     |     |     |     | a html   |
| `results$clinicalSummary`     |     |     |     |     | a html   |
| `results$educationalOverview` |     |     |     |     | a html   |
| `results$weightedDataInfo`    |     |     |     |     | a html   |
| `results$contingencyTable`    |     |     |     |     | a html   |
| `results$residualsGuidance`   |     |     |     |     | a html   |
| `results$residualsAnalysis`   |     |     |     |     | a html   |
| `results$multipleTestingInfo` |     |     |     |     | a html   |
| `results$posthocTable`        |     |     |     |     | a table  |
| `results$detailedComparisons` |     |     |     |     | a html   |
| `results$exportTable`         |     |     |     |     | a table  |
| `results$reportSentences`     |     |     |     |     | a html   |
| `results$glossaryPanel`       |     |     |     |     | a html   |
| `results$plot`                |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$chisqTable$asDF`

`as.data.frame(results$chisqTable)`
