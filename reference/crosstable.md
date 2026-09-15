# Cross Tables

Function for making Cross Tables with multiple table styles.

## Usage

``` r
crosstable(
  data,
  vars = NULL,
  group = NULL,
  sty = "nejm",
  excl = FALSE,
  cont = "mean",
  pcat = "chisq",
  p_adjust = "none",
  showSMD = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  The variable(s) that will appear as rows in the cross table.

- group:

  The variable that will appear as columns (groups) in the table.

- sty:

  .

- excl:

  Exclude rows with missing values.

- cont:

  .

- pcat:

  .

- p_adjust:

  Method for adjusting p-values for multiple comparisons across
  variables. Only available with gtsummary table style.

- showSMD:

  Add a standardized mean difference (SMD) column comparing the groups
  for each variable — the standard balance diagnostic for matched,
  weighted, or propensity cohorts. Requires exactly two groups. \|SMD\|
  \< 0.1 conventionally indicates negligible imbalance.

## Value

A results object containing:

|                             |     |     |     |     |                |
|-----------------------------|-----|-----|-----|-----|----------------|
| `results$errorNotice`       |     |     |     |     | a html         |
| `results$dataQualityNotice` |     |     |     |     | a html         |
| `results$analysisInfo`      |     |     |     |     | a html         |
| `results$subtitle`          |     |     |     |     | a preformatted |
| `results$todo`              |     |     |     |     | a html         |
| `results$todo2`             |     |     |     |     | a html         |
| `results$varNameWarnings`   |     |     |     |     | a html         |
| `results$tablestyle1`       |     |     |     |     | a html         |
| `results$tablestyle2`       |     |     |     |     | a html         |
| `results$tablestyle3`       |     |     |     |     | a html         |
| `results$tablestyle4`       |     |     |     |     | a html         |
| `results$qvalueExplanation` |     |     |     |     | a html         |
| `results$testInformation`   |     |     |     |     | a html         |
| `results$smdTable`          |     |     |     |     | a table        |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$smdTable$asDF`

`as.data.frame(results$smdTable)`

## Details

Currently implemented features:

- Multiple table styles (arsenal, finalfit, gtsummary, NEJM, Lancet,
  hmisc)

- Automatic test selection (chi-square, Fisher's exact, t-test, ANOVA)

- Multiple testing correction (Bonferroni, Holm, Benjamini-Hochberg,
  Benjamini-Yekutieli)

- Variable name safety (special characters, spaces)

- Data quality validation warnings

Note: Advanced features (pairwise comparisons, effect sizes, residual
analysis, correspondence analysis) are planned but not yet available.
