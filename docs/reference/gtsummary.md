# Publication-Ready Tables with gtsummary

Publication-Ready Tables with gtsummary

## Usage

``` r
gtsummary(
  data,
  vars,
  byvar = NULL,
  tableType = "summary",
  statistics = list("mean_sd", "median_iqr", "n_percent"),
  includeOverall = TRUE,
  includeMissing = "ifany",
  addPValue = FALSE,
  testMethod = "auto",
  pairedTest = FALSE,
  addQ = FALSE,
  boldLabels = TRUE,
  boldLevels = FALSE,
  boldPValues = FALSE,
  pValueThreshold = 0.05,
  italicizeLabels = FALSE,
  italicizeLevels = FALSE,
  addSpanningHeader = FALSE,
  spanningHeaderText = "",
  sortVariables = "original",
  showNHeader = TRUE,
  percentType = "column",
  digitsOverall = 1,
  digitsByGroup = 1,
  digitsPValue = 3,
  outputFormat = "html",
  tableTitle = "",
  tableCaption = "",
  footnote = "",
  exportTable = FALSE,
  showCode = FALSE
)
```

## Arguments

- data:

  .

- vars:

  .

- byvar:

  .

- tableType:

  .

- statistics:

  .

- includeOverall:

  .

- includeMissing:

  .

- addPValue:

  .

- testMethod:

  .

- pairedTest:

  .

- addQ:

  .

- boldLabels:

  .

- boldLevels:

  .

- boldPValues:

  .

- pValueThreshold:

  .

- italicizeLabels:

  .

- italicizeLevels:

  .

- addSpanningHeader:

  .

- spanningHeaderText:

  .

- sortVariables:

  .

- showNHeader:

  .

- percentType:

  .

- digitsOverall:

  .

- digitsByGroup:

  .

- digitsPValue:

  .

- outputFormat:

  .

- tableTitle:

  .

- tableCaption:

  .

- footnote:

  .

- exportTable:

  .

- showCode:

  .

## Value

A results object containing:

|                         |     |     |     |     |                |
|-------------------------|-----|-----|-----|-----|----------------|
| `results$todo`          |     |     |     |     | a html         |
| `results$maintable`     |     |     |     |     | a html         |
| `results$stats_summary` |     |     |     |     | a html         |
| `results$table_notes`   |     |     |     |     | a html         |
| `results$export_info`   |     |     |     |     | a html         |
| `results$code_output`   |     |     |     |     | a preformatted |
