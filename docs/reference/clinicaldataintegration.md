# Clinical Data Integration

Clinical Data Integration

## Usage

``` r
clinicaldataintegration(
  data,
  dataSource = "csv",
  patientIdVar,
  dateVars,
  clinicalVars,
  qualityCheck = TRUE,
  completenessThreshold = 80,
  consistencyCheck = TRUE,
  outlierDetection = TRUE,
  terminologyMapping = "none",
  exportFormat = "csv",
  generateReport = TRUE
)
```

## Arguments

- data:

  .

- dataSource:

  .

- patientIdVar:

  .

- dateVars:

  .

- clinicalVars:

  .

- qualityCheck:

  .

- completenessThreshold:

  .

- consistencyCheck:

  .

- outlierDetection:

  .

- terminologyMapping:

  .

- exportFormat:

  .

- generateReport:

  .

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$overview`           |     |     |     |     | a table  |
| `results$qualityAssessment`  |     |     |     |     | a table  |
| `results$consistencyChecks`  |     |     |     |     | a table  |
| `results$terminologyMapping` |     |     |     |     | a table  |
| `results$exportSummary`      |     |     |     |     | a html   |
| `results$qualityPlot`        |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
