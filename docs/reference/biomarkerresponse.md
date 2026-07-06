# Biomarker Response Association

Biomarker Response Association

## Usage

``` r
biomarkerresponse(
  data,
  biomarker,
  response,
  responseType = "categorical",
  positiveLevel,
  plotType = "boxplot",
  showThreshold = FALSE,
  thresholdValue = "",
  thresholdMethod = "median",
  addTrendLine = FALSE,
  trendMethod = "loess",
  performTests = FALSE,
  groupVariable = NULL,
  showCorrelation = FALSE,
  logTransform = FALSE,
  outlierHandling = "highlight",
  confidenceLevel = "0.95"
)
```

## Arguments

- data:

  The data as a data frame.

- biomarker:

  Continuous biomarker measurement (e.g., expression level,
  concentration).

- response:

  Treatment response variable (binary, categorical, or continuous).

- responseType:

  Type of response variable for appropriate analysis and visualization.

- positiveLevel:

  Specify which level represents positive response (e.g., 'Responder',
  'Yes', '1'). If blank, uses second level alphabetically.

- plotType:

  Primary visualization method for biomarker-response relationship.

- showThreshold:

  Display threshold lines for biomarker positivity/negativity.

- thresholdValue:

  Threshold value for biomarker positivity (if known).

- thresholdMethod:

  Method for determining biomarker threshold.

- addTrendLine:

  Add fitted trend line for continuous responses.

- trendMethod:

  Method for fitting trend line.

- performTests:

  Perform appropriate statistical tests for biomarker-response
  association.

- groupVariable:

  Optional grouping variable (e.g., treatment arm, disease stage).

- showCorrelation:

  Display correlation coefficients and tests.

- logTransform:

  Apply log transformation to biomarker values.

- outlierHandling:

  Method for handling outlier values.

- confidenceLevel:

  Confidence level for intervals and tests.

## Value

A results object containing:

|                              |     |     |     |     |                |
|------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`            |     |     |     |     | a preformatted |
| `results$todo`               |     |     |     |     | a html         |
| `results$dataWarning`        |     |     |     |     | a html         |
| `results$plot`               |     |     |     |     | an image       |
| `results$correlation`        |     |     |     |     | a table        |
| `results$threshold`          |     |     |     |     | a table        |
| `results$groupComparison`    |     |     |     |     | a table        |
| `results$statisticalTests`   |     |     |     |     | a table        |
| `results$postHocTests`       |     |     |     |     | a table        |
| `results$stratifiedAnalysis` |     |     |     |     | a table        |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$correlation$asDF`

`as.data.frame(results$correlation)`
