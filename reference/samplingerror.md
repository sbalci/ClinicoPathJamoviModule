# Sampling Error & Efficiency Calculator

Practical tool for evaluating sampling adequacy and statistical power in
clinicopathological research.

## Usage

``` r
samplingerror(
  data,
  detectionSensitivity = 95,
  biologicalVarianceCV = 15,
  sampleSize = 10,
  eventFrequency = 5,
  referenceVolume = 100,
  sampleVolume = 10,
  calculationMode = "theoretical",
  sampleData = NULL,
  targetError = 10,
  showErrorComponents = TRUE,
  showOptimization = TRUE,
  showVisualization = TRUE,
  showMethodology = TRUE,
  showReferences = FALSE,
  confidenceLevel = 0.95
)
```

## Arguments

- data:

  .

- detectionSensitivity:

  Probability of correctly detecting/identifying an event (e.g., tumor
  cell). 100 percent means perfect detection, 95 percent means 5 percent
  false negative rate.

- biologicalVarianceCV:

  Coefficient of variation representing tissue heterogeneity. Low (5-10
  percent) = homogeneous tissue, Medium (15-25 percent) = moderate
  heterogeneity, High (\>30 percent) = highly heterogeneous tissue.

- sampleSize:

  Number of samples/sections/areas examined.

- eventFrequency:

  Proportion of sample containing the event of interest (e.g., percent
  of tissue with tumor, percent positive cells).

- referenceVolume:

  Total reference space being sampled (e.g., organ volume, total tissue
  area).

- sampleVolume:

  Volume/area of each sample examined (same units as reference volume).

- calculationMode:

  Theoretical: Calculate error from theoretical parameters. Empirical:
  Estimate error components from actual data.

- sampleData:

  Actual measurements from samples (for empirical calculation).

- targetError:

  Desired maximum total sampling error. Used for sample size
  recommendations.

- showErrorComponents:

  Display breakdown of the three error components: E(Ne), E(B(n)),
  E(Ne/sv).

- showOptimization:

  Calculate optimal sample sizes for different target error rates.

- showVisualization:

  .

- showMethodology:

  .

- showReferences:

  .

- confidenceLevel:

  .

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$errorSummary`    |     |     |     |     | a table  |
| `results$errorComponents` |     |     |     |     | a table  |
| `results$optimization`    |     |     |     |     | a table  |
| `results$plot`            |     |     |     |     | an image |
| `results$methodology`     |     |     |     |     | a html   |
| `results$references`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$errorSummary$asDF`

`as.data.frame(results$errorSummary)`

## Details

Calculates sampling efficiency and error rates according to Kayser
(2009).
