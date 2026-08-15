# IHC Diagnostic Analysis

Diagnostic performance analysis and clinical classification using IHC
markers. Designed for differential diagnosis and antibody panel
optimization.

## Usage

``` r
ihcdiagnostic(
  data,
  markers,
  diagnosis,
  id = NULL,
  differentialDiagnosis = TRUE,
  antibodyOptimization = FALSE,
  calculateDiagnosticMetrics = TRUE,
  clusterMethod = "hierarchical",
  cutpointMethod = "optimal",
  confidenceLevel = 0.95,
  crossValidation = TRUE,
  minimumGroupSize = 10
)
```

## Arguments

- data:

  the data as a data frame

- markers:

  Select IHC marker variables (e.g., ER, PR, HER2, Ki67, CD markers).
  These can be percentage scores, H-scores, or categorical results.

- diagnosis:

  Select the reference diagnosis variable (e.g., tumor type, grade,
  subtype). This is your gold standard for evaluating marker
  performance.

- id:

  Optional case identifier for tracking individual results in
  differential diagnosis. Useful for matching predictions with actual
  diagnoses.

- differentialDiagnosis:

  Predict diagnosis for each case based on IHC marker patterns. Uses
  clustering to classify cases into diagnostic groups.

- antibodyOptimization:

  Find the best combination of markers for diagnosis. Tests single
  markers and combinations to identify optimal panels.

- calculateDiagnosticMetrics:

  Calculate sensitivity, specificity, PPV, NPV, and AUC for each marker.
  Essential for evaluating marker diagnostic performance.

- clusterMethod:

  Clustering method for differential diagnosis

- cutpointMethod:

  Method for determining positive/negative cutpoints

- confidenceLevel:

  Confidence level for performance metrics (0.95 = 95 percent confidence
  intervals). Higher values give wider, more conservative intervals.

- crossValidation:

  Use 5-fold cross-validation for more reliable performance estimates.
  Recommended for clinical validation but takes longer to compute.

- minimumGroupSize:

  Minimum number of cases required per diagnostic group. Groups with
  fewer cases will trigger a warning (recommend \>=10).

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`          |     |     |     |     | a html   |
| `results$clinicalSummary`       |     |     |     |     | a html   |
| `results$interpretationGuide`   |     |     |     |     | a html   |
| `results$diagnosticPerformance` |     |     |     |     | a table  |
| `results$differentialResults`   |     |     |     |     | a table  |
| `results$confusionMatrix`       |     |     |     |     | a table  |
| `results$panelOptimization`     |     |     |     |     | a table  |
| `results$rocPlot`               |     |     |     |     | an image |
| `results$diagnosticPlot`        |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$diagnosticPerformance$asDF`

`as.data.frame(results$diagnosticPerformance)`

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`ihcdiagnosticBase` -\> `ihcdiagnosticClass`

## Methods

### Public methods

- [`ihcdiagnosticClass$clone()`](#method-ihcdiagnosticClass-clone)

Inherited methods

- [`jmvcore::Analysis$.createImage()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImage)
- [`jmvcore::Analysis$.createImages()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImages)
- [`jmvcore::Analysis$.createPlotObject()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createPlotObject)
- [`jmvcore::Analysis$.getSessionTemp()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.getSessionTemp)
- [`jmvcore::Analysis$.load()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.load)
- [`jmvcore::Analysis$.render()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.render)
- [`jmvcore::Analysis$.save()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.save)
- [`jmvcore::Analysis$.savePart()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.savePart)
- [`jmvcore::Analysis$.setCheckpoint()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setCheckpoint)
- [`jmvcore::Analysis$.setParent()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setParent)
- [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetHeaderSource)
- [`jmvcore::Analysis$.setReadDatasetSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetSource)
- [`jmvcore::Analysis$.setResourcesPathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setResourcesPathSource)
- [`jmvcore::Analysis$.setStatePathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setStatePathSource)
- [`jmvcore::Analysis$addAddon()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-addAddon)
- [`jmvcore::Analysis$asProtoBuf()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asProtoBuf)
- [`jmvcore::Analysis$asSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asSource)
- [`jmvcore::Analysis$check()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-check)
- [`jmvcore::Analysis$init()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-init)
- [`jmvcore::Analysis$optionsChangedHandler()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-optionsChangedHandler)
- [`jmvcore::Analysis$postInit()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-postInit)
- [`jmvcore::Analysis$print()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-print)
- [`jmvcore::Analysis$readDataset()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-readDataset)
- [`jmvcore::Analysis$run()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-run)
- [`jmvcore::Analysis$serialize()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-serialize)
- [`jmvcore::Analysis$setError()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setError)
- [`jmvcore::Analysis$setStatus()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setStatus)
- [`jmvcore::Analysis$translate()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-translate)
- `ihcdiagnosticBase$initialize()`

------------------------------------------------------------------------

### `ihcdiagnosticClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ihcdiagnosticClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
