# Clinical Heatmap with tidyheatmaps

Advanced heatmap visualization for clinical and biomedical data using
the tidyheatmaps package. This function creates publication-ready
heatmaps from tidy data with support for clinical annotations, multiple
scaling methods, and comprehensive customization options. Perfect for
visualizing biomarker expression patterns, treatment responses, genomic
data, and quality control metrics.

## Value

A jamovi analysis object containing the clinical heatmap and supporting
information

## Details

The Clinical Heatmap module leverages the powerful tidyheatmaps package
to create sophisticated visualizations of multivariate clinical data. It
supports various data types and use cases:

**Supported Data Types:**

- **Biomarker Expression:** IHC scores, molecular markers, protein
  levels

- **Genomic Data:** Gene expression, mutation status, copy number
  variations

- **Clinical Metrics:** Laboratory values, vital signs, diagnostic
  scores

- **Quality Control:** Batch effects, instrument performance,
  reproducibility

- **Treatment Response:** Longitudinal measurements, dose-response
  relationships

**Key Features:**

- **Tidy Data Integration:** Works directly with long-format clinical
  datasets

- **Clinical Annotations:** Row and column annotations for
  patient/biomarker characteristics

- **Flexible Scaling:** Row, column, or bidirectional Z-score
  normalization

- **Hierarchical Clustering:** Automatic pattern discovery with
  customizable clustering

- **Color Palettes:** Clinical-friendly color schemes including
  colorblind-safe options

- **Missing Data Handling:** Multiple strategies for incomplete clinical
  datasets

- **Export Options:** Publication-ready outputs with customizable
  dimensions

## Clinical Applications

- **Biomarker Profiling:** Visualize multi-marker expression panels
  across patient cohorts

- **Precision Medicine:** Display genomic alterations and therapeutic
  targets

- **Quality Assurance:** Monitor laboratory performance and batch
  effects

- **Clinical Trials:** Show treatment response patterns and dose
  relationships

- **Diagnostic Patterns:** Reveal disease subtypes and molecular
  classifications

## Data Format Requirements

The function expects data in "tidy" (long) format with three essential
variables:

- **Row Variable:** Defines heatmap rows (e.g., patient IDs, gene names)

- **Column Variable:** Defines heatmap columns (e.g., biomarkers, time
  points)

- **Value Variable:** Numeric values to visualize (e.g., expression
  levels, scores)

## Scaling Methods

- **None:** Raw values displayed without transformation

- **Row:** Z-score normalization within each row (standardizes across
  columns)

- **Column:** Z-score normalization within each column (standardizes
  across rows)

- **Both:** Global Z-score normalization across entire dataset

## Missing Data Strategies

- **Exclude:** Remove rows/columns with missing values (complete cases
  only)

- **Mean:** Replace missing values with column/row means

- **Median:** Replace missing values with column/row medians (robust to
  outliers)

- **Zero:** Replace missing values with zero (appropriate for some
  clinical scales)

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`clinicalheatmapBase` -\> `clinicalheatmapClass`

## Methods

### Public methods

- [`clinicalheatmapClass$clone()`](#method-clinicalheatmapClass-clone)

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
- `clinicalheatmapBase$initialize()`

------------------------------------------------------------------------

### `clinicalheatmapClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    clinicalheatmapClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# TODO: Add proper examples with working datasets
# Currently commented to prevent installation errors
# Need to use existing datasets from ClinicoPathDescriptives

# # Example 1: Basic biomarker heatmap
# data(histopathology)
#
# # Create sample biomarker data in long format
# biomarker_data <- data.frame(
#   patient_id = rep(paste0("P", 1:50), each = 4),
#   biomarker = rep(c("ER", "PR", "HER2", "Ki67"), 50),
#   expression = rnorm(200, mean = 50, sd = 20)
# )
#
# clinicalheatmap(
#   data = biomarker_data,
#   rowVar = "patient_id",
#   colVar = "biomarker",
#   valueVar = "expression",
#   scaleMethod = "row",
#   colorPalette = "RdBu"
# )
#
# # Example 2: Gene expression with clinical annotations
# gene_data <- expand.grid(
#   sample_id = paste0("S", 1:30),
#   gene = paste0("Gene", 1:20)
# )
# gene_data$expression <- rnorm(nrow(gene_data), mean = 0, sd = 1)
# gene_data$tumor_type <- rep(c("TypeA", "TypeB", "TypeC"), length.out = 30)
#
# clinicalheatmap(
#   data = gene_data,
#   rowVar = "sample_id",
#   colVar = "gene",
#   valueVar = "expression",
#   annotationCols = "tumor_type",
#   scaleMethod = "column",
#   clusterRows = TRUE,
#   clusterCols = TRUE
# )
} # }
```
