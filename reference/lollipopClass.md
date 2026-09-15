# Lollipop Charts for Categorical Data Visualization

Creates comprehensive lollipop charts for categorical data visualization
with emphasis on clinical applications. Lollipop charts are particularly
effective for displaying categorical data with a focus on individual
values, making them ideal for patient timelines, treatment outcomes,
biomarker levels, and comparative clinical assessments.

## Value

An `R6` class generator object for the `lollipopClass` backend; used
internally by the jamovi analysis wrapper and not called directly.

## Details

The lollipop chart function is designed specifically for clinical
research applications where categorical data visualization with emphasis
on individual values is crucial. Unlike bar charts, lollipop charts
reduce ink-to-data ratio and provide cleaner visualization for sparse
data or when highlighting specific categories.

Key features:

- Flexible orientation (vertical/horizontal)

- Advanced sorting options (by value, alphabetical)

- Clinical color schemes and themes

- Highlighting capabilities for specific categories

- Statistical summary integration

- Professional publication-ready appearance

Common clinical applications:

- Patient timeline visualization

- Biomarker level comparisons

- Treatment outcome rankings

- Survey response visualization

- Quality metric displays

- Diagnostic test results

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`lollipopBase` -\> `lollipopClass`

## Methods

### Public methods

- [`lollipopClass$asSource()`](#method-lollipopClass-asSource)

- [`lollipopClass$clone()`](#method-lollipopClass-clone)

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
- `lollipopBase$initialize()`

------------------------------------------------------------------------

### `lollipopClass$asSource()`

Generate R source code for Lollipop Plot analysis

#### Usage

    lollipopClass$asSource()

#### Returns

Character string with R syntax for reproducible analysis

------------------------------------------------------------------------

### `lollipopClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    lollipopClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic lollipop chart
result <- lollipop(
  data = patient_data,
  dep = "biomarker_level",
  group = "patient_id"
)

# Horizontal lollipop with sorting
result <- lollipop(
  data = treatment_data,
  dep = "response_score",
  group = "treatment_type",
  sortBy = "value_desc",
  orientation = "horizontal",
  showValues = TRUE
)

# Clinical timeline with highlighting
result <- lollipop(
  data = timeline_data,
  dep = "days_to_event",
  group = "patient_id",
  highlight = "high_risk_patient",
  showMean = TRUE
)
} # }
```
