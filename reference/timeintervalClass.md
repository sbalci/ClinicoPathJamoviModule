# Comprehensive Time Interval Calculator for Survival Analysis

Advanced time interval calculation tool designed for survival analysis,
epidemiological studies, and person-time analysis. Provides robust date
parsing, time interval calculation, landmark analysis, and comprehensive
data quality assessment.

## Value

List containing calculated intervals, filtered data, quality metrics,
and metadata

## Details

This function provides comprehensive time interval calculation
capabilities including:

- Multiple date format parsing with automatic detection

- Flexible output units (days, weeks, months, years)

- Landmark analysis for conditional survival

- Person-time calculations for epidemiological studies

- Data quality assessment and validation

- Statistical summaries with confidence intervals

- Export capabilities for downstream analysis

## Note

**Landmark Analysis Exclusion:** When landmark analysis is enabled,
participants with missing follow-up times (NA values) are implicitly
excluded from the "at-risk" cohort because their eligibility for
landmark criteria cannot be determined.

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`timeintervalBase` -\> `timeintervalClass`

## Methods

### Public methods

- [`timeintervalClass$clone()`](#method-timeintervalClass-clone)

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
- `timeintervalBase$initialize()`

------------------------------------------------------------------------

### `timeintervalClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    timeintervalClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic time interval calculation:
timeinterval(
  data = study_data,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  time_format = "ymd",
  output_unit = "months"
)

# With landmark analysis:
timeinterval(
  data = study_data,
  dx_date = "start_date",
  fu_date = "end_date",
  use_landmark = TRUE,
  landmark_time = 6,
  output_unit = "months"
)
} # }
```
