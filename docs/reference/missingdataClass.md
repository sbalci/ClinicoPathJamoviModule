# Missing Data Analysis and Multiple Imputation

Comprehensive missing data analysis and multiple imputation using mice
and ggmice packages. This function provides a complete workflow for
analyzing missing data patterns, performing multiple imputation by
chained equations (MICE), and evaluating imputation quality. Designed
specifically for clinical research applications where missing data is
common and proper handling is critical for valid statistical inference.

## Value

An `R6` class generator object for the `missingdataClass` backend; used
internally by the jamovi analysis wrapper and not called directly.

## Details

The missing data analysis function provides three main analysis types:

1.  **Pattern Analysis**: Explores missing data structure and patterns

2.  **Multiple Imputation**: Performs MICE imputation with convergence
    diagnostics

3.  **Complete Analysis**: Combines pattern analysis and imputation

Key features include:

- Visual and tabular missing data pattern analysis

- Multiple imputation methods (PMM, Bayesian regression, logistic
  regression)

- Convergence diagnostics with trace plots

- Quality evaluation comparing observed vs imputed data

- Flexible parameter customization

- Clinical research focused interpretations

Common clinical applications:

- Data quality assessment for clinical trials

- Missing data handling in observational studies

- Regulatory compliance for pharmaceutical research

- Sensitivity analysis for missing data assumptions

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`missingdataBase` -\> `missingdataClass`

## Methods

### Public methods

- [`missingdataClass$clone()`](#method-missingdataClass-clone)

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
- `missingdataBase$initialize()`

------------------------------------------------------------------------

### `missingdataClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    missingdataClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic pattern analysis
result <- missingdata(
  data = clinical_data,
  analysis_vars = c("age", "bmi", "biomarker"),
  analysis_type = "pattern"
)

# Multiple imputation
result <- missingdata(
  data = clinical_data,
  analysis_vars = c("age", "bmi", "biomarker"),
  analysis_type = "imputation",
  n_imputations = 10,
  imputation_method = "pmm"
)

# Complete analysis
result <- missingdata(
  data = clinical_data,
  analysis_vars = c("age", "bmi", "biomarker"),
  analysis_type = "complete",
  n_imputations = 5,
  max_iterations = 10
)
} # }
```
