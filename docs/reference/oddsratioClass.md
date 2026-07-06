# Odds Ratio Analysis for Binary Outcomes

Performs logistic regression analysis to calculate odds ratios for
binary outcomes. This function provides comprehensive odds ratio tables,
forest plots, and optional nomogram generation for clinical prediction.
It supports both categorical and continuous explanatory variables and
includes diagnostic metrics for binary predictors.

## Details

The function performs the following analyses:

- Logistic regression using finalfit package

- Odds ratio calculation with 95% confidence intervals

- Forest plot generation for visualization

- Optional nomogram creation for clinical prediction

- Likelihood ratio calculations for diagnostic metrics

- Sensitivity and specificity analysis for binary predictors

The function automatically cleans variable names using
janitor::clean_names() and preserves original variable labels for
display. It handles missing data through complete case analysis.

## International Usage

For international users, the function includes an outcomeLevel parameter
to explicitly specify which outcome level represents the positive case.
This is important for correct interpretation of likelihood ratios and
diagnostic metrics.

## Nomogram Features

When showNomogram is enabled, the function generates:

- Interactive nomogram for risk prediction

- Diagnostic metrics (sensitivity, specificity, likelihood ratios)

- Contingency table analysis

- User guidance for interpretation

## References

Harrison, E., Drake, T., & Ots, R. (2019). finalfit: Quickly create
elegant regression results tables and plots when modelling. R package
version 0.9.7.

## See also

[`finalfit`](https://rdrr.io/pkg/finalfit/man/finalfit.html),
[`rms`](https://rdrr.io/pkg/rms/man/rms.html)

## Author

ClinicoPath Development Team

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`oddsratioBase` -\> `oddsratioClass`

## Methods

### Public methods

- [`oddsratioClass$clone()`](#method-oddsratioClass-clone)

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
- `oddsratioBase$initialize()`

------------------------------------------------------------------------

### `oddsratioClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    oddsratioClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic odds ratio analysis
result <- oddsratio(
  data = clinical_data,
  explanatory = c("age", "gender", "smoking"),
  outcome = "mortality"
)

# With nomogram and specified outcome level
result <- oddsratio(
  data = clinical_data,
  explanatory = c("age", "treatment"),
  outcome = "recurrence",
  outcomeLevel = "Yes",
  showNomogram = TRUE
)
} # }
```
