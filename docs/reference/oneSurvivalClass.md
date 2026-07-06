# Kaplan-Meier Survival Analysis for Single Group

Performs Kaplan-Meier survival analysis for a single group of subjects.
This function estimates survival probabilities over time and provides
summary statistics including median survival time with confidence
intervals. It generates both tabular results and Kaplan-Meier survival
plots.

## Details

The function performs the following analyses:

- Kaplan-Meier survival estimation using survival package

- Calculation of median survival time with 95% confidence intervals

- Summary statistics (number of subjects, number of events)

- Kaplan-Meier survival curve visualization

- Optional confidence interval bands on plots

The function requires two variables:

- Time variable: Time to event or censoring (continuous, positive
  values)

- Status variable: Event indicator (0 = censored, 1 = event occurred)

## Data Requirements

For valid survival analysis, ensure:

- Time values are non-negative and numeric

- Status values are binary (0/1 or equivalent)

- At least some events have occurred (not all censored)

- Sufficient sample size for reliable estimates

## References

Kaplan, E. L., & Meier, P. (1958). Nonparametric estimation from
incomplete observations. Journal of the American Statistical
Association, 53(282), 457-481.

## See also

[`survival`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/survival.md),
[`survfit`](https://rdrr.io/pkg/survival/man/survfit.html)

## Author

ClinicoPath Development Team

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`oneSurvivalBase` -\> `oneSurvivalClass`

## Methods

### Public methods

- [`oneSurvivalClass$clone()`](#method-oneSurvivalClass-clone)

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
- `oneSurvivalBase$initialize()`

------------------------------------------------------------------------

### `oneSurvivalClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    oneSurvivalClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic survival analysis
result <- oneSurvival(
  data = lung_data,
  times = "survival_time",
  status = "death_indicator"
)

# With confidence intervals and time units
result <- oneSurvival(
  data = clinical_data,
  times = "days_to_event",
  status = "event_occurred",
  ciyn = TRUE,
  timeunits = "Days"
)
} # }
```
