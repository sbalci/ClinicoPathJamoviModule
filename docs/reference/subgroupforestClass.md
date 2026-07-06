# Subgroup Analysis Forest Plot

Creates forest plots showing treatment effects across different patient
subgroups. This function performs subgroup analysis for clinical trials
and observational studies, calculating treatment effects within patient
subgroups and testing for interactions. Supports survival
(time-to-event), binary, and continuous outcomes.

**Clinical Applications:**

- Identify patient subgroups with differential treatment benefit

- Test for treatment-by-subgroup interactions

- Guide personalized treatment decisions

- Explore heterogeneity in treatment effects

**Statistical Methods:**

- Survival outcomes: Cox proportional hazards models (Hazard Ratios)

- Binary outcomes: Logistic regression (Odds Ratios, Risk Ratios)

- Continuous outcomes: Linear regression (Mean Differences)

- Interaction testing: Likelihood ratio tests

## Value

A results object containing forest plot, summary tables, and interaction
tests

## Details

**Effect Measures by Outcome Type:**

*Survival Outcomes:*

- Hazard Ratio (HR): Compares hazard rates between treatment groups

- HR \> 1: Increased hazard (worse outcome) with treatment

- HR \< 1: Decreased hazard (better outcome) with treatment

*Binary Outcomes:*

- Odds Ratio (OR): Compares odds of outcome between groups

- Risk Ratio (RR): Compares probability of outcome between groups

- OR/RR \> 1: Higher risk with treatment

- OR/RR \< 1: Lower risk with treatment

*Continuous Outcomes:*

- Mean Difference (MD): Difference in means between groups

- MD \> 0: Higher values with treatment

- MD \< 0: Lower values with treatment

**Interaction Testing:** Tests whether treatment effect varies
significantly across subgroups using likelihood ratio tests comparing
models with and without interaction terms.

**Sample Size Requirements:**

- Minimum 5 patients per subgroup for analysis

- Larger samples recommended for stable estimates

- Consider multiple comparison adjustments for many subgroups

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`subgroupforestBase` -\> `subgroupforestClass`

## Methods

### Public methods

- [`subgroupforestClass$clone()`](#method-subgroupforestClass-clone)

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
- `subgroupforestBase$initialize()`

------------------------------------------------------------------------

### `subgroupforestClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    subgroupforestClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
# Survival outcome subgroup analysis
subgroupforest(
  data = clinical_trial,
  outcome = "time_to_event",
  treatment = "treatment_arm", 
  subgroups = c("age_group", "gender", "stage"),
  time = "time_to_event",
  event = "event_occurred",
  outcomeType = "survival",
  effectMeasure = "hr"
)
#> Error: object 'clinical_trial' not found

# Binary outcome analysis
subgroupforest(
  data = study_data,
  outcome = "response",
  treatment = "intervention",
  subgroups = c("age_category", "sex"),
  outcomeType = "binary",
  effectMeasure = "or"
)
#> Error: object 'study_data' not found
# }
```
