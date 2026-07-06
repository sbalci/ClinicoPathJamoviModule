# Survival Feature Ranking - Univariate Cox Screening

Performs univariate survival analysis for multiple features to identify
potential prognostic factors. This analysis runs a separate Cox
proportional hazards model for each selected feature and ranks them by
statistical significance, hazard ratio, or concordance index. Inspired
by Orange Data Mining's Rank Survival Features widget, adapted for
jamovi with comprehensive statistical reporting.

## Details

The function performs the following analyses for each feature:

- Univariate Cox proportional hazards regression

- Hazard ratio with 95% confidence intervals

- Wald test p-value for association with survival

- Concordance index (C-index) for discriminative ability

- Optional multiple testing correction (FDR, Bonferroni, etc.)

Features can be ranked by:

- P-value: Most statistically significant features first

- Hazard ratio: Features with largest effect size (furthest from 1)

- C-index: Features with best discriminative ability

## Use Cases

- Biomarker screening: Test many candidate biomarkers for prognostic
  value

- Exploratory analysis: Identify promising variables before
  multivariable modeling

- Feature selection: Prioritize variables for inclusion in complex
  models

- Publication tables: Generate univariate analysis tables for
  manuscripts

## Interpretation

- HR \> 1: Feature associated with higher hazard (worse outcome)

- HR \< 1: Feature associated with lower hazard (better outcome)

- C-index \> 0.7: Good discrimination, \> 0.8: Excellent

- Adjusted p-values: Control false discovery rate when testing multiple
  features

## References

Orange Data Mining:
https://orangedatamining.com/workflows/Survival-Analysis/

## Author

ClinicoPath Development Team

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`survivalfeaturerankBase` -\> `survivalfeaturerankClass`

## Methods

### Public methods

- [`survivalfeaturerankClass$clone()`](#method-survivalfeaturerankClass-clone)

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
- `survivalfeaturerankBase$initialize()`

------------------------------------------------------------------------

### `survivalfeaturerankClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    survivalfeaturerankClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Screen multiple biomarkers
survivalfeaturerank(
  data = cancer_data,
  survtime = "survival_months",
  event = "death",
  eventLevel = "1",
  features = c("age", "stage", "grade", "ki67", "p53", "her2"),
  rankBy = "pvalue",
  adjustPValues = TRUE,
  showTopKM = TRUE,
  topN = 3
)
} # }
```
