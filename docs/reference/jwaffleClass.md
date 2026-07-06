# Waffle Charts for Categorical Data Visualization

Creates professional waffle charts (square pie charts) to visualize
categorical distributions using a grid of colored squares. Each square
represents a fixed proportion of the total, making it ideal for showing
parts-of-whole relationships in clinical and pathological data.

## Value

A jamovi analysis object with waffle chart visualization, optional
summary, and optional methodology explanation

## Details

**Data Requirements:**

- Categorical grouping variable (factor, character, or logical)

- Minimum 30 cases recommended for stable proportions

- 2-10 categories optimal for visual clarity (\>10 categories may be
  cluttered)

- Haven labelled data automatically converted to factors

**How Waffle Charts Work:** A waffle chart uses a grid of colored
squares (typically 10x10 = 100 squares) where each square represents a
fixed proportion of the total sample. This makes percentages immediately
intuitive - each square ~= 1% of the sample.

**Clinical Applications:**

- **Disease Classification:** Show distribution of tumor grades, cancer
  stages, or pathological subtypes

- **Treatment Outcomes:** Display response rates (complete/partial/no
  response) across patient cohorts

- **Demographic Analysis:** Present patient characteristics, risk
  factors, or comorbidity patterns

- **Quality Metrics:** Visualize compliance rates, diagnostic accuracy,
  or safety outcomes

**Advantages over Other Charts:**

- More intuitive than pie charts for showing proportions

- Each square = 1% makes percentages immediately clear

- Handles many categories better than bar charts

- Effective for presentations and publications

- Faceting enables subgroup comparisons

**Statistical Considerations:**

- Most effective with n\>=30; smaller samples may show unstable
  proportions

- Works best when no single category dominates (\>80%)

- Each square represents approximately 1% of the sample

- Categories with \<5 cases may be statistically unreliable

- Chi-square tests can evaluate proportion differences between groups

## Performance Optimization

The function implements sophisticated caching:

- Data aggregation cached based on variable selection and data content

- Color palettes cached separately to minimize recomputation

- Plot state management prevents unnecessary regeneration

- Automatic width scaling for faceted plots

## Clinical Validation

The function performs comprehensive data quality checks:

- Validates categorical data types (with automatic labelled data
  conversion)

- Detects single-category data (requires multiple categories)

- Warns about many categories (\>10 may need grouping)

- Alerts to small samples (\<30 cases)

- Identifies rare categories (\<5 cases)

- Checks for negative count values (must be non-negative)

## References

Wilke, C. O. (2019). waffle: Create Waffle Chart Visualizations in R. R
package version 1.0.1.

## See also

[`geom_waffle`](https://rdrr.io/pkg/waffle/man/geom_waffle.html) for the
underlying waffle geom

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`jwaffleBase` -\> `jwaffleClass`

## Methods

### Public methods

- [`jwaffleClass$asSource()`](#method-jwaffleClass-asSource)

- [`jwaffleClass$clone()`](#method-jwaffleClass-clone)

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
- `jwaffleBase$initialize()`

------------------------------------------------------------------------

### `jwaffleClass$asSource()`

Generate R source code for jwaffle analysis

#### Usage

    jwaffleClass$asSource()

#### Returns

Character string with R syntax for reproducible analysis

------------------------------------------------------------------------

### `jwaffleClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    jwaffleClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic tumor grade distribution
jwaffle(
    data = pathology_data,
    groups = "TumorGrade",
    color_palette = "colorblind",
    show_legend = TRUE,
    showSummaries = TRUE
)

# Treatment response by cohort with faceting
jwaffle(
    data = clinical_data,
    groups = "Response",
    facet = "TreatmentArm",
    color_palette = "professional",
    mytitle = "Treatment Response Rates by Arm",
    showSummaries = TRUE
)

# Weighted risk distribution
jwaffle(
    data = aggregated_data,
    groups = "RiskCategory",
    counts = "PatientCount",
    color_palette = "presentation",
    legendtitle = "Risk Level"
)
} # }
```
