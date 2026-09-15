# Violin Plots to Compare Within Group (Repeated Measures)

Creates violin plots for within-subjects (repeated measures) analysis
using ggstatsplot::ggwithinstats. Compares 2-4 measurements from the
same subjects with statistical testing and pairwise comparisons. Ideal
for biomarker tracking, treatment response monitoring, and longitudinal
clinical studies.

## Value

A jamovi analysis object with violin plots, statistical tests, and
clinical interpretation

## Details

**Data Requirements:**

- Wide format required (one row per subject)

- Each column represents a different time point or condition

- Complete data required for paired analysis (listwise deletion)

- Minimum 3 subjects with complete data across all measurements

**Statistical Tests:**

- Parametric: Repeated measures ANOVA (assumes normality)

- Nonparametric: Friedman test (no distribution assumptions)

- Robust: Uses trimmed means (resistant to outliers)

- Bayesian: Provides evidence strength via Bayes Factors

**Clinical Presets:**

- Biomarker: Optimized for laboratory biomarker tracking (nonparametric)

- Treatment: Optimized for treatment response monitoring (parametric
  with pairwise)

- Laboratory: Optimized for clinical lab values (robust)

## Performance Optimization

The function implements sophisticated caching:

- Data preparation cached based on variable selection and data content

- Options cached separately to minimize reprocessing

- Plot state management prevents unnecessary regeneration

- Checkpoint calls before expensive operations for responsiveness

## Clinical Validation

The function performs comprehensive data quality checks:

- Validates paired design requirements (complete cases)

- Detects small sample sizes (\< 10 subjects)

- Identifies potential outliers (\> 10% outliers)

- Warns about skewed data for parametric tests

- Alerts to high missing data rates (\> 50%)

## References

Patil, I. (2021). Visualizations with statistical details: The
'ggstatsplot' approach. Journal of Open Source Software, 6(61), 3167.
[doi:10.21105/joss.03167](https://doi.org/10.21105/joss.03167)

## See also

[`ggwithinstats`](https://www.indrapatil.com/ggstatsplot/reference/ggwithinstats.html)
for the underlying plotting function

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`jjwithinstatsBase` -\> `jjwithinstatsClass`

## Methods

### Public methods

- [`jjwithinstatsClass$asSource()`](#method-jjwithinstatsClass-asSource)

- [`jjwithinstatsClass$clone()`](#method-jjwithinstatsClass-clone)

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
- `jjwithinstatsBase$initialize()`

------------------------------------------------------------------------

### `jjwithinstatsClass$asSource()`

Generate R source code for Within-Groups Statistics analysis

#### Usage

    jjwithinstatsClass$asSource()

#### Returns

Character string with R syntax for reproducible analysis

------------------------------------------------------------------------

### `jjwithinstatsClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    jjwithinstatsClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic within-subjects analysis
data(iris)
iris_wide <- data.frame(
    Subject = 1:50,
    Baseline = iris$Sepal.Length[1:50],
    Month3 = iris$Sepal.Width[1:50] * 2.5,
    Month6 = iris$Petal.Length[1:50] * 1.8
)

jjwithinstats(
    data = iris_wide,
    dep1 = "Baseline",
    dep2 = "Month3",
    dep3 = "Month6",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    centralityplotting = TRUE,
    pointpath = TRUE
)

# Clinical biomarker tracking (nonparametric)
jjwithinstats(
    data = clinical_data,
    dep1 = "CRP_baseline",
    dep2 = "CRP_week4",
    dep3 = "CRP_week12",
    clinicalpreset = "biomarker",
    typestatistics = "nonparametric",
    pairwisecomparisons = TRUE,
    mytitle = "C-Reactive Protein Levels During Treatment"
)

# Robust analysis with publication plot
jjwithinstats(
    data = lab_data,
    dep1 = "ALT_pre",
    dep2 = "ALT_post",
    typestatistics = "robust",
    centralityplotting = TRUE,
    centralitytype = "robust",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    ggpubrAddStats = TRUE
)
} # }
```
