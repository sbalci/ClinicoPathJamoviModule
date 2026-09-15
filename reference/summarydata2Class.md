# Enhanced Summary Statistics for Continuous and Date Variables

This function provides comprehensive descriptive statistics for
continuous and date variables with multiple output formats and enhanced
visualization capabilities. It supports various summary formats
including standard descriptives, enhanced pivot tables, and automated
EDA reports using the summarytools package.

**Key Features:**

- Multiple summary formats (standard, enhanced, pivot tables,
  summarytools)

- Date variable support with automatic format detection

- Distribution diagnostics (normality tests, skewness, kurtosis)

- Grouping capabilities for stratified analysis

- Professional HTML output with embedded visualizations

- Export-ready pivot tables with multiple layout styles

**Summary Formats:**

- **Standard**: Basic descriptive statistics (mean, SD, median, range)

- **Enhanced (sumvar style)**: Comprehensive statistics with confidence
  intervals

- **Pivot Enhanced**: Professional pivot tables with customizable
  layouts

- **summarytools Integration**: Automated EDA with embedded
  visualizations

## Value

A results object containing HTML summaries, pivot tables, and
visualizations

## Details

**Date Variable Support:** Automatically detects and parses multiple
date formats including:

- YYYY-MM-DD (ISO 8601)

- DD/MM/YYYY and MM/DD/YYYY

- Date-time combinations with HMS

- Provides date-specific statistics (range, median date, time span)

**Distribution Diagnostics:** When enabled, provides comprehensive
distributional analysis:

- Shapiro-Wilk normality test (for n=3-5000)

- Skewness and kurtosis measures

- Normality interpretation and recommendations

**Pivot Table Layouts:**

- **Clinical**: Optimized for clinical research presentations

- **Statistical**: Focused on statistical analysis requirements

- **Comparative**: Designed for comparative studies and meta-analyses

**summarytools Integration:** Leverages the summarytools package for
automated EDA:

- dfSummary: Comprehensive dataset overview with embedded plots

- descr: Advanced descriptive statistics with robust measures

- freq: Enhanced frequency tables for categorical variables

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`summarydata2Base` -\> `summarydata2Class`

## Methods

### Public methods

- [`summarydata2Class$clone()`](#method-summarydata2Class-clone)

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
- `summarydata2Base$initialize()`

------------------------------------------------------------------------

### `summarydata2Class$clone()`

The objects of this class are cloneable with this method.

#### Usage

    summarydata2Class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
# Basic continuous variable summary
summarydata(
  data = mtcars,
  vars = c("mpg", "hp", "wt"),
  summary_format = "standard",
  distr = TRUE
)
#> Error in summarydata(data = mtcars, vars = c("mpg", "hp", "wt"), summary_format = "standard",     distr = TRUE): unused argument (summary_format = "standard")

# Enhanced pivot table summary
summarydata(
  data = clinical_data,
  vars = c("age", "weight", "height"),
  summary_format = "pivot",
  pivot_layout = "clinical",
  include_confidence = TRUE,
  advanced_metrics = TRUE
)
#> Error in summarydata(data = clinical_data, vars = c("age", "weight", "height"),     summary_format = "pivot", pivot_layout = "clinical", include_confidence = TRUE,     advanced_metrics = TRUE): unused arguments (summary_format = "pivot", pivot_layout = "clinical", include_confidence = TRUE, advanced_metrics = TRUE)

# Date variable analysis
summarydata(
  data = study_data,
  date_vars = c("enrollment_date", "follow_up_date"),
  summary_format = "sumvar"
)
#> Error in summarydata(data = study_data, date_vars = c("enrollment_date",     "follow_up_date"), summary_format = "sumvar"): unused arguments (date_vars = c("enrollment_date", "follow_up_date"), summary_format = "sumvar")

# Grouped analysis with summarytools
summarydata(
  data = trial_data,
  vars = c("baseline_score", "outcome_measure"),
  grvar = "treatment_group",
  summary_format = "summarytools_df",
  summarytools_graphs = TRUE
)
#> Error in summarydata(data = trial_data, vars = c("baseline_score", "outcome_measure"),     grvar = "treatment_group", summary_format = "summarytools_df",     summarytools_graphs = TRUE): unused arguments (grvar = "treatment_group", summary_format = "summarytools_df", summarytools_graphs = TRUE)
# }
```
