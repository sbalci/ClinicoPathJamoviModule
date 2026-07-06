# Survival Analysis for Continuous Explanatory Variable

Comprehensive survival analysis for continuous explanatory variables
with optimal cut-off determination, multiple cut-offs analysis, RMST
analysis, residual diagnostics, and advanced visualization options.

## Details

This function provides advanced survival analysis specifically designed
for continuous explanatory variables. It includes:

**Core Features:**

- Optimal cut-off determination using maximally selected rank statistics

- Multiple cut-offs analysis with 4 different methods (quantile,
  recursive, tree-based, minimum p-value)

- Person-time analysis with interval stratification

- Date-based time calculation with multiple format support

- Multiple event level support (overall, cause-specific, competing
  risks)

- Landmark analysis for time-dependent effects

**Advanced Analytics:**

- Restricted Mean Survival Time (RMST) analysis

- Cox model residual diagnostics (Martingale, Deviance, Score,
  Schoenfeld)

- Log-log plots for proportional hazards assessment

- Enhanced error handling and data validation

**Visualization Options:**

- Kaplan-Meier survival curves with optimal cut-offs

- Multiple cut-offs histogram with cut-point annotations

- Cumulative events and hazard plots

- KMunicate-style plots for publication

- Residual diagnostic plots (4-panel layout)

- Log-log plots for assumption checking

## References

Hothorn, T., & Zeileis, A. (2008). Generalized maximally selected
statistics. Biometrics, 64(4), 1263-1269.

Royston, P., & Parmar, M. K. (2013). Restricted mean survival time: an
alternative to the hazard ratio for the design and analysis of
randomized trials with a time-to-event outcome. BMC Medical Research
Methodology, 13(1), 152.

Morris, T. P., et al. (2019). Proposals on Kaplan-Meier plots in medical
research and a survey of stakeholder views: KMunicate. BMJ Open, 9(9),
e030874.

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`survivalcontBase` -\> `survivalcontClass`

## Methods

### Public methods

- [`survivalcontClass$asSource()`](#method-survivalcontClass-asSource)

- [`survivalcontClass$clone()`](#method-survivalcontClass-clone)

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
- `survivalcontBase$initialize()`

------------------------------------------------------------------------

### `survivalcontClass$asSource()`

Generate R source code for survivalcont analysis

#### Usage

    survivalcontClass$asSource()

#### Returns

Character string with R syntax for reproducible analysis

------------------------------------------------------------------------

### `survivalcontClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    survivalcontClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic survival analysis with optimal cut-off
data("lung", package = "survival")
lung$status_binary <- ifelse(lung$status == 2, 1, 0)

result1 <- survivalcont(
  data = lung,
  elapsedtime = "time",
  outcome = "status_binary",
  contexpl = "age",
  findcut = TRUE,
  sc = TRUE
)

# Multiple cut-offs analysis with different methods
result2 <- survivalcont(
  data = lung,
  elapsedtime = "time",
  outcome = "status_binary",
  contexpl = "ph.karno",
  multiple_cutoffs = TRUE,
  num_cutoffs = "three",
  cutoff_method = "recursive",
  min_group_size = 15,
  sc = TRUE
)

# RMST analysis with residual diagnostics
result3 <- survivalcont(
  data = lung,
  elapsedtime = "time",
  outcome = "status_binary",
  contexpl = "wt.loss",
  findcut = TRUE,
  rmst_analysis = TRUE,
  rmst_tau = 500,
  residual_diagnostics = TRUE,
  loglog = TRUE
)

# Person-time analysis with date calculation
# Create sample data with dates
set.seed(123)
n <- 200
sample_data <- data.frame(
  biomarker = rnorm(n, 100, 25),
  event = rbinom(n, 1, 0.6),
  dx_date = as.Date("2020-01-01") + sample(0:365, n, replace = TRUE),
  fu_date = as.Date("2020-01-01") + sample(366:1095, n, replace = TRUE)
)

result4 <- survivalcont(
  data = sample_data,
  tint = TRUE,
  dxdate = "dx_date",
  fudate = "fu_date",
  timetypedata = "ymd",
  timetypeoutput = "months",
  outcome = "event",
  contexpl = "biomarker",
  person_time = TRUE,
  time_intervals = "6, 12, 24",
  rate_multiplier = 1000,
  calculatedtime = TRUE
)

# Comprehensive analysis with all features
result5 <- survivalcont(
  data = lung,
  elapsedtime = "time",
  outcome = "status_binary",
  contexpl = "meal.cal",
  findcut = TRUE,
  multiple_cutoffs = TRUE,
  num_cutoffs = "two",
  cutoff_method = "quantile",
  rmst_analysis = TRUE,
  rmst_tau = 400,
  residual_diagnostics = TRUE,
  person_time = TRUE,
  time_intervals = "100, 300, 500",
  sc = TRUE,
  ce = TRUE,
  ch = TRUE,
  kmunicate = TRUE,
  loglog = TRUE,
  ci95 = TRUE,
  risktable = TRUE,
  calculatedcutoff = TRUE,
  calculatedmulticut = TRUE
)
} # }
```
