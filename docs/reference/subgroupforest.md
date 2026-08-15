# Subgroup Analysis Forest Plot

Creates forest plots showing treatment effects across different patient
subgroups. Performs subgroup analysis for clinical trials and
observational studies, calculating treatment effects within patient
subgroups and testing for interactions. Supports survival
(time-to-event), binary, and continuous outcomes with comprehensive
statistical validation and heterogeneity testing.

## Usage

``` r
subgroupforest(
  data,
  outcome,
  treatment,
  subgroups,
  time = NULL,
  event = NULL,
  outcomeType = "survival",
  effectMeasure = "hr",
  confidenceLevel = "0.95",
  showOverall = TRUE,
  showInteraction = TRUE,
  sortBy = "effect",
  showSampleSizes = TRUE,
  logScale = TRUE,
  nullLine = 1
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Primary outcome variable. For survival analysis, this should be the
  time variable. For binary outcomes, use 0/1 or factor with 2 levels.
  For continuous outcomes, use numeric variables.

- treatment:

  Treatment or exposure variable (must be binary: 0/1,
  control/treatment). This variable defines the two groups being
  compared in the analysis.

- subgroups:

  Variables defining patient subgroups for analysis (categorical
  variables). Each variable will be analyzed separately to identify
  differential treatment effects. Examples: age groups, gender, disease
  stage, biomarker status.

- time:

  Time variable for survival analysis (numeric, required if outcomeType
  = "survival"). Should contain time to event or censoring in consistent
  units (days, months, years).

- event:

  Event indicator for survival analysis (binary: 1=event occurred,
  0=censored). Required for survival analysis to distinguish between
  observed events and censored observations.

- outcomeType:

  Type of outcome variable: "survival" (time-to-event), "binary"
  (yes/no), "continuous". Determines the statistical method used for
  analysis (Cox regression, logistic regression, linear regression).

- effectMeasure:

  Statistical measure for treatment effect: "hr" (hazard ratio), "or"
  (odds ratio), "rr" (risk ratio), "md" (mean difference). Should match
  the outcome type.

- confidenceLevel:

  Confidence level for intervals.

- showOverall:

  Display overall treatment effect across all patients.

- showInteraction:

  Perform statistical tests for subgroup interactions using likelihood
  ratio tests. Tests whether treatment effect varies significantly
  across subgroups.

- sortBy:

  Method for ordering subgroups in the plot: "effect" (by effect size),
  "n" (by sample size), "alpha" (alphabetical). Affects visual
  presentation.

- showSampleSizes:

  Display sample sizes for each subgroup.

- logScale:

  Display effects on log scale (recommended for ratios: HR, OR, RR).
  Makes ratio effects more interpretable and symmetric around null
  value.

- nullLine:

  Value for null effect reference line (1 for ratios: HR/OR/RR, 0 for
  differences: MD). Vertical line indicating no treatment effect.

## Value

A results object containing:

|                         |     |     |     |     |          |
|-------------------------|-----|-----|-----|-----|----------|
| `results$todo`          |     |     |     |     | a html   |
| `results$plot`          |     |     |     |     | an image |
| `results$summary`       |     |     |     |     | a table  |
| `results$interactions`  |     |     |     |     | a table  |
| `results$overall`       |     |     |     |     | a table  |
| `results$heterogeneity` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
