# Survey-Weighted Survival Analysis

Perform survival analysis on complex survey data using the survey
package. Supports weighted estimates, complex sampling designs, and
weighted Cox proportional hazards models.

## Usage

``` r
surveysurvival(
  data,
  elapsedtime = NULL,
  tint = FALSE,
  dxdate = NULL,
  fudate = NULL,
  timetypedata = "ymd",
  timetypeoutput = "months",
  outcome = NULL,
  outcomeLevel,
  weights = NULL,
  strata = NULL,
  cluster = NULL,
  fpc = NULL,
  design_type = "srs",
  nest_clusters = FALSE,
  explanatory = NULL,
  contexpl = NULL,
  km_weighted = TRUE,
  cox_weighted = FALSE,
  robust_se = TRUE,
  ci_level = 0.95,
  population_totals = FALSE,
  subpopulation = NULL,
  km_plot = TRUE,
  endplot = 60,
  byplot = 12,
  ci95 = TRUE,
  risktable = FALSE,
  design_summary = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The survey dataset to be analyzed, provided as a data frame. Must
  contain survival variables, survey design variables (weights, strata,
  clusters), and any explanatory variables for analysis.

- elapsedtime:

  The numeric variable representing follow-up time until the event or
  last observation. Time should be in consistent units across all
  observations.

- tint:

  If true, survival time will be calculated from diagnosis and follow-up
  dates. If false, elapsedtime should be provided as a pre-calculated
  numeric variable.

- dxdate:

  Date of diagnosis or start of follow-up. Required if tint = true. Must
  match the format specified in timetypedata.

- fudate:

  Follow-up date or date of last observation. Required if tint = true.
  Must match the format specified in timetypedata.

- timetypedata:

  Specifies the format of date variables in the input data. Used when
  tint = true to parse diagnosis and follow-up dates.

- timetypeoutput:

  The units in which survival time is reported in the output.

- outcome:

  The outcome variable indicating event status (e.g., death, disease
  occurrence).

- outcomeLevel:

  The level of outcome considered as the event.

- weights:

  Variable containing survey sampling weights for each observation.
  Required for survey-weighted analysis.

- strata:

  Variable defining survey strata. Used in stratified sampling designs.

- cluster:

  Variable defining primary sampling units or clusters. Used in cluster
  and multi-stage sampling designs.

- fpc:

  Variable containing finite population correction factors. Optional for
  improved variance estimation when sampling fraction is large.

- design_type:

  Type of survey sampling design used to collect the data.

- nest_clusters:

  Whether clusters are nested within strata (TRUE) or crossed (FALSE).
  Relevant for stratified cluster designs.

- explanatory:

  Categorical explanatory variables for weighted Cox regression.

- contexpl:

  Continuous explanatory variables for weighted Cox regression.

- km_weighted:

  Perform survey-weighted Kaplan-Meier survival estimation.

- cox_weighted:

  Perform survey-weighted Cox proportional hazards regression.

- robust_se:

  Use robust variance estimation accounting for survey design effects.

- ci_level:

  Confidence level for confidence intervals (e.g., 0.95 for 95 percent
  CI).

- population_totals:

  Calculate population-level survival estimates and totals.

- subpopulation:

  Variable defining subpopulation for domain estimation.

- km_plot:

  Generate survey-weighted Kaplan-Meier survival plot.

- endplot:

  Maximum follow-up time to display on survival plots.

- byplot:

  Time interval for plot labels and risk tables.

- ci95:

  Display confidence intervals on survival plots.

- risktable:

  Display number at risk below survival plots.

- design_summary:

  Display summary of survey design characteristics.

- showSummaries:

  Display natural language summaries alongside tables and plots for
  interpretation of survey-weighted survival results.

- showExplanations:

  Display detailed explanations of survey-weighted survival methods and
  interpretation guidelines.

## Value

A results object containing:

|                                          |     |     |     |     |           |
|------------------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`                           |     |     |     |     | a html    |
| `results$designSummary`                  |     |     |     |     | a html    |
| `results$survivalAnalysis`               |     |     |     |     | a html    |
| `results$coxAnalysis`                    |     |     |     |     | a html    |
| `results$populationEstimates`            |     |     |     |     | a html    |
| `results$kmPlot`                         |     |     |     |     | an image  |
| `results$designDiagnostics`              |     |     |     |     | a table   |
| `results$weightedSurvivalTable`          |     |     |     |     | a table   |
| `results$coxCoefficients`                |     |     |     |     | a table   |
| `results$populationTotalsTable`          |     |     |     |     | a table   |
| `results$subpopulationAnalysis`          |     |     |     |     | a table   |
| `results$modelFitStatistics`             |     |     |     |     | a table   |
| `results$designEffectsSummary`           |     |     |     |     | a html    |
| `results$analysisSummary`                |     |     |     |     | a html    |
| `results$methodExplanation`              |     |     |     |     | a html    |
| `results$surveyDesignExplanation`        |     |     |     |     | a html    |
| `results$kmWeightedExplanation`          |     |     |     |     | a html    |
| `results$coxWeightedExplanation`         |     |     |     |     | a html    |
| `results$populationInferenceExplanation` |     |     |     |     | a html    |
| `results$calculatedtime`                 |     |     |     |     | an output |
| `results$outcomeredefined`               |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$designDiagnostics$asDF`

`as.data.frame(results$designDiagnostics)`

## Details

This module implements survey-weighted survival methods for
population-based inference from complex sampling designs including
stratified, clustered, and multi-stage sampling. The analysis accounts
for survey design effects on standard errors and confidence intervals,
enabling proper population-level survival estimates from survey data.
