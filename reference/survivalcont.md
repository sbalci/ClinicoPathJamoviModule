# Survival Analysis for Continuous Variable

Survival Analysis for Continuous Variable

## Usage

``` r
survivalcont(
  data,
  elapsedtime = NULL,
  tint = FALSE,
  dxdate = NULL,
  fudate = NULL,
  contexpl = NULL,
  outcome = NULL,
  outcomeLevel,
  dod,
  dooc,
  awd,
  awod,
  analysistype = "overall",
  cutp = "12, 36, 60",
  timetypedata = "ymd",
  timetypeoutput = "months",
  uselandmark = FALSE,
  landmark = 3,
  sc = FALSE,
  kmunicate = FALSE,
  ce = FALSE,
  ch = FALSE,
  endplot = 60,
  ybegin_plot = 0,
  yend_plot = 1,
  byplot = 12,
  findcut = FALSE,
  multiple_cutoffs = FALSE,
  num_cutoffs = "two",
  cutoff_method = "quantile",
  min_group_size = 10,
  multievent = FALSE,
  ci95 = FALSE,
  risktable = FALSE,
  censored = FALSE,
  medianline = "none",
  person_time = FALSE,
  time_intervals = "12, 36, 60",
  rate_multiplier = 100,
  rmst_analysis = FALSE,
  rmst_tau = 0,
  residual_diagnostics = FALSE,
  stratified_cox = FALSE,
  strata_variable = NULL,
  loglog = FALSE,
  showExplanations = FALSE,
  showSummaries = FALSE,
  seed = 12345
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  The time-to-event or follow-up duration for each patient. Should be
  numeric and continuous, measured in consistent units (e.g., months or
  years). Can be calculated automatically from dates if using the date
  options below.

- tint:

  Enable this option if you want to calculate survival time from dates
  in your data. This is useful when you have separate columns for
  diagnosis date and follow-up date and want to calculate the time
  elapsed between them.

- dxdate:

  The date of diagnosis or study entry. Accepts: (1) Date/datetime text
  (e.g., "2024-01-15"), (2) Numeric Unix epoch seconds (from DateTime
  Converter's corrected_datetime_numeric output), (3) Numeric datetime
  values from R. Time intervals calculated as difference from follow-up
  date.

- fudate:

  The date of last follow-up or event. Accepts: (1) Date/datetime text
  (e.g., "2024-01-15"), (2) Numeric Unix epoch seconds (from DateTime
  Converter's corrected_datetime_numeric output), (3) Numeric datetime
  values from R. Must be in same format as diagnosis date.

- contexpl:

  The continuous explanatory variable to be used in the analysis.

- outcome:

  The outcome variable to be used in the analysis.

- outcomeLevel:

  The level of the outcome variable that represents the event of
  interest.

- dod:

  .

- dooc:

  .

- awd:

  .

- awod:

  Select the levels of the outcome variable that correspond to different
  event types in your data. For example, you might have separate levels
  for "Dead of Disease" and "Alive w Disease" in a survival analysis of
  cancer patients.

- analysistype:

  Select the survival estimand. Overall counts both death categories as
  events; Cause Specific treats other-cause death as censored;
  Disease-Free counts disease, recurrence, and death categories as
  events according to the selected mapping. Competing-risk coding is
  retained for compatibility but is not analysed by this continuous
  cut-off function; select it to receive guidance to a competing-risk
  analysis.

- cutp:

  Specify time points for survival-probability estimates. The historical
  factory value "12, 36, 60" and the word "default" both select
  unit-aware 1-, 3-, and 5-year points (for example 365, 1095, and 1825
  for days, or 1, 3, and 5 for years). Enter any other comma- or
  space-separated positive values to request custom points in the
  selected output time scale.

- timetypedata:

  select the time type in data

- timetypeoutput:

  select the time type in output

- uselandmark:

  Enable this option to perform landmark survival analysis at a
  specified time point.

- landmark:

  Specify the landmark time at which to evaluate survival probabilities
  in landmark analysis. This option is only available if you enable the
  "Use Landmark Time" option.

- sc:

  Create Kaplan-Meier curves after the continuous explanatory variable
  has been divided by a selected single or multiple cut-off method.

- kmunicate:

  Enable this option to create a KMunicate-style survival plot for the
  continuous explanatory variable.

- ce:

  Enable this option to create a plot of cumulative events over time for
  the continuous explanatory variable.

- ch:

  Enable this option to create a plot of cumulative hazard over time for
  the continuous explanatory variable.

- endplot:

  Specify the end time for the survival plots; this is the maximum time
  point shown on the x-axis. The factory value 60 is interpreted as five
  years in the selected output time scale (1825 for days, 260 for weeks,
  60 for months, 5 for years). Enter any other value to set the axis
  limit explicitly in that scale.

- ybegin_plot:

  Specify the starting value for the y-axis in the survival plots. This
  option allows you to customize the range of the y-axis.

- yend_plot:

  Specify the ending value for the y-axis in the survival plots. This
  option allows you to customize the range of the y-axis.

- byplot:

  Specify the spacing of x-axis tick marks on the survival plots. The
  factory value 12 is interpreted as one year in the selected output
  time scale (365 for days, 52 for weeks, 12 for months, 1 for years).
  Enter any other value to set the spacing explicitly in that scale.

- findcut:

  Derive a single data-dependent cut-off for the continuous explanatory
  variable using a maximally selected rank statistic. Treat the
  continuous Cox model as the primary analysis and validate the selected
  cut-off in independent data.

- multiple_cutoffs:

  Enable this option to derive multiple candidate cut-off points for the
  continuous explanatory variable. Depending on the selected method,
  these may be quantile-based, tree-derived, recursively optimized, or
  minimum-p-value cut-offs. All data-derived cut-offs are exploratory
  and require external validation.

- num_cutoffs:

  Select the number of cut-off points to identify. This creates ordered
  marker-value groups (for example, 2 cut-offs create 3 groups); the
  observed survival ordering must be read from the results and is not
  assumed in advance.

- cutoff_method:

  Method for finding multiple cut-offs. Quantile-based uses
  tertiles/quartiles, Recursive finds sequential optimal points,
  Tree-based uses survival trees, Minimum P-value finds points that
  minimize log-rank p-values.

- min_group_size:

  Minimum percentage of patients required in each group created by
  cut-offs. Prevents creating groups with insufficient sample sizes for
  reliable analysis.

- multievent:

  Enable this option if your data includes multiple event levels (e.g.,
  different types of events or outcomes). This option is required for
  cause-specific and competing risk survival analyses.

- ci95:

  Enable this option to display 95 percent confidence intervals around
  the survival estimates in the plots.

- risktable:

  Enable this option to display a table of risk estimates for each group
  in the survival analysis.

- censored:

  Enable this option to display censored observations in the survival
  plots.

- medianline:

  If true, displays a line indicating the median survival time on the
  survival plot.

- person_time:

  Enable this option to calculate and display person-time metrics,
  including total follow-up time and incidence rates. These metrics help
  quantify the rate of events per unit of time in your study population.

- time_intervals:

  Specify time intervals for stratified person-time analysis. Enter a
  comma-separated list of time points to create intervals. For example,
  "12, 36, 60" will create intervals 0-12, 12-36, 36-60, and 60+.

- rate_multiplier:

  Specify the multiplier for incidence rates (for example, 100 reports
  events per 100 units of the selected output time scale).

- rmst_analysis:

  Report Restricted Mean Survival Time (RMST) up to a specified horizon.
  Without a cut-off this is an overall cohort summary; with a single
  cut-off, the displayed groups are compared descriptively at a common
  observed horizon.

- rmst_tau:

  Specify the time horizon for RMST calculation. If left as 0, will use
  75th percentile of observed survival times. This represents the
  maximum follow-up time for RMST calculation.

- residual_diagnostics:

  Enable Cox model residual diagnostics including case-level Martingale,
  Deviance, and Score residuals plus event-time Schoenfeld residuals.
  These are diagnostic aids; Schoenfeld residuals alone are not a formal
  proportional-hazards test.

- stratified_cox:

  Enable stratified Cox regression analysis. This allows for different
  baseline hazards across strata while maintaining proportional hazards
  within strata.

- strata_variable:

  Categorical variable used for stratification in Cox regression,
  allowing a different baseline hazard in each stratum. Must be a factor
  with a small number of reasonably sized groups (for example treating
  centre or stage). A near-continuous variable would give one stratum
  per patient and no estimable hazard ratio.

- loglog:

  Enable log-log plot for assessing proportional hazards assumption.
  Parallel lines in the log-log plot suggest that proportional hazards
  assumption holds.

- showExplanations:

  Display detailed explanations for each analysis component to help
  interpret the statistical methods and results.

- showSummaries:

  Display natural language summaries alongside tables and plots. These
  summaries provide plain-language interpretations of the statistical
  results. Turn off to reduce visual clutter when summaries are not
  needed.

- seed:

  Random seed for Monte Carlo components of minimum-p-value and
  selection-adjusted cut-point calculations.

## Value

A results object containing:

|                                          |     |     |     |     |                |
|------------------------------------------|-----|-----|-----|-----|----------------|
| `results$eventRecodeInfo`                |     |     |     |     | a html         |
| `results$todo`                           |     |     |     |     | a html         |
| `results$clinicalWarnings`               |     |     |     |     | a html         |
| `results$errors`                         |     |     |     |     | a html         |
| `results$strongWarnings`                 |     |     |     |     | a html         |
| `results$warnings`                       |     |     |     |     | a html         |
| `results$infoMessages`                   |     |     |     |     | a html         |
| `results$coxRegressionHeading`           |     |     |     |     | a preformatted |
| `results$coxSummary`                     |     |     |     |     | a preformatted |
| `results$coxTable`                       |     |     |     |     | a table        |
| `results$stratifiedCoxTable`             |     |     |     |     | a table        |
| `results$tCoxtext2`                      |     |     |     |     | a html         |
| `results$coxRegressionHeading3`          |     |     |     |     | a preformatted |
| `results$coxRegressionExplanation`       |     |     |     |     | a html         |
| `results$personTimeHeading`              |     |     |     |     | a preformatted |
| `results$personTimeTable`                |     |     |     |     | a table        |
| `results$personTimeSummary`              |     |     |     |     | a html         |
| `results$personTimeExplanation`          |     |     |     |     | a html         |
| `results$rmstHeading`                    |     |     |     |     | a preformatted |
| `results$rmstTable`                      |     |     |     |     | a table        |
| `results$rmstSummary`                    |     |     |     |     | a preformatted |
| `results$rmstExplanation`                |     |     |     |     | a html         |
| `results$residualsTable`                 |     |     |     |     | a table        |
| `results$schoenfeldResidualsTable`       |     |     |     |     | a table        |
| `results$residualDiagnosticsExplanation` |     |     |     |     | a html         |
| `results$cutoffAnalysisHeading`          |     |     |     |     | a preformatted |
| `results$rescutTable`                    |     |     |     |     | a table        |
| `results$cutoffAnalysisHeading3`         |     |     |     |     | a preformatted |
| `results$cutoffAnalysisExplanation`      |     |     |     |     | a html         |
| `results$plot4`                          |     |     |     |     | an image       |
| `results$plot5`                          |     |     |     |     | an image       |
| `results$medianSummary`                  |     |     |     |     | a preformatted |
| `results$medianTable`                    |     |     |     |     | a table        |
| `results$survTableSummary`               |     |     |     |     | a preformatted |
| `results$survTable`                      |     |     |     |     | a table        |
| `results$plot2`                          |     |     |     |     | an image       |
| `results$plot3`                          |     |     |     |     | an image       |
| `results$plot6`                          |     |     |     |     | an image       |
| `results$survivalPlotsHeading3`          |     |     |     |     | a preformatted |
| `results$survivalPlotsExplanation`       |     |     |     |     | a html         |
| `results$plot7`                          |     |     |     |     | an image       |
| `results$loglogPlotExplanation`          |     |     |     |     | a html         |
| `results$residualsPlot`                  |     |     |     |     | an image       |
| `results$calculatedtime`                 |     |     |     |     | an output      |
| `results$outcomeredefined`               |     |     |     |     | an output      |
| `results$calculatedcutoff`               |     |     |     |     | an output      |
| `results$multipleCutTable`               |     |     |     |     | a table        |
| `results$multipleMedianTable`            |     |     |     |     | a table        |
| `results$multipleCutoffsExplanation`     |     |     |     |     | a html         |
| `results$multipleSurvTable`              |     |     |     |     | a table        |
| `results$plotMultipleCutoffs`            |     |     |     |     | an image       |
| `results$plotMultipleSurvival`           |     |     |     |     | an image       |
| `results$calculatedmulticut`             |     |     |     |     | an output      |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coxTable$asDF`

`as.data.frame(results$coxTable)`
