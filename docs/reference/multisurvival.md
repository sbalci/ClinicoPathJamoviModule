# Multivariable Survival Analysis

Performs multivariable survival analysis using Cox proportional hazards
regression. In multivariable survival analysis, person-time follow-up is
crucial for properly adjusting for covariates while accounting for
varying observation periods. The Cox proportional hazards model
incorporates person-time by modeling the hazard function, which
represents the instantaneous event rate per unit of person-time. When
stratifying analyses or examining multiple predictors, the model
accounts for how these factors influence event rates relative to the
person-time at risk in each subgroup.

## Usage

``` r
multisurvival(
  data,
  elapsedtime = NULL,
  tint = FALSE,
  dxdate = NULL,
  fudate = NULL,
  timetypedata = "ymd",
  timetypeoutput = "months",
  uselandmark = FALSE,
  landmark = 3,
  outcome = NULL,
  outcomeLevel,
  dod,
  dooc,
  awd,
  awod,
  analysistype = "overall",
  explanatory = NULL,
  contexpl = NULL,
  interactions = NULL,
  multievent = FALSE,
  hr = FALSE,
  sty = "t1",
  ph_cox = TRUE,
  km = FALSE,
  endplot = 60,
  byplot = 12,
  ci95 = FALSE,
  risktable = FALSE,
  censored = FALSE,
  medianline = "none",
  pplot = FALSE,
  cutp = "12, 36, 60",
  calculateRiskScore = FALSE,
  numRiskGroups = "four",
  plotRiskGroups = FALSE,
  ac = FALSE,
  adjexplanatory = NULL,
  ac_method = "average",
  showNomogram = FALSE,
  use_stratify = FALSE,
  stratvar = NULL,
  person_time = FALSE,
  time_intervals = "12, 36, 60",
  rate_multiplier = 100,
  use_tree = FALSE,
  min_node = 20,
  complexity = 0.01,
  max_depth = 5,
  show_terminal_nodes = FALSE,
  showExplanations = FALSE,
  showSummaries = TRUE
)
```

## Arguments

- data:

  The dataset to be analyzed, provided as a data frame. Must contain the
  variables specified in the options below.

- elapsedtime:

  The numeric variable representing follow-up time until the event or
  last observation. If `tint` = false, this should be a pre-calculated
  numeric time variable. If `tint` = true, `dxdate` and `fudate` will be
  used to calculate this time.

- tint:

  If true, survival time will be calculated from `dxdate` and `fudate`.
  If false, `elapsedtime` should be provided as a pre-calculated numeric
  variable.

- dxdate:

  Date of diagnosis. Required if `tint` = true. Accepts: (1)
  Date/datetime text, (2) Numeric Unix epoch seconds (from DateTime
  Converter's corrected_datetime_numeric output), (3) Numeric datetime
  values from R. Time intervals calculated as difference from follow-up
  date.

- fudate:

  Follow-up date or date of last observation. Required if `tint` = true.
  Accepts: (1) Date/datetime text, (2) Numeric Unix epoch seconds (from
  DateTime Converter's corrected_datetime_numeric output), (3) Numeric
  datetime values from R. Must be in same format as diagnosis date.

- timetypedata:

  Specifies the format of the date variables in the input data. This is
  critical if `tint = true`, as `dxdate` and `fudate` will be parsed
  according to this format to calculate survival time. For example, if
  your data files record dates as "YYYY-MM-DD", select `ymd`.

- timetypeoutput:

  The units in which survival time is reported in the output. Choose
  from days, weeks, months, or years.

- uselandmark:

  If true, applies a landmark analysis starting at a specified time
  point.

- landmark:

  The time point (in the units defined by `timetypeoutput`) at which to
  start landmark analyses. Only used if `uselandmark` = true.

- outcome:

  The outcome variable. Typically indicates event status (e.g., death,
  recurrence). For survival analysis, this may be a factor or numeric
  event indicator.

- outcomeLevel:

  The level of `outcome` considered as the event. For example, if
  `outcome` is a factor, specify which level indicates the event
  occurrence.

- dod:

  The level of `outcome` corresponding to death due to disease, if
  applicable.

- dooc:

  The level of `outcome` corresponding to death due to other causes, if
  applicable.

- awd:

  The level of `outcome` corresponding to alive with disease, if
  applicable.

- awod:

  The level of `outcome` corresponding to alive without disease, if
  applicable.

- analysistype:

  Type of survival analysis: - overall: All-cause survival - cause:
  Cause-specific survival - compete: Competing risks analysis

- explanatory:

  Categorical explanatory (predictor) variables included in the Cox
  model.

- contexpl:

  Continuous explanatory (predictor) variables included in the Cox
  model.

- interactions:

  Interaction (crossed) terms added to the Cox model, built from
  variables already selected as explanatory or continuous explanatory
  variables. Each term tests effect modification - e.g. Treatment x
  Biomarker for predictive-biomarker analysis. For a 2-way term the
  first variable is the focal effect and the second is the moderator.

- multievent:

  If true, multiple event levels will be considered for competing risks
  analysis. Requires specifying `dod`, `dooc`, etc.

- hr:

  If true, generates a plot of hazard ratios for each explanatory
  variable in the Cox model.

- sty:

  The style of the hazard ratio (forest) plot. "finalfit" or "survminer
  forestplot".

- ph_cox:

  If true, tests the proportional hazards assumption for the Cox model
  using survival::cox.zph and surfaces global + per-covariate Schoenfeld
  residual statistics. REMARK reporting recommends this be reported for
  any Cox-based prognostic study. Disable only to suppress the
  diagnostic when not needed.

- km:

  If true, produces a Kaplan-Meier survival plot. Useful for
  visualization of survival functions without covariate adjustment.

- endplot:

  The maximum follow-up time (in units defined by `timetypeoutput`) to
  display on survival plots.

- byplot:

  The interval (in units defined by `timetypeoutput`) at which time
  points or labels are shown on plots.

- ci95:

  If true, displays 95 percent confidence intervals around the survival
  estimates on plots.

- risktable:

  If true, displays the number of subjects at risk at each time point
  below the survival plot.

- censored:

  If true, marks censored observations (e.g., using tick marks) on the
  survival plot.

- medianline:

  If true, displays a line indicating the median survival time on the
  survival plot.

- pplot:

  If true, displays the p-value from the survival comparison test on the
  survival plot.

- cutp:

  .

- calculateRiskScore:

  If true, calculates a risk score from the Cox model coefficients for
  each individual.

- numRiskGroups:

  Select the number of risk groups to create from the risk scores. The
  data will be divided into equal quantiles based on this selection.

- plotRiskGroups:

  If true, stratifies individuals into risk groups based on their
  calculated risk scores and plots their survival curves.

- ac:

  .

- adjexplanatory:

  .

- ac_method:

  Method for computing adjusted survival curves

- showNomogram:

  .

- use_stratify:

  If true, uses stratification to handle variables that violate the
  proportional hazards assumption. Stratification creates separate
  baseline hazard functions for different groups.

- stratvar:

  Variables used for stratification. When proportional hazards are not
  met, stratification can adjust the model to better fit the data by
  allowing different baseline hazards.

- person_time:

  Enable this option to calculate and display person-time metrics,
  including total follow-up time and incidence rates. These metrics help
  quantify the rate of events per unit of time in your study population.

- time_intervals:

  Specify time intervals for stratified person-time analysis. Enter a
  comma-separated list of time points to create intervals. For example,
  "12, 36, 60" will create intervals 0-12, 12-36, 36-60, and 60+.

- rate_multiplier:

  Specify the multiplier for incidence rates (e.g., 100 for rates per
  100 person-years, 1000 for rates per 1000 person-years).

- use_tree:

  If true, fits a survival decision tree to identify subgroups with
  different survival outcomes. Decision trees provide an intuitive
  alternative to Cox regression for identifying risk factors.

- min_node:

  The minimum number of observations required in a terminal node. Larger
  values create simpler trees that may be more generalizable but
  potentially miss important subgroups.

- complexity:

  The complexity parameter for tree pruning. Higher values result in
  smaller trees. This parameter controls the trade-off between tree size
  and goodness of fit.

- max_depth:

  The maximum depth of the decision tree. Limits the complexity of the
  tree to avoid overfitting.

- show_terminal_nodes:

  If true, displays Kaplan-Meier survival curves for each terminal node
  of the decision tree.

- showExplanations:

  Display detailed explanations for each analysis component to help
  interpret the statistical methods and results.

- showSummaries:

  Display natural language summaries alongside tables and plots. These
  summaries provide plain-language interpretations of the statistical
  results. Recommended for clinical users. Turn off to reduce visual
  clutter when summaries are not needed.

## Value

A results object containing:

|                                          |     |     |     |     |                |
|------------------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                        |     |     |     |     | a preformatted |
| `results$todo`                           |     |     |     |     | a html         |
| `results$errors`                         |     |     |     |     | a html         |
| `results$strongWarnings`                 |     |     |     |     | a html         |
| `results$warnings`                       |     |     |     |     | a html         |
| `results$infoMessages`                   |     |     |     |     | a html         |
| `results$multivariableCoxHeading`        |     |     |     |     | a preformatted |
| `results$text`                           |     |     |     |     | a html         |
| `results$text2`                          |     |     |     |     | a html         |
| `results$interactionTest`                |     |     |     |     | a table        |
| `results$subgroupHR`                     |     |     |     |     | a table        |
| `results$multivariableCoxSummaryHeading` |     |     |     |     | a preformatted |
| `results$multivariableCoxSummary`        |     |     |     |     | a html         |
| `results$glossaryPanel`                  |     |     |     |     | a html         |
| `results$assumptionsPanel`               |     |     |     |     | a html         |
| `results$personTimeHeading`              |     |     |     |     | a preformatted |
| `results$personTimeTable`                |     |     |     |     | a table        |
| `results$personTimeSummaryHeading`       |     |     |     |     | a preformatted |
| `results$personTimeSummary`              |     |     |     |     | a html         |
| `results$survivalPlotsHeading`           |     |     |     |     | a preformatted |
| `results$plot`                           |     |     |     |     | an image       |
| `results$plot3`                          |     |     |     |     | an image       |
| `results$cox_ph`                         |     |     |     |     | a preformatted |
| `results$plot8`                          |     |     |     |     | an image       |
| `results$plotKM`                         |     |     |     |     | an image       |
| `results$risk_score_analysis`            |     |     |     |     | a preformatted |
| `results$risk_score_analysis2`           |     |     |     |     | a html         |
| `results$riskScoreHeading`               |     |     |     |     | a preformatted |
| `results$riskScoreSummaryHeading`        |     |     |     |     | a preformatted |
| `results$riskScoreTable`                 |     |     |     |     | a table        |
| `results$riskScoreSummary`               |     |     |     |     | a html         |
| `results$riskScoreMetrics`               |     |     |     |     | a html         |
| `results$riskGroupPlot`                  |     |     |     |     | an image       |
| `results$stratificationExplanation`      |     |     |     |     | a html         |
| `results$calculatedtime`                 |     |     |     |     | an output      |
| `results$outcomeredefined`               |     |     |     |     | an output      |
| `results$addRiskScore`                   |     |     |     |     | an output      |
| `results$addRiskGroup`                   |     |     |     |     | an output      |
| `results$adjustedSurvivalHeading`        |     |     |     |     | a preformatted |
| `results$plot_adj`                       |     |     |     |     | an image       |
| `results$adjustedSurvivalSummaryHeading` |     |     |     |     | a preformatted |
| `results$adjustedSurvivalSummary`        |     |     |     |     | a html         |
| `results$nomogramHeading`                |     |     |     |     | a preformatted |
| `results$plot_nomogram`                  |     |     |     |     | an image       |
| `results$nomogram_display`               |     |     |     |     | a html         |
| `results$nomogramSummaryHeading`         |     |     |     |     | a preformatted |
| `results$nomogramSummary`                |     |     |     |     | a html         |
| `results$multivariableCoxExplanation`    |     |     |     |     | a html         |
| `results$multivariableCoxHeading3`       |     |     |     |     | a preformatted |
| `results$adjustedSurvivalExplanation`    |     |     |     |     | a html         |
| `results$riskScoreExplanation`           |     |     |     |     | a html         |
| `results$nomogramExplanation`            |     |     |     |     | a html         |
| `results$personTimeExplanation`          |     |     |     |     | a html         |
| `results$stratifiedAnalysisExplanation`  |     |     |     |     | a html         |
| `results$survivalPlotsHeading3`          |     |     |     |     | a preformatted |
| `results$survivalPlotsExplanation`       |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$interactionTest$asDF`

`as.data.frame(results$interactionTest)`
