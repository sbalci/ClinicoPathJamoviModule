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
  ph_cox = FALSE,
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
  ci_optimism = FALSE,
  ci_optimism_boot = 150,
  ac = FALSE,
  adjexplanatory = NULL,
  ac_method = "average",
  ac_summary = FALSE,
  showNomogram = FALSE,
  compare_models = FALSE,
  use_stratify = FALSE,
  stratvar = NULL,
  person_time = FALSE,
  time_intervals = "12, 36, 60",
  rate_multiplier = 100,
  show_survmetrics = FALSE,
  survmetrics_timepoints = "12, 24, 36, 60",
  survmetrics_show_plots = FALSE,
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
  Biomarker. Calling a treatment interaction predictive requires an
  appropriate treatment-comparison design and pre-specified validation;
  an interaction alone does not establish clinical utility. For a 2-way
  term the first variable is the focal effect and the second is the
  moderator.

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

  Selects whether horizontal, vertical, both, or no median-survival
  reference lines are displayed on survival plots.

- pplot:

  If true, displays the p-value from the survival comparison test on the
  survival plot.

- cutp:

  Positive, comma-separated prediction timepoints in the selected time
  unit. They are used by adjusted-probability tables and the nomogram;
  invalid values and nomogram timepoints beyond observed follow-up are
  omitted with a warning.

- calculateRiskScore:

  If true, calculates the Cox relative-risk score, exp(centered linear
  predictor), for each individual. This ranks fitted hazard and is not
  an absolute event probability; clinical use requires external
  validation.

- numRiskGroups:

  Select the number of risk groups to create from the risk scores. The
  data will be divided into equal quantiles based on this selection.

- plotRiskGroups:

  If true, stratifies individuals into risk groups based on their
  calculated risk scores and plots their survival curves.

- ci_optimism:

  If true, computes a bootstrap optimism-corrected Harrell's C-index
  (apparent, optimism, and corrected) to quantify overfitting of the Cox
  model's discrimination. Not available for competing-risks (Fine-Gray)
  models.

- ci_optimism_boot:

  Number of bootstrap resamples used for optimism correction of the
  C-index. Larger values give more stable estimates but take longer to
  compute.

- ac:

  .

- adjexplanatory:

  Categorical model variable whose levels are contrasted in the adjusted
  curves. The variable must also be selected as an explanatory or
  stratification variable so it is part of the fitted model.

- ac_method:

  Estimand for the adjusted survival curves. "Standardised over cohort"
  sets every observed patient to each level in turn and averages the
  model-predicted curves (g-computation), so the curves differ only by
  the adjustment variable. "At reference covariate profile" predicts a
  single curve per level at the mean/mode of the other covariates.
  "Whole-cohort expected survival" returns one curve for the cohort at
  its observed covariates and does not contrast the levels at all.

- ac_summary:

  Display numeric model-adjusted probability outputs at the cutpoint
  timepoints, adjusted median time to event, and adjusted model effects.
  In competing-risk mode, probabilities are cumulative incidence and
  effects are Fine-Gray subdistribution hazard ratios.

- showNomogram:

  Display a Cox-model nomogram for the requested prediction timepoints.
  Predictions are apparent estimates from the fitted data and are not a
  point-of-care tool without calibration and external validation.

- compare_models:

  If true, reports a likelihood-ratio test and AIC for dropping each
  covariate from the full model (base R drop1). Shows which covariates
  significantly improve model fit.

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

  Specify the multiplier for incidence rates (for example, 100 for rates
  per 100 person-time units). The person-time unit is the unit selected
  in `timetypeoutput`; choose years there before interpreting a rate as
  events per person-year.

- show_survmetrics:

  Report model performance metrics for the Cox model: Harrell's
  concordance (C-index), inverse-probability-of-censoring-weighted
  (IPCW) Brier score and time-dependent AUC at the chosen timepoints,
  and the Integrated Brier Score. Computed with the riskRegression
  package.

- survmetrics_timepoints:

  Comma-separated timepoints at which to report the Brier score and
  time-dependent AUC. Timepoints beyond the observed follow-up are
  ignored. Should correspond to clinically meaningful follow-up times.

- survmetrics_show_plots:

  Display a plot of the IPCW Brier score across follow-up time.

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
| `results$eventRecodeInfo`                |     |     |     |     | a html         |
| `results$todo`                           |     |     |     |     | a html         |
| `results$errors`                         |     |     |     |     | a html         |
| `results$strongWarnings`                 |     |     |     |     | a html         |
| `results$warnings`                       |     |     |     |     | a html         |
| `results$infoMessages`                   |     |     |     |     | a html         |
| `results$multivariableCoxHeading`        |     |     |     |     | a preformatted |
| `results$text`                           |     |     |     |     | a html         |
| `results$text2`                          |     |     |     |     | a html         |
| `results$interactionExplanation`         |     |     |     |     | a html         |
| `results$interactionTest`                |     |     |     |     | a table        |
| `results$subgroupHR`                     |     |     |     |     | a table        |
| `results$multivariableCoxSummaryHeading` |     |     |     |     | a preformatted |
| `results$multivariableCoxSummary`        |     |     |     |     | a html         |
| `results$glossaryPanel`                  |     |     |     |     | a html         |
| `results$assumptionsPanel`               |     |     |     |     | a html         |
| `results$survMetricsTable`               |     |     |     |     | a table        |
| `results$survMetricsSummary`             |     |     |     |     | a html         |
| `results$survMetricsPlot`                |     |     |     |     | an image       |
| `results$personTimeHeading`              |     |     |     |     | a preformatted |
| `results$personTimeTable`                |     |     |     |     | a table        |
| `results$personTimeSummaryHeading`       |     |     |     |     | a preformatted |
| `results$personTimeSummary`              |     |     |     |     | a html         |
| `results$survivalPlotsHeading`           |     |     |     |     | a preformatted |
| `results$plot`                           |     |     |     |     | an image       |
| `results$plot3`                          |     |     |     |     | an image       |
| `results$cox_phTable`                    |     |     |     |     | a table        |
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
| `results$cindexValidation`               |     |     |     |     | a table        |
| `results$stratificationExplanation`      |     |     |     |     | a html         |
| `results$calculatedtime`                 |     |     |     |     | an output      |
| `results$outcomeredefined`               |     |     |     |     | an output      |
| `results$addRiskScore`                   |     |     |     |     | an output      |
| `results$addRiskGroup`                   |     |     |     |     | an output      |
| `results$adjustedSurvivalHeading`        |     |     |     |     | a preformatted |
| `results$adjustedEstimandPanel`          |     |     |     |     | a html         |
| `results$plot_adj`                       |     |     |     |     | an image       |
| `results$adjustedSurvivalSummaryHeading` |     |     |     |     | a preformatted |
| `results$adjustedSurvivalSummary`        |     |     |     |     | a html         |
| `results$nomogramHeading`                |     |     |     |     | a preformatted |
| `results$plot_nomogram`                  |     |     |     |     | an image       |
| `results$nomogram_display`               |     |     |     |     | a html         |
| `results$nomogramSummaryHeading`         |     |     |     |     | a preformatted |
| `results$nomogramSummary`                |     |     |     |     | a html         |
| `results$adjustedSurvTable`              |     |     |     |     | a table        |
| `results$adjustedSurvTableSummary`       |     |     |     |     | a html         |
| `results$adjustedMedianTable`            |     |     |     |     | a table        |
| `results$adjustedMedianSummary`          |     |     |     |     | a html         |
| `results$adjustedCoxTable`               |     |     |     |     | a table        |
| `results$adjustedCoxText`                |     |     |     |     | a html         |
| `results$adjustedCoxSummary`             |     |     |     |     | a html         |
| `results$adjustedCoxPH`                  |     |     |     |     | a preformatted |
| `results$modelContributionTable`         |     |     |     |     | a table        |
| `results$modelContributionSummary`       |     |     |     |     | a html         |
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
