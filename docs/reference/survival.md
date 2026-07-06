# Survival Analysis

Performs univariate survival analysis comparing survival between groups.
This analysis calculates person-time follow-up for each group and uses
this to derive accurate survival estimates and incidence rates that
account for varying follow-up durations across groups. The Cox
proportional hazards model incorporates person-time by modeling the
hazard function, which represents the instantaneous event rate per unit
of person-time.

## Usage

``` r
survival(
  data,
  elapsedtime = NULL,
  tint = FALSE,
  dxdate = NULL,
  fudate = NULL,
  explanatory = NULL,
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
  pw = FALSE,
  padjustmethod = "holm",
  weightedLogRank = FALSE,
  survivalTestType = "logrank",
  ph_cox = FALSE,
  sc = FALSE,
  kmunicate = FALSE,
  ce = FALSE,
  ch = FALSE,
  endplot = 60,
  ybegin_plot = 0,
  yend_plot = 1,
  byplot = 12,
  multievent = FALSE,
  ci95 = FALSE,
  risktable = FALSE,
  censored = FALSE,
  pplot = FALSE,
  medianline = "none",
  person_time = FALSE,
  time_intervals = "12, 36, 60",
  rate_multiplier = 100,
  rmst_analysis = FALSE,
  rmst_tau = 0,
  stratified_cox = FALSE,
  strata_variable = NULL,
  age_adjustment = FALSE,
  age_variable = NULL,
  age_interaction = FALSE,
  age_stratified_cox = FALSE,
  age_group_cutpoints = "50, 65, 75",
  age_time_scale = FALSE,
  age_standardization = FALSE,
  age_standardization_method = "indirect",
  age_stratified_km = FALSE,
  adjusted_curves = FALSE,
  remark_checklist = FALSE,
  residual_diagnostics = FALSE,
  loglog = FALSE,
  showExplanations = FALSE,
  showSummaries = FALSE,
  use_parametric = FALSE,
  parametric_distribution = "weibull",
  parametric_covariates = TRUE,
  spline_knots = 3,
  spline_scale = "hazard",
  parametric_extrapolation = FALSE,
  extrapolation_time = 0,
  parametric_diagnostics = TRUE,
  compare_distributions = FALSE,
  parametric_survival_plots = FALSE,
  hazard_plots = FALSE,
  calibration_curves = FALSE,
  calibration_timepoint = 0,
  calibration_ngroups = 5,
  rcs_analysis = FALSE,
  rcs_variable = NULL,
  rcs_knots = 4,
  bootstrapValidation = FALSE,
  bootstrapValN = 200
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  The time elapsed from the start of the study to the event or
  censoring.

- tint:

  If the time is in date format, select this option to calculate the
  survival time. The time will be calculated as the difference between
  the event date and the diagnosis date. If the follow-up date is
  available, the time will be calculated as the difference between the
  event date and the follow-up date.

- dxdate:

  The date of diagnosis. Accepts: (1) Date/datetime text formats (e.g.,
  "2024-01-15"), (2) Numeric Unix epoch seconds (from DateTime
  Converter's corrected_datetime_numeric output), (3) Numeric datetime
  values from R. Time intervals will be automatically calculated as the
  difference between follow-up/event date and diagnosis date.

- fudate:

  The date of follow-up or event. Accepts: (1) Date/datetime text
  formats (e.g., "2024-01-15"), (2) Numeric Unix epoch seconds (from
  DateTime Converter's corrected_datetime_numeric output), (3) Numeric
  datetime values from R. Must be in the same format as diagnosis date.
  Time intervals calculated as difference from diagnosis date.

- explanatory:

  The explanatory variable that will be used to compare the survival
  times of different groups.

- outcome:

  The outcome variable that will be used to compare the survival times
  of different groups.

- outcomeLevel:

  The level of the outcome variable that will be used as the event
  level.

- dod:

  The level of the outcome variable that indicates death from disease.

- dooc:

  The level of the outcome variable that indicates death from other
  causes.

- awd:

  The level of the outcome variable that indicates alive with disease.

- awod:

  The level of the outcome variable that indicates alive without
  disease.

- analysistype:

  .

- cutp:

  .

- timetypedata:

  select the time type in data

- timetypeoutput:

  select the time type in output

- uselandmark:

  .

- landmark:

  .

- pw:

  .

- padjustmethod:

  .

- weightedLogRank:

  .

- survivalTestType:

  .

- ph_cox:

  .

- sc:

  .

- kmunicate:

  .

- ce:

  .

- ch:

  .

- endplot:

  .

- ybegin_plot:

  .

- yend_plot:

  .

- byplot:

  .

- multievent:

  .

- ci95:

  .

- risktable:

  .

- censored:

  .

- pplot:

  .

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

  Specify the multiplier for incidence rates (e.g., 100 for rates per
  100 person-years, 1000 for rates per 1000 person-years).

- rmst_analysis:

  Calculate Restricted Mean Survival Time, which represents the average
  survival time up to a specified time horizon. Useful when median
  survival cannot be estimated or for comparing survival over a specific
  time period.

- rmst_tau:

  Time horizon for RMST calculation. If 0 or not specified, uses the
  75th percentile of follow-up time. Should be specified in the same
  units as the survival time.

- stratified_cox:

  Perform stratified Cox regression to account for non-proportional
  hazards or unmeasured confounders that affect baseline hazard.

- strata_variable:

  Variable to use for stratification in Cox regression. This variable
  should represent groups with different baseline hazards.

- age_adjustment:

  Enable age adjustment in Cox regression. When enabled, age is included
  as a covariate in the Cox model, providing age-adjusted hazard ratios.
  Recommended when comparing groups that may differ in age distribution.

- age_variable:

  Continuous variable representing patient age at diagnosis or study
  entry. Used as a covariate in Cox regression for age adjustment. Must
  be numeric.

- age_interaction:

  Test whether age modifies the effect of the explanatory variable on
  survival (age-group interaction). Reports the interaction term
  p-value.

- age_stratified_cox:

  Stratify the Cox model by age groups instead of adjusting as a
  covariate. Allows different baseline hazards for each age group
  without assuming a linear age effect.

- age_group_cutpoints:

  Comma-separated age cutpoints for creating age groups. For example,
  50, 65, 75 creates groups less than 50, 50-64, 65-74, 75 and above.

- age_time_scale:

  Use biological age as the time axis instead of follow-up time. The Cox
  model becomes Surv(age_at_entry, age_at_event, event). Most rigorous
  for cancer epidemiology where age drives risk.

- age_standardization:

  Compute age-standardized mortality using direct or indirect
  standardization. Reports Standardized Mortality Ratio comparing
  observed vs expected deaths by age group.

- age_standardization_method:

  Indirect standardization computes SMR (observed/expected). Direct
  standardization applies age-specific rates to a standard population.

- age_stratified_km:

  Show Kaplan-Meier survival curves stratified by age groups. Displays
  separate curves for each age group within the explanatory variable.

- adjusted_curves:

  Show Kaplan-Meier-style survival curves adjusted for age using the Cox
  model. Displays what the survival curves would look like after
  removing the effect of age differences between groups.

- remark_checklist:

  Display a REMARK (REporting recommendations for tumor MARKer
  prognostic studies) checklist showing which reporting items are
  addressed by the current analysis configuration.

- residual_diagnostics:

  Calculate and display Cox model residuals for diagnostic purposes,
  including Martingale, deviance, score, and Schoenfeld residuals.

- loglog:

  Display log-log survival plot for visual assessment of proportional
  hazards assumption. Parallel lines suggest proportional hazards.

- showExplanations:

  Display detailed explanations for each analysis component to help
  interpret the statistical methods and results.

- showSummaries:

  Display natural language summaries alongside tables and plots. These
  summaries provide plain-language interpretations of the statistical
  results. Turn off to reduce visual clutter when summaries are not
  needed.

- use_parametric:

  Enable parametric survival modeling using flexsurv package. Provides
  alternative to Cox regression with explicit hazard functions and
  extrapolation capabilities beyond observed follow-up.

- parametric_distribution:

  Choose parametric distribution for survival modeling. Weibull is most
  common, while splines provide flexible hazard shapes. Different
  distributions make different assumptions about hazard function shape.

- parametric_covariates:

  Include explanatory variables as covariates in parametric models,
  similar to Cox regression but with parametric baseline hazard.

- spline_knots:

  Number of knots for spline-based models. More knots allow more
  flexible hazard shapes but may lead to overfitting. Used only for
  Royston-Parmar splines.

- spline_scale:

  Scale for spline-based models. Hazard scale models log hazard
  function, odds scale models log cumulative odds, normal scale models
  normal scores.

- parametric_extrapolation:

  Perform survival extrapolation beyond observed follow-up time using
  fitted parametric models. Useful for health economic modeling and
  long-term prognosis assessment.

- extrapolation_time:

  Maximum time for survival extrapolation (in same units as survival
  time). If 0, uses 2x maximum observed time. Use with caution as
  extrapolation relies on distributional assumptions.

- parametric_diagnostics:

  Display model diagnostics including AIC/BIC for model comparison,
  residual plots, and goodness-of-fit statistics for parametric models.

- compare_distributions:

  Fit and compare multiple parametric distributions using AIC/BIC
  criteria. Helps select the best-fitting distribution for your data.

- parametric_survival_plots:

  Generate survival curves from fitted parametric models with confidence
  intervals. Compare with Kaplan-Meier estimates for model validation.

- hazard_plots:

  Plot estimated hazard functions from parametric models. Shows how
  instantaneous risk changes over time for different distributions.

- calibration_curves:

  Assess calibration of the Cox model by comparing predicted versus
  observed survival at specified time points. Groups patients by
  predicted risk quintiles and computes Kaplan-Meier survival per group.
  Includes calibration slope, calibration-in-the-large, and calibration
  plot. Requires Cox model to be fitted.

- calibration_timepoint:

  Time point for calibration assessment (e.g., 60 for 5-year OS in
  months). If 0, uses the median observed time. The calibration plot
  compares predicted vs observed survival probability at this time
  point.

- calibration_ngroups:

  Number of risk groups (quantiles) for calibration assessment. Default
  is 5 (quintiles). More groups provide finer resolution but require
  larger sample sizes. Each group should have at least 20-30 patients.

- rcs_analysis:

  Assess non-linear effects of continuous predictors using restricted
  cubic splines (natural splines). Fits Cox model with
  spline-transformed predictor, tests for non-linearity via likelihood
  ratio test, and plots hazard ratio curve. Important for variables like
  age and tumor size where the relationship with outcome may not be
  linear.

- rcs_variable:

  Select a continuous predictor variable to assess for non-linear
  effects. The variable will be modeled with restricted cubic splines
  and compared to a linear model. Common candidates include age, tumor
  size, biomarker levels, and follow-up duration.

- rcs_knots:

  Number of knots for restricted cubic splines. 3 knots = 2 df
  (simplest), 4 knots = 3 df (recommended default), 5 knots = 4 df (more
  flexible). Knots are placed at Harrell-recommended percentiles. More
  knots allow detection of complex non-linear patterns but may overfit
  with small samples.

- bootstrapValidation:

  .

- bootstrapValN:

  .

## Value

A results object containing:

|                                             |     |     |     |     |                |
|---------------------------------------------|-----|-----|-----|-----|----------------|
| `results$subtitle`                          |     |     |     |     | a preformatted |
| `results$todo`                              |     |     |     |     | a html         |
| `results$errors`                            |     |     |     |     | a html         |
| `results$strongWarnings`                    |     |     |     |     | a html         |
| `results$warnings`                          |     |     |     |     | a html         |
| `results$infoMessages`                      |     |     |     |     | a html         |
| `results$medianSurvivalHeading`             |     |     |     |     | a preformatted |
| `results$medianSummary`                     |     |     |     |     | a preformatted |
| `results$medianTable`                       |     |     |     |     | a table        |
| `results$medianSurvivalHeading3`            |     |     |     |     | a preformatted |
| `results$medianSurvivalExplanation`         |     |     |     |     | a html         |
| `results$coxRegressionHeading`              |     |     |     |     | a preformatted |
| `results$coxSummary`                        |     |     |     |     | a preformatted |
| `results$coxTable`                          |     |     |     |     | a table        |
| `results$tCoxtext2`                         |     |     |     |     | a html         |
| `results$coxRegressionHeading3`             |     |     |     |     | a preformatted |
| `results$coxRegressionExplanation`          |     |     |     |     | a html         |
| `results$ageAdjustedCoxHeading`             |     |     |     |     | a preformatted |
| `results$ageAdjustedCoxTable`               |     |     |     |     | a table        |
| `results$ageInteractionTable`               |     |     |     |     | a table        |
| `results$ageAdjustedInterpretation`         |     |     |     |     | a html         |
| `results$ageAdjustedExplanation`            |     |     |     |     | a html         |
| `results$ageTimeScaleTable`                 |     |     |     |     | a table        |
| `results$ageTimeScaleInterpretation`        |     |     |     |     | a html         |
| `results$ageStandardizationTable`           |     |     |     |     | a table        |
| `results$ageStandardizationInterpretation`  |     |     |     |     | a html         |
| `results$ageStratifiedKMPlot`               |     |     |     |     | an image       |
| `results$adjustedCurvesPlot`                |     |     |     |     | an image       |
| `results$remarkChecklist`                   |     |     |     |     | a html         |
| `results$cox_ph`                            |     |     |     |     | a preformatted |
| `results$phInterpretation`                  |     |     |     |     | a html         |
| `results$plot8`                             |     |     |     |     | an image       |
| `results$survivalTablesHeading`             |     |     |     |     | a preformatted |
| `results$survTableSummary`                  |     |     |     |     | a preformatted |
| `results$survTable`                         |     |     |     |     | a table        |
| `results$survivalTablesHeading3`            |     |     |     |     | a preformatted |
| `results$survivalTablesExplanation`         |     |     |     |     | a html         |
| `results$personTimeHeading`                 |     |     |     |     | a preformatted |
| `results$personTimeTable`                   |     |     |     |     | a table        |
| `results$personTimeSummary`                 |     |     |     |     | a html         |
| `results$personTimeExplanation`             |     |     |     |     | a html         |
| `results$rmstHeading`                       |     |     |     |     | a preformatted |
| `results$rmstTable`                         |     |     |     |     | a table        |
| `results$rmstSummary`                       |     |     |     |     | a preformatted |
| `results$rmstExplanation`                   |     |     |     |     | a html         |
| `results$residualDiagnosticsExplanation`    |     |     |     |     | a html         |
| `results$residualsTable`                    |     |     |     |     | a table        |
| `results$survivalExport`                    |     |     |     |     | an output      |
| `results$survivalExportSummary`             |     |     |     |     | a html         |
| `results$pairwiseComparisonHeading`         |     |     |     |     | a preformatted |
| `results$pairwiseTable`                     |     |     |     |     | a table        |
| `results$pairwiseSummary`                   |     |     |     |     | a preformatted |
| `results$weightedLogRankTable`              |     |     |     |     | a table        |
| `results$weightedLogRankExplanation`        |     |     |     |     | a html         |
| `results$plot`                              |     |     |     |     | an image       |
| `results$plot2`                             |     |     |     |     | an image       |
| `results$plot3`                             |     |     |     |     | an image       |
| `results$plot6`                             |     |     |     |     | an image       |
| `results$survivalPlotsHeading3`             |     |     |     |     | a preformatted |
| `results$survivalPlotsExplanation`          |     |     |     |     | a html         |
| `results$plot7`                             |     |     |     |     | an image       |
| `results$residualsPlot`                     |     |     |     |     | an image       |
| `results$calculatedtime`                    |     |     |     |     | an output      |
| `results$outcomeredefined`                  |     |     |     |     | an output      |
| `results$calibrationTable`                  |     |     |     |     | a table        |
| `results$calibrationGroupTable`             |     |     |     |     | a table        |
| `results$calibrationPlot`                   |     |     |     |     | an image       |
| `results$calibrationInterpretation`         |     |     |     |     | a html         |
| `results$rcsTestTable`                      |     |     |     |     | a table        |
| `results$rcsPlot`                           |     |     |     |     | an image       |
| `results$rcsInterpretation`                 |     |     |     |     | a html         |
| `results$bootstrapValidationTable`          |     |     |     |     | a table        |
| `results$bootstrapValidationExplanation`    |     |     |     |     | a html         |
| `results$parametricModelComparison`         |     |     |     |     | a table        |
| `results$parametricModelSummary`            |     |     |     |     | a table        |
| `results$parametricDiagnostics`             |     |     |     |     | a html         |
| `results$parametricSurvivalPlot`            |     |     |     |     | an image       |
| `results$hazardFunctionPlot`                |     |     |     |     | an image       |
| `results$extrapolationPlot`                 |     |     |     |     | an image       |
| `results$extrapolationTable`                |     |     |     |     | a table        |
| `results$parametricModelsExplanation`       |     |     |     |     | a html         |
| `results$clinicalGlossaryExplanation`       |     |     |     |     | a html         |
| `results$clinicalInterpretationExplanation` |     |     |     |     | a html         |
| `results$copyReadySentencesExplanation`     |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$medianTable$asDF`

`as.data.frame(results$medianTable)`
