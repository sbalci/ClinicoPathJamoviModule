# Single Arm Survival

Performs survival analysis for a single cohort of patients without group
comparisons. The analysis calculates total person-time follow-up (the
sum of all individual observation periods) and uses this to derive
accurate survival estimates that account for varying follow-up
durations. Use this when you want to analyze overall survival
characteristics of your entire study population - for example, to
determine median survival time or 1/3/5-year survival rates for all
patients collectively.

## Usage

``` r
singlearm(
  data,
  elapsedtime,
  tint = FALSE,
  dxdate,
  fudate,
  outcome,
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
  multievent = FALSE,
  ci95 = FALSE,
  risktable = FALSE,
  censored = FALSE,
  medianline = "none",
  person_time = FALSE,
  time_intervals = "12, 36, 60",
  rate_multiplier = 100,
  baseline_hazard = FALSE,
  hazard_smoothing = FALSE,
  showExplanations = FALSE,
  showSummaries = FALSE,
  advancedDiagnostics = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  The time-to-event or follow-up duration for each patient. The sum of
  these values represents the total person-time follow-up in the study,
  which serves as the denominator for calculating event rates and is
  fundamental for Kaplan-Meier estimates. Should be numeric and
  continuous, measured in consistent units (e.g., months or years).

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

- outcome:

  The outcome or event of interest for each patient. Should be a factor
  or numeric variable indicating whether the patient experienced the
  event (e.g., death) or censoring (e.g., end of follow-up).

- outcomeLevel:

  Select the level of the outcome variable that represents the event of
  interest. For example, if the outcome variable is "death_status" with
  levels "Alive" and "Dead", select "Dead" as the event level.

- dod:

  Select the level of the outcome variable that represents death due to
  disease. This is useful for competing risk analysis when there are
  multiple event types.

- dooc:

  Select the level of the outcome variable that represents death due to
  other causes. This is useful for competing risk analysis when there
  are multiple event types.

- awd:

  Select the level of the outcome variable that represents being alive
  with disease. This is useful for competing risk analysis when there
  are multiple event types.

- awod:

  Select the level of the outcome variable that represents being alive
  without disease. This is useful for competing risk analysis when there
  are multiple event types.

- analysistype:

  Select the type of survival analysis to perform. "Overall" analyzes
  the survival of all patients regardless of event type. "Cause
  Specific" analyzes the survival for a specific event type (e.g., death
  due to disease). "Competing Risk" analyzes the survival for multiple
  event types simultaneously.

- cutp:

  Specify the time points at which to calculate survival probabilities.
  Enter a comma-separated list of time points in consistent units (e.g.,
  months or years). For example, "12, 36, 60" calculates survival
  probabilities at 1, 3, and 5 years.

- timetypedata:

  select the time type in data (e.g., YYYY-MM-DD)

- timetypeoutput:

  Select the time unit for displaying survival results. Months is most
  common in clinical oncology studies. Choose the unit that best matches
  your clinical practice and makes results most interpretable for your
  audience.

- uselandmark:

  Enables landmark analysis, which addresses immortal time bias by
  analyzing survival only for patients who survive to a specified
  timepoint (the landmark). Use this when you want to eliminate the
  effect of early deaths or when comparing treatments that can only be
  given to patients who survive long enough to receive them.

- landmark:

  Enables landmark analysis, which addresses immortal time bias by
  analyzing survival only for patients who survive to a specified
  timepoint (the landmark). Use this when you want to eliminate the
  effect of early deaths or when comparing treatments that can only be
  given to patients who survive long enough to receive them.

- sc:

  Enable this option to generate a Kaplan-Meier survival plot with
  confidence intervals. This plot shows the estimated survival
  probability over time and is useful for visualizing survival trends in
  your data.

- kmunicate:

  Enable this option to generate a publication-ready survival plot in
  the style of KMunicate. This plot shows the estimated survival
  probability over time with confidence intervals and is suitable for
  publication or presentation.

- ce:

  Enable this option to calculate and plot the cumulative number of
  events over time. This plot shows the total number of events (e.g.,
  deaths) that have occurred at each time point and is useful for
  visualizing event rates in your data.

- ch:

  Enable this option to calculate and plot the cumulative hazard
  function over time. This plot shows the cumulative risk of
  experiencing the event (e.g., death) at each time point and is useful
  for visualizing the risk of the event over time.

- endplot:

  The maximum time point to include in the survival plots. This is the
  end time for the survival curves and cumulative event/hazard plots.
  Enter a positive integer representing the time in consistent units
  (e.g., months or years).

- ybegin_plot:

  The minimum value for the y-axis in the survival plots. Enter a number
  between 0 and 1 to set the lower limit of the y-axis.

- yend_plot:

  The maximum value for the y-axis in the survival plots. Enter a number
  between 0 and 1 to set the upper limit of the y-axis.

- byplot:

  The interval for plotting survival probabilities. Enter a positive
  integer representing the time interval in consistent units (e.g.,
  months or years).

- multievent:

  Enable this option to perform survival analysis for datasets with
  multiple event levels. This is useful for competing risk analysis when
  there are multiple event types (e.g., death due to disease, death due
  to other causes).

- ci95:

  Enable this option to display 95 percent confidence intervals on the
  survival plots. These intervals show the range of uncertainty around
  the estimated survival probabilities and are useful for assessing the
  precision of the estimates.

- risktable:

  Enable this option to display a table of risk estimates at each time
  point. This table shows the estimated survival probability, cumulative
  event rate, and cumulative hazard at each time point and is useful for
  summarizing the survival characteristics of your data.

- censored:

  Enable this option to display censored observations on the survival
  plots. Censored observations are patients who have not experienced the
  event of interest by the end of follow-up and are indicated by
  vertical ticks on the survival curves.

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

- baseline_hazard:

  Estimate and plot the baseline hazard function to assess how the
  instantaneous risk of events changes over time in your study
  population.

- hazard_smoothing:

  Generate smoothed hazard rate estimates to better visualize patterns
  in event risk over time and assess proportional hazards assumptions.

- showExplanations:

  Display detailed explanations for each analysis component to help
  interpret the statistical methods and results.

- showSummaries:

  Display natural language summaries alongside tables and plots. These
  summaries provide plain-language interpretations of the statistical
  results. Turn off to reduce visual clutter when summaries are not
  needed.

- advancedDiagnostics:

  Enable advanced diagnostic features including enhanced data quality
  assessment, performance optimizations with caching, and improved error
  reporting. This provides additional insights into data reliability and
  analysis quality without affecting core results.

## Value

A results object containing:

|                                          |     |     |     |     |                |
|------------------------------------------|-----|-----|-----|-----|----------------|
| `results$todo`                           |     |     |     |     | a html         |
| `results$errors`                         |     |     |     |     | a html         |
| `results$warnings`                       |     |     |     |     | a html         |
| `results$info`                           |     |     |     |     | a html         |
| `results$medianHeading`                  |     |     |     |     | a preformatted |
| `results$medianTable`                    |     |     |     |     | a table        |
| `results$clinicalSummary`                |     |     |     |     | a html         |
| `results$medianSummary`                  |     |     |     |     | a preformatted |
| `results$medianHeading3`                 |     |     |     |     | a preformatted |
| `results$medianSurvivalExplanation`      |     |     |     |     | a html         |
| `results$survTableHeading`               |     |     |     |     | a preformatted |
| `results$survTable`                      |     |     |     |     | a table        |
| `results$survTableSummary`               |     |     |     |     | a preformatted |
| `results$survTableHeading3`              |     |     |     |     | a preformatted |
| `results$survivalProbabilityExplanation` |     |     |     |     | a html         |
| `results$personTimeHeading`              |     |     |     |     | a preformatted |
| `results$personTimeTable`                |     |     |     |     | a table        |
| `results$personTimeHeading2`             |     |     |     |     | a preformatted |
| `results$personTimeSummary`              |     |     |     |     | a html         |
| `results$personTimeHeading3`             |     |     |     |     | a preformatted |
| `results$personTimeExplanation`          |     |     |     |     | a html         |
| `results$plot`                           |     |     |     |     | an image       |
| `results$plot6`                          |     |     |     |     | an image       |
| `results$plot2`                          |     |     |     |     | an image       |
| `results$plot3`                          |     |     |     |     | an image       |
| `results$survivalPlotsHeading3`          |     |     |     |     | a preformatted |
| `results$survivalPlotsExplanation`       |     |     |     |     | a html         |
| `results$baselineHazardHeading`          |     |     |     |     | a preformatted |
| `results$baselineHazardTable`            |     |     |     |     | a table        |
| `results$baselineHazardPlot`             |     |     |     |     | an image       |
| `results$smoothedHazardPlot`             |     |     |     |     | an image       |
| `results$baselineHazardSummary`          |     |     |     |     | a html         |
| `results$baselineHazardHeading3`         |     |     |     |     | a preformatted |
| `results$baselineHazardExplanation`      |     |     |     |     | a html         |
| `results$dataQualityHeading`             |     |     |     |     | a preformatted |
| `results$dataQualityTable`               |     |     |     |     | a table        |
| `results$dataQualitySummary`             |     |     |     |     | a html         |
| `results$calculatedtime`                 |     |     |     |     | an output      |
| `results$outcomeredefined`               |     |     |     |     | an output      |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$medianTable$asDF`

`as.data.frame(results$medianTable)`
