# Single Arm Survival

Performs survival analysis for a single cohort without group
comparisons. Kaplan-Meier estimates use event times and risk sets; in
competing-risk mode, cumulative incidence retains competing terminal
events as separate states. Optional person-time rates use the sum of
individual observation periods as their denominator. This is descriptive
analysis of one cohort, not a treatment-effect estimate.

## Usage

``` r
singlearm(
  data,
  elapsedtime = NULL,
  tint = FALSE,
  dxdate = NULL,
  fudate = NULL,
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

  The time-to-event or follow-up duration for each patient. The sum is
  the denominator when person-time rates are requested; Kaplan-Meier
  estimates instead use ordered event times and risk sets. Values must
  be finite and zero or positive, in one consistent unit.

- tint:

  Enable this option if you want to calculate survival time from dates
  in your data. This is useful when you have separate columns for
  diagnosis date and follow-up date and want to calculate the time
  elapsed between them.

- dxdate:

  The date of diagnosis or study entry. Accepts: (1) Date/datetime text
  (e.g., "2024-01-15"), (2) Numeric values. Each bare numeric date
  column is classified by its overall magnitude: values below 100000
  indicate DAYS since 1970-01-01 (the R Date encoding), whereas values
  at or above 100000 indicate Unix epoch SECONDS (the DateTime
  Converter's corrected_datetime_numeric output). Columns with
  inconsistent scales or values on both sides of this boundary are
  rejected. Time intervals are calculated as the difference from the
  follow-up date.

- fudate:

  The date of last follow-up or event. Accepts: (1) Date/datetime text
  (e.g., "2024-01-15"), (2) Numeric values, classified by column
  magnitude exactly as for the diagnosis date (below 100000 = days since
  1970-01-01; at or above 100000 = Unix epoch seconds). Mixed values
  within a column and inconsistent numeric encodings between columns are
  rejected. Must be in the same format as the diagnosis date.

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

  Defines how the four mapped categories are coded. Overall counts both
  death categories as events. Cause Specific counts Dead of Disease as
  the event and treats other categories as censored; its Kaplan-Meier
  probability is net/cause-specific survival and can overstate
  real-world absolute risk when other-cause death competes. Disease-Free
  counts death and Alive with Disease as events; the supplied time for
  Alive with Disease must be time to recurrence/progression, not last
  follow-up. Competing Risk estimates cumulative incidence of Dead of
  Disease while retaining Dead of Other Causes as a competing terminal
  event.

- cutp:

  Time points at which to report survival probabilities, as a
  comma-separated list in the selected Time Unit. Values are always used
  exactly as entered. The built-in text "12, 36, 60" is written in
  months, so under a different unit it means 12, 36 and 60 of that unit;
  an information notice explains how to enter 1, 3 and 5 years instead.
  Points beyond follow-up are omitted unless every remaining subject has
  had a terminal event, in which case the final KM/CIF state is carried
  forward. Negative, non-finite, and non-numeric entries are ignored
  with a warning. Time zero is accepted because events can occur at the
  origin.

- timetypedata:

  select the time type in data (e.g., YYYY-MM-DD)

- timetypeoutput:

  The time unit used throughout the analysis. When survival time is
  calculated from dates, dates are CONVERTED to this unit. When a
  pre-calculated elapsed-time variable is supplied, no conversion is
  possible (the column carries no unit), so this option DECLARES the
  unit that variable is already recorded in. Either way it determines
  how cutpoints and plausibility checks are interpreted, and how axes
  are labelled - so selecting the wrong unit for pre-calculated time
  changes the reported results, not just the labels.

- uselandmark:

  Performs a conditional landmark description: only subjects still
  event-free and under observation after the landmark are retained, and
  their time scale is reset there. This changes the target population
  and does not by itself remove immortal-time bias, estimate a treatment
  effect, or justify excluding early events.

- landmark:

  The landmark time point, in the selected Time Unit. Must be zero or
  positive and must fall inside the observed follow-up range. Subjects
  whose follow-up ended at or before the landmark are excluded, and time
  is then measured from the landmark, so all estimates are conditional
  on surviving to it.

- sc:

  Generate a Kaplan-Meier survival plot in standard analyses or an
  Aalen-Johansen cumulative-incidence plot in competing-risk analyses.
  Confidence intervals are shown only when the separate 95 percent CI
  option is selected.

- kmunicate:

  Enable this option to generate a publication-ready survival plot in
  the style of KMunicate. The 95 percent CI and Risk table options
  control its pointwise interval ribbon and risk/censoring panel.
  Individual censoring marks and median reference lines are not
  supported on this plot.

- ce:

  Plot the cumulative probability of the event over time, 1 - S(t),
  estimated by Kaplan-Meier. This is a probability on a 0-1 scale, not a
  running count of events: a raw count ignores censoring and is not
  comparable between cohorts of different size or follow-up. Read it as
  "the estimated proportion of the cohort that had had the event by time
  t".

- ch:

  Plot cumulative hazard over time. This is an accumulated rate-scale
  quantity, not an event probability, and it is not bounded by 1. Use
  cumulative event probability for the Kaplan-Meier estimate 1 - S(t).

- endplot:

  The maximum time point to include in the survival plots. This is the
  end time for the survival curves and cumulative event/hazard plots, in
  the selected Time Unit. Must be greater than zero; zero or a negative
  value is rejected rather than drawn.

- ybegin_plot:

  The minimum value for the probability y-axis in the Kaplan-Meier,
  cumulative-incidence, and cumulative-event plots. It must be between 0
  and 1 and below the end value. Cumulative hazard is auto-scaled and
  the KMunicate-style plot manages its own y-axis.

- yend_plot:

  The maximum value for the probability y-axis in the Kaplan-Meier,
  cumulative-incidence, and cumulative-event plots. It must be between 0
  and 1 and above the start value. Cumulative hazard is auto-scaled and
  the KMunicate-style plot manages its own y-axis.

- byplot:

  The spacing between tick marks on the time axis. Must be greater than
  zero; zero or a negative value is rejected rather than drawn.

- multievent:

  Enable this option to perform survival analysis for datasets with
  multiple event levels. This is useful for competing risk analysis when
  there are multiple event types (e.g., death due to disease, death due
  to other causes).

- ci95:

  Display 95 percent confidence intervals for the plotted estimand:
  survival in a standard analysis or cumulative incidence in a
  competing-risk analysis.

- risktable:

  Display the number of subjects still at risk below supported
  Kaplan-Meier plots. This is a count, not a table of probabilities or
  hazards. A combined risk panel is not available for the competing-risk
  CIF plot; use the cumulative-incidence table for counts at selected
  times.

- censored:

  Enable this option to display censored observations on the survival
  plots. Censored observations have not experienced the modeled event by
  their last observed time; this may reflect administrative censoring,
  withdrawal, or loss to follow-up. They are indicated by ticks on
  supported Kaplan-Meier curves. This display option is not available on
  the competing-risk CIF plot.

- medianline:

  Display a horizontal and/or vertical reference line at the
  Kaplan-Meier median, when estimable. On the cumulative-hazard plot the
  horizontal reference is log(2), corresponding to S(t) = 0.5. Median
  reference lines are not drawn on the competing-risk CIF or
  KMunicate-style plots.

- person_time:

  Enable this option to calculate and display person-time metrics,
  including total follow-up time and crude occurrence/exposure rates. A
  person-time rate is events divided by observed time at risk; it is not
  an event probability. With competing risks it is a crude
  cause-specific rate for the target event, not the cumulative incidence
  or absolute risk.

- time_intervals:

  Time intervals for stratified person-time analysis, as a
  comma-separated list in the selected Time Unit. For example "12, 36,
  60" creates the intervals 0-12, 12-36, 36-60 and 60+. Values are
  always interpreted in the selected Time Unit; the built-in text is 12,
  36 and 60 of that unit and is not silently rescaled. A zero boundary
  is silently treated as the origin because the analysis already starts
  at zero. Negative, non-finite, or non-numeric boundaries are ignored
  with a warning. Boundaries at or beyond the longest observed follow-up
  are also omitted because person-time cannot accrue before the start of
  observation or after the last one.

- rate_multiplier:

  Specify the multiplier for incidence rates (e.g., 100 for rates per
  100 units of person-time in the selected Time Unit, or 1000 for rates
  per 1000). It is the scale the rates are expressed on, so it must be
  greater than zero; a negative or zero value is rejected and the
  person-time analysis is not performed.

- baseline_hazard:

  Estimate exploratory interval event rates as events divided by exact
  person-time in equal-width intervals. The number of intervals is
  limited according to the total event count to reduce sparsity. These
  are piecewise occurrence/exposure rates, not exact instantaneous
  hazards or Cox-model coefficients, and should not be used alone to
  choose treatment or surveillance timing. The rate output is not
  estimated when an event occurs at time zero, because such an event is
  a probability mass at the origin rather than a finite continuous
  hazard.

- hazard_smoothing:

  Smooth the equal-width interval rates with a person-time-weighted,
  local-constant LOESS curve. This is an exploratory,
  bandwidth-dependent trend, not an exact instantaneous hazard. At least
  three usable automatic intervals are required; otherwise the plot
  explains why no curve was estimated. There is no proportional-hazards
  assumption to assess in a single-arm analysis. No curve is estimated
  with zero observed events or an event at time zero.

- showExplanations:

  Display detailed explanations for each analysis component to help
  interpret the statistical methods and results.

- showSummaries:

  Display natural language summaries alongside tables and plots. These
  summaries provide plain-language interpretations of the statistical
  results. Turn off to reduce visual clutter when summaries are not
  needed.

- advancedDiagnostics:

  Report cohort size, event count, observed event proportion, follow-up
  summaries, raw-variable completeness, and memory footprint. These are
  descriptive checks, not a validated risk-of-bias assessment; they
  cannot verify non-informative censoring, representativeness, or
  adequacy for a particular clinical decision.

## Value

A results object containing:

|                                          |     |     |     |     |                |
|------------------------------------------|-----|-----|-----|-----|----------------|
| `results$eventRecodeInfo`                |     |     |     |     | a html         |
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
| `results$plot_cif`                       |     |     |     |     | an image       |
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
