# Comprehensive Time Interval Calculator

Advanced time interval calculator designed for survival analysis,
epidemiological studies, and person-time analysis. Features intelligent
date parsing, comprehensive data quality assessment, landmark analysis,
and robust statistical summaries. Time intervals form the foundation of
person-time follow-up calculations, capturing both participant counts
and observation duration for accurate incidence rate calculations.

## Usage

``` r
timeinterval(
  data,
  dx_date,
  fu_date,
  time_format = "auto",
  output_unit = "months",
  time_basis = "standardized",
  use_landmark = FALSE,
  landmark_time = 6,
  remove_negative = FALSE,
  remove_extreme = FALSE,
  extreme_multiplier = 2,
  add_times = FALSE,
  include_quality_metrics = FALSE,
  confidence_level = 95,
  show_summary = FALSE,
  show_glossary = FALSE,
  timezone = "system"
)
```

## Arguments

- data:

  The data as a data frame containing date columns for interval
  calculation.

- dx_date:

  Column containing start dates (e.g., diagnosis date, study entry,
  treatment start). Supports various date formats including text and
  numeric representations.

- fu_date:

  Column containing end dates (e.g., follow-up date, event date, study
  exit). Must be in the same format as the start date variable.

- time_format:

  Date format specification. 'Auto-detect' attempts to identify the
  format automatically. Manual selection ensures accurate parsing for
  specific date formats.

- output_unit:

  Unit for calculated time intervals. Affects person-time calculations
  and statistical summaries. Choose based on study duration and event
  frequency.

- time_basis:

  Controls how months/years are computed. Standardized uses fixed
  lengths (30.44 days per month, 365.25 days per year) suited for
  person-time denominators. Calendar-aware respects actual month lengths
  (28

  - 31 days) when converting intervals to months/years.

- use_landmark:

  Enables conditional analysis from a specific time point. Useful for
  studying outcomes after a landmark time (e.g., 6-month survivors
  only).

- landmark_time:

  Time point for landmark analysis in the specified output units. Only
  participants surviving past this time are included in analysis.

- remove_negative:

  Automatically exclude negative time intervals (end date before start
  date). Recommended for data quality assurance.

- remove_extreme:

  Identify and flag potentially extreme time intervals for quality
  review. Uses statistical outlier detection methods.

- extreme_multiplier:

  Multiplier for 99th percentile to define extreme values. Default 2.0
  means values \>2× the 99th percentile are flagged. Higher values are
  more conservative (fewer flagged values).

- add_times:

  Appends calculated time intervals as a new variable for downstream
  analysis. Useful for subsequent survival analysis or person-time
  calculations.

- include_quality_metrics:

  Provides comprehensive data quality assessment including missing
  values, negative intervals, and distribution statistics.

- confidence_level:

  Confidence level for statistical intervals (mean confidence
  intervals). Standard epidemiological practice uses 95 percent
  confidence intervals.

- show_summary:

  Generate a plain-language interpretation of results suitable for
  copying into reports or clinical notes.

- show_glossary:

  Display definitions of key terms (person-time, incidence rate,
  landmark analysis, etc.) to help interpret results.

- timezone:

  Timezone for datetime parsing. 'System Default' uses your computer's
  timezone. 'UTC' interprets datetimes as Coordinated Universal Time.
  Ensures consistent time interval calculations across different systems
  and time zones.

## Value

A results object containing:

|                             |     |     |     |     |           |
|-----------------------------|-----|-----|-----|-----|-----------|
| `results$messages`          |     |     |     |     | a html    |
| `results$todo`              |     |     |     |     | a html    |
| `results$aboutPanel`        |     |     |     |     | a html    |
| `results$personTimeInfo`    |     |     |     |     | a html    |
| `results$qualityAssessment` |     |     |     |     | a html    |
| `results$caveatsPanel`      |     |     |     |     | a html    |
| `results$summary`           |     |     |     |     | a html    |
| `results$nlSummary`         |     |     |     |     | a html    |
| `results$glossaryPanel`     |     |     |     |     | a html    |
| `results$calculated_time`   |     |     |     |     | an output |
