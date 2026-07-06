# DateTime Converter

Convert datetime variables to standardized format and extract datetime
components (year, month, day, hour, minute, day name, week number,
quarter, etc.). Features automatic format detection, quality assessment,
and preview of converted data. Perfect for preparing datetime data for
analysis and creating time-based variables.

## Usage

``` r
datetimeconverter(
  data,
  datetime_var,
  datetime_format = "auto",
  timezone = "system",
  preview_rows = 20,
  extract_year = FALSE,
  extract_month = FALSE,
  extract_monthname = FALSE,
  extract_day = FALSE,
  extract_hour = FALSE,
  extract_minute = FALSE,
  extract_second = FALSE,
  extract_dayname = FALSE,
  extract_weeknum = FALSE,
  extract_quarter = FALSE,
  extract_dayofyear = FALSE,
  show_quality_metrics = FALSE,
  show_summary = FALSE,
  show_explanations = FALSE,
  show_glossary = FALSE
)
```

## Arguments

- data:

  The data as a data frame containing datetime variable to convert.

- datetime_var:

  Variable containing datetime information in various formats. Can
  handle character strings, numeric values, or factors with datetime
  representations.

- datetime_format:

  DateTime format specification. 'Auto-detect' attempts to identify the
  format automatically. Manual selection ensures accurate parsing for
  specific datetime formats.

- timezone:

  Timezone for datetime parsing. Provide an Olson identifier such as
  "Europe/Istanbul", "America/New_York", "UTC", or "system" to use the
  local machine timezone. Excel serial and Unix epoch conversions always
  use UTC regardless of this setting.

- preview_rows:

  Number of rows to display in preview tables. Shows original values,
  corrected datetimes, and extracted components for quality checking.

- extract_year:

  Extract and display year component in preview table.

- extract_month:

  Extract and display month component in preview table.

- extract_monthname:

  Extract and display month name in preview table.

- extract_day:

  Extract and display day component in preview table.

- extract_hour:

  Extract and display hour component in preview table.

- extract_minute:

  Extract and display minute component in preview table.

- extract_second:

  Extract and display second component in preview table.

- extract_dayname:

  Extract and display day name in preview table.

- extract_weeknum:

  Extract and display week number in preview table.

- extract_quarter:

  Extract and display quarter in preview table.

- extract_dayofyear:

  Extract and display day of year in preview table.

- show_quality_metrics:

  Display comprehensive quality metrics including parsing success rate,
  invalid dates, and data quality warnings.

- show_summary:

  Generate a plain-language summary suitable for copying into reports or
  clinical documentation.

- show_explanations:

  Display educational content explaining what this function does, when
  to use it, and how to interpret results.

- show_glossary:

  Display definitions of datetime-related terms (Excel serial dates,
  Unix epoch, timezones, etc.) to aid interpretation.

## Value

A results object containing:

|                                      |     |     |     |     |           |
|--------------------------------------|-----|-----|-----|-----|-----------|
| `results$notices`                    |     |     |     |     | a html    |
| `results$welcome`                    |     |     |     |     | a html    |
| `results$formatInfo`                 |     |     |     |     | a html    |
| `results$qualityMetrics`             |     |     |     |     | a html    |
| `results$previewTable`               |     |     |     |     | a html    |
| `results$componentPreview`           |     |     |     |     | a html    |
| `results$qualityAssessment`          |     |     |     |     | a html    |
| `results$nlSummary`                  |     |     |     |     | a html    |
| `results$aboutPanel`                 |     |     |     |     | a html    |
| `results$caveatsPanel`               |     |     |     |     | a html    |
| `results$glossaryPanel`              |     |     |     |     | a html    |
| `results$corrected_datetime_char`    |     |     |     |     | an output |
| `results$corrected_datetime_numeric` |     |     |     |     | an output |
| `results$year_out`                   |     |     |     |     | an output |
| `results$month_out`                  |     |     |     |     | an output |
| `results$monthname_out`              |     |     |     |     | an output |
| `results$day_out`                    |     |     |     |     | an output |
| `results$hour_out`                   |     |     |     |     | an output |
| `results$minute_out`                 |     |     |     |     | an output |
| `results$second_out`                 |     |     |     |     | an output |
| `results$dayname_out`                |     |     |     |     | an output |
| `results$weeknum_out`                |     |     |     |     | an output |
| `results$quarter_out`                |     |     |     |     | an output |
| `results$dayofyear_out`              |     |     |     |     | an output |
