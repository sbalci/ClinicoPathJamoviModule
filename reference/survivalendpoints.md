# Survival Endpoint Derivation

Derives common survival endpoints (PFS, OS, TTP, DOR) from clinical
trial timeline data. Automatically calculates time-to-event and event
status variables for use in survival analysis. Designed for seamless
integration with swimmer/waterfall plot data.

## Usage

``` r
survivalendpoints(
  data,
  patientId,
  startDate,
  lastFollowup,
  progressionDate = NULL,
  deathDate = NULL,
  deathEvent = NULL,
  progressionEvent = NULL,
  treatmentEnd = NULL,
  responseDate = NULL,
  timeUnit = "months",
  inputType = "dates",
  calculatePFS = FALSE,
  calculateOS = FALSE,
  calculateTTP = FALSE,
  calculateDOR = FALSE,
  calculateTOT = FALSE,
  showDerivedData = FALSE,
  showSummaryStats = FALSE,
  showEventRates = FALSE,
  showKMPlot = FALSE,
  showMilestones = FALSE,
  milestones = "6,12,24",
  exportEndpoints = FALSE,
  showUsageGuide = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- patientId:

  Patient identifier variable

- startDate:

  Treatment start date or baseline time (Date or numeric)

- lastFollowup:

  Last follow-up date or time

- progressionDate:

  Date/time of disease progression (required for accurate PFS/TTP/DOR)

- deathDate:

  Date/time of death (required for accurate OS/PFS)

- deathEvent:

  Death event indicator (0=alive, 1=dead). Not needed if death date is
  provided.

- progressionEvent:

  Disease progression indicator (0=no progression, 1=progression). Not
  needed if progression date is provided.

- treatmentEnd:

  Treatment end date (optional, for time on treatment calculation)

- responseDate:

  Date of confirmed response (optional, for DOR calculation)

- timeUnit:

  Unit for time calculations (for numeric time variables)

- inputType:

  Type of input data (dates or numeric time values)

- calculatePFS:

  Calculate progression-free survival (time to progression or death)

- calculateOS:

  Calculate overall survival (time to death)

- calculateTTP:

  Calculate time to progression (censors death without progression)

- calculateDOR:

  Calculate duration of response (requires response date)

- calculateTOT:

  Calculate time on treatment (requires treatment end date)

- showDerivedData:

  Display table with all derived survival endpoints

- showSummaryStats:

  Display summary statistics for each endpoint

- showEventRates:

  Display event rates and censoring proportions

- showKMPlot:

  Display simple Kaplan-Meier curves for derived endpoints

- showMilestones:

  Calculate survival at specific time milestones

- milestones:

  Comma-separated milestone times (in selected time unit)

- exportEndpoints:

  Export derived endpoints to CSV for use in survival modules

- showUsageGuide:

  Display guide for using derived endpoints in survival analysis

## Value

A results object containing:

|                            |     |     |     |     |                |
|----------------------------|-----|-----|-----|-----|----------------|
| `results$notices`          |     |     |     |     | a preformatted |
| `results$instructions`     |     |     |     |     | a html         |
| `results$dataWarning`      |     |     |     |     | a html         |
| `results$dataInfo`         |     |     |     |     | a table        |
| `results$derivedEndpoints` |     |     |     |     | a table        |
| `results$summaryStats`     |     |     |     |     | a table        |
| `results$eventRates`       |     |     |     |     | a table        |
| `results$milestoneTable`   |     |     |     |     | a table        |
| `results$kmPlot`           |     |     |     |     | an image       |
| `results$usageGuide`       |     |     |     |     | a html         |
| `results$exportMessage`    |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
