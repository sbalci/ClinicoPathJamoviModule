# Outcome Organizer for Survival Analysis

Advanced tool for preparing outcome variables for various types of
survival analysis including overall survival, cause-specific, competing
risks, progression-free survival, and multistate models.

## Usage

``` r
outcomeorganizer(
  data,
  outcome,
  outcomeLevel,
  recurrence,
  recurrenceLevel,
  patientID,
  analysistype = "os",
  multievent = FALSE,
  dod,
  dooc,
  awd,
  awod,
  useHierarchy = FALSE,
  eventPriority = 1,
  intervalCensoring = FALSE,
  intervalStart,
  intervalEnd,
  adminCensoring = FALSE,
  adminDate,
  outputTable = FALSE,
  diagnostics = FALSE,
  visualization = FALSE,
  showNaturalSummary = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  The primary outcome variable to be recoded for survival analysis
  (e.g., vital status).

- outcomeLevel:

  The level of the outcome variable that represents the event of
  interest (e.g., "Death", "Relapse").

- recurrence:

  Variable indicating disease recurrence or progression (for RFS/PFS/DFS
  analyses).

- recurrenceLevel:

  The level indicating recurrence or progression has occurred.

- patientID:

  Patient identifier for handling multiple records or applying event
  hierarchies.

- analysistype:

  The type of survival analysis to prepare the outcome for.

- multievent:

  If true, allows for multiple event types (e.g., death from disease vs
  death from other causes).

- dod:

  The level representing death from the disease of interest.

- dooc:

  The level representing death from causes other than the disease of
  interest.

- awd:

  The level representing patients who are alive but have the disease.

- awod:

  The level representing patients who are alive and disease-free.

- useHierarchy:

  If true, applies a hierarchy when multiple events occur for the same
  patient.

- eventPriority:

  The event code (e.g., 1, 2) that takes precedence when multiple events
  occur.

- intervalCensoring:

  If true, prepares data for interval-censored analysis where exact
  event times are unknown.

- intervalStart:

  Variable containing the start of the interval when the event might
  have occurred.

- intervalEnd:

  Variable containing the end of the interval when the event might have
  occurred.

- adminCensoring:

  If true, applies administrative censoring at a specified date.

- adminDate:

  Variable containing the administrative censoring date.

- outputTable:

  If true, displays a table showing the frequency of each recoded
  outcome value.

- diagnostics:

  If true, displays diagnostic information about the recoding process.

- visualization:

  If true, displays a visualization of the distribution of recoded
  outcomes.

- showNaturalSummary:

  Display a plain-language summary of the recoding suitable for copying
  to reports.

- showGlossary:

  Display definitions of survival analysis terms.

## Value

A results object containing:

|                            |     |     |     |     |           |
|----------------------------|-----|-----|-----|-----|-----------|
| `results$todo`             |     |     |     |     | a html    |
| `results$errors`           |     |     |     |     | a html    |
| `results$strongWarnings`   |     |     |     |     | a html    |
| `results$warnings`         |     |     |     |     | a html    |
| `results$infoMessages`     |     |     |     |     | a html    |
| `results$summary`          |     |     |     |     | a html    |
| `results$outputTable`      |     |     |     |     | a table   |
| `results$diagnosticsTable` |     |     |     |     | a table   |
| `results$outcomeViz`       |     |     |     |     | an image  |
| `results$naturalSummary`   |     |     |     |     | a html    |
| `results$glossary`         |     |     |     |     | a html    |
| `results$addOutcome`       |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$outputTable$asDF`

`as.data.frame(results$outputTable)`
