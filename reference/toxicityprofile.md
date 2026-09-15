# Treatment Toxicity Profile

Treatment Toxicity Profile

## Usage

``` r
toxicityprofile(
  data,
  patientID,
  adverseEvent,
  grade,
  treatment = NULL,
  systemOrganClass = NULL,
  timeToEvent = NULL,
  plotType = "stacked_bar",
  sortBy = "frequency",
  showHighGradeOnly = FALSE,
  minIncidence = 5,
  gradeColors = "ctcae",
  showPercentages = TRUE,
  showConfidenceIntervals = FALSE,
  groupComparison = FALSE,
  cumulativeIncidence = FALSE,
  confidenceLevel = "0.95"
)
```

## Arguments

- data:

  The data as a data frame.

- patientID:

  Variable containing patient identifiers.

- adverseEvent:

  Variable containing adverse event names or categories.

- grade:

  CTCAE grade or severity level (1-5 or equivalent scale).

- treatment:

  Treatment or study arm for comparison (optional).

- systemOrganClass:

  System organ class (SOC) for grouping adverse events.

- timeToEvent:

  Time from treatment start to adverse event occurrence.

- plotType:

  Primary visualization method for toxicity profile.

- sortBy:

  Method for ordering adverse events in the plot.

- showHighGradeOnly:

  Display only grade 3+ adverse events.

- minIncidence:

  Minimum incidence percentage for events to be displayed.

- gradeColors:

  Color scheme for toxicity grades.

- showPercentages:

  Display percentages on the plot.

- showConfidenceIntervals:

  Display confidence intervals for incidence rates.

- groupComparison:

  Perform statistical comparison between treatment groups.

- cumulativeIncidence:

  Display cumulative incidence over time (requires time variable).

- confidenceLevel:

  Confidence level for intervals and tests.

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$plot`              |     |     |     |     | an image |
| `results$summary`           |     |     |     |     | a table  |
| `results$gradeDistribution` |     |     |     |     | a table  |
| `results$groupComparison`   |     |     |     |     | a table  |
| `results$socSummary`        |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
