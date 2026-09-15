# Conditional Survival Estimation

Performs conditional survival estimation, which calculates the
probability of surviving beyond a specific time point given survival to
a conditioning time point. This analysis is particularly useful for
updating prognosis estimates for patients who have already survived a
certain period. Multiple estimation methods are supported including
Kaplan-Meier weights, landmark approaches, and inverse probability
weighting.

## Usage

``` r
conditionalsurvival(
  data,
  timeVar,
  outcomeVar,
  conditionVar,
  conditionTime = 0,
  method = "km",
  bandwidth = 0,
  confInt = 0.95,
  timePoints = "",
  plotType = "curves",
  showTable = TRUE,
  showPlot = TRUE,
  showExplanations = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- timeVar:

  Survival time variable (numeric)

- outcomeVar:

  Event indicator (0=censored, 1=event)

- conditionVar:

  Variable for conditional survival estimation

- conditionTime:

  Time point at which to condition survival (0 = use median follow-up)

- method:

  Method for conditional survival estimation

- bandwidth:

  Bandwidth for kernel smoothing (0 = auto-select)

- confInt:

  Confidence level for intervals

- timePoints:

  Comma-separated time points for conditional survival (e.g., 12,24,60)

- plotType:

  Type of plot to display

- showTable:

  Display conditional survival probabilities table

- showPlot:

  Display conditional survival plot

- showExplanations:

  Display interpretation and methodology explanations

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$condsurvTable`     |     |     |     |     | a table  |
| `results$survplot`          |     |     |     |     | an image |
| `results$methodExplanation` |     |     |     |     | a html   |
| `results$reportSentence`    |     |     |     |     | a html   |
| `results$assumptions`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$condsurvTable$asDF`

`as.data.frame(results$condsurvTable)`
