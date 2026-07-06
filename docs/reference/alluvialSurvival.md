# Treatment Pathway Alluvial Plot

Treatment Pathway Alluvial Plot

## Usage

``` r
alluvialSurvival(
  data,
  timeVar,
  stageVar,
  treatmentVar,
  patientId,
  survivalVar,
  showRightAxis = FALSE,
  colorScheme = "clinical",
  showSurvival = FALSE
)
```

## Arguments

- data:

  .

- timeVar:

  The name of the time variable in the dataset

- stageVar:

  The name of the disease stage variable

- treatmentVar:

  Treatment received at each time point

- patientId:

  Unique patient identifier for tracking

- survivalVar:

  Survival status at each time point

- showRightAxis:

  .

- colorScheme:

  .

- showSurvival:

  .

## Value

A results object containing:

|                         |     |     |     |     |                |
|-------------------------|-----|-----|-----|-----|----------------|
| `results$todo`          |     |     |     |     | a html         |
| `results$mydataview`    |     |     |     |     | a preformatted |
| `results$plot`          |     |     |     |     | an image       |
| `results$summaryTable`  |     |     |     |     | a table        |
| `results$survivalStats` |     |     |     |     | a table        |
| `results$survivalPlot`  |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`
