# Functional Sampling - Rare Event Analysis

Functional Sampling - Rare Event Analysis

## Usage

``` r
functionalsampling(
  data,
  x_coord,
  y_coord,
  event_type,
  rare_event = "Rare",
  frequency_threshold = 5,
  calculate_nnd = TRUE,
  test_randomness = TRUE,
  analyze_neighborhoods = TRUE,
  show_summary = TRUE,
  show_plot = TRUE,
  show_methodology = TRUE,
  show_references = FALSE,
  significance_level = 0.05
)
```

## Arguments

- data:

  .

- x_coord:

  X spatial coordinate of events/cells.

- y_coord:

  Y spatial coordinate of events/cells.

- event_type:

  Type of event or cell. Used to identify rare events vs majority
  population.

- rare_event:

  Label identifying the rare event/cell type in the event_type variable.

- frequency_threshold:

  Maximum frequency to consider an event as "rare" (default 5 percent).

- calculate_nnd:

  Calculate distances between each rare event and its nearest neighbor.

- test_randomness:

  Test if rare events are randomly distributed (Clark-Evans test).

- analyze_neighborhoods:

  Test if rare events have consistent distances to neighboring cells
  (catalyst function indicator).

- show_summary:

  .

- show_plot:

  .

- show_methodology:

  .

- show_references:

  .

- significance_level:

  .

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$summary`               |     |     |     |     | a table  |
| `results$nnd_table`             |     |     |     |     | a table  |
| `results$randomness_test`       |     |     |     |     | a table  |
| `results$functional_assessment` |     |     |     |     | a table  |
| `results$plot`                  |     |     |     |     | an image |
| `results$methodology`           |     |     |     |     | a html   |
| `results$references`            |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
