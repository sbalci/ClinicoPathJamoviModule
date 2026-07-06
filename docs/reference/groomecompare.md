# Groome Staging System Comparison

Compare two staging systems using Groome criteria (Groome et al., 2001).
Calculates four key metrics: hazard consistency, hazard discrimination,
sample balance, and outcome prediction. Provides overall rank to
determine which staging system performs better. Includes C-index
comparison, radar charts, and side-by-side Kaplan-Meier curves.

## Usage

``` r
groomecompare(
  data,
  time,
  event,
  stage1,
  stage2,
  eventValue = "1",
  stage1name = "Staging System 1",
  stage2name = "Staging System 2",
  radarplot = TRUE,
  barplot = FALSE,
  kmplots = TRUE,
  detailedmetrics = TRUE,
  hazardratios = TRUE,
  samplesize = TRUE,
  cindexcompare = TRUE,
  bootstrap = FALSE,
  nboot = 1000,
  seed = 12345
)
```

## Arguments

- data:

  .

- time:

  a (non-negative valued) vector of survival times

- event:

  the status indicator (0=censored, 1=event)

- stage1:

  first staging system to compare (e.g., ypTNM stage)

- stage2:

  second staging system to compare (e.g., RPA stage)

- eventValue:

  the value representing an event

- stage1name:

  descriptive name for first staging system

- stage2name:

  descriptive name for second staging system

- radarplot:

  .

- barplot:

  .

- kmplots:

  .

- detailedmetrics:

  .

- hazardratios:

  .

- samplesize:

  .

- cindexcompare:

  .

- bootstrap:

  .

- nboot:

  .

- seed:

  .

## Value

A results object containing:

|                                          |     |     |     |     |          |
|------------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`                   |     |     |     |     | a html   |
| `results$summary`                        |     |     |     |     | a table  |
| `results$detailedmetrics$consistency`    |     |     |     |     | a table  |
| `results$detailedmetrics$discrimination` |     |     |     |     | a table  |
| `results$hazardratios$hrs1`              |     |     |     |     | a table  |
| `results$hazardratios$hrs2`              |     |     |     |     | a table  |
| `results$samplesize`                     |     |     |     |     | a table  |
| `results$cindexcompare`                  |     |     |     |     | a table  |
| `results$radarplot`                      |     |     |     |     | an image |
| `results$barplot`                        |     |     |     |     | an image |
| `results$kmplot1`                        |     |     |     |     | an image |
| `results$kmplot2`                        |     |     |     |     | an image |
| `results$bootstrap`                      |     |     |     |     | a table  |
| `results$notices`                        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
