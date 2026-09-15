# Latent Biomarker Construct + Cox Regression

Estimate a single reflective latent biomarker construct from 3+
indicators and relate it to a right-censored survival outcome via Cox
regression. Auto-selects MLR (continuous) or WLSMV (ordinal/binary)
estimation. Refuses inappropriate inputs with explanatory messages.

## Usage

``` r
latentbiomarker(
  data,
  dep_time = NULL,
  dep_event = NULL,
  event_level,
  indicators = NULL,
  adjusters = NULL,
  indicator_types = "auto",
  reflective_confirmed = FALSE,
  factor_score_method = "regression",
  standardize_scores = TRUE,
  factor_score_name = "biomarker_factor",
  km_strata = "tertile",
  show_plot_km = TRUE,
  show_plot_loadings = TRUE,
  show_plot_path = TRUE,
  show_diagnostics = TRUE,
  show_r_code = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- dep_time:

  .

- dep_event:

  .

- event_level:

  .

- indicators:

  .

- adjusters:

  .

- indicator_types:

  .

- reflective_confirmed:

  .

- factor_score_method:

  .

- standardize_scores:

  .

- factor_score_name:

  .

- km_strata:

  .

- show_plot_km:

  .

- show_plot_loadings:

  .

- show_plot_path:

  .

- show_diagnostics:

  .

- show_r_code:

  .

## Value

A results object containing:

|                              |     |     |     |     |                |
|------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`            |     |     |     |     | a html         |
| `results$summaryTable`       |     |     |     |     | a table        |
| `results$loadingsTable`      |     |     |     |     | a table        |
| `results$reliabilityTable`   |     |     |     |     | a table        |
| `results$fitTable`           |     |     |     |     | a table        |
| `results$coxTable`           |     |     |     |     | a table        |
| `results$phTable`            |     |     |     |     | a table        |
| `results$kmPlot`             |     |     |     |     | an image       |
| `results$loadingsPlot`       |     |     |     |     | an image       |
| `results$pathPlot`           |     |     |     |     | an image       |
| `results$miTable`            |     |     |     |     | a table        |
| `results$rCode`              |     |     |     |     | a preformatted |
| `results$save_factor_scores` |     |     |     |     | an output      |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`
