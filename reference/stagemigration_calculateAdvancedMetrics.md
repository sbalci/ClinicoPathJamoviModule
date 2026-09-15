# Advanced Metrics Calculation for Stage Migration

Advanced Metrics Calculation for Stage Migration

## Usage

``` r
stagemigration_calculateAdvancedMetrics(
  data,
  options,
  checkpoint_callback = NULL
)
```

## Value

A named list of stage-migration discrimination and calibration metrics
comparing the old and new staging Cox models (fitted models, concordance
indices, C-index improvement with SE/CI/bootstrap, AIC/BIC improvements,
likelihood-ratio and linear-trend tests, pseudo R-squared, and
individual model LR statistics); on failure a list with a single `error`
element containing the error message.
