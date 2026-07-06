# Bootstrap Concordance Comparison

Bootstrap validation of C-index difference accounting for correlation
between staging systems (same patients).

## Usage

``` r
stagemigration_bootstrapConcordance(
  data,
  old_formula,
  new_formula,
  n_boot = 1000,
  checkpoint_callback = NULL
)
```
