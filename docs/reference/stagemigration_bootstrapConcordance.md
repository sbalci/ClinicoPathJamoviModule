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
  checkpoint_callback = NULL,
  seed = 20250101L
)
```

## Value

A list with the bootstrap comparison of the C-index difference: `c_diff`
(original new-vs-old C-index difference), `p_value` (two-sided bootstrap
p-value), `ci_lower` and `ci_upper` (2.5 and 97.5 percentile confidence
limits), `se` (bootstrap standard error), and `n_successful_boots`
(number of successful bootstrap resamples). If fewer than 50 resamples
succeed or an error occurs, a reduced list with `NA` values for
`p_value`, `se`, `ci_lower`, and `ci_upper` is returned.
