# Validate Data with Clinical Context

Validate data with clinical research considerations

## Usage

``` r
validate_clinical_data(
  data,
  required_vars = NULL,
  min_observations = 1,
  clinical_checks = TRUE
)
```

## Arguments

- data:

  The data to validate

- required_vars:

  Required variable names

- min_observations:

  Minimum number of observations required

- clinical_checks:

  Additional clinical validation checks

## Value

A list with `valid` (logical), character vectors `errors` and
`warnings`, and integer counts `n_observations` and `n_variables`.
