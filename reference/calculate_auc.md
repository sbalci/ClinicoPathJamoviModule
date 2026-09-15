# Approximate AUC from sensitivity and specificity

Uses a simplified formula to approximate AUC from sensitivity and
specificity

## Usage

``` r
calculate_auc(sens, spec)
```

## Arguments

- sens:

  Sensitivity of the test

- spec:

  Specificity of the test

## Value

Numeric AUC value or NA when inputs are not valid
