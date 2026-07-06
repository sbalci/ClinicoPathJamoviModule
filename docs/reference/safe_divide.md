# Safe division function

Performs division with safe handling of division by zero

## Usage

``` r
safe_divide(x, y, na_value = NA_real_)
```

## Arguments

- x:

  Numerator

- y:

  Denominator

- na_value:

  Value to return when division by zero occurs

## Value

Result of x/y or na_value if y is zero
