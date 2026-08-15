# Format a follow-up time for display in an interval label

Interval labels are built from raw numeric bounds, and the final bound
is the observed maximum follow-up – an unrounded double that rendered as
"60-134.449661066093". Whole numbers stay integer-looking; anything else
is shown to one decimal, which is the precision follow-up times are
reported in.

## Usage

``` r
.fmtTimeLabel(x)
```

## Arguments

- x:

  numeric time value

## Value

character scalar
